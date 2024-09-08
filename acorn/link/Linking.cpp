#include "Linking.h"

#ifdef _WIN32
#include <Windows.h>
#undef min
#undef max
#endif

#include <filesystem>
#include <codecvt>

#include "../Logger.h"
#include "../Util.h"
#include "../Context.h"

// COM objects from visual studio's configuration dll. Do not want to go through the process
// of including the setup configuration headers so the interfaces we want are defined here with
// their respective IDs. The COM object for ISetupConfiguration will be loaded via COM library.
// 

#ifdef _WIN32

class DECLSPEC_UUID("B41463C3-8866-43B5-BC33-2B0676F7F42E") DECLSPEC_NOVTABLE ISetupInstance : public IUnknown {
public:
    virtual HRESULT STDMETHODCALLTYPE GetInstanceId(BSTR * pbstrInstanceId) = 0;
    virtual HRESULT STDMETHODCALLTYPE GetInstallDate(LPFILETIME pInstallDate) = 0;
    virtual HRESULT STDMETHODCALLTYPE GetInstallationName(BSTR* pbstrInstallationName) = 0;
    virtual HRESULT STDMETHODCALLTYPE GetInstallationPath(BSTR* pbstrInstallationPath) = 0;
    virtual HRESULT STDMETHODCALLTYPE GetInstallationVersion(BSTR* pbstrInstallationVersion) = 0;
    virtual HRESULT STDMETHODCALLTYPE GetDisplayName(LCID lcid, BSTR* pbstrDisplayName) = 0;
    virtual HRESULT STDMETHODCALLTYPE GetDescription(LCID lcid, BSTR* pbstrDescription) = 0;
    virtual HRESULT STDMETHODCALLTYPE ResolvePath(LPCOLESTR pwszRelativePath, BSTR* pbstrAbsolutePath) = 0;
};

class DECLSPEC_UUID("6380BCFF-41D3-4B2E-8B2E-BF8A6810C848") DECLSPEC_NOVTABLE IEnumSetupInstances : public IUnknown {
public:
    virtual HRESULT STDMETHODCALLTYPE Next(ULONG celt, ISetupInstance * *rgelt, ULONG * pceltFetched) = 0;
    virtual HRESULT STDMETHODCALLTYPE Skip(ULONG celt) = 0;
    virtual HRESULT STDMETHODCALLTYPE Reset(void) = 0;
    virtual HRESULT STDMETHODCALLTYPE Clone(IEnumSetupInstances** ppenum) = 0;
};


class DECLSPEC_UUID("42843719-DB4C-46C2-8E7C-64F1816EFD5B") DECLSPEC_NOVTABLE ISetupConfiguration : public IUnknown {
public:
    virtual HRESULT STDMETHODCALLTYPE EnumInstances(IEnumSetupInstances** ppEnumInstances) = 0;
    virtual HRESULT STDMETHODCALLTYPE GetInstanceForCurrentProcess(ISetupInstance** ppInstance) = 0;
    virtual HRESULT STDMETHODCALLTYPE GetInstanceForPath(LPCWSTR wzPath, ISetupInstance** ppInstance) = 0;
};

template<typename F>
struct Deferer {
    F cb;
    Deferer(F&& cb) : cb(cb) {}
    ~Deferer() { cb(); }
};

template<typename F>
Deferer<F> new_deferer(F&& cb) {
    return Deferer<F>(std::forward<F>(cb));
}

#define defer_name3(x, y) x##y
#define defer_name2(x, y) defer_name3(x, y)
#define defer_name(x) defer_name2(x, __COUNTER__)
#define defer(code) auto defer_name(_defer_val) = new_deferer([&] { code; });

namespace acorn {
    std::wstring find_newest_ver_directory(const std::wstring& path) {
        
        int best_ver[4]{0};
        namespace fs = std::filesystem;

        // TODO: error handling!
        for (const auto& entry : fs::directory_iterator(path)) {
            if (!entry.is_directory()) continue;
            std::string dir_name = entry.path().filename().string().c_str();
            
            int ver[4];
            auto r = sscanf_s((const char* const)dir_name.c_str(), "%d.%d.%d.%d", &ver[0], &ver[1], &ver[2], &ver[3]);
            if (r < 4) continue;

            for (int i = 0; i < 4; i++) {
                if (ver[i] > best_ver[i]) {
                    memcpy_s(best_ver, sizeof(best_ver), ver, sizeof(ver));
                    break;
                }
            }
        }
        return std::format(L"{}.{}.{}.{}", best_ver[0], best_ver[1], best_ver[2], best_ver[3]);
    }
}

bool acorn::get_windows_kits_install_paths(Context& context, PageAllocator& allocator, bool is_64bit_target,
                                           std::wstring& winkit_lib_um_path, std::wstring& winkit_lib_ucrt_path) {
    
    HKEY main_key;
    if (RegOpenKeyExA(HKEY_LOCAL_MACHINE, "SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots",
                      0, KEY_QUERY_VALUE | KEY_WOW64_32KEY | KEY_ENUMERATE_SUB_KEYS, &main_key) != ERROR_SUCCESS) {
        Logger::global_error(context, "Failed to find registry key for Windows Kits");
        return false;
    }
    defer(RegCloseKey(main_key));

    DWORD al_length;
    // First request the allocation size.
    if (RegQueryValueExA(main_key, "KitsRoot10", nullptr, nullptr, nullptr, &al_length) != ERROR_SUCCESS) {
        Logger::global_error(context, "Failed to find registry value for KitsRoot10");
        return false;
    }

    // Retrieving the registry value.
    DWORD req_length = al_length + 2;
    char* buffer = static_cast<char*>(allocator.allocate(req_length));
    if (RegQueryValueExA(main_key, "KitsRoot10", nullptr, nullptr, (LPBYTE)buffer, &req_length) != ERROR_SUCCESS) {
        Logger::global_error(context, "Failed to find registry value for KitsRoot10");
        return false;
    }

    if (!buffer) {
        Logger::global_error(context, "Failed to properly retrieve the KitsRoot10 buffer");
        return false;
    }

    // Null terminating.
    if (buffer[req_length]) {
        buffer[req_length] = '\0';
    }

    std::wstring_convert<std::codecvt_utf8<wchar_t>> converter;
    auto sdk_path = converter.from_bytes(buffer);

    auto lib_path = sdk_path + L"Lib\\";

    auto best_dir = find_newest_ver_directory(lib_path);
    winkit_lib_um_path = lib_path + best_dir + L"\\um";
    winkit_lib_ucrt_path = lib_path + best_dir + L"\\ucrt";
    if (is_64bit_target) {
        winkit_lib_um_path += L"\\x64";
        winkit_lib_ucrt_path += L"\\x64";
    } else {
        winkit_lib_um_path += L"\\x86";
        winkit_lib_ucrt_path += L"\\x86";
    }
    
    return true;
}

bool acorn::get_msvc_install_paths(Context& context, PageAllocator& allocator, bool is_64bit_target,
                                   std::wstring& bin_path, std::wstring& lib_path) {

    // Initialize some OOP library thing with windows that we require.
    if (CoInitialize(nullptr) != S_OK) {
        return false;
    }
    defer(CoUninitialize());

    // The clsid that refers to visual studio's setup configuration dll.
    GUID clsid_vs_setup_config = {0x177F0C4A, 0x1CD3, 0x4DE7, {0xA3, 0x2C, 0x71, 0xDB, 0xBB, 0x9F, 0xA3, 0x6D}};

    // Id for the ISetupConfiguration contained within vs setup dll.
    GUID interface_id = { 0x42843719, 0xDB4C, 0x46C2, {0x8E, 0x7C, 0x64, 0xF1, 0x81, 0x6E, 0xFD, 0x5B} };

    ISetupConfiguration* vs_config;
    if (CoCreateInstance(clsid_vs_setup_config, nullptr, CLSCTX_INPROC_SERVER, interface_id, (void**)&vs_config) != S_OK) {
        return false;
    }

    defer(vs_config->Release());

    IEnumSetupInstances* instances = nullptr;
    if (vs_config->EnumInstances(&instances) != S_OK || !instances) {
        return false;
    }
    defer(instances->Release());

    bool has_preferred = false;
    while (true) {
        ULONG found = 0;
        ISetupInstance* instance = nullptr;
        if (instances->Next(1, &instance, &found) != S_OK) {
            break;
        }
        defer(instance->Release());

        wchar_t* inst_path_ptr;
        if (instance->GetInstallationPath(&inst_path_ptr) != S_OK) {
            continue;
        }
        defer(SysFreeString(inst_path_ptr));

        auto inst_path = std::wstring(inst_path_ptr);

        // We will try and use C:\Program Files (x86) for 32 bit host machines and
        // C:\Program Files for 64 bit host machines. Note the target build of x32/x64
        // bit is different than the host so each folder can still contain x86 and x64
        // builds.
#ifdef _WIN64
        bool is_preferred = !inst_path.starts_with(L"C:\\Program Files (x86)");
#else
        bool is_preferred =  inst_path.starts_with(L"C:\\Program Files (x86)");
#endif

        // Microsoft.VCToolsVersion.default.txt contains the default version that is invoked by default
        // when executing command line tools so we will use that version.
        auto default_ver_file_path = inst_path + L"\\VC\\Auxiliary\\Build\\Microsoft.VCToolsVersion.default.txt";

        char*  buffer;
        size_t length;
        if (!read_entire_file(default_ver_file_path, buffer, length, allocator)) {
            Logger::global_error(context, "Your system is missing file '%s'", default_ver_file_path);
            return false;
        }

        auto version = std::string(buffer, length);
        trim(version);

        std::wstring_convert<std::codecvt_utf8<wchar_t>> converter;
        auto wversion = converter.from_bytes(version);

        auto base_path = inst_path + L"\\VC\\Tools\\MSVC\\" + wversion;
        
        auto msvc_lib = base_path + L"\\lib";
        auto msvc_bin = base_path + L"\\bin";
#ifdef _WIN64
        msvc_bin += L"\\Hostx64";
#else   
        msvc_bin += L"\\Hostx32";
#endif

        if (is_64bit_target) {
            msvc_bin += L"\\x64";
            msvc_lib += L"\\x64";
        } else {
            msvc_bin += L"\\x86";
            msvc_lib += L"\\x86";
        }
        

        bin_path = msvc_bin;
        lib_path = msvc_lib;
        if (is_preferred) {
            return true;
        }
    }

    // TODO: if we get here we should instead try to find older versions of visual studio

    return false;
}

#endif // _WIN32