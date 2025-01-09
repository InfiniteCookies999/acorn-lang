
add_executable (fuzzing "Fuzzer.cpp" "RandomFuzzer.h" "RandomFuzzer.cpp" "ValidCharsFuzzer.h" "ValidCharsFuzzer.cpp" "ValidLexemesFuzzer.h" "ValidLexemesFuzzer.cpp")

# Win32 actually refers to windows OS that supports 32 and 64 bit.
if(WIN32)
    target_compile_definitions(tests PUBLIC UNICODE _UNICODE)
endif()
target_link_libraries(fuzzing LINK_PUBLIC acorn)
