#include "Context.h"

#include "Type.h"
#include "Logger.h"
#include "ir/IRGen.h"

acorn::Context::Context(llvm::LLVMContext& ll_context, llvm::Module& ll_module, PageAllocator& allocator)
    : allocator(allocator),
      ll_context(ll_context),
      ll_module(ll_module),
      type_table(allocator),

      invalid_type(Type::create(allocator, TypeKind::Invalid)),
      void_type(Type::create(allocator, TypeKind::Void)),
      int_type(Type::create(allocator, TypeKind::Int)),
      int8_type(Type::create(allocator, TypeKind::Int8)),
      int16_type(Type::create(allocator, TypeKind::Int16)),
      int32_type(Type::create(allocator, TypeKind::Int32)),
      int64_type(Type::create(allocator, TypeKind::Int64)),
      uint8_type(Type::create(allocator, TypeKind::UInt8)),
      uint16_type(Type::create(allocator, TypeKind::UInt16)),
      uint32_type(Type::create(allocator, TypeKind::UInt32)),
      uint64_type(Type::create(allocator, TypeKind::UInt64)),
      bool_type(Type::create(allocator, TypeKind::Bool)),
      char_type(Type::create(allocator, TypeKind::Char)),
      char16_type(Type::create(allocator, TypeKind::Char16)),
      char32_type(Type::create(allocator, TypeKind::Char32)),
      funcs_ref_type(Type::create(allocator, TypeKind::FuncsRef)),
      const_char_ptr_type(type_table.get_ptr_type(type_table.get_const_type(char_type))),
      const_char16_ptr_type(type_table.get_ptr_type(type_table.get_const_type(char16_type))),
      const_char32_ptr_type(type_table.get_ptr_type(type_table.get_const_type(char32_type))),
      null_type(Type::create(allocator, TypeKind::Null)),
      void_ptr_type(type_table.get_ptr_type(void_type)),
      const_void_type(type_table.get_const_type(void_type)),

      main_identifier(Identifier::get("main")),

      keyword_mapping({
          { "void"     , Token::KwVoid      },
          { "int"      , Token::KwInt       },
          { "int8"     , Token::KwInt8      },
          { "int16"    , Token::KwInt16     },
          { "int32"    , Token::KwInt32     },
          { "int64"    , Token::KwInt64     },
          { "uint8"    , Token::KwUInt8     },
          { "uint16"   , Token::KwUInt16    },
          { "uint32"   , Token::KwUInt32    },
          { "uint64"   , Token::KwUInt64    },
          { "bool"     , Token::KwBool      },
          { "char"     , Token::KwChar      },
          { "char16"   , Token::KwChar16    },
          { "char32"   , Token::KwChar32    },
          { "const"    , Token::KwConst     },
          { "null"     , Token::KwNull      },
          { "true"     , Token::KwTrue      },
          { "false"    , Token::KwFalse     },
          { "as"       , Token::KwAs        },
          { "if"       , Token::KwIf        },
          { "elif"     , Token::KwElIf      },
          { "else"     , Token::KwElse      },

          { "native"   , Token::KwNative    },
          { "dllimport", Token::KwDllimport },
          { "return"   , Token::KwReturn    },
      }),

    precedence({

        { '*', 9 },
        { '/', 9 },
        { '%', 9 },

        { '+', 8 },
        { '-', 8 },

        { Token::LtLt, 7 }, // <<
        { Token::GtGt, 7 }, // >>

        { '<', 6 },
        { '>', 6 },
        { Token::LtEq, 6 }, // <=
        { Token::GtEq, 6 }, // >=

        { Token::EqEq, 5 }, // ==
        { Token::ExEq, 5 }, // !=

        { '&', 4 },
        { '^', 3 },
        { '|', 2 },

    })
{
    for (auto ptr = keyword_mapping.begin(), end = keyword_mapping.end();
         ptr != end; ++ptr) {
        inv_keyword_mapping[ptr->second] = ptr->first();
    }
}

void acorn::Context::queue_gen(Decl* decl) {
    if (decl->generated) {
        return;
    }
    unchcked_gen_queue.erase(std::ranges::find(unchcked_gen_queue, decl));
    decl->generated = true;
    decls_gen_queue.push_back(decl);
}

acorn::Func* acorn::Context::set_or_get_main_function(Func* main_func) {
    std::lock_guard<std::mutex> lock(main_function_mtx);
    if (this->main_func) {
        return this->main_func;
    }
    this->main_func = main_func;
    return nullptr;
}

acorn::tokkind acorn::Context::get_keyword_kind(llvm::StringRef word) const {
    auto itr = keyword_mapping.find(word);
    if (itr != keyword_mapping.end()) {
        return itr->second;
    }
    return Token::Invalid;
}

bool acorn::Context::inc_error_count() {
    return ++error_count >= max_error_count;
}
