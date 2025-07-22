#include "Context.h"

#include "Type.h"
#include "Logger.h"
#include "ir/IRGen.h"
#include "Module.h"

namespace acorn {
    template<typename E>
        requires std::is_base_of_v<Expr, E>
    static std::pair<Identifier, Expr*> new_universal(PageAllocator& allocator, const char* name, auto&& initializer) {
        E* expr = allocator.alloc_type<E>();
        new (expr) E();
        initializer(expr);
        return { Identifier::get(name), expr };
    }
}

acorn::Context::Context(llvm::LLVMContext& ll_context, llvm::Module& ll_module, PageAllocator& allocator)
    : allocator(allocator),
      ll_context(ll_context),
      ll_module(ll_module),
      type_table(allocator, *this),

      invalid_type(Type::create(allocator, TypeKind::INVALID)),
      void_type(Type::create(allocator, TypeKind::VOID_T)),
      int_type(Type::create(allocator, TypeKind::INT)),
      const_int_type(type_table.get_const_type(int_type)),
      int8_type(Type::create(allocator, TypeKind::INT8)),
      int16_type(Type::create(allocator, TypeKind::INT16)),
      int32_type(Type::create(allocator, TypeKind::INT32)),
      int64_type(Type::create(allocator, TypeKind::INT64)),
      uint8_type(Type::create(allocator, TypeKind::UINT8)),
      uint16_type(Type::create(allocator, TypeKind::UINT16)),
      uint32_type(Type::create(allocator, TypeKind::UINT32)),
      uint64_type(Type::create(allocator, TypeKind::UINT64)),
      isize_type(Type::create(allocator, TypeKind::ISIZE)),
      usize_type(Type::create(allocator, TypeKind::USIZE)),
      float_type(Type::create(allocator, TypeKind::FLOAT)),
      double_type(Type::create(allocator, TypeKind::DOUBLE)),
      bool_type(Type::create(allocator, TypeKind::BOOL_T)),
      char_type(Type::create(allocator, TypeKind::CHAR)),
      char16_type(Type::create(allocator, TypeKind::CHAR16)),
      funcs_ref_type(Type::create(allocator, TypeKind::FUNCS_REF)),
      namespace_ref_type(Type::create(allocator, TypeKind::NAMESPACE_REF)),
      const_char_ptr_type(type_table.get_ptr_type(type_table.get_const_type(char_type))),
      char16_ptr_type(type_table.get_ptr_type(char16_type)),
      const_char16_ptr_type(type_table.get_ptr_type(type_table.get_const_type(char16_type))),
      const_char_ptr_ptr_type(type_table.get_ptr_type(const_char_ptr_type)),
      null_type(Type::create(allocator, TypeKind::NULL_T)),
      void_ptr_type(type_table.get_ptr_type(void_type)),
      const_void_type(type_table.get_const_type(void_type)),
      const_void_ptr_type(type_table.get_ptr_type(const_void_type)),
      empty_array_type(Type::create(allocator, TypeKind::EMPTY_ARRAY)),
      auto_type(Type::create(allocator, TypeKind::AUTO)),
      const_auto_type(type_table.get_const_type(auto_type)),
      auto_ptr_type(type_table.get_ptr_type(auto_type)),
      auto_const_ptr_type(type_table.get_const_type(auto_ptr_type)),
      expr_type(Type::create(allocator, TypeKind::EXPR)),
      indeterminate_type(Type::create(allocator, TypeKind::INDETERMINATE)),

      main_identifier(Identifier::get("main")),
      length_identifier(Identifier::get("length")),
      ptr_identifier(Identifier::get("ptr")),
      access_identifier(Identifier::get("access")),
      namespace_identifier(Identifier::get("namespace")),
      module_identifier(Identifier::get("module")),
      string_struct_identifier(Identifier::get("String")),
      reflect_identifier(Identifier::get("reflect")),
      type_id_enum_identifier(Identifier::get("TypeId")),
      type_struct_identifier(Identifier::get("Type")),
      struct_type_info_struct_identifier(Identifier::get("StructTypeInfo")),
      field_type_info_struct_identifier(Identifier::get("FieldTypeInfo")),
      any_struct_identifier(Identifier::get("Any")),
      enum_type_info_struct_identifier(Identifier::get("EnumTypeInfo")),
      value_identifier(Identifier::get("value")),
      error_interface_identifier(Identifier::get("Error")),
      get_name_function_identifier(Identifier::get("get_name")),
      new_identifier(Identifier::get("new")),
      moveobj_identifier(Identifier::get("moveobj")),
      copyobj_identifier(Identifier::get("copyobj")),
      delete_identifier(Identifier::get("delete")),

      reflect_identifiers({
          { "#type_info", ReflectKind::TypeInfo }
      }),

      keyword_mapping({
          { "void"         , Token::KW_VOID        },
          { "int"          , Token::KW_INT         },
          { "int8"         , Token::KW_INT8        },
          { "int16"        , Token::KW_INT16       },
          { "int32"        , Token::KW_INT32       },
          { "int64"        , Token::KW_INT64       },
          { "uint8"        , Token::KW_UINT8       },
          { "uint16"       , Token::KW_UINT16      },
          { "uint32"       , Token::KW_UINT32      },
          { "uint64"       , Token::KW_UINT64      },
          { "isize"        , Token::KW_ISIZE       },
          { "usize"        , Token::KW_USIZE       },
          { "float"        , Token::KW_FLOAT       },
          { "double"       , Token::KW_DOUBLE      },
          { "bool"         , Token::KW_BOOL        },
          { "char"         , Token::KW_CHAR        },
          { "char16"       , Token::KW_CHAR16      },
          { "const"        , Token::KW_CONST       },
          { "fn"           , Token::KW_FN          },
          { "null"         , Token::KW_NULL        },
          { "true"         , Token::KW_TRUE        },
          { "false"        , Token::KW_FALSE       },
          { "as"           , Token::KW_AS          },
          { "if"           , Token::KW_IF          },
          { "elif"         , Token::KW_ELFIF        },
          { "else"         , Token::KW_ELSE        },
          { "import"       , Token::KW_IMPORT      },
          { "continue"     , Token::KW_CONTINUE    },
          { "break"        , Token::KW_BREAK       },
          { "switch"       , Token::KW_SWITCH      },
          { "case"         , Token::KW_CASE        },
          { "static"       , Token::KW_STATIC      },
          { "struct"       , Token::KW_STRUCT      },
          { "interface"    , Token::KW_INTERFACE   },
          { "enum"         , Token::KW_ENUM        },
          { "this"         , Token::KW_THIS        },
          { "sizeof"       , Token::KW_SIZEOF      },
          { "copyobj"      , Token::KW_COPYOBJ     },
          { "moveobj"      , Token::KW_MOVEOBJ     },
          { "new"          , Token::KW_NEW         },
          { "uninit_new"   , Token::KW_UNINIT_NEW   },
          { "delete"       , Token::KW_DELETE      },
          { "raise"        , Token::KW_RAISE       },
          { "raises"       , Token::KW_RAISES      },
          { "try"          , Token::KW_TRY         },
          { "recover"      , Token::KW_RECOVER     },
          { "generics"     , Token::KW_GENERICS    },

          { "native"       , Token::KW_NATIVE      },
          { "dllimport"    , Token::KW_DLLIMPORT   },
          { "public"       , Token::KW_PUBLIC      },
          { "private"      , Token::KW_PRIVATE     },
          { "readonly"     , Token::KW_READONLY    },
          { "return"       , Token::KW_RETURN      },
          { "loop"         , Token::KW_LOOP        },
          { "in"           , Token::KW_IN          },

          { "#if"          , Token::KW_CT_IF        },
          { "#elif"        , Token::KW_CT_ELIF      },
          { "#else"        , Token::KW_CT_ELSE      },
          { "#endif"       , Token::KW_CT_ENDIF     },
          { "#file"        , Token::KW_CT_FILE      },
          { "#type_info"   , Token::KW_CT_TYPEINFO  },
          { "#aborts"      , Token::KW_CT_ABORTS    },
      }),

      ll_intrinsics_table({
          { Identifier::get("memcpy") , llvm::Intrinsic::IndependentIntrinsics::memcpy  },
          { Identifier::get("memset") , llvm::Intrinsic::IndependentIntrinsics::memset  },
          { Identifier::get("memmove"), llvm::Intrinsic::IndependentIntrinsics::memmove },
          { Identifier::get("floor")  , llvm::Intrinsic::IndependentIntrinsics::floor   },
          { Identifier::get("ceil")   , llvm::Intrinsic::IndependentIntrinsics::ceil    },
          { Identifier::get("pow")    , llvm::Intrinsic::IndependentIntrinsics::pow     },
          { Identifier::get("log")    , llvm::Intrinsic::IndependentIntrinsics::log     },
          { Identifier::get("log10")  , llvm::Intrinsic::IndependentIntrinsics::log10   },
          { Identifier::get("sqrt")   , llvm::Intrinsic::IndependentIntrinsics::sqrt    },
          { Identifier::get("sin")    , llvm::Intrinsic::IndependentIntrinsics::sin     },
          { Identifier::get("cos")    , llvm::Intrinsic::IndependentIntrinsics::cos     },
          { Identifier::get("tan")    , llvm::Intrinsic::IndependentIntrinsics::tan     },
          { Identifier::get("asin")   , llvm::Intrinsic::IndependentIntrinsics::asin    },
          { Identifier::get("acos")   , llvm::Intrinsic::IndependentIntrinsics::acos    },
          { Identifier::get("atan")   , llvm::Intrinsic::IndependentIntrinsics::atan    },
      }),

      ll_valid_intrinsic_defs({
          { Identifier::get("memcpy"), { void_ptr_type, const_void_ptr_type, usize_type }, void_type     },
          { Identifier::get("memset"), { void_ptr_type, int8_type          , usize_type }, void_type     },
          { Identifier::get("memmove"),{ void_ptr_type, const_void_ptr_type, usize_type }, void_type     },
          { Identifier::get("floor") , { double_type }                                   , double_type   },
          { Identifier::get("floor") , { float_type }                                    , float_type    },
          { Identifier::get("ceil")  , { double_type }                                   , double_type   },
          { Identifier::get("ceil")  , { float_type }                                    , float_type    },
          { Identifier::get("pow")   , { double_type, double_type }                      , double_type   },
          { Identifier::get("pow")   , { float_type, float_type }                        , float_type    },
          { Identifier::get("log")   , { double_type }                                   , double_type   },
          { Identifier::get("log")   , { float_type }                                    , float_type    },
          { Identifier::get("log10") , { double_type }                                   , double_type   },
          { Identifier::get("log10") , { float_type }                                    , float_type    },
          { Identifier::get("sqrt")  , { double_type }                                   , double_type   },
          { Identifier::get("sqrt")  , { float_type }                                    , float_type    },
          { Identifier::get("sin")   , { double_type }                                   , double_type   },
          { Identifier::get("sin")   , { float_type }                                    , float_type    },
          { Identifier::get("cos")   , { double_type }                                   , double_type   },
          { Identifier::get("cos")   , { float_type }                                    , float_type    },
          { Identifier::get("tan")   , { double_type }                                   , double_type   },
          { Identifier::get("tan")   , { float_type }                                    , float_type    },
          { Identifier::get("asin")  , { double_type }                                   , double_type   },
          { Identifier::get("asin")  , { float_type }                                    , float_type    },
          { Identifier::get("acos")  , { double_type }                                   , double_type   },
          { Identifier::get("acos")  , { float_type }                                    , float_type    },
          { Identifier::get("atan")  , { double_type }                                   , double_type   },
          { Identifier::get("atan")  , { float_type }                                    , float_type    },
      }),

      precedence({

          { '*', 10 },
          { '/', 10 },
          { '%', 10 },

          { '+', 9 },
          { '-', 9 },

          { Token::LT_LT, 8 }, // <<
          { Token::GT_GT, 8 }, // >>

          { Token::RANGE_EQ, 5 }, // ..=
          { Token::RANGE_LT, 5 }, // ..<

          { '<', 6 },
          { '>', 6 },
          { Token::LT_EQ, 6 }, // <=
          { Token::GT_EQ, 6 }, // >=

          { Token::EQ_EQ, 5 }, // ==
          { Token::EX_EQ, 5 }, // !=

          { '&', 4 },
          { '^', 3 },
          { '|', 2 },

          { Token::AND_AND, 1 }, // &&
          { Token::OR_OR  , 1 }, // ||

      })
{

    for (auto ptr = keyword_mapping.begin(), end = keyword_mapping.end();
         ptr != end; ++ptr) {
        inv_keyword_mapping[ptr->second] = ptr->first();
    }

    universal_constants.insert(new_universal<Bool>(allocator, "OS_GROUP_WINDOWS", [this](Bool* v) {
        v->type = bool_type;
#if WIN_OS
        v->value = true;
#else
        v->value = false;
#endif
    }));

    universal_constants.insert(new_universal<Bool>(allocator, "OS_GROUP_UNIX", [this](Bool* v) {
        v->type = bool_type;
#if UNIX_OS
        v->value = true;
#else
        v->value = false;
#endif
    }));

    universal_constants.insert(new_universal<Bool>(allocator, "SYS_64", [this](Bool* v) {
        v->type = bool_type;
#if IS_64_BITS
        v->value = true;
#else
        v->value = false;
#endif
    }));

    universal_constants.insert(new_universal<Bool>(allocator, "__USE_XOPEN2K8", [this](Bool* v) {
        v->type = bool_type;
#ifdef __USE_XOPEN2K8
        v->value = true;
#else
        v->value = false;
#endif
    }));

    universal_constants.insert(new_universal<Bool>(allocator, "__USE_TIME_BITS64", [this](Bool* v) {
        v->type = bool_type;
#ifdef __USE_TIME_BITS64
        v->value = true;
#else
        v->value = false;
#endif
    }));

    universal_constants.insert(new_universal<Number>(allocator, "SYS_ENDIAN", [this](Number* n) {
        n->type = int_type;
        if constexpr (std::endian::native == std::endian::big) {
            n->value_s64 = 1;
        } else {
            n->value_s64 = 2;
        }
    }));

    universal_constants.insert(new_universal<Number>(allocator, "SYS_BIG_ENDIAN", [this](Number* n) {
        n->type = int_type;
        n->value_s64 = 1;
    }));

    universal_constants.insert(new_universal<Number>(allocator, "SYS_LITTLE_ENDIAN", [this](Number* n) {
        n->type = int_type;
        n->value_s64 = 2;
    }));

}

acorn::Module* acorn::Context::get_or_create_modl(llvm::StringRef mod_name) {
    auto name = Identifier::get(mod_name);
    auto itr = modls.find(name);
    if (itr != modls.end()) {
        return itr->second;
    }
    Module* modl = allocator.alloc_type<Module>();
    new (modl) Module();
    modls.insert({ name, modl });
    return modl;
}

acorn::Module* acorn::Context::find_module(Identifier name) {
    auto itr = modls.find(name);
    return itr == modls.end() ? nullptr : itr->second;
}

void acorn::Context::queue_gen(Decl* decl, GenericInstance* generic_instance) {
    if (!generic_instance) {
        // TODO (maddie): we search through a list of all declarations ever established.
        // this will probably be rather slow as the size of the applications grow. It should
        // probably use a different data structure.
        auto itr = std::ranges::find(unchecked_gen_queue, decl);
        if (itr != unchecked_gen_queue.end()) {
            unchecked_gen_queue.erase(itr);
        }
    }
    decls_gen_queue.push_back({ decl, generic_instance });
}

void acorn::Context::queue_gen_implicit_function(ImplicitFunc* implicit_func) {
    decls_gen_queue.push_back({ implicit_func, nullptr });
}

void acorn::Context::add_canidate_main_function(Func* main_func) {
    std::lock_guard<std::mutex> lock(main_function_mtx);
    canidate_main_funcs.push_back(main_func);
}

acorn::TokenKind acorn::Context::get_keyword_kind(llvm::StringRef word) const {
    auto itr = keyword_mapping.find(word);
    if (itr != keyword_mapping.end()) {
        return itr->second;
    }
    return Token::INVALID;
}

bool acorn::Context::inc_error_count() {
    return ++error_count >= max_error_count;
}

acorn::Expr* acorn::Context::get_universal_constant(Identifier identifier) const {
    auto itr = universal_constants.find(identifier);
    if (itr != universal_constants.end()) {
        return itr->second;
    }
    return nullptr;
}
