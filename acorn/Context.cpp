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

      invalid_type(Type::create(allocator, TypeKind::Invalid)),
      void_type(Type::create(allocator, TypeKind::Void)),
      int_type(Type::create(allocator, TypeKind::Int)),
      const_int_type(type_table.get_const_type(int_type)),
      int8_type(Type::create(allocator, TypeKind::Int8)),
      int16_type(Type::create(allocator, TypeKind::Int16)),
      int32_type(Type::create(allocator, TypeKind::Int32)),
      int64_type(Type::create(allocator, TypeKind::Int64)),
      uint8_type(Type::create(allocator, TypeKind::UInt8)),
      uint16_type(Type::create(allocator, TypeKind::UInt16)),
      uint32_type(Type::create(allocator, TypeKind::UInt32)),
      uint64_type(Type::create(allocator, TypeKind::UInt64)),
      isize_type(Type::create(allocator, TypeKind::ISize)),
      usize_type(Type::create(allocator, TypeKind::USize)),
      float32_type(Type::create(allocator, TypeKind::Float32)),
      float64_type(Type::create(allocator, TypeKind::Float64)),
      bool_type(Type::create(allocator, TypeKind::Bool)),
      char_type(Type::create(allocator, TypeKind::Char)),
      char16_type(Type::create(allocator, TypeKind::Char16)),
      char32_type(Type::create(allocator, TypeKind::Char32)),
      funcs_ref_type(Type::create(allocator, TypeKind::FuncsRef)),
      namespace_ref_type(Type::create(allocator, TypeKind::NamespaceRef)),
      const_char_ptr_type(type_table.get_ptr_type(type_table.get_const_type(char_type))),
      const_char16_ptr_type(type_table.get_ptr_type(type_table.get_const_type(char16_type))),
      const_char32_ptr_type(type_table.get_ptr_type(type_table.get_const_type(char32_type))),
      const_char_ptr_ptr_type(type_table.get_ptr_type(const_char_ptr_type)),
      null_type(Type::create(allocator, TypeKind::Null)),
      void_ptr_type(type_table.get_ptr_type(void_type)),
      const_void_type(type_table.get_const_type(void_type)),
      const_void_ptr_type(type_table.get_ptr_type(const_void_type)),
      empty_array_type(Type::create(allocator, TypeKind::EmptyArray)),
      int_range_type(RangeType::create(allocator, int_type)),
      int8_range_type(RangeType::create(allocator, int8_type)),
      int16_range_type(RangeType::create(allocator, int16_type)),
      int32_range_type(RangeType::create(allocator, int32_type)),
      int64_range_type(RangeType::create(allocator, int64_type)),
      uint8_range_type(RangeType::create(allocator, uint8_type)),
      uint16_range_type(RangeType::create(allocator, uint16_type)),
      uint32_range_type(RangeType::create(allocator, uint32_type)),
      uint64_range_type(RangeType::create(allocator, uint64_type)),
      isize_range_type(RangeType::create(allocator, isize_type)),
      usize_range_type(RangeType::create(allocator, usize_type)),
      char_range_type(RangeType::create(allocator, char_type)),
      char16_range_type(RangeType::create(allocator, char16_type)),
      char32_range_type(RangeType::create(allocator, char32_type)),
      
      main_identifier(Identifier::get("main")),
      length_identifier(Identifier::get("length")),
      access_identifier(Identifier::get("access")),
      namespace_identifier(Identifier::get("namespace")),

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
          { "isize"    , Token::KwISize     },
          { "usize"    , Token::KwUSize     },
          { "float32"  , Token::KwFloat32   },
          { "float64"  , Token::KwFloat64   },
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
          { "import"   , Token::KwImport    },
          { "continue" , Token::KwContinue  },
          { "break"    , Token::KwBreak     },
          { "switch"   , Token::KwSwitch    },
          { "case"     , Token::KwCase      },
          { "static"   , Token::KwStatic    },
          { "struct"   , Token::KwStruct    },
          { "this"     , Token::KwThis      },
          { "sizeof"   , Token::KwSizeof    },
          { "copyobj"  , Token::KwCopyobj   },
          { "moveobj"  , Token::KwMoveobj   },

          { "native"   , Token::KwNative    },
          { "dllimport", Token::KwDllimport },
          { "pub"      , Token::KwPub       },
          { "prv"      , Token::KwPrv       },
          { "return"   , Token::KwReturn    },
          { "loop"     , Token::KwLoop      },

          { "#if"      , Token::KwCTIf      },
          { "#elif"    , Token::KwCTElIf    },
          { "#else"    , Token::KwCTElse    },
          { "#endif"   , Token::KwCTEndIf   },
          { "#file"    , Token::KwCTFile    },
      }),

      ll_intrinsics_table({
          { Identifier::get("memcpy"), llvm::Intrinsic::IndependentIntrinsics::memcpy },
          { Identifier::get("memset"), llvm::Intrinsic::IndependentIntrinsics::memset },
          { Identifier::get("floor") , llvm::Intrinsic::IndependentIntrinsics::floor  },
          { Identifier::get("ceil")  , llvm::Intrinsic::IndependentIntrinsics::ceil   },
          { Identifier::get("pow")   , llvm::Intrinsic::IndependentIntrinsics::pow    },
          { Identifier::get("log")   , llvm::Intrinsic::IndependentIntrinsics::log    },
          { Identifier::get("log10") , llvm::Intrinsic::IndependentIntrinsics::log10  },
          { Identifier::get("sqrt")  , llvm::Intrinsic::IndependentIntrinsics::sqrt   },
          { Identifier::get("sin")   , llvm::Intrinsic::IndependentIntrinsics::sin    },
          { Identifier::get("cos")   , llvm::Intrinsic::IndependentIntrinsics::cos    },
          { Identifier::get("tan")   , llvm::Intrinsic::IndependentIntrinsics::tan    },
          { Identifier::get("asin")  , llvm::Intrinsic::IndependentIntrinsics::asin   },
          { Identifier::get("acos")  , llvm::Intrinsic::IndependentIntrinsics::acos   },
          { Identifier::get("atan")  , llvm::Intrinsic::IndependentIntrinsics::atan   },
      }),

      ll_valid_intrinsic_defs({
          { Identifier::get("memcpy"), { void_ptr_type, const_void_ptr_type, usize_type }, void_type      },
          { Identifier::get("memset"), { void_ptr_type, int8_type          , usize_type }, void_ptr_type  },
          { Identifier::get("floor") , { float64_type }                                  , float64_type   },
          { Identifier::get("floor") , { float32_type }                                  , float32_type   },
          { Identifier::get("ceil")  , { float64_type }                                  , float64_type   },
          { Identifier::get("ceil")  , { float32_type }                                  , float32_type   },
          { Identifier::get("pow")   , { float64_type, float64_type }                    , float64_type   },
          { Identifier::get("pow")   , { float32_type, float32_type }                    , float32_type   },
          { Identifier::get("log")   , { float64_type }                                  , float64_type   },
          { Identifier::get("log")   , { float32_type }                                  , float32_type   },
          { Identifier::get("log10") , { float64_type }                                  , float64_type   },
          { Identifier::get("log10") , { float32_type }                                  , float32_type   },
          { Identifier::get("sqrt")  , { float64_type }                                  , float64_type   },
          { Identifier::get("sqrt")  , { float32_type }                                  , float32_type   },
          { Identifier::get("sin")   , { float64_type }                                  , float64_type   },
          { Identifier::get("sin")   , { float32_type }                                  , float32_type   },
          { Identifier::get("cos")   , { float64_type }                                  , float64_type   },
          { Identifier::get("cos")   , { float32_type }                                  , float32_type   },
          { Identifier::get("tan")   , { float64_type }                                  , float64_type   },
          { Identifier::get("tan")   , { float32_type }                                  , float32_type   },
          { Identifier::get("asin")  , { float64_type }                                  , float64_type   },
          { Identifier::get("asin")  , { float32_type }                                  , float32_type   },
          { Identifier::get("acos")  , { float64_type }                                  , float64_type   },
          { Identifier::get("acos")  , { float32_type }                                  , float32_type   },
          { Identifier::get("atan")  , { float64_type }                                  , float64_type   },
          { Identifier::get("atan")  , { float32_type }                                  , float32_type   },
      }),

      precedence({

          { '*', 10 },
          { '/', 10 },
          { '%', 10 },
          
          { '+', 9 },
          { '-', 9 },
          
          { Token::LtLt, 8 }, // <<
          { Token::GtGt, 8 }, // >>

          { Token::RangeEq, 5 }, // ..=
          { Token::RangeLt, 5 }, // ..<

          { '<', 6 },
          { '>', 6 },
          { Token::LtEq, 6 }, // <=
          { Token::GtEq, 6 }, // >=
          
          { Token::EqEq, 5 }, // ==
          { Token::ExEq, 5 }, // !=
          
          { '&', 4 },
          { '^', 3 },
          { '|', 2 },

          { Token::AndAnd, 1 }, // &&
          { Token::OrOr  , 1 }, // ||

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

void acorn::Context::queue_gen(Decl* decl) {
    if (decl->generated) {
        return;
    }
    auto itr = std::ranges::find(unchcked_gen_queue, decl);
    if (itr != unchcked_gen_queue.end()) {
        unchcked_gen_queue.erase(itr);
    }
    decl->generated = true;
    decls_gen_queue.push_back(decl);
}

void acorn::Context::add_canidate_main_function(Func* main_func) {
    std::lock_guard<std::mutex> lock(main_function_mtx);
    canidate_main_funcs.push_back(main_func);
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

acorn::Expr* acorn::Context::get_universal_constant(Identifier identifier) const {
    auto itr = universal_constants.find(identifier);
    if (itr != universal_constants.end()) {
        return itr->second;
    }
    return nullptr;
}