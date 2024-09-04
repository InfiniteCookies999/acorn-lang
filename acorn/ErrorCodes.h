#ifndef ERROR_H
#define ERROR_H

namespace acorn {
    enum class ErrCode {
        LexInvalidChar                        = 10000,
        LexNumberCannotEndUnderscore          = 10001,
        LexNumberBadTypeSpec                  = 10002,
        LexInvalidUnicodeSeq                  = 10003,
        LexStringMissingEndQuote              = 10004,
        LexUnknownComptimeDirective           = 10005,

        ParseExpectedExpression               = 20002,
        ParseExceededMaxFuncParams            = 20003,
        ParseDuplicateModifier                = 20005,
        ParseConstVoidNotType                 = 20006,
        ParseInvalidType                      = 20007,
        ParseExceededMaxFuncCallArgs          = 20008,
        ParseDuplicateMainFunc                = 20009,
        ParseIntegerValueCalcOverflow         = 20010,
        ParseExpect                           = 20011,
        ParseExpectIdent                      = 20012,
        ParseMissingComptimeEndIf             = 20013,

        SemaNotAllFuncPathReturn              = 30000,
        SemaVariableCannotHaveVoidType        = 30001,
        SemaVariableConstNoValue              = 30002,
        SemaVariableTypeMismatch              = 30003,
        SemaFuncReturnTypeMismatch            = 30004,
        SemaUnreachableStmt                   = 30005,
        SemaIncompleteStmt                    = 30006,
        SemaBinOpTypeCannotApply              = 30007,
        SemaBinOpTypeMismatch                 = 30008,
        SemaUnaryOpTypeCannotApply            = 30009,
        SemaUnaryOpAddressAccessNotLvalue     = 30010,
        SemaUnaryOpIncDecNotLValue            = 30011,
        SemaNoFindIdentRef                    = 30012,
        SemaNoFindFuncIdentRef                = 30013,
        SemaTypeNoCallable                    = 30014,
        SemaInvalidFuncCallSingle             = 30015,
        SemaInvalidFuncCallOverloaded         = 30016,
        SemaInvalidCast                       = 30017,
        SemaExpectedModifiable                = 30018,
        SemaReassignConstVariable             = 30019,
        SemaReassignConstAddress              = 30020,
        SemaDivisionByZero                    = 30021,
        SemaExpectedCondition                 = 30022,
        SemaNotComptimeCompute                = 30023,
        SemaMainBadReturnType                 = 30024,
        SemaDuplicateGlobalFunc               = 30025,
        SemaDuplicateLocVariableDecl          = 20026,
        SemaMainCannotHaveModifier            = 20027,

        SignedOverflow                        = 40000,
    };
}

#endif // ERROR_H