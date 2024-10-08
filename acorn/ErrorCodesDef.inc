X(LexInvalidChar                           , 10000)
X(LexNumberCannotEndUnderscore             , 10001)
X(LexNumberBadTypeSpec                     , 10002)
X(LexInvalidUnicodeSeq                     , 10003)
X(LexStringMissingEndQuote                 , 10004)
X(LexUnknownComptimeDirective              , 10005)
X(LexMultilineCommentMissingClose          , 10006)
X(LexCharMissingEndQuote                   , 10007)
                                           
X(ParseExpectedExpression                  , 20002)
X(ParseExceededMaxFuncParams               , 20003)
X(ParseDuplicateModifier                   , 20005)
X(ParseConstVoidNotType                    , 20006)
X(ParseInvalidType                         , 20007)
X(ParseExceededMaxFuncCallArgs             , 20008)
X(ParseDuplicateMainFunc                   , 20009)
X(ParseIntegerValueCalcOverflow            , 20010)
X(ParseExpect                              , 20011)
X(ParseExpectIdent                         , 20012)
X(ParseMissingComptimeEndIf                , 20013)
X(ParseNativeFuncNoHaveBodies              , 20014)
X(ParseIntegerValueCalcUnderflow           , 20015)
X(ParseIntegerValueNotFitType              , 20016)
X(ParseExpectedImportIdentifier            , 20017)
X(ParseDuplicateImport                     , 20018)
X(ParseImportNotTopOfFile                  , 20019)
X(ParseNativeLinkNameNotString             , 20020)
X(ParseNativeLinkNameHasUnicode            , 20021)
                                           
X(SemaNotAllFuncPathReturn                 , 30000)
X(SemaVariableCannotHaveVoidType           , 30001)
X(SemaVariableConstNoValue                 , 30002)
X(SemaVariableTypeMismatch                 , 30003)
X(SemaFuncReturnTypeMismatch               , 30004)
X(SemaUnreachableStmt                      , 30005)
X(SemaIncompleteStmt                       , 30006)
X(SemaBinOpTypeCannotApply                 , 30007)
X(SemaBinOpTypeMismatch                    , 30008)
X(SemaUnaryOpTypeCannotApply               , 30009)
X(SemaUnaryOpAddressAccessNotLvalue        , 30010)
X(SemaUnaryOpIncDecNotLValue               , 30011)
X(SemaNoFindIdentRef                       , 30012)
X(SemaNoFindFuncIdentRef                   , 30013)
X(SemaTypeNoCallable                       , 30014)
X(SemaInvalidFuncCallSingle                , 30015)
X(SemaInvalidFuncCallOverloaded            , 30016)
X(SemaInvalidCast                          , 30017)
X(SemaExpectedModifiable                   , 30018)
X(SemaReassignConstVariable                , 30019)
X(SemaReassignConstAddress                 , 30020)
X(SemaDivisionByZero                       , 30021)
X(SemaExpectedCondition                    , 30022)
X(SemaNotComptimeCompute                   , 30023)
X(SemaMainBadReturnType                    , 30024)
X(SemaDuplicateGlobalFunc                  , 30025)
X(SemaDuplicateLocVariableDecl             , 20026)
X(SemaMainCannotHaveModifier               , 20027)
X(SemaNoLocalFuncs                         , 20028)
X(SemaDllImportWithoutNativeModifier       , 20029)
X(SemaDuplicateGlobalVar                   , 30030)
X(SemaNodeAtWrongScope                     , 30031)
X(SemaLocalVarHasModifiers                 , 30032)
X(SemaComptimeIfCannotCompareToVar         , 30033)
X(SemaExpectedAssignmentIfStmt             , 30034)
X(SemaCouldNotResolveImport                , 30035)
X(SemaArrayLengthNotInteger                , 30036)
X(SemaArrayLengthTooLargeType              , 30037)
X(SemaArrayLengthZero                      , 30038)
X(SemaArrayLengthNegative                  , 30039)
X(SemaMemoryAccessBadType                  , 30040)
X(SemaMemoryIndexNotInteger                , 30041)
X(SemaArrayLengthNotComptime               , 30042)
X(SemaDotOperatorCannotAccessType          , 30043)
X(SemaIncompatibleArrayElmTypes            , 30044)
X(SemaCannotAssignArrayDirectlyToPtr       , 30045)
X(SemaGlobalCircularDependency             , 30046)

X(NumericOverflow                          , 40000)
X(NumericUnderflow                         , 40001)

X(GlobalFailedToFindMsvcPaths              , 50001)
X(GlobalFailedToFindLLVMTarget             , 50002)
X(GlobalFailedToWriteObjFile               , 50003)
X(GlobalFailedToEmitObjFileType            , 50004)
X(GlobalFailedToFindCurrentPath            , 50005)
X(GlobalFailedToCreateOutputDirectory      , 50006)
X(GlobalFailedToInitializeLLVMNativeTarget , 50007)
X(GlobalCouldNotFindEntryPointFunc         , 50008)
X(GlobalFailedToMoveTempObjFile            , 50009)
X(GlobalFailedToFindMSVCPathsForLinking    , 50010)
X(GlobalFailedToDeleteObjFile              , 50011)
X(GlobalCouldNotCheckIfSourceExists        , 50012)
X(GlobalSourceDoesNotExists                , 50013)
X(GlobalNoSourcesProvided                  , 50014)
X(GlobalFailedToCheckSourceIsDir           , 50015)
X(GlobalWrongExtensionTypeForFile          , 50016)
X(GlobalFailedToReadSourceFile             , 50017)
X(GlobalMissingArgumentForFlag             , 50018)
X(GlobalUnknownCompilerFlag                , 50019)
X(GlobalNoStdLibrary                       , 50020)
