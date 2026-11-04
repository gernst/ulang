#include "ulang.h"

const struct u_name n_nat_46_Zero = {"nat", "Zero", 0};

struct u_object o_nat_46_Zero = U_UNIT(n_nat_46_Zero);

const struct u_name n_nat_46_Succ = {"nat", "Succ", 1};

const struct u_name n_syntax_46_Name = {"syntax", "Name", 0};

const struct u_name n_syntax_46_THole = {"syntax", "THole", 0};

struct u_object o_syntax_46_THole = U_UNIT(n_syntax_46_THole);

const struct u_name n_syntax_46_TMeta = {"syntax", "TMeta", 1};

const struct u_name n_syntax_46_TParam = {"syntax", "TParam", 2};

const struct u_name n_syntax_46_TSort = {"syntax", "TSort", 3};

const struct u_name n_syntax_46_TFun = {"syntax", "TFun", 4};

const struct u_name n_syntax_46_TProc = {"syntax", "TProc", 5};

const struct u_name n_syntax_46_TTuple = {"syntax", "TTuple", 6};

const struct u_name n_syntax_46_VUnit = {"syntax", "VUnit", 0};

struct u_object o_syntax_46_VUnit = U_UNIT(n_syntax_46_VUnit);

const struct u_name n_syntax_46_VBool = {"syntax", "VBool", 1};

const struct u_name n_syntax_46_VInt = {"syntax", "VInt", 2};

const struct u_name n_syntax_46_VStr = {"syntax", "VStr", 3};

const struct u_name n_syntax_46_VChar = {"syntax", "VChar", 4};

const struct u_name n_syntax_46_Var = {"syntax", "Var", 0};

const struct u_name n_syntax_46_ELit = {"syntax", "ELit", 0};

const struct u_name n_syntax_46_EVar = {"syntax", "EVar", 1};

const struct u_name n_syntax_46_EApp = {"syntax", "EApp", 2};

const struct u_name n_syntax_46_ELam = {"syntax", "ELam", 3};

const struct u_name n_syntax_46_EProc = {"syntax", "EProc", 4};

const struct u_name n_syntax_46_ELet = {"syntax", "ELet", 5};

const struct u_name n_syntax_46_EIte = {"syntax", "EIte", 6};

const struct u_name n_syntax_46_EMatch = {"syntax", "EMatch", 7};

const struct u_name n_syntax_46_EComp = {"syntax", "EComp", 8};

const struct u_name n_syntax_46_ETuple = {"syntax", "ETuple", 9};

const struct u_name n_syntax_46_ERaise = {"syntax", "ERaise", 10};

const struct u_name n_syntax_46_PWild = {"syntax", "PWild", 0};

struct u_object o_syntax_46_PWild = U_UNIT(n_syntax_46_PWild);

const struct u_name n_syntax_46_PLit = {"syntax", "PLit", 1};

const struct u_name n_syntax_46_PVar = {"syntax", "PVar", 2};

const struct u_name n_syntax_46_PApp = {"syntax", "PApp", 3};

const struct u_name n_syntax_46_PTuple = {"syntax", "PTuple", 4};

const struct u_name n_syntax_46_Branch = {"syntax", "Branch", 0};

const struct u_name n_syntax_46_SSkip = {"syntax", "SSkip", 0};

struct u_object o_syntax_46_SSkip = U_UNIT(n_syntax_46_SSkip);

const struct u_name n_syntax_46_SAssign = {"syntax", "SAssign", 1};

const struct u_name n_syntax_46_SBlock = {"syntax", "SBlock", 2};

const struct u_name n_syntax_46_SIf = {"syntax", "SIf", 3};

const struct u_name n_syntax_46_SWhile = {"syntax", "SWhile", 4};

const struct u_name n_syntax_46_SCall = {"syntax", "SCall", 5};

const struct u_name n_syntax_46_SMatch = {"syntax", "SMatch", 6};

const struct u_name n_syntax_46_SAssert = {"syntax", "SAssert", 7};

const struct u_name n_syntax_46_SRaise = {"syntax", "SRaise", 8};

const struct u_name n_syntax_46_STry = {"syntax", "STry", 9};

const struct u_name n_syntax_46_Handler = {"syntax", "Handler", 0};

const struct u_name n_syntax_46_Constr = {"syntax", "Constr", 0};

const struct u_name n_syntax_46_FPrefix = {"syntax", "FPrefix", 0};

struct u_object o_syntax_46_FPrefix = U_UNIT(n_syntax_46_FPrefix);

const struct u_name n_syntax_46_FPostfix = {"syntax", "FPostfix", 1};

struct u_object o_syntax_46_FPostfix = U_UNIT(n_syntax_46_FPostfix);

const struct u_name n_syntax_46_FInfix = {"syntax", "FInfix", 2};

struct u_object o_syntax_46_FInfix = U_UNIT(n_syntax_46_FInfix);

const struct u_name n_syntax_46_FLeft = {"syntax", "FLeft", 3};

struct u_object o_syntax_46_FLeft = U_UNIT(n_syntax_46_FLeft);

const struct u_name n_syntax_46_FRight = {"syntax", "FRight", 4};

struct u_object o_syntax_46_FRight = U_UNIT(n_syntax_46_FRight);

const struct u_name n_syntax_46_CSort = {"syntax", "CSort", 0};

const struct u_name n_syntax_46_CData = {"syntax", "CData", 1};

const struct u_name n_syntax_46_CFun = {"syntax", "CFun", 2};

const struct u_name n_syntax_46_CProc = {"syntax", "CProc", 3};

const struct u_name n_syntax_46_CModule = {"syntax", "CModule", 4};

const struct u_name n_syntax_46_CImport = {"syntax", "CImport", 5};

const struct u_name n_syntax_46_CNotation = {"syntax", "CNotation", 6};

const struct u_name n_syntax_46_CTest = {"syntax", "CTest", 7};

const struct u_name n_syntax_46_Program = {"syntax", "Program", 0};

const struct u_name n_lex_46_TName = {"lex", "TName", 0};

const struct u_name n_lex_46_TSym = {"lex", "TSym", 1};

const struct u_name n_lex_46_TPunct = {"lex", "TPunct", 2};

const struct u_name n_lex_46_TNum = {"lex", "TNum", 3};

const struct u_name n_lex_46_TStr = {"lex", "TStr", 4};

const struct u_name n_lex_46_TChar = {"lex", "TChar", 5};

const struct u_name n_lex_46_TEnd = {"lex", "TEnd", 6};

struct u_object o_lex_46_TEnd = U_UNIT(n_lex_46_TEnd);

const struct u_name n_lex_46_LexError = {"lex", "LexError", 0};

const struct u_name n_set_46_SLeaf = {"set", "SLeaf", 0};

struct u_object o_set_46_SLeaf = U_UNIT(n_set_46_SLeaf);

const struct u_name n_set_46_SNode = {"set", "SNode", 1};

const struct u_name n_parse_46_ParseError = {"parse", "ParseError", 0};

const struct u_name n_parse_46_Notations = {"parse", "Notations", 0};

const struct u_name n_parse_46_Scope = {"parse", "Scope", 0};

const struct u_name n_parse_46_Operand = {"parse", "Operand", 0};

const struct u_name n_parse_46_Operator = {"parse", "Operator", 1};

const struct u_name n_parse_46_Ops = {"parse", "Ops", 0};

const struct u_name n_parse_46_Context = {"parse", "Context", 0};

const struct u_name n_map_46_MLeaf = {"map", "MLeaf", 0};

struct u_object o_map_46_MLeaf = U_UNIT(n_map_46_MLeaf);

const struct u_name n_map_46_MNode = {"map", "MNode", 1};

const struct u_name n_ctx_46_Ctx = {"ctx", "Ctx", 0};

const struct u_name n_check_46_State = {"check", "State", 0};

const struct u_name n_check_46_UnknownSort = {"check", "UnknownSort", 0};

const struct u_name n_check_46_SortArity = {"check", "SortArity", 1};

const struct u_name n_check_46_SortDefined = {"check", "SortDefined", 2};

const struct u_name n_check_46_MissingType = {"check", "MissingType", 3};

const struct u_name n_check_46_UndeclaredParams = {"check", "UndeclaredParams", 4};

const struct u_name n_check_46_CannotUnify = {"check", "CannotUnify", 5};

const struct u_name n_check_46_InfiniteType = {"check", "InfiniteType", 6};

const struct u_name n_check_46_UnknownVariable = {"check", "UnknownVariable", 7};

const struct u_name n_check_46_OverloadedUnapplied = {"check", "OverloadedUnapplied", 8};

const struct u_name n_check_46_WrongArity = {"check", "WrongArity", 9};

const struct u_name n_check_46_AlreadyDefined = {"check", "AlreadyDefined", 10};

const struct u_name n_check_46_Duplicate = {"check", "Duplicate", 11};

const struct u_name n_check_46_Ambiguous = {"check", "Ambiguous", 12};

const struct u_name n_check_46_UnknownField = {"check", "UnknownField", 13};

const struct u_name n_check_46_UnknownFieldType = {"check", "UnknownFieldType", 14};

const struct u_name n_check_46_CountMismatch = {"check", "CountMismatch", 15};

const struct u_name n_check_46_UnknownConstructor = {"check", "UnknownConstructor", 16};

const struct u_name n_check_46_NotExhaustive = {"check", "NotExhaustive", 17};

const struct u_name n_check_46_Unreachable = {"check", "Unreachable", 18};

const struct u_name n_check_46_NotAssignable = {"check", "NotAssignable", 19};

const struct u_name n_check_46_UnknownProcedure = {"check", "UnknownProcedure", 20};

const struct u_name n_check_46_UnknownException = {"check", "UnknownException", 21};

const struct u_name n_check_46_TestParams = {"check", "TestParams", 22};

const struct u_name n_check_46_HCon = {"check", "HCon", 0};

const struct u_name n_check_46_HLit = {"check", "HLit", 1};

const struct u_name n_check_46_Checked = {"check", "Checked", 0};

const struct u_name n_value_46_Lit = {"value", "Lit", 0};

const struct u_name n_value_46_Obj = {"value", "Obj", 1};

const struct u_name n_value_46_Clos = {"value", "Clos", 2};

const struct u_name n_value_46_Fn = {"value", "Fn", 3};

const struct u_name n_value_46_Con = {"value", "Con", 4};

const struct u_name n_value_46_Prim = {"value", "Prim", 5};

const struct u_name n_value_46_Proc = {"value", "Proc", 6};

const struct u_name n_value_46_ProcClos = {"value", "ProcClos", 7};

const struct u_name n_run_46_ProcDef = {"run", "ProcDef", 0};

const struct u_name n_run_46_Prog = {"run", "Prog", 0};

const struct u_name n_run_46_Uninitialized = {"run", "Uninitialized", 0};

const struct u_name n_run_46_NotFunction = {"run", "NotFunction", 1};

const struct u_name n_run_46_Unspecified = {"run", "Unspecified", 2};

const struct u_name n_run_46_NoCase = {"run", "NoCase", 3};

const struct u_name n_run_46_CompareFunctions = {"run", "CompareFunctions", 4};

struct u_object o_run_46_CompareFunctions = U_UNIT(n_run_46_CompareFunctions);

const struct u_name n_run_46_NotList = {"run", "NotList", 5};

const struct u_name n_run_46_AssertionFailed = {"run", "AssertionFailed", 6};

struct u_object o_run_46_AssertionFailed = U_UNIT(n_run_46_AssertionFailed);

const struct u_name n_run_46_AssertionUnequal = {"run", "AssertionUnequal", 7};

const struct u_name n_run_46_UnassignedOutput = {"run", "UnassignedOutput", 8};

const struct u_name n_run_46_UnknownProc = {"run", "UnknownProc", 9};

const struct u_name n_run_46_NotProc = {"run", "NotProc", 10};

const struct u_name n_run_46_RecursiveInitialization = {"run", "RecursiveInitialization", 11};

const struct u_name n_run_46_Thrown = {"run", "Thrown", 0};

const struct u_name n_c_46_CtVoid = {"c", "CtVoid", 0};

struct u_object o_c_46_CtVoid = U_UNIT(n_c_46_CtVoid);

const struct u_name n_c_46_CtInt = {"c", "CtInt", 1};

struct u_object o_c_46_CtInt = U_UNIT(n_c_46_CtInt);

const struct u_name n_c_46_CtNamed = {"c", "CtNamed", 2};

const struct u_name n_c_46_CtPtr = {"c", "CtPtr", 3};

const struct u_name n_c_46_CeVar = {"c", "CeVar", 0};

const struct u_name n_c_46_CeNum = {"c", "CeNum", 1};

const struct u_name n_c_46_CeStr = {"c", "CeStr", 2};

const struct u_name n_c_46_CeCall = {"c", "CeCall", 3};

const struct u_name n_c_46_CeUnary = {"c", "CeUnary", 4};

const struct u_name n_c_46_CeBinary = {"c", "CeBinary", 5};

const struct u_name n_c_46_CeIndex = {"c", "CeIndex", 6};

const struct u_name n_c_46_CeArrow = {"c", "CeArrow", 7};

const struct u_name n_c_46_CeCast = {"c", "CeCast", 8};

const struct u_name n_c_46_CeCond = {"c", "CeCond", 9};

const struct u_name n_c_46_CeInit = {"c", "CeInit", 10};

const struct u_name n_c_46_CsExpr = {"c", "CsExpr", 0};

const struct u_name n_c_46_CsDecl = {"c", "CsDecl", 1};

const struct u_name n_c_46_CsAssign = {"c", "CsAssign", 2};

const struct u_name n_c_46_CsIf = {"c", "CsIf", 3};

const struct u_name n_c_46_CsWhile = {"c", "CsWhile", 4};

const struct u_name n_c_46_CsReturn = {"c", "CsReturn", 5};

const struct u_name n_c_46_CsBlock = {"c", "CsBlock", 6};

const struct u_name n_c_46_CdInclude = {"c", "CdInclude", 0};

const struct u_name n_c_46_CdPrototype = {"c", "CdPrototype", 1};

const struct u_name n_c_46_CdFunction = {"c", "CdFunction", 2};

const struct u_name n_c_46_CdGlobal = {"c", "CdGlobal", 3};

const struct u_name n_c_46_CFile = {"c", "CFile", 0};

const struct u_name n_compile_46_Unsupported = {"compile", "Unsupported", 0};

const struct u_name n_compile_46_UnknownEntry = {"compile", "UnknownEntry", 1};

const struct u_name n_compile_46_Globals = {"compile", "Globals", 0};

const struct u_name n_compile_46_Gen = {"compile", "Gen", 0};

u_value w_f_prelude_46_not_1(u_value *env, u_value *args);

u_value w_f_prelude_46__61__61__62__2(u_value *env, u_value *args);

u_value w_f_prelude_46__33__61__2(u_value *env, u_value *args);

u_value w_f_prelude_46__60__61__2(u_value *env, u_value *args);

u_value w_f_prelude_46__62__2(u_value *env, u_value *args);

u_value w_f_prelude_46__62__61__2(u_value *env, u_value *args);

u_value w_f_prelude_46__45__1(u_value *env, u_value *args);

u_value w_f_option_46_or_else_2(u_value *env, u_value *args);

u_value w_f_list_46__43__43__2(u_value *env, u_value *args);

u_value w_f_list_46_length_1(u_value *env, u_value *args);

u_value w_f_list_46_lookup_2(u_value *env, u_value *args);

u_value w_f_list_46_update_3(u_value *env, u_value *args);

u_value w_f_list_46_replaced_3(u_value *env, u_value *args);

u_value w_f_list_46_remove_2(u_value *env, u_value *args);

u_value w_f_list_46_has_key_2(u_value *env, u_value *args);

u_value w_f_list_46_keys_1(u_value *env, u_value *args);

u_value w_f_list_46_values_1(u_value *env, u_value *args);

u_value w_f_list_46_contains_2(u_value *env, u_value *args);

u_value w_f_list_46_exists_2(u_value *env, u_value *args);

u_value w_f_list_46_forall_2(u_value *env, u_value *args);

u_value w_f_list_46_distinct_1(u_value *env, u_value *args);

u_value w_f_list_46_repeated_1(u_value *env, u_value *args);

u_value w_f_list_46_diff_2(u_value *env, u_value *args);

u_value w_f_list_46_concat_1(u_value *env, u_value *args);

u_value w_f_list_46_reverse_onto_2(u_value *env, u_value *args);

u_value w_f_list_46_reverse_1(u_value *env, u_value *args);

u_value w_f_list_46_zip_2(u_value *env, u_value *args);

u_value w_f_list_46_take_2(u_value *env, u_value *args);

u_value w_f_list_46_drop_2(u_value *env, u_value *args);

u_value w_f_list_46_replicate_2(u_value *env, u_value *args);

u_value w_f_list_46_firsts_1(u_value *env, u_value *args);

u_value w_f_list_46_seconds_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Name_46_module_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Name_46_base_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Name_46_module_58__61__2(u_value *env, u_value *args);

u_value w_f_syntax_46_Name_46_base_58__61__2(u_value *env, u_value *args);

u_value w_f_syntax_46_Var_46_name_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Var_46_typ_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Var_46_name_58__61__2(u_value *env, u_value *args);

u_value w_f_syntax_46_Var_46_typ_58__61__2(u_value *env, u_value *args);

u_value w_f_syntax_46_Branch_46_pat_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Branch_46_guard_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Branch_46_body_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Branch_46_pat_58__61__2(u_value *env, u_value *args);

u_value w_f_syntax_46_Branch_46_guard_58__61__2(u_value *env, u_value *args);

u_value w_f_syntax_46_Branch_46_body_58__61__2(u_value *env, u_value *args);

u_value w_f_syntax_46_Handler_46_typ_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Handler_46_pat_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Handler_46_guard_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Handler_46_body_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Handler_46_typ_58__61__2(u_value *env, u_value *args);

u_value w_f_syntax_46_Handler_46_pat_58__61__2(u_value *env, u_value *args);

u_value w_f_syntax_46_Handler_46_guard_58__61__2(u_value *env, u_value *args);

u_value w_f_syntax_46_Handler_46_body_58__61__2(u_value *env, u_value *args);

u_value w_f_syntax_46_Constr_46_name_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Constr_46_args_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Constr_46_name_58__61__2(u_value *env, u_value *args);

u_value w_f_syntax_46_Constr_46_args_58__61__2(u_value *env, u_value *args);

u_value w_f_syntax_46_Program_46_cmds_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Program_46_cmds_58__61__2(u_value *env, u_value *args);

u_value w_f_syntax_46_unqualified_1(u_value *env, u_value *args);

u_value w_f_syntax_46_qualified_1(u_value *env, u_value *args);

u_value w_f_syntax_46_list_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_nil_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_cons_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_option_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_none_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_some_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_os_error_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_division_by_zero_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_no_index_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_no_character_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_missing_field_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_tuple_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_names_1(u_value *env, u_value *args);

u_value w_f_syntax_46_types_1(u_value *env, u_value *args);

u_value w_f_syntax_46_arity_1(u_value *env, u_value *args);

u_value w_f_lex_46_is_symbol_1(u_value *env, u_value *args);

u_value w_f_lex_46_is_punct_1(u_value *env, u_value *args);

u_value w_f_lex_46_is_name_char_1(u_value *env, u_value *args);

u_value w_f_lex_46_peek_2(u_value *env, u_value *args);

u_value w_f_lex_46_unescape_1(u_value *env, u_value *args);

u_value w_f_lex_46_scan_3(u_value *env, u_value *args);

u_value w_f_lex_46_tokens_1(u_value *env, u_value *args);

u_value w_f_set_46_empty_set_0(u_value *env, u_value *args);

u_value w_f_set_46_member_2(u_value *env, u_value *args);

u_value w_f_set_46_insert_2(u_value *env, u_value *args);

u_value w_f_set_46_from_list_1(u_value *env, u_value *args);

u_value w_f_set_46_insert_all_2(u_value *env, u_value *args);

u_value w_f_set_46_elements_1(u_value *env, u_value *args);

u_value w_f_set_46_elements_onto_2(u_value *env, u_value *args);

u_value w_f_set_46_count_1(u_value *env, u_value *args);

u_value w_f_set_46_depth_1(u_value *env, u_value *args);

u_value w_f_set_46_snode_depth_2(u_value *env, u_value *args);

u_value w_f_set_46_snode_3(u_value *env, u_value *args);

u_value w_f_set_46_turn_left_1(u_value *env, u_value *args);

u_value w_f_set_46_turn_right_1(u_value *env, u_value *args);

u_value w_f_set_46_smaller_1(u_value *env, u_value *args);

u_value w_f_set_46_greater_1(u_value *env, u_value *args);

u_value w_f_set_46_rebalance_3(u_value *env, u_value *args);

u_value w_f_set_46_well_formed_1(u_value *env, u_value *args);

u_value w_f_set_46_upto_1(u_value *env, u_value *args);

u_value w_f_parse_46_Notations_46_cmds_1(u_value *env, u_value *args);

u_value w_f_parse_46_Notations_46_ops_1(u_value *env, u_value *args);

u_value w_f_parse_46_Notations_46_cmds_58__61__2(u_value *env, u_value *args);

u_value w_f_parse_46_Notations_46_ops_58__61__2(u_value *env, u_value *args);

u_value w_f_parse_46_Scope_46_params_1(u_value *env, u_value *args);

u_value w_f_parse_46_Scope_46_notations_1(u_value *env, u_value *args);

u_value w_f_parse_46_Scope_46_params_58__61__2(u_value *env, u_value *args);

u_value w_f_parse_46_Scope_46_notations_58__61__2(u_value *env, u_value *args);

u_value w_f_parse_46_keywords_0(u_value *env, u_value *args);

u_value w_f_parse_46_is_keyword_1(u_value *env, u_value *args);

u_value w_f_parse_46_is_reserved_1(u_value *env, u_value *args);

u_value w_f_parse_46_prefix_prec_2(u_value *env, u_value *args);

u_value w_f_parse_46_postfix_prec_2(u_value *env, u_value *args);

u_value w_f_parse_46_infix_of_2(u_value *env, u_value *args);

u_value w_f_parse_46_declared_2(u_value *env, u_value *args);

u_value w_f_parse_46_no_notations_0(u_value *env, u_value *args);

u_value w_f_parse_46_with_notations_2(u_value *env, u_value *args);

u_value w_f_parse_46_operators_of_1(u_value *env, u_value *args);

u_value w_f_parse_46_head_token_1(u_value *env, u_value *args);

u_value w_f_parse_46_second_token_1(u_value *env, u_value *args);

u_value w_f_parse_46_head_offset_1(u_value *env, u_value *args);

u_value w_f_parse_46_at_keyword_2(u_value *env, u_value *args);

u_value w_f_parse_46_at_punct_2(u_value *env, u_value *args);

u_value w_f_parse_46_describe_1(u_value *env, u_value *args);

u_value w_f_parse_46_operator_at_2(u_value *env, u_value *args);

u_value w_f_parse_46_Ops_46_notations_1(u_value *env, u_value *args);

u_value w_f_parse_46_Ops_46_prefix_op_1(u_value *env, u_value *args);

u_value w_f_parse_46_Ops_46_postfix_op_1(u_value *env, u_value *args);

u_value w_f_parse_46_Ops_46_infix_op_1(u_value *env, u_value *args);

u_value w_f_parse_46_Ops_46_notations_58__61__2(u_value *env, u_value *args);

u_value w_f_parse_46_Ops_46_prefix_op_58__61__2(u_value *env, u_value *args);

u_value w_f_parse_46_Ops_46_postfix_op_58__61__2(u_value *env, u_value *args);

u_value w_f_parse_46_Ops_46_infix_op_58__61__2(u_value *env, u_value *args);

u_value w_f_parse_46_is_prec_2(u_value *env, u_value *args);

u_value w_f_parse_46_type_one_1(u_value *env, u_value *args);

u_value w_f_parse_46_type_components_1(u_value *env, u_value *args);

u_value w_f_parse_46_type_prefix_2(u_value *env, u_value *args);

u_value w_f_parse_46_type_infix_3(u_value *env, u_value *args);

u_value w_f_parse_46_has_invalid_1(u_value *env, u_value *args);

u_value w_f_parse_46_any_invalid_1(u_value *env, u_value *args);

u_value w_f_parse_46_starts_type_2(u_value *env, u_value *args);

u_value w_f_parse_46_expr_op1_2(u_value *env, u_value *args);

u_value w_f_parse_46_expr_prefix_2(u_value *env, u_value *args);

u_value w_f_parse_46_expr_op2_3(u_value *env, u_value *args);

u_value w_f_parse_46_cons_exprs_2(u_value *env, u_value *args);

u_value w_f_parse_46_starts_expr_2(u_value *env, u_value *args);

u_value w_f_parse_46_at_update_1(u_value *env, u_value *args);

u_value w_f_parse_46_pattern_vars_1(u_value *env, u_value *args);

u_value w_f_parse_46_at_section_1(u_value *env, u_value *args);

u_value w_f_parse_46_nil_expr_0(u_value *env, u_value *args);

u_value w_f_parse_46_in_library_3(u_value *env, u_value *args);

u_value w_f_parse_46_hidden_vars_1(u_value *env, u_value *args);

u_value w_f_parse_46_unpack_2(u_value *env, u_value *args);

u_value w_f_parse_46_pat_op1_2(u_value *env, u_value *args);

u_value w_f_parse_46_pat_prefix_2(u_value *env, u_value *args);

u_value w_f_parse_46_pat_op2_3(u_value *env, u_value *args);

u_value w_f_parse_46_cons_pats_2(u_value *env, u_value *args);

u_value w_f_parse_46_starts_pattern_2(u_value *env, u_value *args);

u_value w_f_parse_46_starts_stmt_1(u_value *env, u_value *args);

u_value w_f_parse_46_handlers_1(u_value *env, u_value *args);

u_value w_f_parse_46_section_at_3(u_value *env, u_value *args);

u_value w_f_parse_46_item_follows_1(u_value *env, u_value *args);

u_value w_f_parse_46_declarations_1(u_value *env, u_value *args);

u_value w_f_parse_46_notations_of_1(u_value *env, u_value *args);

u_value w_f_parse_46_is_notation_1(u_value *env, u_value *args);

u_value w_f_parse_46_Context_46_notations_1(u_value *env, u_value *args);

u_value w_f_parse_46_Context_46_modules_1(u_value *env, u_value *args);

u_value w_f_parse_46_Context_46_loading_1(u_value *env, u_value *args);

u_value w_f_parse_46_Context_46_base_1(u_value *env, u_value *args);

u_value w_f_parse_46_Context_46_dir_1(u_value *env, u_value *args);

u_value w_f_parse_46_Context_46_notations_58__61__2(u_value *env, u_value *args);

u_value w_f_parse_46_Context_46_modules_58__61__2(u_value *env, u_value *args);

u_value w_f_parse_46_Context_46_loading_58__61__2(u_value *env, u_value *args);

u_value w_f_parse_46_Context_46_base_58__61__2(u_value *env, u_value *args);

u_value w_f_parse_46_Context_46_dir_58__61__2(u_value *env, u_value *args);

u_value w_f_parse_46_with_commands_2(u_value *env, u_value *args);

u_value w_f_parse_46_with_module_2(u_value *env, u_value *args);

u_value w_f_parse_46_modules_of_1(u_value *env, u_value *args);

u_value w_f_parse_46_own_notations_2(u_value *env, u_value *args);

u_value w_f_parse_46_field_names_1(u_value *env, u_value *args);

u_value w_f_parse_46_differing_1(u_value *env, u_value *args);

u_value w_f_parse_46_type_of_field_1(u_value *env, u_value *args);

u_value w_f_parse_46_fields_named_2(u_value *env, u_value *args);

u_value w_f_parse_46_fields_of_1(u_value *env, u_value *args);

u_value w_f_parse_46_named_fields_4(u_value *env, u_value *args);

u_value w_f_parse_46_accessors_3(u_value *env, u_value *args);

u_value w_f_parse_46_accessor_5(u_value *env, u_value *args);

u_value w_f_parse_46_field_type_1(u_value *env, u_value *args);

u_value w_f_parse_46_field_case_1(u_value *env, u_value *args);

u_value w_f_parse_46_missing_field_2(u_value *env, u_value *args);

u_value w_f_parse_46_updaters_3(u_value *env, u_value *args);

u_value w_f_parse_46_updater_5(u_value *env, u_value *args);

u_value w_f_parse_46_update_case_1(u_value *env, u_value *args);

u_value w_f_parse_46_positions_1(u_value *env, u_value *args);

u_value w_f_parse_46_positions_from_2(u_value *env, u_value *args);

u_value w_f_parse_46_base_notations_1(u_value *env, u_value *args);

u_value w_f_parse_46_last_part_1(u_value *env, u_value *args);

u_value w_f_parse_46_base_name_1(u_value *env, u_value *args);

u_value w_f_parse_46_dir_name_1(u_value *env, u_value *args);

u_value w_f_map_46_empty_0(u_value *env, u_value *args);

u_value w_f_map_46_get_2(u_value *env, u_value *args);

u_value w_f_map_46_has_2(u_value *env, u_value *args);

u_value w_f_map_46_put_3(u_value *env, u_value *args);

u_value w_f_map_46_entries_1(u_value *env, u_value *args);

u_value w_f_map_46_entries_onto_2(u_value *env, u_value *args);

u_value w_f_map_46_size_1(u_value *env, u_value *args);

u_value w_f_map_46_height_1(u_value *env, u_value *args);

u_value w_f_map_46_mnode_4(u_value *env, u_value *args);

u_value w_f_map_46_rotate_left_1(u_value *env, u_value *args);

u_value w_f_map_46_rotate_right_1(u_value *env, u_value *args);

u_value w_f_map_46_left_of_1(u_value *env, u_value *args);

u_value w_f_map_46_right_of_1(u_value *env, u_value *args);

u_value w_f_map_46_balance_4(u_value *env, u_value *args);

u_value w_f_map_46_balanced_1(u_value *env, u_value *args);

u_value w_f_map_46_mnode_height_2(u_value *env, u_value *args);

u_value w_f_map_46_delete_2(u_value *env, u_value *args);

u_value w_f_map_46_delete_all_2(u_value *env, u_value *args);

u_value w_f_map_46_join_2(u_value *env, u_value *args);

u_value w_f_map_46_first_entry_1(u_value *env, u_value *args);

u_value w_f_map_46_remove_first_1(u_value *env, u_value *args);

u_value w_f_map_46_put_all_2(u_value *env, u_value *args);

u_value w_f_map_46_numbers_1(u_value *env, u_value *args);

u_value w_f_ctx_46_Ctx_46_aliases_1(u_value *env, u_value *args);

u_value w_f_ctx_46_Ctx_46_funs_1(u_value *env, u_value *args);

u_value w_f_ctx_46_Ctx_46_procs_1(u_value *env, u_value *args);

u_value w_f_ctx_46_Ctx_46_datas_1(u_value *env, u_value *args);

u_value w_f_ctx_46_Ctx_46_members_1(u_value *env, u_value *args);

u_value w_f_ctx_46_Ctx_46_constrs_1(u_value *env, u_value *args);

u_value w_f_ctx_46_Ctx_46_named_1(u_value *env, u_value *args);

u_value w_f_ctx_46_Ctx_46_aliases_58__61__2(u_value *env, u_value *args);

u_value w_f_ctx_46_Ctx_46_funs_58__61__2(u_value *env, u_value *args);

u_value w_f_ctx_46_Ctx_46_procs_58__61__2(u_value *env, u_value *args);

u_value w_f_ctx_46_Ctx_46_datas_58__61__2(u_value *env, u_value *args);

u_value w_f_ctx_46_Ctx_46_members_58__61__2(u_value *env, u_value *args);

u_value w_f_ctx_46_Ctx_46_constrs_58__61__2(u_value *env, u_value *args);

u_value w_f_ctx_46_Ctx_46_named_58__61__2(u_value *env, u_value *args);

u_value w_f_ctx_46_empty_ctx_0(u_value *env, u_value *args);

u_value w_f_ctx_46_ctx_of_2(u_value *env, u_value *args);

u_value w_f_ctx_46_owned_map_2(u_value *env, u_value *args);

u_value w_f_ctx_46_owned_2(u_value *env, u_value *args);

u_value w_f_ctx_46_owned_named_2(u_value *env, u_value *args);

u_value w_f_ctx_46_in_modules_2(u_value *env, u_value *args);

u_value w_f_ctx_46_ctx_union_2(u_value *env, u_value *args);

u_value w_f_ctx_46_union_named_2(u_value *env, u_value *args);

u_value w_f_ctx_46_alias_of_2(u_value *env, u_value *args);

u_value w_f_ctx_46_schemes_of_2(u_value *env, u_value *args);

u_value w_f_ctx_46_proc_of_2(u_value *env, u_value *args);

u_value w_f_ctx_46_data_of_2(u_value *env, u_value *args);

u_value w_f_ctx_46_members_of_2(u_value *env, u_value *args);

u_value w_f_ctx_46_constr_of_2(u_value *env, u_value *args);

u_value w_f_ctx_46_is_fun_2(u_value *env, u_value *args);

u_value w_f_ctx_46_is_proc_2(u_value *env, u_value *args);

u_value w_f_ctx_46_is_data_2(u_value *env, u_value *args);

u_value w_f_ctx_46_is_alias_2(u_value *env, u_value *args);

u_value w_f_ctx_46_is_constr_2(u_value *env, u_value *args);

u_value w_f_ctx_46_is_sort_2(u_value *env, u_value *args);

u_value w_f_ctx_46_names_of_2(u_value *env, u_value *args);

u_value w_f_ctx_46_with_named_2(u_value *env, u_value *args);

u_value w_f_ctx_46_with_alias_4(u_value *env, u_value *args);

u_value w_f_ctx_46_with_fun_3(u_value *env, u_value *args);

u_value w_f_ctx_46_with_proc_3(u_value *env, u_value *args);

u_value w_f_ctx_46_with_data_3(u_value *env, u_value *args);

u_value w_f_ctx_46_with_members_3(u_value *env, u_value *args);

u_value w_f_ctx_46_with_constr_3(u_value *env, u_value *args);

u_value w_f_check_46_State_46_ctx_1(u_value *env, u_value *args);

u_value w_f_check_46_State_46_sub_1(u_value *env, u_value *args);

u_value w_f_check_46_State_46_count_1(u_value *env, u_value *args);

u_value w_f_check_46_State_46_module_1(u_value *env, u_value *args);

u_value w_f_check_46_State_46_visible_1(u_value *env, u_value *args);

u_value w_f_check_46_State_46_table_1(u_value *env, u_value *args);

u_value w_f_check_46_State_46_ctx_58__61__2(u_value *env, u_value *args);

u_value w_f_check_46_State_46_sub_58__61__2(u_value *env, u_value *args);

u_value w_f_check_46_State_46_count_58__61__2(u_value *env, u_value *args);

u_value w_f_check_46_State_46_module_58__61__2(u_value *env, u_value *args);

u_value w_f_check_46_State_46_visible_58__61__2(u_value *env, u_value *args);

u_value w_f_check_46_State_46_table_58__61__2(u_value *env, u_value *args);

u_value w_f_check_46_with_ctx_2(u_value *env, u_value *args);

u_value w_f_check_46_enter_2(u_value *env, u_value *args);

u_value w_f_check_46_sees_2(u_value *env, u_value *args);

u_value w_f_check_46_start_2(u_value *env, u_value *args);

u_value w_f_check_46_flatten_2(u_value *env, u_value *args);

u_value w_f_check_46_in_module_2(u_value *env, u_value *args);

u_value w_f_check_46_qualify_2(u_value *env, u_value *args);

u_value w_f_check_46_qualify_constr_2(u_value *env, u_value *args);

u_value w_f_check_46_imports_in_2(u_value *env, u_value *args);

u_value w_f_check_46_visibility_2(u_value *env, u_value *args);

u_value w_f_check_46_imports_of_2(u_value *env, u_value *args);

u_value w_f_check_46_visible_names_3(u_value *env, u_value *args);

u_value w_f_check_46_is_visible_2(u_value *env, u_value *args);

u_value w_f_check_46_tbool_0(u_value *env, u_value *args);

u_value w_f_check_46_tint_0(u_value *env, u_value *args);

u_value w_f_check_46_tstr_0(u_value *env, u_value *args);

u_value w_f_check_46_tchar_0(u_value *env, u_value *args);

u_value w_f_check_46_tlist_1(u_value *env, u_value *args);

u_value w_f_check_46_fun_2(u_value *env, u_value *args);

u_value w_f_check_46_params_1(u_value *env, u_value *args);

u_value w_f_check_46_basic_1(u_value *env, u_value *args);

u_value w_f_check_46_literal_1(u_value *env, u_value *args);

u_value w_f_check_46_initial_0(u_value *env, u_value *args);

u_value w_f_check_46_arith_0(u_value *env, u_value *args);

u_value w_f_check_46_logic_0(u_value *env, u_value *args);

u_value w_f_check_46_builtin_1(u_value *env, u_value *args);

u_value w_f_check_46_builtin_scheme_1(u_value *env, u_value *args);

u_value w_f_check_46_is_meta_1(u_value *env, u_value *args);

u_value w_f_check_46_free_1(u_value *env, u_value *args);

u_value w_f_check_46_holes_1(u_value *env, u_value *args);

u_value w_f_check_46_subst_2(u_value *env, u_value *args);

u_value w_f_check_46_resolve_2(u_value *env, u_value *args);

u_value w_f_check_46_occurs_3(u_value *env, u_value *args);

u_value w_f_check_46_constructor_scheme_1(u_value *env, u_value *args);

u_value w_f_check_46_constructor_of_2(u_value *env, u_value *args);

u_value w_f_check_46_candidates_3(u_value *env, u_value *args);

u_value w_f_check_46_proc_scheme_2(u_value *env, u_value *args);

u_value w_f_check_46_with_arity_2(u_value *env, u_value *args);

u_value w_f_check_46_has_arity_2(u_value *env, u_value *args);

u_value w_f_check_46_bind_1(u_value *env, u_value *args);

u_value w_f_check_46_fixed_2(u_value *env, u_value *args);

u_value w_f_check_46_generalize_3(u_value *env, u_value *args);

u_value w_f_check_46_generalized_3(u_value *env, u_value *args);

u_value w_f_check_46_case_of_1(u_value *env, u_value *args);

u_value w_f_check_46_zonk_guard_2(u_value *env, u_value *args);

u_value w_f_check_46_zonk_type_2(u_value *env, u_value *args);

u_value w_f_check_46_zonk_var_2(u_value *env, u_value *args);

u_value w_f_check_46_zonk_vars_2(u_value *env, u_value *args);

u_value w_f_check_46_zonk_expr_2(u_value *env, u_value *args);

u_value w_f_check_46_zonk_binding_2(u_value *env, u_value *args);

u_value w_f_check_46_zonk_expr_branch_2(u_value *env, u_value *args);

u_value w_f_check_46_zonk_pat_2(u_value *env, u_value *args);

u_value w_f_check_46_zonk_stmt_2(u_value *env, u_value *args);

u_value w_f_check_46_zonk_stmt_branch_2(u_value *env, u_value *args);

u_value w_f_check_46_zonk_handler_2(u_value *env, u_value *args);

u_value w_f_check_46_zonk_cmd_2(u_value *env, u_value *args);

u_value w_f_check_46_unknown_type_1(u_value *env, u_value *args);

u_value w_f_check_46_unknown_in_1(u_value *env, u_value *args);

u_value w_f_check_46_unknown_in_expr_1(u_value *env, u_value *args);

u_value w_f_check_46_unknown_in_exprs_1(u_value *env, u_value *args);

u_value w_f_check_46_branch_exprs_1(u_value *env, u_value *args);

u_value w_f_check_46_unknown_in_all_1(u_value *env, u_value *args);

u_value w_f_check_46_unknown_in_branches_1(u_value *env, u_value *args);

u_value w_f_check_46_unknown_in_handlers_1(u_value *env, u_value *args);

u_value w_f_check_46_unknown_in_cmd_1(u_value *env, u_value *args);

u_value w_f_check_46_head_of_1(u_value *env, u_value *args);

u_value w_f_check_46_heads_1(u_value *env, u_value *args);

u_value w_f_check_46_field_types_3(u_value *env, u_value *args);

u_value w_f_check_46_constructors_of_3(u_value *env, u_value *args);

u_value w_f_check_46_fields_of_4(u_value *env, u_value *args);

u_value w_f_check_46_covers_2(u_value *env, u_value *args);

u_value w_f_check_46_defaults_1(u_value *env, u_value *args);

u_value w_f_check_46_specialize_3(u_value *env, u_value *args);

u_value w_f_check_46_useful_5(u_value *env, u_value *args);

u_value w_f_check_46_useful_in_6(u_value *env, u_value *args);

u_value w_f_check_46_witness_2(u_value *env, u_value *args);

u_value w_f_check_46_missing_4(u_value *env, u_value *args);

u_value w_f_check_46_missing_in_6(u_value *env, u_value *args);

u_value w_f_check_46_first_missing_5(u_value *env, u_value *args);

u_value w_f_check_46_other_missing_6(u_value *env, u_value *args);

u_value w_f_check_46_uncovered_2(u_value *env, u_value *args);

u_value w_f_check_46_first_uncovered_2(u_value *env, u_value *args);

u_value w_f_check_46_unguarded_1(u_value *env, u_value *args);

u_value w_f_check_46_initialized_1(u_value *env, u_value *args);

u_value w_f_check_46_os_procs_1(u_value *env, u_value *args);

u_value w_f_check_46_syntax_procs_1(u_value *env, u_value *args);

u_value w_f_check_46_curried_2(u_value *env, u_value *args);

u_value w_f_check_46_first_list_1(u_value *env, u_value *args);

u_value w_f_check_46_block_names_1(u_value *env, u_value *args);

u_value w_f_check_46_Checked_46_cmds_1(u_value *env, u_value *args);

u_value w_f_check_46_Checked_46_ctx_1(u_value *env, u_value *args);

u_value w_f_check_46_Checked_46_imports_1(u_value *env, u_value *args);

u_value w_f_check_46_Checked_46_cmds_58__61__2(u_value *env, u_value *args);

u_value w_f_check_46_Checked_46_ctx_58__61__2(u_value *env, u_value *args);

u_value w_f_check_46_Checked_46_imports_58__61__2(u_value *env, u_value *args);

u_value w_f_check_46_positions_of_2(u_value *env, u_value *args);

u_value w_f_check_46_position_in_3(u_value *env, u_value *args);

u_value w_f_check_46_modules_1(u_value *env, u_value *args);

u_value w_f_check_46_own_decls_2(u_value *env, u_value *args);

u_value w_f_check_46_is_module_1(u_value *env, u_value *args);

u_value w_f_check_46_ev_1(u_value *env, u_value *args);

u_value w_f_check_46_pv_1(u_value *env, u_value *args);

u_value w_f_check_46_num_1(u_value *env, u_value *args);

u_value w_f_check_46_ap_2(u_value *env, u_value *args);

u_value w_f_check_46_tl_1(u_value *env, u_value *args);

u_value w_f_check_46_lists_0(u_value *env, u_value *args);

u_value w_f_value_46_list_value_1(u_value *env, u_value *args);

u_value w_f_value_46_list_elements_1(u_value *env, u_value *args);

u_value w_f_print_46_show_value_1(u_value *env, u_value *args);

u_value w_f_print_46_show_values_1(u_value *env, u_value *args);

u_value w_f_print_46_list_items_1(u_value *env, u_value *args);

u_value w_f_print_46_escape_char_1(u_value *env, u_value *args);

u_value w_f_print_46_show_type_1(u_value *env, u_value *args);

u_value w_f_print_46_show_types_1(u_value *env, u_value *args);

u_value w_f_print_46_type_section_2(u_value *env, u_value *args);

u_value w_f_print_46_type_operand_1(u_value *env, u_value *args);

u_value w_f_print_46_type_component_1(u_value *env, u_value *args);

u_value w_f_print_46_show_pat_1(u_value *env, u_value *args);

u_value w_f_print_46_show_pats_1(u_value *env, u_value *args);

u_value w_f_print_46_symbolic_1(u_value *env, u_value *args);

u_value w_f_print_46_infix_text_3(u_value *env, u_value *args);

u_value w_f_print_46_pat_operand_1(u_value *env, u_value *args);

u_value w_f_print_46_text_of_1(u_value *env, u_value *args);

u_value w_f_run_46_Prog_46_funs_1(u_value *env, u_value *args);

u_value w_f_run_46_Prog_46_procs_1(u_value *env, u_value *args);

u_value w_f_run_46_Prog_46_constrs_1(u_value *env, u_value *args);

u_value w_f_run_46_Prog_46_positions_1(u_value *env, u_value *args);

u_value w_f_run_46_Prog_46_computing_1(u_value *env, u_value *args);

u_value w_f_run_46_Prog_46_args_1(u_value *env, u_value *args);

u_value w_f_run_46_Prog_46_funs_58__61__2(u_value *env, u_value *args);

u_value w_f_run_46_Prog_46_procs_58__61__2(u_value *env, u_value *args);

u_value w_f_run_46_Prog_46_constrs_58__61__2(u_value *env, u_value *args);

u_value w_f_run_46_Prog_46_positions_58__61__2(u_value *env, u_value *args);

u_value w_f_run_46_Prog_46_computing_58__61__2(u_value *env, u_value *args);

u_value w_f_run_46_Prog_46_args_58__61__2(u_value *env, u_value *args);

u_value w_f_run_46_position_of_2(u_value *env, u_value *args);

u_value w_f_run_46_load_1(u_value *env, u_value *args);

u_value w_f_run_46_fun_defs_1(u_value *env, u_value *args);

u_value w_f_run_46_proc_defs_1(u_value *env, u_value *args);

u_value w_f_run_46_constr_positions_1(u_value *env, u_value *args);

u_value w_f_run_46_numbers_from_2(u_value *env, u_value *args);

u_value w_f_run_46_constr_defs_1(u_value *env, u_value *args);

u_value w_f_run_46_constr_arity_1(u_value *env, u_value *args);

u_value w_f_run_46_builtin_names_0(u_value *env, u_value *args);

u_value w_f_run_46_is_builtin_1(u_value *env, u_value *args);

u_value w_f_run_46_is_function_1(u_value *env, u_value *args);

u_value w_f_run_46_truth_1(u_value *env, u_value *args);

u_value w_f_run_46_lambdas_2(u_value *env, u_value *args);

u_value w_f_run_46_str_values_1(u_value *env, u_value *args);

u_value w_f_run_46_char_values_1(u_value *env, u_value *args);

u_value w_f_run_46_bind_all_3(u_value *env, u_value *args);

u_value w_f_run_46_match_into_3(u_value *env, u_value *args);

u_value w_f_run_46_match_all_into_3(u_value *env, u_value *args);

u_value w_f_run_46_pattern_vars_1(u_value *env, u_value *args);

u_value w_f_run_46_variable_value_4(u_value *env, u_value *args);

u_value w_f_run_46_failure_2(u_value *env, u_value *args);

u_value w_f_run_46_order_2(u_value *env, u_value *args);

u_value w_f_run_46_restored_3(u_value *env, u_value *args);

u_value w_f_run_46_values_in_2(u_value *env, u_value *args);

u_value w_f_run_46_read_outs_3(u_value *env, u_value *args);

u_value w_f_run_46_asserting_3(u_value *env, u_value *args);

u_value w_f_run_46_assigning_1(u_value *env, u_value *args);

u_value w_f_run_46_int_1(u_value *env, u_value *args);

u_value w_f_run_46_procedures_0(u_value *env, u_value *args);

u_value w_f_run_46_hole_var_1(u_value *env, u_value *args);

u_value w_f_run_46_equals_2(u_value *env, u_value *args);

u_value w_f_c_46_c_type_1(u_value *env, u_value *args);

u_value w_f_c_46_c_declare_2(u_value *env, u_value *args);

u_value w_f_c_46_c_expr_1(u_value *env, u_value *args);

u_value w_f_c_46_c_operand_1(u_value *env, u_value *args);

u_value w_f_c_46_c_stmt_2(u_value *env, u_value *args);

u_value w_f_c_46_c_stmts_2(u_value *env, u_value *args);

u_value w_f_c_46_c_params_1(u_value *env, u_value *args);

u_value w_f_c_46_c_param_1(u_value *env, u_value *args);

u_value w_f_c_46_c_decl_1(u_value *env, u_value *args);

u_value w_f_c_46_c_file_1(u_value *env, u_value *args);

u_value w_f_c_46_c_call_2(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_funs_1(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_constrs_1(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_arities_1(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_fields_1(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_procs_1(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_funs_58__61__2(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_constrs_58__61__2(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_arities_58__61__2(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_fields_58__61__2(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_procs_58__61__2(u_value *env, u_value *args);

u_value w_f_compile_46_Gen_46_count_1(u_value *env, u_value *args);

u_value w_f_compile_46_Gen_46_lifted_1(u_value *env, u_value *args);

u_value w_f_compile_46_Gen_46_count_58__61__2(u_value *env, u_value *args);

u_value w_f_compile_46_Gen_46_lifted_58__61__2(u_value *env, u_value *args);

u_value w_f_compile_46_value_type_0(u_value *env, u_value *args);

u_value w_f_compile_46_value_ptr_0(u_value *env, u_value *args);

u_value w_f_compile_46_mangle_1(u_value *env, u_value *args);

u_value w_f_compile_46_mangle_char_1(u_value *env, u_value *args);

u_value w_f_compile_46_fun_name_2(u_value *env, u_value *args);

u_value w_f_compile_46_proc_name_1(u_value *env, u_value *args);

u_value w_f_compile_46_var_name_1(u_value *env, u_value *args);

u_value w_f_compile_46_ref_name_1(u_value *env, u_value *args);

u_value w_f_compile_46_builtin_functions_0(u_value *env, u_value *args);

u_value w_f_compile_46_descriptor_1(u_value *env, u_value *args);

u_value w_f_compile_46_unit_value_1(u_value *env, u_value *args);

u_value w_f_compile_46_runtime_constructors_0(u_value *env, u_value *args);

u_value w_f_compile_46_descriptor_decl_2(u_value *env, u_value *args);

u_value w_f_compile_46_unit_decl_1(u_value *env, u_value *args);

u_value w_f_compile_46_descriptor_decls_1(u_value *env, u_value *args);

u_value w_f_compile_46_constructor_decls_2(u_value *env, u_value *args);

u_value w_f_compile_46_in_scope_1(u_value *env, u_value *args);

u_value w_f_compile_46_pointer_1(u_value *env, u_value *args);

u_value w_f_compile_46_var_expr_2(u_value *env, u_value *args);

u_value w_f_compile_46_address_1(u_value *env, u_value *args);

u_value w_f_compile_46_type_key_1(u_value *env, u_value *args);

u_value w_f_compile_46_globals_1(u_value *env, u_value *args);

u_value w_f_compile_46_proc_arities_1(u_value *env, u_value *args);

u_value w_f_compile_46_arity_map_2(u_value *env, u_value *args);

u_value w_f_compile_46_is_global_fun_3(u_value *env, u_value *args);

u_value w_f_compile_46_fields_count_2(u_value *env, u_value *args);

u_value w_f_compile_46_fun_arities_1(u_value *env, u_value *args);

u_value w_f_compile_46_constr_arities_1(u_value *env, u_value *args);

u_value w_f_compile_46_constr_arity_1(u_value *env, u_value *args);

u_value w_f_compile_46_numbered_2(u_value *env, u_value *args);

u_value w_f_compile_46_index_2(u_value *env, u_value *args);

u_value w_f_compile_46_field_2(u_value *env, u_value *args);

u_value w_f_compile_46_truth_1(u_value *env, u_value *args);

u_value w_f_compile_46_conjunction_1(u_value *env, u_value *args);

u_value w_f_compile_46_literal_value_1(u_value *env, u_value *args);

u_value w_f_compile_46_pattern_tests_2(u_value *env, u_value *args);

u_value w_f_compile_46_field_tests_2(u_value *env, u_value *args);

u_value w_f_compile_46_pattern_binds_2(u_value *env, u_value *args);

u_value w_f_compile_46_field_binds_2(u_value *env, u_value *args);

u_value w_f_compile_46_pattern_names_1(u_value *env, u_value *args);

u_value w_f_compile_46_free_vars_2(u_value *env, u_value *args);

u_value w_f_compile_46_free_in_stmt_2(u_value *env, u_value *args);

u_value w_f_compile_46_free_in_stmt_branch_2(u_value *env, u_value *args);

u_value w_f_compile_46_free_in_handler_2(u_value *env, u_value *args);

u_value w_f_compile_46_free_in_branch_2(u_value *env, u_value *args);

u_value w_f_compile_46_lambdas_2(u_value *env, u_value *args);

u_value w_f_compile_46_global_value_3(u_value *env, u_value *args);

u_value w_f_compile_46_rebind_2(u_value *env, u_value *args);

u_value w_f_compile_46_proc_params_0(u_value *env, u_value *args);

u_value w_f_compile_46_unpack_pointers_1(u_value *env, u_value *args);

u_value w_f_compile_46_unpack_2(u_value *env, u_value *args);

u_value w_f_compile_46_reassign_3(u_value *env, u_value *args);

u_value w_f_compile_46_cached_3(u_value *env, u_value *args);

u_value w_f_compile_46_wrappers_1(u_value *env, u_value *args);

u_value w_f_compile_46_proc_wrapper_1(u_value *env, u_value *args);

u_value w_f_compile_46_fun_wrapper_1(u_value *env, u_value *args);

u_value w_f_compile_46_builtin_wrapper_1(u_value *env, u_value *args);

u_value w_f_compile_46_wrapper_3(u_value *env, u_value *args);

u_value w_f_compile_46_constr_wrapper_1(u_value *env, u_value *args);

u_value w_f_compile_46_range_to_1(u_value *env, u_value *args);

u_value w_f_compile_46_prototype_1(u_value *env, u_value *args);

u_value w_f_compile_46_is_entry_2(u_value *env, u_value *args);

u_value w_f_ulang_46_usage_0(u_value *env, u_value *args);

u_value w_f_ulang_46_before_dashes_1(u_value *env, u_value *args);

u_value w_f_ulang_46_after_dashes_1(u_value *env, u_value *args);

u_value w_f_ulang_46_in_module_1(u_value *env, u_value *args);

u_value w_f_ulang_46_module_names_1(u_value *env, u_value *args);

u_value w_f_ulang_46_is_any_test_1(u_value *env, u_value *args);

u_value w_f_ulang_46_is_selected_test_2(u_value *env, u_value *args);

u_value w_f_ulang_46_is_test_2(u_value *env, u_value *args);

u_value w_f_ulang_46_cmd_name_1(u_value *env, u_value *args);

u_value w_f_ulang_46_type_message_1(u_value *env, u_value *args);

u_value w_f_ulang_46_field_message_1(u_value *env, u_value *args);

u_value w_f_ulang_46_without_update_1(u_value *env, u_value *args);

u_value w_f_ulang_46_runtime_message_1(u_value *env, u_value *args);

u_value wc_prelude_46_NoIndex(u_value *env, u_value *args);

u_value wc_prelude_46_NoCharacter(u_value *env, u_value *args);

u_value wc_prelude_46_MissingField(u_value *env, u_value *args);

u_value wc_nat_46_Succ(u_value *env, u_value *args);

u_value wc_option_46_Some(u_value *env, u_value *args);

u_value wc_list_46_Cons(u_value *env, u_value *args);

u_value wc_syntax_46_Name(u_value *env, u_value *args);

u_value wc_syntax_46_TMeta(u_value *env, u_value *args);

u_value wc_syntax_46_TParam(u_value *env, u_value *args);

u_value wc_syntax_46_TSort(u_value *env, u_value *args);

u_value wc_syntax_46_TFun(u_value *env, u_value *args);

u_value wc_syntax_46_TProc(u_value *env, u_value *args);

u_value wc_syntax_46_TTuple(u_value *env, u_value *args);

u_value wc_syntax_46_VBool(u_value *env, u_value *args);

u_value wc_syntax_46_VInt(u_value *env, u_value *args);

u_value wc_syntax_46_VStr(u_value *env, u_value *args);

u_value wc_syntax_46_VChar(u_value *env, u_value *args);

u_value wc_syntax_46_Var(u_value *env, u_value *args);

u_value wc_syntax_46_ELit(u_value *env, u_value *args);

u_value wc_syntax_46_EVar(u_value *env, u_value *args);

u_value wc_syntax_46_EApp(u_value *env, u_value *args);

u_value wc_syntax_46_ELam(u_value *env, u_value *args);

u_value wc_syntax_46_EProc(u_value *env, u_value *args);

u_value wc_syntax_46_ELet(u_value *env, u_value *args);

u_value wc_syntax_46_EIte(u_value *env, u_value *args);

u_value wc_syntax_46_EMatch(u_value *env, u_value *args);

u_value wc_syntax_46_EComp(u_value *env, u_value *args);

u_value wc_syntax_46_ETuple(u_value *env, u_value *args);

u_value wc_syntax_46_ERaise(u_value *env, u_value *args);

u_value wc_syntax_46_PLit(u_value *env, u_value *args);

u_value wc_syntax_46_PVar(u_value *env, u_value *args);

u_value wc_syntax_46_PApp(u_value *env, u_value *args);

u_value wc_syntax_46_PTuple(u_value *env, u_value *args);

u_value wc_syntax_46_Branch(u_value *env, u_value *args);

u_value wc_syntax_46_SAssign(u_value *env, u_value *args);

u_value wc_syntax_46_SBlock(u_value *env, u_value *args);

u_value wc_syntax_46_SIf(u_value *env, u_value *args);

u_value wc_syntax_46_SWhile(u_value *env, u_value *args);

u_value wc_syntax_46_SCall(u_value *env, u_value *args);

u_value wc_syntax_46_SMatch(u_value *env, u_value *args);

u_value wc_syntax_46_SAssert(u_value *env, u_value *args);

u_value wc_syntax_46_SRaise(u_value *env, u_value *args);

u_value wc_syntax_46_STry(u_value *env, u_value *args);

u_value wc_syntax_46_Handler(u_value *env, u_value *args);

u_value wc_syntax_46_Constr(u_value *env, u_value *args);

u_value wc_syntax_46_CSort(u_value *env, u_value *args);

u_value wc_syntax_46_CData(u_value *env, u_value *args);

u_value wc_syntax_46_CFun(u_value *env, u_value *args);

u_value wc_syntax_46_CProc(u_value *env, u_value *args);

u_value wc_syntax_46_CModule(u_value *env, u_value *args);

u_value wc_syntax_46_CImport(u_value *env, u_value *args);

u_value wc_syntax_46_CNotation(u_value *env, u_value *args);

u_value wc_syntax_46_CTest(u_value *env, u_value *args);

u_value wc_syntax_46_Program(u_value *env, u_value *args);

u_value wc_os_46_OsError(u_value *env, u_value *args);

u_value wc_lex_46_TName(u_value *env, u_value *args);

u_value wc_lex_46_TSym(u_value *env, u_value *args);

u_value wc_lex_46_TPunct(u_value *env, u_value *args);

u_value wc_lex_46_TNum(u_value *env, u_value *args);

u_value wc_lex_46_TStr(u_value *env, u_value *args);

u_value wc_lex_46_TChar(u_value *env, u_value *args);

u_value wc_lex_46_LexError(u_value *env, u_value *args);

u_value wc_set_46_SNode(u_value *env, u_value *args);

u_value wc_parse_46_ParseError(u_value *env, u_value *args);

u_value wc_parse_46_Notations(u_value *env, u_value *args);

u_value wc_parse_46_Scope(u_value *env, u_value *args);

u_value wc_parse_46_Operand(u_value *env, u_value *args);

u_value wc_parse_46_Operator(u_value *env, u_value *args);

u_value wc_parse_46_Ops(u_value *env, u_value *args);

u_value wc_parse_46_Context(u_value *env, u_value *args);

u_value wc_map_46_MNode(u_value *env, u_value *args);

u_value wc_ctx_46_Ctx(u_value *env, u_value *args);

u_value wc_check_46_State(u_value *env, u_value *args);

u_value wc_check_46_UnknownSort(u_value *env, u_value *args);

u_value wc_check_46_SortArity(u_value *env, u_value *args);

u_value wc_check_46_SortDefined(u_value *env, u_value *args);

u_value wc_check_46_MissingType(u_value *env, u_value *args);

u_value wc_check_46_UndeclaredParams(u_value *env, u_value *args);

u_value wc_check_46_CannotUnify(u_value *env, u_value *args);

u_value wc_check_46_InfiniteType(u_value *env, u_value *args);

u_value wc_check_46_UnknownVariable(u_value *env, u_value *args);

u_value wc_check_46_OverloadedUnapplied(u_value *env, u_value *args);

u_value wc_check_46_WrongArity(u_value *env, u_value *args);

u_value wc_check_46_AlreadyDefined(u_value *env, u_value *args);

u_value wc_check_46_Duplicate(u_value *env, u_value *args);

u_value wc_check_46_Ambiguous(u_value *env, u_value *args);

u_value wc_check_46_UnknownField(u_value *env, u_value *args);

u_value wc_check_46_UnknownFieldType(u_value *env, u_value *args);

u_value wc_check_46_CountMismatch(u_value *env, u_value *args);

u_value wc_check_46_UnknownConstructor(u_value *env, u_value *args);

u_value wc_check_46_NotExhaustive(u_value *env, u_value *args);

u_value wc_check_46_Unreachable(u_value *env, u_value *args);

u_value wc_check_46_NotAssignable(u_value *env, u_value *args);

u_value wc_check_46_UnknownProcedure(u_value *env, u_value *args);

u_value wc_check_46_UnknownException(u_value *env, u_value *args);

u_value wc_check_46_TestParams(u_value *env, u_value *args);

u_value wc_check_46_HCon(u_value *env, u_value *args);

u_value wc_check_46_HLit(u_value *env, u_value *args);

u_value wc_check_46_Checked(u_value *env, u_value *args);

u_value wc_value_46_Lit(u_value *env, u_value *args);

u_value wc_value_46_Obj(u_value *env, u_value *args);

u_value wc_value_46_Clos(u_value *env, u_value *args);

u_value wc_value_46_Fn(u_value *env, u_value *args);

u_value wc_value_46_Con(u_value *env, u_value *args);

u_value wc_value_46_Prim(u_value *env, u_value *args);

u_value wc_value_46_Proc(u_value *env, u_value *args);

u_value wc_value_46_ProcClos(u_value *env, u_value *args);

u_value wc_run_46_ProcDef(u_value *env, u_value *args);

u_value wc_run_46_Prog(u_value *env, u_value *args);

u_value wc_run_46_Uninitialized(u_value *env, u_value *args);

u_value wc_run_46_NotFunction(u_value *env, u_value *args);

u_value wc_run_46_Unspecified(u_value *env, u_value *args);

u_value wc_run_46_NoCase(u_value *env, u_value *args);

u_value wc_run_46_NotList(u_value *env, u_value *args);

u_value wc_run_46_AssertionUnequal(u_value *env, u_value *args);

u_value wc_run_46_UnassignedOutput(u_value *env, u_value *args);

u_value wc_run_46_UnknownProc(u_value *env, u_value *args);

u_value wc_run_46_NotProc(u_value *env, u_value *args);

u_value wc_run_46_RecursiveInitialization(u_value *env, u_value *args);

u_value wc_run_46_Thrown(u_value *env, u_value *args);

u_value wc_c_46_CtNamed(u_value *env, u_value *args);

u_value wc_c_46_CtPtr(u_value *env, u_value *args);

u_value wc_c_46_CeVar(u_value *env, u_value *args);

u_value wc_c_46_CeNum(u_value *env, u_value *args);

u_value wc_c_46_CeStr(u_value *env, u_value *args);

u_value wc_c_46_CeCall(u_value *env, u_value *args);

u_value wc_c_46_CeUnary(u_value *env, u_value *args);

u_value wc_c_46_CeBinary(u_value *env, u_value *args);

u_value wc_c_46_CeIndex(u_value *env, u_value *args);

u_value wc_c_46_CeArrow(u_value *env, u_value *args);

u_value wc_c_46_CeCast(u_value *env, u_value *args);

u_value wc_c_46_CeCond(u_value *env, u_value *args);

u_value wc_c_46_CeInit(u_value *env, u_value *args);

u_value wc_c_46_CsExpr(u_value *env, u_value *args);

u_value wc_c_46_CsDecl(u_value *env, u_value *args);

u_value wc_c_46_CsAssign(u_value *env, u_value *args);

u_value wc_c_46_CsIf(u_value *env, u_value *args);

u_value wc_c_46_CsWhile(u_value *env, u_value *args);

u_value wc_c_46_CsReturn(u_value *env, u_value *args);

u_value wc_c_46_CsBlock(u_value *env, u_value *args);

u_value wc_c_46_CdInclude(u_value *env, u_value *args);

u_value wc_c_46_CdPrototype(u_value *env, u_value *args);

u_value wc_c_46_CdFunction(u_value *env, u_value *args);

u_value wc_c_46_CdGlobal(u_value *env, u_value *args);

u_value wc_c_46_CFile(u_value *env, u_value *args);

u_value wc_compile_46_Unsupported(u_value *env, u_value *args);

u_value wc_compile_46_UnknownEntry(u_value *env, u_value *args);

u_value wc_compile_46_Globals(u_value *env, u_value *args);

u_value wc_compile_46_Gen(u_value *env, u_value *args);

u_value wu_mod(u_value *env, u_value *args);

u_value wu_mul(u_value *env, u_value *args);

u_value wu_add(u_value *env, u_value *args);

u_value wu_sub(u_value *env, u_value *args);

u_value wu_div(u_value *env, u_value *args);

u_value wu_lt(u_value *env, u_value *args);

u_value wu_eq(u_value *env, u_value *args);

u_value wu_concat(u_value *env, u_value *args);

u_value wu_chars(u_value *env, u_value *args);

u_value wu_chr(u_value *env, u_value *args);

u_value wu_compare(u_value *env, u_value *args);

u_value wu_is_digit(u_value *env, u_value *args);

u_value wu_is_letter(u_value *env, u_value *args);

u_value wu_is_space(u_value *env, u_value *args);

u_value wu_ord(u_value *env, u_value *args);

u_value wu_str_compare(u_value *env, u_value *args);

u_value wu_str_contains(u_value *env, u_value *args);

u_value wu_str_get(u_value *env, u_value *args);

u_value wu_str_join(u_value *env, u_value *args);

u_value wu_str_number(u_value *env, u_value *args);

u_value wu_str_quote(u_value *env, u_value *args);

u_value wu_str_show(u_value *env, u_value *args);

u_value wu_str_size(u_value *env, u_value *args);

u_value wu_str_slice(u_value *env, u_value *args);

u_value wu_str_split(u_value *env, u_value *args);

u_value wu_string(u_value *env, u_value *args);

void wp_check_46_application(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_apply(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_arguments(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_bind_meta(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_binder_list(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_binders(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_call_declared(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_call_proc(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_call_variable(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_check(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_check_modules(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_check_program(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_check_unit(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_check_units(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_checks(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_closed(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_command(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_commands(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_coverage(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_declare_constrs(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_declare_constructors(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_declare_datas(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_declare_funs(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_declare_name(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_declare_procs(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_declare_sort(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_declare_sorts(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_expr_branches(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_field_access(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_formals(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_fresh(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_fresh_list(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_generators(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_guard(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_handler_list(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_infer(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_infers(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_instance(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_instantiate(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_known(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_known_vars(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_knowns(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_normalize(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_normalize_sort(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_normalize_vars(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_normalizes(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_overload(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_pattern(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_reachable(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_resolve_name(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_same_length(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_signature(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_signatures(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_statement(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_statements(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_stmt_branches(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_type_of(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_unifies(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_unify(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_unique(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_unique_ids(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_untyped(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_use(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_variable(u_value *env, u_value *ins, u_value **outs);

void wp_check_46_variables(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_compile_cmd(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_compile_cmds(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_compile_expr(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_compile_exprs(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_compile_file(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_compile_lambda(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_compile_loops(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_compile_named(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_compile_proc_lambda(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_compile_program(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_compile_stmt(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_compile_stmts(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_compile_with(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_expr_cases(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_gen_name(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_handler_cases(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_lift(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_stmt_cases(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_temps(u_value *env, u_value *ins, u_value **outs);

void wp_lex_46_lex(u_value *env, u_value *ins, u_value **outs);

void wp_lex_46_lex_char(u_value *env, u_value *ins, u_value **outs);

void wp_lex_46_lex_literal(u_value *env, u_value *ins, u_value **outs);

void wp_lex_46_lex_name(u_value *env, u_value *ins, u_value **outs);

void wp_lex_46_lex_number(u_value *env, u_value *ins, u_value **outs);

void wp_lex_46_lex_string(u_value *env, u_value *ins, u_value **outs);

void wp_lex_46_lex_symbol(u_value *env, u_value *ins, u_value **outs);

void wp_lex_46_lex_token(u_value *env, u_value *ins, u_value **outs);

void wp_lex_46_position(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_advance(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_climb(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_climb_loop(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_constructor(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_declare_all(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_expect(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_expr_items(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_expr_list(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_field(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_keyword(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_lambda_function(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_library_text(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_load_module(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_map_comprehension(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_map_generator(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_module_after(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_module_text(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_nonassociative(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_operand(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_operators(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_app(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_assign(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_atom(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_body(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_braces(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_brackets(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_call(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_command(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_commands(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_data(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_expr(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_expr_cases(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_exprs(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_failures(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_file(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_file_after(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_formal(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_formals(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_function(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_guard(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_if(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_lambda(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_let(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_library(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_local(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_name(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_name_or_symbol(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_notation(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_pattern(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_plain(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_procedure(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_stmt(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_stmt_cases(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_text(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_type(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_update(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_parse_variable(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_pattern_items(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_pattern_list(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_pattern_simple(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_resolve_items(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_same_commands(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_same_parses(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_section_items(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_sections(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_set_comprehension(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_skip_comma(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_type_basic(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_type_items(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_type_list(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_type_params(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_apply_expr(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_apply_value(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_cached(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_check_and_run(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_combinations(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_compare_all(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_compare_values(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_comprehension(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_equal(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_equal_all(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_evaluate(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_evaluate_all(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_exec(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_exec_all(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_extend(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_handle(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_holds(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_invoke(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_invoke_value(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_os_proc(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_primitive(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_print_all(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_run_all(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_run_body(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_run_cmd(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_run_proc(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_run_program(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_scoped(u_value *env, u_value *ins, u_value **outs);

void wp_run_46_select(u_value *env, u_value *ins, u_value **outs);

void wp_ulang_46_attempt(u_value *env, u_value *ins, u_value **outs);

void wp_ulang_46_check_file(u_value *env, u_value *ins, u_value **outs);

void wp_ulang_46_compile_to(u_value *env, u_value *ins, u_value **outs);

void wp_ulang_46_load_file(u_value *env, u_value *ins, u_value **outs);

void wp_ulang_46_located(u_value *env, u_value *ins, u_value **outs);

void wp_ulang_46_main(u_value *env, u_value *ins, u_value **outs);

void wp_ulang_46_run_file(u_value *env, u_value *ins, u_value **outs);

void wp_ulang_46_run_test(u_value *env, u_value *ins, u_value **outs);

void wp_ulang_46_run_tests(u_value *env, u_value *ins, u_value **outs);

void wp_ulang_46_test_file(u_value *env, u_value *ins, u_value **outs);

u_value f_prelude_46_not_1(u_value v_b);

u_value f_prelude_46__61__61__62__2(u_value v_a, u_value v_b);

u_value f_prelude_46__33__61__2(u_value v_x, u_value v_y);

u_value f_prelude_46__60__61__2(u_value v_m, u_value v_n);

u_value f_prelude_46__62__2(u_value v_m, u_value v_n);

u_value f_prelude_46__62__61__2(u_value v_m, u_value v_n);

u_value f_prelude_46__45__1(u_value v_n);

u_value f_option_46_or_else_2(u_value v_a, u_value v_b);

u_value f_list_46__43__43__2(u_value v_xs, u_value v_ys);

u_value f_list_46_length_1(u_value v_xs);

u_value f_list_46_lookup_2(u_value v_k, u_value v_xs);

u_value f_list_46_update_3(u_value v_k, u_value v_v, u_value v_xs);

u_value f_list_46_replaced_3(u_value v_k, u_value v_v, u_value v_xs);

u_value f_list_46_remove_2(u_value v_k, u_value v_xs);

u_value f_list_46_has_key_2(u_value v_k, u_value v_xs);

u_value f_list_46_keys_1(u_value v_xs);

u_value f_list_46_values_1(u_value v_xs);

u_value f_list_46_contains_2(u_value v_x, u_value v_xs);

u_value f_list_46_exists_2(u_value v_xs, u_value v_p);

u_value f_list_46_forall_2(u_value v_xs, u_value v_p);

u_value f_list_46_distinct_1(u_value v_xs);

u_value f_list_46_repeated_1(u_value v_xs);

u_value f_list_46_diff_2(u_value v_xs, u_value v_ys);

u_value f_list_46_concat_1(u_value v_xss);

u_value f_list_46_reverse_onto_2(u_value v_xs, u_value v_ys);

u_value f_list_46_reverse_1(u_value v_xs);

u_value f_list_46_zip_2(u_value v_xs, u_value v_ys);

u_value f_list_46_take_2(u_value v_n, u_value v_xs);

u_value f_list_46_drop_2(u_value v_n, u_value v_xs);

u_value f_list_46_replicate_2(u_value v_n, u_value v_x);

u_value f_list_46_firsts_1(u_value v_ps);

u_value f_list_46_seconds_1(u_value v_ps);

u_value f_syntax_46_Name_46_module_1(u_value v_x);

u_value f_syntax_46_Name_46_base_1(u_value v_x);

u_value f_syntax_46_Name_46_module_58__61__2(u_value v_x, u_value v_y);

u_value f_syntax_46_Name_46_base_58__61__2(u_value v_x, u_value v_y);

u_value f_syntax_46_Var_46_name_1(u_value v_x);

u_value f_syntax_46_Var_46_typ_1(u_value v_x);

u_value f_syntax_46_Var_46_name_58__61__2(u_value v_x, u_value v_y);

u_value f_syntax_46_Var_46_typ_58__61__2(u_value v_x, u_value v_y);

u_value f_syntax_46_Branch_46_pat_1(u_value v_x);

u_value f_syntax_46_Branch_46_guard_1(u_value v_x);

u_value f_syntax_46_Branch_46_body_1(u_value v_x);

u_value f_syntax_46_Branch_46_pat_58__61__2(u_value v_x, u_value v_y);

u_value f_syntax_46_Branch_46_guard_58__61__2(u_value v_x, u_value v_y);

u_value f_syntax_46_Branch_46_body_58__61__2(u_value v_x, u_value v_y);

u_value f_syntax_46_Handler_46_typ_1(u_value v_x);

u_value f_syntax_46_Handler_46_pat_1(u_value v_x);

u_value f_syntax_46_Handler_46_guard_1(u_value v_x);

u_value f_syntax_46_Handler_46_body_1(u_value v_x);

u_value f_syntax_46_Handler_46_typ_58__61__2(u_value v_x, u_value v_y);

u_value f_syntax_46_Handler_46_pat_58__61__2(u_value v_x, u_value v_y);

u_value f_syntax_46_Handler_46_guard_58__61__2(u_value v_x, u_value v_y);

u_value f_syntax_46_Handler_46_body_58__61__2(u_value v_x, u_value v_y);

u_value f_syntax_46_Constr_46_name_1(u_value v_x);

u_value f_syntax_46_Constr_46_args_1(u_value v_x);

u_value f_syntax_46_Constr_46_name_58__61__2(u_value v_x, u_value v_y);

u_value f_syntax_46_Constr_46_args_58__61__2(u_value v_x, u_value v_y);

u_value f_syntax_46_Program_46_cmds_1(u_value v_x);

u_value f_syntax_46_Program_46_cmds_58__61__2(u_value v_x, u_value v_y);

u_value f_syntax_46_unqualified_1(u_value v_x);

u_value f_syntax_46_qualified_1(u_value v_n);

u_value f_syntax_46_list_name_0(void);

u_value f_syntax_46_nil_name_0(void);

u_value f_syntax_46_cons_name_0(void);

u_value f_syntax_46_option_name_0(void);

u_value f_syntax_46_none_name_0(void);

u_value f_syntax_46_some_name_0(void);

u_value f_syntax_46_os_error_name_0(void);

u_value f_syntax_46_division_by_zero_name_0(void);

u_value f_syntax_46_no_index_name_0(void);

u_value f_syntax_46_no_character_name_0(void);

u_value f_syntax_46_missing_field_name_0(void);

u_value f_syntax_46_tuple_name_0(void);

u_value f_syntax_46_names_1(u_value v_xs);

u_value f_syntax_46_types_1(u_value v_xs);

u_value f_syntax_46_arity_1(u_value v_t);

u_value f_lex_46_is_symbol_1(u_value v_c);

u_value f_lex_46_is_punct_1(u_value v_c);

u_value f_lex_46_is_name_char_1(u_value v_c);

u_value f_lex_46_peek_2(u_value v_s, u_value v_i);

u_value f_lex_46_unescape_1(u_value v_c);

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p);

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column);

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i);

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i);

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i);

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i);

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i);

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i);

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i);

void p_lex_46_lex(u_value v_s, u_value *r_ts);

u_value f_lex_46_tokens_1(u_value v_ts);

u_value f_set_46_empty_set_0(void);

u_value f_set_46_member_2(u_value v_x, u_value v_s);

u_value f_set_46_insert_2(u_value v_x, u_value v_s);

u_value f_set_46_from_list_1(u_value v_xs);

u_value f_set_46_insert_all_2(u_value v_xs, u_value v_s);

u_value f_set_46_elements_1(u_value v_s);

u_value f_set_46_elements_onto_2(u_value v_s, u_value v_rest);

u_value f_set_46_count_1(u_value v_s);

u_value f_set_46_depth_1(u_value v_s);

u_value f_set_46_snode_depth_2(u_value v_l, u_value v_r);

u_value f_set_46_snode_3(u_value v_l, u_value v_x, u_value v_r);

u_value f_set_46_turn_left_1(u_value v_s);

u_value f_set_46_turn_right_1(u_value v_s);

u_value f_set_46_smaller_1(u_value v_s);

u_value f_set_46_greater_1(u_value v_s);

u_value f_set_46_rebalance_3(u_value v_l, u_value v_x, u_value v_r);

u_value f_set_46_well_formed_1(u_value v_s);

u_value f_set_46_upto_1(u_value v_n);

u_value f_parse_46_Notations_46_cmds_1(u_value v_x);

u_value f_parse_46_Notations_46_ops_1(u_value v_x);

u_value f_parse_46_Notations_46_cmds_58__61__2(u_value v_x, u_value v_y);

u_value f_parse_46_Notations_46_ops_58__61__2(u_value v_x, u_value v_y);

u_value f_parse_46_Scope_46_params_1(u_value v_x);

u_value f_parse_46_Scope_46_notations_1(u_value v_x);

u_value f_parse_46_Scope_46_params_58__61__2(u_value v_x, u_value v_y);

u_value f_parse_46_Scope_46_notations_58__61__2(u_value v_x, u_value v_y);

u_value f_parse_46_keywords_0(void);

u_value f_parse_46_is_keyword_1(u_value v_n);

u_value f_parse_46_is_reserved_1(u_value v_s);

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op);

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op);

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op);

u_value f_parse_46_declared_2(u_value v_ns, u_value v_op);

u_value f_parse_46_no_notations_0(void);

u_value f_parse_46_with_notations_2(u_value v_ns, u_value v_cs);

u_value f_parse_46_operators_of_1(u_value v_cs);

u_value f_parse_46_head_token_1(u_value v_ts);

u_value f_parse_46_second_token_1(u_value v_ts);

u_value f_parse_46_head_offset_1(u_value v_ts);

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w);

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c);

u_value f_parse_46_describe_1(u_value v_t);

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts);

void p_parse_46_advance(u_value *r_ts);

void p_parse_46_expect(u_value v_t, u_value *r_ts);

void p_parse_46_keyword(u_value v_w, u_value *r_ts);

void p_parse_46_parse_name(u_value *r_n, u_value *r_ts);

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts);

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts);

u_value f_parse_46_Ops_46_notations_1(u_value v_x);

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x);

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x);

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x);

u_value f_parse_46_Ops_46_notations_58__61__2(u_value v_x, u_value v_y);

u_value f_parse_46_Ops_46_prefix_op_58__61__2(u_value v_x, u_value v_y);

u_value f_parse_46_Ops_46_postfix_op_58__61__2(u_value v_x, u_value v_y);

u_value f_parse_46_Ops_46_infix_op_58__61__2(u_value v_x, u_value v_y);

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a);

void p_parse_46_climb(u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest);

void p_parse_46_operand(u_value v_items, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest);

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest);

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns);

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p);

u_value f_parse_46_type_one_1(u_value v_ts);

u_value f_parse_46_type_components_1(u_value v_ts);

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t);

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r);

u_value f_parse_46_has_invalid_1(u_value v_t);

u_value f_parse_46_any_invalid_1(u_value v_ts);

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t);

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts);

void p_parse_46_type_items(u_value v_sc, u_value *r_items, u_value *r_ts);

void p_parse_46_type_basic(u_value v_sc, u_value *r_r, u_value *r_ts);

void p_parse_46_type_list(u_value v_sc, u_value v_close, u_value *r_r, u_value *r_ts);

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a);

u_value f_parse_46_expr_prefix_2(u_value v_op, u_value v_a);

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b);

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest);

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t);

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts);

void p_parse_46_parse_update(u_value v_sc, u_value *r_e, u_value *r_ts);

u_value f_parse_46_at_update_1(u_value v_ts);

void p_parse_46_operators(u_value v_sc, u_value *r_e, u_value *r_ts);

void p_parse_46_expr_items(u_value v_sc, u_value *r_items, u_value *r_ts);

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts);

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts);

void p_parse_46_parse_app(u_value v_sc, u_value *r_e, u_value *r_ts);

void p_parse_46_parse_atom(u_value v_sc, u_value *r_e, u_value *r_ts);

u_value f_parse_46_pattern_vars_1(u_value v_ps);

void p_parse_46_parse_let(u_value v_sc, u_value *r_e, u_value *r_ts);

void p_parse_46_parse_lambda(u_value v_sc, u_value *r_e, u_value *r_ts);

u_value f_parse_46_at_section_1(u_value v_ts);

void p_parse_46_lambda_function(u_value v_sc, u_value *r_e, u_value *r_ts);

void p_parse_46_parse_braces(u_value v_sc, u_value *r_e, u_value *r_ts);

u_value f_parse_46_nil_expr_0(void);

u_value f_parse_46_in_library_3(u_value v_m, u_value v_f, u_value v_args);

void p_parse_46_set_comprehension(u_value v_sc, u_value v_r, u_value *r_e, u_value *r_ts);

void p_parse_46_map_comprehension(u_value v_sc, u_value v_k, u_value v_v, u_value *r_e, u_value *r_ts);

void p_parse_46_map_generator(u_value v_sc, u_value *r_gs, u_value *r_ls, u_value *r_ts);

u_value f_parse_46_hidden_vars_1(u_value v_gs);

u_value f_parse_46_unpack_2(u_value v_gs, u_value v_e);

void p_parse_46_parse_brackets(u_value v_sc, u_value *r_e, u_value *r_ts);

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts);

void p_parse_46_parse_expr_cases(u_value v_sc, u_value *r_cs, u_value *r_ts);

u_value f_parse_46_pat_op1_2(u_value v_op, u_value v_p);

u_value f_parse_46_pat_prefix_2(u_value v_op, u_value v_p);

u_value f_parse_46_pat_op2_3(u_value v_op, u_value v_l, u_value v_r);

u_value f_parse_46_cons_pats_2(u_value v_ps, u_value v_rest);

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t);

void p_parse_46_parse_pattern(u_value v_sc, u_value *r_p, u_value *r_ts);

void p_parse_46_pattern_items(u_value v_sc, u_value *r_items, u_value *r_ts);

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts);

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts);

u_value f_parse_46_starts_stmt_1(u_value v_t);

u_value f_parse_46_handlers_1(u_value v_cs);

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts);

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts);

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts);

void p_parse_46_parse_if(u_value v_sc, u_value *r_s, u_value *r_ts);

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts);

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts);

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first);

u_value f_parse_46_item_follows_1(u_value v_ts);

void p_parse_46_parse_call(u_value v_sc, u_value *r_s, u_value *r_ts);

void p_parse_46_sections(u_value v_item_in, u_value v_item, u_value v_sc, u_value *r_ins, u_value *r_outs, u_value *r_refs, u_value *r_ts);

void p_parse_46_section_items(u_value v_item, u_value v_sc, u_value *r_xs, u_value *r_ts);

void p_parse_46_skip_comma(u_value v_first, u_value *r_ts);

void p_parse_46_parse_variable(u_value v_sc, u_value *r_x, u_value *r_ts);

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts);

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts);

u_value f_parse_46_declarations_1(u_value v_cs);

u_value f_parse_46_notations_of_1(u_value v_cs);

u_value f_parse_46_is_notation_1(u_value v_c);

u_value f_parse_46_Context_46_notations_1(u_value v_x);

u_value f_parse_46_Context_46_modules_1(u_value v_x);

u_value f_parse_46_Context_46_loading_1(u_value v_x);

u_value f_parse_46_Context_46_base_1(u_value v_x);

u_value f_parse_46_Context_46_dir_1(u_value v_x);

u_value f_parse_46_Context_46_notations_58__61__2(u_value v_x, u_value v_y);

u_value f_parse_46_Context_46_modules_58__61__2(u_value v_x, u_value v_y);

u_value f_parse_46_Context_46_loading_58__61__2(u_value v_x, u_value v_y);

u_value f_parse_46_Context_46_base_58__61__2(u_value v_x, u_value v_y);

u_value f_parse_46_Context_46_dir_58__61__2(u_value v_x, u_value v_y);

u_value f_parse_46_with_commands_2(u_value v_cx, u_value v_cs);

u_value f_parse_46_with_module_2(u_value v_cx, u_value v_c);

u_value f_parse_46_modules_of_1(u_value v_c);

u_value f_parse_46_own_notations_2(u_value v_cx, u_value v_n);

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts);

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts);

void p_parse_46_constructor(u_value v_sc, u_value *r_k, u_value *r_fs, u_value *r_ts);

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts);

void p_parse_46_parse_data(u_value v_ns, u_value *r_cs, u_value *r_ts);

u_value f_parse_46_field_names_1(u_value v_fs);

u_value f_parse_46_differing_1(u_value v_fs);

u_value f_parse_46_type_of_field_1(u_value v_e);

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs);

u_value f_parse_46_fields_of_1(u_value v_ks);

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i);

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks);

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n);

u_value f_parse_46_field_type_1(u_value v_es);

u_value f_parse_46_field_case_1(u_value v_e);

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f);

u_value f_parse_46_updaters_3(u_value v_t, u_value v_ps, u_value v_ks);

u_value f_parse_46_updater_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n);

u_value f_parse_46_update_case_1(u_value v_e);

u_value f_parse_46_positions_1(u_value v_n);

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n);

void p_parse_46_parse_function(u_value v_ns, u_value *r_c, u_value *r_ts);

void p_parse_46_parse_procedure(u_value v_ns, u_value *r_c, u_value *r_ts);

void p_parse_46_parse_command(u_value v_cx, u_value *r_cs, u_value *r_ts);

void p_parse_46_declare_all(u_value v_cs, u_value *r_cx);

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_cx, u_value *r_ts);

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text);

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text);

void p_parse_46_load_module(u_value v_n, u_value v_cx, u_value v_at, u_value *r_cs);

u_value f_parse_46_base_notations_1(u_value v_cx);

void p_parse_46_parse_text(u_value v_text, u_value v_cx, u_value *r_p);

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p);

void p_parse_46_parse_library(u_value v_n, u_value *r_p);

u_value f_parse_46_last_part_1(u_value v_xs);

u_value f_parse_46_base_name_1(u_value v_path);

u_value f_parse_46_dir_name_1(u_value v_path);

void p_parse_46_parse_file_after(u_value v_path, u_value v_pre, u_value *r_p);

void p_parse_46_parse_file(u_value v_path, u_value *r_p);

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds);

void p_parse_46_same_parses(u_value v_paths, u_value v_pre);

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre);

u_value f_map_46_empty_0(void);

u_value f_map_46_get_2(u_value v_k, u_value v_m);

u_value f_map_46_has_2(u_value v_k, u_value v_m);

u_value f_map_46_put_3(u_value v_k, u_value v_v, u_value v_m);

u_value f_map_46_entries_1(u_value v_m);

u_value f_map_46_entries_onto_2(u_value v_m, u_value v_rest);

u_value f_map_46_size_1(u_value v_m);

u_value f_map_46_height_1(u_value v_m);

u_value f_map_46_mnode_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r);

u_value f_map_46_rotate_left_1(u_value v_m);

u_value f_map_46_rotate_right_1(u_value v_m);

u_value f_map_46_left_of_1(u_value v_m);

u_value f_map_46_right_of_1(u_value v_m);

u_value f_map_46_balance_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r);

u_value f_map_46_balanced_1(u_value v_m);

u_value f_map_46_mnode_height_2(u_value v_l, u_value v_r);

u_value f_map_46_delete_2(u_value v_k, u_value v_m);

u_value f_map_46_delete_all_2(u_value v_ks, u_value v_m);

u_value f_map_46_join_2(u_value v_l, u_value v_r);

u_value f_map_46_first_entry_1(u_value v_m);

u_value f_map_46_remove_first_1(u_value v_m);

u_value f_map_46_put_all_2(u_value v_ps, u_value v_m);

u_value f_map_46_numbers_1(u_value v_n);

u_value f_ctx_46_Ctx_46_aliases_1(u_value v_x);

u_value f_ctx_46_Ctx_46_funs_1(u_value v_x);

u_value f_ctx_46_Ctx_46_procs_1(u_value v_x);

u_value f_ctx_46_Ctx_46_datas_1(u_value v_x);

u_value f_ctx_46_Ctx_46_members_1(u_value v_x);

u_value f_ctx_46_Ctx_46_constrs_1(u_value v_x);

u_value f_ctx_46_Ctx_46_named_1(u_value v_x);

u_value f_ctx_46_Ctx_46_aliases_58__61__2(u_value v_x, u_value v_y);

u_value f_ctx_46_Ctx_46_funs_58__61__2(u_value v_x, u_value v_y);

u_value f_ctx_46_Ctx_46_procs_58__61__2(u_value v_x, u_value v_y);

u_value f_ctx_46_Ctx_46_datas_58__61__2(u_value v_x, u_value v_y);

u_value f_ctx_46_Ctx_46_members_58__61__2(u_value v_x, u_value v_y);

u_value f_ctx_46_Ctx_46_constrs_58__61__2(u_value v_x, u_value v_y);

u_value f_ctx_46_Ctx_46_named_58__61__2(u_value v_x, u_value v_y);

u_value f_ctx_46_empty_ctx_0(void);

u_value f_ctx_46_ctx_of_2(u_value v_c, u_value v_ms);

u_value f_ctx_46_owned_map_2(u_value v_m, u_value v_ms);

u_value f_ctx_46_owned_2(u_value v_es, u_value v_ms);

u_value f_ctx_46_owned_named_2(u_value v_es, u_value v_ms);

u_value f_ctx_46_in_modules_2(u_value v_n, u_value v_ms);

u_value f_ctx_46_ctx_union_2(u_value v_a, u_value v_b);

u_value f_ctx_46_union_named_2(u_value v_es, u_value v_bs);

u_value f_ctx_46_alias_of_2(u_value v_c, u_value v_n);

u_value f_ctx_46_schemes_of_2(u_value v_c, u_value v_n);

u_value f_ctx_46_proc_of_2(u_value v_c, u_value v_n);

u_value f_ctx_46_data_of_2(u_value v_c, u_value v_n);

u_value f_ctx_46_members_of_2(u_value v_c, u_value v_n);

u_value f_ctx_46_constr_of_2(u_value v_c, u_value v_n);

u_value f_ctx_46_is_fun_2(u_value v_c, u_value v_n);

u_value f_ctx_46_is_proc_2(u_value v_c, u_value v_n);

u_value f_ctx_46_is_data_2(u_value v_c, u_value v_n);

u_value f_ctx_46_is_alias_2(u_value v_c, u_value v_n);

u_value f_ctx_46_is_constr_2(u_value v_c, u_value v_n);

u_value f_ctx_46_is_sort_2(u_value v_c, u_value v_n);

u_value f_ctx_46_names_of_2(u_value v_c, u_value v_b);

u_value f_ctx_46_with_named_2(u_value v_bs, u_value v_n);

u_value f_ctx_46_with_alias_4(u_value v_c, u_value v_n, u_value v_ps, u_value v_t);

u_value f_ctx_46_with_fun_3(u_value v_c, u_value v_n, u_value v_sc);

u_value f_ctx_46_with_proc_3(u_value v_c, u_value v_n, u_value v_g);

u_value f_ctx_46_with_data_3(u_value v_c, u_value v_n, u_value v_arity);

u_value f_ctx_46_with_members_3(u_value v_c, u_value v_n, u_value v_cs);

u_value f_ctx_46_with_constr_3(u_value v_c, u_value v_n, u_value v_kc);

u_value f_check_46_State_46_ctx_1(u_value v_x);

u_value f_check_46_State_46_sub_1(u_value v_x);

u_value f_check_46_State_46_count_1(u_value v_x);

u_value f_check_46_State_46_module_1(u_value v_x);

u_value f_check_46_State_46_visible_1(u_value v_x);

u_value f_check_46_State_46_table_1(u_value v_x);

u_value f_check_46_State_46_ctx_58__61__2(u_value v_x, u_value v_y);

u_value f_check_46_State_46_sub_58__61__2(u_value v_x, u_value v_y);

u_value f_check_46_State_46_count_58__61__2(u_value v_x, u_value v_y);

u_value f_check_46_State_46_module_58__61__2(u_value v_x, u_value v_y);

u_value f_check_46_State_46_visible_58__61__2(u_value v_x, u_value v_y);

u_value f_check_46_State_46_table_58__61__2(u_value v_x, u_value v_y);

u_value f_check_46_with_ctx_2(u_value v_s, u_value v_c);

u_value f_check_46_enter_2(u_value v_s, u_value v_m);

u_value f_check_46_sees_2(u_value v_m, u_value v_vs);

u_value f_check_46_start_2(u_value v_main, u_value v_vs);

u_value f_check_46_flatten_2(u_value v_m, u_value v_cs);

u_value f_check_46_in_module_2(u_value v_m, u_value v_n);

u_value f_check_46_qualify_2(u_value v_m, u_value v_c);

u_value f_check_46_qualify_constr_2(u_value v_m, u_value v_k);

u_value f_check_46_imports_in_2(u_value v_ds, u_value v_m);

u_value f_check_46_visibility_2(u_value v_imports, u_value v_main);

u_value f_check_46_imports_of_2(u_value v_imports, u_value v_m);

u_value f_check_46_visible_names_3(u_value v_s, u_value v_n, u_value v_declared);

u_value f_check_46_is_visible_2(u_value v_s, u_value v_d);

u_value f_check_46_tbool_0(void);

u_value f_check_46_tint_0(void);

u_value f_check_46_tstr_0(void);

u_value f_check_46_tchar_0(void);

u_value f_check_46_tlist_1(u_value v_t);

u_value f_check_46_fun_2(u_value v_args, u_value v_res);

u_value f_check_46_params_1(u_value v_ps);

u_value f_check_46_basic_1(u_value v_n);

u_value f_check_46_literal_1(u_value v_v);

u_value f_check_46_initial_0(void);

u_value f_check_46_arith_0(void);

u_value f_check_46_logic_0(void);

u_value f_check_46_builtin_1(u_value v_n);

u_value f_check_46_builtin_scheme_1(u_value v_b);

u_value f_check_46_is_meta_1(u_value v_t);

u_value f_check_46_free_1(u_value v_t);

u_value f_check_46_holes_1(u_value v_t);

u_value f_check_46_subst_2(u_value v_t, u_value v_m);

u_value f_check_46_resolve_2(u_value v_su, u_value v_t);

u_value f_check_46_occurs_3(u_value v_su, u_value v_i, u_value v_t);

u_value f_check_46_constructor_scheme_1(u_value v_k);

u_value f_check_46_constructor_of_2(u_value v_c, u_value v_k);

u_value f_check_46_candidates_3(u_value v_s, u_value v_env, u_value v_n);

u_value f_check_46_proc_scheme_2(u_value v_c, u_value v_p);

u_value f_check_46_with_arity_2(u_value v_cs, u_value v_k);

u_value f_check_46_has_arity_2(u_value v_scs, u_value v_k);

u_value f_check_46_bind_1(u_value v_xs);

u_value f_check_46_fixed_2(u_value v_su, u_value v_env);

u_value f_check_46_generalize_3(u_value v_su, u_value v_env, u_value v_t);

u_value f_check_46_generalized_3(u_value v_su, u_value v_env, u_value v_xs);

u_value f_check_46_case_of_1(u_value v_b);

u_value f_check_46_zonk_guard_2(u_value v_su, u_value v_g);

u_value f_check_46_zonk_type_2(u_value v_su, u_value v_t);

u_value f_check_46_zonk_var_2(u_value v_su, u_value v_x);

u_value f_check_46_zonk_vars_2(u_value v_su, u_value v_xs);

u_value f_check_46_zonk_expr_2(u_value v_su, u_value v_e);

u_value f_check_46_zonk_binding_2(u_value v_su, u_value v_p);

u_value f_check_46_zonk_expr_branch_2(u_value v_su, u_value v_b);

u_value f_check_46_zonk_pat_2(u_value v_su, u_value v_p);

u_value f_check_46_zonk_stmt_2(u_value v_su, u_value v_x);

u_value f_check_46_zonk_stmt_branch_2(u_value v_su, u_value v_b);

u_value f_check_46_zonk_handler_2(u_value v_su, u_value v_h);

u_value f_check_46_zonk_cmd_2(u_value v_su, u_value v_c);

u_value f_check_46_unknown_type_1(u_value v_t);

u_value f_check_46_unknown_in_1(u_value v_x);

u_value f_check_46_unknown_in_expr_1(u_value v_e);

u_value f_check_46_unknown_in_exprs_1(u_value v_es);

u_value f_check_46_branch_exprs_1(u_value v_b);

u_value f_check_46_unknown_in_all_1(u_value v_xs);

u_value f_check_46_unknown_in_branches_1(u_value v_bs);

u_value f_check_46_unknown_in_handlers_1(u_value v_hs);

u_value f_check_46_unknown_in_cmd_1(u_value v_c);

u_value f_check_46_head_of_1(u_value v_p);

u_value f_check_46_heads_1(u_value v_rows);

u_value f_check_46_field_types_3(u_value v_c, u_value v_k, u_value v_args);

u_value f_check_46_constructors_of_3(u_value v_c, u_value v_su, u_value v_t);

u_value f_check_46_fields_of_4(u_value v_c, u_value v_su, u_value v_t, u_value v_h);

u_value f_check_46_covers_2(u_value v_hs, u_value v_ks);

u_value f_check_46_defaults_1(u_value v_rows);

u_value f_check_46_specialize_3(u_value v_rows, u_value v_h, u_value v_n);

u_value f_check_46_useful_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_q, u_value v_ts);

u_value f_check_46_useful_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_ps, u_value v_rest);

u_value f_check_46_witness_2(u_value v_h, u_value v_args);

u_value f_check_46_missing_4(u_value v_c, u_value v_su, u_value v_rows, u_value v_ts);

u_value f_check_46_missing_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest);

u_value f_check_46_first_missing_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_rest);

u_value f_check_46_other_missing_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest);

u_value f_check_46_uncovered_2(u_value v_hs, u_value v_cs);

u_value f_check_46_first_uncovered_2(u_value v_hs, u_value v_ks);

void p_check_46_fresh(u_value *r_t, u_value *r_s);

void p_check_46_bind_meta(u_value v_i, u_value v_t, u_value *r_s);

void p_check_46_unify(u_value v_t1, u_value v_t2, u_value *r_s);

void p_check_46_unifies(u_value v_xs, u_value v_ys, u_value *r_s);

void p_check_46_fresh_list(u_value v_xs, u_value *r_ts, u_value *r_s);

void p_check_46_instance(u_value v_ps, u_value *r_m, u_value *r_s);

void p_check_46_instantiate(u_value v_sc, u_value *r_t, u_value *r_s);

void p_check_46_unique(u_value v_ns);

void p_check_46_unique_ids(u_value v_ns);

void p_check_46_same_length(u_value v_m, u_value v_n);

void p_check_46_arguments(u_value v_n, u_value v_k, u_value v_args);

void p_check_46_normalize(u_value v_t, u_value *r_u, u_value *r_s);

void p_check_46_normalize_sort(u_value v_n, u_value v_args, u_value *r_u, u_value *r_s);

void p_check_46_normalizes(u_value v_ts, u_value *r_us, u_value *r_s);

void p_check_46_known(u_value v_t, u_value v_what, u_value *r_u, u_value *r_s);

void p_check_46_knowns(u_value v_ts, u_value v_what, u_value *r_us, u_value *r_s);

void p_check_46_formals(u_value v_xs, u_value *r_ys, u_value *r_s);

void p_check_46_normalize_vars(u_value v_xs, u_value *r_ys, u_value *r_s);

void p_check_46_signature(u_value v_xs, u_value *r_ys, u_value *r_s);

void p_check_46_known_vars(u_value v_xs, u_value *r_ys, u_value *r_s);

void p_check_46_resolve_name(u_value v_s, u_value v_env, u_value v_n, u_value v_k, u_value *r_m, u_value *r_sc);

void p_check_46_use(u_value v_n, u_value v_a, u_value v_sc, u_value *r_x, u_value *r_s);

void p_check_46_infer(u_value v_env, u_value v_e, u_value *r_e1, u_value *r_t, u_value *r_s);

void p_check_46_application(u_value v_env, u_value v_f, u_value v_args, u_value *r_e1, u_value *r_t, u_value *r_s);

void p_check_46_field_access(u_value v_env, u_value v_f, u_value v_a, u_value v_args, u_value *r_e1, u_value *r_t, u_value *r_s);

void p_check_46_apply(u_value v_env, u_value v_f, u_value v_args, u_value *r_args1, u_value *r_r, u_value *r_s);

void p_check_46_check(u_value v_env, u_value v_e, u_value v_t, u_value *r_e1, u_value *r_s);

void p_check_46_checks(u_value v_env, u_value v_es, u_value v_ts, u_value *r_es1, u_value *r_s);

void p_check_46_infers(u_value v_env, u_value v_es, u_value *r_es1, u_value *r_ts, u_value *r_s);

void p_check_46_generators(u_value v_env, u_value v_xs, u_value v_lists, u_value *r_lists1, u_value *r_s);

void p_check_46_expr_branches(u_value v_env, u_value v_cases, u_value v_t, u_value v_r, u_value *r_cases1, u_value *r_s);

void p_check_46_guard(u_value v_env, u_value v_g, u_value *r_g1, u_value *r_s);

void p_check_46_coverage(u_value v_s, u_value v_t, u_value v_cs);

u_value f_check_46_unguarded_1(u_value v_cs);

void p_check_46_reachable(u_value v_s, u_value v_t, u_value v_before, u_value v_cs);

void p_check_46_pattern(u_value v_p, u_value v_t, u_value *r_p1, u_value *r_xs, u_value *r_s);

void p_check_46_binders(u_value v_p, u_value v_t, u_value *r_p1, u_value *r_xs, u_value *r_s);

void p_check_46_binder_list(u_value v_ps, u_value v_ts, u_value *r_qs, u_value *r_xs, u_value *r_s);

void p_check_46_variable(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s);

void p_check_46_variables(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s);

void p_check_46_statement(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s);

u_value f_check_46_initialized_1(u_value v_xs);

void p_check_46_untyped(u_value v_xs, u_value v_inits);

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s);

u_value f_check_46_os_procs_1(u_value v_c);

u_value f_check_46_syntax_procs_1(u_value v_c);

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s);

void p_check_46_call_variable(u_value v_env, u_value v_assignable, u_value v_p, u_value v_sc, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s);

void p_check_46_call_declared(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s);

void p_check_46_stmt_branches(u_value v_env, u_value v_assignable, u_value v_cases, u_value v_t, u_value *r_cases1, u_value *r_s);

void p_check_46_handler_list(u_value v_env, u_value v_assignable, u_value v_hs, u_value *r_hs1, u_value *r_s);

void p_check_46_declare_name(u_value v_c, u_value v_n);

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k);

void p_check_46_declare_sort(u_value v_c, u_value v_n);

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts);

void p_check_46_declare_datas(u_value v_cs, u_value *r_s);

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s);

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s);

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s);

u_value f_check_46_curried_2(u_value v_xss, u_value v_r);

u_value f_check_46_first_list_1(u_value v_xss);

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s);

void p_check_46_declare_funs(u_value v_cs, u_value *r_s);

void p_check_46_declare_procs(u_value v_cs, u_value *r_s);

void p_check_46_command(u_value v_c, u_value *r_c1, u_value *r_s);

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s);

u_value f_check_46_block_names_1(u_value v_cs);

u_value f_check_46_Checked_46_cmds_1(u_value v_x);

u_value f_check_46_Checked_46_ctx_1(u_value v_x);

u_value f_check_46_Checked_46_imports_1(u_value v_x);

u_value f_check_46_Checked_46_cmds_58__61__2(u_value v_x, u_value v_y);

u_value f_check_46_Checked_46_ctx_58__61__2(u_value v_x, u_value v_y);

u_value f_check_46_Checked_46_imports_58__61__2(u_value v_x, u_value v_y);

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q);

void p_check_46_check_modules(u_value v_p, u_value v_main, u_value *r_q, u_value *r_cache);

void p_check_46_check_units(u_value v_us, u_value *r_c, u_value *r_imports, u_value *r_seen, u_value *r_cs, u_value *r_cache);

u_value f_check_46_positions_of_2(u_value v_ms, u_value v_seen);

u_value f_check_46_position_in_3(u_value v_key, u_value v_cache, u_value v_i);

void p_check_46_check_unit(u_value v_ds, u_value v_main, u_value v_before, u_value v_imports, u_value *r_checked);

u_value f_check_46_modules_1(u_value v_cs);

u_value f_check_46_own_decls_2(u_value v_m, u_value v_cs);

u_value f_check_46_is_module_1(u_value v_c);

void p_check_46_type_of(u_value v_e, u_value *r_t);

u_value f_check_46_ev_1(u_value v_n);

u_value f_check_46_pv_1(u_value v_n);

u_value f_check_46_num_1(u_value v_n);

u_value f_check_46_ap_2(u_value v_f, u_value v_args);

u_value f_check_46_tl_1(u_value v_t);

u_value f_check_46_lists_0(void);

u_value f_value_46_list_value_1(u_value v_vs);

u_value f_value_46_list_elements_1(u_value v_v);

u_value f_print_46_show_value_1(u_value v_v);

u_value f_print_46_show_values_1(u_value v_vs);

u_value f_print_46_list_items_1(u_value v_v);

u_value f_print_46_escape_char_1(u_value v_c);

u_value f_print_46_show_type_1(u_value v_t);

u_value f_print_46_show_types_1(u_value v_ts);

u_value f_print_46_type_section_2(u_value v_mode, u_value v_ts);

u_value f_print_46_type_operand_1(u_value v_t);

u_value f_print_46_type_component_1(u_value v_t);

u_value f_print_46_show_pat_1(u_value v_p);

u_value f_print_46_show_pats_1(u_value v_ps);

u_value f_print_46_symbolic_1(u_value v_n);

u_value f_print_46_infix_text_3(u_value v_op, u_value v_l, u_value v_r);

u_value f_print_46_pat_operand_1(u_value v_a);

u_value f_print_46_text_of_1(u_value v_v);

u_value f_run_46_Prog_46_funs_1(u_value v_x);

u_value f_run_46_Prog_46_procs_1(u_value v_x);

u_value f_run_46_Prog_46_constrs_1(u_value v_x);

u_value f_run_46_Prog_46_positions_1(u_value v_x);

u_value f_run_46_Prog_46_computing_1(u_value v_x);

u_value f_run_46_Prog_46_args_1(u_value v_x);

u_value f_run_46_Prog_46_funs_58__61__2(u_value v_x, u_value v_y);

u_value f_run_46_Prog_46_procs_58__61__2(u_value v_x, u_value v_y);

u_value f_run_46_Prog_46_constrs_58__61__2(u_value v_x, u_value v_y);

u_value f_run_46_Prog_46_positions_58__61__2(u_value v_x, u_value v_y);

u_value f_run_46_Prog_46_computing_58__61__2(u_value v_x, u_value v_y);

u_value f_run_46_Prog_46_args_58__61__2(u_value v_x, u_value v_y);

u_value f_run_46_position_of_2(u_value v_p, u_value v_k);

u_value f_run_46_load_1(u_value v_q);

u_value f_run_46_fun_defs_1(u_value v_cs);

u_value f_run_46_proc_defs_1(u_value v_cs);

u_value f_run_46_constr_positions_1(u_value v_cs);

u_value f_run_46_numbers_from_2(u_value v_i, u_value v_n);

u_value f_run_46_constr_defs_1(u_value v_cs);

u_value f_run_46_constr_arity_1(u_value v_k);

u_value f_run_46_builtin_names_0(void);

u_value f_run_46_is_builtin_1(u_value v_n);

u_value f_run_46_is_function_1(u_value v_v);

u_value f_run_46_truth_1(u_value v_v);

u_value f_run_46_lambdas_2(u_value v_xss, u_value v_b);

u_value f_run_46_str_values_1(u_value v_vs);

u_value f_run_46_char_values_1(u_value v_vs);

u_value f_run_46_bind_all_3(u_value v_xs, u_value v_vs, u_value v_env);

u_value f_run_46_match_into_3(u_value v_p, u_value v_v, u_value v_env);

u_value f_run_46_match_all_into_3(u_value v_ps, u_value v_vs, u_value v_env);

u_value f_run_46_pattern_vars_1(u_value v_p);

void p_run_46_evaluate(u_value v_p, u_value v_env, u_value v_e, u_value *r_v, u_value *r_cache);

void p_run_46_evaluate_all(u_value v_p, u_value v_env, u_value v_es, u_value *r_vs, u_value *r_cache);

u_value f_run_46_variable_value_4(u_value v_p, u_value v_env, u_value v_n, u_value v_t);

void p_run_46_apply_expr(u_value v_p, u_value v_env, u_value v_f, u_value v_args, u_value *r_v, u_value *r_cache);

void p_run_46_apply_value(u_value v_p, u_value v_f, u_value v_vs, u_value *r_v, u_value *r_cache);

void p_run_46_cached(u_value v_p, u_value v_n, u_value v_body, u_value *r_v, u_value *r_cache);

u_value f_run_46_failure_2(u_value v_n, u_value v_args);

void p_run_46_primitive(u_value v_n, u_value v_vs, u_value *r_v);

void p_run_46_equal(u_value v_a, u_value v_b, u_value *r_r);

void p_run_46_compare_values(u_value v_p, u_value v_a, u_value v_b, u_value *r_r);

void p_run_46_compare_all(u_value v_p, u_value v_xs, u_value v_ys, u_value *r_r);

u_value f_run_46_order_2(u_value v_x, u_value v_y);

void p_run_46_equal_all(u_value v_xs, u_value v_ys, u_value *r_r);

void p_run_46_combinations(u_value v_p, u_value v_env, u_value v_xs, u_value v_lists, u_value *r_envs, u_value *r_cache);

void p_run_46_extend(u_value v_p, u_value v_env, u_value v_x, u_value v_vs, u_value v_ys, u_value v_lists, u_value *r_envs, u_value *r_cache);

void p_run_46_comprehension(u_value v_p, u_value v_envs, u_value v_result, u_value v_cond, u_value *r_vs, u_value *r_cache);

void p_run_46_exec(u_value v_p, u_value v_x, u_value *r_env, u_value *r_cache);

void p_run_46_exec_all(u_value v_p, u_value v_xs, u_value *r_env, u_value *r_cache);

void p_run_46_scoped(u_value v_p, u_value v_names, u_value v_inner, u_value v_xs, u_value *r_env, u_value *r_cache);

u_value f_run_46_restored_3(u_value v_names, u_value v_outer, u_value v_env);

void p_run_46_select(u_value v_p, u_value v_env, u_value v_v, u_value v_cases, u_value *r_sel, u_value *r_cache);

void p_run_46_holds(u_value v_p, u_value v_env, u_value v_g, u_value *r_ok, u_value *r_cache);

void p_run_46_handle(u_value v_p, u_value v_hs, u_value v_v, u_value v_t, u_value *r_env, u_value *r_cache);

void p_run_46_invoke(u_value v_p, u_value v_n, u_value v_ins, u_value v_refs, u_value *r_outs, u_value *r_env, u_value *r_cache);

void p_run_46_invoke_value(u_value v_p, u_value v_f, u_value v_ins, u_value v_refs, u_value *r_outs, u_value *r_env, u_value *r_cache);

void p_run_46_run_body(u_value v_p, u_value v_n, u_value v_around, u_value v_xs, u_value v_ys, u_value v_zs, u_value v_body, u_value v_ins, u_value v_refs, u_value *r_outs, u_value *r_env, u_value *r_cache);

u_value f_run_46_values_in_2(u_value v_ps, u_value v_env);

u_value f_run_46_read_outs_3(u_value v_p, u_value v_ys, u_value v_env);

void p_run_46_print_all(u_value v_lines);

void p_run_46_os_proc(u_value v_n, u_value v_ins, u_value *r_outs);

void p_run_46_run_proc(u_value v_p, u_value v_n, u_value *r_cache);

void p_run_46_run_cmd(u_value v_p, u_value v_c, u_value *r_cache);

void p_run_46_run_all(u_value v_p, u_value v_cs, u_value *r_cache);

void p_run_46_run_program(u_value v_q);

void p_run_46_check_and_run(u_value v_cs);

u_value f_run_46_asserting_3(u_value v_n, u_value v_e, u_value v_expected);

u_value f_run_46_assigning_1(u_value v_e);

u_value f_run_46_int_1(u_value v_n);

u_value f_run_46_procedures_0(void);

u_value f_run_46_hole_var_1(u_value v_n);

u_value f_run_46_equals_2(u_value v_n, u_value v_k);

u_value f_c_46_c_type_1(u_value v_t);

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x);

u_value f_c_46_c_expr_1(u_value v_e);

u_value f_c_46_c_operand_1(u_value v_e);

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s);

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss);

u_value f_c_46_c_params_1(u_value v_ps);

u_value f_c_46_c_param_1(u_value v_p);

u_value f_c_46_c_decl_1(u_value v_d);

u_value f_c_46_c_file_1(u_value v_f);

u_value f_c_46_c_call_2(u_value v_f, u_value v_args);

u_value f_compile_46_Globals_46_funs_1(u_value v_x);

u_value f_compile_46_Globals_46_constrs_1(u_value v_x);

u_value f_compile_46_Globals_46_arities_1(u_value v_x);

u_value f_compile_46_Globals_46_fields_1(u_value v_x);

u_value f_compile_46_Globals_46_procs_1(u_value v_x);

u_value f_compile_46_Globals_46_funs_58__61__2(u_value v_x, u_value v_y);

u_value f_compile_46_Globals_46_constrs_58__61__2(u_value v_x, u_value v_y);

u_value f_compile_46_Globals_46_arities_58__61__2(u_value v_x, u_value v_y);

u_value f_compile_46_Globals_46_fields_58__61__2(u_value v_x, u_value v_y);

u_value f_compile_46_Globals_46_procs_58__61__2(u_value v_x, u_value v_y);

u_value f_compile_46_Gen_46_count_1(u_value v_x);

u_value f_compile_46_Gen_46_lifted_1(u_value v_x);

u_value f_compile_46_Gen_46_count_58__61__2(u_value v_x, u_value v_y);

u_value f_compile_46_Gen_46_lifted_58__61__2(u_value v_x, u_value v_y);

u_value f_compile_46_value_type_0(void);

u_value f_compile_46_value_ptr_0(void);

u_value f_compile_46_mangle_1(u_value v_n);

u_value f_compile_46_mangle_char_1(u_value v_c);

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k);

u_value f_compile_46_proc_name_1(u_value v_n);

u_value f_compile_46_var_name_1(u_value v_n);

u_value f_compile_46_ref_name_1(u_value v_n);

u_value f_compile_46_builtin_functions_0(void);

u_value f_compile_46_descriptor_1(u_value v_n);

u_value f_compile_46_unit_value_1(u_value v_n);

u_value f_compile_46_runtime_constructors_0(void);

u_value f_compile_46_descriptor_decl_2(u_value v_n, u_value v_i);

u_value f_compile_46_unit_decl_1(u_value v_n);

u_value f_compile_46_descriptor_decls_1(u_value v_cs);

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i);

u_value f_compile_46_in_scope_1(u_value v_xs);

u_value f_compile_46_pointer_1(u_value v_x);

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x);

u_value f_compile_46_address_1(u_value v_c);

u_value f_compile_46_type_key_1(u_value v_t);

u_value f_compile_46_globals_1(u_value v_cs);

u_value f_compile_46_proc_arities_1(u_value v_c);

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m);

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k);

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n);

u_value f_compile_46_fun_arities_1(u_value v_c);

u_value f_compile_46_constr_arities_1(u_value v_c);

u_value f_compile_46_constr_arity_1(u_value v_k);

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i);

u_value f_compile_46_index_2(u_value v_a, u_value v_i);

u_value f_compile_46_field_2(u_value v_v, u_value v_i);

u_value f_compile_46_truth_1(u_value v_v);

u_value f_compile_46_conjunction_1(u_value v_cs);

u_value f_compile_46_literal_value_1(u_value v_l);

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v);

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v);

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v);

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v);

u_value f_compile_46_pattern_names_1(u_value v_p);

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound);

u_value f_compile_46_free_in_stmt_2(u_value v_x, u_value v_bound);

u_value f_compile_46_free_in_stmt_branch_2(u_value v_b, u_value v_bound);

u_value f_compile_46_free_in_handler_2(u_value v_h, u_value v_bound);

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound);

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b);

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g);

void p_compile_46_lift(u_value v_d, u_value *r_g);

u_value f_compile_46_global_value_3(u_value v_gl, u_value v_n, u_value v_t);

void p_compile_46_compile_expr(u_value v_gl, u_value v_env, u_value v_e, u_value *r_ss, u_value *r_r, u_value *r_g);

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g);

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g);

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts);

void p_compile_46_compile_named(u_value v_gl, u_value v_env, u_value v_n, u_value v_ty, u_value v_args, u_value *r_ss, u_value *r_r, u_value *r_g);

void p_compile_46_compile_lambda(u_value v_gl, u_value v_env, u_value v_xs, u_value v_b, u_value *r_ss, u_value *r_r, u_value *r_g);

void p_compile_46_compile_proc_lambda(u_value v_gl, u_value v_env, u_value v_ins, u_value v_outs, u_value v_b, u_value *r_ss, u_value *r_r, u_value *r_g);

u_value f_compile_46_proc_params_0(void);

u_value f_compile_46_unpack_pointers_1(u_value v_xs);

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a);

void p_compile_46_expr_cases(u_value v_gl, u_value v_env, u_value v_cases, u_value v_v, u_value v_t, u_value *r_ss, u_value *r_g);

void p_compile_46_compile_loops(u_value v_gl, u_value v_env, u_value v_xs, u_value v_lists, u_value v_result, u_value v_c, u_value v_t, u_value *r_ss, u_value *r_g);

void p_compile_46_compile_stmt(u_value v_gl, u_value v_env, u_value v_s, u_value *r_ss, u_value *r_g);

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g);

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts);

void p_compile_46_handler_cases(u_value v_gl, u_value v_env, u_value v_hs, u_value v_h, u_value v_d, u_value *r_ss, u_value *r_g);

void p_compile_46_stmt_cases(u_value v_gl, u_value v_env, u_value v_cases, u_value v_v, u_value v_d, u_value *r_ss, u_value *r_g);

void p_compile_46_compile_cmd(u_value v_gl, u_value v_entry, u_value v_c, u_value *r_ds, u_value *r_ms, u_value *r_g);

void p_compile_46_compile_cmds(u_value v_gl, u_value v_entry, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g);

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r);

u_value f_compile_46_wrappers_1(u_value v_gl);

u_value f_compile_46_proc_wrapper_1(u_value v_p);

u_value f_compile_46_fun_wrapper_1(u_value v_p);

u_value f_compile_46_builtin_wrapper_1(u_value v_p);

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k);

u_value f_compile_46_constr_wrapper_1(u_value v_p);

u_value f_compile_46_range_to_1(u_value v_k);

u_value f_compile_46_prototype_1(u_value v_d);

void p_compile_46_compile_program(u_value v_q, u_value v_m, u_value v_entry, u_value *r_f);

void p_compile_46_compile_file(u_value v_source, u_value v_entry, u_value v_target);

u_value f_compile_46_is_entry_2(u_value v_c, u_value v_n);

void p_compile_46_compile_with(u_value v_source, u_value v_entry, u_value v_target, u_value *r_cache);

void p_ulang_46_main(void);

u_value f_ulang_46_usage_0(void);

u_value f_ulang_46_before_dashes_1(u_value v_args);

u_value f_ulang_46_after_dashes_1(u_value v_args);

void p_ulang_46_attempt(u_value v_file, u_value v_command, u_value *r_ok);

void p_ulang_46_located(u_value v_file, u_value v_m, u_value v_i, u_value *r_message);

u_value f_ulang_46_in_module_1(u_value v_words);

void p_ulang_46_load_file(u_value v_file, u_value *r_q);

u_value f_ulang_46_module_names_1(u_value v_cs);

u_value f_ulang_46_is_any_test_1(u_value v_c);

void p_ulang_46_check_file(u_value v_file, u_value *r_ok);

void p_ulang_46_compile_to(u_value v_file, u_value v_entry, u_value v_target, u_value *r_ok);

void p_ulang_46_run_file(u_value v_file, u_value v_name, u_value v_args, u_value *r_ok);

void p_ulang_46_test_file(u_value v_file, u_value v_names, u_value v_args, u_value *r_ok);

void p_ulang_46_run_tests(u_value v_p, u_value v_tests, u_value *r_ok);

void p_ulang_46_run_test(u_value v_p, u_value v_c, u_value *r_passed);

u_value f_ulang_46_is_selected_test_2(u_value v_c, u_value v_names);

u_value f_ulang_46_is_test_2(u_value v_c, u_value v_n);

u_value f_ulang_46_cmd_name_1(u_value v_c);

u_value f_ulang_46_type_message_1(u_value v_e);

u_value f_ulang_46_field_message_1(u_value v_parts);

u_value f_ulang_46_without_update_1(u_value v_f);

u_value f_ulang_46_runtime_message_1(u_value v_e);

u_value lam215(u_value *env, u_value *args);

u_value lam899(u_value *env, u_value *args);

u_value lam1284(u_value *env, u_value *args);

u_value lam1285(u_value *env, u_value *args);

u_value lam1287(u_value *env, u_value *args);

u_value lam1288(u_value *env, u_value *args);

u_value lam1298(u_value *env, u_value *args);

u_value lam1305(u_value *env, u_value *args);

u_value lam1309(u_value *env, u_value *args);

u_value lam1539(u_value *env, u_value *args);

u_value lam1650(u_value *env, u_value *args);

u_value lam1653(u_value *env, u_value *args);

u_value lam1724(u_value *env, u_value *args);

u_value lam3062(u_value *env, u_value *args);

void proc3089(u_value *env, u_value *ins, u_value **outs);

void proc3091(u_value *env, u_value *ins, u_value **outs);

void proc3092(u_value *env, u_value *ins, u_value **outs);

void proc3093(u_value *env, u_value *ins, u_value **outs);

void proc3094(u_value *env, u_value *ins, u_value **outs);

u_value lam3151(u_value *env, u_value *args);

u_value lam3164(u_value *env, u_value *args);

u_value w_f_prelude_46_not_1(u_value *env, u_value *args) {
  return f_prelude_46_not_1(args[0]);
}

u_value w_f_prelude_46__61__61__62__2(u_value *env, u_value *args) {
  return f_prelude_46__61__61__62__2(args[0], args[1]);
}

u_value w_f_prelude_46__33__61__2(u_value *env, u_value *args) {
  return f_prelude_46__33__61__2(args[0], args[1]);
}

u_value w_f_prelude_46__60__61__2(u_value *env, u_value *args) {
  return f_prelude_46__60__61__2(args[0], args[1]);
}

u_value w_f_prelude_46__62__2(u_value *env, u_value *args) {
  return f_prelude_46__62__2(args[0], args[1]);
}

u_value w_f_prelude_46__62__61__2(u_value *env, u_value *args) {
  return f_prelude_46__62__61__2(args[0], args[1]);
}

u_value w_f_prelude_46__45__1(u_value *env, u_value *args) {
  return f_prelude_46__45__1(args[0]);
}

u_value w_f_option_46_or_else_2(u_value *env, u_value *args) {
  return f_option_46_or_else_2(args[0], args[1]);
}

u_value w_f_list_46__43__43__2(u_value *env, u_value *args) {
  return f_list_46__43__43__2(args[0], args[1]);
}

u_value w_f_list_46_length_1(u_value *env, u_value *args) {
  return f_list_46_length_1(args[0]);
}

u_value w_f_list_46_lookup_2(u_value *env, u_value *args) {
  return f_list_46_lookup_2(args[0], args[1]);
}

u_value w_f_list_46_update_3(u_value *env, u_value *args) {
  return f_list_46_update_3(args[0], args[1], args[2]);
}

u_value w_f_list_46_replaced_3(u_value *env, u_value *args) {
  return f_list_46_replaced_3(args[0], args[1], args[2]);
}

u_value w_f_list_46_remove_2(u_value *env, u_value *args) {
  return f_list_46_remove_2(args[0], args[1]);
}

u_value w_f_list_46_has_key_2(u_value *env, u_value *args) {
  return f_list_46_has_key_2(args[0], args[1]);
}

u_value w_f_list_46_keys_1(u_value *env, u_value *args) {
  return f_list_46_keys_1(args[0]);
}

u_value w_f_list_46_values_1(u_value *env, u_value *args) {
  return f_list_46_values_1(args[0]);
}

u_value w_f_list_46_contains_2(u_value *env, u_value *args) {
  return f_list_46_contains_2(args[0], args[1]);
}

u_value w_f_list_46_exists_2(u_value *env, u_value *args) {
  return f_list_46_exists_2(args[0], args[1]);
}

u_value w_f_list_46_forall_2(u_value *env, u_value *args) {
  return f_list_46_forall_2(args[0], args[1]);
}

u_value w_f_list_46_distinct_1(u_value *env, u_value *args) {
  return f_list_46_distinct_1(args[0]);
}

u_value w_f_list_46_repeated_1(u_value *env, u_value *args) {
  return f_list_46_repeated_1(args[0]);
}

u_value w_f_list_46_diff_2(u_value *env, u_value *args) {
  return f_list_46_diff_2(args[0], args[1]);
}

u_value w_f_list_46_concat_1(u_value *env, u_value *args) {
  return f_list_46_concat_1(args[0]);
}

u_value w_f_list_46_reverse_onto_2(u_value *env, u_value *args) {
  return f_list_46_reverse_onto_2(args[0], args[1]);
}

u_value w_f_list_46_reverse_1(u_value *env, u_value *args) {
  return f_list_46_reverse_1(args[0]);
}

u_value w_f_list_46_zip_2(u_value *env, u_value *args) {
  return f_list_46_zip_2(args[0], args[1]);
}

u_value w_f_list_46_take_2(u_value *env, u_value *args) {
  return f_list_46_take_2(args[0], args[1]);
}

u_value w_f_list_46_drop_2(u_value *env, u_value *args) {
  return f_list_46_drop_2(args[0], args[1]);
}

u_value w_f_list_46_replicate_2(u_value *env, u_value *args) {
  return f_list_46_replicate_2(args[0], args[1]);
}

u_value w_f_list_46_firsts_1(u_value *env, u_value *args) {
  return f_list_46_firsts_1(args[0]);
}

u_value w_f_list_46_seconds_1(u_value *env, u_value *args) {
  return f_list_46_seconds_1(args[0]);
}

u_value w_f_syntax_46_Name_46_module_1(u_value *env, u_value *args) {
  return f_syntax_46_Name_46_module_1(args[0]);
}

u_value w_f_syntax_46_Name_46_base_1(u_value *env, u_value *args) {
  return f_syntax_46_Name_46_base_1(args[0]);
}

u_value w_f_syntax_46_Name_46_module_58__61__2(u_value *env, u_value *args) {
  return f_syntax_46_Name_46_module_58__61__2(args[0], args[1]);
}

u_value w_f_syntax_46_Name_46_base_58__61__2(u_value *env, u_value *args) {
  return f_syntax_46_Name_46_base_58__61__2(args[0], args[1]);
}

u_value w_f_syntax_46_Var_46_name_1(u_value *env, u_value *args) {
  return f_syntax_46_Var_46_name_1(args[0]);
}

u_value w_f_syntax_46_Var_46_typ_1(u_value *env, u_value *args) {
  return f_syntax_46_Var_46_typ_1(args[0]);
}

u_value w_f_syntax_46_Var_46_name_58__61__2(u_value *env, u_value *args) {
  return f_syntax_46_Var_46_name_58__61__2(args[0], args[1]);
}

u_value w_f_syntax_46_Var_46_typ_58__61__2(u_value *env, u_value *args) {
  return f_syntax_46_Var_46_typ_58__61__2(args[0], args[1]);
}

u_value w_f_syntax_46_Branch_46_pat_1(u_value *env, u_value *args) {
  return f_syntax_46_Branch_46_pat_1(args[0]);
}

u_value w_f_syntax_46_Branch_46_guard_1(u_value *env, u_value *args) {
  return f_syntax_46_Branch_46_guard_1(args[0]);
}

u_value w_f_syntax_46_Branch_46_body_1(u_value *env, u_value *args) {
  return f_syntax_46_Branch_46_body_1(args[0]);
}

u_value w_f_syntax_46_Branch_46_pat_58__61__2(u_value *env, u_value *args) {
  return f_syntax_46_Branch_46_pat_58__61__2(args[0], args[1]);
}

u_value w_f_syntax_46_Branch_46_guard_58__61__2(u_value *env, u_value *args) {
  return f_syntax_46_Branch_46_guard_58__61__2(args[0], args[1]);
}

u_value w_f_syntax_46_Branch_46_body_58__61__2(u_value *env, u_value *args) {
  return f_syntax_46_Branch_46_body_58__61__2(args[0], args[1]);
}

u_value w_f_syntax_46_Handler_46_typ_1(u_value *env, u_value *args) {
  return f_syntax_46_Handler_46_typ_1(args[0]);
}

u_value w_f_syntax_46_Handler_46_pat_1(u_value *env, u_value *args) {
  return f_syntax_46_Handler_46_pat_1(args[0]);
}

u_value w_f_syntax_46_Handler_46_guard_1(u_value *env, u_value *args) {
  return f_syntax_46_Handler_46_guard_1(args[0]);
}

u_value w_f_syntax_46_Handler_46_body_1(u_value *env, u_value *args) {
  return f_syntax_46_Handler_46_body_1(args[0]);
}

u_value w_f_syntax_46_Handler_46_typ_58__61__2(u_value *env, u_value *args) {
  return f_syntax_46_Handler_46_typ_58__61__2(args[0], args[1]);
}

u_value w_f_syntax_46_Handler_46_pat_58__61__2(u_value *env, u_value *args) {
  return f_syntax_46_Handler_46_pat_58__61__2(args[0], args[1]);
}

u_value w_f_syntax_46_Handler_46_guard_58__61__2(u_value *env, u_value *args) {
  return f_syntax_46_Handler_46_guard_58__61__2(args[0], args[1]);
}

u_value w_f_syntax_46_Handler_46_body_58__61__2(u_value *env, u_value *args) {
  return f_syntax_46_Handler_46_body_58__61__2(args[0], args[1]);
}

u_value w_f_syntax_46_Constr_46_name_1(u_value *env, u_value *args) {
  return f_syntax_46_Constr_46_name_1(args[0]);
}

u_value w_f_syntax_46_Constr_46_args_1(u_value *env, u_value *args) {
  return f_syntax_46_Constr_46_args_1(args[0]);
}

u_value w_f_syntax_46_Constr_46_name_58__61__2(u_value *env, u_value *args) {
  return f_syntax_46_Constr_46_name_58__61__2(args[0], args[1]);
}

u_value w_f_syntax_46_Constr_46_args_58__61__2(u_value *env, u_value *args) {
  return f_syntax_46_Constr_46_args_58__61__2(args[0], args[1]);
}

u_value w_f_syntax_46_Program_46_cmds_1(u_value *env, u_value *args) {
  return f_syntax_46_Program_46_cmds_1(args[0]);
}

u_value w_f_syntax_46_Program_46_cmds_58__61__2(u_value *env, u_value *args) {
  return f_syntax_46_Program_46_cmds_58__61__2(args[0], args[1]);
}

u_value w_f_syntax_46_unqualified_1(u_value *env, u_value *args) {
  return f_syntax_46_unqualified_1(args[0]);
}

u_value w_f_syntax_46_qualified_1(u_value *env, u_value *args) {
  return f_syntax_46_qualified_1(args[0]);
}

u_value w_f_syntax_46_list_name_0(u_value *env, u_value *args) {
  return f_syntax_46_list_name_0();
}

u_value w_f_syntax_46_nil_name_0(u_value *env, u_value *args) {
  return f_syntax_46_nil_name_0();
}

u_value w_f_syntax_46_cons_name_0(u_value *env, u_value *args) {
  return f_syntax_46_cons_name_0();
}

u_value w_f_syntax_46_option_name_0(u_value *env, u_value *args) {
  return f_syntax_46_option_name_0();
}

u_value w_f_syntax_46_none_name_0(u_value *env, u_value *args) {
  return f_syntax_46_none_name_0();
}

u_value w_f_syntax_46_some_name_0(u_value *env, u_value *args) {
  return f_syntax_46_some_name_0();
}

u_value w_f_syntax_46_os_error_name_0(u_value *env, u_value *args) {
  return f_syntax_46_os_error_name_0();
}

u_value w_f_syntax_46_division_by_zero_name_0(u_value *env, u_value *args) {
  return f_syntax_46_division_by_zero_name_0();
}

u_value w_f_syntax_46_no_index_name_0(u_value *env, u_value *args) {
  return f_syntax_46_no_index_name_0();
}

u_value w_f_syntax_46_no_character_name_0(u_value *env, u_value *args) {
  return f_syntax_46_no_character_name_0();
}

u_value w_f_syntax_46_missing_field_name_0(u_value *env, u_value *args) {
  return f_syntax_46_missing_field_name_0();
}

u_value w_f_syntax_46_tuple_name_0(u_value *env, u_value *args) {
  return f_syntax_46_tuple_name_0();
}

u_value w_f_syntax_46_names_1(u_value *env, u_value *args) {
  return f_syntax_46_names_1(args[0]);
}

u_value w_f_syntax_46_types_1(u_value *env, u_value *args) {
  return f_syntax_46_types_1(args[0]);
}

u_value w_f_syntax_46_arity_1(u_value *env, u_value *args) {
  return f_syntax_46_arity_1(args[0]);
}

u_value w_f_lex_46_is_symbol_1(u_value *env, u_value *args) {
  return f_lex_46_is_symbol_1(args[0]);
}

u_value w_f_lex_46_is_punct_1(u_value *env, u_value *args) {
  return f_lex_46_is_punct_1(args[0]);
}

u_value w_f_lex_46_is_name_char_1(u_value *env, u_value *args) {
  return f_lex_46_is_name_char_1(args[0]);
}

u_value w_f_lex_46_peek_2(u_value *env, u_value *args) {
  return f_lex_46_peek_2(args[0], args[1]);
}

u_value w_f_lex_46_unescape_1(u_value *env, u_value *args) {
  return f_lex_46_unescape_1(args[0]);
}

u_value w_f_lex_46_scan_3(u_value *env, u_value *args) {
  return f_lex_46_scan_3(args[0], args[1], args[2]);
}

u_value w_f_lex_46_tokens_1(u_value *env, u_value *args) {
  return f_lex_46_tokens_1(args[0]);
}

u_value w_f_set_46_empty_set_0(u_value *env, u_value *args) {
  return f_set_46_empty_set_0();
}

u_value w_f_set_46_member_2(u_value *env, u_value *args) {
  return f_set_46_member_2(args[0], args[1]);
}

u_value w_f_set_46_insert_2(u_value *env, u_value *args) {
  return f_set_46_insert_2(args[0], args[1]);
}

u_value w_f_set_46_from_list_1(u_value *env, u_value *args) {
  return f_set_46_from_list_1(args[0]);
}

u_value w_f_set_46_insert_all_2(u_value *env, u_value *args) {
  return f_set_46_insert_all_2(args[0], args[1]);
}

u_value w_f_set_46_elements_1(u_value *env, u_value *args) {
  return f_set_46_elements_1(args[0]);
}

u_value w_f_set_46_elements_onto_2(u_value *env, u_value *args) {
  return f_set_46_elements_onto_2(args[0], args[1]);
}

u_value w_f_set_46_count_1(u_value *env, u_value *args) {
  return f_set_46_count_1(args[0]);
}

u_value w_f_set_46_depth_1(u_value *env, u_value *args) {
  return f_set_46_depth_1(args[0]);
}

u_value w_f_set_46_snode_depth_2(u_value *env, u_value *args) {
  return f_set_46_snode_depth_2(args[0], args[1]);
}

u_value w_f_set_46_snode_3(u_value *env, u_value *args) {
  return f_set_46_snode_3(args[0], args[1], args[2]);
}

u_value w_f_set_46_turn_left_1(u_value *env, u_value *args) {
  return f_set_46_turn_left_1(args[0]);
}

u_value w_f_set_46_turn_right_1(u_value *env, u_value *args) {
  return f_set_46_turn_right_1(args[0]);
}

u_value w_f_set_46_smaller_1(u_value *env, u_value *args) {
  return f_set_46_smaller_1(args[0]);
}

u_value w_f_set_46_greater_1(u_value *env, u_value *args) {
  return f_set_46_greater_1(args[0]);
}

u_value w_f_set_46_rebalance_3(u_value *env, u_value *args) {
  return f_set_46_rebalance_3(args[0], args[1], args[2]);
}

u_value w_f_set_46_well_formed_1(u_value *env, u_value *args) {
  return f_set_46_well_formed_1(args[0]);
}

u_value w_f_set_46_upto_1(u_value *env, u_value *args) {
  return f_set_46_upto_1(args[0]);
}

u_value w_f_parse_46_Notations_46_cmds_1(u_value *env, u_value *args) {
  return f_parse_46_Notations_46_cmds_1(args[0]);
}

u_value w_f_parse_46_Notations_46_ops_1(u_value *env, u_value *args) {
  return f_parse_46_Notations_46_ops_1(args[0]);
}

u_value w_f_parse_46_Notations_46_cmds_58__61__2(u_value *env, u_value *args) {
  return f_parse_46_Notations_46_cmds_58__61__2(args[0], args[1]);
}

u_value w_f_parse_46_Notations_46_ops_58__61__2(u_value *env, u_value *args) {
  return f_parse_46_Notations_46_ops_58__61__2(args[0], args[1]);
}

u_value w_f_parse_46_Scope_46_params_1(u_value *env, u_value *args) {
  return f_parse_46_Scope_46_params_1(args[0]);
}

u_value w_f_parse_46_Scope_46_notations_1(u_value *env, u_value *args) {
  return f_parse_46_Scope_46_notations_1(args[0]);
}

u_value w_f_parse_46_Scope_46_params_58__61__2(u_value *env, u_value *args) {
  return f_parse_46_Scope_46_params_58__61__2(args[0], args[1]);
}

u_value w_f_parse_46_Scope_46_notations_58__61__2(u_value *env, u_value *args) {
  return f_parse_46_Scope_46_notations_58__61__2(args[0], args[1]);
}

u_value w_f_parse_46_keywords_0(u_value *env, u_value *args) {
  return f_parse_46_keywords_0();
}

u_value w_f_parse_46_is_keyword_1(u_value *env, u_value *args) {
  return f_parse_46_is_keyword_1(args[0]);
}

u_value w_f_parse_46_is_reserved_1(u_value *env, u_value *args) {
  return f_parse_46_is_reserved_1(args[0]);
}

u_value w_f_parse_46_prefix_prec_2(u_value *env, u_value *args) {
  return f_parse_46_prefix_prec_2(args[0], args[1]);
}

u_value w_f_parse_46_postfix_prec_2(u_value *env, u_value *args) {
  return f_parse_46_postfix_prec_2(args[0], args[1]);
}

u_value w_f_parse_46_infix_of_2(u_value *env, u_value *args) {
  return f_parse_46_infix_of_2(args[0], args[1]);
}

u_value w_f_parse_46_declared_2(u_value *env, u_value *args) {
  return f_parse_46_declared_2(args[0], args[1]);
}

u_value w_f_parse_46_no_notations_0(u_value *env, u_value *args) {
  return f_parse_46_no_notations_0();
}

u_value w_f_parse_46_with_notations_2(u_value *env, u_value *args) {
  return f_parse_46_with_notations_2(args[0], args[1]);
}

u_value w_f_parse_46_operators_of_1(u_value *env, u_value *args) {
  return f_parse_46_operators_of_1(args[0]);
}

u_value w_f_parse_46_head_token_1(u_value *env, u_value *args) {
  return f_parse_46_head_token_1(args[0]);
}

u_value w_f_parse_46_second_token_1(u_value *env, u_value *args) {
  return f_parse_46_second_token_1(args[0]);
}

u_value w_f_parse_46_head_offset_1(u_value *env, u_value *args) {
  return f_parse_46_head_offset_1(args[0]);
}

u_value w_f_parse_46_at_keyword_2(u_value *env, u_value *args) {
  return f_parse_46_at_keyword_2(args[0], args[1]);
}

u_value w_f_parse_46_at_punct_2(u_value *env, u_value *args) {
  return f_parse_46_at_punct_2(args[0], args[1]);
}

u_value w_f_parse_46_describe_1(u_value *env, u_value *args) {
  return f_parse_46_describe_1(args[0]);
}

u_value w_f_parse_46_operator_at_2(u_value *env, u_value *args) {
  return f_parse_46_operator_at_2(args[0], args[1]);
}

u_value w_f_parse_46_Ops_46_notations_1(u_value *env, u_value *args) {
  return f_parse_46_Ops_46_notations_1(args[0]);
}

u_value w_f_parse_46_Ops_46_prefix_op_1(u_value *env, u_value *args) {
  return f_parse_46_Ops_46_prefix_op_1(args[0]);
}

u_value w_f_parse_46_Ops_46_postfix_op_1(u_value *env, u_value *args) {
  return f_parse_46_Ops_46_postfix_op_1(args[0]);
}

u_value w_f_parse_46_Ops_46_infix_op_1(u_value *env, u_value *args) {
  return f_parse_46_Ops_46_infix_op_1(args[0]);
}

u_value w_f_parse_46_Ops_46_notations_58__61__2(u_value *env, u_value *args) {
  return f_parse_46_Ops_46_notations_58__61__2(args[0], args[1]);
}

u_value w_f_parse_46_Ops_46_prefix_op_58__61__2(u_value *env, u_value *args) {
  return f_parse_46_Ops_46_prefix_op_58__61__2(args[0], args[1]);
}

u_value w_f_parse_46_Ops_46_postfix_op_58__61__2(u_value *env, u_value *args) {
  return f_parse_46_Ops_46_postfix_op_58__61__2(args[0], args[1]);
}

u_value w_f_parse_46_Ops_46_infix_op_58__61__2(u_value *env, u_value *args) {
  return f_parse_46_Ops_46_infix_op_58__61__2(args[0], args[1]);
}

u_value w_f_parse_46_is_prec_2(u_value *env, u_value *args) {
  return f_parse_46_is_prec_2(args[0], args[1]);
}

u_value w_f_parse_46_type_one_1(u_value *env, u_value *args) {
  return f_parse_46_type_one_1(args[0]);
}

u_value w_f_parse_46_type_components_1(u_value *env, u_value *args) {
  return f_parse_46_type_components_1(args[0]);
}

u_value w_f_parse_46_type_prefix_2(u_value *env, u_value *args) {
  return f_parse_46_type_prefix_2(args[0], args[1]);
}

u_value w_f_parse_46_type_infix_3(u_value *env, u_value *args) {
  return f_parse_46_type_infix_3(args[0], args[1], args[2]);
}

u_value w_f_parse_46_has_invalid_1(u_value *env, u_value *args) {
  return f_parse_46_has_invalid_1(args[0]);
}

u_value w_f_parse_46_any_invalid_1(u_value *env, u_value *args) {
  return f_parse_46_any_invalid_1(args[0]);
}

u_value w_f_parse_46_starts_type_2(u_value *env, u_value *args) {
  return f_parse_46_starts_type_2(args[0], args[1]);
}

u_value w_f_parse_46_expr_op1_2(u_value *env, u_value *args) {
  return f_parse_46_expr_op1_2(args[0], args[1]);
}

u_value w_f_parse_46_expr_prefix_2(u_value *env, u_value *args) {
  return f_parse_46_expr_prefix_2(args[0], args[1]);
}

u_value w_f_parse_46_expr_op2_3(u_value *env, u_value *args) {
  return f_parse_46_expr_op2_3(args[0], args[1], args[2]);
}

u_value w_f_parse_46_cons_exprs_2(u_value *env, u_value *args) {
  return f_parse_46_cons_exprs_2(args[0], args[1]);
}

u_value w_f_parse_46_starts_expr_2(u_value *env, u_value *args) {
  return f_parse_46_starts_expr_2(args[0], args[1]);
}

u_value w_f_parse_46_at_update_1(u_value *env, u_value *args) {
  return f_parse_46_at_update_1(args[0]);
}

u_value w_f_parse_46_pattern_vars_1(u_value *env, u_value *args) {
  return f_parse_46_pattern_vars_1(args[0]);
}

u_value w_f_parse_46_at_section_1(u_value *env, u_value *args) {
  return f_parse_46_at_section_1(args[0]);
}

u_value w_f_parse_46_nil_expr_0(u_value *env, u_value *args) {
  return f_parse_46_nil_expr_0();
}

u_value w_f_parse_46_in_library_3(u_value *env, u_value *args) {
  return f_parse_46_in_library_3(args[0], args[1], args[2]);
}

u_value w_f_parse_46_hidden_vars_1(u_value *env, u_value *args) {
  return f_parse_46_hidden_vars_1(args[0]);
}

u_value w_f_parse_46_unpack_2(u_value *env, u_value *args) {
  return f_parse_46_unpack_2(args[0], args[1]);
}

u_value w_f_parse_46_pat_op1_2(u_value *env, u_value *args) {
  return f_parse_46_pat_op1_2(args[0], args[1]);
}

u_value w_f_parse_46_pat_prefix_2(u_value *env, u_value *args) {
  return f_parse_46_pat_prefix_2(args[0], args[1]);
}

u_value w_f_parse_46_pat_op2_3(u_value *env, u_value *args) {
  return f_parse_46_pat_op2_3(args[0], args[1], args[2]);
}

u_value w_f_parse_46_cons_pats_2(u_value *env, u_value *args) {
  return f_parse_46_cons_pats_2(args[0], args[1]);
}

u_value w_f_parse_46_starts_pattern_2(u_value *env, u_value *args) {
  return f_parse_46_starts_pattern_2(args[0], args[1]);
}

u_value w_f_parse_46_starts_stmt_1(u_value *env, u_value *args) {
  return f_parse_46_starts_stmt_1(args[0]);
}

u_value w_f_parse_46_handlers_1(u_value *env, u_value *args) {
  return f_parse_46_handlers_1(args[0]);
}

u_value w_f_parse_46_section_at_3(u_value *env, u_value *args) {
  return f_parse_46_section_at_3(args[0], args[1], args[2]);
}

u_value w_f_parse_46_item_follows_1(u_value *env, u_value *args) {
  return f_parse_46_item_follows_1(args[0]);
}

u_value w_f_parse_46_declarations_1(u_value *env, u_value *args) {
  return f_parse_46_declarations_1(args[0]);
}

u_value w_f_parse_46_notations_of_1(u_value *env, u_value *args) {
  return f_parse_46_notations_of_1(args[0]);
}

u_value w_f_parse_46_is_notation_1(u_value *env, u_value *args) {
  return f_parse_46_is_notation_1(args[0]);
}

u_value w_f_parse_46_Context_46_notations_1(u_value *env, u_value *args) {
  return f_parse_46_Context_46_notations_1(args[0]);
}

u_value w_f_parse_46_Context_46_modules_1(u_value *env, u_value *args) {
  return f_parse_46_Context_46_modules_1(args[0]);
}

u_value w_f_parse_46_Context_46_loading_1(u_value *env, u_value *args) {
  return f_parse_46_Context_46_loading_1(args[0]);
}

u_value w_f_parse_46_Context_46_base_1(u_value *env, u_value *args) {
  return f_parse_46_Context_46_base_1(args[0]);
}

u_value w_f_parse_46_Context_46_dir_1(u_value *env, u_value *args) {
  return f_parse_46_Context_46_dir_1(args[0]);
}

u_value w_f_parse_46_Context_46_notations_58__61__2(u_value *env, u_value *args) {
  return f_parse_46_Context_46_notations_58__61__2(args[0], args[1]);
}

u_value w_f_parse_46_Context_46_modules_58__61__2(u_value *env, u_value *args) {
  return f_parse_46_Context_46_modules_58__61__2(args[0], args[1]);
}

u_value w_f_parse_46_Context_46_loading_58__61__2(u_value *env, u_value *args) {
  return f_parse_46_Context_46_loading_58__61__2(args[0], args[1]);
}

u_value w_f_parse_46_Context_46_base_58__61__2(u_value *env, u_value *args) {
  return f_parse_46_Context_46_base_58__61__2(args[0], args[1]);
}

u_value w_f_parse_46_Context_46_dir_58__61__2(u_value *env, u_value *args) {
  return f_parse_46_Context_46_dir_58__61__2(args[0], args[1]);
}

u_value w_f_parse_46_with_commands_2(u_value *env, u_value *args) {
  return f_parse_46_with_commands_2(args[0], args[1]);
}

u_value w_f_parse_46_with_module_2(u_value *env, u_value *args) {
  return f_parse_46_with_module_2(args[0], args[1]);
}

u_value w_f_parse_46_modules_of_1(u_value *env, u_value *args) {
  return f_parse_46_modules_of_1(args[0]);
}

u_value w_f_parse_46_own_notations_2(u_value *env, u_value *args) {
  return f_parse_46_own_notations_2(args[0], args[1]);
}

u_value w_f_parse_46_field_names_1(u_value *env, u_value *args) {
  return f_parse_46_field_names_1(args[0]);
}

u_value w_f_parse_46_differing_1(u_value *env, u_value *args) {
  return f_parse_46_differing_1(args[0]);
}

u_value w_f_parse_46_type_of_field_1(u_value *env, u_value *args) {
  return f_parse_46_type_of_field_1(args[0]);
}

u_value w_f_parse_46_fields_named_2(u_value *env, u_value *args) {
  return f_parse_46_fields_named_2(args[0], args[1]);
}

u_value w_f_parse_46_fields_of_1(u_value *env, u_value *args) {
  return f_parse_46_fields_of_1(args[0]);
}

u_value w_f_parse_46_named_fields_4(u_value *env, u_value *args) {
  return f_parse_46_named_fields_4(args[0], args[1], args[2], args[3]);
}

u_value w_f_parse_46_accessors_3(u_value *env, u_value *args) {
  return f_parse_46_accessors_3(args[0], args[1], args[2]);
}

u_value w_f_parse_46_accessor_5(u_value *env, u_value *args) {
  return f_parse_46_accessor_5(args[0], args[1], args[2], args[3], args[4]);
}

u_value w_f_parse_46_field_type_1(u_value *env, u_value *args) {
  return f_parse_46_field_type_1(args[0]);
}

u_value w_f_parse_46_field_case_1(u_value *env, u_value *args) {
  return f_parse_46_field_case_1(args[0]);
}

u_value w_f_parse_46_missing_field_2(u_value *env, u_value *args) {
  return f_parse_46_missing_field_2(args[0], args[1]);
}

u_value w_f_parse_46_updaters_3(u_value *env, u_value *args) {
  return f_parse_46_updaters_3(args[0], args[1], args[2]);
}

u_value w_f_parse_46_updater_5(u_value *env, u_value *args) {
  return f_parse_46_updater_5(args[0], args[1], args[2], args[3], args[4]);
}

u_value w_f_parse_46_update_case_1(u_value *env, u_value *args) {
  return f_parse_46_update_case_1(args[0]);
}

u_value w_f_parse_46_positions_1(u_value *env, u_value *args) {
  return f_parse_46_positions_1(args[0]);
}

u_value w_f_parse_46_positions_from_2(u_value *env, u_value *args) {
  return f_parse_46_positions_from_2(args[0], args[1]);
}

u_value w_f_parse_46_base_notations_1(u_value *env, u_value *args) {
  return f_parse_46_base_notations_1(args[0]);
}

u_value w_f_parse_46_last_part_1(u_value *env, u_value *args) {
  return f_parse_46_last_part_1(args[0]);
}

u_value w_f_parse_46_base_name_1(u_value *env, u_value *args) {
  return f_parse_46_base_name_1(args[0]);
}

u_value w_f_parse_46_dir_name_1(u_value *env, u_value *args) {
  return f_parse_46_dir_name_1(args[0]);
}

u_value w_f_map_46_empty_0(u_value *env, u_value *args) {
  return f_map_46_empty_0();
}

u_value w_f_map_46_get_2(u_value *env, u_value *args) {
  return f_map_46_get_2(args[0], args[1]);
}

u_value w_f_map_46_has_2(u_value *env, u_value *args) {
  return f_map_46_has_2(args[0], args[1]);
}

u_value w_f_map_46_put_3(u_value *env, u_value *args) {
  return f_map_46_put_3(args[0], args[1], args[2]);
}

u_value w_f_map_46_entries_1(u_value *env, u_value *args) {
  return f_map_46_entries_1(args[0]);
}

u_value w_f_map_46_entries_onto_2(u_value *env, u_value *args) {
  return f_map_46_entries_onto_2(args[0], args[1]);
}

u_value w_f_map_46_size_1(u_value *env, u_value *args) {
  return f_map_46_size_1(args[0]);
}

u_value w_f_map_46_height_1(u_value *env, u_value *args) {
  return f_map_46_height_1(args[0]);
}

u_value w_f_map_46_mnode_4(u_value *env, u_value *args) {
  return f_map_46_mnode_4(args[0], args[1], args[2], args[3]);
}

u_value w_f_map_46_rotate_left_1(u_value *env, u_value *args) {
  return f_map_46_rotate_left_1(args[0]);
}

u_value w_f_map_46_rotate_right_1(u_value *env, u_value *args) {
  return f_map_46_rotate_right_1(args[0]);
}

u_value w_f_map_46_left_of_1(u_value *env, u_value *args) {
  return f_map_46_left_of_1(args[0]);
}

u_value w_f_map_46_right_of_1(u_value *env, u_value *args) {
  return f_map_46_right_of_1(args[0]);
}

u_value w_f_map_46_balance_4(u_value *env, u_value *args) {
  return f_map_46_balance_4(args[0], args[1], args[2], args[3]);
}

u_value w_f_map_46_balanced_1(u_value *env, u_value *args) {
  return f_map_46_balanced_1(args[0]);
}

u_value w_f_map_46_mnode_height_2(u_value *env, u_value *args) {
  return f_map_46_mnode_height_2(args[0], args[1]);
}

u_value w_f_map_46_delete_2(u_value *env, u_value *args) {
  return f_map_46_delete_2(args[0], args[1]);
}

u_value w_f_map_46_delete_all_2(u_value *env, u_value *args) {
  return f_map_46_delete_all_2(args[0], args[1]);
}

u_value w_f_map_46_join_2(u_value *env, u_value *args) {
  return f_map_46_join_2(args[0], args[1]);
}

u_value w_f_map_46_first_entry_1(u_value *env, u_value *args) {
  return f_map_46_first_entry_1(args[0]);
}

u_value w_f_map_46_remove_first_1(u_value *env, u_value *args) {
  return f_map_46_remove_first_1(args[0]);
}

u_value w_f_map_46_put_all_2(u_value *env, u_value *args) {
  return f_map_46_put_all_2(args[0], args[1]);
}

u_value w_f_map_46_numbers_1(u_value *env, u_value *args) {
  return f_map_46_numbers_1(args[0]);
}

u_value w_f_ctx_46_Ctx_46_aliases_1(u_value *env, u_value *args) {
  return f_ctx_46_Ctx_46_aliases_1(args[0]);
}

u_value w_f_ctx_46_Ctx_46_funs_1(u_value *env, u_value *args) {
  return f_ctx_46_Ctx_46_funs_1(args[0]);
}

u_value w_f_ctx_46_Ctx_46_procs_1(u_value *env, u_value *args) {
  return f_ctx_46_Ctx_46_procs_1(args[0]);
}

u_value w_f_ctx_46_Ctx_46_datas_1(u_value *env, u_value *args) {
  return f_ctx_46_Ctx_46_datas_1(args[0]);
}

u_value w_f_ctx_46_Ctx_46_members_1(u_value *env, u_value *args) {
  return f_ctx_46_Ctx_46_members_1(args[0]);
}

u_value w_f_ctx_46_Ctx_46_constrs_1(u_value *env, u_value *args) {
  return f_ctx_46_Ctx_46_constrs_1(args[0]);
}

u_value w_f_ctx_46_Ctx_46_named_1(u_value *env, u_value *args) {
  return f_ctx_46_Ctx_46_named_1(args[0]);
}

u_value w_f_ctx_46_Ctx_46_aliases_58__61__2(u_value *env, u_value *args) {
  return f_ctx_46_Ctx_46_aliases_58__61__2(args[0], args[1]);
}

u_value w_f_ctx_46_Ctx_46_funs_58__61__2(u_value *env, u_value *args) {
  return f_ctx_46_Ctx_46_funs_58__61__2(args[0], args[1]);
}

u_value w_f_ctx_46_Ctx_46_procs_58__61__2(u_value *env, u_value *args) {
  return f_ctx_46_Ctx_46_procs_58__61__2(args[0], args[1]);
}

u_value w_f_ctx_46_Ctx_46_datas_58__61__2(u_value *env, u_value *args) {
  return f_ctx_46_Ctx_46_datas_58__61__2(args[0], args[1]);
}

u_value w_f_ctx_46_Ctx_46_members_58__61__2(u_value *env, u_value *args) {
  return f_ctx_46_Ctx_46_members_58__61__2(args[0], args[1]);
}

u_value w_f_ctx_46_Ctx_46_constrs_58__61__2(u_value *env, u_value *args) {
  return f_ctx_46_Ctx_46_constrs_58__61__2(args[0], args[1]);
}

u_value w_f_ctx_46_Ctx_46_named_58__61__2(u_value *env, u_value *args) {
  return f_ctx_46_Ctx_46_named_58__61__2(args[0], args[1]);
}

u_value w_f_ctx_46_empty_ctx_0(u_value *env, u_value *args) {
  return f_ctx_46_empty_ctx_0();
}

u_value w_f_ctx_46_ctx_of_2(u_value *env, u_value *args) {
  return f_ctx_46_ctx_of_2(args[0], args[1]);
}

u_value w_f_ctx_46_owned_map_2(u_value *env, u_value *args) {
  return f_ctx_46_owned_map_2(args[0], args[1]);
}

u_value w_f_ctx_46_owned_2(u_value *env, u_value *args) {
  return f_ctx_46_owned_2(args[0], args[1]);
}

u_value w_f_ctx_46_owned_named_2(u_value *env, u_value *args) {
  return f_ctx_46_owned_named_2(args[0], args[1]);
}

u_value w_f_ctx_46_in_modules_2(u_value *env, u_value *args) {
  return f_ctx_46_in_modules_2(args[0], args[1]);
}

u_value w_f_ctx_46_ctx_union_2(u_value *env, u_value *args) {
  return f_ctx_46_ctx_union_2(args[0], args[1]);
}

u_value w_f_ctx_46_union_named_2(u_value *env, u_value *args) {
  return f_ctx_46_union_named_2(args[0], args[1]);
}

u_value w_f_ctx_46_alias_of_2(u_value *env, u_value *args) {
  return f_ctx_46_alias_of_2(args[0], args[1]);
}

u_value w_f_ctx_46_schemes_of_2(u_value *env, u_value *args) {
  return f_ctx_46_schemes_of_2(args[0], args[1]);
}

u_value w_f_ctx_46_proc_of_2(u_value *env, u_value *args) {
  return f_ctx_46_proc_of_2(args[0], args[1]);
}

u_value w_f_ctx_46_data_of_2(u_value *env, u_value *args) {
  return f_ctx_46_data_of_2(args[0], args[1]);
}

u_value w_f_ctx_46_members_of_2(u_value *env, u_value *args) {
  return f_ctx_46_members_of_2(args[0], args[1]);
}

u_value w_f_ctx_46_constr_of_2(u_value *env, u_value *args) {
  return f_ctx_46_constr_of_2(args[0], args[1]);
}

u_value w_f_ctx_46_is_fun_2(u_value *env, u_value *args) {
  return f_ctx_46_is_fun_2(args[0], args[1]);
}

u_value w_f_ctx_46_is_proc_2(u_value *env, u_value *args) {
  return f_ctx_46_is_proc_2(args[0], args[1]);
}

u_value w_f_ctx_46_is_data_2(u_value *env, u_value *args) {
  return f_ctx_46_is_data_2(args[0], args[1]);
}

u_value w_f_ctx_46_is_alias_2(u_value *env, u_value *args) {
  return f_ctx_46_is_alias_2(args[0], args[1]);
}

u_value w_f_ctx_46_is_constr_2(u_value *env, u_value *args) {
  return f_ctx_46_is_constr_2(args[0], args[1]);
}

u_value w_f_ctx_46_is_sort_2(u_value *env, u_value *args) {
  return f_ctx_46_is_sort_2(args[0], args[1]);
}

u_value w_f_ctx_46_names_of_2(u_value *env, u_value *args) {
  return f_ctx_46_names_of_2(args[0], args[1]);
}

u_value w_f_ctx_46_with_named_2(u_value *env, u_value *args) {
  return f_ctx_46_with_named_2(args[0], args[1]);
}

u_value w_f_ctx_46_with_alias_4(u_value *env, u_value *args) {
  return f_ctx_46_with_alias_4(args[0], args[1], args[2], args[3]);
}

u_value w_f_ctx_46_with_fun_3(u_value *env, u_value *args) {
  return f_ctx_46_with_fun_3(args[0], args[1], args[2]);
}

u_value w_f_ctx_46_with_proc_3(u_value *env, u_value *args) {
  return f_ctx_46_with_proc_3(args[0], args[1], args[2]);
}

u_value w_f_ctx_46_with_data_3(u_value *env, u_value *args) {
  return f_ctx_46_with_data_3(args[0], args[1], args[2]);
}

u_value w_f_ctx_46_with_members_3(u_value *env, u_value *args) {
  return f_ctx_46_with_members_3(args[0], args[1], args[2]);
}

u_value w_f_ctx_46_with_constr_3(u_value *env, u_value *args) {
  return f_ctx_46_with_constr_3(args[0], args[1], args[2]);
}

u_value w_f_check_46_State_46_ctx_1(u_value *env, u_value *args) {
  return f_check_46_State_46_ctx_1(args[0]);
}

u_value w_f_check_46_State_46_sub_1(u_value *env, u_value *args) {
  return f_check_46_State_46_sub_1(args[0]);
}

u_value w_f_check_46_State_46_count_1(u_value *env, u_value *args) {
  return f_check_46_State_46_count_1(args[0]);
}

u_value w_f_check_46_State_46_module_1(u_value *env, u_value *args) {
  return f_check_46_State_46_module_1(args[0]);
}

u_value w_f_check_46_State_46_visible_1(u_value *env, u_value *args) {
  return f_check_46_State_46_visible_1(args[0]);
}

u_value w_f_check_46_State_46_table_1(u_value *env, u_value *args) {
  return f_check_46_State_46_table_1(args[0]);
}

u_value w_f_check_46_State_46_ctx_58__61__2(u_value *env, u_value *args) {
  return f_check_46_State_46_ctx_58__61__2(args[0], args[1]);
}

u_value w_f_check_46_State_46_sub_58__61__2(u_value *env, u_value *args) {
  return f_check_46_State_46_sub_58__61__2(args[0], args[1]);
}

u_value w_f_check_46_State_46_count_58__61__2(u_value *env, u_value *args) {
  return f_check_46_State_46_count_58__61__2(args[0], args[1]);
}

u_value w_f_check_46_State_46_module_58__61__2(u_value *env, u_value *args) {
  return f_check_46_State_46_module_58__61__2(args[0], args[1]);
}

u_value w_f_check_46_State_46_visible_58__61__2(u_value *env, u_value *args) {
  return f_check_46_State_46_visible_58__61__2(args[0], args[1]);
}

u_value w_f_check_46_State_46_table_58__61__2(u_value *env, u_value *args) {
  return f_check_46_State_46_table_58__61__2(args[0], args[1]);
}

u_value w_f_check_46_with_ctx_2(u_value *env, u_value *args) {
  return f_check_46_with_ctx_2(args[0], args[1]);
}

u_value w_f_check_46_enter_2(u_value *env, u_value *args) {
  return f_check_46_enter_2(args[0], args[1]);
}

u_value w_f_check_46_sees_2(u_value *env, u_value *args) {
  return f_check_46_sees_2(args[0], args[1]);
}

u_value w_f_check_46_start_2(u_value *env, u_value *args) {
  return f_check_46_start_2(args[0], args[1]);
}

u_value w_f_check_46_flatten_2(u_value *env, u_value *args) {
  return f_check_46_flatten_2(args[0], args[1]);
}

u_value w_f_check_46_in_module_2(u_value *env, u_value *args) {
  return f_check_46_in_module_2(args[0], args[1]);
}

u_value w_f_check_46_qualify_2(u_value *env, u_value *args) {
  return f_check_46_qualify_2(args[0], args[1]);
}

u_value w_f_check_46_qualify_constr_2(u_value *env, u_value *args) {
  return f_check_46_qualify_constr_2(args[0], args[1]);
}

u_value w_f_check_46_imports_in_2(u_value *env, u_value *args) {
  return f_check_46_imports_in_2(args[0], args[1]);
}

u_value w_f_check_46_visibility_2(u_value *env, u_value *args) {
  return f_check_46_visibility_2(args[0], args[1]);
}

u_value w_f_check_46_imports_of_2(u_value *env, u_value *args) {
  return f_check_46_imports_of_2(args[0], args[1]);
}

u_value w_f_check_46_visible_names_3(u_value *env, u_value *args) {
  return f_check_46_visible_names_3(args[0], args[1], args[2]);
}

u_value w_f_check_46_is_visible_2(u_value *env, u_value *args) {
  return f_check_46_is_visible_2(args[0], args[1]);
}

u_value w_f_check_46_tbool_0(u_value *env, u_value *args) {
  return f_check_46_tbool_0();
}

u_value w_f_check_46_tint_0(u_value *env, u_value *args) {
  return f_check_46_tint_0();
}

u_value w_f_check_46_tstr_0(u_value *env, u_value *args) {
  return f_check_46_tstr_0();
}

u_value w_f_check_46_tchar_0(u_value *env, u_value *args) {
  return f_check_46_tchar_0();
}

u_value w_f_check_46_tlist_1(u_value *env, u_value *args) {
  return f_check_46_tlist_1(args[0]);
}

u_value w_f_check_46_fun_2(u_value *env, u_value *args) {
  return f_check_46_fun_2(args[0], args[1]);
}

u_value w_f_check_46_params_1(u_value *env, u_value *args) {
  return f_check_46_params_1(args[0]);
}

u_value w_f_check_46_basic_1(u_value *env, u_value *args) {
  return f_check_46_basic_1(args[0]);
}

u_value w_f_check_46_literal_1(u_value *env, u_value *args) {
  return f_check_46_literal_1(args[0]);
}

u_value w_f_check_46_initial_0(u_value *env, u_value *args) {
  return f_check_46_initial_0();
}

u_value w_f_check_46_arith_0(u_value *env, u_value *args) {
  return f_check_46_arith_0();
}

u_value w_f_check_46_logic_0(u_value *env, u_value *args) {
  return f_check_46_logic_0();
}

u_value w_f_check_46_builtin_1(u_value *env, u_value *args) {
  return f_check_46_builtin_1(args[0]);
}

u_value w_f_check_46_builtin_scheme_1(u_value *env, u_value *args) {
  return f_check_46_builtin_scheme_1(args[0]);
}

u_value w_f_check_46_is_meta_1(u_value *env, u_value *args) {
  return f_check_46_is_meta_1(args[0]);
}

u_value w_f_check_46_free_1(u_value *env, u_value *args) {
  return f_check_46_free_1(args[0]);
}

u_value w_f_check_46_holes_1(u_value *env, u_value *args) {
  return f_check_46_holes_1(args[0]);
}

u_value w_f_check_46_subst_2(u_value *env, u_value *args) {
  return f_check_46_subst_2(args[0], args[1]);
}

u_value w_f_check_46_resolve_2(u_value *env, u_value *args) {
  return f_check_46_resolve_2(args[0], args[1]);
}

u_value w_f_check_46_occurs_3(u_value *env, u_value *args) {
  return f_check_46_occurs_3(args[0], args[1], args[2]);
}

u_value w_f_check_46_constructor_scheme_1(u_value *env, u_value *args) {
  return f_check_46_constructor_scheme_1(args[0]);
}

u_value w_f_check_46_constructor_of_2(u_value *env, u_value *args) {
  return f_check_46_constructor_of_2(args[0], args[1]);
}

u_value w_f_check_46_candidates_3(u_value *env, u_value *args) {
  return f_check_46_candidates_3(args[0], args[1], args[2]);
}

u_value w_f_check_46_proc_scheme_2(u_value *env, u_value *args) {
  return f_check_46_proc_scheme_2(args[0], args[1]);
}

u_value w_f_check_46_with_arity_2(u_value *env, u_value *args) {
  return f_check_46_with_arity_2(args[0], args[1]);
}

u_value w_f_check_46_has_arity_2(u_value *env, u_value *args) {
  return f_check_46_has_arity_2(args[0], args[1]);
}

u_value w_f_check_46_bind_1(u_value *env, u_value *args) {
  return f_check_46_bind_1(args[0]);
}

u_value w_f_check_46_fixed_2(u_value *env, u_value *args) {
  return f_check_46_fixed_2(args[0], args[1]);
}

u_value w_f_check_46_generalize_3(u_value *env, u_value *args) {
  return f_check_46_generalize_3(args[0], args[1], args[2]);
}

u_value w_f_check_46_generalized_3(u_value *env, u_value *args) {
  return f_check_46_generalized_3(args[0], args[1], args[2]);
}

u_value w_f_check_46_case_of_1(u_value *env, u_value *args) {
  return f_check_46_case_of_1(args[0]);
}

u_value w_f_check_46_zonk_guard_2(u_value *env, u_value *args) {
  return f_check_46_zonk_guard_2(args[0], args[1]);
}

u_value w_f_check_46_zonk_type_2(u_value *env, u_value *args) {
  return f_check_46_zonk_type_2(args[0], args[1]);
}

u_value w_f_check_46_zonk_var_2(u_value *env, u_value *args) {
  return f_check_46_zonk_var_2(args[0], args[1]);
}

u_value w_f_check_46_zonk_vars_2(u_value *env, u_value *args) {
  return f_check_46_zonk_vars_2(args[0], args[1]);
}

u_value w_f_check_46_zonk_expr_2(u_value *env, u_value *args) {
  return f_check_46_zonk_expr_2(args[0], args[1]);
}

u_value w_f_check_46_zonk_binding_2(u_value *env, u_value *args) {
  return f_check_46_zonk_binding_2(args[0], args[1]);
}

u_value w_f_check_46_zonk_expr_branch_2(u_value *env, u_value *args) {
  return f_check_46_zonk_expr_branch_2(args[0], args[1]);
}

u_value w_f_check_46_zonk_pat_2(u_value *env, u_value *args) {
  return f_check_46_zonk_pat_2(args[0], args[1]);
}

u_value w_f_check_46_zonk_stmt_2(u_value *env, u_value *args) {
  return f_check_46_zonk_stmt_2(args[0], args[1]);
}

u_value w_f_check_46_zonk_stmt_branch_2(u_value *env, u_value *args) {
  return f_check_46_zonk_stmt_branch_2(args[0], args[1]);
}

u_value w_f_check_46_zonk_handler_2(u_value *env, u_value *args) {
  return f_check_46_zonk_handler_2(args[0], args[1]);
}

u_value w_f_check_46_zonk_cmd_2(u_value *env, u_value *args) {
  return f_check_46_zonk_cmd_2(args[0], args[1]);
}

u_value w_f_check_46_unknown_type_1(u_value *env, u_value *args) {
  return f_check_46_unknown_type_1(args[0]);
}

u_value w_f_check_46_unknown_in_1(u_value *env, u_value *args) {
  return f_check_46_unknown_in_1(args[0]);
}

u_value w_f_check_46_unknown_in_expr_1(u_value *env, u_value *args) {
  return f_check_46_unknown_in_expr_1(args[0]);
}

u_value w_f_check_46_unknown_in_exprs_1(u_value *env, u_value *args) {
  return f_check_46_unknown_in_exprs_1(args[0]);
}

u_value w_f_check_46_branch_exprs_1(u_value *env, u_value *args) {
  return f_check_46_branch_exprs_1(args[0]);
}

u_value w_f_check_46_unknown_in_all_1(u_value *env, u_value *args) {
  return f_check_46_unknown_in_all_1(args[0]);
}

u_value w_f_check_46_unknown_in_branches_1(u_value *env, u_value *args) {
  return f_check_46_unknown_in_branches_1(args[0]);
}

u_value w_f_check_46_unknown_in_handlers_1(u_value *env, u_value *args) {
  return f_check_46_unknown_in_handlers_1(args[0]);
}

u_value w_f_check_46_unknown_in_cmd_1(u_value *env, u_value *args) {
  return f_check_46_unknown_in_cmd_1(args[0]);
}

u_value w_f_check_46_head_of_1(u_value *env, u_value *args) {
  return f_check_46_head_of_1(args[0]);
}

u_value w_f_check_46_heads_1(u_value *env, u_value *args) {
  return f_check_46_heads_1(args[0]);
}

u_value w_f_check_46_field_types_3(u_value *env, u_value *args) {
  return f_check_46_field_types_3(args[0], args[1], args[2]);
}

u_value w_f_check_46_constructors_of_3(u_value *env, u_value *args) {
  return f_check_46_constructors_of_3(args[0], args[1], args[2]);
}

u_value w_f_check_46_fields_of_4(u_value *env, u_value *args) {
  return f_check_46_fields_of_4(args[0], args[1], args[2], args[3]);
}

u_value w_f_check_46_covers_2(u_value *env, u_value *args) {
  return f_check_46_covers_2(args[0], args[1]);
}

u_value w_f_check_46_defaults_1(u_value *env, u_value *args) {
  return f_check_46_defaults_1(args[0]);
}

u_value w_f_check_46_specialize_3(u_value *env, u_value *args) {
  return f_check_46_specialize_3(args[0], args[1], args[2]);
}

u_value w_f_check_46_useful_5(u_value *env, u_value *args) {
  return f_check_46_useful_5(args[0], args[1], args[2], args[3], args[4]);
}

u_value w_f_check_46_useful_in_6(u_value *env, u_value *args) {
  return f_check_46_useful_in_6(args[0], args[1], args[2], args[3], args[4], args[5]);
}

u_value w_f_check_46_witness_2(u_value *env, u_value *args) {
  return f_check_46_witness_2(args[0], args[1]);
}

u_value w_f_check_46_missing_4(u_value *env, u_value *args) {
  return f_check_46_missing_4(args[0], args[1], args[2], args[3]);
}

u_value w_f_check_46_missing_in_6(u_value *env, u_value *args) {
  return f_check_46_missing_in_6(args[0], args[1], args[2], args[3], args[4], args[5]);
}

u_value w_f_check_46_first_missing_5(u_value *env, u_value *args) {
  return f_check_46_first_missing_5(args[0], args[1], args[2], args[3], args[4]);
}

u_value w_f_check_46_other_missing_6(u_value *env, u_value *args) {
  return f_check_46_other_missing_6(args[0], args[1], args[2], args[3], args[4], args[5]);
}

u_value w_f_check_46_uncovered_2(u_value *env, u_value *args) {
  return f_check_46_uncovered_2(args[0], args[1]);
}

u_value w_f_check_46_first_uncovered_2(u_value *env, u_value *args) {
  return f_check_46_first_uncovered_2(args[0], args[1]);
}

u_value w_f_check_46_unguarded_1(u_value *env, u_value *args) {
  return f_check_46_unguarded_1(args[0]);
}

u_value w_f_check_46_initialized_1(u_value *env, u_value *args) {
  return f_check_46_initialized_1(args[0]);
}

u_value w_f_check_46_os_procs_1(u_value *env, u_value *args) {
  return f_check_46_os_procs_1(args[0]);
}

u_value w_f_check_46_syntax_procs_1(u_value *env, u_value *args) {
  return f_check_46_syntax_procs_1(args[0]);
}

u_value w_f_check_46_curried_2(u_value *env, u_value *args) {
  return f_check_46_curried_2(args[0], args[1]);
}

u_value w_f_check_46_first_list_1(u_value *env, u_value *args) {
  return f_check_46_first_list_1(args[0]);
}

u_value w_f_check_46_block_names_1(u_value *env, u_value *args) {
  return f_check_46_block_names_1(args[0]);
}

u_value w_f_check_46_Checked_46_cmds_1(u_value *env, u_value *args) {
  return f_check_46_Checked_46_cmds_1(args[0]);
}

u_value w_f_check_46_Checked_46_ctx_1(u_value *env, u_value *args) {
  return f_check_46_Checked_46_ctx_1(args[0]);
}

u_value w_f_check_46_Checked_46_imports_1(u_value *env, u_value *args) {
  return f_check_46_Checked_46_imports_1(args[0]);
}

u_value w_f_check_46_Checked_46_cmds_58__61__2(u_value *env, u_value *args) {
  return f_check_46_Checked_46_cmds_58__61__2(args[0], args[1]);
}

u_value w_f_check_46_Checked_46_ctx_58__61__2(u_value *env, u_value *args) {
  return f_check_46_Checked_46_ctx_58__61__2(args[0], args[1]);
}

u_value w_f_check_46_Checked_46_imports_58__61__2(u_value *env, u_value *args) {
  return f_check_46_Checked_46_imports_58__61__2(args[0], args[1]);
}

u_value w_f_check_46_positions_of_2(u_value *env, u_value *args) {
  return f_check_46_positions_of_2(args[0], args[1]);
}

u_value w_f_check_46_position_in_3(u_value *env, u_value *args) {
  return f_check_46_position_in_3(args[0], args[1], args[2]);
}

u_value w_f_check_46_modules_1(u_value *env, u_value *args) {
  return f_check_46_modules_1(args[0]);
}

u_value w_f_check_46_own_decls_2(u_value *env, u_value *args) {
  return f_check_46_own_decls_2(args[0], args[1]);
}

u_value w_f_check_46_is_module_1(u_value *env, u_value *args) {
  return f_check_46_is_module_1(args[0]);
}

u_value w_f_check_46_ev_1(u_value *env, u_value *args) {
  return f_check_46_ev_1(args[0]);
}

u_value w_f_check_46_pv_1(u_value *env, u_value *args) {
  return f_check_46_pv_1(args[0]);
}

u_value w_f_check_46_num_1(u_value *env, u_value *args) {
  return f_check_46_num_1(args[0]);
}

u_value w_f_check_46_ap_2(u_value *env, u_value *args) {
  return f_check_46_ap_2(args[0], args[1]);
}

u_value w_f_check_46_tl_1(u_value *env, u_value *args) {
  return f_check_46_tl_1(args[0]);
}

u_value w_f_check_46_lists_0(u_value *env, u_value *args) {
  return f_check_46_lists_0();
}

u_value w_f_value_46_list_value_1(u_value *env, u_value *args) {
  return f_value_46_list_value_1(args[0]);
}

u_value w_f_value_46_list_elements_1(u_value *env, u_value *args) {
  return f_value_46_list_elements_1(args[0]);
}

u_value w_f_print_46_show_value_1(u_value *env, u_value *args) {
  return f_print_46_show_value_1(args[0]);
}

u_value w_f_print_46_show_values_1(u_value *env, u_value *args) {
  return f_print_46_show_values_1(args[0]);
}

u_value w_f_print_46_list_items_1(u_value *env, u_value *args) {
  return f_print_46_list_items_1(args[0]);
}

u_value w_f_print_46_escape_char_1(u_value *env, u_value *args) {
  return f_print_46_escape_char_1(args[0]);
}

u_value w_f_print_46_show_type_1(u_value *env, u_value *args) {
  return f_print_46_show_type_1(args[0]);
}

u_value w_f_print_46_show_types_1(u_value *env, u_value *args) {
  return f_print_46_show_types_1(args[0]);
}

u_value w_f_print_46_type_section_2(u_value *env, u_value *args) {
  return f_print_46_type_section_2(args[0], args[1]);
}

u_value w_f_print_46_type_operand_1(u_value *env, u_value *args) {
  return f_print_46_type_operand_1(args[0]);
}

u_value w_f_print_46_type_component_1(u_value *env, u_value *args) {
  return f_print_46_type_component_1(args[0]);
}

u_value w_f_print_46_show_pat_1(u_value *env, u_value *args) {
  return f_print_46_show_pat_1(args[0]);
}

u_value w_f_print_46_show_pats_1(u_value *env, u_value *args) {
  return f_print_46_show_pats_1(args[0]);
}

u_value w_f_print_46_symbolic_1(u_value *env, u_value *args) {
  return f_print_46_symbolic_1(args[0]);
}

u_value w_f_print_46_infix_text_3(u_value *env, u_value *args) {
  return f_print_46_infix_text_3(args[0], args[1], args[2]);
}

u_value w_f_print_46_pat_operand_1(u_value *env, u_value *args) {
  return f_print_46_pat_operand_1(args[0]);
}

u_value w_f_print_46_text_of_1(u_value *env, u_value *args) {
  return f_print_46_text_of_1(args[0]);
}

u_value w_f_run_46_Prog_46_funs_1(u_value *env, u_value *args) {
  return f_run_46_Prog_46_funs_1(args[0]);
}

u_value w_f_run_46_Prog_46_procs_1(u_value *env, u_value *args) {
  return f_run_46_Prog_46_procs_1(args[0]);
}

u_value w_f_run_46_Prog_46_constrs_1(u_value *env, u_value *args) {
  return f_run_46_Prog_46_constrs_1(args[0]);
}

u_value w_f_run_46_Prog_46_positions_1(u_value *env, u_value *args) {
  return f_run_46_Prog_46_positions_1(args[0]);
}

u_value w_f_run_46_Prog_46_computing_1(u_value *env, u_value *args) {
  return f_run_46_Prog_46_computing_1(args[0]);
}

u_value w_f_run_46_Prog_46_args_1(u_value *env, u_value *args) {
  return f_run_46_Prog_46_args_1(args[0]);
}

u_value w_f_run_46_Prog_46_funs_58__61__2(u_value *env, u_value *args) {
  return f_run_46_Prog_46_funs_58__61__2(args[0], args[1]);
}

u_value w_f_run_46_Prog_46_procs_58__61__2(u_value *env, u_value *args) {
  return f_run_46_Prog_46_procs_58__61__2(args[0], args[1]);
}

u_value w_f_run_46_Prog_46_constrs_58__61__2(u_value *env, u_value *args) {
  return f_run_46_Prog_46_constrs_58__61__2(args[0], args[1]);
}

u_value w_f_run_46_Prog_46_positions_58__61__2(u_value *env, u_value *args) {
  return f_run_46_Prog_46_positions_58__61__2(args[0], args[1]);
}

u_value w_f_run_46_Prog_46_computing_58__61__2(u_value *env, u_value *args) {
  return f_run_46_Prog_46_computing_58__61__2(args[0], args[1]);
}

u_value w_f_run_46_Prog_46_args_58__61__2(u_value *env, u_value *args) {
  return f_run_46_Prog_46_args_58__61__2(args[0], args[1]);
}

u_value w_f_run_46_position_of_2(u_value *env, u_value *args) {
  return f_run_46_position_of_2(args[0], args[1]);
}

u_value w_f_run_46_load_1(u_value *env, u_value *args) {
  return f_run_46_load_1(args[0]);
}

u_value w_f_run_46_fun_defs_1(u_value *env, u_value *args) {
  return f_run_46_fun_defs_1(args[0]);
}

u_value w_f_run_46_proc_defs_1(u_value *env, u_value *args) {
  return f_run_46_proc_defs_1(args[0]);
}

u_value w_f_run_46_constr_positions_1(u_value *env, u_value *args) {
  return f_run_46_constr_positions_1(args[0]);
}

u_value w_f_run_46_numbers_from_2(u_value *env, u_value *args) {
  return f_run_46_numbers_from_2(args[0], args[1]);
}

u_value w_f_run_46_constr_defs_1(u_value *env, u_value *args) {
  return f_run_46_constr_defs_1(args[0]);
}

u_value w_f_run_46_constr_arity_1(u_value *env, u_value *args) {
  return f_run_46_constr_arity_1(args[0]);
}

u_value w_f_run_46_builtin_names_0(u_value *env, u_value *args) {
  return f_run_46_builtin_names_0();
}

u_value w_f_run_46_is_builtin_1(u_value *env, u_value *args) {
  return f_run_46_is_builtin_1(args[0]);
}

u_value w_f_run_46_is_function_1(u_value *env, u_value *args) {
  return f_run_46_is_function_1(args[0]);
}

u_value w_f_run_46_truth_1(u_value *env, u_value *args) {
  return f_run_46_truth_1(args[0]);
}

u_value w_f_run_46_lambdas_2(u_value *env, u_value *args) {
  return f_run_46_lambdas_2(args[0], args[1]);
}

u_value w_f_run_46_str_values_1(u_value *env, u_value *args) {
  return f_run_46_str_values_1(args[0]);
}

u_value w_f_run_46_char_values_1(u_value *env, u_value *args) {
  return f_run_46_char_values_1(args[0]);
}

u_value w_f_run_46_bind_all_3(u_value *env, u_value *args) {
  return f_run_46_bind_all_3(args[0], args[1], args[2]);
}

u_value w_f_run_46_match_into_3(u_value *env, u_value *args) {
  return f_run_46_match_into_3(args[0], args[1], args[2]);
}

u_value w_f_run_46_match_all_into_3(u_value *env, u_value *args) {
  return f_run_46_match_all_into_3(args[0], args[1], args[2]);
}

u_value w_f_run_46_pattern_vars_1(u_value *env, u_value *args) {
  return f_run_46_pattern_vars_1(args[0]);
}

u_value w_f_run_46_variable_value_4(u_value *env, u_value *args) {
  return f_run_46_variable_value_4(args[0], args[1], args[2], args[3]);
}

u_value w_f_run_46_failure_2(u_value *env, u_value *args) {
  return f_run_46_failure_2(args[0], args[1]);
}

u_value w_f_run_46_order_2(u_value *env, u_value *args) {
  return f_run_46_order_2(args[0], args[1]);
}

u_value w_f_run_46_restored_3(u_value *env, u_value *args) {
  return f_run_46_restored_3(args[0], args[1], args[2]);
}

u_value w_f_run_46_values_in_2(u_value *env, u_value *args) {
  return f_run_46_values_in_2(args[0], args[1]);
}

u_value w_f_run_46_read_outs_3(u_value *env, u_value *args) {
  return f_run_46_read_outs_3(args[0], args[1], args[2]);
}

u_value w_f_run_46_asserting_3(u_value *env, u_value *args) {
  return f_run_46_asserting_3(args[0], args[1], args[2]);
}

u_value w_f_run_46_assigning_1(u_value *env, u_value *args) {
  return f_run_46_assigning_1(args[0]);
}

u_value w_f_run_46_int_1(u_value *env, u_value *args) {
  return f_run_46_int_1(args[0]);
}

u_value w_f_run_46_procedures_0(u_value *env, u_value *args) {
  return f_run_46_procedures_0();
}

u_value w_f_run_46_hole_var_1(u_value *env, u_value *args) {
  return f_run_46_hole_var_1(args[0]);
}

u_value w_f_run_46_equals_2(u_value *env, u_value *args) {
  return f_run_46_equals_2(args[0], args[1]);
}

u_value w_f_c_46_c_type_1(u_value *env, u_value *args) {
  return f_c_46_c_type_1(args[0]);
}

u_value w_f_c_46_c_declare_2(u_value *env, u_value *args) {
  return f_c_46_c_declare_2(args[0], args[1]);
}

u_value w_f_c_46_c_expr_1(u_value *env, u_value *args) {
  return f_c_46_c_expr_1(args[0]);
}

u_value w_f_c_46_c_operand_1(u_value *env, u_value *args) {
  return f_c_46_c_operand_1(args[0]);
}

u_value w_f_c_46_c_stmt_2(u_value *env, u_value *args) {
  return f_c_46_c_stmt_2(args[0], args[1]);
}

u_value w_f_c_46_c_stmts_2(u_value *env, u_value *args) {
  return f_c_46_c_stmts_2(args[0], args[1]);
}

u_value w_f_c_46_c_params_1(u_value *env, u_value *args) {
  return f_c_46_c_params_1(args[0]);
}

u_value w_f_c_46_c_param_1(u_value *env, u_value *args) {
  return f_c_46_c_param_1(args[0]);
}

u_value w_f_c_46_c_decl_1(u_value *env, u_value *args) {
  return f_c_46_c_decl_1(args[0]);
}

u_value w_f_c_46_c_file_1(u_value *env, u_value *args) {
  return f_c_46_c_file_1(args[0]);
}

u_value w_f_c_46_c_call_2(u_value *env, u_value *args) {
  return f_c_46_c_call_2(args[0], args[1]);
}

u_value w_f_compile_46_Globals_46_funs_1(u_value *env, u_value *args) {
  return f_compile_46_Globals_46_funs_1(args[0]);
}

u_value w_f_compile_46_Globals_46_constrs_1(u_value *env, u_value *args) {
  return f_compile_46_Globals_46_constrs_1(args[0]);
}

u_value w_f_compile_46_Globals_46_arities_1(u_value *env, u_value *args) {
  return f_compile_46_Globals_46_arities_1(args[0]);
}

u_value w_f_compile_46_Globals_46_fields_1(u_value *env, u_value *args) {
  return f_compile_46_Globals_46_fields_1(args[0]);
}

u_value w_f_compile_46_Globals_46_procs_1(u_value *env, u_value *args) {
  return f_compile_46_Globals_46_procs_1(args[0]);
}

u_value w_f_compile_46_Globals_46_funs_58__61__2(u_value *env, u_value *args) {
  return f_compile_46_Globals_46_funs_58__61__2(args[0], args[1]);
}

u_value w_f_compile_46_Globals_46_constrs_58__61__2(u_value *env, u_value *args) {
  return f_compile_46_Globals_46_constrs_58__61__2(args[0], args[1]);
}

u_value w_f_compile_46_Globals_46_arities_58__61__2(u_value *env, u_value *args) {
  return f_compile_46_Globals_46_arities_58__61__2(args[0], args[1]);
}

u_value w_f_compile_46_Globals_46_fields_58__61__2(u_value *env, u_value *args) {
  return f_compile_46_Globals_46_fields_58__61__2(args[0], args[1]);
}

u_value w_f_compile_46_Globals_46_procs_58__61__2(u_value *env, u_value *args) {
  return f_compile_46_Globals_46_procs_58__61__2(args[0], args[1]);
}

u_value w_f_compile_46_Gen_46_count_1(u_value *env, u_value *args) {
  return f_compile_46_Gen_46_count_1(args[0]);
}

u_value w_f_compile_46_Gen_46_lifted_1(u_value *env, u_value *args) {
  return f_compile_46_Gen_46_lifted_1(args[0]);
}

u_value w_f_compile_46_Gen_46_count_58__61__2(u_value *env, u_value *args) {
  return f_compile_46_Gen_46_count_58__61__2(args[0], args[1]);
}

u_value w_f_compile_46_Gen_46_lifted_58__61__2(u_value *env, u_value *args) {
  return f_compile_46_Gen_46_lifted_58__61__2(args[0], args[1]);
}

u_value w_f_compile_46_value_type_0(u_value *env, u_value *args) {
  return f_compile_46_value_type_0();
}

u_value w_f_compile_46_value_ptr_0(u_value *env, u_value *args) {
  return f_compile_46_value_ptr_0();
}

u_value w_f_compile_46_mangle_1(u_value *env, u_value *args) {
  return f_compile_46_mangle_1(args[0]);
}

u_value w_f_compile_46_mangle_char_1(u_value *env, u_value *args) {
  return f_compile_46_mangle_char_1(args[0]);
}

u_value w_f_compile_46_fun_name_2(u_value *env, u_value *args) {
  return f_compile_46_fun_name_2(args[0], args[1]);
}

u_value w_f_compile_46_proc_name_1(u_value *env, u_value *args) {
  return f_compile_46_proc_name_1(args[0]);
}

u_value w_f_compile_46_var_name_1(u_value *env, u_value *args) {
  return f_compile_46_var_name_1(args[0]);
}

u_value w_f_compile_46_ref_name_1(u_value *env, u_value *args) {
  return f_compile_46_ref_name_1(args[0]);
}

u_value w_f_compile_46_builtin_functions_0(u_value *env, u_value *args) {
  return f_compile_46_builtin_functions_0();
}

u_value w_f_compile_46_descriptor_1(u_value *env, u_value *args) {
  return f_compile_46_descriptor_1(args[0]);
}

u_value w_f_compile_46_unit_value_1(u_value *env, u_value *args) {
  return f_compile_46_unit_value_1(args[0]);
}

u_value w_f_compile_46_runtime_constructors_0(u_value *env, u_value *args) {
  return f_compile_46_runtime_constructors_0();
}

u_value w_f_compile_46_descriptor_decl_2(u_value *env, u_value *args) {
  return f_compile_46_descriptor_decl_2(args[0], args[1]);
}

u_value w_f_compile_46_unit_decl_1(u_value *env, u_value *args) {
  return f_compile_46_unit_decl_1(args[0]);
}

u_value w_f_compile_46_descriptor_decls_1(u_value *env, u_value *args) {
  return f_compile_46_descriptor_decls_1(args[0]);
}

u_value w_f_compile_46_constructor_decls_2(u_value *env, u_value *args) {
  return f_compile_46_constructor_decls_2(args[0], args[1]);
}

u_value w_f_compile_46_in_scope_1(u_value *env, u_value *args) {
  return f_compile_46_in_scope_1(args[0]);
}

u_value w_f_compile_46_pointer_1(u_value *env, u_value *args) {
  return f_compile_46_pointer_1(args[0]);
}

u_value w_f_compile_46_var_expr_2(u_value *env, u_value *args) {
  return f_compile_46_var_expr_2(args[0], args[1]);
}

u_value w_f_compile_46_address_1(u_value *env, u_value *args) {
  return f_compile_46_address_1(args[0]);
}

u_value w_f_compile_46_type_key_1(u_value *env, u_value *args) {
  return f_compile_46_type_key_1(args[0]);
}

u_value w_f_compile_46_globals_1(u_value *env, u_value *args) {
  return f_compile_46_globals_1(args[0]);
}

u_value w_f_compile_46_proc_arities_1(u_value *env, u_value *args) {
  return f_compile_46_proc_arities_1(args[0]);
}

u_value w_f_compile_46_arity_map_2(u_value *env, u_value *args) {
  return f_compile_46_arity_map_2(args[0], args[1]);
}

u_value w_f_compile_46_is_global_fun_3(u_value *env, u_value *args) {
  return f_compile_46_is_global_fun_3(args[0], args[1], args[2]);
}

u_value w_f_compile_46_fields_count_2(u_value *env, u_value *args) {
  return f_compile_46_fields_count_2(args[0], args[1]);
}

u_value w_f_compile_46_fun_arities_1(u_value *env, u_value *args) {
  return f_compile_46_fun_arities_1(args[0]);
}

u_value w_f_compile_46_constr_arities_1(u_value *env, u_value *args) {
  return f_compile_46_constr_arities_1(args[0]);
}

u_value w_f_compile_46_constr_arity_1(u_value *env, u_value *args) {
  return f_compile_46_constr_arity_1(args[0]);
}

u_value w_f_compile_46_numbered_2(u_value *env, u_value *args) {
  return f_compile_46_numbered_2(args[0], args[1]);
}

u_value w_f_compile_46_index_2(u_value *env, u_value *args) {
  return f_compile_46_index_2(args[0], args[1]);
}

u_value w_f_compile_46_field_2(u_value *env, u_value *args) {
  return f_compile_46_field_2(args[0], args[1]);
}

u_value w_f_compile_46_truth_1(u_value *env, u_value *args) {
  return f_compile_46_truth_1(args[0]);
}

u_value w_f_compile_46_conjunction_1(u_value *env, u_value *args) {
  return f_compile_46_conjunction_1(args[0]);
}

u_value w_f_compile_46_literal_value_1(u_value *env, u_value *args) {
  return f_compile_46_literal_value_1(args[0]);
}

u_value w_f_compile_46_pattern_tests_2(u_value *env, u_value *args) {
  return f_compile_46_pattern_tests_2(args[0], args[1]);
}

u_value w_f_compile_46_field_tests_2(u_value *env, u_value *args) {
  return f_compile_46_field_tests_2(args[0], args[1]);
}

u_value w_f_compile_46_pattern_binds_2(u_value *env, u_value *args) {
  return f_compile_46_pattern_binds_2(args[0], args[1]);
}

u_value w_f_compile_46_field_binds_2(u_value *env, u_value *args) {
  return f_compile_46_field_binds_2(args[0], args[1]);
}

u_value w_f_compile_46_pattern_names_1(u_value *env, u_value *args) {
  return f_compile_46_pattern_names_1(args[0]);
}

u_value w_f_compile_46_free_vars_2(u_value *env, u_value *args) {
  return f_compile_46_free_vars_2(args[0], args[1]);
}

u_value w_f_compile_46_free_in_stmt_2(u_value *env, u_value *args) {
  return f_compile_46_free_in_stmt_2(args[0], args[1]);
}

u_value w_f_compile_46_free_in_stmt_branch_2(u_value *env, u_value *args) {
  return f_compile_46_free_in_stmt_branch_2(args[0], args[1]);
}

u_value w_f_compile_46_free_in_handler_2(u_value *env, u_value *args) {
  return f_compile_46_free_in_handler_2(args[0], args[1]);
}

u_value w_f_compile_46_free_in_branch_2(u_value *env, u_value *args) {
  return f_compile_46_free_in_branch_2(args[0], args[1]);
}

u_value w_f_compile_46_lambdas_2(u_value *env, u_value *args) {
  return f_compile_46_lambdas_2(args[0], args[1]);
}

u_value w_f_compile_46_global_value_3(u_value *env, u_value *args) {
  return f_compile_46_global_value_3(args[0], args[1], args[2]);
}

u_value w_f_compile_46_rebind_2(u_value *env, u_value *args) {
  return f_compile_46_rebind_2(args[0], args[1]);
}

u_value w_f_compile_46_proc_params_0(u_value *env, u_value *args) {
  return f_compile_46_proc_params_0();
}

u_value w_f_compile_46_unpack_pointers_1(u_value *env, u_value *args) {
  return f_compile_46_unpack_pointers_1(args[0]);
}

u_value w_f_compile_46_unpack_2(u_value *env, u_value *args) {
  return f_compile_46_unpack_2(args[0], args[1]);
}

u_value w_f_compile_46_reassign_3(u_value *env, u_value *args) {
  return f_compile_46_reassign_3(args[0], args[1], args[2]);
}

u_value w_f_compile_46_cached_3(u_value *env, u_value *args) {
  return f_compile_46_cached_3(args[0], args[1], args[2]);
}

u_value w_f_compile_46_wrappers_1(u_value *env, u_value *args) {
  return f_compile_46_wrappers_1(args[0]);
}

u_value w_f_compile_46_proc_wrapper_1(u_value *env, u_value *args) {
  return f_compile_46_proc_wrapper_1(args[0]);
}

u_value w_f_compile_46_fun_wrapper_1(u_value *env, u_value *args) {
  return f_compile_46_fun_wrapper_1(args[0]);
}

u_value w_f_compile_46_builtin_wrapper_1(u_value *env, u_value *args) {
  return f_compile_46_builtin_wrapper_1(args[0]);
}

u_value w_f_compile_46_wrapper_3(u_value *env, u_value *args) {
  return f_compile_46_wrapper_3(args[0], args[1], args[2]);
}

u_value w_f_compile_46_constr_wrapper_1(u_value *env, u_value *args) {
  return f_compile_46_constr_wrapper_1(args[0]);
}

u_value w_f_compile_46_range_to_1(u_value *env, u_value *args) {
  return f_compile_46_range_to_1(args[0]);
}

u_value w_f_compile_46_prototype_1(u_value *env, u_value *args) {
  return f_compile_46_prototype_1(args[0]);
}

u_value w_f_compile_46_is_entry_2(u_value *env, u_value *args) {
  return f_compile_46_is_entry_2(args[0], args[1]);
}

u_value w_f_ulang_46_usage_0(u_value *env, u_value *args) {
  return f_ulang_46_usage_0();
}

u_value w_f_ulang_46_before_dashes_1(u_value *env, u_value *args) {
  return f_ulang_46_before_dashes_1(args[0]);
}

u_value w_f_ulang_46_after_dashes_1(u_value *env, u_value *args) {
  return f_ulang_46_after_dashes_1(args[0]);
}

u_value w_f_ulang_46_in_module_1(u_value *env, u_value *args) {
  return f_ulang_46_in_module_1(args[0]);
}

u_value w_f_ulang_46_module_names_1(u_value *env, u_value *args) {
  return f_ulang_46_module_names_1(args[0]);
}

u_value w_f_ulang_46_is_any_test_1(u_value *env, u_value *args) {
  return f_ulang_46_is_any_test_1(args[0]);
}

u_value w_f_ulang_46_is_selected_test_2(u_value *env, u_value *args) {
  return f_ulang_46_is_selected_test_2(args[0], args[1]);
}

u_value w_f_ulang_46_is_test_2(u_value *env, u_value *args) {
  return f_ulang_46_is_test_2(args[0], args[1]);
}

u_value w_f_ulang_46_cmd_name_1(u_value *env, u_value *args) {
  return f_ulang_46_cmd_name_1(args[0]);
}

u_value w_f_ulang_46_type_message_1(u_value *env, u_value *args) {
  return f_ulang_46_type_message_1(args[0]);
}

u_value w_f_ulang_46_field_message_1(u_value *env, u_value *args) {
  return f_ulang_46_field_message_1(args[0]);
}

u_value w_f_ulang_46_without_update_1(u_value *env, u_value *args) {
  return f_ulang_46_without_update_1(args[0]);
}

u_value w_f_ulang_46_runtime_message_1(u_value *env, u_value *args) {
  return f_ulang_46_runtime_message_1(args[0]);
}

u_value wc_prelude_46_NoIndex(u_value *env, u_value *args) {
  return u_mk_con_array(&n_prelude_46_NoIndex, 2, args);
}

u_value wc_prelude_46_NoCharacter(u_value *env, u_value *args) {
  return u_mk_con_array(&n_prelude_46_NoCharacter, 1, args);
}

u_value wc_prelude_46_MissingField(u_value *env, u_value *args) {
  return u_mk_con_array(&n_prelude_46_MissingField, 1, args);
}

u_value wc_nat_46_Succ(u_value *env, u_value *args) {
  return u_mk_con_array(&n_nat_46_Succ, 1, args);
}

u_value wc_option_46_Some(u_value *env, u_value *args) {
  return u_mk_con_array(&n_option_46_Some, 1, args);
}

u_value wc_list_46_Cons(u_value *env, u_value *args) {
  return u_mk_con_array(&n_list_46_Cons, 2, args);
}

u_value wc_syntax_46_Name(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_Name, 2, args);
}

u_value wc_syntax_46_TMeta(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_TMeta, 1, args);
}

u_value wc_syntax_46_TParam(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_TParam, 1, args);
}

u_value wc_syntax_46_TSort(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_TSort, 2, args);
}

u_value wc_syntax_46_TFun(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_TFun, 2, args);
}

u_value wc_syntax_46_TProc(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_TProc, 3, args);
}

u_value wc_syntax_46_TTuple(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_TTuple, 1, args);
}

u_value wc_syntax_46_VBool(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_VBool, 1, args);
}

u_value wc_syntax_46_VInt(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_VInt, 1, args);
}

u_value wc_syntax_46_VStr(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_VStr, 1, args);
}

u_value wc_syntax_46_VChar(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_VChar, 1, args);
}

u_value wc_syntax_46_Var(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_Var, 2, args);
}

u_value wc_syntax_46_ELit(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_ELit, 1, args);
}

u_value wc_syntax_46_EVar(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_EVar, 1, args);
}

u_value wc_syntax_46_EApp(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_EApp, 2, args);
}

u_value wc_syntax_46_ELam(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_ELam, 2, args);
}

u_value wc_syntax_46_EProc(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_EProc, 4, args);
}

u_value wc_syntax_46_ELet(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_ELet, 2, args);
}

u_value wc_syntax_46_EIte(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_EIte, 3, args);
}

u_value wc_syntax_46_EMatch(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_EMatch, 2, args);
}

u_value wc_syntax_46_EComp(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_EComp, 4, args);
}

u_value wc_syntax_46_ETuple(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_ETuple, 1, args);
}

u_value wc_syntax_46_ERaise(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_ERaise, 2, args);
}

u_value wc_syntax_46_PLit(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_PLit, 1, args);
}

u_value wc_syntax_46_PVar(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_PVar, 1, args);
}

u_value wc_syntax_46_PApp(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_PApp, 2, args);
}

u_value wc_syntax_46_PTuple(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_PTuple, 1, args);
}

u_value wc_syntax_46_Branch(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_Branch, 3, args);
}

u_value wc_syntax_46_SAssign(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_SAssign, 1, args);
}

u_value wc_syntax_46_SBlock(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_SBlock, 2, args);
}

u_value wc_syntax_46_SIf(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_SIf, 3, args);
}

u_value wc_syntax_46_SWhile(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_SWhile, 2, args);
}

u_value wc_syntax_46_SCall(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_SCall, 4, args);
}

u_value wc_syntax_46_SMatch(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_SMatch, 2, args);
}

u_value wc_syntax_46_SAssert(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_SAssert, 1, args);
}

u_value wc_syntax_46_SRaise(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_SRaise, 2, args);
}

u_value wc_syntax_46_STry(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_STry, 2, args);
}

u_value wc_syntax_46_Handler(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_Handler, 4, args);
}

u_value wc_syntax_46_Constr(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_Constr, 2, args);
}

u_value wc_syntax_46_CSort(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_CSort, 3, args);
}

u_value wc_syntax_46_CData(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_CData, 3, args);
}

u_value wc_syntax_46_CFun(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_CFun, 5, args);
}

u_value wc_syntax_46_CProc(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_CProc, 6, args);
}

u_value wc_syntax_46_CModule(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_CModule, 2, args);
}

u_value wc_syntax_46_CImport(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_CImport, 1, args);
}

u_value wc_syntax_46_CNotation(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_CNotation, 3, args);
}

u_value wc_syntax_46_CTest(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_CTest, 3, args);
}

u_value wc_syntax_46_Program(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_Program, 1, args);
}

u_value wc_os_46_OsError(u_value *env, u_value *args) {
  return u_mk_con_array(&n_os_46_OsError, 1, args);
}

u_value wc_lex_46_TName(u_value *env, u_value *args) {
  return u_mk_con_array(&n_lex_46_TName, 1, args);
}

u_value wc_lex_46_TSym(u_value *env, u_value *args) {
  return u_mk_con_array(&n_lex_46_TSym, 1, args);
}

u_value wc_lex_46_TPunct(u_value *env, u_value *args) {
  return u_mk_con_array(&n_lex_46_TPunct, 1, args);
}

u_value wc_lex_46_TNum(u_value *env, u_value *args) {
  return u_mk_con_array(&n_lex_46_TNum, 1, args);
}

u_value wc_lex_46_TStr(u_value *env, u_value *args) {
  return u_mk_con_array(&n_lex_46_TStr, 1, args);
}

u_value wc_lex_46_TChar(u_value *env, u_value *args) {
  return u_mk_con_array(&n_lex_46_TChar, 1, args);
}

u_value wc_lex_46_LexError(u_value *env, u_value *args) {
  return u_mk_con_array(&n_lex_46_LexError, 2, args);
}

u_value wc_set_46_SNode(u_value *env, u_value *args) {
  return u_mk_con_array(&n_set_46_SNode, 4, args);
}

u_value wc_parse_46_ParseError(u_value *env, u_value *args) {
  return u_mk_con_array(&n_parse_46_ParseError, 2, args);
}

u_value wc_parse_46_Notations(u_value *env, u_value *args) {
  return u_mk_con_array(&n_parse_46_Notations, 2, args);
}

u_value wc_parse_46_Scope(u_value *env, u_value *args) {
  return u_mk_con_array(&n_parse_46_Scope, 2, args);
}

u_value wc_parse_46_Operand(u_value *env, u_value *args) {
  return u_mk_con_array(&n_parse_46_Operand, 1, args);
}

u_value wc_parse_46_Operator(u_value *env, u_value *args) {
  return u_mk_con_array(&n_parse_46_Operator, 2, args);
}

u_value wc_parse_46_Ops(u_value *env, u_value *args) {
  return u_mk_con_array(&n_parse_46_Ops, 4, args);
}

u_value wc_parse_46_Context(u_value *env, u_value *args) {
  return u_mk_con_array(&n_parse_46_Context, 5, args);
}

u_value wc_map_46_MNode(u_value *env, u_value *args) {
  return u_mk_con_array(&n_map_46_MNode, 5, args);
}

u_value wc_ctx_46_Ctx(u_value *env, u_value *args) {
  return u_mk_con_array(&n_ctx_46_Ctx, 7, args);
}

u_value wc_check_46_State(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_State, 6, args);
}

u_value wc_check_46_UnknownSort(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_UnknownSort, 1, args);
}

u_value wc_check_46_SortArity(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_SortArity, 3, args);
}

u_value wc_check_46_SortDefined(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_SortDefined, 1, args);
}

u_value wc_check_46_MissingType(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_MissingType, 1, args);
}

u_value wc_check_46_UndeclaredParams(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_UndeclaredParams, 2, args);
}

u_value wc_check_46_CannotUnify(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_CannotUnify, 2, args);
}

u_value wc_check_46_InfiniteType(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_InfiniteType, 2, args);
}

u_value wc_check_46_UnknownVariable(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_UnknownVariable, 1, args);
}

u_value wc_check_46_OverloadedUnapplied(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_OverloadedUnapplied, 1, args);
}

u_value wc_check_46_WrongArity(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_WrongArity, 2, args);
}

u_value wc_check_46_AlreadyDefined(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_AlreadyDefined, 1, args);
}

u_value wc_check_46_Duplicate(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_Duplicate, 1, args);
}

u_value wc_check_46_Ambiguous(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_Ambiguous, 2, args);
}

u_value wc_check_46_UnknownField(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_UnknownField, 1, args);
}

u_value wc_check_46_UnknownFieldType(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_UnknownFieldType, 2, args);
}

u_value wc_check_46_CountMismatch(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_CountMismatch, 2, args);
}

u_value wc_check_46_UnknownConstructor(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_UnknownConstructor, 1, args);
}

u_value wc_check_46_NotExhaustive(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_NotExhaustive, 1, args);
}

u_value wc_check_46_Unreachable(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_Unreachable, 1, args);
}

u_value wc_check_46_NotAssignable(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_NotAssignable, 1, args);
}

u_value wc_check_46_UnknownProcedure(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_UnknownProcedure, 1, args);
}

u_value wc_check_46_UnknownException(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_UnknownException, 1, args);
}

u_value wc_check_46_TestParams(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_TestParams, 1, args);
}

u_value wc_check_46_HCon(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_HCon, 1, args);
}

u_value wc_check_46_HLit(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_HLit, 1, args);
}

u_value wc_check_46_Checked(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_Checked, 3, args);
}

u_value wc_value_46_Lit(u_value *env, u_value *args) {
  return u_mk_con_array(&n_value_46_Lit, 1, args);
}

u_value wc_value_46_Obj(u_value *env, u_value *args) {
  return u_mk_con_array(&n_value_46_Obj, 2, args);
}

u_value wc_value_46_Clos(u_value *env, u_value *args) {
  return u_mk_con_array(&n_value_46_Clos, 3, args);
}

u_value wc_value_46_Fn(u_value *env, u_value *args) {
  return u_mk_con_array(&n_value_46_Fn, 2, args);
}

u_value wc_value_46_Con(u_value *env, u_value *args) {
  return u_mk_con_array(&n_value_46_Con, 1, args);
}

u_value wc_value_46_Prim(u_value *env, u_value *args) {
  return u_mk_con_array(&n_value_46_Prim, 1, args);
}

u_value wc_value_46_Proc(u_value *env, u_value *args) {
  return u_mk_con_array(&n_value_46_Proc, 1, args);
}

u_value wc_value_46_ProcClos(u_value *env, u_value *args) {
  return u_mk_con_array(&n_value_46_ProcClos, 5, args);
}

u_value wc_run_46_ProcDef(u_value *env, u_value *args) {
  return u_mk_con_array(&n_run_46_ProcDef, 4, args);
}

u_value wc_run_46_Prog(u_value *env, u_value *args) {
  return u_mk_con_array(&n_run_46_Prog, 6, args);
}

u_value wc_run_46_Uninitialized(u_value *env, u_value *args) {
  return u_mk_con_array(&n_run_46_Uninitialized, 1, args);
}

u_value wc_run_46_NotFunction(u_value *env, u_value *args) {
  return u_mk_con_array(&n_run_46_NotFunction, 1, args);
}

u_value wc_run_46_Unspecified(u_value *env, u_value *args) {
  return u_mk_con_array(&n_run_46_Unspecified, 2, args);
}

u_value wc_run_46_NoCase(u_value *env, u_value *args) {
  return u_mk_con_array(&n_run_46_NoCase, 1, args);
}

u_value wc_run_46_NotList(u_value *env, u_value *args) {
  return u_mk_con_array(&n_run_46_NotList, 1, args);
}

u_value wc_run_46_AssertionUnequal(u_value *env, u_value *args) {
  return u_mk_con_array(&n_run_46_AssertionUnequal, 2, args);
}

u_value wc_run_46_UnassignedOutput(u_value *env, u_value *args) {
  return u_mk_con_array(&n_run_46_UnassignedOutput, 2, args);
}

u_value wc_run_46_UnknownProc(u_value *env, u_value *args) {
  return u_mk_con_array(&n_run_46_UnknownProc, 1, args);
}

u_value wc_run_46_NotProc(u_value *env, u_value *args) {
  return u_mk_con_array(&n_run_46_NotProc, 1, args);
}

u_value wc_run_46_RecursiveInitialization(u_value *env, u_value *args) {
  return u_mk_con_array(&n_run_46_RecursiveInitialization, 1, args);
}

u_value wc_run_46_Thrown(u_value *env, u_value *args) {
  return u_mk_con_array(&n_run_46_Thrown, 2, args);
}

u_value wc_c_46_CtNamed(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CtNamed, 1, args);
}

u_value wc_c_46_CtPtr(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CtPtr, 1, args);
}

u_value wc_c_46_CeVar(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CeVar, 1, args);
}

u_value wc_c_46_CeNum(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CeNum, 1, args);
}

u_value wc_c_46_CeStr(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CeStr, 1, args);
}

u_value wc_c_46_CeCall(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CeCall, 2, args);
}

u_value wc_c_46_CeUnary(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CeUnary, 2, args);
}

u_value wc_c_46_CeBinary(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CeBinary, 3, args);
}

u_value wc_c_46_CeIndex(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CeIndex, 2, args);
}

u_value wc_c_46_CeArrow(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CeArrow, 2, args);
}

u_value wc_c_46_CeCast(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CeCast, 2, args);
}

u_value wc_c_46_CeCond(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CeCond, 3, args);
}

u_value wc_c_46_CeInit(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CeInit, 1, args);
}

u_value wc_c_46_CsExpr(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CsExpr, 1, args);
}

u_value wc_c_46_CsDecl(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CsDecl, 3, args);
}

u_value wc_c_46_CsAssign(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CsAssign, 2, args);
}

u_value wc_c_46_CsIf(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CsIf, 3, args);
}

u_value wc_c_46_CsWhile(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CsWhile, 2, args);
}

u_value wc_c_46_CsReturn(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CsReturn, 1, args);
}

u_value wc_c_46_CsBlock(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CsBlock, 1, args);
}

u_value wc_c_46_CdInclude(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CdInclude, 1, args);
}

u_value wc_c_46_CdPrototype(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CdPrototype, 3, args);
}

u_value wc_c_46_CdFunction(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CdFunction, 4, args);
}

u_value wc_c_46_CdGlobal(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CdGlobal, 3, args);
}

u_value wc_c_46_CFile(u_value *env, u_value *args) {
  return u_mk_con_array(&n_c_46_CFile, 1, args);
}

u_value wc_compile_46_Unsupported(u_value *env, u_value *args) {
  return u_mk_con_array(&n_compile_46_Unsupported, 1, args);
}

u_value wc_compile_46_UnknownEntry(u_value *env, u_value *args) {
  return u_mk_con_array(&n_compile_46_UnknownEntry, 1, args);
}

u_value wc_compile_46_Globals(u_value *env, u_value *args) {
  return u_mk_con_array(&n_compile_46_Globals, 5, args);
}

u_value wc_compile_46_Gen(u_value *env, u_value *args) {
  return u_mk_con_array(&n_compile_46_Gen, 2, args);
}

u_value wu_mod(u_value *env, u_value *args) {
  return u_mod(args[0], args[1]);
}

u_value wu_mul(u_value *env, u_value *args) {
  return u_mul(args[0], args[1]);
}

u_value wu_add(u_value *env, u_value *args) {
  return u_add(args[0], args[1]);
}

u_value wu_sub(u_value *env, u_value *args) {
  return u_sub(args[0], args[1]);
}

u_value wu_div(u_value *env, u_value *args) {
  return u_div(args[0], args[1]);
}

u_value wu_lt(u_value *env, u_value *args) {
  return u_lt(args[0], args[1]);
}

u_value wu_eq(u_value *env, u_value *args) {
  return u_eq(args[0], args[1]);
}

u_value wu_concat(u_value *env, u_value *args) {
  return u_concat(args[0], args[1]);
}

u_value wu_chars(u_value *env, u_value *args) {
  return u_chars(args[0]);
}

u_value wu_chr(u_value *env, u_value *args) {
  return u_chr(args[0]);
}

u_value wu_compare(u_value *env, u_value *args) {
  return u_compare(args[0], args[1]);
}

u_value wu_is_digit(u_value *env, u_value *args) {
  return u_is_digit(args[0]);
}

u_value wu_is_letter(u_value *env, u_value *args) {
  return u_is_letter(args[0]);
}

u_value wu_is_space(u_value *env, u_value *args) {
  return u_is_space(args[0]);
}

u_value wu_ord(u_value *env, u_value *args) {
  return u_ord(args[0]);
}

u_value wu_str_compare(u_value *env, u_value *args) {
  return u_str_compare(args[0], args[1]);
}

u_value wu_str_contains(u_value *env, u_value *args) {
  return u_str_contains(args[0], args[1]);
}

u_value wu_str_get(u_value *env, u_value *args) {
  return u_str_get(args[0], args[1]);
}

u_value wu_str_join(u_value *env, u_value *args) {
  return u_str_join(args[0], args[1]);
}

u_value wu_str_number(u_value *env, u_value *args) {
  return u_str_number(args[0]);
}

u_value wu_str_quote(u_value *env, u_value *args) {
  return u_str_quote(args[0]);
}

u_value wu_str_show(u_value *env, u_value *args) {
  return u_str_show(args[0]);
}

u_value wu_str_size(u_value *env, u_value *args) {
  return u_str_size(args[0]);
}

u_value wu_str_slice(u_value *env, u_value *args) {
  return u_str_slice(args[0], args[1], args[2]);
}

u_value wu_str_split(u_value *env, u_value *args) {
  return u_str_split(args[0], args[1]);
}

u_value wu_string(u_value *env, u_value *args) {
  return u_string(args[0]);
}

void wp_check_46_application(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_application(ins[0], ins[1], ins[2], outs[0], outs[1], outs[2]);
}

void wp_check_46_apply(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_apply(ins[0], ins[1], ins[2], outs[0], outs[1], outs[2]);
}

void wp_check_46_arguments(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_arguments(ins[0], ins[1], ins[2]);
}

void wp_check_46_bind_meta(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_bind_meta(ins[0], ins[1], outs[0]);
}

void wp_check_46_binder_list(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_binder_list(ins[0], ins[1], outs[0], outs[1], outs[2]);
}

void wp_check_46_binders(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_binders(ins[0], ins[1], outs[0], outs[1], outs[2]);
}

void wp_check_46_call_declared(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_call_declared(ins[0], ins[1], ins[2], ins[3], ins[4], ins[5], outs[0], outs[1]);
}

void wp_check_46_call_proc(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_call_proc(ins[0], ins[1], ins[2], ins[3], ins[4], ins[5], outs[0], outs[1]);
}

void wp_check_46_call_variable(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_call_variable(ins[0], ins[1], ins[2], ins[3], ins[4], ins[5], ins[6], outs[0], outs[1]);
}

void wp_check_46_check(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_check(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_check_46_check_modules(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_check_modules(ins[0], ins[1], outs[0], outs[1]);
}

void wp_check_46_check_program(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_check_program(ins[0], ins[1], outs[0]);
}

void wp_check_46_check_unit(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_check_unit(ins[0], ins[1], ins[2], ins[3], outs[0]);
}

void wp_check_46_check_units(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_check_units(ins[0], outs[0], outs[1], outs[2], outs[3], outs[4]);
}

void wp_check_46_checks(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_checks(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_check_46_closed(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_closed(ins[0], ins[1], ins[2]);
}

void wp_check_46_command(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_command(ins[0], outs[0], outs[1]);
}

void wp_check_46_commands(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_commands(ins[0], outs[0], outs[1]);
}

void wp_check_46_coverage(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_coverage(ins[0], ins[1], ins[2]);
}

void wp_check_46_declare_constrs(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_declare_constrs(ins[0], outs[0]);
}

void wp_check_46_declare_constructors(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_declare_constructors(ins[0], ins[1], ins[2], outs[0]);
}

void wp_check_46_declare_datas(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_declare_datas(ins[0], outs[0]);
}

void wp_check_46_declare_funs(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_declare_funs(ins[0], outs[0]);
}

void wp_check_46_declare_name(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_declare_name(ins[0], ins[1]);
}

void wp_check_46_declare_procs(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_declare_procs(ins[0], outs[0]);
}

void wp_check_46_declare_sort(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_declare_sort(ins[0], ins[1]);
}

void wp_check_46_declare_sorts(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_declare_sorts(ins[0], outs[0]);
}

void wp_check_46_expr_branches(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_expr_branches(ins[0], ins[1], ins[2], ins[3], outs[0], outs[1]);
}

void wp_check_46_field_access(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_field_access(ins[0], ins[1], ins[2], ins[3], outs[0], outs[1], outs[2]);
}

void wp_check_46_formals(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_formals(ins[0], outs[0], outs[1]);
}

void wp_check_46_fresh(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_fresh(outs[0], outs[1]);
}

void wp_check_46_fresh_list(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_fresh_list(ins[0], outs[0], outs[1]);
}

void wp_check_46_generators(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_generators(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_check_46_guard(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_guard(ins[0], ins[1], outs[0], outs[1]);
}

void wp_check_46_handler_list(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_handler_list(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_check_46_infer(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_infer(ins[0], ins[1], outs[0], outs[1], outs[2]);
}

void wp_check_46_infers(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_infers(ins[0], ins[1], outs[0], outs[1], outs[2]);
}

void wp_check_46_instance(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_instance(ins[0], outs[0], outs[1]);
}

void wp_check_46_instantiate(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_instantiate(ins[0], outs[0], outs[1]);
}

void wp_check_46_known(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_known(ins[0], ins[1], outs[0], outs[1]);
}

void wp_check_46_known_vars(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_known_vars(ins[0], outs[0], outs[1]);
}

void wp_check_46_knowns(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_knowns(ins[0], ins[1], outs[0], outs[1]);
}

void wp_check_46_normalize(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_normalize(ins[0], outs[0], outs[1]);
}

void wp_check_46_normalize_sort(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_normalize_sort(ins[0], ins[1], outs[0], outs[1]);
}

void wp_check_46_normalize_vars(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_normalize_vars(ins[0], outs[0], outs[1]);
}

void wp_check_46_normalizes(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_normalizes(ins[0], outs[0], outs[1]);
}

void wp_check_46_overload(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_overload(ins[0], ins[1], ins[2]);
}

void wp_check_46_pattern(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_pattern(ins[0], ins[1], outs[0], outs[1], outs[2]);
}

void wp_check_46_reachable(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_reachable(ins[0], ins[1], ins[2], ins[3]);
}

void wp_check_46_resolve_name(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_resolve_name(ins[0], ins[1], ins[2], ins[3], outs[0], outs[1]);
}

void wp_check_46_same_length(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_same_length(ins[0], ins[1]);
}

void wp_check_46_signature(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_signature(ins[0], outs[0], outs[1]);
}

void wp_check_46_signatures(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_signatures(ins[0], outs[0], outs[1]);
}

void wp_check_46_statement(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_statement(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_check_46_statements(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_statements(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_check_46_stmt_branches(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_stmt_branches(ins[0], ins[1], ins[2], ins[3], outs[0], outs[1]);
}

void wp_check_46_type_of(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_type_of(ins[0], outs[0]);
}

void wp_check_46_unifies(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_unifies(ins[0], ins[1], outs[0]);
}

void wp_check_46_unify(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_unify(ins[0], ins[1], outs[0]);
}

void wp_check_46_unique(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_unique(ins[0]);
}

void wp_check_46_unique_ids(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_unique_ids(ins[0]);
}

void wp_check_46_untyped(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_untyped(ins[0], ins[1]);
}

void wp_check_46_use(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_use(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_check_46_variable(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_variable(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_check_46_variables(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_variables(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_compile_46_compile_cmd(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_cmd(ins[0], ins[1], ins[2], outs[0], outs[1], outs[2]);
}

void wp_compile_46_compile_cmds(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_cmds(ins[0], ins[1], ins[2], outs[0], outs[1], outs[2]);
}

void wp_compile_46_compile_expr(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_expr(ins[0], ins[1], ins[2], outs[0], outs[1], outs[2]);
}

void wp_compile_46_compile_exprs(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_exprs(ins[0], ins[1], ins[2], outs[0], outs[1], outs[2]);
}

void wp_compile_46_compile_file(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_file(ins[0], ins[1], ins[2]);
}

void wp_compile_46_compile_lambda(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_lambda(ins[0], ins[1], ins[2], ins[3], outs[0], outs[1], outs[2]);
}

void wp_compile_46_compile_loops(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_loops(ins[0], ins[1], ins[2], ins[3], ins[4], ins[5], ins[6], outs[0], outs[1]);
}

void wp_compile_46_compile_named(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_named(ins[0], ins[1], ins[2], ins[3], ins[4], outs[0], outs[1], outs[2]);
}

void wp_compile_46_compile_proc_lambda(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_proc_lambda(ins[0], ins[1], ins[2], ins[3], ins[4], outs[0], outs[1], outs[2]);
}

void wp_compile_46_compile_program(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_program(ins[0], ins[1], ins[2], outs[0]);
}

void wp_compile_46_compile_stmt(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_stmt(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_compile_46_compile_stmts(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_stmts(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_compile_46_compile_with(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_with(ins[0], ins[1], ins[2], outs[0]);
}

void wp_compile_46_expr_cases(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_expr_cases(ins[0], ins[1], ins[2], ins[3], ins[4], outs[0], outs[1]);
}

void wp_compile_46_gen_name(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_gen_name(ins[0], outs[0], outs[1]);
}

void wp_compile_46_handler_cases(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_handler_cases(ins[0], ins[1], ins[2], ins[3], ins[4], outs[0], outs[1]);
}

void wp_compile_46_lift(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_lift(ins[0], outs[0]);
}

void wp_compile_46_stmt_cases(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_stmt_cases(ins[0], ins[1], ins[2], ins[3], ins[4], outs[0], outs[1]);
}

void wp_compile_46_temps(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_temps(ins[0], outs[0], outs[1]);
}

void wp_lex_46_lex(u_value *env, u_value *ins, u_value **outs) {
  p_lex_46_lex(ins[0], outs[0]);
}

void wp_lex_46_lex_char(u_value *env, u_value *ins, u_value **outs) {
  p_lex_46_lex_char(ins[0], outs[0], outs[1]);
}

void wp_lex_46_lex_literal(u_value *env, u_value *ins, u_value **outs) {
  p_lex_46_lex_literal(ins[0], ins[1], outs[0], outs[1]);
}

void wp_lex_46_lex_name(u_value *env, u_value *ins, u_value **outs) {
  p_lex_46_lex_name(ins[0], outs[0], outs[1]);
}

void wp_lex_46_lex_number(u_value *env, u_value *ins, u_value **outs) {
  p_lex_46_lex_number(ins[0], outs[0], outs[1]);
}

void wp_lex_46_lex_string(u_value *env, u_value *ins, u_value **outs) {
  p_lex_46_lex_string(ins[0], outs[0], outs[1]);
}

void wp_lex_46_lex_symbol(u_value *env, u_value *ins, u_value **outs) {
  p_lex_46_lex_symbol(ins[0], outs[0], outs[1]);
}

void wp_lex_46_lex_token(u_value *env, u_value *ins, u_value **outs) {
  p_lex_46_lex_token(ins[0], outs[0], outs[1]);
}

void wp_lex_46_position(u_value *env, u_value *ins, u_value **outs) {
  p_lex_46_position(ins[0], ins[1], outs[0], outs[1]);
}

void wp_parse_46_advance(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_advance(outs[0]);
}

void wp_parse_46_climb(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_climb(ins[0], ins[1], ins[2], ins[3], outs[0], outs[1]);
}

void wp_parse_46_climb_loop(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_climb_loop(ins[0], ins[1], ins[2], ins[3], ins[4], outs[0], outs[1]);
}

void wp_parse_46_constructor(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_constructor(ins[0], outs[0], outs[1], outs[2]);
}

void wp_parse_46_declare_all(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_declare_all(ins[0], outs[0]);
}

void wp_parse_46_expect(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_expect(ins[0], outs[0]);
}

void wp_parse_46_expr_items(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_expr_items(ins[0], outs[0], outs[1]);
}

void wp_parse_46_expr_list(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_expr_list(ins[0], ins[1], outs[0], outs[1]);
}

void wp_parse_46_field(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_field(ins[0], outs[0], outs[1], outs[2]);
}

void wp_parse_46_keyword(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_keyword(ins[0], outs[0]);
}

void wp_parse_46_lambda_function(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_lambda_function(ins[0], outs[0], outs[1]);
}

void wp_parse_46_library_text(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_library_text(ins[0], ins[1], outs[0]);
}

void wp_parse_46_load_module(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_load_module(ins[0], ins[1], ins[2], outs[0]);
}

void wp_parse_46_map_comprehension(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_map_comprehension(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_parse_46_map_generator(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_map_generator(ins[0], outs[0], outs[1], outs[2]);
}

void wp_parse_46_module_after(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_module_after(ins[0], ins[1], ins[2], ins[3], outs[0]);
}

void wp_parse_46_module_text(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_module_text(ins[0], ins[1], ins[2], outs[0]);
}

void wp_parse_46_nonassociative(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_nonassociative(ins[0], ins[1], ins[2], ins[3], ins[4]);
}

void wp_parse_46_operand(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_operand(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_parse_46_operators(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_operators(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_app(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_app(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_assign(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_assign(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_atom(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_atom(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_body(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_body(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_braces(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_braces(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_brackets(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_brackets(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_call(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_call(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_command(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_command(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_commands(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_commands(outs[0], outs[1], outs[2]);
}

void wp_parse_46_parse_data(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_data(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_expr(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_expr(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_expr_cases(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_expr_cases(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_exprs(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_exprs(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_failures(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_failures(ins[0], ins[1]);
}

void wp_parse_46_parse_file(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_file(ins[0], outs[0]);
}

void wp_parse_46_parse_file_after(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_file_after(ins[0], ins[1], outs[0]);
}

void wp_parse_46_parse_formal(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_formal(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_formals(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_formals(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_function(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_function(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_guard(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_guard(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_if(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_if(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_lambda(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_lambda(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_let(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_let(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_library(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_library(ins[0], outs[0]);
}

void wp_parse_46_parse_local(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_local(ins[0], outs[0], outs[1], outs[2]);
}

void wp_parse_46_parse_name(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_name(outs[0], outs[1]);
}

void wp_parse_46_parse_name_or_symbol(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_name_or_symbol(outs[0], outs[1]);
}

void wp_parse_46_parse_notation(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_notation(outs[0], outs[1]);
}

void wp_parse_46_parse_pattern(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_pattern(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_plain(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_plain(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_procedure(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_procedure(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_stmt(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_stmt(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_stmt_cases(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_stmt_cases(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_text(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_text(ins[0], ins[1], outs[0]);
}

void wp_parse_46_parse_type(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_type(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_update(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_update(ins[0], outs[0], outs[1]);
}

void wp_parse_46_parse_variable(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_variable(ins[0], outs[0], outs[1]);
}

void wp_parse_46_pattern_items(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_pattern_items(ins[0], outs[0], outs[1]);
}

void wp_parse_46_pattern_list(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_pattern_list(ins[0], ins[1], outs[0], outs[1]);
}

void wp_parse_46_pattern_simple(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_pattern_simple(ins[0], outs[0], outs[1]);
}

void wp_parse_46_resolve_items(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_resolve_items(ins[0], ins[1], ins[2], outs[0]);
}

void wp_parse_46_same_commands(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_same_commands(ins[0], ins[1], ins[2]);
}

void wp_parse_46_same_parses(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_same_parses(ins[0], ins[1]);
}

void wp_parse_46_section_items(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_section_items(ins[0], ins[1], outs[0], outs[1]);
}

void wp_parse_46_sections(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_sections(ins[0], ins[1], ins[2], outs[0], outs[1], outs[2], outs[3]);
}

void wp_parse_46_set_comprehension(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_set_comprehension(ins[0], ins[1], outs[0], outs[1]);
}

void wp_parse_46_skip_comma(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_skip_comma(ins[0], outs[0]);
}

void wp_parse_46_type_basic(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_type_basic(ins[0], outs[0], outs[1]);
}

void wp_parse_46_type_items(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_type_items(ins[0], outs[0], outs[1]);
}

void wp_parse_46_type_list(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_type_list(ins[0], ins[1], outs[0], outs[1]);
}

void wp_parse_46_type_params(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_type_params(outs[0], outs[1]);
}

void wp_run_46_apply_expr(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_apply_expr(ins[0], ins[1], ins[2], ins[3], outs[0], outs[1]);
}

void wp_run_46_apply_value(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_apply_value(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_run_46_cached(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_cached(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_run_46_check_and_run(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_check_and_run(ins[0]);
}

void wp_run_46_combinations(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_combinations(ins[0], ins[1], ins[2], ins[3], outs[0], outs[1]);
}

void wp_run_46_compare_all(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_compare_all(ins[0], ins[1], ins[2], outs[0]);
}

void wp_run_46_compare_values(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_compare_values(ins[0], ins[1], ins[2], outs[0]);
}

void wp_run_46_comprehension(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_comprehension(ins[0], ins[1], ins[2], ins[3], outs[0], outs[1]);
}

void wp_run_46_equal(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_equal(ins[0], ins[1], outs[0]);
}

void wp_run_46_equal_all(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_equal_all(ins[0], ins[1], outs[0]);
}

void wp_run_46_evaluate(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_evaluate(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_run_46_evaluate_all(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_evaluate_all(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_run_46_exec(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_exec(ins[0], ins[1], outs[0], outs[1]);
}

void wp_run_46_exec_all(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_exec_all(ins[0], ins[1], outs[0], outs[1]);
}

void wp_run_46_extend(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_extend(ins[0], ins[1], ins[2], ins[3], ins[4], ins[5], outs[0], outs[1]);
}

void wp_run_46_handle(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_handle(ins[0], ins[1], ins[2], ins[3], outs[0], outs[1]);
}

void wp_run_46_holds(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_holds(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_run_46_invoke(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_invoke(ins[0], ins[1], ins[2], ins[3], outs[0], outs[1], outs[2]);
}

void wp_run_46_invoke_value(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_invoke_value(ins[0], ins[1], ins[2], ins[3], outs[0], outs[1], outs[2]);
}

void wp_run_46_os_proc(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_os_proc(ins[0], ins[1], outs[0]);
}

void wp_run_46_primitive(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_primitive(ins[0], ins[1], outs[0]);
}

void wp_run_46_print_all(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_print_all(ins[0]);
}

void wp_run_46_run_all(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_run_all(ins[0], ins[1], outs[0]);
}

void wp_run_46_run_body(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_run_body(ins[0], ins[1], ins[2], ins[3], ins[4], ins[5], ins[6], ins[7], ins[8], outs[0], outs[1], outs[2]);
}

void wp_run_46_run_cmd(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_run_cmd(ins[0], ins[1], outs[0]);
}

void wp_run_46_run_proc(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_run_proc(ins[0], ins[1], outs[0]);
}

void wp_run_46_run_program(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_run_program(ins[0]);
}

void wp_run_46_scoped(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_scoped(ins[0], ins[1], ins[2], ins[3], outs[0], outs[1]);
}

void wp_run_46_select(u_value *env, u_value *ins, u_value **outs) {
  p_run_46_select(ins[0], ins[1], ins[2], ins[3], outs[0], outs[1]);
}

void wp_ulang_46_attempt(u_value *env, u_value *ins, u_value **outs) {
  p_ulang_46_attempt(ins[0], ins[1], outs[0]);
}

void wp_ulang_46_check_file(u_value *env, u_value *ins, u_value **outs) {
  p_ulang_46_check_file(ins[0], outs[0]);
}

void wp_ulang_46_compile_to(u_value *env, u_value *ins, u_value **outs) {
  p_ulang_46_compile_to(ins[0], ins[1], ins[2], outs[0]);
}

void wp_ulang_46_load_file(u_value *env, u_value *ins, u_value **outs) {
  p_ulang_46_load_file(ins[0], outs[0]);
}

void wp_ulang_46_located(u_value *env, u_value *ins, u_value **outs) {
  p_ulang_46_located(ins[0], ins[1], ins[2], outs[0]);
}

void wp_ulang_46_main(u_value *env, u_value *ins, u_value **outs) {
  p_ulang_46_main();
}

void wp_ulang_46_run_file(u_value *env, u_value *ins, u_value **outs) {
  p_ulang_46_run_file(ins[0], ins[1], ins[2], outs[0]);
}

void wp_ulang_46_run_test(u_value *env, u_value *ins, u_value **outs) {
  p_ulang_46_run_test(ins[0], ins[1], outs[0]);
}

void wp_ulang_46_run_tests(u_value *env, u_value *ins, u_value **outs) {
  p_ulang_46_run_tests(ins[0], ins[1], outs[0]);
}

void wp_ulang_46_test_file(u_value *env, u_value *ins, u_value **outs) {
  p_ulang_46_test_file(ins[0], ins[1], ins[2], outs[0]);
}

u_value f_prelude_46_not_1(u_value v_b) {
  u_value t0;
  if (u_truth(v_b)) {
    t0 = u_mk_bool(0);
  } else {
    t0 = u_mk_bool(1);
  }
  return t0;
}

u_value f_prelude_46__61__61__62__2(u_value v_a, u_value v_b) {
  u_value t1 = f_prelude_46_not_1(v_a);
  if (!u_truth(t1)) {
    t1 = v_b;
  }
  return t1;
}

u_value f_prelude_46__33__61__2(u_value v_x, u_value v_y) {
  return f_prelude_46_not_1(u_eq(v_x, v_y));
}

u_value f_prelude_46__60__61__2(u_value v_m, u_value v_n) {
  u_value t2 = u_lt(v_m, v_n);
  if (!u_truth(t2)) {
    t2 = u_eq(v_m, v_n);
  }
  return t2;
}

u_value f_prelude_46__62__2(u_value v_m, u_value v_n) {
  return u_lt(v_n, v_m);
}

u_value f_prelude_46__62__61__2(u_value v_m, u_value v_n) {
  return f_prelude_46__60__61__2(v_n, v_m);
}

u_value f_prelude_46__45__1(u_value v_n) {
  return u_sub(u_mk_int(0), v_n);
}

u_value f_option_46_or_else_2(u_value v_a, u_value v_b) {
  u_value s3 = v_a;
  u_value t4 = NULL;
  if ((!t4) && u_is_con(s3, &n_option_46_Some)) {
    t4 = v_a;
  }
  if ((!t4) && u_is_con(s3, &n_option_46_None)) {
    t4 = v_b;
  }
  if (!t4) {
    u_no_case(s3);
  }
  return t4;
}

u_value f_list_46__43__43__2(u_value v_xs, u_value v_ys) {
  u_value s5 = v_xs;
  u_value t6 = NULL;
  if ((!t6) && u_is_con(s5, &n_list_46_Nil)) {
    t6 = v_ys;
  }
  if ((!t6) && u_is_con(s5, &n_list_46_Cons)) {
    u_value v_x = u_field(s5, 0);
    u_value v_zs = u_field(s5, 1);
    t6 = u_mk_con(&n_list_46_Cons, 2, v_x, f_list_46__43__43__2(v_zs, v_ys));
  }
  if (!t6) {
    u_no_case(s5);
  }
  return t6;
}

u_value f_list_46_length_1(u_value v_xs) {
  u_value s7 = v_xs;
  u_value t8 = NULL;
  if ((!t8) && u_is_con(s7, &n_list_46_Nil)) {
    t8 = u_mk_int(0);
  }
  if ((!t8) && u_is_con(s7, &n_list_46_Cons)) {
    u_value v_ys = u_field(s7, 1);
    t8 = u_add(u_mk_int(1), f_list_46_length_1(v_ys));
  }
  if (!t8) {
    u_no_case(s7);
  }
  return t8;
}

u_value f_list_46_lookup_2(u_value v_k, u_value v_xs) {
  u_value s9 = v_xs;
  u_value t10 = NULL;
  if ((!t10) && u_is_con(s9, &n_list_46_Nil)) {
    t10 = &o_option_46_None;
  }
  if ((!t10) && u_is_con(s9, &n_list_46_Cons)) {
    u_value v_j = u_field(u_field(s9, 0), 0);
    u_value v_v = u_field(u_field(s9, 0), 1);
    if (u_truth(u_eq(v_j, v_k))) {
      t10 = u_mk_con(&n_option_46_Some, 1, v_v);
    }
  }
  if ((!t10) && u_is_con(s9, &n_list_46_Cons)) {
    u_value v_ys = u_field(s9, 1);
    t10 = f_list_46_lookup_2(v_k, v_ys);
  }
  if (!t10) {
    u_no_case(s9);
  }
  return t10;
}

u_value f_list_46_update_3(u_value v_k, u_value v_v, u_value v_xs) {
  u_value s11 = f_list_46_replaced_3(v_k, v_v, v_xs);
  u_value t12 = NULL;
  if ((!t12) && u_is_con(s11, &n_option_46_Some)) {
    u_value v_ys = u_field(s11, 0);
    t12 = v_ys;
  }
  if ((!t12) && u_is_con(s11, &n_option_46_None)) {
    t12 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_v), v_xs);
  }
  if (!t12) {
    u_no_case(s11);
  }
  return t12;
}

u_value f_list_46_replaced_3(u_value v_k, u_value v_v, u_value v_xs) {
  u_value s13 = v_xs;
  u_value t14 = NULL;
  if ((!t14) && u_is_con(s13, &n_list_46_Nil)) {
    t14 = &o_option_46_None;
  }
  if ((!t14) && u_is_con(s13, &n_list_46_Cons)) {
    u_value v_j = u_field(u_field(s13, 0), 0);
    u_value v_ys = u_field(s13, 1);
    if (u_truth(u_eq(v_j, v_k))) {
      t14 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_v), v_ys));
    }
  }
  if ((!t14) && u_is_con(s13, &n_list_46_Cons)) {
    u_value v_p = u_field(s13, 0);
    u_value v_ys = u_field(s13, 1);
    u_value s15 = f_list_46_replaced_3(v_k, v_v, v_ys);
    u_value t16 = NULL;
    if ((!t16) && u_is_con(s15, &n_option_46_Some)) {
      u_value v_zs = u_field(s15, 0);
      t16 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_p, v_zs));
    }
    if ((!t16) && u_is_con(s15, &n_option_46_None)) {
      t16 = &o_option_46_None;
    }
    if (!t16) {
      u_no_case(s15);
    }
    t14 = t16;
  }
  if (!t14) {
    u_no_case(s13);
  }
  return t14;
}

u_value f_list_46_remove_2(u_value v_k, u_value v_xs) {
  u_value s17 = v_xs;
  u_value t18 = NULL;
  if ((!t18) && u_is_con(s17, &n_list_46_Nil)) {
    t18 = &o_list_46_Nil;
  }
  if ((!t18) && u_is_con(s17, &n_list_46_Cons)) {
    u_value v_j = u_field(u_field(s17, 0), 0);
    u_value v_ys = u_field(s17, 1);
    if (u_truth(u_eq(v_j, v_k))) {
      t18 = f_list_46_remove_2(v_k, v_ys);
    }
  }
  if ((!t18) && u_is_con(s17, &n_list_46_Cons)) {
    u_value v_p = u_field(s17, 0);
    u_value v_ys = u_field(s17, 1);
    t18 = u_mk_con(&n_list_46_Cons, 2, v_p, f_list_46_remove_2(v_k, v_ys));
  }
  if (!t18) {
    u_no_case(s17);
  }
  return t18;
}

u_value f_list_46_has_key_2(u_value v_k, u_value v_xs) {
  u_value s19 = v_xs;
  u_value t20 = NULL;
  if ((!t20) && u_is_con(s19, &n_list_46_Nil)) {
    t20 = u_mk_bool(0);
  }
  if ((!t20) && u_is_con(s19, &n_list_46_Cons)) {
    u_value v_j = u_field(u_field(s19, 0), 0);
    u_value v_ys = u_field(s19, 1);
    u_value t21 = u_eq(v_j, v_k);
    if (!u_truth(t21)) {
      t21 = f_list_46_has_key_2(v_k, v_ys);
    }
    t20 = t21;
  }
  if (!t20) {
    u_no_case(s19);
  }
  return t20;
}

u_value f_list_46_keys_1(u_value v_xs) {
  u_value s22 = v_xs;
  u_value t23 = NULL;
  if ((!t23) && u_is_con(s22, &n_list_46_Nil)) {
    t23 = &o_list_46_Nil;
  }
  if ((!t23) && u_is_con(s22, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s22, 0), 0);
    u_value v_ys = u_field(s22, 1);
    t23 = u_mk_con(&n_list_46_Cons, 2, v_k, f_list_46_keys_1(v_ys));
  }
  if (!t23) {
    u_no_case(s22);
  }
  return t23;
}

u_value f_list_46_values_1(u_value v_xs) {
  u_value s24 = v_xs;
  u_value t25 = NULL;
  if ((!t25) && u_is_con(s24, &n_list_46_Nil)) {
    t25 = &o_list_46_Nil;
  }
  if ((!t25) && u_is_con(s24, &n_list_46_Cons)) {
    u_value v_v = u_field(u_field(s24, 0), 1);
    u_value v_ys = u_field(s24, 1);
    t25 = u_mk_con(&n_list_46_Cons, 2, v_v, f_list_46_values_1(v_ys));
  }
  if (!t25) {
    u_no_case(s24);
  }
  return t25;
}

u_value f_list_46_contains_2(u_value v_x, u_value v_xs) {
  u_value s26 = v_xs;
  u_value t27 = NULL;
  if ((!t27) && u_is_con(s26, &n_list_46_Nil)) {
    t27 = u_mk_bool(0);
  }
  if ((!t27) && u_is_con(s26, &n_list_46_Cons)) {
    u_value v_y = u_field(s26, 0);
    u_value v_ys = u_field(s26, 1);
    u_value t28 = u_eq(v_x, v_y);
    if (!u_truth(t28)) {
      t28 = f_list_46_contains_2(v_x, v_ys);
    }
    t27 = t28;
  }
  if (!t27) {
    u_no_case(s26);
  }
  return t27;
}

u_value f_list_46_exists_2(u_value v_xs, u_value v_p) {
  u_value s29 = v_xs;
  u_value t30 = NULL;
  if ((!t30) && u_is_con(s29, &n_list_46_Nil)) {
    t30 = u_mk_bool(0);
  }
  if ((!t30) && u_is_con(s29, &n_list_46_Cons)) {
    u_value v_y = u_field(s29, 0);
    u_value v_ys = u_field(s29, 1);
    u_value t31 = u_apply(v_p, 1, v_y);
    if (!u_truth(t31)) {
      t31 = f_list_46_exists_2(v_ys, v_p);
    }
    t30 = t31;
  }
  if (!t30) {
    u_no_case(s29);
  }
  return t30;
}

u_value f_list_46_forall_2(u_value v_xs, u_value v_p) {
  u_value s32 = v_xs;
  u_value t33 = NULL;
  if ((!t33) && u_is_con(s32, &n_list_46_Nil)) {
    t33 = u_mk_bool(1);
  }
  if ((!t33) && u_is_con(s32, &n_list_46_Cons)) {
    u_value v_y = u_field(s32, 0);
    u_value v_ys = u_field(s32, 1);
    u_value t34 = u_apply(v_p, 1, v_y);
    if (u_truth(t34)) {
      t34 = f_list_46_forall_2(v_ys, v_p);
    }
    t33 = t34;
  }
  if (!t33) {
    u_no_case(s32);
  }
  return t33;
}

u_value f_list_46_distinct_1(u_value v_xs) {
  u_value s35 = v_xs;
  u_value t36 = NULL;
  if ((!t36) && u_is_con(s35, &n_list_46_Nil)) {
    t36 = &o_list_46_Nil;
  }
  if ((!t36) && u_is_con(s35, &n_list_46_Cons)) {
    u_value v_y = u_field(s35, 0);
    u_value v_ys = u_field(s35, 1);
    u_value t37 = &o_list_46_Nil;
    u_value *e38 = &t37;
    u_value l39 = v_ys;
    while (u_is_con(l39, &n_list_46_Cons)) {
      u_value v_z = u_field(l39, 0);
      if (u_truth(f_prelude_46__33__61__2(v_z, v_y))) {
        e38 = u_append(e38, v_z);
      }
      l39 = u_field(l39, 1);
    }
    t36 = u_mk_con(&n_list_46_Cons, 2, v_y, f_list_46_distinct_1(t37));
  }
  if (!t36) {
    u_no_case(s35);
  }
  return t36;
}

u_value f_list_46_repeated_1(u_value v_xs) {
  u_value s40 = v_xs;
  u_value t41 = NULL;
  if ((!t41) && u_is_con(s40, &n_list_46_Nil)) {
    t41 = &o_option_46_None;
  }
  if ((!t41) && u_is_con(s40, &n_list_46_Cons)) {
    u_value v_y = u_field(s40, 0);
    u_value v_ys = u_field(s40, 1);
    u_value t42;
    if (u_truth(f_list_46_contains_2(v_y, v_ys))) {
      t42 = u_mk_con(&n_option_46_Some, 1, v_y);
    } else {
      t42 = f_list_46_repeated_1(v_ys);
    }
    t41 = t42;
  }
  if (!t41) {
    u_no_case(s40);
  }
  return t41;
}

u_value f_list_46_diff_2(u_value v_xs, u_value v_ys) {
  u_value t43 = &o_list_46_Nil;
  u_value *e44 = &t43;
  u_value l45 = v_xs;
  while (u_is_con(l45, &n_list_46_Cons)) {
    u_value v_x = u_field(l45, 0);
    if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_x, v_ys)))) {
      e44 = u_append(e44, v_x);
    }
    l45 = u_field(l45, 1);
  }
  return t43;
}

u_value f_list_46_concat_1(u_value v_xss) {
  u_value s46 = v_xss;
  u_value t47 = NULL;
  if ((!t47) && u_is_con(s46, &n_list_46_Nil)) {
    t47 = &o_list_46_Nil;
  }
  if ((!t47) && u_is_con(s46, &n_list_46_Cons)) {
    u_value v_xs = u_field(s46, 0);
    u_value v_yss = u_field(s46, 1);
    t47 = f_list_46__43__43__2(v_xs, f_list_46_concat_1(v_yss));
  }
  if (!t47) {
    u_no_case(s46);
  }
  return t47;
}

u_value f_list_46_reverse_onto_2(u_value v_xs, u_value v_ys) {
  u_value s48 = v_xs;
  u_value t49 = NULL;
  if ((!t49) && u_is_con(s48, &n_list_46_Nil)) {
    t49 = v_ys;
  }
  if ((!t49) && u_is_con(s48, &n_list_46_Cons)) {
    u_value v_x = u_field(s48, 0);
    u_value v_rest = u_field(s48, 1);
    t49 = f_list_46_reverse_onto_2(v_rest, u_mk_con(&n_list_46_Cons, 2, v_x, v_ys));
  }
  if (!t49) {
    u_no_case(s48);
  }
  return t49;
}

u_value f_list_46_reverse_1(u_value v_xs) {
  return f_list_46_reverse_onto_2(v_xs, &o_list_46_Nil);
}

u_value f_list_46_zip_2(u_value v_xs, u_value v_ys) {
  u_value s50 = u_mk_tuple(2, v_xs, v_ys);
  u_value t51 = NULL;
  if ((!t51) && (u_is_con(u_field(s50, 0), &n_list_46_Cons) && u_is_con(u_field(s50, 1), &n_list_46_Cons))) {
    u_value v_x = u_field(u_field(s50, 0), 0);
    u_value v_xs1 = u_field(u_field(s50, 0), 1);
    u_value v_y = u_field(u_field(s50, 1), 0);
    u_value v_ys1 = u_field(u_field(s50, 1), 1);
    t51 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_y), f_list_46_zip_2(v_xs1, v_ys1));
  }
  if (!t51) {
    t51 = &o_list_46_Nil;
  }
  if (!t51) {
    u_no_case(s50);
  }
  return t51;
}

u_value f_list_46_take_2(u_value v_n, u_value v_xs) {
  u_value s52 = v_xs;
  u_value t53 = NULL;
  if ((!t53) && u_is_con(s52, &n_list_46_Nil)) {
    t53 = &o_list_46_Nil;
  }
  if ((!t53) && u_is_con(s52, &n_list_46_Cons)) {
    u_value v_y = u_field(s52, 0);
    u_value v_ys = u_field(s52, 1);
    u_value t54;
    if (u_truth(u_eq(v_n, u_mk_int(0)))) {
      t54 = &o_list_46_Nil;
    } else {
      t54 = u_mk_con(&n_list_46_Cons, 2, v_y, f_list_46_take_2(u_sub(v_n, u_mk_int(1)), v_ys));
    }
    t53 = t54;
  }
  if (!t53) {
    u_no_case(s52);
  }
  return t53;
}

u_value f_list_46_drop_2(u_value v_n, u_value v_xs) {
  u_value s55 = v_xs;
  u_value t56 = NULL;
  if ((!t56) && u_is_con(s55, &n_list_46_Nil)) {
    t56 = &o_list_46_Nil;
  }
  if ((!t56) && u_is_con(s55, &n_list_46_Cons)) {
    u_value v_ys = u_field(s55, 1);
    u_value t57;
    if (u_truth(u_eq(v_n, u_mk_int(0)))) {
      t57 = v_xs;
    } else {
      t57 = f_list_46_drop_2(u_sub(v_n, u_mk_int(1)), v_ys);
    }
    t56 = t57;
  }
  if (!t56) {
    u_no_case(s55);
  }
  return t56;
}

u_value f_list_46_replicate_2(u_value v_n, u_value v_x) {
  u_value t58;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t58 = &o_list_46_Nil;
  } else {
    t58 = u_mk_con(&n_list_46_Cons, 2, v_x, f_list_46_replicate_2(u_sub(v_n, u_mk_int(1)), v_x));
  }
  return t58;
}

u_value f_list_46_firsts_1(u_value v_ps) {
  u_value s59 = v_ps;
  u_value t60 = NULL;
  if ((!t60) && u_is_con(s59, &n_list_46_Nil)) {
    t60 = &o_list_46_Nil;
  }
  if ((!t60) && u_is_con(s59, &n_list_46_Cons)) {
    u_value v_a = u_field(u_field(s59, 0), 0);
    u_value v_rest = u_field(s59, 1);
    t60 = u_mk_con(&n_list_46_Cons, 2, v_a, f_list_46_firsts_1(v_rest));
  }
  if (!t60) {
    u_no_case(s59);
  }
  return t60;
}

u_value f_list_46_seconds_1(u_value v_ps) {
  u_value s61 = v_ps;
  u_value t62 = NULL;
  if ((!t62) && u_is_con(s61, &n_list_46_Nil)) {
    t62 = &o_list_46_Nil;
  }
  if ((!t62) && u_is_con(s61, &n_list_46_Cons)) {
    u_value v_b = u_field(u_field(s61, 0), 1);
    u_value v_rest = u_field(s61, 1);
    t62 = u_mk_con(&n_list_46_Cons, 2, v_b, f_list_46_seconds_1(v_rest));
  }
  if (!t62) {
    u_no_case(s61);
  }
  return t62;
}

u_value f_syntax_46_Name_46_module_1(u_value v_x) {
  u_value s63 = v_x;
  u_value t64 = NULL;
  if ((!t64) && u_is_con(s63, &n_syntax_46_Name)) {
    u_value v_y = u_field(s63, 0);
    t64 = v_y;
  }
  if (!t64) {
    u_no_case(s63);
  }
  return t64;
}

u_value f_syntax_46_Name_46_base_1(u_value v_x) {
  u_value s65 = v_x;
  u_value t66 = NULL;
  if ((!t66) && u_is_con(s65, &n_syntax_46_Name)) {
    u_value v_y = u_field(s65, 1);
    t66 = v_y;
  }
  if (!t66) {
    u_no_case(s65);
  }
  return t66;
}

u_value f_syntax_46_Name_46_module_58__61__2(u_value v_x, u_value v_y) {
  u_value s67 = v_x;
  u_value t68 = NULL;
  if ((!t68) && u_is_con(s67, &n_syntax_46_Name)) {
    u_value v_a1 = u_field(s67, 1);
    t68 = u_mk_con(&n_syntax_46_Name, 2, v_y, v_a1);
  }
  if (!t68) {
    u_no_case(s67);
  }
  return t68;
}

u_value f_syntax_46_Name_46_base_58__61__2(u_value v_x, u_value v_y) {
  u_value s69 = v_x;
  u_value t70 = NULL;
  if ((!t70) && u_is_con(s69, &n_syntax_46_Name)) {
    u_value v_a0 = u_field(s69, 0);
    t70 = u_mk_con(&n_syntax_46_Name, 2, v_a0, v_y);
  }
  if (!t70) {
    u_no_case(s69);
  }
  return t70;
}

u_value f_syntax_46_Var_46_name_1(u_value v_x) {
  u_value s71 = v_x;
  u_value t72 = NULL;
  if ((!t72) && u_is_con(s71, &n_syntax_46_Var)) {
    u_value v_y = u_field(s71, 0);
    t72 = v_y;
  }
  if (!t72) {
    u_no_case(s71);
  }
  return t72;
}

u_value f_syntax_46_Var_46_typ_1(u_value v_x) {
  u_value s73 = v_x;
  u_value t74 = NULL;
  if ((!t74) && u_is_con(s73, &n_syntax_46_Var)) {
    u_value v_y = u_field(s73, 1);
    t74 = v_y;
  }
  if (!t74) {
    u_no_case(s73);
  }
  return t74;
}

u_value f_syntax_46_Var_46_name_58__61__2(u_value v_x, u_value v_y) {
  u_value s75 = v_x;
  u_value t76 = NULL;
  if ((!t76) && u_is_con(s75, &n_syntax_46_Var)) {
    u_value v_a1 = u_field(s75, 1);
    t76 = u_mk_con(&n_syntax_46_Var, 2, v_y, v_a1);
  }
  if (!t76) {
    u_no_case(s75);
  }
  return t76;
}

u_value f_syntax_46_Var_46_typ_58__61__2(u_value v_x, u_value v_y) {
  u_value s77 = v_x;
  u_value t78 = NULL;
  if ((!t78) && u_is_con(s77, &n_syntax_46_Var)) {
    u_value v_a0 = u_field(s77, 0);
    t78 = u_mk_con(&n_syntax_46_Var, 2, v_a0, v_y);
  }
  if (!t78) {
    u_no_case(s77);
  }
  return t78;
}

u_value f_syntax_46_Branch_46_pat_1(u_value v_x) {
  u_value s79 = v_x;
  u_value t80 = NULL;
  if ((!t80) && u_is_con(s79, &n_syntax_46_Branch)) {
    u_value v_y = u_field(s79, 0);
    t80 = v_y;
  }
  if (!t80) {
    u_no_case(s79);
  }
  return t80;
}

u_value f_syntax_46_Branch_46_guard_1(u_value v_x) {
  u_value s81 = v_x;
  u_value t82 = NULL;
  if ((!t82) && u_is_con(s81, &n_syntax_46_Branch)) {
    u_value v_y = u_field(s81, 1);
    t82 = v_y;
  }
  if (!t82) {
    u_no_case(s81);
  }
  return t82;
}

u_value f_syntax_46_Branch_46_body_1(u_value v_x) {
  u_value s83 = v_x;
  u_value t84 = NULL;
  if ((!t84) && u_is_con(s83, &n_syntax_46_Branch)) {
    u_value v_y = u_field(s83, 2);
    t84 = v_y;
  }
  if (!t84) {
    u_no_case(s83);
  }
  return t84;
}

u_value f_syntax_46_Branch_46_pat_58__61__2(u_value v_x, u_value v_y) {
  u_value s85 = v_x;
  u_value t86 = NULL;
  if ((!t86) && u_is_con(s85, &n_syntax_46_Branch)) {
    u_value v_a1 = u_field(s85, 1);
    u_value v_a2 = u_field(s85, 2);
    t86 = u_mk_con(&n_syntax_46_Branch, 3, v_y, v_a1, v_a2);
  }
  if (!t86) {
    u_no_case(s85);
  }
  return t86;
}

u_value f_syntax_46_Branch_46_guard_58__61__2(u_value v_x, u_value v_y) {
  u_value s87 = v_x;
  u_value t88 = NULL;
  if ((!t88) && u_is_con(s87, &n_syntax_46_Branch)) {
    u_value v_a0 = u_field(s87, 0);
    u_value v_a2 = u_field(s87, 2);
    t88 = u_mk_con(&n_syntax_46_Branch, 3, v_a0, v_y, v_a2);
  }
  if (!t88) {
    u_no_case(s87);
  }
  return t88;
}

u_value f_syntax_46_Branch_46_body_58__61__2(u_value v_x, u_value v_y) {
  u_value s89 = v_x;
  u_value t90 = NULL;
  if ((!t90) && u_is_con(s89, &n_syntax_46_Branch)) {
    u_value v_a0 = u_field(s89, 0);
    u_value v_a1 = u_field(s89, 1);
    t90 = u_mk_con(&n_syntax_46_Branch, 3, v_a0, v_a1, v_y);
  }
  if (!t90) {
    u_no_case(s89);
  }
  return t90;
}

u_value f_syntax_46_Handler_46_typ_1(u_value v_x) {
  u_value s91 = v_x;
  u_value t92 = NULL;
  if ((!t92) && u_is_con(s91, &n_syntax_46_Handler)) {
    u_value v_y = u_field(s91, 0);
    t92 = v_y;
  }
  if (!t92) {
    u_no_case(s91);
  }
  return t92;
}

u_value f_syntax_46_Handler_46_pat_1(u_value v_x) {
  u_value s93 = v_x;
  u_value t94 = NULL;
  if ((!t94) && u_is_con(s93, &n_syntax_46_Handler)) {
    u_value v_y = u_field(s93, 1);
    t94 = v_y;
  }
  if (!t94) {
    u_no_case(s93);
  }
  return t94;
}

u_value f_syntax_46_Handler_46_guard_1(u_value v_x) {
  u_value s95 = v_x;
  u_value t96 = NULL;
  if ((!t96) && u_is_con(s95, &n_syntax_46_Handler)) {
    u_value v_y = u_field(s95, 2);
    t96 = v_y;
  }
  if (!t96) {
    u_no_case(s95);
  }
  return t96;
}

u_value f_syntax_46_Handler_46_body_1(u_value v_x) {
  u_value s97 = v_x;
  u_value t98 = NULL;
  if ((!t98) && u_is_con(s97, &n_syntax_46_Handler)) {
    u_value v_y = u_field(s97, 3);
    t98 = v_y;
  }
  if (!t98) {
    u_no_case(s97);
  }
  return t98;
}

u_value f_syntax_46_Handler_46_typ_58__61__2(u_value v_x, u_value v_y) {
  u_value s99 = v_x;
  u_value t100 = NULL;
  if ((!t100) && u_is_con(s99, &n_syntax_46_Handler)) {
    u_value v_a1 = u_field(s99, 1);
    u_value v_a2 = u_field(s99, 2);
    u_value v_a3 = u_field(s99, 3);
    t100 = u_mk_con(&n_syntax_46_Handler, 4, v_y, v_a1, v_a2, v_a3);
  }
  if (!t100) {
    u_no_case(s99);
  }
  return t100;
}

u_value f_syntax_46_Handler_46_pat_58__61__2(u_value v_x, u_value v_y) {
  u_value s101 = v_x;
  u_value t102 = NULL;
  if ((!t102) && u_is_con(s101, &n_syntax_46_Handler)) {
    u_value v_a0 = u_field(s101, 0);
    u_value v_a2 = u_field(s101, 2);
    u_value v_a3 = u_field(s101, 3);
    t102 = u_mk_con(&n_syntax_46_Handler, 4, v_a0, v_y, v_a2, v_a3);
  }
  if (!t102) {
    u_no_case(s101);
  }
  return t102;
}

u_value f_syntax_46_Handler_46_guard_58__61__2(u_value v_x, u_value v_y) {
  u_value s103 = v_x;
  u_value t104 = NULL;
  if ((!t104) && u_is_con(s103, &n_syntax_46_Handler)) {
    u_value v_a0 = u_field(s103, 0);
    u_value v_a1 = u_field(s103, 1);
    u_value v_a3 = u_field(s103, 3);
    t104 = u_mk_con(&n_syntax_46_Handler, 4, v_a0, v_a1, v_y, v_a3);
  }
  if (!t104) {
    u_no_case(s103);
  }
  return t104;
}

u_value f_syntax_46_Handler_46_body_58__61__2(u_value v_x, u_value v_y) {
  u_value s105 = v_x;
  u_value t106 = NULL;
  if ((!t106) && u_is_con(s105, &n_syntax_46_Handler)) {
    u_value v_a0 = u_field(s105, 0);
    u_value v_a1 = u_field(s105, 1);
    u_value v_a2 = u_field(s105, 2);
    t106 = u_mk_con(&n_syntax_46_Handler, 4, v_a0, v_a1, v_a2, v_y);
  }
  if (!t106) {
    u_no_case(s105);
  }
  return t106;
}

u_value f_syntax_46_Constr_46_name_1(u_value v_x) {
  u_value s107 = v_x;
  u_value t108 = NULL;
  if ((!t108) && u_is_con(s107, &n_syntax_46_Constr)) {
    u_value v_y = u_field(s107, 0);
    t108 = v_y;
  }
  if (!t108) {
    u_no_case(s107);
  }
  return t108;
}

u_value f_syntax_46_Constr_46_args_1(u_value v_x) {
  u_value s109 = v_x;
  u_value t110 = NULL;
  if ((!t110) && u_is_con(s109, &n_syntax_46_Constr)) {
    u_value v_y = u_field(s109, 1);
    t110 = v_y;
  }
  if (!t110) {
    u_no_case(s109);
  }
  return t110;
}

u_value f_syntax_46_Constr_46_name_58__61__2(u_value v_x, u_value v_y) {
  u_value s111 = v_x;
  u_value t112 = NULL;
  if ((!t112) && u_is_con(s111, &n_syntax_46_Constr)) {
    u_value v_a1 = u_field(s111, 1);
    t112 = u_mk_con(&n_syntax_46_Constr, 2, v_y, v_a1);
  }
  if (!t112) {
    u_no_case(s111);
  }
  return t112;
}

u_value f_syntax_46_Constr_46_args_58__61__2(u_value v_x, u_value v_y) {
  u_value s113 = v_x;
  u_value t114 = NULL;
  if ((!t114) && u_is_con(s113, &n_syntax_46_Constr)) {
    u_value v_a0 = u_field(s113, 0);
    t114 = u_mk_con(&n_syntax_46_Constr, 2, v_a0, v_y);
  }
  if (!t114) {
    u_no_case(s113);
  }
  return t114;
}

u_value f_syntax_46_Program_46_cmds_1(u_value v_x) {
  u_value s115 = v_x;
  u_value t116 = NULL;
  if ((!t116) && u_is_con(s115, &n_syntax_46_Program)) {
    u_value v_y = u_field(s115, 0);
    t116 = v_y;
  }
  if (!t116) {
    u_no_case(s115);
  }
  return t116;
}

u_value f_syntax_46_Program_46_cmds_58__61__2(u_value v_x, u_value v_y) {
  u_value s117 = v_x;
  u_value t118 = NULL;
  if ((!t118) && u_is_con(s117, &n_syntax_46_Program)) {
    t118 = u_mk_con(&n_syntax_46_Program, 1, v_y);
  }
  if (!t118) {
    u_no_case(s117);
  }
  return t118;
}

u_value f_syntax_46_unqualified_1(u_value v_x) {
  return u_mk_con(&n_syntax_46_Name, 2, &o_option_46_None, v_x);
}

u_value f_syntax_46_qualified_1(u_value v_n) {
  u_value s119 = v_n;
  u_value t120 = NULL;
  if ((!t120) && (u_is_con(s119, &n_syntax_46_Name) && u_is_con(u_field(s119, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s119, 0), 0);
    u_value v_b = u_field(s119, 1);
    static struct u_object l__46__121 = U_LIT(".");
    t120 = u_concat(v_m, u_concat(&l__46__121, v_b));
  }
  if ((!t120) && (u_is_con(s119, &n_syntax_46_Name) && u_is_con(u_field(s119, 0), &n_option_46_None))) {
    u_value v_b = u_field(s119, 1);
    t120 = v_b;
  }
  if (!t120) {
    u_no_case(s119);
  }
  return t120;
}

u_value f_syntax_46_list_name_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of syntax.list_name");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_list_122 = U_LIT("list");
  static struct u_object l_List_123 = U_LIT("List");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_list_122), &l_List_123);
  return cache;
}

u_value f_syntax_46_nil_name_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of syntax.nil_name");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_list_124 = U_LIT("list");
  static struct u_object l_Nil_125 = U_LIT("Nil");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_list_124), &l_Nil_125);
  return cache;
}

u_value f_syntax_46_cons_name_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of syntax.cons_name");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_list_126 = U_LIT("list");
  static struct u_object l_Cons_127 = U_LIT("Cons");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_list_126), &l_Cons_127);
  return cache;
}

u_value f_syntax_46_option_name_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of syntax.option_name");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_option_128 = U_LIT("option");
  static struct u_object l_Option_129 = U_LIT("Option");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_option_128), &l_Option_129);
  return cache;
}

u_value f_syntax_46_none_name_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of syntax.none_name");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_option_130 = U_LIT("option");
  static struct u_object l_None_131 = U_LIT("None");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_option_130), &l_None_131);
  return cache;
}

u_value f_syntax_46_some_name_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of syntax.some_name");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_option_132 = U_LIT("option");
  static struct u_object l_Some_133 = U_LIT("Some");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_option_132), &l_Some_133);
  return cache;
}

u_value f_syntax_46_os_error_name_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of syntax.os_error_name");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_os_134 = U_LIT("os");
  static struct u_object l_OsError_135 = U_LIT("OsError");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_os_134), &l_OsError_135);
  return cache;
}

u_value f_syntax_46_division_by_zero_name_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of syntax.division_by_zero_name");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_prelude_136 = U_LIT("prelude");
  static struct u_object l_DivisionByZero_137 = U_LIT("DivisionByZero");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_prelude_136), &l_DivisionByZero_137);
  return cache;
}

u_value f_syntax_46_no_index_name_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of syntax.no_index_name");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_prelude_138 = U_LIT("prelude");
  static struct u_object l_NoIndex_139 = U_LIT("NoIndex");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_prelude_138), &l_NoIndex_139);
  return cache;
}

u_value f_syntax_46_no_character_name_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of syntax.no_character_name");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_prelude_140 = U_LIT("prelude");
  static struct u_object l_NoCharacter_141 = U_LIT("NoCharacter");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_prelude_140), &l_NoCharacter_141);
  return cache;
}

u_value f_syntax_46_missing_field_name_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of syntax.missing_field_name");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_prelude_142 = U_LIT("prelude");
  static struct u_object l_MissingField_143 = U_LIT("MissingField");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_prelude_142), &l_MissingField_143);
  return cache;
}

u_value f_syntax_46_tuple_name_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of syntax.tuple_name");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l__44__144 = U_LIT(",");
  u_pop(handler);
  cache = f_syntax_46_unqualified_1(&l__44__144);
  return cache;
}

u_value f_syntax_46_names_1(u_value v_xs) {
  u_value t145 = &o_list_46_Nil;
  u_value *e146 = &t145;
  u_value l147 = v_xs;
  while (u_is_con(l147, &n_list_46_Cons)) {
    u_value v_x = u_field(l147, 0);
    if (u_truth(u_mk_bool(1))) {
      e146 = u_append(e146, f_syntax_46_Var_46_name_1(v_x));
    }
    l147 = u_field(l147, 1);
  }
  return t145;
}

u_value f_syntax_46_types_1(u_value v_xs) {
  u_value t148 = &o_list_46_Nil;
  u_value *e149 = &t148;
  u_value l150 = v_xs;
  while (u_is_con(l150, &n_list_46_Cons)) {
    u_value v_x = u_field(l150, 0);
    if (u_truth(u_mk_bool(1))) {
      e149 = u_append(e149, f_syntax_46_Var_46_typ_1(v_x));
    }
    l150 = u_field(l150, 1);
  }
  return t148;
}

u_value f_syntax_46_arity_1(u_value v_t) {
  u_value s151 = v_t;
  u_value t152 = NULL;
  if ((!t152) && u_is_con(s151, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s151, 0);
    t152 = f_list_46_length_1(v_args);
  }
  if (!t152) {
    t152 = u_mk_int(-1);
  }
  if (!t152) {
    u_no_case(s151);
  }
  return t152;
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__153 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__153, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__123__125__44__59__46__154 = U_LIT("()[]{},;.");
  return u_str_contains(&l__40__41__91__93__123__125__44__59__46__154, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t155 = u_is_letter(v_c);
  if (!u_truth(t155)) {
    t155 = u_is_digit(v_c);
  }
  u_value t156 = t155;
  if (!u_truth(t156)) {
    t156 = u_eq(v_c, u_mk_char(95));
  }
  return t156;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t157;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t157 = u_str_get(v_s, v_i);
  } else {
    t157 = u_chr(u_mk_int(0));
  }
  return t157;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s158 = v_c;
  u_value t159 = NULL;
  if ((!t159) && u_equals(s158, u_mk_char(34))) {
    t159 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t159) && u_equals(s158, u_mk_char(39))) {
    t159 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t159) && u_equals(s158, u_mk_char(92))) {
    t159 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t159) && u_equals(s158, u_mk_char(110))) {
    t159 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t159) && u_equals(s158, u_mk_char(116))) {
    t159 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t159) && u_equals(s158, u_mk_char(114))) {
    t159 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t159) {
    t159 = &o_option_46_None;
  }
  if (!t159) {
    u_no_case(s158);
  }
  return t159;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t160;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t160 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t160 = v_i;
  }
  return t160;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t161 = u_mk_int(0);
    u_value t162 = u_mk_int(1);
    u_value t163 = u_mk_int(1);
    v_j = t161;
    *r_line = t162;
    *r_column = t163;
    u_value t168 = u_lt(v_j, v_i);
    while (u_truth(t168)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t164 = u_add(*r_line, u_mk_int(1));
            u_value t165 = u_mk_int(1);
            *r_line = t164;
            *r_column = t165;
          }
        } else {
          {
            u_value t166 = u_add(*r_column, u_mk_int(1));
            *r_column = t166;
          }
        }
        u_value t167 = u_add(v_j, u_mk_int(1));
        v_j = t167;
      }
      t168 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t169 = *r_i;
    u_value t170 = u_add(*r_i, u_mk_int(1));
    u_value t171 = &o_list_46_Nil;
    v_start = t169;
    *r_i = t170;
    v_parts = t171;
    u_value t172 = *r_i;
    v_from = t172;
    u_value t183 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t183)) {
      {
        u_value t173 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t173;
        u_value t174 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t174)) {
          t174 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t174)) {
          {
            static struct u_object l_unterminated_32_lit_175 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_175, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s176 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d177 = 0;
            if ((!d177) && u_is_con(s176, &n_option_46_Some)) {
              u_value v_d = u_field(s176, 0);
              d177 = 1;
              {
                u_value t178 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, &o_list_46_Nil)), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t179 = u_add(*r_i, u_mk_int(2));
                v_parts = t178;
                *r_i = t179;
                u_value t180 = *r_i;
                v_from = t180;
              }
            }
            if ((!d177) && u_is_con(s176, &n_option_46_None)) {
              d177 = 1;
              {
                static struct u_object l_invalid_32_escape_181 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_181, *r_i), "lex.LexError");
              }
            }
            if (!d177) {
              u_no_case(s176);
            }
          }
        } else {
          {
            u_value t182 = u_add(*r_i, u_mk_int(1));
            *r_i = t182;
          }
        }
      }
      t183 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__184 = U_LIT("");
    u_value t185 = u_str_join(&l__184, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t186 = u_add(*r_i, u_mk_int(1));
    *r_text = t185;
    *r_i = t186;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t187 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t187;
    u_value t188 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t189 = v_j;
    *r_t = t188;
    *r_i = t189;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t190 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t190;
    u_value s191 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d192 = 0;
    if ((!d192) && u_is_con(s191, &n_option_46_Some)) {
      u_value v_n = u_field(s191, 0);
      d192 = 1;
      {
        u_value t193 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t194 = v_j;
        *r_t = t193;
        *r_i = t194;
      }
    }
    if ((!d192) && u_is_con(s191, &n_option_46_None)) {
      d192 = 1;
      {
        static struct u_object l_invalid_32_number_195 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_195, *r_i), "lex.LexError");
      }
    }
    if (!d192) {
      u_no_case(s191);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t196 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t196;
    u_value t197 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t198 = v_j;
    *r_t = t197;
    *r_i = t198;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t199 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t199;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t200 = *r_i;
    v_start = t200;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_201 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_201, v_start), "lex.LexError");
      }
    }
    u_value t202 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t202;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t203 = u_str_get(v_s, *r_i);
    v_c = t203;
    u_value s204 = v_c;
    int d205 = 0;
    if (!d205) {
      u_value t206 = u_is_letter(v_c);
      if (!u_truth(t206)) {
        t206 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t206)) {
        d205 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d205) {
      if (u_truth(u_is_digit(v_c))) {
        d205 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d205) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d205 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d205) && u_equals(s204, u_mk_char(34))) {
      d205 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d205) && u_equals(s204, u_mk_char(39))) {
      d205 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d205) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d205 = 1;
        {
          u_value t207 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t208 = u_add(*r_i, u_mk_int(1));
          *r_t = t207;
          *r_i = t208;
        }
      }
    }
    if (!d205) {
      d205 = 1;
      {
        static struct u_object l_unexpected_32_chara_209 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_209, *r_i), "lex.LexError");
      }
    }
    if (!d205) {
      u_no_case(s204);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t210 = u_mk_int(0);
    v_i = t210;
    u_value t211 = &o_list_46_Nil;
    v_rev = t211;
    u_value t218 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t218)) {
      {
        u_value t212 = v_i;
        v_start = t212;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t213 = u_add(v_i, u_mk_int(1));
            v_i = t213;
          }
        } else {
          u_value t214 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t214)) {
            t214 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t214)) {
            {
              u_value t216 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam215, 0));
              v_i = t216;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t217 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t217;
            }
          }
        }
      }
      t218 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t219 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_lex_46_TEnd, v_i), v_rev));
    *r_ts = t219;
  }
}

u_value f_lex_46_tokens_1(u_value v_ts) {
  return f_list_46_firsts_1(v_ts);
}

u_value f_set_46_empty_set_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of set.empty_set");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  u_pop(handler);
  cache = &o_set_46_SLeaf;
  return cache;
}

u_value f_set_46_member_2(u_value v_x, u_value v_s) {
  u_value s220 = v_s;
  u_value t221 = NULL;
  if ((!t221) && u_is_con(s220, &n_set_46_SLeaf)) {
    t221 = u_mk_bool(0);
  }
  if ((!t221) && u_is_con(s220, &n_set_46_SNode)) {
    u_value v_l = u_field(s220, 0);
    u_value v_y = u_field(s220, 1);
    u_value v_r = u_field(s220, 2);
    u_value s222 = u_compare(v_x, v_y);
    u_value t223 = NULL;
    if ((!t223) && u_equals(s222, u_mk_int(-1))) {
      t223 = f_set_46_member_2(v_x, v_l);
    }
    if ((!t223) && u_equals(s222, u_mk_int(1))) {
      t223 = f_set_46_member_2(v_x, v_r);
    }
    if (!t223) {
      t223 = u_mk_bool(1);
    }
    if (!t223) {
      u_no_case(s222);
    }
    t221 = t223;
  }
  if (!t221) {
    u_no_case(s220);
  }
  return t221;
}

u_value f_set_46_insert_2(u_value v_x, u_value v_s) {
  u_value s224 = v_s;
  u_value t225 = NULL;
  if ((!t225) && u_is_con(s224, &n_set_46_SLeaf)) {
    t225 = u_mk_con(&n_set_46_SNode, 4, &o_set_46_SLeaf, v_x, &o_set_46_SLeaf, u_mk_int(1));
  }
  if ((!t225) && u_is_con(s224, &n_set_46_SNode)) {
    u_value v_l = u_field(s224, 0);
    u_value v_y = u_field(s224, 1);
    u_value v_r = u_field(s224, 2);
    u_value s226 = u_compare(v_x, v_y);
    u_value t227 = NULL;
    if ((!t227) && u_equals(s226, u_mk_int(-1))) {
      t227 = f_set_46_rebalance_3(f_set_46_insert_2(v_x, v_l), v_y, v_r);
    }
    if ((!t227) && u_equals(s226, u_mk_int(1))) {
      t227 = f_set_46_rebalance_3(v_l, v_y, f_set_46_insert_2(v_x, v_r));
    }
    if (!t227) {
      t227 = v_s;
    }
    if (!t227) {
      u_no_case(s226);
    }
    t225 = t227;
  }
  if (!t225) {
    u_no_case(s224);
  }
  return t225;
}

u_value f_set_46_from_list_1(u_value v_xs) {
  return f_set_46_insert_all_2(v_xs, &o_set_46_SLeaf);
}

u_value f_set_46_insert_all_2(u_value v_xs, u_value v_s) {
  u_value s228 = v_xs;
  u_value t229 = NULL;
  if ((!t229) && u_is_con(s228, &n_list_46_Nil)) {
    t229 = v_s;
  }
  if ((!t229) && u_is_con(s228, &n_list_46_Cons)) {
    u_value v_x = u_field(s228, 0);
    u_value v_rest = u_field(s228, 1);
    t229 = f_set_46_insert_all_2(v_rest, f_set_46_insert_2(v_x, v_s));
  }
  if (!t229) {
    u_no_case(s228);
  }
  return t229;
}

u_value f_set_46_elements_1(u_value v_s) {
  return f_set_46_elements_onto_2(v_s, &o_list_46_Nil);
}

u_value f_set_46_elements_onto_2(u_value v_s, u_value v_rest) {
  u_value s230 = v_s;
  u_value t231 = NULL;
  if ((!t231) && u_is_con(s230, &n_set_46_SLeaf)) {
    t231 = v_rest;
  }
  if ((!t231) && u_is_con(s230, &n_set_46_SNode)) {
    u_value v_l = u_field(s230, 0);
    u_value v_x = u_field(s230, 1);
    u_value v_r = u_field(s230, 2);
    t231 = f_set_46_elements_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, v_x, f_set_46_elements_onto_2(v_r, v_rest)));
  }
  if (!t231) {
    u_no_case(s230);
  }
  return t231;
}

u_value f_set_46_count_1(u_value v_s) {
  u_value s232 = v_s;
  u_value t233 = NULL;
  if ((!t233) && u_is_con(s232, &n_set_46_SLeaf)) {
    t233 = u_mk_int(0);
  }
  if ((!t233) && u_is_con(s232, &n_set_46_SNode)) {
    u_value v_l = u_field(s232, 0);
    u_value v_r = u_field(s232, 2);
    t233 = u_add(u_add(f_set_46_count_1(v_l), u_mk_int(1)), f_set_46_count_1(v_r));
  }
  if (!t233) {
    u_no_case(s232);
  }
  return t233;
}

u_value f_set_46_depth_1(u_value v_s) {
  u_value s234 = v_s;
  u_value t235 = NULL;
  if ((!t235) && u_is_con(s234, &n_set_46_SLeaf)) {
    t235 = u_mk_int(0);
  }
  if ((!t235) && u_is_con(s234, &n_set_46_SNode)) {
    u_value v_d = u_field(s234, 3);
    t235 = v_d;
  }
  if (!t235) {
    u_no_case(s234);
  }
  return t235;
}

u_value f_set_46_snode_depth_2(u_value v_l, u_value v_r) {
  u_value t236;
  if (u_truth(u_lt(f_set_46_depth_1(v_l), f_set_46_depth_1(v_r)))) {
    t236 = f_set_46_depth_1(v_r);
  } else {
    t236 = f_set_46_depth_1(v_l);
  }
  return u_add(u_mk_int(1), t236);
}

u_value f_set_46_snode_3(u_value v_l, u_value v_x, u_value v_r) {
  return u_mk_con(&n_set_46_SNode, 4, v_l, v_x, v_r, f_set_46_snode_depth_2(v_l, v_r));
}

u_value f_set_46_turn_left_1(u_value v_s) {
  u_value s237 = v_s;
  u_value t238 = NULL;
  if ((!t238) && (u_is_con(s237, &n_set_46_SNode) && u_is_con(u_field(s237, 2), &n_set_46_SNode))) {
    u_value v_a = u_field(s237, 0);
    u_value v_x = u_field(s237, 1);
    u_value v_b = u_field(u_field(s237, 2), 0);
    u_value v_y = u_field(u_field(s237, 2), 1);
    u_value v_c = u_field(u_field(s237, 2), 2);
    t238 = f_set_46_snode_3(f_set_46_snode_3(v_a, v_x, v_b), v_y, v_c);
  }
  if (!t238) {
    t238 = v_s;
  }
  if (!t238) {
    u_no_case(s237);
  }
  return t238;
}

u_value f_set_46_turn_right_1(u_value v_s) {
  u_value s239 = v_s;
  u_value t240 = NULL;
  if ((!t240) && (u_is_con(s239, &n_set_46_SNode) && u_is_con(u_field(s239, 0), &n_set_46_SNode))) {
    u_value v_a = u_field(u_field(s239, 0), 0);
    u_value v_x = u_field(u_field(s239, 0), 1);
    u_value v_b = u_field(u_field(s239, 0), 2);
    u_value v_y = u_field(s239, 1);
    u_value v_c = u_field(s239, 2);
    t240 = f_set_46_snode_3(v_a, v_x, f_set_46_snode_3(v_b, v_y, v_c));
  }
  if (!t240) {
    t240 = v_s;
  }
  if (!t240) {
    u_no_case(s239);
  }
  return t240;
}

u_value f_set_46_smaller_1(u_value v_s) {
  u_value s241 = v_s;
  u_value t242 = NULL;
  if ((!t242) && u_is_con(s241, &n_set_46_SNode)) {
    u_value v_l = u_field(s241, 0);
    t242 = v_l;
  }
  if (!t242) {
    t242 = v_s;
  }
  if (!t242) {
    u_no_case(s241);
  }
  return t242;
}

u_value f_set_46_greater_1(u_value v_s) {
  u_value s243 = v_s;
  u_value t244 = NULL;
  if ((!t244) && u_is_con(s243, &n_set_46_SNode)) {
    u_value v_r = u_field(s243, 2);
    t244 = v_r;
  }
  if (!t244) {
    t244 = v_s;
  }
  if (!t244) {
    u_no_case(s243);
  }
  return t244;
}

u_value f_set_46_rebalance_3(u_value v_l, u_value v_x, u_value v_r) {
  u_value t248;
  if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1))))) {
    u_value t245;
    if (u_truth(u_lt(f_set_46_depth_1(f_set_46_smaller_1(v_l)), f_set_46_depth_1(f_set_46_greater_1(v_l))))) {
      t245 = f_set_46_turn_left_1(v_l);
    } else {
      t245 = v_l;
    }
    t248 = f_set_46_turn_right_1(f_set_46_snode_3(t245, v_x, v_r));
  } else {
    u_value t247;
    if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1))))) {
      u_value t246;
      if (u_truth(u_lt(f_set_46_depth_1(f_set_46_greater_1(v_r)), f_set_46_depth_1(f_set_46_smaller_1(v_r))))) {
        t246 = f_set_46_turn_right_1(v_r);
      } else {
        t246 = v_r;
      }
      t247 = f_set_46_turn_left_1(f_set_46_snode_3(v_l, v_x, t246));
    } else {
      t247 = f_set_46_snode_3(v_l, v_x, v_r);
    }
    t248 = t247;
  }
  return t248;
}

u_value f_set_46_well_formed_1(u_value v_s) {
  u_value s249 = v_s;
  u_value t250 = NULL;
  if ((!t250) && u_is_con(s249, &n_set_46_SLeaf)) {
    t250 = u_mk_bool(1);
  }
  if ((!t250) && u_is_con(s249, &n_set_46_SNode)) {
    u_value v_l = u_field(s249, 0);
    u_value v_r = u_field(s249, 2);
    u_value v_d = u_field(s249, 3);
    u_value t251 = f_set_46_well_formed_1(v_l);
    if (u_truth(t251)) {
      t251 = f_set_46_well_formed_1(v_r);
    }
    u_value t252 = t251;
    if (u_truth(t252)) {
      t252 = u_eq(v_d, f_set_46_snode_depth_2(v_l, v_r));
    }
    u_value t253 = t252;
    if (u_truth(t253)) {
      t253 = f_prelude_46__60__61__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1)));
    }
    u_value t254 = t253;
    if (u_truth(t254)) {
      t254 = f_prelude_46__60__61__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1)));
    }
    t250 = t254;
  }
  if (!t250) {
    u_no_case(s249);
  }
  return t250;
}

u_value f_set_46_upto_1(u_value v_n) {
  u_value t255;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t255 = &o_list_46_Nil;
  } else {
    t255 = f_list_46__43__43__2(f_set_46_upto_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, &o_list_46_Nil));
  }
  return t255;
}

u_value f_parse_46_Notations_46_cmds_1(u_value v_x) {
  u_value s256 = v_x;
  u_value t257 = NULL;
  if ((!t257) && u_is_con(s256, &n_parse_46_Notations)) {
    u_value v_y = u_field(s256, 0);
    t257 = v_y;
  }
  if (!t257) {
    u_no_case(s256);
  }
  return t257;
}

u_value f_parse_46_Notations_46_ops_1(u_value v_x) {
  u_value s258 = v_x;
  u_value t259 = NULL;
  if ((!t259) && u_is_con(s258, &n_parse_46_Notations)) {
    u_value v_y = u_field(s258, 1);
    t259 = v_y;
  }
  if (!t259) {
    u_no_case(s258);
  }
  return t259;
}

u_value f_parse_46_Notations_46_cmds_58__61__2(u_value v_x, u_value v_y) {
  u_value s260 = v_x;
  u_value t261 = NULL;
  if ((!t261) && u_is_con(s260, &n_parse_46_Notations)) {
    u_value v_a1 = u_field(s260, 1);
    t261 = u_mk_con(&n_parse_46_Notations, 2, v_y, v_a1);
  }
  if (!t261) {
    u_no_case(s260);
  }
  return t261;
}

u_value f_parse_46_Notations_46_ops_58__61__2(u_value v_x, u_value v_y) {
  u_value s262 = v_x;
  u_value t263 = NULL;
  if ((!t263) && u_is_con(s262, &n_parse_46_Notations)) {
    u_value v_a0 = u_field(s262, 0);
    t263 = u_mk_con(&n_parse_46_Notations, 2, v_a0, v_y);
  }
  if (!t263) {
    u_no_case(s262);
  }
  return t263;
}

u_value f_parse_46_Scope_46_params_1(u_value v_x) {
  u_value s264 = v_x;
  u_value t265 = NULL;
  if ((!t265) && u_is_con(s264, &n_parse_46_Scope)) {
    u_value v_y = u_field(s264, 0);
    t265 = v_y;
  }
  if (!t265) {
    u_no_case(s264);
  }
  return t265;
}

u_value f_parse_46_Scope_46_notations_1(u_value v_x) {
  u_value s266 = v_x;
  u_value t267 = NULL;
  if ((!t267) && u_is_con(s266, &n_parse_46_Scope)) {
    u_value v_y = u_field(s266, 1);
    t267 = v_y;
  }
  if (!t267) {
    u_no_case(s266);
  }
  return t267;
}

u_value f_parse_46_Scope_46_params_58__61__2(u_value v_x, u_value v_y) {
  u_value s268 = v_x;
  u_value t269 = NULL;
  if ((!t269) && u_is_con(s268, &n_parse_46_Scope)) {
    u_value v_a1 = u_field(s268, 1);
    t269 = u_mk_con(&n_parse_46_Scope, 2, v_y, v_a1);
  }
  if (!t269) {
    u_no_case(s268);
  }
  return t269;
}

u_value f_parse_46_Scope_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s270 = v_x;
  u_value t271 = NULL;
  if ((!t271) && u_is_con(s270, &n_parse_46_Scope)) {
    u_value v_a0 = u_field(s270, 0);
    t271 = u_mk_con(&n_parse_46_Scope, 2, v_a0, v_y);
  }
  if (!t271) {
    u_no_case(s270);
  }
  return t271;
}

u_value f_parse_46_keywords_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of parse.keywords");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l___272 = U_LIT("_");
  static struct u_object l_assert_273 = U_LIT("assert");
  static struct u_object l_assign_274 = U_LIT("assign");
  static struct u_object l_call_275 = U_LIT("call");
  static struct u_object l_case_276 = U_LIT("case");
  static struct u_object l_catch_277 = U_LIT("catch");
  static struct u_object l_data_278 = U_LIT("data");
  static struct u_object l_else_279 = U_LIT("else");
  static struct u_object l_end_280 = U_LIT("end");
  static struct u_object l_false_281 = U_LIT("false");
  static struct u_object l_for_282 = U_LIT("for");
  static struct u_object l_function_283 = U_LIT("function");
  static struct u_object l_if_284 = U_LIT("if");
  static struct u_object l_import_285 = U_LIT("import");
  static struct u_object l_in_286 = U_LIT("in");
  static struct u_object l_infix_287 = U_LIT("infix");
  static struct u_object l_lambda_288 = U_LIT("lambda");
  static struct u_object l_let_289 = U_LIT("let");
  static struct u_object l_local_290 = U_LIT("local");
  static struct u_object l_match_291 = U_LIT("match");
  static struct u_object l_out_292 = U_LIT("out");
  static struct u_object l_postfix_293 = U_LIT("postfix");
  static struct u_object l_prefix_294 = U_LIT("prefix");
  static struct u_object l_procedure_295 = U_LIT("procedure");
  static struct u_object l_raise_296 = U_LIT("raise");
  static struct u_object l_ref_297 = U_LIT("ref");
  static struct u_object l_skip_298 = U_LIT("skip");
  static struct u_object l_test_299 = U_LIT("test");
  static struct u_object l_then_300 = U_LIT("then");
  static struct u_object l_true_301 = U_LIT("true");
  static struct u_object l_type_302 = U_LIT("type");
  static struct u_object l_try_303 = U_LIT("try");
  static struct u_object l_while_304 = U_LIT("while");
  static struct u_object l_with_305 = U_LIT("with");
  u_pop(handler);
  cache = f_set_46_from_list_1(u_mk_con(&n_list_46_Cons, 2, &l___272, u_mk_con(&n_list_46_Cons, 2, &l_assert_273, u_mk_con(&n_list_46_Cons, 2, &l_assign_274, u_mk_con(&n_list_46_Cons, 2, &l_call_275, u_mk_con(&n_list_46_Cons, 2, &l_case_276, u_mk_con(&n_list_46_Cons, 2, &l_catch_277, u_mk_con(&n_list_46_Cons, 2, &l_data_278, u_mk_con(&n_list_46_Cons, 2, &l_else_279, u_mk_con(&n_list_46_Cons, 2, &l_end_280, u_mk_con(&n_list_46_Cons, 2, &l_false_281, u_mk_con(&n_list_46_Cons, 2, &l_for_282, u_mk_con(&n_list_46_Cons, 2, &l_function_283, u_mk_con(&n_list_46_Cons, 2, &l_if_284, u_mk_con(&n_list_46_Cons, 2, &l_import_285, u_mk_con(&n_list_46_Cons, 2, &l_in_286, u_mk_con(&n_list_46_Cons, 2, &l_infix_287, u_mk_con(&n_list_46_Cons, 2, &l_lambda_288, u_mk_con(&n_list_46_Cons, 2, &l_let_289, u_mk_con(&n_list_46_Cons, 2, &l_local_290, u_mk_con(&n_list_46_Cons, 2, &l_match_291, u_mk_con(&n_list_46_Cons, 2, &l_out_292, u_mk_con(&n_list_46_Cons, 2, &l_postfix_293, u_mk_con(&n_list_46_Cons, 2, &l_prefix_294, u_mk_con(&n_list_46_Cons, 2, &l_procedure_295, u_mk_con(&n_list_46_Cons, 2, &l_raise_296, u_mk_con(&n_list_46_Cons, 2, &l_ref_297, u_mk_con(&n_list_46_Cons, 2, &l_skip_298, u_mk_con(&n_list_46_Cons, 2, &l_test_299, u_mk_con(&n_list_46_Cons, 2, &l_then_300, u_mk_con(&n_list_46_Cons, 2, &l_true_301, u_mk_con(&n_list_46_Cons, 2, &l_type_302, u_mk_con(&n_list_46_Cons, 2, &l_try_303, u_mk_con(&n_list_46_Cons, 2, &l_while_304, u_mk_con(&n_list_46_Cons, 2, &l_with_305, &o_list_46_Nil)))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_set_46_member_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__306 = U_LIT("=");
  static struct u_object l__61__62__307 = U_LIT("=>");
  static struct u_object l__58__61__308 = U_LIT(":=");
  static struct u_object l__58__309 = U_LIT(":");
  static struct u_object l__124__310 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__306, u_mk_con(&n_list_46_Cons, 2, &l__61__62__307, u_mk_con(&n_list_46_Cons, 2, &l__58__61__308, u_mk_con(&n_list_46_Cons, 2, &l__58__309, u_mk_con(&n_list_46_Cons, 2, &l__124__310, &o_list_46_Nil))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s311 = v_ns;
  u_value t312 = NULL;
  if ((!t312) && u_is_con(s311, &n_list_46_Nil)) {
    t312 = &o_option_46_None;
  }
  if ((!t312) && (u_is_con(s311, &n_list_46_Cons) && (u_is_con(u_field(s311, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s311, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s311, 0), 1);
    u_value v_ops = u_field(u_field(s311, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t312 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t312) && u_is_con(s311, &n_list_46_Cons)) {
    u_value v_more = u_field(s311, 1);
    t312 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t312) {
    u_no_case(s311);
  }
  return t312;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s313 = v_ns;
  u_value t314 = NULL;
  if ((!t314) && u_is_con(s313, &n_list_46_Nil)) {
    t314 = &o_option_46_None;
  }
  if ((!t314) && (u_is_con(s313, &n_list_46_Cons) && (u_is_con(u_field(s313, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s313, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s313, 0), 1);
    u_value v_ops = u_field(u_field(s313, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t314 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t314) && u_is_con(s313, &n_list_46_Cons)) {
    u_value v_more = u_field(s313, 1);
    t314 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t314) {
    u_no_case(s313);
  }
  return t314;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s315 = v_ns;
  u_value t316 = NULL;
  if ((!t316) && u_is_con(s315, &n_list_46_Nil)) {
    t316 = &o_option_46_None;
  }
  if ((!t316) && (u_is_con(s315, &n_list_46_Cons) && u_is_con(u_field(s315, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s315, 0), 0);
    u_value v_p = u_field(u_field(s315, 0), 1);
    u_value v_ops = u_field(u_field(s315, 0), 2);
    u_value t317 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t317)) {
      t317 = f_prelude_46__33__61__2(v_f, &o_syntax_46_FPrefix);
    }
    u_value t318 = t317;
    if (u_truth(t318)) {
      t318 = f_prelude_46__33__61__2(v_f, &o_syntax_46_FPostfix);
    }
    if (u_truth(t318)) {
      t316 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t316) && u_is_con(s315, &n_list_46_Cons)) {
    u_value v_more = u_field(s315, 1);
    t316 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t316) {
    u_no_case(s315);
  }
  return t316;
}

u_value f_parse_46_declared_2(u_value v_ns, u_value v_op) {
  return f_set_46_member_2(v_op, f_parse_46_Notations_46_ops_1(v_ns));
}

u_value f_parse_46_no_notations_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of parse.no_notations");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  u_pop(handler);
  cache = u_mk_con(&n_parse_46_Notations, 2, &o_list_46_Nil, f_set_46_empty_set_0());
  return cache;
}

u_value f_parse_46_with_notations_2(u_value v_ns, u_value v_cs) {
  return u_mk_con(&n_parse_46_Notations, 2, f_list_46__43__43__2(f_parse_46_Notations_46_cmds_1(v_ns), v_cs), f_set_46_insert_all_2(f_parse_46_operators_of_1(v_cs), f_parse_46_Notations_46_ops_1(v_ns)));
}

u_value f_parse_46_operators_of_1(u_value v_cs) {
  u_value s319 = v_cs;
  u_value t320 = NULL;
  if ((!t320) && u_is_con(s319, &n_list_46_Nil)) {
    t320 = &o_list_46_Nil;
  }
  if ((!t320) && (u_is_con(s319, &n_list_46_Cons) && u_is_con(u_field(s319, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s319, 0), 2);
    u_value v_more = u_field(s319, 1);
    t320 = f_list_46__43__43__2(v_ops, f_parse_46_operators_of_1(v_more));
  }
  if ((!t320) && u_is_con(s319, &n_list_46_Cons)) {
    u_value v_more = u_field(s319, 1);
    t320 = f_parse_46_operators_of_1(v_more);
  }
  if (!t320) {
    u_no_case(s319);
  }
  return t320;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s321 = v_ts;
  u_value t322 = NULL;
  if ((!t322) && u_is_con(s321, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s321, 0), 0);
    t322 = v_t;
  }
  if (!t322) {
    t322 = &o_lex_46_TEnd;
  }
  if (!t322) {
    u_no_case(s321);
  }
  return t322;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s323 = v_ts;
  u_value t324 = NULL;
  if ((!t324) && (u_is_con(s323, &n_list_46_Cons) && u_is_con(u_field(s323, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s323, 1), 0), 0);
    t324 = v_t;
  }
  if (!t324) {
    t324 = &o_lex_46_TEnd;
  }
  if (!t324) {
    u_no_case(s323);
  }
  return t324;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s325 = v_ts;
  u_value t326 = NULL;
  if ((!t326) && u_is_con(s325, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s325, 0), 1);
    t326 = v_i;
  }
  if (!t326) {
    t326 = u_mk_int(0);
  }
  if (!t326) {
    u_no_case(s325);
  }
  return t326;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s327 = v_t;
  u_value t328 = NULL;
  if ((!t328) && u_is_con(s327, &n_lex_46_TName)) {
    u_value v_n = u_field(s327, 0);
    t328 = v_n;
  }
  if ((!t328) && u_is_con(s327, &n_lex_46_TSym)) {
    u_value v_s = u_field(s327, 0);
    t328 = v_s;
  }
  if ((!t328) && u_is_con(s327, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s327, 0);
    t328 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, &o_list_46_Nil));
  }
  if ((!t328) && u_is_con(s327, &n_lex_46_TNum)) {
    u_value v_n = u_field(s327, 0);
    t328 = u_str_show(v_n);
  }
  if ((!t328) && u_is_con(s327, &n_lex_46_TStr)) {
    u_value v_s = u_field(s327, 0);
    t328 = u_str_quote(v_s);
  }
  if ((!t328) && u_is_con(s327, &n_lex_46_TChar)) {
    u_value v_c = u_field(s327, 0);
    static struct u_object l__39__329 = U_LIT("'");
    static struct u_object l__39__330 = U_LIT("'");
    t328 = u_concat(&l__39__329, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, &o_list_46_Nil)), &l__39__330));
  }
  if ((!t328) && u_is_con(s327, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_331 = U_LIT("end of input");
    t328 = &l_end_32_of_32_input_331;
  }
  if (!t328) {
    u_no_case(s327);
  }
  return t328;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s332 = f_parse_46_head_token_1(v_ts);
  u_value t333 = NULL;
  if ((!t333) && u_is_con(s332, &n_lex_46_TSym)) {
    u_value v_s = u_field(s332, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t333 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t333) && u_is_con(s332, &n_lex_46_TName)) {
    u_value v_n = u_field(s332, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t333 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t333) {
    t333 = &o_option_46_None;
  }
  if (!t333) {
    u_no_case(s332);
  }
  return t333;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s334 = *r_ts;
    int d335 = 0;
    if ((!d335) && u_is_con(s334, &n_list_46_Cons)) {
      u_value v_more = u_field(s334, 1);
      d335 = 1;
      {
        u_value t336 = v_more;
        *r_ts = t336;
      }
    }
    if (!d335) {
      d335 = 1;
      {
      }
    }
    if (!d335) {
      u_no_case(s334);
    }
  }
}

void p_parse_46_expect(u_value v_t, u_value *r_ts) {
  {
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), v_t))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        static struct u_object l__32_expected_32_but_32__337 = U_LIT(" expected but ");
        static struct u_object l__32_found_338 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__337, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_338))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
  }
}

void p_parse_46_keyword(u_value v_w, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TName, 1, v_w), r_ts);
  }
}

void p_parse_46_parse_name(u_value *r_n, u_value *r_ts) {
  {
    u_value s339 = f_parse_46_head_token_1(*r_ts);
    int d340 = 0;
    if ((!d340) && u_is_con(s339, &n_lex_46_TName)) {
      u_value v_m = u_field(s339, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d340 = 1;
        {
          u_value t341 = v_m;
          *r_n = t341;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d340) {
      d340 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_342 = U_LIT("name expected but ");
        static struct u_object l__32_found_343 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_342, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_343)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d340) {
      u_no_case(s339);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s344 = f_parse_46_head_token_1(*r_ts);
    int d345 = 0;
    if ((!d345) && u_is_con(s344, &n_lex_46_TName)) {
      u_value v_m = u_field(s344, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_m))) {
        d345 = 1;
        {
          static struct u_object l_operator_32__346 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_347 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__346, u_concat(v_m, &l__32_is_32_not_32_a_32_name_347)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d345) {
      d345 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d345) {
      u_no_case(s344);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s348 = f_parse_46_head_token_1(*r_ts);
    int d349 = 0;
    if ((!d349) && u_is_con(s348, &n_lex_46_TSym)) {
      u_value v_s = u_field(s348, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d349 = 1;
        {
          u_value t350 = v_s;
          *r_n = t350;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d349) {
      d349 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d349) {
      u_no_case(s348);
    }
  }
}

u_value f_parse_46_Ops_46_notations_1(u_value v_x) {
  u_value s351 = v_x;
  u_value t352 = NULL;
  if ((!t352) && u_is_con(s351, &n_parse_46_Ops)) {
    u_value v_y = u_field(s351, 0);
    t352 = v_y;
  }
  if (!t352) {
    u_no_case(s351);
  }
  return t352;
}

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x) {
  u_value s353 = v_x;
  u_value t354 = NULL;
  if ((!t354) && u_is_con(s353, &n_parse_46_Ops)) {
    u_value v_y = u_field(s353, 1);
    t354 = v_y;
  }
  if (!t354) {
    u_no_case(s353);
  }
  return t354;
}

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x) {
  u_value s355 = v_x;
  u_value t356 = NULL;
  if ((!t356) && u_is_con(s355, &n_parse_46_Ops)) {
    u_value v_y = u_field(s355, 2);
    t356 = v_y;
  }
  if (!t356) {
    u_no_case(s355);
  }
  return t356;
}

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x) {
  u_value s357 = v_x;
  u_value t358 = NULL;
  if ((!t358) && u_is_con(s357, &n_parse_46_Ops)) {
    u_value v_y = u_field(s357, 3);
    t358 = v_y;
  }
  if (!t358) {
    u_no_case(s357);
  }
  return t358;
}

u_value f_parse_46_Ops_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s359 = v_x;
  u_value t360 = NULL;
  if ((!t360) && u_is_con(s359, &n_parse_46_Ops)) {
    u_value v_a1 = u_field(s359, 1);
    u_value v_a2 = u_field(s359, 2);
    u_value v_a3 = u_field(s359, 3);
    t360 = u_mk_con(&n_parse_46_Ops, 4, v_y, v_a1, v_a2, v_a3);
  }
  if (!t360) {
    u_no_case(s359);
  }
  return t360;
}

u_value f_parse_46_Ops_46_prefix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s361 = v_x;
  u_value t362 = NULL;
  if ((!t362) && u_is_con(s361, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s361, 0);
    u_value v_a2 = u_field(s361, 2);
    u_value v_a3 = u_field(s361, 3);
    t362 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_y, v_a2, v_a3);
  }
  if (!t362) {
    u_no_case(s361);
  }
  return t362;
}

u_value f_parse_46_Ops_46_postfix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s363 = v_x;
  u_value t364 = NULL;
  if ((!t364) && u_is_con(s363, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s363, 0);
    u_value v_a1 = u_field(s363, 1);
    u_value v_a3 = u_field(s363, 3);
    t364 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_y, v_a3);
  }
  if (!t364) {
    u_no_case(s363);
  }
  return t364;
}

u_value f_parse_46_Ops_46_infix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s365 = v_x;
  u_value t366 = NULL;
  if ((!t366) && u_is_con(s365, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s365, 0);
    u_value v_a1 = u_field(s365, 1);
    u_value v_a2 = u_field(s365, 2);
    t366 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_a2, v_y);
  }
  if (!t366) {
    u_no_case(s365);
  }
  return t366;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s367 = v_rest;
    int d368 = 0;
    if ((!d368) && u_is_con(s367, &n_list_46_Nil)) {
      d368 = 1;
      {
      }
    }
    if ((!d368) && (u_is_con(s367, &n_list_46_Cons) && u_is_con(u_field(s367, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s367, 0), 0);
      u_value v_i = u_field(u_field(s367, 0), 1);
      d368 = 1;
      {
        static struct u_object l_unexpected_32_opera_369 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_369, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d368) {
      d368 = 1;
      {
        static struct u_object l_missing_32_operator_370 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_370, v_at), "parse.ParseError");
      }
    }
    if (!d368) {
      u_no_case(s367);
    }
  }
}

void p_parse_46_climb(u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_lhs = NULL;
    u_value v_more = NULL;
    p_parse_46_operand(v_items, v_o, v_at, &v_lhs, &v_more);
    p_parse_46_climb_loop(v_lhs, v_more, v_min, v_o, v_at, r_a, r_rest);
  }
}

void p_parse_46_operand(u_value v_items, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_b = NULL;
    u_value s371 = v_items;
    int d372 = 0;
    if ((!d372) && (u_is_con(s371, &n_list_46_Cons) && u_is_con(u_field(s371, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s371, 0), 0);
      u_value v_more = u_field(s371, 1);
      d372 = 1;
      {
        u_value t373 = v_x;
        u_value t374 = v_more;
        *r_a = t373;
        *r_rest = t374;
      }
    }
    if ((!d372) && (u_is_con(s371, &n_list_46_Cons) && u_is_con(u_field(s371, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s371, 0), 0);
      u_value v_i = u_field(u_field(s371, 0), 1);
      u_value v_more = u_field(s371, 1);
      d372 = 1;
      {
        u_value s375 = f_parse_46_prefix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op);
        int d376 = 0;
        if ((!d376) && u_is_con(s375, &n_option_46_Some)) {
          u_value v_p = u_field(s375, 0);
          d376 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t377 = u_apply(f_parse_46_Ops_46_prefix_op_1(v_o), 2, v_op, v_b);
            *r_a = t377;
          }
        }
        if ((!d376) && u_is_con(s375, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op))) {
            d376 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_378 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_378), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d376) && u_is_con(s375, &n_option_46_None)) {
          d376 = 1;
          {
            static struct u_object l_undeclared_32_opera_379 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_379, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d376) {
          u_no_case(s375);
        }
      }
    }
    if ((!d372) && u_is_con(s371, &n_list_46_Nil)) {
      d372 = 1;
      {
        static struct u_object l_missing_32_operand_380 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_380, v_at), "parse.ParseError");
      }
    }
    if (!d372) {
      u_no_case(s371);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s381 = v_items;
    int d382 = 0;
    if ((!d382) && (u_is_con(s381, &n_list_46_Cons) && u_is_con(u_field(s381, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s381, 0), 0);
      u_value v_i = u_field(u_field(s381, 0), 1);
      u_value v_tail = u_field(s381, 1);
      d382 = 1;
      {
        u_value s383 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op), f_parse_46_infix_of_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op));
        int d384 = 0;
        if ((!d384) && u_is_con(u_field(s383, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s383, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d384 = 1;
            {
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_postfix_op_1(v_o), 2, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d384) && u_is_con(u_field(s383, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s383, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s383, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d384 = 1;
            {
              u_value t385;
              if (u_truth(u_eq(v_f, &o_syntax_46_FRight))) {
                t385 = v_p;
              } else {
                t385 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t385, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)));
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_infix_op_1(v_o), 3, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d384) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op)))) {
            d384 = 1;
            {
              static struct u_object l_undeclared_32_opera_386 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_386, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d384) {
          d384 = 1;
          {
            u_value t387 = v_lhs;
            u_value t388 = v_items;
            *r_a = t387;
            *r_rest = t388;
          }
        }
        if (!d384) {
          u_no_case(s383);
        }
      }
    }
    if (!d382) {
      d382 = 1;
      {
        u_value t389 = v_lhs;
        u_value t390 = v_items;
        *r_a = t389;
        *r_rest = t390;
      }
    }
    if (!d382) {
      u_no_case(s381);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s391 = v_rest;
    int d392 = 0;
    if ((!d392) && (u_is_con(s391, &n_list_46_Cons) && u_is_con(u_field(s391, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s391, 0), 0);
      u_value v_i = u_field(u_field(s391, 0), 1);
      u_value t395 = u_eq(v_f, &o_syntax_46_FInfix);
      if (u_truth(t395)) {
        t395 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t395)) {
        d392 = 1;
        {
          static struct u_object l_operator_32__393 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_394 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__393, u_concat(v_op, &l__32_is_32_not_32_associat_394)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d392) {
      d392 = 1;
      {
      }
    }
    if (!d392) {
      u_no_case(s391);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s396 = v_fp;
  u_value t397 = NULL;
  if ((!t397) && u_is_con(s396, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s396, 0), 1);
    t397 = u_eq(v_q, v_p);
  }
  if (!t397) {
    t397 = u_mk_bool(0);
  }
  if (!t397) {
    u_no_case(s396);
  }
  return t397;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s398 = v_ts;
  u_value t399 = NULL;
  if ((!t399) && (u_is_con(s398, &n_list_46_Cons) && (u_is_con(u_field(s398, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s398, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s398, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s398, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s398, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s398, 0), 1);
    t399 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t399) && (u_is_con(s398, &n_list_46_Cons) && u_is_con(u_field(s398, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s398, 0);
    t399 = v_t;
  }
  if ((!t399) && u_is_con(s398, &n_list_46_Nil)) {
    t399 = u_mk_con(&n_syntax_46_TTuple, 1, &o_list_46_Nil);
  }
  if (!t399) {
    static struct u_object l__40_invalid_41__400 = U_LIT("(invalid)");
    t399 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__400), v_ts);
  }
  if (!t399) {
    u_no_case(s398);
  }
  return t399;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s401 = v_ts;
  u_value t402 = NULL;
  if ((!t402) && (u_is_con(s401, &n_list_46_Cons) && (u_is_con(u_field(s401, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s401, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s401, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s401, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s401, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s401, 0), 1);
    t402 = v_cs;
  }
  if (!t402) {
    t402 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), &o_list_46_Nil);
  }
  if (!t402) {
    u_no_case(s401);
  }
  return t402;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), &o_list_46_Nil)), &o_list_46_Nil);
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__403 = U_LIT("->");
  u_value t410;
  if (u_truth(u_eq(v_op, &l__45__62__403))) {
    u_value t404 = &o_list_46_Nil;
    u_value *e405 = &t404;
    u_value l406 = v_l;
    while (u_is_con(l406, &n_list_46_Cons)) {
      u_value v_t = u_field(l406, 0);
      if (u_truth(u_mk_bool(1))) {
        e405 = u_append(e405, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, &o_list_46_Nil)));
      }
      l406 = u_field(l406, 1);
    }
    t410 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t404, f_parse_46_type_one_1(v_r)), &o_list_46_Nil);
  } else {
    static struct u_object l__42__407 = U_LIT("*");
    u_value t409;
    if (u_truth(u_eq(v_op, &l__42__407))) {
      static struct u_object l__40_product_41__408 = U_LIT("(product)");
      t409 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__408), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), &o_list_46_Nil);
    } else {
      t409 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), &o_list_46_Nil))), &o_list_46_Nil);
    }
    t410 = t409;
  }
  return t410;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s411 = v_t;
  u_value t412 = NULL;
  if ((!t412) && (u_is_con(s411, &n_syntax_46_TSort) && (u_is_con(u_field(s411, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s411, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s411, 0), 1), u_mk_lit("(invalid)")))))) {
    t412 = u_mk_bool(1);
  }
  if ((!t412) && u_is_con(s411, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s411, 1);
    t412 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t412) && u_is_con(s411, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s411, 0);
    u_value v_r = u_field(s411, 1);
    u_value t413 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t413)) {
      t413 = f_parse_46_has_invalid_1(v_r);
    }
    t412 = t413;
  }
  if ((!t412) && u_is_con(s411, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s411, 0);
    t412 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t412) {
    t412 = u_mk_bool(0);
  }
  if (!t412) {
    u_no_case(s411);
  }
  return t412;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s414 = v_ts;
  u_value t415 = NULL;
  if ((!t415) && u_is_con(s414, &n_list_46_Nil)) {
    t415 = u_mk_bool(0);
  }
  if ((!t415) && u_is_con(s414, &n_list_46_Cons)) {
    u_value v_t = u_field(s414, 0);
    u_value v_more = u_field(s414, 1);
    u_value t416 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t416)) {
      t416 = f_parse_46_any_invalid_1(v_more);
    }
    t415 = t416;
  }
  if (!t415) {
    u_no_case(s414);
  }
  return t415;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s417 = v_t;
  u_value t418 = NULL;
  if ((!t418) && u_is_con(s417, &n_lex_46_TName)) {
    u_value v_n = u_field(s417, 0);
    u_value t419 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t419)) {
      t419 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t418 = t419;
  }
  if ((!t418) && (u_is_con(s417, &n_lex_46_TPunct) && u_equals(u_field(s417, 0), u_mk_char(40)))) {
    t418 = u_mk_bool(1);
  }
  if (!t418) {
    t418 = u_mk_bool(0);
  }
  if (!t418) {
    u_no_case(s417);
  }
  return t418;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t420 = f_parse_46_head_offset_1(*r_ts);
    v_start = t420;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t421 = f_parse_46_type_one_1(v_r);
    *r_t = t421;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__422 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__422, v_start), "parse.ParseError");
      }
    }
  }
}

void p_parse_46_type_items(u_value v_sc, u_value *r_items, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_next = NULL;
    u_value v_go = NULL;
    u_value v_b = NULL;
    u_value t423 = &o_list_46_Nil;
    v_rev = t423;
    u_value t424 = u_mk_bool(1);
    v_next = t424;
    u_value t425 = u_mk_bool(1);
    v_go = t425;
    u_value t434 = v_go;
    while (u_truth(t434)) {
      {
        u_value s426 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d427 = 0;
        if ((!d427) && u_is_con(s426, &n_option_46_Some)) {
          u_value v_op = u_field(s426, 0);
          d427 = 1;
          {
            u_value t428 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t429 = u_mk_bool(1);
            v_rev = t428;
            v_next = t429;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d427) && u_is_con(s426, &n_option_46_None)) {
          u_value t432 = v_next;
          if (u_truth(t432)) {
            t432 = f_parse_46_starts_type_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t432)) {
            d427 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t430 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t431 = u_mk_bool(0);
              v_rev = t430;
              v_next = t431;
            }
          }
        }
        if (!d427) {
          d427 = 1;
          {
            u_value t433 = u_mk_bool(0);
            v_go = t433;
          }
        }
        if (!d427) {
          u_no_case(s426);
        }
      }
      t434 = v_go;
    }
    u_value s435 = v_rev;
    int d436 = 0;
    if ((!d436) && u_is_con(s435, &n_list_46_Nil)) {
      d436 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_437 = U_LIT("type expected but ");
        static struct u_object l__32_found_438 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_437, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_438)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d436) {
      d436 = 1;
      {
        u_value t439 = f_list_46_reverse_1(v_rev);
        *r_items = t439;
      }
    }
    if (!d436) {
      u_no_case(s435);
    }
  }
}

void p_parse_46_type_basic(u_value v_sc, u_value *r_r, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_args = NULL;
    u_value v_ins = NULL;
    u_value v_outs = NULL;
    u_value v_refs = NULL;
    u_value t443 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (u_truth(t443)) {
      static struct u_object l_in_440 = U_LIT("in");
      static struct u_object l_out_441 = U_LIT("out");
      static struct u_object l_ref_442 = U_LIT("ref");
      t443 = f_list_46_contains_2(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_in_440), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_out_441), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_ref_442), &o_list_46_Nil))));
    }
    if (u_truth(t443)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_sections(u_mk_proc(wp_parse_46_parse_type, 0), u_mk_proc(wp_parse_46_parse_type, 0), v_sc, &v_ins, &v_outs, &v_refs, r_ts);
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
        u_value t444 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TProc, 3, v_ins, v_outs, v_refs), &o_list_46_Nil);
        *r_r = t444;
      }
    } else {
      if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_type_list(v_sc, u_mk_char(41), r_r, r_ts);
        }
      } else {
        {
          p_parse_46_parse_plain(v_sc, &v_n, r_ts);
          if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
            {
              p_parse_46_advance(r_ts);
              p_parse_46_type_list(v_sc, u_mk_char(93), &v_args, r_ts);
              u_value t445 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), &o_list_46_Nil);
              *r_r = t445;
            }
          } else {
            if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Scope_46_params_1(v_sc)))) {
              {
                u_value t446 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), &o_list_46_Nil);
                *r_r = t446;
              }
            } else {
              {
                u_value t447 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), &o_list_46_Nil), &o_list_46_Nil);
                *r_r = t447;
              }
            }
          }
        }
      }
    }
  }
}

void p_parse_46_type_list(u_value v_sc, u_value v_close, u_value *r_r, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_t = NULL;
    u_value t448 = &o_list_46_Nil;
    v_rev = t448;
    u_value t449 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t449)) {
      t449 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t449)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t450 = u_mk_con(&n_list_46_Cons, 2, v_t, &o_list_46_Nil);
        v_rev = t450;
        u_value t452 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t452)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t451 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t451;
          }
          t452 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t453 = f_list_46_reverse_1(v_rev);
    *r_r = t453;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), &o_syntax_46_THole)), u_mk_con(&n_list_46_Cons, 2, v_a, &o_list_46_Nil));
}

u_value f_parse_46_expr_prefix_2(u_value v_op, u_value v_a) {
  u_value s454 = v_a;
  u_value t455 = NULL;
  if ((!t455) && (u_is_con(s454, &n_syntax_46_ELit) && u_is_con(u_field(s454, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s454, 0), 0);
    static struct u_object l__45__456 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__456))) {
      t455 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t455) {
    t455 = f_parse_46_expr_op1_2(v_op, v_a);
  }
  if (!t455) {
    u_no_case(s454);
  }
  return t455;
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), &o_syntax_46_THole)), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, &o_list_46_Nil)));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s457 = v_es;
  u_value t458 = NULL;
  if ((!t458) && u_is_con(s457, &n_list_46_Nil)) {
    t458 = v_rest;
  }
  if ((!t458) && u_is_con(s457, &n_list_46_Cons)) {
    u_value v_e = u_field(s457, 0);
    u_value v_more = u_field(s457, 1);
    static struct u_object l_Cons_459 = U_LIT("Cons");
    t458 = f_parse_46_expr_op2_3(&l_Cons_459, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t458) {
    u_no_case(s457);
  }
  return t458;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s460 = v_t;
  u_value t461 = NULL;
  if ((!t461) && u_is_con(s460, &n_lex_46_TName)) {
    u_value v_n = u_field(s460, 0);
    static struct u_object l_let_462 = U_LIT("let");
    static struct u_object l_if_463 = U_LIT("if");
    static struct u_object l_lambda_464 = U_LIT("lambda");
    static struct u_object l_match_465 = U_LIT("match");
    static struct u_object l_true_466 = U_LIT("true");
    static struct u_object l_false_467 = U_LIT("false");
    u_value t469 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_462, u_mk_con(&n_list_46_Cons, 2, &l_if_463, u_mk_con(&n_list_46_Cons, 2, &l_lambda_464, u_mk_con(&n_list_46_Cons, 2, &l_match_465, u_mk_con(&n_list_46_Cons, 2, &l_true_466, u_mk_con(&n_list_46_Cons, 2, &l_false_467, &o_list_46_Nil)))))));
    if (!u_truth(t469)) {
      u_value t468 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t468)) {
        t468 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t469 = t468;
    }
    t461 = t469;
  }
  if ((!t461) && u_is_con(s460, &n_lex_46_TNum)) {
    t461 = u_mk_bool(1);
  }
  if ((!t461) && u_is_con(s460, &n_lex_46_TStr)) {
    t461 = u_mk_bool(1);
  }
  if ((!t461) && u_is_con(s460, &n_lex_46_TChar)) {
    t461 = u_mk_bool(1);
  }
  if ((!t461) && u_is_con(s460, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s460, 0);
    u_value t470 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t470)) {
      t470 = u_eq(v_c, u_mk_char(91));
    }
    u_value t471 = t470;
    if (!u_truth(t471)) {
      t471 = u_eq(v_c, u_mk_char(123));
    }
    t461 = t471;
  }
  if (!t461) {
    t461 = u_mk_bool(0);
  }
  if (!t461) {
    u_no_case(s460);
  }
  return t461;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    static struct u_object l_raise_472 = U_LIT("raise");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_raise_472))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, r_e, r_ts);
        u_value t473 = u_mk_con(&n_syntax_46_ERaise, 2, *r_e, &o_syntax_46_THole);
        *r_e = t473;
      }
    } else {
      {
        p_parse_46_operators(v_sc, r_e, r_ts);
        static struct u_object l_with_474 = U_LIT("with");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_with_474))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_update(v_sc, r_e, r_ts);
            u_value t475 = f_parse_46_at_update_1(*r_ts);
            while (u_truth(t475)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_update(v_sc, r_e, r_ts);
              }
              t475 = f_parse_46_at_update_1(*r_ts);
            }
          }
        }
      }
    }
  }
}

void p_parse_46_parse_update(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_a = NULL;
    p_parse_46_parse_name(&v_f, r_ts);
    static struct u_object l__58__61__476 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__476), r_ts);
    p_parse_46_operators(v_sc, &v_a, r_ts);
    static struct u_object l__46__477 = U_LIT(".");
    static struct u_object l__58__61__478 = U_LIT(":=");
    u_value t479 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__477, u_concat(v_f, &l__58__61__478))), &o_syntax_46_THole)), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Cons, 2, v_a, &o_list_46_Nil)));
    *r_e = t479;
  }
}

u_value f_parse_46_at_update_1(u_value v_ts) {
  u_value s480 = v_ts;
  u_value t481 = NULL;
  if ((!t481) && (u_is_con(s480, &n_list_46_Cons) && (u_is_con(u_field(u_field(s480, 0), 0), &n_lex_46_TPunct) && (u_equals(u_field(u_field(u_field(s480, 0), 0), 0), u_mk_char(44)) && (u_is_con(u_field(s480, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s480, 1), 0), 0), &n_lex_46_TName) && (u_is_con(u_field(u_field(s480, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(u_field(s480, 1), 1), 0), 0), &n_lex_46_TSym) && u_equals(u_field(u_field(u_field(u_field(u_field(s480, 1), 1), 0), 0), 0), u_mk_lit(":=")))))))))) {
    t481 = u_mk_bool(1);
  }
  if (!t481) {
    t481 = u_mk_bool(0);
  }
  if (!t481) {
    u_no_case(s480);
  }
  return t481;
}

void p_parse_46_operators(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_items = NULL;
    p_parse_46_expr_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_expr_prefix_2, 0), u_mk_closure(w_f_parse_46_expr_op1_2, 0), u_mk_closure(w_f_parse_46_expr_op2_3, 0)), f_parse_46_head_offset_1(*r_ts), r_e);
  }
}

void p_parse_46_expr_items(u_value v_sc, u_value *r_items, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_next = NULL;
    u_value v_go = NULL;
    u_value v_e = NULL;
    u_value t482 = &o_list_46_Nil;
    v_rev = t482;
    u_value t483 = u_mk_bool(1);
    v_next = t483;
    u_value t484 = u_mk_bool(1);
    v_go = t484;
    u_value t493 = v_go;
    while (u_truth(t493)) {
      {
        u_value s485 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d486 = 0;
        if ((!d486) && u_is_con(s485, &n_option_46_Some)) {
          u_value v_op = u_field(s485, 0);
          d486 = 1;
          {
            u_value t487 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t488 = u_mk_bool(1);
            v_rev = t487;
            v_next = t488;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d486) && u_is_con(s485, &n_option_46_None)) {
          u_value t491 = v_next;
          if (u_truth(t491)) {
            t491 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t491)) {
            d486 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t489 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t490 = u_mk_bool(0);
              v_rev = t489;
              v_next = t490;
            }
          }
        }
        if (!d486) {
          d486 = 1;
          {
            u_value t492 = u_mk_bool(0);
            v_go = t492;
          }
        }
        if (!d486) {
          u_no_case(s485);
        }
      }
      t493 = v_go;
    }
    u_value s494 = v_rev;
    int d495 = 0;
    if ((!d495) && u_is_con(s494, &n_list_46_Nil)) {
      d495 = 1;
      {
        static struct u_object l_expression_32_expec_496 = U_LIT("expression expected but ");
        static struct u_object l__32_found_497 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_496, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_497)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d495) {
      d495 = 1;
      {
        u_value t498 = f_list_46_reverse_1(v_rev);
        *r_items = t498;
      }
    }
    if (!d495) {
      u_no_case(s494);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t499 = u_mk_con(&n_list_46_Cons, 2, v_e, &o_list_46_Nil);
    v_rev = t499;
    u_value t501 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t501)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t500 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t500;
      }
      t501 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t502 = f_list_46_reverse_1(v_rev);
    *r_es = t502;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t503 = &o_list_46_Nil;
        *r_es = t503;
      }
    } else {
      {
        p_parse_46_parse_exprs(v_sc, r_es, r_ts);
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
  }
}

void p_parse_46_parse_app(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_args = NULL;
    u_value v_f = NULL;
    p_parse_46_parse_atom(v_sc, r_e, r_ts);
    u_value t504 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t504)) {
      t504 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t508 = t504;
    while (u_truth(t508)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t505 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t505;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__506 = U_LIT(".");
            u_value t507 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__506, v_f)), &o_syntax_46_THole)), u_mk_con(&n_list_46_Cons, 2, *r_e, &o_list_46_Nil));
            *r_e = t507;
          }
        }
      }
      u_value t509 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t509)) {
        t509 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t508 = t509;
    }
  }
}

void p_parse_46_parse_atom(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_a = NULL;
    u_value v_l = NULL;
    u_value v_r = NULL;
    u_value v_cs = NULL;
    u_value v_es = NULL;
    u_value s510 = f_parse_46_head_token_1(*r_ts);
    int d511 = 0;
    if ((!d511) && (u_is_con(s510, &n_lex_46_TName) && u_equals(u_field(s510, 0), u_mk_lit("let")))) {
      d511 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d511) && (u_is_con(s510, &n_lex_46_TName) && u_equals(u_field(s510, 0), u_mk_lit("if")))) {
      d511 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_512 = U_LIT("then");
        p_parse_46_keyword(&l_then_512, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_513 = U_LIT("else");
        p_parse_46_keyword(&l_else_513, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t514 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t514;
      }
    }
    if ((!d511) && (u_is_con(s510, &n_lex_46_TName) && u_equals(u_field(s510, 0), u_mk_lit("lambda")))) {
      d511 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d511) && (u_is_con(s510, &n_lex_46_TName) && u_equals(u_field(s510, 0), u_mk_lit("match")))) {
      d511 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t515 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t515;
      }
    }
    if ((!d511) && (u_is_con(s510, &n_lex_46_TName) && u_equals(u_field(s510, 0), u_mk_lit("true")))) {
      d511 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t516 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t516;
      }
    }
    if ((!d511) && (u_is_con(s510, &n_lex_46_TName) && u_equals(u_field(s510, 0), u_mk_lit("false")))) {
      d511 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t517 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t517;
      }
    }
    if ((!d511) && u_is_con(s510, &n_lex_46_TNum)) {
      u_value v_n = u_field(s510, 0);
      d511 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t518 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t518;
      }
    }
    if ((!d511) && u_is_con(s510, &n_lex_46_TStr)) {
      u_value v_s = u_field(s510, 0);
      d511 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t519 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t519;
      }
    }
    if ((!d511) && u_is_con(s510, &n_lex_46_TChar)) {
      u_value v_c = u_field(s510, 0);
      d511 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t520 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t520;
      }
    }
    if ((!d511) && (u_is_con(s510, &n_lex_46_TPunct) && u_equals(u_field(s510, 0), u_mk_char(40)))) {
      d511 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s521 = v_es;
        int d522 = 0;
        if ((!d522) && (u_is_con(s521, &n_list_46_Cons) && u_is_con(u_field(s521, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s521, 0);
          d522 = 1;
          {
            u_value t523 = v_x;
            *r_e = t523;
          }
        }
        if (!d522) {
          d522 = 1;
          {
            u_value t524 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t524;
          }
        }
        if (!d522) {
          u_no_case(s521);
        }
      }
    }
    if ((!d511) && (u_is_con(s510, &n_lex_46_TPunct) && u_equals(u_field(s510, 0), u_mk_char(91)))) {
      d511 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d511) && (u_is_con(s510, &n_lex_46_TPunct) && u_equals(u_field(s510, 0), u_mk_char(123)))) {
      d511 = 1;
      {
        p_parse_46_parse_braces(v_sc, r_e, r_ts);
      }
    }
    if ((!d511) && u_is_con(s510, &n_lex_46_TName)) {
      u_value v_n = u_field(s510, 0);
      u_value t526 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t526)) {
        t526 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t526)) {
        d511 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t525 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), &o_syntax_46_THole));
          *r_e = t525;
        }
      }
    }
    if (!d511) {
      d511 = 1;
      {
        static struct u_object l_expression_32_expec_527 = U_LIT("expression expected but ");
        static struct u_object l__32_found_528 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_527, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_528)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d511) {
      u_no_case(s510);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s529 = v_ps;
  u_value t530 = NULL;
  if ((!t530) && u_is_con(s529, &n_list_46_Nil)) {
    t530 = u_mk_con(&n_option_46_Some, 1, &o_list_46_Nil);
  }
  if ((!t530) && (u_is_con(s529, &n_list_46_Cons) && u_is_con(u_field(s529, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s529, 0), 0);
    u_value v_more = u_field(s529, 1);
    u_value s531 = f_parse_46_pattern_vars_1(v_more);
    u_value t532 = NULL;
    if ((!t532) && u_is_con(s531, &n_option_46_Some)) {
      u_value v_xs = u_field(s531, 0);
      t532 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t532) && u_is_con(s531, &n_option_46_None)) {
      t532 = &o_option_46_None;
    }
    if (!t532) {
      u_no_case(s531);
    }
    t530 = t532;
  }
  if (!t530) {
    t530 = &o_option_46_None;
  }
  if (!t530) {
    u_no_case(s529);
  }
  return t530;
}

void p_parse_46_parse_let(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_p = NULL;
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value v_ps = NULL;
    u_value v_es = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
    static struct u_object l__58__61__533 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__533), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t534 = u_mk_con(&n_list_46_Cons, 2, v_p, &o_list_46_Nil);
    u_value t535 = u_mk_con(&n_list_46_Cons, 2, v_a, &o_list_46_Nil);
    v_ps = t534;
    v_es = t535;
    u_value t539 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t539)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__536 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__536), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t537 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t538 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t537;
        v_es = t538;
      }
      t539 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_540 = U_LIT("in");
    p_parse_46_keyword(&l_in_540, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t541 = f_list_46_reverse_1(v_ps);
    u_value t542 = f_list_46_reverse_1(v_es);
    v_ps = t541;
    v_es = t542;
    u_value s543 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d544 = 0;
    if ((!d544) && u_is_con(u_field(s543, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s543, 0), 0);
      d544 = 1;
      {
        u_value t545 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t545;
      }
    }
    if ((!d544) && (u_is_con(u_field(s543, 0), &n_option_46_None) && (u_is_con(u_field(s543, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s543, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s543, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s543, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s543, 1), 0);
      u_value v_c = u_field(u_field(s543, 2), 0);
      d544 = 1;
      {
        u_value t546 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, &o_option_46_None, v_b), &o_list_46_Nil));
        *r_e = t546;
      }
    }
    if (!d544) {
      d544 = 1;
      {
        u_value t547 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), &o_option_46_None, v_b), &o_list_46_Nil));
        *r_e = t547;
      }
    }
    if (!d544) {
      u_no_case(s543);
    }
  }
}

void p_parse_46_parse_lambda(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_ins = NULL;
    u_value v_outs = NULL;
    u_value v_refs = NULL;
    u_value v_body = NULL;
    p_parse_46_advance(r_ts);
    if (u_truth(f_parse_46_at_section_1(*r_ts))) {
      {
        p_parse_46_sections(u_mk_proc(wp_parse_46_parse_formal, 0), u_mk_proc(wp_parse_46_parse_formal, 0), v_sc, &v_ins, &v_outs, &v_refs, r_ts);
        static struct u_object l__61__62__548 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__548), r_ts);
        p_parse_46_parse_body(v_sc, &v_body, r_ts);
        static struct u_object l_end_549 = U_LIT("end");
        p_parse_46_keyword(&l_end_549, r_ts);
        u_value t550 = u_mk_con(&n_syntax_46_EProc, 4, v_ins, v_outs, v_refs, v_body);
        *r_e = t550;
      }
    } else {
      {
        p_parse_46_lambda_function(v_sc, r_e, r_ts);
      }
    }
  }
}

u_value f_parse_46_at_section_1(u_value v_ts) {
  static struct u_object l_in_551 = U_LIT("in");
  u_value t553 = f_parse_46_at_keyword_2(v_ts, &l_in_551);
  if (!u_truth(t553)) {
    static struct u_object l_out_552 = U_LIT("out");
    t553 = f_parse_46_at_keyword_2(v_ts, &l_out_552);
  }
  u_value t555 = t553;
  if (!u_truth(t555)) {
    static struct u_object l_ref_554 = U_LIT("ref");
    t555 = f_parse_46_at_keyword_2(v_ts, &l_ref_554);
  }
  return t555;
}

void p_parse_46_lambda_function(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    u_value t556 = &o_list_46_Nil;
    v_xs = t556;
    static struct u_object l__61__62__557 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__557)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t558 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), &o_syntax_46_THole), &o_list_46_Nil);
        v_xs = t558;
        u_value t560 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t560)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t559 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), &o_syntax_46_THole), v_xs);
            v_xs = t559;
          }
          t560 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__561 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__561), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t562 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t562;
  }
}

void p_parse_46_parse_braces(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_at = NULL;
    u_value v_k = NULL;
    u_value v_v = NULL;
    u_value v_entries = NULL;
    u_value v_rev = NULL;
    u_value t563 = f_parse_46_head_offset_1(*r_ts);
    v_at = t563;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_expr(v_sc, &v_k, r_ts);
    static struct u_object l__61__62__564 = U_LIT("=>");
    u_value t565 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__564));
    v_entries = t565;
    if (u_truth(v_entries)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_v, r_ts);
      }
    }
    static struct u_object l_for_566 = U_LIT("for");
    u_value t567 = f_parse_46_at_keyword_2(*r_ts, &l_for_566);
    if (u_truth(t567)) {
      t567 = v_entries;
    }
    if (u_truth(t567)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_comprehension(v_sc, v_k, v_v, r_e, r_ts);
      }
    } else {
      static struct u_object l_for_568 = U_LIT("for");
      if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_568))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_set_comprehension(v_sc, v_k, r_e, r_ts);
        }
      } else {
        {
          u_value t569;
          if (u_truth(v_entries)) {
            t569 = u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, &o_list_46_Nil)));
          } else {
            t569 = v_k;
          }
          u_value t570 = u_mk_con(&n_list_46_Cons, 2, t569, &o_list_46_Nil);
          v_rev = t570;
          u_value t575 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          while (u_truth(t575)) {
            {
              p_parse_46_advance(r_ts);
              p_parse_46_parse_expr(v_sc, &v_k, r_ts);
              static struct u_object l__61__62__571 = U_LIT("=>");
              if (u_truth(f_prelude_46__33__61__2(v_entries, u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__571))))) {
                {
                  static struct u_object l_a_32_literal_32__123__46__46__46__125__32__572 = U_LIT("a literal {...} has either elements a or entries k => v");
                  u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_a_32_literal_32__123__46__46__46__125__32__572, v_at), "parse.ParseError");
                }
              }
              if (u_truth(v_entries)) {
                {
                  p_parse_46_advance(r_ts);
                  p_parse_46_parse_expr(v_sc, &v_v, r_ts);
                  u_value t573 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, &o_list_46_Nil))), v_rev);
                  v_rev = t573;
                }
              } else {
                {
                  u_value t574 = u_mk_con(&n_list_46_Cons, 2, v_k, v_rev);
                  v_rev = t574;
                }
              }
            }
            t575 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          }
          u_value t582;
          if (u_truth(v_entries)) {
            static struct u_object l_map_576 = U_LIT("map");
            static struct u_object l_put_all_577 = U_LIT("put_all");
            static struct u_object l_map_578 = U_LIT("map");
            static struct u_object l_empty_579 = U_LIT("empty");
            t582 = f_parse_46_in_library_3(&l_map_576, &l_put_all_577, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_578, &l_empty_579, &o_list_46_Nil), &o_list_46_Nil)));
          } else {
            static struct u_object l_set_580 = U_LIT("set");
            static struct u_object l_from_list_581 = U_LIT("from_list");
            t582 = f_parse_46_in_library_3(&l_set_580, &l_from_list_581, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), &o_list_46_Nil));
          }
          u_value t583 = t582;
          *r_e = t583;
        }
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(125)), r_ts);
  }
}

u_value f_parse_46_nil_expr_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of parse.nil_expr");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_Nil_584 = U_LIT("Nil");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_584), &o_syntax_46_THole));
  return cache;
}

u_value f_parse_46_in_library_3(u_value v_m, u_value v_f, u_value v_args) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), v_f), &o_syntax_46_THole)), v_args);
}

void p_parse_46_set_comprehension(u_value v_sc, u_value v_r, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_l = NULL;
    u_value v_xs = NULL;
    u_value v_ls = NULL;
    u_value v_c = NULL;
    u_value t585 = &o_list_46_Nil;
    v_xs = t585;
    u_value t586 = &o_list_46_Nil;
    v_ls = t586;
    u_value t587 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t587;
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l_in_588 = U_LIT("in");
    p_parse_46_keyword(&l_in_588, r_ts);
    p_parse_46_parse_expr(v_sc, &v_l, r_ts);
    static struct u_object l_set_589 = U_LIT("set");
    static struct u_object l_elements_590 = U_LIT("elements");
    u_value t591 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), &o_syntax_46_THole), &o_list_46_Nil);
    u_value t592 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_589, &l_elements_590, u_mk_con(&n_list_46_Cons, 2, v_l, &o_list_46_Nil)), &o_list_46_Nil);
    v_xs = t591;
    v_ls = t592;
    u_value t598 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t598)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l_in_593 = U_LIT("in");
        p_parse_46_keyword(&l_in_593, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_set_594 = U_LIT("set");
        static struct u_object l_elements_595 = U_LIT("elements");
        u_value t596 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), &o_syntax_46_THole), v_xs);
        u_value t597 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_594, &l_elements_595, u_mk_con(&n_list_46_Cons, 2, v_l, &o_list_46_Nil)), v_ls);
        v_xs = t596;
        v_ls = t597;
      }
      t598 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_599 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_599))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
      }
    }
    static struct u_object l_set_600 = U_LIT("set");
    static struct u_object l_from_list_601 = U_LIT("from_list");
    u_value t602 = f_parse_46_in_library_3(&l_set_600, &l_from_list_601, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, v_r, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_ls), v_c), &o_list_46_Nil));
    *r_e = t602;
  }
}

void p_parse_46_map_comprehension(u_value v_sc, u_value v_k, u_value v_v, u_value *r_e, u_value *r_ts) {
  {
    u_value v_gs = NULL;
    u_value v_ls = NULL;
    u_value v_c = NULL;
    u_value t603 = &o_list_46_Nil;
    v_gs = t603;
    u_value t604 = &o_list_46_Nil;
    v_ls = t604;
    u_value t605 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t605;
    p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
    u_value t606 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t606)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
      }
      t606 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_607 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_607))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
        u_value t608 = f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), v_c);
        v_c = t608;
      }
    }
    static struct u_object l_map_609 = U_LIT("map");
    static struct u_object l_put_all_610 = U_LIT("put_all");
    static struct u_object l_map_611 = U_LIT("map");
    static struct u_object l_empty_612 = U_LIT("empty");
    u_value t613 = f_parse_46_in_library_3(&l_map_609, &l_put_all_610, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, &o_list_46_Nil)))), f_list_46_reverse_1(f_parse_46_hidden_vars_1(v_gs)), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_611, &l_empty_612, &o_list_46_Nil), &o_list_46_Nil)));
    *r_e = t613;
  }
}

void p_parse_46_map_generator(u_value v_sc, u_value *r_gs, u_value *r_ls, u_value *r_ts) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value v_m = NULL;
    p_parse_46_parse_plain(v_sc, &v_a, r_ts);
    static struct u_object l__61__62__614 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__614), r_ts);
    p_parse_46_parse_plain(v_sc, &v_b, r_ts);
    static struct u_object l_in_615 = U_LIT("in");
    p_parse_46_keyword(&l_in_615, r_ts);
    p_parse_46_parse_expr(v_sc, &v_m, r_ts);
    static struct u_object l_map_616 = U_LIT("map");
    static struct u_object l_entries_617 = U_LIT("entries");
    u_value t618 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_616, &l_entries_617, u_mk_con(&n_list_46_Cons, 2, f_parse_46_unpack_2(f_list_46_reverse_1(*r_gs), v_m), &o_list_46_Nil)), *r_ls);
    *r_ls = t618;
    static struct u_object l__40_entry_32__619 = U_LIT("(entry ");
    static struct u_object l__41__620 = U_LIT(")");
    u_value t621 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(3, v_a, v_b, u_concat(&l__40_entry_32__619, u_concat(u_str_show(u_add(f_list_46_length_1(*r_gs), u_mk_int(1))), &l__41__620))), *r_gs);
    *r_gs = t621;
  }
}

u_value f_parse_46_hidden_vars_1(u_value v_gs) {
  u_value s622 = v_gs;
  u_value t623 = NULL;
  if ((!t623) && u_is_con(s622, &n_list_46_Nil)) {
    t623 = &o_list_46_Nil;
  }
  if ((!t623) && u_is_con(s622, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s622, 0), 2);
    u_value v_rest = u_field(s622, 1);
    t623 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), &o_syntax_46_THole), f_parse_46_hidden_vars_1(v_rest));
  }
  if (!t623) {
    u_no_case(s622);
  }
  return t623;
}

u_value f_parse_46_unpack_2(u_value v_gs, u_value v_e) {
  u_value s624 = v_gs;
  u_value t625 = NULL;
  if ((!t625) && u_is_con(s624, &n_list_46_Nil)) {
    t625 = v_e;
  }
  if ((!t625) && u_is_con(s624, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s624, 0), 0);
    u_value v_v = u_field(u_field(s624, 0), 1);
    u_value v_h = u_field(u_field(s624, 0), 2);
    u_value v_rest = u_field(s624, 1);
    t625 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), &o_syntax_46_THole)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_k), &o_syntax_46_THole)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_v), &o_syntax_46_THole)), &o_list_46_Nil))), &o_option_46_None, f_parse_46_unpack_2(v_rest, v_e)), &o_list_46_Nil));
  }
  if (!t625) {
    u_no_case(s624);
  }
  return t625;
}

void p_parse_46_parse_brackets(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_first = NULL;
    u_value v_es = NULL;
    u_value v_rest = NULL;
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_l = NULL;
    u_value v_lists = NULL;
    u_value v_c = NULL;
    p_parse_46_advance(r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
      {
        p_parse_46_advance(r_ts);
        static struct u_object l_Nil_626 = U_LIT("Nil");
        u_value t627 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_626), &o_syntax_46_THole));
        *r_e = t627;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_628 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_628))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_629 = U_LIT("in");
            p_parse_46_keyword(&l_in_629, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t630 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), &o_syntax_46_THole), &o_list_46_Nil);
            u_value t631 = u_mk_con(&n_list_46_Cons, 2, v_l, &o_list_46_Nil);
            v_xs = t630;
            v_lists = t631;
            u_value t635 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t635)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_632 = U_LIT("in");
                p_parse_46_keyword(&l_in_632, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t633 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), &o_syntax_46_THole), v_xs);
                u_value t634 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t633;
                v_lists = t634;
              }
              t635 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t636 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t636;
            static struct u_object l_if_637 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_637))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t638 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t638;
          }
        } else {
          {
            u_value t639 = u_mk_con(&n_list_46_Cons, 2, v_first, &o_list_46_Nil);
            v_es = t639;
            u_value t641 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t641)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t640 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t640;
              }
              t641 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_642 = U_LIT("Nil");
            u_value t643 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_642), &o_syntax_46_THole));
            v_rest = t643;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t644 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t644;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_645 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_645))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t646 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t646;
      }
    } else {
      {
        u_value t647 = &o_option_46_None;
        *r_g = t647;
      }
    }
  }
}

void p_parse_46_parse_expr_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t648 = &o_list_46_Nil;
    v_rev = t648;
    static struct u_object l_case_649 = U_LIT("case");
    u_value t652 = f_parse_46_at_keyword_2(*r_ts, &l_case_649);
    while (u_truth(t652)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__650 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__650), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t651 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t651;
      }
      static struct u_object l_case_653 = U_LIT("case");
      t652 = f_parse_46_at_keyword_2(*r_ts, &l_case_653);
    }
    static struct u_object l_end_654 = U_LIT("end");
    p_parse_46_keyword(&l_end_654, r_ts);
    u_value t655 = f_list_46_reverse_1(v_rev);
    *r_cs = t655;
  }
}

u_value f_parse_46_pat_op1_2(u_value v_op, u_value v_p) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_p, &o_list_46_Nil));
}

u_value f_parse_46_pat_prefix_2(u_value v_op, u_value v_p) {
  u_value s656 = v_p;
  u_value t657 = NULL;
  if ((!t657) && (u_is_con(s656, &n_syntax_46_PLit) && u_is_con(u_field(s656, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s656, 0), 0);
    static struct u_object l__45__658 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__658))) {
      t657 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t657) {
    t657 = f_parse_46_pat_op1_2(v_op, v_p);
  }
  if (!t657) {
    u_no_case(s656);
  }
  return t657;
}

u_value f_parse_46_pat_op2_3(u_value v_op, u_value v_l, u_value v_r) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, &o_list_46_Nil)));
}

u_value f_parse_46_cons_pats_2(u_value v_ps, u_value v_rest) {
  u_value s659 = v_ps;
  u_value t660 = NULL;
  if ((!t660) && u_is_con(s659, &n_list_46_Nil)) {
    t660 = v_rest;
  }
  if ((!t660) && u_is_con(s659, &n_list_46_Cons)) {
    u_value v_p = u_field(s659, 0);
    u_value v_more = u_field(s659, 1);
    static struct u_object l_Cons_661 = U_LIT("Cons");
    t660 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_661), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), &o_list_46_Nil)));
  }
  if (!t660) {
    u_no_case(s659);
  }
  return t660;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s662 = v_t;
  u_value t663 = NULL;
  if ((!t663) && u_is_con(s662, &n_lex_46_TName)) {
    u_value v_n = u_field(s662, 0);
    static struct u_object l___664 = U_LIT("_");
    static struct u_object l_true_665 = U_LIT("true");
    static struct u_object l_false_666 = U_LIT("false");
    u_value t668 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___664, u_mk_con(&n_list_46_Cons, 2, &l_true_665, u_mk_con(&n_list_46_Cons, 2, &l_false_666, &o_list_46_Nil))));
    if (!u_truth(t668)) {
      u_value t667 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t667)) {
        t667 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t668 = t667;
    }
    t663 = t668;
  }
  if ((!t663) && u_is_con(s662, &n_lex_46_TNum)) {
    t663 = u_mk_bool(1);
  }
  if ((!t663) && u_is_con(s662, &n_lex_46_TStr)) {
    t663 = u_mk_bool(1);
  }
  if ((!t663) && u_is_con(s662, &n_lex_46_TChar)) {
    t663 = u_mk_bool(1);
  }
  if ((!t663) && u_is_con(s662, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s662, 0);
    u_value t669 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t669)) {
      t669 = u_eq(v_c, u_mk_char(91));
    }
    t663 = t669;
  }
  if (!t663) {
    t663 = u_mk_bool(0);
  }
  if (!t663) {
    u_no_case(s662);
  }
  return t663;
}

void p_parse_46_parse_pattern(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_items = NULL;
    p_parse_46_pattern_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_pat_prefix_2, 0), u_mk_closure(w_f_parse_46_pat_op1_2, 0), u_mk_closure(w_f_parse_46_pat_op2_3, 0)), f_parse_46_head_offset_1(*r_ts), r_p);
  }
}

void p_parse_46_pattern_items(u_value v_sc, u_value *r_items, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_next = NULL;
    u_value v_go = NULL;
    u_value v_p = NULL;
    u_value t670 = &o_list_46_Nil;
    v_rev = t670;
    u_value t671 = u_mk_bool(1);
    v_next = t671;
    u_value t672 = u_mk_bool(1);
    v_go = t672;
    u_value t681 = v_go;
    while (u_truth(t681)) {
      {
        u_value s673 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d674 = 0;
        if ((!d674) && u_is_con(s673, &n_option_46_Some)) {
          u_value v_op = u_field(s673, 0);
          d674 = 1;
          {
            u_value t675 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t676 = u_mk_bool(1);
            v_rev = t675;
            v_next = t676;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d674) && u_is_con(s673, &n_option_46_None)) {
          u_value t679 = v_next;
          if (u_truth(t679)) {
            t679 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t679)) {
            d674 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t677 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t678 = u_mk_bool(0);
              v_rev = t677;
              v_next = t678;
            }
          }
        }
        if (!d674) {
          d674 = 1;
          {
            u_value t680 = u_mk_bool(0);
            v_go = t680;
          }
        }
        if (!d674) {
          u_no_case(s673);
        }
      }
      t681 = v_go;
    }
    u_value s682 = v_rev;
    int d683 = 0;
    if ((!d683) && u_is_con(s682, &n_list_46_Nil)) {
      d683 = 1;
      {
        static struct u_object l_pattern_32_expected_684 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_685 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_684, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_685)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d683) {
      d683 = 1;
      {
        u_value t686 = f_list_46_reverse_1(v_rev);
        *r_items = t686;
      }
    }
    if (!d683) {
      u_no_case(s682);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t687 = &o_list_46_Nil;
    v_rev = t687;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t688 = u_mk_con(&n_list_46_Cons, 2, v_p, &o_list_46_Nil);
        v_rev = t688;
        u_value t690 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t690)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t689 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t689;
          }
          t690 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t691 = f_list_46_reverse_1(v_rev);
    *r_ps = t691;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s692 = f_parse_46_head_token_1(*r_ts);
    int d693 = 0;
    if ((!d693) && (u_is_con(s692, &n_lex_46_TName) && u_equals(u_field(s692, 0), u_mk_lit("_")))) {
      d693 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t694 = &o_syntax_46_PWild;
        *r_p = t694;
      }
    }
    if ((!d693) && (u_is_con(s692, &n_lex_46_TName) && u_equals(u_field(s692, 0), u_mk_lit("true")))) {
      d693 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t695 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t695;
      }
    }
    if ((!d693) && (u_is_con(s692, &n_lex_46_TName) && u_equals(u_field(s692, 0), u_mk_lit("false")))) {
      d693 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t696 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t696;
      }
    }
    if ((!d693) && u_is_con(s692, &n_lex_46_TNum)) {
      u_value v_k = u_field(s692, 0);
      d693 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t697 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t697;
      }
    }
    if ((!d693) && u_is_con(s692, &n_lex_46_TStr)) {
      u_value v_s = u_field(s692, 0);
      d693 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t698 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t698;
      }
    }
    if ((!d693) && u_is_con(s692, &n_lex_46_TChar)) {
      u_value v_c = u_field(s692, 0);
      d693 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t699 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t699;
      }
    }
    if ((!d693) && (u_is_con(s692, &n_lex_46_TPunct) && u_equals(u_field(s692, 0), u_mk_char(40)))) {
      d693 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s700 = v_ps;
        int d701 = 0;
        if ((!d701) && (u_is_con(s700, &n_list_46_Cons) && u_is_con(u_field(s700, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s700, 0);
          d701 = 1;
          {
            u_value t702 = v_q;
            *r_p = t702;
          }
        }
        if (!d701) {
          d701 = 1;
          {
            u_value t703 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t703;
          }
        }
        if (!d701) {
          u_no_case(s700);
        }
      }
    }
    if ((!d693) && (u_is_con(s692, &n_lex_46_TPunct) && u_equals(u_field(s692, 0), u_mk_char(91)))) {
      d693 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_704 = U_LIT("Nil");
            u_value t705 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_704), &o_list_46_Nil);
            *r_p = t705;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t706 = u_mk_con(&n_list_46_Cons, 2, *r_p, &o_list_46_Nil);
            v_ps = t706;
            u_value t708 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t708)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t707 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t707;
              }
              t708 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_709 = U_LIT("Nil");
            u_value t710 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_709), &o_list_46_Nil);
            v_rest = t710;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t711 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t711;
          }
        }
      }
    }
    if (!d693) {
      d693 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t712 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t712;
          }
        } else {
          {
            u_value t713 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), &o_syntax_46_THole));
            *r_p = t713;
          }
        }
      }
    }
    if (!d693) {
      u_no_case(s692);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s714 = v_t;
  u_value t715 = NULL;
  if ((!t715) && u_is_con(s714, &n_lex_46_TName)) {
    u_value v_n = u_field(s714, 0);
    static struct u_object l_skip_716 = U_LIT("skip");
    static struct u_object l_if_717 = U_LIT("if");
    static struct u_object l_while_718 = U_LIT("while");
    static struct u_object l_call_719 = U_LIT("call");
    static struct u_object l_match_720 = U_LIT("match");
    static struct u_object l_assert_721 = U_LIT("assert");
    static struct u_object l_assign_722 = U_LIT("assign");
    static struct u_object l_raise_723 = U_LIT("raise");
    static struct u_object l_try_724 = U_LIT("try");
    t715 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_716, u_mk_con(&n_list_46_Cons, 2, &l_if_717, u_mk_con(&n_list_46_Cons, 2, &l_while_718, u_mk_con(&n_list_46_Cons, 2, &l_call_719, u_mk_con(&n_list_46_Cons, 2, &l_match_720, u_mk_con(&n_list_46_Cons, 2, &l_assert_721, u_mk_con(&n_list_46_Cons, 2, &l_assign_722, u_mk_con(&n_list_46_Cons, 2, &l_raise_723, u_mk_con(&n_list_46_Cons, 2, &l_try_724, &o_list_46_Nil))))))))));
  }
  if (!t715) {
    t715 = u_mk_bool(0);
  }
  if (!t715) {
    u_no_case(s714);
  }
  return t715;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s725 = v_cs;
  u_value t726 = NULL;
  if ((!t726) && u_is_con(s725, &n_list_46_Nil)) {
    t726 = &o_list_46_Nil;
  }
  if ((!t726) && (u_is_con(s725, &n_list_46_Cons) && u_is_con(u_field(s725, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s725, 0), 0);
    u_value v_g = u_field(u_field(s725, 0), 1);
    u_value v_b = u_field(u_field(s725, 0), 2);
    u_value v_more = u_field(s725, 1);
    t726 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, &o_syntax_46_THole, v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t726) {
    u_no_case(s725);
  }
  return t726;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t727 = &o_list_46_Nil;
    v_xs = t727;
    u_value t728 = &o_list_46_Nil;
    v_inits = t728;
    u_value t729 = &o_list_46_Nil;
    v_rev = t729;
    static struct u_object l_local_730 = U_LIT("local");
    u_value t732 = f_parse_46_at_keyword_2(*r_ts, &l_local_730);
    while (u_truth(t732)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t731 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t731)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t731 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_733 = U_LIT("local");
      t732 = f_parse_46_at_keyword_2(*r_ts, &l_local_733);
    }
    u_value t735 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t735)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t734 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t734;
      }
      t735 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t736 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t736;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t737 = &o_syntax_46_THole;
    v_t = t737;
    static struct u_object l__58__738 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__738)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__739 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__739)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t740 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), &o_syntax_46_THole), v_e), &o_list_46_Nil)), &o_list_46_Nil));
        *r_inits = t740;
      }
    }
    u_value t741 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), &o_list_46_Nil));
    *r_xs = t741;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s742 = f_parse_46_head_token_1(*r_ts);
    int d743 = 0;
    if ((!d743) && (u_is_con(s742, &n_lex_46_TName) && u_equals(u_field(s742, 0), u_mk_lit("skip")))) {
      d743 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t744 = &o_syntax_46_SSkip;
        *r_s = t744;
      }
    }
    if ((!d743) && (u_is_con(s742, &n_lex_46_TName) && u_equals(u_field(s742, 0), u_mk_lit("if")))) {
      d743 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d743) && (u_is_con(s742, &n_lex_46_TName) && u_equals(u_field(s742, 0), u_mk_lit("while")))) {
      d743 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_745 = U_LIT("end");
        p_parse_46_keyword(&l_end_745, r_ts);
        u_value t746 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t746;
      }
    }
    if ((!d743) && (u_is_con(s742, &n_lex_46_TName) && u_equals(u_field(s742, 0), u_mk_lit("call")))) {
      d743 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d743) && (u_is_con(s742, &n_lex_46_TName) && u_equals(u_field(s742, 0), u_mk_lit("match")))) {
      d743 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t747 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t747;
      }
    }
    if ((!d743) && (u_is_con(s742, &n_lex_46_TName) && u_equals(u_field(s742, 0), u_mk_lit("assert")))) {
      d743 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t748 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t748;
      }
    }
    if ((!d743) && (u_is_con(s742, &n_lex_46_TName) && u_equals(u_field(s742, 0), u_mk_lit("assign")))) {
      d743 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d743) && (u_is_con(s742, &n_lex_46_TName) && u_equals(u_field(s742, 0), u_mk_lit("raise")))) {
      d743 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t749 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, &o_syntax_46_THole);
        *r_s = t749;
      }
    }
    if ((!d743) && (u_is_con(s742, &n_lex_46_TName) && u_equals(u_field(s742, 0), u_mk_lit("try")))) {
      d743 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_750 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_750, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t751 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t751;
      }
    }
    if (!d743) {
      d743 = 1;
      {
        static struct u_object l_statement_32_expect_752 = U_LIT("statement expected but ");
        static struct u_object l__32_found_753 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_752, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_753)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d743) {
      u_no_case(s742);
    }
  }
}

void p_parse_46_parse_if(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_c = NULL;
    u_value v_l = NULL;
    u_value v_r = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_expr(v_sc, &v_c, r_ts);
    p_parse_46_parse_body(v_sc, &v_l, r_ts);
    static struct u_object l_else_754 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_754)))) {
      {
        static struct u_object l_end_755 = U_LIT("end");
        p_parse_46_keyword(&l_end_755, r_ts);
        u_value t756 = &o_syntax_46_SSkip;
        v_r = t756;
      }
    } else {
      static struct u_object l_if_757 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_757)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_758 = U_LIT("end");
          p_parse_46_keyword(&l_end_758, r_ts);
        }
      }
    }
    u_value t759 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t759;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__760 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__760), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t761 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), &o_syntax_46_THole), v_e), &o_list_46_Nil);
    v_rev = t761;
    u_value t764 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t764)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__762 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__762), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t763 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), &o_syntax_46_THole), v_e), v_rev);
        v_rev = t763;
      }
      t764 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t765 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t765;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t766 = &o_list_46_Nil;
    v_rev = t766;
    static struct u_object l_case_767 = U_LIT("case");
    u_value t770 = f_parse_46_at_keyword_2(*r_ts, &l_case_767);
    while (u_truth(t770)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__768 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__768), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t769 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t769;
      }
      static struct u_object l_case_771 = U_LIT("case");
      t770 = f_parse_46_at_keyword_2(*r_ts, &l_case_771);
    }
    static struct u_object l_end_772 = U_LIT("end");
    p_parse_46_keyword(&l_end_772, r_ts);
    u_value t773 = f_list_46_reverse_1(v_rev);
    *r_cs = t773;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t775;
  if (u_truth(v_first)) {
    t775 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t774 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t774)) {
      t774 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t775 = t774;
  }
  return t775;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t777 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t777)) {
    static struct u_object l_out_776 = U_LIT("out");
    t777 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_776));
  }
  u_value t779 = t777;
  if (u_truth(t779)) {
    static struct u_object l_ref_778 = U_LIT("ref");
    t779 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_778));
  }
  return t779;
}

void p_parse_46_parse_call(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_p = NULL;
    u_value v_ins = NULL;
    u_value v_outs = NULL;
    u_value v_refs = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_name(&v_p, r_ts);
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    p_parse_46_sections(u_mk_proc(wp_parse_46_parse_expr, 0), u_mk_proc(wp_parse_46_parse_variable, 0), v_sc, &v_ins, &v_outs, &v_refs, r_ts);
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    u_value t780 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), v_ins, v_outs, v_refs);
    *r_s = t780;
  }
}

void p_parse_46_sections(u_value v_item_in, u_value v_item, u_value v_sc, u_value *r_ins, u_value *r_outs, u_value *r_refs, u_value *r_ts) {
  {
    u_value v_first = NULL;
    u_value t781 = u_mk_bool(1);
    v_first = t781;
    u_value t782 = &o_list_46_Nil;
    u_value t783 = &o_list_46_Nil;
    u_value t784 = &o_list_46_Nil;
    *r_ins = t782;
    *r_outs = t783;
    *r_refs = t784;
    static struct u_object l_in_785 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_785))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_section_items(v_item_in, v_sc, r_ins, r_ts);
        u_value t786 = u_mk_bool(0);
        v_first = t786;
      }
    }
    static struct u_object l_out_787 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_787, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_section_items(v_item, v_sc, r_outs, r_ts);
        u_value t788 = u_mk_bool(0);
        v_first = t788;
      }
    }
    static struct u_object l_ref_789 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_789, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_section_items(v_item, v_sc, r_refs, r_ts);
      }
    }
  }
}

void p_parse_46_section_items(u_value v_item, u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_rev = NULL;
    u_call_proc(v_item, 1, 2, v_sc, &v_x, r_ts);
    u_value t790 = u_mk_con(&n_list_46_Cons, 2, v_x, &o_list_46_Nil);
    v_rev = t790;
    u_value t792 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t792)) {
      {
        p_parse_46_advance(r_ts);
        u_call_proc(v_item, 1, 2, v_sc, &v_x, r_ts);
        u_value t791 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t791;
      }
      t792 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t793 = f_list_46_reverse_1(v_rev);
    *r_xs = t793;
  }
}

void p_parse_46_skip_comma(u_value v_first, u_value *r_ts) {
  {
    if (u_truth(f_prelude_46_not_1(v_first))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_advance(r_ts);
  }
}

void p_parse_46_parse_variable(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t794 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), &o_syntax_46_THole);
    *r_x = t794;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t795 = &o_syntax_46_THole;
    v_t = t795;
    static struct u_object l__58__796 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__796)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t797 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t797;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t798 = &o_list_46_Nil;
        *r_xs = t798;
      }
    } else {
      {
        p_parse_46_section_items(u_mk_proc(wp_parse_46_parse_formal, 0), v_sc, r_xs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
  }
}

u_value f_parse_46_declarations_1(u_value v_cs) {
  u_value s799 = v_cs;
  u_value t800 = NULL;
  if ((!t800) && u_is_con(s799, &n_list_46_Nil)) {
    t800 = &o_list_46_Nil;
  }
  if ((!t800) && (u_is_con(s799, &n_list_46_Cons) && u_is_con(u_field(s799, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s799, 1);
    t800 = f_parse_46_declarations_1(v_more);
  }
  if ((!t800) && u_is_con(s799, &n_list_46_Cons)) {
    u_value v_c = u_field(s799, 0);
    u_value v_more = u_field(s799, 1);
    t800 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t800) {
    u_no_case(s799);
  }
  return t800;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t801 = &o_list_46_Nil;
  u_value *e802 = &t801;
  u_value l803 = v_cs;
  while (u_is_con(l803, &n_list_46_Cons)) {
    u_value v_c = u_field(l803, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e802 = u_append(e802, v_c);
    }
    l803 = u_field(l803, 1);
  }
  return t801;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s804 = v_c;
  u_value t805 = NULL;
  if ((!t805) && u_is_con(s804, &n_syntax_46_CNotation)) {
    t805 = u_mk_bool(1);
  }
  if (!t805) {
    t805 = u_mk_bool(0);
  }
  if (!t805) {
    u_no_case(s804);
  }
  return t805;
}

u_value f_parse_46_Context_46_notations_1(u_value v_x) {
  u_value s806 = v_x;
  u_value t807 = NULL;
  if ((!t807) && u_is_con(s806, &n_parse_46_Context)) {
    u_value v_y = u_field(s806, 0);
    t807 = v_y;
  }
  if (!t807) {
    u_no_case(s806);
  }
  return t807;
}

u_value f_parse_46_Context_46_modules_1(u_value v_x) {
  u_value s808 = v_x;
  u_value t809 = NULL;
  if ((!t809) && u_is_con(s808, &n_parse_46_Context)) {
    u_value v_y = u_field(s808, 1);
    t809 = v_y;
  }
  if (!t809) {
    u_no_case(s808);
  }
  return t809;
}

u_value f_parse_46_Context_46_loading_1(u_value v_x) {
  u_value s810 = v_x;
  u_value t811 = NULL;
  if ((!t811) && u_is_con(s810, &n_parse_46_Context)) {
    u_value v_y = u_field(s810, 2);
    t811 = v_y;
  }
  if (!t811) {
    u_no_case(s810);
  }
  return t811;
}

u_value f_parse_46_Context_46_base_1(u_value v_x) {
  u_value s812 = v_x;
  u_value t813 = NULL;
  if ((!t813) && u_is_con(s812, &n_parse_46_Context)) {
    u_value v_y = u_field(s812, 3);
    t813 = v_y;
  }
  if (!t813) {
    u_no_case(s812);
  }
  return t813;
}

u_value f_parse_46_Context_46_dir_1(u_value v_x) {
  u_value s814 = v_x;
  u_value t815 = NULL;
  if ((!t815) && u_is_con(s814, &n_parse_46_Context)) {
    u_value v_y = u_field(s814, 4);
    t815 = v_y;
  }
  if (!t815) {
    u_no_case(s814);
  }
  return t815;
}

u_value f_parse_46_Context_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s816 = v_x;
  u_value t817 = NULL;
  if ((!t817) && u_is_con(s816, &n_parse_46_Context)) {
    u_value v_a1 = u_field(s816, 1);
    u_value v_a2 = u_field(s816, 2);
    u_value v_a3 = u_field(s816, 3);
    u_value v_a4 = u_field(s816, 4);
    t817 = u_mk_con(&n_parse_46_Context, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t817) {
    u_no_case(s816);
  }
  return t817;
}

u_value f_parse_46_Context_46_modules_58__61__2(u_value v_x, u_value v_y) {
  u_value s818 = v_x;
  u_value t819 = NULL;
  if ((!t819) && u_is_con(s818, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s818, 0);
    u_value v_a2 = u_field(s818, 2);
    u_value v_a3 = u_field(s818, 3);
    u_value v_a4 = u_field(s818, 4);
    t819 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t819) {
    u_no_case(s818);
  }
  return t819;
}

u_value f_parse_46_Context_46_loading_58__61__2(u_value v_x, u_value v_y) {
  u_value s820 = v_x;
  u_value t821 = NULL;
  if ((!t821) && u_is_con(s820, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s820, 0);
    u_value v_a1 = u_field(s820, 1);
    u_value v_a3 = u_field(s820, 3);
    u_value v_a4 = u_field(s820, 4);
    t821 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t821) {
    u_no_case(s820);
  }
  return t821;
}

u_value f_parse_46_Context_46_base_58__61__2(u_value v_x, u_value v_y) {
  u_value s822 = v_x;
  u_value t823 = NULL;
  if ((!t823) && u_is_con(s822, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s822, 0);
    u_value v_a1 = u_field(s822, 1);
    u_value v_a2 = u_field(s822, 2);
    u_value v_a4 = u_field(s822, 4);
    t823 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t823) {
    u_no_case(s822);
  }
  return t823;
}

u_value f_parse_46_Context_46_dir_58__61__2(u_value v_x, u_value v_y) {
  u_value s824 = v_x;
  u_value t825 = NULL;
  if ((!t825) && u_is_con(s824, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s824, 0);
    u_value v_a1 = u_field(s824, 1);
    u_value v_a2 = u_field(s824, 2);
    u_value v_a3 = u_field(s824, 3);
    t825 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t825) {
    u_no_case(s824);
  }
  return t825;
}

u_value f_parse_46_with_commands_2(u_value v_cx, u_value v_cs) {
  return f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_with_notations_2(f_parse_46_Context_46_notations_1(v_cx), v_cs));
}

u_value f_parse_46_with_module_2(u_value v_cx, u_value v_c) {
  return f_parse_46_Context_46_modules_58__61__2(v_cx, f_list_46__43__43__2(f_parse_46_modules_of_1(v_c), f_parse_46_Context_46_modules_1(v_cx)));
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s826 = v_c;
  u_value t827 = NULL;
  if ((!t827) && u_is_con(s826, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s826, 0);
    u_value v_cs = u_field(s826, 1);
    u_value t828 = &o_list_46_Nil;
    u_value *e829 = &t828;
    u_value l830 = v_cs;
    while (u_is_con(l830, &n_list_46_Cons)) {
      u_value v_d = u_field(l830, 0);
      if (u_truth(u_mk_bool(1))) {
        e829 = u_append(e829, f_parse_46_modules_of_1(v_d));
      }
      l830 = u_field(l830, 1);
    }
    t827 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t828));
  }
  if (!t827) {
    t827 = &o_list_46_Nil;
  }
  if (!t827) {
    u_no_case(s826);
  }
  return t827;
}

u_value f_parse_46_own_notations_2(u_value v_cx, u_value v_n) {
  u_value s831 = f_list_46_lookup_2(v_n, f_parse_46_Context_46_modules_1(v_cx));
  u_value t832 = NULL;
  if ((!t832) && u_is_con(s831, &n_option_46_Some)) {
    u_value v_ns = u_field(s831, 0);
    t832 = v_ns;
  }
  if ((!t832) && u_is_con(s831, &n_option_46_None)) {
    t832 = &o_list_46_Nil;
  }
  if (!t832) {
    u_no_case(s831);
  }
  return t832;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t833 = &o_list_46_Nil;
    v_rev = t833;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t834 = u_mk_con(&n_list_46_Cons, 2, v_n, &o_list_46_Nil);
        v_rev = t834;
        u_value t836 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t836)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t835 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t835;
          }
          t836 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t837 = f_list_46_reverse_1(v_rev);
    *r_ps = t837;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t838 = &o_syntax_46_FInfix;
    v_f = t838;
    u_value s839 = f_parse_46_head_token_1(*r_ts);
    int d840 = 0;
    if ((!d840) && (u_is_con(s839, &n_lex_46_TName) && u_equals(u_field(s839, 0), u_mk_lit("prefix")))) {
      d840 = 1;
      {
        u_value t841 = &o_syntax_46_FPrefix;
        v_f = t841;
      }
    }
    if ((!d840) && (u_is_con(s839, &n_lex_46_TName) && u_equals(u_field(s839, 0), u_mk_lit("postfix")))) {
      d840 = 1;
      {
        u_value t842 = &o_syntax_46_FPostfix;
        v_f = t842;
      }
    }
    if (!d840) {
      d840 = 1;
      {
        u_value s843 = f_parse_46_second_token_1(*r_ts);
        int d844 = 0;
        if ((!d844) && (u_is_con(s843, &n_lex_46_TName) && u_equals(u_field(s843, 0), u_mk_lit("left")))) {
          d844 = 1;
          {
            u_value t845 = &o_syntax_46_FLeft;
            v_f = t845;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d844) && (u_is_con(s843, &n_lex_46_TName) && u_equals(u_field(s843, 0), u_mk_lit("right")))) {
          d844 = 1;
          {
            u_value t846 = &o_syntax_46_FRight;
            v_f = t846;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d844) {
          d844 = 1;
          {
          }
        }
        if (!d844) {
          u_no_case(s843);
        }
      }
    }
    if (!d840) {
      u_no_case(s839);
    }
    p_parse_46_advance(r_ts);
    u_value s847 = f_parse_46_head_token_1(*r_ts);
    int d848 = 0;
    if ((!d848) && u_is_con(s847, &n_lex_46_TNum)) {
      u_value v_n = u_field(s847, 0);
      d848 = 1;
      {
        u_value t849 = v_n;
        v_p = t849;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d848) {
      d848 = 1;
      {
        static struct u_object l_precedence_32_expec_850 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_851 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_850, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_851)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d848) {
      u_no_case(s847);
    }
    u_value t852 = &o_list_46_Nil;
    u_value t853 = u_mk_bool(1);
    v_ops = t852;
    v_go = t853;
    u_value t859 = v_go;
    while (u_truth(t859)) {
      {
        u_value s854 = f_parse_46_head_token_1(*r_ts);
        int d855 = 0;
        if ((!d855) && u_is_con(s854, &n_lex_46_TSym)) {
          u_value v_s = u_field(s854, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d855 = 1;
            {
              u_value t856 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t856;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d855) && u_is_con(s854, &n_lex_46_TName)) {
          u_value v_n = u_field(s854, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d855 = 1;
            {
              u_value t857 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t857;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d855) {
          d855 = 1;
          {
            u_value t858 = u_mk_bool(0);
            v_go = t858;
          }
        }
        if (!d855) {
          u_no_case(s854);
        }
      }
      t859 = v_go;
    }
    u_value s860 = v_ops;
    int d861 = 0;
    if ((!d861) && u_is_con(s860, &n_list_46_Nil)) {
      d861 = 1;
      {
        static struct u_object l_operator_32_expecte_862 = U_LIT("operator expected but ");
        static struct u_object l__32_found_863 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_862, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_863)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d861) {
      d861 = 1;
      {
        u_value t864 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t864;
      }
    }
    if (!d861) {
      u_no_case(s860);
    }
  }
}

void p_parse_46_constructor(u_value v_sc, u_value *r_k, u_value *r_fs, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_args = NULL;
    u_value v_t = NULL;
    u_value v_f = NULL;
    p_parse_46_parse_name_or_symbol(&v_n, r_ts);
    u_value t865 = &o_list_46_Nil;
    u_value t866 = &o_list_46_Nil;
    v_args = t865;
    *r_fs = t866;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t867 = u_mk_con(&n_list_46_Cons, 2, v_t, &o_list_46_Nil);
            u_value t868 = u_mk_con(&n_list_46_Cons, 2, v_f, &o_list_46_Nil);
            v_args = t867;
            *r_fs = t868;
            u_value t871 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t871)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t869 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t870 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t869;
                *r_fs = t870;
              }
              t871 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t872 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t873 = f_list_46_reverse_1(*r_fs);
    *r_k = t872;
    *r_fs = t873;
    u_value s874 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d875 = 0;
    if ((!d875) && u_is_con(s874, &n_option_46_Some)) {
      u_value v_f = u_field(s874, 0);
      d875 = 1;
      {
        static struct u_object l_repeated_32_field_32__876 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__877 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__876, u_concat(v_f, u_concat(&l__32_of_32__877, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d875) && u_is_con(s874, &n_option_46_None)) {
      d875 = 1;
      {
      }
    }
    if (!d875) {
      u_no_case(s874);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s878 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d879 = 0;
    if ((!d879) && (u_is_con(u_field(s878, 0), &n_lex_46_TName) && (u_is_con(u_field(s878, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s878, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s878, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d879 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t880 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t880;
        }
      }
    }
    if (!d879) {
      d879 = 1;
      {
        u_value t881 = &o_option_46_None;
        *r_f = t881;
      }
    }
    if (!d879) {
      u_no_case(s878);
    }
    p_parse_46_parse_type(v_sc, r_t, r_ts);
  }
}

void p_parse_46_parse_data(u_value v_ns, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_k = NULL;
    u_value v_fs = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_name(&v_n, r_ts);
    p_parse_46_type_params(&v_ps, r_ts);
    static struct u_object l__58__61__882 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__882), r_ts);
    static struct u_object l__124__883 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__883)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t884 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), &o_list_46_Nil);
    v_rev = t884;
    static struct u_object l__124__885 = U_LIT("|");
    u_value t887 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__885));
    while (u_truth(t887)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t886 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t886;
      }
      static struct u_object l__124__888 = U_LIT("|");
      t887 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__888));
    }
    u_value s889 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d890 = 0;
    if ((!d890) && u_is_con(s889, &n_option_46_Some)) {
      u_value v_f = u_field(s889, 0);
      d890 = 1;
      {
        static struct u_object l_field_32__891 = U_LIT("field ");
        static struct u_object l__32_of_32__892 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_893 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__891, u_concat(v_f, u_concat(&l__32_of_32__892, u_concat(v_n, &l__32_has_32_different_32_t_893)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d890) && u_is_con(s889, &n_option_46_None)) {
      d890 = 1;
      {
      }
    }
    if (!d890) {
      u_no_case(s889);
    }
    u_value t894 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_list_46__43__43__2(f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)), f_parse_46_updaters_3(v_n, v_ps, f_list_46_reverse_1(v_rev))));
    *r_cs = t894;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s895 = v_fs;
  u_value t896 = NULL;
  if ((!t896) && u_is_con(s895, &n_list_46_Nil)) {
    t896 = &o_list_46_Nil;
  }
  if ((!t896) && (u_is_con(s895, &n_list_46_Cons) && u_is_con(u_field(s895, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s895, 0), 0);
    u_value v_rest = u_field(s895, 1);
    t896 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t896) && (u_is_con(s895, &n_list_46_Cons) && u_is_con(u_field(s895, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s895, 1);
    t896 = f_parse_46_field_names_1(v_rest);
  }
  if (!t896) {
    u_no_case(s895);
  }
  return t896;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s897 = v_fs;
  u_value t898 = NULL;
  if ((!t898) && u_is_con(s897, &n_list_46_Nil)) {
    t898 = &o_option_46_None;
  }
  if ((!t898) && u_is_con(s897, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s897, 0), 0);
    u_value v_t = u_field(u_field(u_field(s897, 0), 1), 3);
    u_value v_rest = u_field(s897, 1);
    u_value t900;
    if (u_truth(f_list_46_exists_2(f_parse_46_fields_named_2(v_f, v_rest), u_mk_closure(lam899, 1, v_t)))) {
      t900 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t900 = f_parse_46_differing_1(v_rest);
    }
    t898 = t900;
  }
  if (!t898) {
    u_no_case(s897);
  }
  return t898;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s901 = v_e;
  u_value t902 = NULL;
  if (!t902) {
    u_value v_t = u_field(u_field(s901, 1), 3);
    t902 = v_t;
  }
  if (!t902) {
    u_no_case(s901);
  }
  return t902;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s903 = v_fs;
  u_value t904 = NULL;
  if ((!t904) && u_is_con(s903, &n_list_46_Nil)) {
    t904 = &o_list_46_Nil;
  }
  if ((!t904) && u_is_con(s903, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s903, 0), 0);
    u_value v_d = u_field(u_field(s903, 0), 1);
    u_value v_rest = u_field(s903, 1);
    u_value t905;
    if (u_truth(u_eq(v_g, v_f))) {
      t905 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t905 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t904 = t905;
  }
  if (!t904) {
    u_no_case(s903);
  }
  return t904;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s906 = v_ks;
  u_value t907 = NULL;
  if ((!t907) && u_is_con(s906, &n_list_46_Nil)) {
    t907 = &o_list_46_Nil;
  }
  if ((!t907) && (u_is_con(s906, &n_list_46_Cons) && u_is_con(u_field(u_field(s906, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s906, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s906, 0), 0), 1);
    u_value v_fs = u_field(u_field(s906, 0), 1);
    u_value v_rest = u_field(s906, 1);
    t907 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t907) {
    u_no_case(s906);
  }
  return t907;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s908 = v_fs;
  u_value t909 = NULL;
  if ((!t909) && u_is_con(s908, &n_list_46_Nil)) {
    t909 = &o_list_46_Nil;
  }
  if ((!t909) && (u_is_con(s908, &n_list_46_Cons) && u_is_con(u_field(u_field(s908, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s908, 0), 0), 0);
    u_value v_t = u_field(u_field(s908, 0), 1);
    u_value v_rest = u_field(s908, 1);
    t909 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t909) && (u_is_con(s908, &n_list_46_Cons) && u_is_con(u_field(u_field(s908, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s908, 1);
    t909 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t909) {
    u_no_case(s908);
  }
  return t909;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t914 = f_parse_46_fields_of_1(v_ks);
  u_value t913;
  {
    u_value v_fs = t914;
    u_value t910 = &o_list_46_Nil;
    u_value *e911 = &t910;
    u_value l912 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l912, &n_list_46_Cons)) {
      u_value v_f = u_field(l912, 0);
      if (u_truth(u_mk_bool(1))) {
        e911 = u_append(e911, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l912 = u_field(l912, 1);
    }
    t913 = t910;
  }
  return t913;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_915 = U_LIT("x");
  u_value t916 = &o_list_46_Nil;
  u_value *e917 = &t916;
  u_value l918 = v_ps;
  while (u_is_con(l918, &n_list_46_Cons)) {
    u_value v_p = u_field(l918, 0);
    if (u_truth(u_mk_bool(1))) {
      e917 = u_append(e917, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l918 = u_field(l918, 1);
  }
  u_value t926 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_915), &o_syntax_46_THole);
  u_value t927 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t916);
  u_value t925;
  {
    u_value v_x = t926;
    u_value v_owner = t927;
    static struct u_object l__46__919 = U_LIT(".");
    static struct u_object l_x_920 = U_LIT("x");
    u_value t921 = &o_list_46_Nil;
    u_value *e922 = &t921;
    u_value l923 = v_es;
    while (u_is_con(l923, &n_list_46_Cons)) {
      u_value v_e = u_field(l923, 0);
      if (u_truth(u_mk_bool(1))) {
        e922 = u_append(e922, f_parse_46_field_case_1(v_e));
      }
      l923 = u_field(l923, 1);
    }
    u_value t924;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t924 = &o_list_46_Nil;
    } else {
      t924 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), &o_list_46_Nil);
    }
    t925 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__919, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_920), v_owner), &o_list_46_Nil), &o_list_46_Nil), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t921, t924)));
  }
  return t925;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s928 = v_es;
  u_value t929 = NULL;
  if ((!t929) && u_is_con(s928, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s928, 0), 1), 3);
    t929 = v_t;
  }
  if (!t929) {
    t929 = &o_syntax_46_THole;
  }
  if (!t929) {
    u_no_case(s928);
  }
  return t929;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s930 = v_e;
  u_value t931 = NULL;
  if (!t931) {
    u_value v_k = u_field(u_field(s930, 1), 0);
    u_value v_i = u_field(u_field(s930, 1), 1);
    u_value v_n = u_field(u_field(s930, 1), 2);
    u_value t932 = &o_list_46_Nil;
    u_value *e933 = &t932;
    u_value l934 = f_parse_46_positions_1(v_n);
    while (u_is_con(l934, &n_list_46_Cons)) {
      u_value v_j = u_field(l934, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t936;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_935 = U_LIT("y");
          t936 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_935), &o_syntax_46_THole));
        } else {
          t936 = &o_syntax_46_PWild;
        }
        e933 = u_append(e933, t936);
      }
      l934 = u_field(l934, 1);
    }
    static struct u_object l_y_937 = U_LIT("y");
    t931 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t932), &o_option_46_None, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_937), &o_syntax_46_THole)));
  }
  if (!t931) {
    u_no_case(s930);
  }
  return t931;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_MissingField_938 = U_LIT("MissingField");
  static struct u_object l__46__939 = U_LIT(".");
  return u_mk_con(&n_syntax_46_Branch, 3, &o_syntax_46_PWild, &o_option_46_None, u_mk_con(&n_syntax_46_ERaise, 2, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_MissingField_938), &o_syntax_46_THole)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_t, u_concat(&l__46__939, v_f)))), &o_list_46_Nil)), &o_syntax_46_THole));
}

u_value f_parse_46_updaters_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t944 = f_parse_46_fields_of_1(v_ks);
  u_value t943;
  {
    u_value v_fs = t944;
    u_value t940 = &o_list_46_Nil;
    u_value *e941 = &t940;
    u_value l942 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l942, &n_list_46_Cons)) {
      u_value v_f = u_field(l942, 0);
      if (u_truth(u_mk_bool(1))) {
        e941 = u_append(e941, f_parse_46_updater_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l942 = u_field(l942, 1);
    }
    t943 = t940;
  }
  return t943;
}

u_value f_parse_46_updater_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_945 = U_LIT("x");
  u_value t946 = &o_list_46_Nil;
  u_value *e947 = &t946;
  u_value l948 = v_ps;
  while (u_is_con(l948, &n_list_46_Cons)) {
    u_value v_p = u_field(l948, 0);
    if (u_truth(u_mk_bool(1))) {
      e947 = u_append(e947, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l948 = u_field(l948, 1);
  }
  u_value t958 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_945), &o_syntax_46_THole);
  u_value t959 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t946);
  u_value t957;
  {
    u_value v_x = t958;
    u_value v_owner = t959;
    static struct u_object l__46__949 = U_LIT(".");
    static struct u_object l__58__61__950 = U_LIT(":=");
    static struct u_object l_x_951 = U_LIT("x");
    static struct u_object l_y_952 = U_LIT("y");
    u_value t953 = &o_list_46_Nil;
    u_value *e954 = &t953;
    u_value l955 = v_es;
    while (u_is_con(l955, &n_list_46_Cons)) {
      u_value v_e = u_field(l955, 0);
      if (u_truth(u_mk_bool(1))) {
        e954 = u_append(e954, f_parse_46_update_case_1(v_e));
      }
      l955 = u_field(l955, 1);
    }
    u_value t956;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t956 = &o_list_46_Nil;
    } else {
      t956 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), &o_list_46_Nil);
    }
    t957 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__949, u_concat(v_f, &l__58__61__950)))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_951), v_owner), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_952), f_parse_46_field_type_1(v_es)), &o_list_46_Nil)), &o_list_46_Nil), v_owner, u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t953, t956)));
  }
  return t957;
}

u_value f_parse_46_update_case_1(u_value v_e) {
  u_value s960 = v_e;
  u_value t961 = NULL;
  if (!t961) {
    u_value v_k = u_field(u_field(s960, 1), 0);
    u_value v_i = u_field(u_field(s960, 1), 1);
    u_value v_n = u_field(u_field(s960, 1), 2);
    u_value t962 = &o_list_46_Nil;
    u_value *e963 = &t962;
    u_value l964 = f_parse_46_positions_1(v_n);
    while (u_is_con(l964, &n_list_46_Cons)) {
      u_value v_j = u_field(l964, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t966;
        if (u_truth(u_eq(v_j, v_i))) {
          t966 = &o_syntax_46_PWild;
        } else {
          static struct u_object l_a_965 = U_LIT("a");
          t966 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l_a_965, u_str_show(v_j))), &o_syntax_46_THole));
        }
        e963 = u_append(e963, t966);
      }
      l964 = u_field(l964, 1);
    }
    u_value t967 = &o_list_46_Nil;
    u_value *e968 = &t967;
    u_value l969 = f_parse_46_positions_1(v_n);
    while (u_is_con(l969, &n_list_46_Cons)) {
      u_value v_j = u_field(l969, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t972;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_970 = U_LIT("y");
          t972 = &l_y_970;
        } else {
          static struct u_object l_a_971 = U_LIT("a");
          t972 = u_concat(&l_a_971, u_str_show(v_j));
        }
        e968 = u_append(e968, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(t972), &o_syntax_46_THole)));
      }
      l969 = u_field(l969, 1);
    }
    t961 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t962), &o_option_46_None, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_k, &o_syntax_46_THole)), t967));
  }
  if (!t961) {
    u_no_case(s960);
  }
  return t961;
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t973;
  if (u_truth(u_eq(v_i, v_n))) {
    t973 = &o_list_46_Nil;
  } else {
    t973 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t973;
}

void p_parse_46_parse_function(u_value v_ns, u_value *r_c, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_xs = NULL;
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value v_b = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_name_or_symbol(&v_n, r_ts);
    p_parse_46_type_params(&v_ps, r_ts);
    p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
    u_value t974 = u_mk_con(&n_list_46_Cons, 2, v_xs, &o_list_46_Nil);
    v_xss = t974;
    u_value t976 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t976)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t975 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t975;
      }
      t976 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__977 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__977), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__978 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__978), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t979 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t979;
  }
}

void p_parse_46_parse_procedure(u_value v_ns, u_value *r_c, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_ins = NULL;
    u_value v_outs = NULL;
    u_value v_refs = NULL;
    u_value v_b = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_name(&v_n, r_ts);
    p_parse_46_type_params(&v_ps, r_ts);
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    p_parse_46_sections(u_mk_proc(wp_parse_46_parse_formal, 0), u_mk_proc(wp_parse_46_parse_formal, 0), u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_ins, &v_outs, &v_refs, r_ts);
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    static struct u_object l__58__61__980 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__980), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t981 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t981;
  }
}

void p_parse_46_parse_command(u_value v_cx, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_ns = NULL;
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_t = NULL;
    u_value v_c = NULL;
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_xs = NULL;
    u_value v_at = NULL;
    u_value t982 = f_parse_46_Context_46_notations_1(v_cx);
    v_ns = t982;
    u_value s983 = f_parse_46_head_token_1(*r_ts);
    int d984 = 0;
    if ((!d984) && (u_is_con(s983, &n_lex_46_TName) && u_equals(u_field(s983, 0), u_mk_lit("import")))) {
      d984 = 1;
      {
        u_value t985 = f_parse_46_head_offset_1(*r_ts);
        v_at = t985;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Context_46_modules_1(v_cx))))) {
          {
            u_value t986 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), &o_list_46_Nil);
            *r_cs = t986;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_cx, v_at, r_cs);
            u_value t987 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), &o_list_46_Nil));
            *r_cs = t987;
          }
        }
      }
    }
    if ((!d984) && u_is_con(s983, &n_lex_46_TName)) {
      u_value v_w = u_field(s983, 0);
      static struct u_object l_prefix_989 = U_LIT("prefix");
      static struct u_object l_postfix_990 = U_LIT("postfix");
      static struct u_object l_infix_991 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_989, u_mk_con(&n_list_46_Cons, 2, &l_postfix_990, u_mk_con(&n_list_46_Cons, 2, &l_infix_991, &o_list_46_Nil)))))) {
        d984 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t988 = u_mk_con(&n_list_46_Cons, 2, v_c, &o_list_46_Nil);
          *r_cs = t988;
        }
      }
    }
    if ((!d984) && (u_is_con(s983, &n_lex_46_TName) && u_equals(u_field(s983, 0), u_mk_lit("type")))) {
      d984 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__992 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__992), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t993 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), &o_list_46_Nil);
        *r_cs = t993;
      }
    }
    if ((!d984) && (u_is_con(s983, &n_lex_46_TName) && u_equals(u_field(s983, 0), u_mk_lit("data")))) {
      d984 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d984) && (u_is_con(s983, &n_lex_46_TName) && u_equals(u_field(s983, 0), u_mk_lit("function")))) {
      d984 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t994 = u_mk_con(&n_list_46_Cons, 2, v_c, &o_list_46_Nil);
        *r_cs = t994;
      }
    }
    if ((!d984) && (u_is_con(s983, &n_lex_46_TName) && u_equals(u_field(s983, 0), u_mk_lit("procedure")))) {
      d984 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t995 = u_mk_con(&n_list_46_Cons, 2, v_c, &o_list_46_Nil);
        *r_cs = t995;
      }
    }
    if ((!d984) && (u_is_con(s983, &n_lex_46_TName) && u_equals(u_field(s983, 0), u_mk_lit("test")))) {
      d984 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, &o_list_46_Nil, v_ns), &v_xs, r_ts);
        static struct u_object l__58__61__996 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__996), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, &o_list_46_Nil, v_ns), &v_b, r_ts);
        u_value t997 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 3, v_n, v_xs, v_b), &o_list_46_Nil);
        *r_cs = t997;
      }
    }
    if (!d984) {
      d984 = 1;
      {
        static struct u_object l_declaration_32_expe_998 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_999 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_998, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_999)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d984) {
      u_no_case(s983);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_cx) {
  {
    u_value s1000 = v_cs;
    int d1001 = 0;
    if ((!d1001) && u_is_con(s1000, &n_list_46_Nil)) {
      d1001 = 1;
      {
      }
    }
    if ((!d1001) && (u_is_con(s1000, &n_list_46_Cons) && u_is_con(u_field(s1000, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1000, 0), 0);
      u_value v_p = u_field(u_field(s1000, 0), 1);
      u_value v_ops = u_field(u_field(s1000, 0), 2);
      u_value v_more = u_field(s1000, 1);
      d1001 = 1;
      {
        u_value t1002 = f_parse_46_with_commands_2(*r_cx, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), &o_list_46_Nil));
        *r_cx = t1002;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1001) && (u_is_con(s1000, &n_list_46_Cons) && u_is_con(u_field(s1000, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1000, 0), 0);
      u_value v_ds = u_field(u_field(s1000, 0), 1);
      u_value v_more = u_field(s1000, 1);
      d1001 = 1;
      {
        u_value t1003 = f_parse_46_with_module_2(*r_cx, u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds));
        *r_cx = t1003;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1001) && (u_is_con(s1000, &n_list_46_Cons) && u_is_con(u_field(s1000, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1000, 0), 0);
      u_value v_more = u_field(s1000, 1);
      d1001 = 1;
      {
        u_value t1004 = f_parse_46_with_commands_2(*r_cx, f_parse_46_own_notations_2(*r_cx, v_n));
        *r_cx = t1004;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1001) && u_is_con(s1000, &n_list_46_Cons)) {
      u_value v_more = u_field(s1000, 1);
      d1001 = 1;
      {
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if (!d1001) {
      u_no_case(s1000);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_cx, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1005 = &o_list_46_Nil;
    v_rev = t1005;
    u_value t1007 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), &o_lex_46_TEnd);
    while (u_truth(t1007)) {
      {
        p_parse_46_parse_command(*r_cx, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_cx);
        u_value t1006 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1006;
      }
      t1007 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), &o_lex_46_TEnd);
    }
    u_value t1008 = f_list_46_reverse_1(v_rev);
    *r_cs = t1008;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1009 = v_dir;
    int d1010 = 0;
    if ((!d1010) && u_is_con(s1009, &n_option_46_Some)) {
      u_value v_d = u_field(s1009, 0);
      d1010 = 1;
      {
        {
          struct u_handler *h1013 = u_push();
          if (setjmp(h1013->env) == 0) {
            {
              static struct u_object l__47__1011 = U_LIT("/");
              static struct u_object l__46_u_1012 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1011, u_concat(v_n, &l__46_u_1012))), r_text);
            }
            u_pop(h1013);
          } else {
            u_pop(h1013);
            int d1014 = 0;
            if ((!d1014) && (u_has_type(h1013, "os.OsError") && u_is_con(u_caught(h1013), &n_os_46_OsError))) {
              d1014 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1014) {
              u_reraise(h1013);
            }
          }
        }
      }
    }
    if ((!d1010) && u_is_con(s1009, &n_option_46_None)) {
      d1010 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1010) {
      u_no_case(s1009);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1015 = u_push();
      if (setjmp(h1015->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1015);
      } else {
        u_pop(h1015);
        int d1016 = 0;
        if ((!d1016) && (u_has_type(h1015, "os.OsError") && u_is_con(u_caught(h1015), &n_os_46_OsError))) {
          d1016 = 1;
          {
            static struct u_object l_unknown_32_module_32__1017 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1017, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1016) {
          u_reraise(h1015);
        }
      }
    }
  }
}

void p_parse_46_load_module(u_value v_n, u_value v_cx, u_value v_at, u_value *r_cs) {
  {
    u_value v_text = NULL;
    u_value v_mts = NULL;
    u_value v_mcx = NULL;
    if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Context_46_loading_1(v_cx)))) {
      {
        static struct u_object l_cyclic_32_import_32_of_1018 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1018, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Context_46_dir_1(v_cx), v_at, &v_text);
    u_value t1019 = f_parse_46_Context_46_loading_58__61__2(f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_base_notations_1(v_cx)), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Context_46_loading_1(v_cx)));
    v_mcx = t1019;
    {
      struct u_handler *h1020 = u_push();
      if (setjmp(h1020->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mcx, &v_mts);
        }
        u_pop(h1020);
      } else {
        u_pop(h1020);
        int d1021 = 0;
        if ((!d1021) && (u_has_type(h1020, "lex.LexError") && u_is_con(u_caught(h1020), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1020), 0);
          u_value v_i = u_field(u_caught(h1020), 1);
          d1021 = 1;
          {
            static struct u_object l__32_in_32__1022 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1022, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1021) && (u_has_type(h1020, "parse.ParseError") && u_is_con(u_caught(h1020), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1020), 0);
          u_value v_i = u_field(u_caught(h1020), 1);
          d1021 = 1;
          {
            static struct u_object l__32_in_32__1023 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1023, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1021) {
          u_reraise(h1020);
        }
      }
    }
  }
}

u_value f_parse_46_base_notations_1(u_value v_cx) {
  u_value s1024 = f_parse_46_Context_46_base_1(v_cx);
  u_value t1025 = NULL;
  if ((!t1025) && u_is_con(s1024, &n_option_46_Some)) {
    u_value v_b = u_field(s1024, 0);
    t1025 = v_b;
  }
  if ((!t1025) && u_is_con(s1024, &n_option_46_None)) {
    t1025 = f_parse_46_Context_46_notations_1(v_cx);
  }
  if (!t1025) {
    u_no_case(s1024);
  }
  return t1025;
}

void p_parse_46_parse_text(u_value v_text, u_value v_cx, u_value *r_p) {
  {
    u_value v_ts = NULL;
    u_value v_cs = NULL;
    u_value v_after = NULL;
    u_value t1026 = v_cx;
    v_after = t1026;
    {
      struct u_handler *h1027 = u_push();
      if (setjmp(h1027->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1027);
      } else {
        u_pop(h1027);
        int d1028 = 0;
        if ((!d1028) && (u_has_type(h1027, "lex.LexError") && u_is_con(u_caught(h1027), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1027), 0);
          u_value v_i = u_field(u_caught(h1027), 1);
          d1028 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1028) {
          u_reraise(h1027);
        }
      }
    }
    p_parse_46_parse_commands(&v_cs, &v_after, &v_ts);
    u_value t1029 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1029;
  }
}

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p) {
  {
    u_value v_cx = NULL;
    u_value t1030 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), &o_list_46_Nil, &o_list_46_Nil, &o_option_46_None, &o_option_46_None);
    v_cx = t1030;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), &v_cx);
    p_parse_46_parse_text(v_text, f_parse_46_Context_46_dir_58__61__2(f_parse_46_Context_46_base_58__61__2(f_parse_46_Context_46_loading_58__61__2(v_cx, u_mk_con(&n_list_46_Cons, 2, v_name, &o_list_46_Nil)), u_mk_con(&n_option_46_Some, 1, f_parse_46_Context_46_notations_1(v_cx))), v_dir), r_p);
  }
}

void p_parse_46_parse_library(u_value v_n, u_value *r_p) {
  {
    u_value v_text = NULL;
    u_value v_pre = NULL;
    p_parse_46_library_text(v_n, u_mk_int(0), &v_text);
    static struct u_object l_prelude_1031 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1031))) {
      {
        static struct u_object l_prelude_1032 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, &l_prelude_1032, &o_list_46_Nil), &o_option_46_None, &o_option_46_None), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1033 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1033, &v_pre);
        p_parse_46_module_after(v_n, v_pre, &o_option_46_None, v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1034 = v_xs;
  u_value t1035 = NULL;
  if ((!t1035) && (u_is_con(s1034, &n_list_46_Cons) && u_is_con(u_field(s1034, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1034, 0);
    t1035 = v_x;
  }
  if ((!t1035) && u_is_con(s1034, &n_list_46_Cons)) {
    u_value v_more = u_field(s1034, 1);
    t1035 = f_parse_46_last_part_1(v_more);
  }
  if (!t1035) {
    static struct u_object l__1036 = U_LIT("");
    t1035 = &l__1036;
  }
  if (!t1035) {
    u_no_case(s1034);
  }
  return t1035;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1041 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1040;
  {
    u_value v_f = t1041;
    u_value t1038 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1038)) {
      static struct u_object l__46_u_1037 = U_LIT(".u");
      t1038 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1037);
    }
    u_value t1039;
    if (u_truth(t1038)) {
      t1039 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1039 = v_f;
    }
    t1040 = t1039;
  }
  return t1040;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1046 = u_str_split(v_path, u_mk_char(47));
  u_value t1045;
  {
    u_value v_parts = t1046;
    u_value t1044;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1042 = U_LIT(".");
      t1044 = &l__46__1042;
    } else {
      static struct u_object l__47__1043 = U_LIT("/");
      t1044 = u_str_join(&l__47__1043, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1045 = t1044;
  }
  return t1045;
}

void p_parse_46_parse_file_after(u_value v_path, u_value v_pre, u_value *r_p) {
  {
    u_value v_text = NULL;
    p_os_read(v_path, &v_text);
    p_parse_46_module_after(f_parse_46_base_name_1(v_path), v_pre, u_mk_con(&n_option_46_Some, 1, f_parse_46_dir_name_1(v_path)), v_text, r_p);
  }
}

void p_parse_46_parse_file(u_value v_path, u_value *r_p) {
  {
    u_value v_pre = NULL;
    static struct u_object l_prelude_1047 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1047, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1048 = u_mk_tuple(2, v_cs, v_ds);
    int d1049 = 0;
    if ((!d1049) && (u_is_con(u_field(s1048, 0), &n_list_46_Cons) && u_is_con(u_field(s1048, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1048, 0), 0);
      u_value v_more = u_field(u_field(s1048, 0), 1);
      u_value v_d = u_field(u_field(s1048, 1), 0);
      u_value v_rest = u_field(u_field(s1048, 1), 1);
      d1049 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1049) {
      d1049 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1049) {
      u_no_case(s1048);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1050 = v_paths;
    int d1051 = 0;
    if ((!d1051) && u_is_con(s1050, &n_list_46_Nil)) {
      d1051 = 1;
      {
      }
    }
    if ((!d1051) && u_is_con(s1050, &n_list_46_Cons)) {
      u_value v_path = u_field(s1050, 0);
      u_value v_more = u_field(s1050, 1);
      d1051 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1051) {
      u_no_case(s1050);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1052 = v_paths;
    int d1053 = 0;
    if ((!d1053) && u_is_con(s1052, &n_list_46_Nil)) {
      d1053 = 1;
      {
      }
    }
    if ((!d1053) && u_is_con(s1052, &n_list_46_Cons)) {
      u_value v_path = u_field(s1052, 0);
      u_value v_more = u_field(s1052, 1);
      d1053 = 1;
      {
        u_value t1054 = u_mk_bool(0);
        v_failed = t1054;
        {
          struct u_handler *h1055 = u_push();
          if (setjmp(h1055->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1055);
          } else {
            u_pop(h1055);
            int d1056 = 0;
            if ((!d1056) && (u_has_type(h1055, "parse.ParseError") && u_is_con(u_caught(h1055), &n_parse_46_ParseError))) {
              d1056 = 1;
              {
                u_value t1057 = u_mk_bool(1);
                v_failed = t1057;
              }
            }
            if (!d1056) {
              u_reraise(h1055);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1053) {
      u_no_case(s1052);
    }
  }
}

u_value f_map_46_empty_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of map.empty");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  u_pop(handler);
  cache = &o_map_46_MLeaf;
  return cache;
}

u_value f_map_46_get_2(u_value v_k, u_value v_m) {
  u_value s1058 = v_m;
  u_value t1059 = NULL;
  if ((!t1059) && u_is_con(s1058, &n_map_46_MLeaf)) {
    t1059 = &o_option_46_None;
  }
  if ((!t1059) && u_is_con(s1058, &n_map_46_MNode)) {
    u_value v_l = u_field(s1058, 0);
    u_value v_j = u_field(s1058, 1);
    u_value v_v = u_field(s1058, 2);
    u_value v_r = u_field(s1058, 3);
    u_value s1060 = u_compare(v_k, v_j);
    u_value t1061 = NULL;
    if ((!t1061) && u_equals(s1060, u_mk_int(-1))) {
      t1061 = f_map_46_get_2(v_k, v_l);
    }
    if ((!t1061) && u_equals(s1060, u_mk_int(1))) {
      t1061 = f_map_46_get_2(v_k, v_r);
    }
    if (!t1061) {
      t1061 = u_mk_con(&n_option_46_Some, 1, v_v);
    }
    if (!t1061) {
      u_no_case(s1060);
    }
    t1059 = t1061;
  }
  if (!t1059) {
    u_no_case(s1058);
  }
  return t1059;
}

u_value f_map_46_has_2(u_value v_k, u_value v_m) {
  return f_prelude_46__33__61__2(f_map_46_get_2(v_k, v_m), &o_option_46_None);
}

u_value f_map_46_put_3(u_value v_k, u_value v_v, u_value v_m) {
  u_value s1062 = v_m;
  u_value t1063 = NULL;
  if ((!t1063) && u_is_con(s1062, &n_map_46_MLeaf)) {
    t1063 = u_mk_con(&n_map_46_MNode, 5, &o_map_46_MLeaf, v_k, v_v, &o_map_46_MLeaf, u_mk_int(1));
  }
  if ((!t1063) && u_is_con(s1062, &n_map_46_MNode)) {
    u_value v_l = u_field(s1062, 0);
    u_value v_j = u_field(s1062, 1);
    u_value v_w = u_field(s1062, 2);
    u_value v_r = u_field(s1062, 3);
    u_value v_h = u_field(s1062, 4);
    u_value s1064 = u_compare(v_k, v_j);
    u_value t1065 = NULL;
    if ((!t1065) && u_equals(s1064, u_mk_int(-1))) {
      t1065 = f_map_46_balance_4(f_map_46_put_3(v_k, v_v, v_l), v_j, v_w, v_r);
    }
    if ((!t1065) && u_equals(s1064, u_mk_int(1))) {
      t1065 = f_map_46_balance_4(v_l, v_j, v_w, f_map_46_put_3(v_k, v_v, v_r));
    }
    if (!t1065) {
      t1065 = u_mk_con(&n_map_46_MNode, 5, v_l, v_k, v_v, v_r, v_h);
    }
    if (!t1065) {
      u_no_case(s1064);
    }
    t1063 = t1065;
  }
  if (!t1063) {
    u_no_case(s1062);
  }
  return t1063;
}

u_value f_map_46_entries_1(u_value v_m) {
  return f_map_46_entries_onto_2(v_m, &o_list_46_Nil);
}

u_value f_map_46_entries_onto_2(u_value v_m, u_value v_rest) {
  u_value s1066 = v_m;
  u_value t1067 = NULL;
  if ((!t1067) && u_is_con(s1066, &n_map_46_MLeaf)) {
    t1067 = v_rest;
  }
  if ((!t1067) && u_is_con(s1066, &n_map_46_MNode)) {
    u_value v_l = u_field(s1066, 0);
    u_value v_k = u_field(s1066, 1);
    u_value v_v = u_field(s1066, 2);
    u_value v_r = u_field(s1066, 3);
    t1067 = f_map_46_entries_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_v), f_map_46_entries_onto_2(v_r, v_rest)));
  }
  if (!t1067) {
    u_no_case(s1066);
  }
  return t1067;
}

u_value f_map_46_size_1(u_value v_m) {
  u_value s1068 = v_m;
  u_value t1069 = NULL;
  if ((!t1069) && u_is_con(s1068, &n_map_46_MLeaf)) {
    t1069 = u_mk_int(0);
  }
  if ((!t1069) && u_is_con(s1068, &n_map_46_MNode)) {
    u_value v_l = u_field(s1068, 0);
    u_value v_r = u_field(s1068, 3);
    t1069 = u_add(u_add(f_map_46_size_1(v_l), u_mk_int(1)), f_map_46_size_1(v_r));
  }
  if (!t1069) {
    u_no_case(s1068);
  }
  return t1069;
}

u_value f_map_46_height_1(u_value v_m) {
  u_value s1070 = v_m;
  u_value t1071 = NULL;
  if ((!t1071) && u_is_con(s1070, &n_map_46_MLeaf)) {
    t1071 = u_mk_int(0);
  }
  if ((!t1071) && u_is_con(s1070, &n_map_46_MNode)) {
    u_value v_h = u_field(s1070, 4);
    t1071 = v_h;
  }
  if (!t1071) {
    u_no_case(s1070);
  }
  return t1071;
}

u_value f_map_46_mnode_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r) {
  return u_mk_con(&n_map_46_MNode, 5, v_l, v_k, v_v, v_r, f_map_46_mnode_height_2(v_l, v_r));
}

u_value f_map_46_rotate_left_1(u_value v_m) {
  u_value s1072 = v_m;
  u_value t1073 = NULL;
  if ((!t1073) && (u_is_con(s1072, &n_map_46_MNode) && u_is_con(u_field(s1072, 3), &n_map_46_MNode))) {
    u_value v_a = u_field(s1072, 0);
    u_value v_j = u_field(s1072, 1);
    u_value v_v = u_field(s1072, 2);
    u_value v_b = u_field(u_field(s1072, 3), 0);
    u_value v_k = u_field(u_field(s1072, 3), 1);
    u_value v_w = u_field(u_field(s1072, 3), 2);
    u_value v_c = u_field(u_field(s1072, 3), 3);
    t1073 = f_map_46_mnode_4(f_map_46_mnode_4(v_a, v_j, v_v, v_b), v_k, v_w, v_c);
  }
  if (!t1073) {
    t1073 = v_m;
  }
  if (!t1073) {
    u_no_case(s1072);
  }
  return t1073;
}

u_value f_map_46_rotate_right_1(u_value v_m) {
  u_value s1074 = v_m;
  u_value t1075 = NULL;
  if ((!t1075) && (u_is_con(s1074, &n_map_46_MNode) && u_is_con(u_field(s1074, 0), &n_map_46_MNode))) {
    u_value v_a = u_field(u_field(s1074, 0), 0);
    u_value v_j = u_field(u_field(s1074, 0), 1);
    u_value v_v = u_field(u_field(s1074, 0), 2);
    u_value v_b = u_field(u_field(s1074, 0), 3);
    u_value v_k = u_field(s1074, 1);
    u_value v_w = u_field(s1074, 2);
    u_value v_c = u_field(s1074, 3);
    t1075 = f_map_46_mnode_4(v_a, v_j, v_v, f_map_46_mnode_4(v_b, v_k, v_w, v_c));
  }
  if (!t1075) {
    t1075 = v_m;
  }
  if (!t1075) {
    u_no_case(s1074);
  }
  return t1075;
}

u_value f_map_46_left_of_1(u_value v_m) {
  u_value s1076 = v_m;
  u_value t1077 = NULL;
  if ((!t1077) && u_is_con(s1076, &n_map_46_MNode)) {
    u_value v_l = u_field(s1076, 0);
    t1077 = v_l;
  }
  if (!t1077) {
    t1077 = v_m;
  }
  if (!t1077) {
    u_no_case(s1076);
  }
  return t1077;
}

u_value f_map_46_right_of_1(u_value v_m) {
  u_value s1078 = v_m;
  u_value t1079 = NULL;
  if ((!t1079) && u_is_con(s1078, &n_map_46_MNode)) {
    u_value v_r = u_field(s1078, 3);
    t1079 = v_r;
  }
  if (!t1079) {
    t1079 = v_m;
  }
  if (!t1079) {
    u_no_case(s1078);
  }
  return t1079;
}

u_value f_map_46_balance_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r) {
  u_value t1083;
  if (u_truth(f_prelude_46__62__2(f_map_46_height_1(v_l), u_add(f_map_46_height_1(v_r), u_mk_int(1))))) {
    u_value t1080;
    if (u_truth(u_lt(f_map_46_height_1(f_map_46_left_of_1(v_l)), f_map_46_height_1(f_map_46_right_of_1(v_l))))) {
      t1080 = f_map_46_rotate_left_1(v_l);
    } else {
      t1080 = v_l;
    }
    t1083 = f_map_46_rotate_right_1(f_map_46_mnode_4(t1080, v_k, v_v, v_r));
  } else {
    u_value t1082;
    if (u_truth(f_prelude_46__62__2(f_map_46_height_1(v_r), u_add(f_map_46_height_1(v_l), u_mk_int(1))))) {
      u_value t1081;
      if (u_truth(u_lt(f_map_46_height_1(f_map_46_right_of_1(v_r)), f_map_46_height_1(f_map_46_left_of_1(v_r))))) {
        t1081 = f_map_46_rotate_right_1(v_r);
      } else {
        t1081 = v_r;
      }
      t1082 = f_map_46_rotate_left_1(f_map_46_mnode_4(v_l, v_k, v_v, t1081));
    } else {
      t1082 = f_map_46_mnode_4(v_l, v_k, v_v, v_r);
    }
    t1083 = t1082;
  }
  return t1083;
}

u_value f_map_46_balanced_1(u_value v_m) {
  u_value s1084 = v_m;
  u_value t1085 = NULL;
  if ((!t1085) && u_is_con(s1084, &n_map_46_MLeaf)) {
    t1085 = u_mk_bool(1);
  }
  if ((!t1085) && u_is_con(s1084, &n_map_46_MNode)) {
    u_value v_l = u_field(s1084, 0);
    u_value v_r = u_field(s1084, 3);
    u_value v_h = u_field(s1084, 4);
    u_value t1086 = f_map_46_balanced_1(v_l);
    if (u_truth(t1086)) {
      t1086 = f_map_46_balanced_1(v_r);
    }
    u_value t1087 = t1086;
    if (u_truth(t1087)) {
      t1087 = u_eq(v_h, f_map_46_mnode_height_2(v_l, v_r));
    }
    u_value t1088 = t1087;
    if (u_truth(t1088)) {
      t1088 = f_prelude_46__60__61__2(f_map_46_height_1(v_l), u_add(f_map_46_height_1(v_r), u_mk_int(1)));
    }
    u_value t1089 = t1088;
    if (u_truth(t1089)) {
      t1089 = f_prelude_46__60__61__2(f_map_46_height_1(v_r), u_add(f_map_46_height_1(v_l), u_mk_int(1)));
    }
    t1085 = t1089;
  }
  if (!t1085) {
    u_no_case(s1084);
  }
  return t1085;
}

u_value f_map_46_mnode_height_2(u_value v_l, u_value v_r) {
  u_value t1090;
  if (u_truth(u_lt(f_map_46_height_1(v_l), f_map_46_height_1(v_r)))) {
    t1090 = f_map_46_height_1(v_r);
  } else {
    t1090 = f_map_46_height_1(v_l);
  }
  return u_add(u_mk_int(1), t1090);
}

u_value f_map_46_delete_2(u_value v_k, u_value v_m) {
  u_value s1091 = v_m;
  u_value t1092 = NULL;
  if ((!t1092) && u_is_con(s1091, &n_map_46_MLeaf)) {
    t1092 = &o_map_46_MLeaf;
  }
  if ((!t1092) && u_is_con(s1091, &n_map_46_MNode)) {
    u_value v_l = u_field(s1091, 0);
    u_value v_j = u_field(s1091, 1);
    u_value v_v = u_field(s1091, 2);
    u_value v_r = u_field(s1091, 3);
    u_value s1093 = u_compare(v_k, v_j);
    u_value t1094 = NULL;
    if ((!t1094) && u_equals(s1093, u_mk_int(-1))) {
      t1094 = f_map_46_balance_4(f_map_46_delete_2(v_k, v_l), v_j, v_v, v_r);
    }
    if ((!t1094) && u_equals(s1093, u_mk_int(1))) {
      t1094 = f_map_46_balance_4(v_l, v_j, v_v, f_map_46_delete_2(v_k, v_r));
    }
    if (!t1094) {
      t1094 = f_map_46_join_2(v_l, v_r);
    }
    if (!t1094) {
      u_no_case(s1093);
    }
    t1092 = t1094;
  }
  if (!t1092) {
    u_no_case(s1091);
  }
  return t1092;
}

u_value f_map_46_delete_all_2(u_value v_ks, u_value v_m) {
  u_value s1095 = v_ks;
  u_value t1096 = NULL;
  if ((!t1096) && u_is_con(s1095, &n_list_46_Nil)) {
    t1096 = v_m;
  }
  if ((!t1096) && u_is_con(s1095, &n_list_46_Cons)) {
    u_value v_k = u_field(s1095, 0);
    u_value v_rest = u_field(s1095, 1);
    t1096 = f_map_46_delete_all_2(v_rest, f_map_46_delete_2(v_k, v_m));
  }
  if (!t1096) {
    u_no_case(s1095);
  }
  return t1096;
}

u_value f_map_46_join_2(u_value v_l, u_value v_r) {
  u_value s1097 = f_map_46_first_entry_1(v_r);
  u_value t1098 = NULL;
  if ((!t1098) && u_is_con(s1097, &n_option_46_Some)) {
    u_value v_k = u_field(u_field(s1097, 0), 0);
    u_value v_v = u_field(u_field(s1097, 0), 1);
    t1098 = f_map_46_balance_4(v_l, v_k, v_v, f_map_46_remove_first_1(v_r));
  }
  if ((!t1098) && u_is_con(s1097, &n_option_46_None)) {
    t1098 = v_l;
  }
  if (!t1098) {
    u_no_case(s1097);
  }
  return t1098;
}

u_value f_map_46_first_entry_1(u_value v_m) {
  u_value s1099 = v_m;
  u_value t1100 = NULL;
  if ((!t1100) && u_is_con(s1099, &n_map_46_MLeaf)) {
    t1100 = &o_option_46_None;
  }
  if ((!t1100) && (u_is_con(s1099, &n_map_46_MNode) && u_is_con(u_field(s1099, 0), &n_map_46_MLeaf))) {
    u_value v_k = u_field(s1099, 1);
    u_value v_v = u_field(s1099, 2);
    t1100 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_k, v_v));
  }
  if ((!t1100) && u_is_con(s1099, &n_map_46_MNode)) {
    u_value v_l = u_field(s1099, 0);
    t1100 = f_map_46_first_entry_1(v_l);
  }
  if (!t1100) {
    u_no_case(s1099);
  }
  return t1100;
}

u_value f_map_46_remove_first_1(u_value v_m) {
  u_value s1101 = v_m;
  u_value t1102 = NULL;
  if ((!t1102) && u_is_con(s1101, &n_map_46_MLeaf)) {
    t1102 = &o_map_46_MLeaf;
  }
  if ((!t1102) && (u_is_con(s1101, &n_map_46_MNode) && u_is_con(u_field(s1101, 0), &n_map_46_MLeaf))) {
    u_value v_r = u_field(s1101, 3);
    t1102 = v_r;
  }
  if ((!t1102) && u_is_con(s1101, &n_map_46_MNode)) {
    u_value v_l = u_field(s1101, 0);
    u_value v_k = u_field(s1101, 1);
    u_value v_v = u_field(s1101, 2);
    u_value v_r = u_field(s1101, 3);
    t1102 = f_map_46_balance_4(f_map_46_remove_first_1(v_l), v_k, v_v, v_r);
  }
  if (!t1102) {
    u_no_case(s1101);
  }
  return t1102;
}

u_value f_map_46_put_all_2(u_value v_ps, u_value v_m) {
  u_value s1103 = v_ps;
  u_value t1104 = NULL;
  if ((!t1104) && u_is_con(s1103, &n_list_46_Nil)) {
    t1104 = v_m;
  }
  if ((!t1104) && u_is_con(s1103, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s1103, 0), 0);
    u_value v_v = u_field(u_field(s1103, 0), 1);
    u_value v_rest = u_field(s1103, 1);
    t1104 = f_map_46_put_all_2(v_rest, f_map_46_put_3(v_k, v_v, v_m));
  }
  if (!t1104) {
    u_no_case(s1103);
  }
  return t1104;
}

u_value f_map_46_numbers_1(u_value v_n) {
  u_value t1105;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t1105 = &o_list_46_Nil;
  } else {
    t1105 = f_list_46__43__43__2(f_map_46_numbers_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, &o_list_46_Nil));
  }
  return t1105;
}

u_value f_ctx_46_Ctx_46_aliases_1(u_value v_x) {
  u_value s1106 = v_x;
  u_value t1107 = NULL;
  if ((!t1107) && u_is_con(s1106, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s1106, 0);
    t1107 = v_y;
  }
  if (!t1107) {
    u_no_case(s1106);
  }
  return t1107;
}

u_value f_ctx_46_Ctx_46_funs_1(u_value v_x) {
  u_value s1108 = v_x;
  u_value t1109 = NULL;
  if ((!t1109) && u_is_con(s1108, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s1108, 1);
    t1109 = v_y;
  }
  if (!t1109) {
    u_no_case(s1108);
  }
  return t1109;
}

u_value f_ctx_46_Ctx_46_procs_1(u_value v_x) {
  u_value s1110 = v_x;
  u_value t1111 = NULL;
  if ((!t1111) && u_is_con(s1110, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s1110, 2);
    t1111 = v_y;
  }
  if (!t1111) {
    u_no_case(s1110);
  }
  return t1111;
}

u_value f_ctx_46_Ctx_46_datas_1(u_value v_x) {
  u_value s1112 = v_x;
  u_value t1113 = NULL;
  if ((!t1113) && u_is_con(s1112, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s1112, 3);
    t1113 = v_y;
  }
  if (!t1113) {
    u_no_case(s1112);
  }
  return t1113;
}

u_value f_ctx_46_Ctx_46_members_1(u_value v_x) {
  u_value s1114 = v_x;
  u_value t1115 = NULL;
  if ((!t1115) && u_is_con(s1114, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s1114, 4);
    t1115 = v_y;
  }
  if (!t1115) {
    u_no_case(s1114);
  }
  return t1115;
}

u_value f_ctx_46_Ctx_46_constrs_1(u_value v_x) {
  u_value s1116 = v_x;
  u_value t1117 = NULL;
  if ((!t1117) && u_is_con(s1116, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s1116, 5);
    t1117 = v_y;
  }
  if (!t1117) {
    u_no_case(s1116);
  }
  return t1117;
}

u_value f_ctx_46_Ctx_46_named_1(u_value v_x) {
  u_value s1118 = v_x;
  u_value t1119 = NULL;
  if ((!t1119) && u_is_con(s1118, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s1118, 6);
    t1119 = v_y;
  }
  if (!t1119) {
    u_no_case(s1118);
  }
  return t1119;
}

u_value f_ctx_46_Ctx_46_aliases_58__61__2(u_value v_x, u_value v_y) {
  u_value s1120 = v_x;
  u_value t1121 = NULL;
  if ((!t1121) && u_is_con(s1120, &n_ctx_46_Ctx)) {
    u_value v_a1 = u_field(s1120, 1);
    u_value v_a2 = u_field(s1120, 2);
    u_value v_a3 = u_field(s1120, 3);
    u_value v_a4 = u_field(s1120, 4);
    u_value v_a5 = u_field(s1120, 5);
    u_value v_a6 = u_field(s1120, 6);
    t1121 = u_mk_con(&n_ctx_46_Ctx, 7, v_y, v_a1, v_a2, v_a3, v_a4, v_a5, v_a6);
  }
  if (!t1121) {
    u_no_case(s1120);
  }
  return t1121;
}

u_value f_ctx_46_Ctx_46_funs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1122 = v_x;
  u_value t1123 = NULL;
  if ((!t1123) && u_is_con(s1122, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s1122, 0);
    u_value v_a2 = u_field(s1122, 2);
    u_value v_a3 = u_field(s1122, 3);
    u_value v_a4 = u_field(s1122, 4);
    u_value v_a5 = u_field(s1122, 5);
    u_value v_a6 = u_field(s1122, 6);
    t1123 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_y, v_a2, v_a3, v_a4, v_a5, v_a6);
  }
  if (!t1123) {
    u_no_case(s1122);
  }
  return t1123;
}

u_value f_ctx_46_Ctx_46_procs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1124 = v_x;
  u_value t1125 = NULL;
  if ((!t1125) && u_is_con(s1124, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s1124, 0);
    u_value v_a1 = u_field(s1124, 1);
    u_value v_a3 = u_field(s1124, 3);
    u_value v_a4 = u_field(s1124, 4);
    u_value v_a5 = u_field(s1124, 5);
    u_value v_a6 = u_field(s1124, 6);
    t1125 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_a1, v_y, v_a3, v_a4, v_a5, v_a6);
  }
  if (!t1125) {
    u_no_case(s1124);
  }
  return t1125;
}

u_value f_ctx_46_Ctx_46_datas_58__61__2(u_value v_x, u_value v_y) {
  u_value s1126 = v_x;
  u_value t1127 = NULL;
  if ((!t1127) && u_is_con(s1126, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s1126, 0);
    u_value v_a1 = u_field(s1126, 1);
    u_value v_a2 = u_field(s1126, 2);
    u_value v_a4 = u_field(s1126, 4);
    u_value v_a5 = u_field(s1126, 5);
    u_value v_a6 = u_field(s1126, 6);
    t1127 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_a1, v_a2, v_y, v_a4, v_a5, v_a6);
  }
  if (!t1127) {
    u_no_case(s1126);
  }
  return t1127;
}

u_value f_ctx_46_Ctx_46_members_58__61__2(u_value v_x, u_value v_y) {
  u_value s1128 = v_x;
  u_value t1129 = NULL;
  if ((!t1129) && u_is_con(s1128, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s1128, 0);
    u_value v_a1 = u_field(s1128, 1);
    u_value v_a2 = u_field(s1128, 2);
    u_value v_a3 = u_field(s1128, 3);
    u_value v_a5 = u_field(s1128, 5);
    u_value v_a6 = u_field(s1128, 6);
    t1129 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_a1, v_a2, v_a3, v_y, v_a5, v_a6);
  }
  if (!t1129) {
    u_no_case(s1128);
  }
  return t1129;
}

u_value f_ctx_46_Ctx_46_constrs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1130 = v_x;
  u_value t1131 = NULL;
  if ((!t1131) && u_is_con(s1130, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s1130, 0);
    u_value v_a1 = u_field(s1130, 1);
    u_value v_a2 = u_field(s1130, 2);
    u_value v_a3 = u_field(s1130, 3);
    u_value v_a4 = u_field(s1130, 4);
    u_value v_a6 = u_field(s1130, 6);
    t1131 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_a1, v_a2, v_a3, v_a4, v_y, v_a6);
  }
  if (!t1131) {
    u_no_case(s1130);
  }
  return t1131;
}

u_value f_ctx_46_Ctx_46_named_58__61__2(u_value v_x, u_value v_y) {
  u_value s1132 = v_x;
  u_value t1133 = NULL;
  if ((!t1133) && u_is_con(s1132, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s1132, 0);
    u_value v_a1 = u_field(s1132, 1);
    u_value v_a2 = u_field(s1132, 2);
    u_value v_a3 = u_field(s1132, 3);
    u_value v_a4 = u_field(s1132, 4);
    u_value v_a5 = u_field(s1132, 5);
    t1133 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_a1, v_a2, v_a3, v_a4, v_a5, v_y);
  }
  if (!t1133) {
    u_no_case(s1132);
  }
  return t1133;
}

u_value f_ctx_46_empty_ctx_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of ctx.empty_ctx");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  u_pop(handler);
  cache = u_mk_con(&n_ctx_46_Ctx, 7, f_map_46_empty_0(), f_map_46_empty_0(), f_map_46_empty_0(), f_map_46_empty_0(), f_map_46_empty_0(), f_map_46_empty_0(), f_map_46_empty_0());
  return cache;
}

u_value f_ctx_46_ctx_of_2(u_value v_c, u_value v_ms) {
  return u_mk_con(&n_ctx_46_Ctx, 7, f_ctx_46_owned_map_2(f_ctx_46_Ctx_46_aliases_1(v_c), v_ms), f_ctx_46_owned_map_2(f_ctx_46_Ctx_46_funs_1(v_c), v_ms), f_ctx_46_owned_map_2(f_ctx_46_Ctx_46_procs_1(v_c), v_ms), f_ctx_46_owned_map_2(f_ctx_46_Ctx_46_datas_1(v_c), v_ms), f_ctx_46_owned_map_2(f_ctx_46_Ctx_46_members_1(v_c), v_ms), f_ctx_46_owned_map_2(f_ctx_46_Ctx_46_constrs_1(v_c), v_ms), f_map_46_put_all_2(f_ctx_46_owned_named_2(f_map_46_entries_1(f_ctx_46_Ctx_46_named_1(v_c)), v_ms), f_map_46_empty_0()));
}

u_value f_ctx_46_owned_map_2(u_value v_m, u_value v_ms) {
  return f_map_46_put_all_2(f_ctx_46_owned_2(f_map_46_entries_1(v_m), v_ms), f_map_46_empty_0());
}

u_value f_ctx_46_owned_2(u_value v_es, u_value v_ms) {
  u_value s1134 = v_es;
  u_value t1135 = NULL;
  if ((!t1135) && u_is_con(s1134, &n_list_46_Nil)) {
    t1135 = &o_list_46_Nil;
  }
  if ((!t1135) && u_is_con(s1134, &n_list_46_Cons)) {
    u_value v_n = u_field(u_field(s1134, 0), 0);
    u_value v_v = u_field(u_field(s1134, 0), 1);
    u_value v_rest = u_field(s1134, 1);
    u_value t1136;
    if (u_truth(f_ctx_46_in_modules_2(v_n, v_ms))) {
      t1136 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, v_v), f_ctx_46_owned_2(v_rest, v_ms));
    } else {
      t1136 = f_ctx_46_owned_2(v_rest, v_ms);
    }
    t1135 = t1136;
  }
  if (!t1135) {
    u_no_case(s1134);
  }
  return t1135;
}

u_value f_ctx_46_owned_named_2(u_value v_es, u_value v_ms) {
  u_value s1137 = v_es;
  u_value t1138 = NULL;
  if ((!t1138) && u_is_con(s1137, &n_list_46_Nil)) {
    t1138 = &o_list_46_Nil;
  }
  if ((!t1138) && u_is_con(s1137, &n_list_46_Cons)) {
    u_value v_b = u_field(u_field(s1137, 0), 0);
    u_value v_ns = u_field(u_field(s1137, 0), 1);
    u_value v_rest = u_field(s1137, 1);
    u_value t1139 = &o_list_46_Nil;
    u_value *e1140 = &t1139;
    u_value l1141 = v_ns;
    while (u_is_con(l1141, &n_list_46_Cons)) {
      u_value v_n = u_field(l1141, 0);
      if (u_truth(f_ctx_46_in_modules_2(v_n, v_ms))) {
        e1140 = u_append(e1140, v_n);
      }
      l1141 = u_field(l1141, 1);
    }
    u_value t1144 = t1139;
    u_value t1143;
    {
      u_value v_ks = t1144;
      u_value t1142;
      if (u_truth(u_eq(v_ks, &o_list_46_Nil))) {
        t1142 = f_ctx_46_owned_named_2(v_rest, v_ms);
      } else {
        t1142 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_b, v_ks), f_ctx_46_owned_named_2(v_rest, v_ms));
      }
      t1143 = t1142;
    }
    t1138 = t1143;
  }
  if (!t1138) {
    u_no_case(s1137);
  }
  return t1138;
}

u_value f_ctx_46_in_modules_2(u_value v_n, u_value v_ms) {
  u_value s1145 = f_syntax_46_Name_46_module_1(v_n);
  u_value t1146 = NULL;
  if ((!t1146) && u_is_con(s1145, &n_option_46_Some)) {
    u_value v_m = u_field(s1145, 0);
    t1146 = f_list_46_contains_2(v_m, v_ms);
  }
  if ((!t1146) && u_is_con(s1145, &n_option_46_None)) {
    t1146 = u_mk_bool(0);
  }
  if (!t1146) {
    u_no_case(s1145);
  }
  return t1146;
}

u_value f_ctx_46_ctx_union_2(u_value v_a, u_value v_b) {
  return u_mk_con(&n_ctx_46_Ctx, 7, f_map_46_put_all_2(f_map_46_entries_1(f_ctx_46_Ctx_46_aliases_1(v_b)), f_ctx_46_Ctx_46_aliases_1(v_a)), f_map_46_put_all_2(f_map_46_entries_1(f_ctx_46_Ctx_46_funs_1(v_b)), f_ctx_46_Ctx_46_funs_1(v_a)), f_map_46_put_all_2(f_map_46_entries_1(f_ctx_46_Ctx_46_procs_1(v_b)), f_ctx_46_Ctx_46_procs_1(v_a)), f_map_46_put_all_2(f_map_46_entries_1(f_ctx_46_Ctx_46_datas_1(v_b)), f_ctx_46_Ctx_46_datas_1(v_a)), f_map_46_put_all_2(f_map_46_entries_1(f_ctx_46_Ctx_46_members_1(v_b)), f_ctx_46_Ctx_46_members_1(v_a)), f_map_46_put_all_2(f_map_46_entries_1(f_ctx_46_Ctx_46_constrs_1(v_b)), f_ctx_46_Ctx_46_constrs_1(v_a)), f_ctx_46_union_named_2(f_map_46_entries_1(f_ctx_46_Ctx_46_named_1(v_b)), f_ctx_46_Ctx_46_named_1(v_a)));
}

u_value f_ctx_46_union_named_2(u_value v_es, u_value v_bs) {
  u_value s1147 = v_es;
  u_value t1148 = NULL;
  if ((!t1148) && u_is_con(s1147, &n_list_46_Nil)) {
    t1148 = v_bs;
  }
  if ((!t1148) && u_is_con(s1147, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s1147, 0), 0);
    u_value v_ns = u_field(u_field(s1147, 0), 1);
    u_value v_rest = u_field(s1147, 1);
    u_value s1149 = f_map_46_get_2(v_k, v_bs);
    u_value t1150 = NULL;
    if ((!t1150) && u_is_con(s1149, &n_option_46_Some)) {
      u_value v_ms = u_field(s1149, 0);
      t1150 = f_ctx_46_union_named_2(v_rest, f_map_46_put_3(v_k, f_list_46__43__43__2(v_ms, v_ns), v_bs));
    }
    if ((!t1150) && u_is_con(s1149, &n_option_46_None)) {
      t1150 = f_ctx_46_union_named_2(v_rest, f_map_46_put_3(v_k, v_ns, v_bs));
    }
    if (!t1150) {
      u_no_case(s1149);
    }
    t1148 = t1150;
  }
  if (!t1148) {
    u_no_case(s1147);
  }
  return t1148;
}

u_value f_ctx_46_alias_of_2(u_value v_c, u_value v_n) {
  return f_map_46_get_2(v_n, f_ctx_46_Ctx_46_aliases_1(v_c));
}

u_value f_ctx_46_schemes_of_2(u_value v_c, u_value v_n) {
  u_value s1151 = f_map_46_get_2(v_n, f_ctx_46_Ctx_46_funs_1(v_c));
  u_value t1152 = NULL;
  if ((!t1152) && u_is_con(s1151, &n_option_46_Some)) {
    u_value v_ss = u_field(s1151, 0);
    t1152 = v_ss;
  }
  if ((!t1152) && u_is_con(s1151, &n_option_46_None)) {
    t1152 = &o_list_46_Nil;
  }
  if (!t1152) {
    u_no_case(s1151);
  }
  return t1152;
}

u_value f_ctx_46_proc_of_2(u_value v_c, u_value v_n) {
  return f_map_46_get_2(v_n, f_ctx_46_Ctx_46_procs_1(v_c));
}

u_value f_ctx_46_data_of_2(u_value v_c, u_value v_n) {
  return f_map_46_get_2(v_n, f_ctx_46_Ctx_46_datas_1(v_c));
}

u_value f_ctx_46_members_of_2(u_value v_c, u_value v_n) {
  return f_map_46_get_2(v_n, f_ctx_46_Ctx_46_members_1(v_c));
}

u_value f_ctx_46_constr_of_2(u_value v_c, u_value v_n) {
  return f_map_46_get_2(v_n, f_ctx_46_Ctx_46_constrs_1(v_c));
}

u_value f_ctx_46_is_fun_2(u_value v_c, u_value v_n) {
  return f_map_46_has_2(v_n, f_ctx_46_Ctx_46_funs_1(v_c));
}

u_value f_ctx_46_is_proc_2(u_value v_c, u_value v_n) {
  return f_map_46_has_2(v_n, f_ctx_46_Ctx_46_procs_1(v_c));
}

u_value f_ctx_46_is_data_2(u_value v_c, u_value v_n) {
  return f_map_46_has_2(v_n, f_ctx_46_Ctx_46_datas_1(v_c));
}

u_value f_ctx_46_is_alias_2(u_value v_c, u_value v_n) {
  return f_map_46_has_2(v_n, f_ctx_46_Ctx_46_aliases_1(v_c));
}

u_value f_ctx_46_is_constr_2(u_value v_c, u_value v_n) {
  return f_map_46_has_2(v_n, f_ctx_46_Ctx_46_constrs_1(v_c));
}

u_value f_ctx_46_is_sort_2(u_value v_c, u_value v_n) {
  u_value t1153 = f_ctx_46_is_data_2(v_c, v_n);
  if (!u_truth(t1153)) {
    t1153 = f_ctx_46_is_alias_2(v_c, v_n);
  }
  return t1153;
}

u_value f_ctx_46_names_of_2(u_value v_c, u_value v_b) {
  u_value s1154 = f_map_46_get_2(v_b, f_ctx_46_Ctx_46_named_1(v_c));
  u_value t1155 = NULL;
  if ((!t1155) && u_is_con(s1154, &n_option_46_Some)) {
    u_value v_ns = u_field(s1154, 0);
    t1155 = v_ns;
  }
  if ((!t1155) && u_is_con(s1154, &n_option_46_None)) {
    t1155 = &o_list_46_Nil;
  }
  if (!t1155) {
    u_no_case(s1154);
  }
  return t1155;
}

u_value f_ctx_46_with_named_2(u_value v_bs, u_value v_n) {
  u_value s1156 = f_map_46_get_2(f_syntax_46_Name_46_base_1(v_n), v_bs);
  u_value t1157 = NULL;
  if ((!t1157) && u_is_con(s1156, &n_option_46_Some)) {
    u_value v_ns = u_field(s1156, 0);
    u_value t1158;
    if (u_truth(f_list_46_contains_2(v_n, v_ns))) {
      t1158 = v_bs;
    } else {
      t1158 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), f_list_46__43__43__2(v_ns, u_mk_con(&n_list_46_Cons, 2, v_n, &o_list_46_Nil)), v_bs);
    }
    t1157 = t1158;
  }
  if ((!t1157) && u_is_con(s1156, &n_option_46_None)) {
    t1157 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, v_n, &o_list_46_Nil), v_bs);
  }
  if (!t1157) {
    u_no_case(s1156);
  }
  return t1157;
}

u_value f_ctx_46_with_alias_4(u_value v_c, u_value v_n, u_value v_ps, u_value v_t) {
  return f_ctx_46_Ctx_46_named_58__61__2(f_ctx_46_Ctx_46_aliases_58__61__2(v_c, f_map_46_put_3(v_n, u_mk_tuple(2, v_ps, v_t), f_ctx_46_Ctx_46_aliases_1(v_c))), f_ctx_46_with_named_2(f_ctx_46_Ctx_46_named_1(v_c), v_n));
}

u_value f_ctx_46_with_fun_3(u_value v_c, u_value v_n, u_value v_sc) {
  return f_ctx_46_Ctx_46_named_58__61__2(f_ctx_46_Ctx_46_funs_58__61__2(v_c, f_map_46_put_3(v_n, f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), u_mk_con(&n_list_46_Cons, 2, v_sc, &o_list_46_Nil)), f_ctx_46_Ctx_46_funs_1(v_c))), f_ctx_46_with_named_2(f_ctx_46_Ctx_46_named_1(v_c), v_n));
}

u_value f_ctx_46_with_proc_3(u_value v_c, u_value v_n, u_value v_g) {
  return f_ctx_46_Ctx_46_named_58__61__2(f_ctx_46_Ctx_46_procs_58__61__2(v_c, f_map_46_put_3(v_n, v_g, f_ctx_46_Ctx_46_procs_1(v_c))), f_ctx_46_with_named_2(f_ctx_46_Ctx_46_named_1(v_c), v_n));
}

u_value f_ctx_46_with_data_3(u_value v_c, u_value v_n, u_value v_arity) {
  return f_ctx_46_Ctx_46_named_58__61__2(f_ctx_46_Ctx_46_datas_58__61__2(v_c, f_map_46_put_3(v_n, v_arity, f_ctx_46_Ctx_46_datas_1(v_c))), f_ctx_46_with_named_2(f_ctx_46_Ctx_46_named_1(v_c), v_n));
}

u_value f_ctx_46_with_members_3(u_value v_c, u_value v_n, u_value v_cs) {
  return f_ctx_46_Ctx_46_members_58__61__2(v_c, f_map_46_put_3(v_n, v_cs, f_ctx_46_Ctx_46_members_1(v_c)));
}

u_value f_ctx_46_with_constr_3(u_value v_c, u_value v_n, u_value v_kc) {
  return f_ctx_46_Ctx_46_named_58__61__2(f_ctx_46_Ctx_46_constrs_58__61__2(v_c, f_map_46_put_3(v_n, v_kc, f_ctx_46_Ctx_46_constrs_1(v_c))), f_ctx_46_with_named_2(f_ctx_46_Ctx_46_named_1(v_c), v_n));
}

u_value f_check_46_State_46_ctx_1(u_value v_x) {
  u_value s1159 = v_x;
  u_value t1160 = NULL;
  if ((!t1160) && u_is_con(s1159, &n_check_46_State)) {
    u_value v_y = u_field(s1159, 0);
    t1160 = v_y;
  }
  if (!t1160) {
    u_no_case(s1159);
  }
  return t1160;
}

u_value f_check_46_State_46_sub_1(u_value v_x) {
  u_value s1161 = v_x;
  u_value t1162 = NULL;
  if ((!t1162) && u_is_con(s1161, &n_check_46_State)) {
    u_value v_y = u_field(s1161, 1);
    t1162 = v_y;
  }
  if (!t1162) {
    u_no_case(s1161);
  }
  return t1162;
}

u_value f_check_46_State_46_count_1(u_value v_x) {
  u_value s1163 = v_x;
  u_value t1164 = NULL;
  if ((!t1164) && u_is_con(s1163, &n_check_46_State)) {
    u_value v_y = u_field(s1163, 2);
    t1164 = v_y;
  }
  if (!t1164) {
    u_no_case(s1163);
  }
  return t1164;
}

u_value f_check_46_State_46_module_1(u_value v_x) {
  u_value s1165 = v_x;
  u_value t1166 = NULL;
  if ((!t1166) && u_is_con(s1165, &n_check_46_State)) {
    u_value v_y = u_field(s1165, 3);
    t1166 = v_y;
  }
  if (!t1166) {
    u_no_case(s1165);
  }
  return t1166;
}

u_value f_check_46_State_46_visible_1(u_value v_x) {
  u_value s1167 = v_x;
  u_value t1168 = NULL;
  if ((!t1168) && u_is_con(s1167, &n_check_46_State)) {
    u_value v_y = u_field(s1167, 4);
    t1168 = v_y;
  }
  if (!t1168) {
    u_no_case(s1167);
  }
  return t1168;
}

u_value f_check_46_State_46_table_1(u_value v_x) {
  u_value s1169 = v_x;
  u_value t1170 = NULL;
  if ((!t1170) && u_is_con(s1169, &n_check_46_State)) {
    u_value v_y = u_field(s1169, 5);
    t1170 = v_y;
  }
  if (!t1170) {
    u_no_case(s1169);
  }
  return t1170;
}

u_value f_check_46_State_46_ctx_58__61__2(u_value v_x, u_value v_y) {
  u_value s1171 = v_x;
  u_value t1172 = NULL;
  if ((!t1172) && u_is_con(s1171, &n_check_46_State)) {
    u_value v_a1 = u_field(s1171, 1);
    u_value v_a2 = u_field(s1171, 2);
    u_value v_a3 = u_field(s1171, 3);
    u_value v_a4 = u_field(s1171, 4);
    u_value v_a5 = u_field(s1171, 5);
    t1172 = u_mk_con(&n_check_46_State, 6, v_y, v_a1, v_a2, v_a3, v_a4, v_a5);
  }
  if (!t1172) {
    u_no_case(s1171);
  }
  return t1172;
}

u_value f_check_46_State_46_sub_58__61__2(u_value v_x, u_value v_y) {
  u_value s1173 = v_x;
  u_value t1174 = NULL;
  if ((!t1174) && u_is_con(s1173, &n_check_46_State)) {
    u_value v_a0 = u_field(s1173, 0);
    u_value v_a2 = u_field(s1173, 2);
    u_value v_a3 = u_field(s1173, 3);
    u_value v_a4 = u_field(s1173, 4);
    u_value v_a5 = u_field(s1173, 5);
    t1174 = u_mk_con(&n_check_46_State, 6, v_a0, v_y, v_a2, v_a3, v_a4, v_a5);
  }
  if (!t1174) {
    u_no_case(s1173);
  }
  return t1174;
}

u_value f_check_46_State_46_count_58__61__2(u_value v_x, u_value v_y) {
  u_value s1175 = v_x;
  u_value t1176 = NULL;
  if ((!t1176) && u_is_con(s1175, &n_check_46_State)) {
    u_value v_a0 = u_field(s1175, 0);
    u_value v_a1 = u_field(s1175, 1);
    u_value v_a3 = u_field(s1175, 3);
    u_value v_a4 = u_field(s1175, 4);
    u_value v_a5 = u_field(s1175, 5);
    t1176 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_y, v_a3, v_a4, v_a5);
  }
  if (!t1176) {
    u_no_case(s1175);
  }
  return t1176;
}

u_value f_check_46_State_46_module_58__61__2(u_value v_x, u_value v_y) {
  u_value s1177 = v_x;
  u_value t1178 = NULL;
  if ((!t1178) && u_is_con(s1177, &n_check_46_State)) {
    u_value v_a0 = u_field(s1177, 0);
    u_value v_a1 = u_field(s1177, 1);
    u_value v_a2 = u_field(s1177, 2);
    u_value v_a4 = u_field(s1177, 4);
    u_value v_a5 = u_field(s1177, 5);
    t1178 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_a2, v_y, v_a4, v_a5);
  }
  if (!t1178) {
    u_no_case(s1177);
  }
  return t1178;
}

u_value f_check_46_State_46_visible_58__61__2(u_value v_x, u_value v_y) {
  u_value s1179 = v_x;
  u_value t1180 = NULL;
  if ((!t1180) && u_is_con(s1179, &n_check_46_State)) {
    u_value v_a0 = u_field(s1179, 0);
    u_value v_a1 = u_field(s1179, 1);
    u_value v_a2 = u_field(s1179, 2);
    u_value v_a3 = u_field(s1179, 3);
    u_value v_a5 = u_field(s1179, 5);
    t1180 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_a2, v_a3, v_y, v_a5);
  }
  if (!t1180) {
    u_no_case(s1179);
  }
  return t1180;
}

u_value f_check_46_State_46_table_58__61__2(u_value v_x, u_value v_y) {
  u_value s1181 = v_x;
  u_value t1182 = NULL;
  if ((!t1182) && u_is_con(s1181, &n_check_46_State)) {
    u_value v_a0 = u_field(s1181, 0);
    u_value v_a1 = u_field(s1181, 1);
    u_value v_a2 = u_field(s1181, 2);
    u_value v_a3 = u_field(s1181, 3);
    u_value v_a4 = u_field(s1181, 4);
    t1182 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_a2, v_a3, v_a4, v_y);
  }
  if (!t1182) {
    u_no_case(s1181);
  }
  return t1182;
}

u_value f_check_46_with_ctx_2(u_value v_s, u_value v_c) {
  return f_check_46_State_46_ctx_58__61__2(v_s, v_c);
}

u_value f_check_46_enter_2(u_value v_s, u_value v_m) {
  return f_check_46_State_46_visible_58__61__2(f_check_46_State_46_module_58__61__2(f_check_46_State_46_count_58__61__2(f_check_46_State_46_sub_58__61__2(v_s, f_map_46_empty_0()), u_mk_int(0)), v_m), f_check_46_sees_2(v_m, f_check_46_State_46_table_1(v_s)));
}

u_value f_check_46_sees_2(u_value v_m, u_value v_vs) {
  u_value s1183 = f_list_46_lookup_2(v_m, v_vs);
  u_value t1184 = NULL;
  if ((!t1184) && u_is_con(s1183, &n_option_46_Some)) {
    u_value v_ms = u_field(s1183, 0);
    t1184 = v_ms;
  }
  if ((!t1184) && u_is_con(s1183, &n_option_46_None)) {
    t1184 = u_mk_con(&n_list_46_Cons, 2, v_m, &o_list_46_Nil);
  }
  if (!t1184) {
    u_no_case(s1183);
  }
  return t1184;
}

u_value f_check_46_start_2(u_value v_main, u_value v_vs) {
  return u_mk_con(&n_check_46_State, 6, f_check_46_initial_0(), f_map_46_empty_0(), u_mk_int(0), v_main, f_check_46_sees_2(v_main, v_vs), v_vs);
}

u_value f_check_46_flatten_2(u_value v_m, u_value v_cs) {
  u_value s1185 = v_cs;
  u_value t1186 = NULL;
  if ((!t1186) && u_is_con(s1185, &n_list_46_Nil)) {
    t1186 = &o_list_46_Nil;
  }
  if ((!t1186) && (u_is_con(s1185, &n_list_46_Cons) && u_is_con(u_field(s1185, 0), &n_syntax_46_CModule))) {
    u_value v_n = u_field(u_field(s1185, 0), 0);
    u_value v_ds = u_field(u_field(s1185, 0), 1);
    u_value v_rest = u_field(s1185, 1);
    t1186 = f_list_46__43__43__2(f_check_46_flatten_2(v_n, v_ds), f_check_46_flatten_2(v_m, v_rest));
  }
  if ((!t1186) && u_is_con(s1185, &n_list_46_Cons)) {
    u_value v_c = u_field(s1185, 0);
    u_value v_rest = u_field(s1185, 1);
    t1186 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_m, f_check_46_qualify_2(v_m, v_c)), f_check_46_flatten_2(v_m, v_rest));
  }
  if (!t1186) {
    u_no_case(s1185);
  }
  return t1186;
}

u_value f_check_46_in_module_2(u_value v_m, u_value v_n) {
  return u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), f_syntax_46_Name_46_base_1(v_n));
}

u_value f_check_46_qualify_2(u_value v_m, u_value v_c) {
  u_value s1187 = v_c;
  u_value t1188 = NULL;
  if ((!t1188) && u_is_con(s1187, &n_syntax_46_CSort)) {
    u_value v_n = u_field(s1187, 0);
    u_value v_ps = u_field(s1187, 1);
    u_value v_t = u_field(s1187, 2);
    t1188 = u_mk_con(&n_syntax_46_CSort, 3, f_check_46_in_module_2(v_m, v_n), v_ps, v_t);
  }
  if ((!t1188) && u_is_con(s1187, &n_syntax_46_CData)) {
    u_value v_n = u_field(s1187, 0);
    u_value v_ps = u_field(s1187, 1);
    u_value v_ks = u_field(s1187, 2);
    u_value t1189 = &o_list_46_Nil;
    u_value *e1190 = &t1189;
    u_value l1191 = v_ks;
    while (u_is_con(l1191, &n_list_46_Cons)) {
      u_value v_k = u_field(l1191, 0);
      if (u_truth(u_mk_bool(1))) {
        e1190 = u_append(e1190, f_check_46_qualify_constr_2(v_m, v_k));
      }
      l1191 = u_field(l1191, 1);
    }
    t1188 = u_mk_con(&n_syntax_46_CData, 3, f_check_46_in_module_2(v_m, v_n), v_ps, t1189);
  }
  if ((!t1188) && u_is_con(s1187, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s1187, 0);
    u_value v_ps = u_field(s1187, 1);
    u_value v_xss = u_field(s1187, 2);
    u_value v_r = u_field(s1187, 3);
    u_value v_b = u_field(s1187, 4);
    t1188 = u_mk_con(&n_syntax_46_CFun, 5, f_check_46_in_module_2(v_m, v_n), v_ps, v_xss, v_r, v_b);
  }
  if ((!t1188) && u_is_con(s1187, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s1187, 0);
    u_value v_ps = u_field(s1187, 1);
    u_value v_ins = u_field(s1187, 2);
    u_value v_outs = u_field(s1187, 3);
    u_value v_refs = u_field(s1187, 4);
    u_value v_b = u_field(s1187, 5);
    t1188 = u_mk_con(&n_syntax_46_CProc, 6, f_check_46_in_module_2(v_m, v_n), v_ps, v_ins, v_outs, v_refs, v_b);
  }
  if (!t1188) {
    t1188 = v_c;
  }
  if (!t1188) {
    u_no_case(s1187);
  }
  return t1188;
}

u_value f_check_46_qualify_constr_2(u_value v_m, u_value v_k) {
  return f_syntax_46_Constr_46_name_58__61__2(v_k, f_check_46_in_module_2(v_m, f_syntax_46_Constr_46_name_1(v_k)));
}

u_value f_check_46_imports_in_2(u_value v_ds, u_value v_m) {
  u_value s1192 = v_ds;
  u_value t1193 = NULL;
  if ((!t1193) && u_is_con(s1192, &n_list_46_Nil)) {
    t1193 = &o_list_46_Nil;
  }
  if ((!t1193) && (u_is_con(s1192, &n_list_46_Cons) && u_is_con(u_field(u_field(s1192, 0), 1), &n_syntax_46_CImport))) {
    u_value v_k = u_field(u_field(s1192, 0), 0);
    u_value v_n = u_field(u_field(u_field(s1192, 0), 1), 0);
    u_value v_rest = u_field(s1192, 1);
    if (u_truth(u_eq(v_k, v_m))) {
      t1193 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_imports_in_2(v_rest, v_m));
    }
  }
  if ((!t1193) && u_is_con(s1192, &n_list_46_Cons)) {
    u_value v_rest = u_field(s1192, 1);
    t1193 = f_check_46_imports_in_2(v_rest, v_m);
  }
  if (!t1193) {
    u_no_case(s1192);
  }
  return t1193;
}

u_value f_check_46_visibility_2(u_value v_imports, u_value v_main) {
  u_value t1194 = &o_list_46_Nil;
  u_value *e1195 = &t1194;
  u_value l1196 = f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_main, &o_list_46_Nil), f_list_46_firsts_1(v_imports)));
  while (u_is_con(l1196, &n_list_46_Cons)) {
    u_value v_m = u_field(l1196, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_prelude_1197 = U_LIT("prelude");
      static struct u_object l_prelude_1198 = U_LIT("prelude");
      e1195 = u_append(e1195, u_mk_tuple(2, v_m, f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_m, &o_list_46_Nil), f_list_46__43__43__2(f_check_46_imports_of_2(v_imports, v_m), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, &l_prelude_1197, &o_list_46_Nil), f_check_46_imports_of_2(v_imports, &l_prelude_1198)))))));
    }
    l1196 = u_field(l1196, 1);
  }
  return t1194;
}

u_value f_check_46_imports_of_2(u_value v_imports, u_value v_m) {
  u_value s1199 = f_list_46_lookup_2(v_m, v_imports);
  u_value t1200 = NULL;
  if ((!t1200) && u_is_con(s1199, &n_option_46_Some)) {
    u_value v_ns = u_field(s1199, 0);
    t1200 = v_ns;
  }
  if ((!t1200) && u_is_con(s1199, &n_option_46_None)) {
    t1200 = &o_list_46_Nil;
  }
  if (!t1200) {
    u_no_case(s1199);
  }
  return t1200;
}

u_value f_check_46_visible_names_3(u_value v_s, u_value v_n, u_value v_declared) {
  u_value t1201 = &o_list_46_Nil;
  u_value *e1202 = &t1201;
  u_value l1203 = f_ctx_46_names_of_2(f_check_46_State_46_ctx_1(v_s), f_syntax_46_Name_46_base_1(v_n));
  while (u_is_con(l1203, &n_list_46_Cons)) {
    u_value v_d = u_field(l1203, 0);
    if (u_truth(u_apply(v_declared, 1, v_d))) {
      e1202 = u_append(e1202, v_d);
    }
    l1203 = u_field(l1203, 1);
  }
  u_value t1212 = t1201;
  u_value t1211;
  {
    u_value v_ds = t1212;
    u_value s1204 = v_n;
    u_value t1205 = NULL;
    if ((!t1205) && (u_is_con(s1204, &n_syntax_46_Name) && u_is_con(u_field(s1204, 0), &n_option_46_Some))) {
      u_value t1206;
      if (u_truth(f_list_46_contains_2(v_n, v_ds))) {
        t1206 = u_mk_con(&n_list_46_Cons, 2, v_n, &o_list_46_Nil);
      } else {
        t1206 = &o_list_46_Nil;
      }
      t1205 = t1206;
    }
    if ((!t1205) && (u_is_con(s1204, &n_syntax_46_Name) && u_is_con(u_field(s1204, 0), &n_option_46_None))) {
      u_value v_b = u_field(s1204, 1);
      u_value t1210;
      if (u_truth(f_list_46_contains_2(u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), v_ds))) {
        t1210 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), &o_list_46_Nil);
      } else {
        u_value t1207 = &o_list_46_Nil;
        u_value *e1208 = &t1207;
        u_value l1209 = v_ds;
        while (u_is_con(l1209, &n_list_46_Cons)) {
          u_value v_d = u_field(l1209, 0);
          if (u_truth(f_check_46_is_visible_2(v_s, v_d))) {
            e1208 = u_append(e1208, v_d);
          }
          l1209 = u_field(l1209, 1);
        }
        t1210 = t1207;
      }
      t1205 = t1210;
    }
    if (!t1205) {
      u_no_case(s1204);
    }
    t1211 = t1205;
  }
  return t1211;
}

u_value f_check_46_is_visible_2(u_value v_s, u_value v_d) {
  u_value s1213 = f_syntax_46_Name_46_module_1(v_d);
  u_value t1214 = NULL;
  if ((!t1214) && u_is_con(s1213, &n_option_46_Some)) {
    u_value v_m = u_field(s1213, 0);
    t1214 = f_list_46_contains_2(v_m, f_check_46_State_46_visible_1(v_s));
  }
  if ((!t1214) && u_is_con(s1213, &n_option_46_None)) {
    t1214 = u_mk_bool(0);
  }
  if (!t1214) {
    u_no_case(s1213);
  }
  return t1214;
}

u_value f_check_46_tbool_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of check.tbool");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_Bool_1215 = U_LIT("Bool");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Bool_1215), &o_list_46_Nil);
  return cache;
}

u_value f_check_46_tint_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of check.tint");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_Int_1216 = U_LIT("Int");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Int_1216), &o_list_46_Nil);
  return cache;
}

u_value f_check_46_tstr_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of check.tstr");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_Str_1217 = U_LIT("Str");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Str_1217), &o_list_46_Nil);
  return cache;
}

u_value f_check_46_tchar_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of check.tchar");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_Char_1218 = U_LIT("Char");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Char_1218), &o_list_46_Nil);
  return cache;
}

u_value f_check_46_tlist_1(u_value v_t) {
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_list_name_0(), u_mk_con(&n_list_46_Cons, 2, v_t, &o_list_46_Nil));
}

u_value f_check_46_fun_2(u_value v_args, u_value v_res) {
  return u_mk_con(&n_syntax_46_TFun, 2, v_args, v_res);
}

u_value f_check_46_params_1(u_value v_ps) {
  u_value t1219 = &o_list_46_Nil;
  u_value *e1220 = &t1219;
  u_value l1221 = v_ps;
  while (u_is_con(l1221, &n_list_46_Cons)) {
    u_value v_p = u_field(l1221, 0);
    if (u_truth(u_mk_bool(1))) {
      e1220 = u_append(e1220, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1221 = u_field(l1221, 1);
  }
  return t1219;
}

u_value f_check_46_basic_1(u_value v_n) {
  u_value s1222 = v_n;
  u_value t1223 = NULL;
  if ((!t1223) && (u_is_con(s1222, &n_syntax_46_Name) && u_is_con(u_field(s1222, 0), &n_option_46_None))) {
    u_value v_b = u_field(s1222, 1);
    static struct u_object l_Bool_1224 = U_LIT("Bool");
    static struct u_object l_Int_1225 = U_LIT("Int");
    static struct u_object l_Str_1226 = U_LIT("Str");
    static struct u_object l_Char_1227 = U_LIT("Char");
    t1223 = f_list_46_contains_2(v_b, u_mk_con(&n_list_46_Cons, 2, &l_Bool_1224, u_mk_con(&n_list_46_Cons, 2, &l_Int_1225, u_mk_con(&n_list_46_Cons, 2, &l_Str_1226, u_mk_con(&n_list_46_Cons, 2, &l_Char_1227, &o_list_46_Nil)))));
  }
  if (!t1223) {
    t1223 = u_mk_bool(0);
  }
  if (!t1223) {
    u_no_case(s1222);
  }
  return t1223;
}

u_value f_check_46_literal_1(u_value v_v) {
  u_value s1228 = v_v;
  u_value t1229 = NULL;
  if ((!t1229) && u_is_con(s1228, &n_syntax_46_VUnit)) {
    t1229 = u_mk_con(&n_syntax_46_TTuple, 1, &o_list_46_Nil);
  }
  if ((!t1229) && u_is_con(s1228, &n_syntax_46_VBool)) {
    t1229 = f_check_46_tbool_0();
  }
  if ((!t1229) && u_is_con(s1228, &n_syntax_46_VInt)) {
    t1229 = f_check_46_tint_0();
  }
  if ((!t1229) && u_is_con(s1228, &n_syntax_46_VStr)) {
    t1229 = f_check_46_tstr_0();
  }
  if ((!t1229) && u_is_con(s1228, &n_syntax_46_VChar)) {
    t1229 = f_check_46_tchar_0();
  }
  if (!t1229) {
    u_no_case(s1228);
  }
  return t1229;
}

u_value f_check_46_initial_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of check.initial");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  u_pop(handler);
  cache = f_ctx_46_empty_ctx_0();
  return cache;
}

u_value f_check_46_arith_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of check.arith");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  u_pop(handler);
  cache = u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), &o_list_46_Nil)), f_check_46_tint_0()));
  return cache;
}

u_value f_check_46_logic_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of check.logic");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  u_pop(handler);
  cache = u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tbool_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tbool_0(), &o_list_46_Nil)), f_check_46_tbool_0()));
  return cache;
}

u_value f_check_46_builtin_1(u_value v_n) {
  u_value s1230 = v_n;
  u_value t1231 = NULL;
  if ((!t1231) && (u_is_con(s1230, &n_syntax_46_Name) && u_is_con(u_field(s1230, 0), &n_option_46_None))) {
    u_value v_b = u_field(s1230, 1);
    t1231 = f_check_46_builtin_scheme_1(v_b);
  }
  if (!t1231) {
    t1231 = &o_list_46_Nil;
  }
  if (!t1231) {
    u_no_case(s1230);
  }
  return t1231;
}

u_value f_check_46_builtin_scheme_1(u_value v_b) {
  static struct u_object l_A_1232 = U_LIT("A");
  u_value t1236 = u_mk_con(&n_syntax_46_TParam, 1, &l_A_1232);
  u_value t1235;
  {
    u_value v_a = t1236;
    u_value s1233 = v_b;
    u_value t1234 = NULL;
    if ((!t1234) && u_equals(s1233, u_mk_lit("+"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("-"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("*"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("/"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("%"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("<"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), &o_list_46_Nil)), f_check_46_tbool_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("=="))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, &o_list_46_Nil), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, &o_list_46_Nil)), f_check_46_tbool_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("compare"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, &o_list_46_Nil), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, &o_list_46_Nil)), f_check_46_tint_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("and"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("or"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("chars"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), &o_list_46_Nil), f_check_46_tlist_1(f_check_46_tchar_0()))), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("string"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tchar_0()), &o_list_46_Nil), f_check_46_tstr_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("ord"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), &o_list_46_Nil), f_check_46_tint_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("chr"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), &o_list_46_Nil), f_check_46_tchar_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("^"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), &o_list_46_Nil)), f_check_46_tstr_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("str_size"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), &o_list_46_Nil), f_check_46_tint_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("str_join"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), &o_list_46_Nil)), f_check_46_tstr_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("str_slice"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), &o_list_46_Nil))), f_check_46_tstr_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("str_split"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), &o_list_46_Nil)), f_check_46_tlist_1(f_check_46_tstr_0()))), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("str_get"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), &o_list_46_Nil)), f_check_46_tchar_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("str_contains"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), &o_list_46_Nil)), f_check_46_tbool_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("str_compare"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), &o_list_46_Nil)), f_check_46_tint_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("str_show"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), &o_list_46_Nil), f_check_46_tstr_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("str_quote"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), &o_list_46_Nil), f_check_46_tstr_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("str_number"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), &o_list_46_Nil), u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_option_name_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), &o_list_46_Nil)))), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("is_digit"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), &o_list_46_Nil), f_check_46_tbool_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("is_letter"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), &o_list_46_Nil), f_check_46_tbool_0())), &o_list_46_Nil);
    }
    if ((!t1234) && u_equals(s1233, u_mk_lit("is_space"))) {
      t1234 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_list_46_Nil, f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), &o_list_46_Nil), f_check_46_tbool_0())), &o_list_46_Nil);
    }
    if (!t1234) {
      t1234 = &o_list_46_Nil;
    }
    if (!t1234) {
      u_no_case(s1233);
    }
    t1235 = t1234;
  }
  return t1235;
}

u_value f_check_46_is_meta_1(u_value v_t) {
  u_value s1237 = v_t;
  u_value t1238 = NULL;
  if ((!t1238) && u_is_con(s1237, &n_syntax_46_TMeta)) {
    t1238 = u_mk_bool(1);
  }
  if (!t1238) {
    t1238 = u_mk_bool(0);
  }
  if (!t1238) {
    u_no_case(s1237);
  }
  return t1238;
}

u_value f_check_46_free_1(u_value v_t) {
  u_value s1239 = v_t;
  u_value t1240 = NULL;
  if ((!t1240) && u_is_con(s1239, &n_syntax_46_TParam)) {
    t1240 = u_mk_con(&n_list_46_Cons, 2, v_t, &o_list_46_Nil);
  }
  if ((!t1240) && u_is_con(s1239, &n_syntax_46_TMeta)) {
    t1240 = u_mk_con(&n_list_46_Cons, 2, v_t, &o_list_46_Nil);
  }
  if ((!t1240) && u_is_con(s1239, &n_syntax_46_THole)) {
    t1240 = &o_list_46_Nil;
  }
  if ((!t1240) && u_is_con(s1239, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s1239, 1);
    u_value t1241 = &o_list_46_Nil;
    u_value *e1242 = &t1241;
    u_value l1243 = v_args;
    while (u_is_con(l1243, &n_list_46_Cons)) {
      u_value v_a = u_field(l1243, 0);
      if (u_truth(u_mk_bool(1))) {
        e1242 = u_append(e1242, f_check_46_free_1(v_a));
      }
      l1243 = u_field(l1243, 1);
    }
    t1240 = f_list_46_distinct_1(f_list_46_concat_1(t1241));
  }
  if ((!t1240) && u_is_con(s1239, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1239, 0);
    u_value v_r = u_field(s1239, 1);
    u_value t1244 = &o_list_46_Nil;
    u_value *e1245 = &t1244;
    u_value l1246 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, &o_list_46_Nil));
    while (u_is_con(l1246, &n_list_46_Cons)) {
      u_value v_a = u_field(l1246, 0);
      if (u_truth(u_mk_bool(1))) {
        e1245 = u_append(e1245, f_check_46_free_1(v_a));
      }
      l1246 = u_field(l1246, 1);
    }
    t1240 = f_list_46_distinct_1(f_list_46_concat_1(t1244));
  }
  if ((!t1240) && u_is_con(s1239, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s1239, 0);
    u_value v_os = u_field(s1239, 1);
    u_value v_rs = u_field(s1239, 2);
    u_value t1247 = &o_list_46_Nil;
    u_value *e1248 = &t1247;
    u_value l1249 = f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs));
    while (u_is_con(l1249, &n_list_46_Cons)) {
      u_value v_a = u_field(l1249, 0);
      if (u_truth(u_mk_bool(1))) {
        e1248 = u_append(e1248, f_check_46_free_1(v_a));
      }
      l1249 = u_field(l1249, 1);
    }
    t1240 = f_list_46_distinct_1(f_list_46_concat_1(t1247));
  }
  if ((!t1240) && u_is_con(s1239, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1239, 0);
    u_value t1250 = &o_list_46_Nil;
    u_value *e1251 = &t1250;
    u_value l1252 = v_ts;
    while (u_is_con(l1252, &n_list_46_Cons)) {
      u_value v_a = u_field(l1252, 0);
      if (u_truth(u_mk_bool(1))) {
        e1251 = u_append(e1251, f_check_46_free_1(v_a));
      }
      l1252 = u_field(l1252, 1);
    }
    t1240 = f_list_46_distinct_1(f_list_46_concat_1(t1250));
  }
  if (!t1240) {
    u_no_case(s1239);
  }
  return t1240;
}

u_value f_check_46_holes_1(u_value v_t) {
  u_value s1253 = v_t;
  u_value t1254 = NULL;
  if ((!t1254) && u_is_con(s1253, &n_syntax_46_THole)) {
    t1254 = u_mk_bool(1);
  }
  if ((!t1254) && u_is_con(s1253, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s1253, 1);
    t1254 = f_list_46_exists_2(v_args, u_mk_closure(w_f_check_46_holes_1, 0));
  }
  if ((!t1254) && u_is_con(s1253, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1253, 0);
    u_value v_r = u_field(s1253, 1);
    u_value t1255 = f_list_46_exists_2(v_args, u_mk_closure(w_f_check_46_holes_1, 0));
    if (!u_truth(t1255)) {
      t1255 = f_check_46_holes_1(v_r);
    }
    t1254 = t1255;
  }
  if ((!t1254) && u_is_con(s1253, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s1253, 0);
    u_value v_os = u_field(s1253, 1);
    u_value v_rs = u_field(s1253, 2);
    t1254 = f_list_46_exists_2(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs)), u_mk_closure(w_f_check_46_holes_1, 0));
  }
  if ((!t1254) && u_is_con(s1253, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1253, 0);
    t1254 = f_list_46_exists_2(v_ts, u_mk_closure(w_f_check_46_holes_1, 0));
  }
  if (!t1254) {
    t1254 = u_mk_bool(0);
  }
  if (!t1254) {
    u_no_case(s1253);
  }
  return t1254;
}

u_value f_check_46_subst_2(u_value v_t, u_value v_m) {
  u_value s1256 = v_t;
  u_value t1257 = NULL;
  if ((!t1257) && u_is_con(s1256, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s1256, 0);
    u_value v_args = u_field(s1256, 1);
    u_value t1258 = &o_list_46_Nil;
    u_value *e1259 = &t1258;
    u_value l1260 = v_args;
    while (u_is_con(l1260, &n_list_46_Cons)) {
      u_value v_a = u_field(l1260, 0);
      if (u_truth(u_mk_bool(1))) {
        e1259 = u_append(e1259, f_check_46_subst_2(v_a, v_m));
      }
      l1260 = u_field(l1260, 1);
    }
    t1257 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t1258);
  }
  if ((!t1257) && u_is_con(s1256, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1256, 0);
    u_value v_r = u_field(s1256, 1);
    u_value t1261 = &o_list_46_Nil;
    u_value *e1262 = &t1261;
    u_value l1263 = v_args;
    while (u_is_con(l1263, &n_list_46_Cons)) {
      u_value v_a = u_field(l1263, 0);
      if (u_truth(u_mk_bool(1))) {
        e1262 = u_append(e1262, f_check_46_subst_2(v_a, v_m));
      }
      l1263 = u_field(l1263, 1);
    }
    t1257 = u_mk_con(&n_syntax_46_TFun, 2, t1261, f_check_46_subst_2(v_r, v_m));
  }
  if ((!t1257) && u_is_con(s1256, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s1256, 0);
    u_value v_os = u_field(s1256, 1);
    u_value v_rs = u_field(s1256, 2);
    u_value t1264 = &o_list_46_Nil;
    u_value *e1265 = &t1264;
    u_value l1266 = v_is;
    while (u_is_con(l1266, &n_list_46_Cons)) {
      u_value v_a = u_field(l1266, 0);
      if (u_truth(u_mk_bool(1))) {
        e1265 = u_append(e1265, f_check_46_subst_2(v_a, v_m));
      }
      l1266 = u_field(l1266, 1);
    }
    u_value t1267 = &o_list_46_Nil;
    u_value *e1268 = &t1267;
    u_value l1269 = v_os;
    while (u_is_con(l1269, &n_list_46_Cons)) {
      u_value v_a = u_field(l1269, 0);
      if (u_truth(u_mk_bool(1))) {
        e1268 = u_append(e1268, f_check_46_subst_2(v_a, v_m));
      }
      l1269 = u_field(l1269, 1);
    }
    u_value t1270 = &o_list_46_Nil;
    u_value *e1271 = &t1270;
    u_value l1272 = v_rs;
    while (u_is_con(l1272, &n_list_46_Cons)) {
      u_value v_a = u_field(l1272, 0);
      if (u_truth(u_mk_bool(1))) {
        e1271 = u_append(e1271, f_check_46_subst_2(v_a, v_m));
      }
      l1272 = u_field(l1272, 1);
    }
    t1257 = u_mk_con(&n_syntax_46_TProc, 3, t1264, t1267, t1270);
  }
  if ((!t1257) && u_is_con(s1256, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1256, 0);
    u_value t1273 = &o_list_46_Nil;
    u_value *e1274 = &t1273;
    u_value l1275 = v_ts;
    while (u_is_con(l1275, &n_list_46_Cons)) {
      u_value v_a = u_field(l1275, 0);
      if (u_truth(u_mk_bool(1))) {
        e1274 = u_append(e1274, f_check_46_subst_2(v_a, v_m));
      }
      l1275 = u_field(l1275, 1);
    }
    t1257 = u_mk_con(&n_syntax_46_TTuple, 1, t1273);
  }
  if (!t1257) {
    u_value s1276 = f_list_46_lookup_2(v_t, v_m);
    u_value t1277 = NULL;
    if ((!t1277) && u_is_con(s1276, &n_option_46_Some)) {
      u_value v_u = u_field(s1276, 0);
      t1277 = v_u;
    }
    if ((!t1277) && u_is_con(s1276, &n_option_46_None)) {
      t1277 = v_t;
    }
    if (!t1277) {
      u_no_case(s1276);
    }
    t1257 = t1277;
  }
  if (!t1257) {
    u_no_case(s1256);
  }
  return t1257;
}

u_value f_check_46_resolve_2(u_value v_su, u_value v_t) {
  u_value s1278 = v_t;
  u_value t1279 = NULL;
  if ((!t1279) && u_is_con(s1278, &n_syntax_46_TMeta)) {
    u_value v_i = u_field(s1278, 0);
    u_value s1280 = f_map_46_get_2(v_i, v_su);
    u_value t1281 = NULL;
    if ((!t1281) && u_is_con(s1280, &n_option_46_Some)) {
      u_value v_u = u_field(s1280, 0);
      t1281 = f_check_46_resolve_2(v_su, v_u);
    }
    if ((!t1281) && u_is_con(s1280, &n_option_46_None)) {
      t1281 = v_t;
    }
    if (!t1281) {
      u_no_case(s1280);
    }
    t1279 = t1281;
  }
  if (!t1279) {
    t1279 = v_t;
  }
  if (!t1279) {
    u_no_case(s1278);
  }
  return t1279;
}

u_value f_check_46_occurs_3(u_value v_su, u_value v_i, u_value v_t) {
  u_value s1282 = f_check_46_resolve_2(v_su, v_t);
  u_value t1283 = NULL;
  if ((!t1283) && u_is_con(s1282, &n_syntax_46_TMeta)) {
    u_value v_j = u_field(s1282, 0);
    t1283 = u_eq(v_i, v_j);
  }
  if ((!t1283) && u_is_con(s1282, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s1282, 1);
    t1283 = f_list_46_exists_2(v_args, u_mk_closure(lam1284, 2, v_su, v_i));
  }
  if ((!t1283) && u_is_con(s1282, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1282, 0);
    u_value v_r = u_field(s1282, 1);
    u_value t1286 = f_list_46_exists_2(v_args, u_mk_closure(lam1285, 2, v_su, v_i));
    if (!u_truth(t1286)) {
      t1286 = f_check_46_occurs_3(v_su, v_i, v_r);
    }
    t1283 = t1286;
  }
  if ((!t1283) && u_is_con(s1282, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s1282, 0);
    u_value v_os = u_field(s1282, 1);
    u_value v_rs = u_field(s1282, 2);
    t1283 = f_list_46_exists_2(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs)), u_mk_closure(lam1287, 2, v_su, v_i));
  }
  if ((!t1283) && u_is_con(s1282, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1282, 0);
    t1283 = f_list_46_exists_2(v_ts, u_mk_closure(lam1288, 2, v_su, v_i));
  }
  if (!t1283) {
    t1283 = u_mk_bool(0);
  }
  if (!t1283) {
    u_no_case(s1282);
  }
  return t1283;
}

u_value f_check_46_constructor_scheme_1(u_value v_k) {
  u_value s1289 = v_k;
  u_value t1290 = NULL;
  if (!t1290) {
    u_value v_ps = u_field(s1289, 0);
    u_value v_ks = u_field(s1289, 1);
    u_value v_r = u_field(s1289, 2);
    u_value t1291;
    if (u_truth(u_eq(v_ks, &o_list_46_Nil))) {
      t1291 = v_r;
    } else {
      t1291 = f_check_46_fun_2(v_ks, v_r);
    }
    t1290 = u_mk_tuple(2, v_ps, t1291);
  }
  if (!t1290) {
    u_no_case(s1289);
  }
  return t1290;
}

u_value f_check_46_constructor_of_2(u_value v_c, u_value v_k) {
  u_value s1292 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t1293 = NULL;
  if ((!t1293) && u_is_con(s1292, &n_option_46_Some)) {
    u_value v_kc = u_field(s1292, 0);
    t1293 = f_check_46_constructor_scheme_1(v_kc);
  }
  if ((!t1293) && u_is_con(s1292, &n_option_46_None)) {
    t1293 = u_mk_tuple(2, &o_list_46_Nil, u_mk_con(&n_syntax_46_TTuple, 1, &o_list_46_Nil));
  }
  if (!t1293) {
    u_no_case(s1292);
  }
  return t1293;
}

u_value f_check_46_candidates_3(u_value v_s, u_value v_env, u_value v_n) {
  u_value s1294 = f_list_46_lookup_2(v_n, v_env);
  u_value t1295 = NULL;
  if ((!t1295) && u_is_con(s1294, &n_option_46_Some)) {
    u_value v_sc = u_field(s1294, 0);
    t1295 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, v_sc), &o_list_46_Nil);
  }
  if ((!t1295) && u_is_con(s1294, &n_option_46_None)) {
    u_value t1296 = &o_list_46_Nil;
    u_value *e1297 = &t1296;
    u_value l1299 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam1298, 1, v_s));
    while (u_is_con(l1299, &n_list_46_Cons)) {
      u_value v_f = u_field(l1299, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1300 = &o_list_46_Nil;
        u_value *e1301 = &t1300;
        u_value l1302 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(v_s), v_f);
        while (u_is_con(l1302, &n_list_46_Cons)) {
          u_value v_sc = u_field(l1302, 0);
          if (u_truth(u_mk_bool(1))) {
            e1301 = u_append(e1301, u_mk_tuple(2, v_f, v_sc));
          }
          l1302 = u_field(l1302, 1);
        }
        e1297 = u_append(e1297, t1300);
      }
      l1299 = u_field(l1299, 1);
    }
    u_value t1303 = &o_list_46_Nil;
    u_value *e1304 = &t1303;
    u_value l1306 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam1305, 1, v_s));
    while (u_is_con(l1306, &n_list_46_Cons)) {
      u_value v_k = u_field(l1306, 0);
      if (u_truth(u_mk_bool(1))) {
        e1304 = u_append(e1304, u_mk_tuple(2, v_k, f_check_46_constructor_of_2(f_check_46_State_46_ctx_1(v_s), v_k)));
      }
      l1306 = u_field(l1306, 1);
    }
    u_value t1307 = &o_list_46_Nil;
    u_value *e1308 = &t1307;
    u_value l1310 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam1309, 1, v_s));
    while (u_is_con(l1310, &n_list_46_Cons)) {
      u_value v_p = u_field(l1310, 0);
      if (u_truth(u_mk_bool(1))) {
        e1308 = u_append(e1308, u_mk_tuple(2, v_p, f_check_46_proc_scheme_2(f_check_46_State_46_ctx_1(v_s), v_p)));
      }
      l1310 = u_field(l1310, 1);
    }
    u_value t1311 = &o_list_46_Nil;
    u_value *e1312 = &t1311;
    u_value l1313 = f_check_46_builtin_1(v_n);
    while (u_is_con(l1313, &n_list_46_Cons)) {
      u_value v_sc = u_field(l1313, 0);
      if (u_truth(u_mk_bool(1))) {
        e1312 = u_append(e1312, u_mk_tuple(2, v_n, v_sc));
      }
      l1313 = u_field(l1313, 1);
    }
    t1295 = f_list_46__43__43__2(f_list_46_concat_1(t1296), f_list_46__43__43__2(t1303, f_list_46__43__43__2(t1307, t1311)));
  }
  if (!t1295) {
    u_no_case(s1294);
  }
  return t1295;
}

u_value f_check_46_proc_scheme_2(u_value v_c, u_value v_p) {
  u_value s1314 = f_ctx_46_proc_of_2(v_c, v_p);
  u_value t1315 = NULL;
  if ((!t1315) && u_is_con(s1314, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s1314, 0), 0);
    u_value v_is = u_field(u_field(s1314, 0), 1);
    u_value v_os = u_field(u_field(s1314, 0), 2);
    u_value v_rs = u_field(u_field(s1314, 0), 3);
    t1315 = u_mk_tuple(2, v_ps, u_mk_con(&n_syntax_46_TProc, 3, v_is, v_os, v_rs));
  }
  if ((!t1315) && u_is_con(s1314, &n_option_46_None)) {
    t1315 = u_mk_tuple(2, &o_list_46_Nil, &o_syntax_46_THole);
  }
  if (!t1315) {
    u_no_case(s1314);
  }
  return t1315;
}

u_value f_check_46_with_arity_2(u_value v_cs, u_value v_k) {
  u_value s1316 = v_cs;
  u_value t1317 = NULL;
  if ((!t1317) && u_is_con(s1316, &n_list_46_Nil)) {
    t1317 = &o_list_46_Nil;
  }
  if ((!t1317) && u_is_con(s1316, &n_list_46_Cons)) {
    u_value v_n = u_field(u_field(s1316, 0), 0);
    u_value v_ps = u_field(u_field(u_field(s1316, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s1316, 0), 1), 1);
    u_value v_rest = u_field(s1316, 1);
    u_value t1318;
    if (u_truth(u_eq(f_syntax_46_arity_1(v_t), v_k))) {
      t1318 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, v_ps, v_t)), f_check_46_with_arity_2(v_rest, v_k));
    } else {
      t1318 = f_check_46_with_arity_2(v_rest, v_k);
    }
    t1317 = t1318;
  }
  if (!t1317) {
    u_no_case(s1316);
  }
  return t1317;
}

u_value f_check_46_has_arity_2(u_value v_scs, u_value v_k) {
  u_value s1319 = v_scs;
  u_value t1320 = NULL;
  if ((!t1320) && u_is_con(s1319, &n_list_46_Nil)) {
    t1320 = u_mk_bool(0);
  }
  if ((!t1320) && u_is_con(s1319, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1319, 0), 1);
    u_value v_rest = u_field(s1319, 1);
    u_value t1321 = u_eq(f_syntax_46_arity_1(v_t), v_k);
    if (!u_truth(t1321)) {
      t1321 = f_check_46_has_arity_2(v_rest, v_k);
    }
    t1320 = t1321;
  }
  if (!t1320) {
    u_no_case(s1319);
  }
  return t1320;
}

u_value f_check_46_bind_1(u_value v_xs) {
  u_value t1322 = &o_list_46_Nil;
  u_value *e1323 = &t1322;
  u_value l1324 = v_xs;
  while (u_is_con(l1324, &n_list_46_Cons)) {
    u_value v_x = u_field(l1324, 0);
    if (u_truth(u_mk_bool(1))) {
      e1323 = u_append(e1323, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), u_mk_tuple(2, &o_list_46_Nil, f_syntax_46_Var_46_typ_1(v_x))));
    }
    l1324 = u_field(l1324, 1);
  }
  return t1322;
}

u_value f_check_46_fixed_2(u_value v_su, u_value v_env) {
  u_value s1325 = v_env;
  u_value t1326 = NULL;
  if ((!t1326) && u_is_con(s1325, &n_list_46_Nil)) {
    t1326 = &o_list_46_Nil;
  }
  if ((!t1326) && u_is_con(s1325, &n_list_46_Cons)) {
    u_value v_ps = u_field(u_field(u_field(s1325, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s1325, 0), 1), 1);
    u_value v_rest = u_field(s1325, 1);
    t1326 = f_list_46__43__43__2(f_list_46_diff_2(f_check_46_free_1(f_check_46_zonk_type_2(v_su, v_t)), v_ps), f_check_46_fixed_2(v_su, v_rest));
  }
  if (!t1326) {
    u_no_case(s1325);
  }
  return t1326;
}

u_value f_check_46_generalize_3(u_value v_su, u_value v_env, u_value v_t) {
  u_value t1332 = f_check_46_fixed_2(v_su, v_env);
  u_value t1333 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t1331;
  {
    u_value v_fx = t1332;
    u_value v_u = t1333;
    u_value t1327 = &o_list_46_Nil;
    u_value *e1328 = &t1327;
    u_value l1329 = f_check_46_free_1(v_u);
    while (u_is_con(l1329, &n_list_46_Cons)) {
      u_value v_m = u_field(l1329, 0);
      u_value t1330 = f_check_46_is_meta_1(v_m);
      if (u_truth(t1330)) {
        t1330 = f_prelude_46_not_1(f_list_46_contains_2(v_m, v_fx));
      }
      if (u_truth(t1330)) {
        e1328 = u_append(e1328, v_m);
      }
      l1329 = u_field(l1329, 1);
    }
    t1331 = u_mk_tuple(2, t1327, v_u);
  }
  return t1331;
}

u_value f_check_46_generalized_3(u_value v_su, u_value v_env, u_value v_xs) {
  u_value t1334 = &o_list_46_Nil;
  u_value *e1335 = &t1334;
  u_value l1336 = v_xs;
  while (u_is_con(l1336, &n_list_46_Cons)) {
    u_value v_x = u_field(l1336, 0);
    if (u_truth(u_mk_bool(1))) {
      e1335 = u_append(e1335, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), f_check_46_generalize_3(v_su, v_env, f_syntax_46_Var_46_typ_1(v_x))));
    }
    l1336 = u_field(l1336, 1);
  }
  return t1334;
}

u_value f_check_46_case_of_1(u_value v_b) {
  u_value s1337 = v_b;
  u_value t1338 = NULL;
  if ((!t1338) && u_is_con(s1337, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s1337, 0);
    u_value v_g = u_field(s1337, 1);
    t1338 = u_mk_tuple(2, v_p, u_eq(v_g, &o_option_46_None));
  }
  if (!t1338) {
    u_no_case(s1337);
  }
  return t1338;
}

u_value f_check_46_zonk_guard_2(u_value v_su, u_value v_g) {
  u_value s1339 = v_g;
  u_value t1340 = NULL;
  if ((!t1340) && u_is_con(s1339, &n_option_46_Some)) {
    u_value v_e = u_field(s1339, 0);
    t1340 = u_mk_con(&n_option_46_Some, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t1340) && u_is_con(s1339, &n_option_46_None)) {
    t1340 = &o_option_46_None;
  }
  if (!t1340) {
    u_no_case(s1339);
  }
  return t1340;
}

u_value f_check_46_zonk_type_2(u_value v_su, u_value v_t) {
  u_value s1341 = f_check_46_resolve_2(v_su, v_t);
  u_value t1342 = NULL;
  if ((!t1342) && u_is_con(s1341, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s1341, 0);
    u_value v_args = u_field(s1341, 1);
    u_value t1343 = &o_list_46_Nil;
    u_value *e1344 = &t1343;
    u_value l1345 = v_args;
    while (u_is_con(l1345, &n_list_46_Cons)) {
      u_value v_a = u_field(l1345, 0);
      if (u_truth(u_mk_bool(1))) {
        e1344 = u_append(e1344, f_check_46_zonk_type_2(v_su, v_a));
      }
      l1345 = u_field(l1345, 1);
    }
    t1342 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t1343);
  }
  if ((!t1342) && u_is_con(s1341, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1341, 0);
    u_value v_r = u_field(s1341, 1);
    u_value t1346 = &o_list_46_Nil;
    u_value *e1347 = &t1346;
    u_value l1348 = v_args;
    while (u_is_con(l1348, &n_list_46_Cons)) {
      u_value v_a = u_field(l1348, 0);
      if (u_truth(u_mk_bool(1))) {
        e1347 = u_append(e1347, f_check_46_zonk_type_2(v_su, v_a));
      }
      l1348 = u_field(l1348, 1);
    }
    t1342 = u_mk_con(&n_syntax_46_TFun, 2, t1346, f_check_46_zonk_type_2(v_su, v_r));
  }
  if ((!t1342) && u_is_con(s1341, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s1341, 0);
    u_value v_os = u_field(s1341, 1);
    u_value v_rs = u_field(s1341, 2);
    u_value t1349 = &o_list_46_Nil;
    u_value *e1350 = &t1349;
    u_value l1351 = v_is;
    while (u_is_con(l1351, &n_list_46_Cons)) {
      u_value v_a = u_field(l1351, 0);
      if (u_truth(u_mk_bool(1))) {
        e1350 = u_append(e1350, f_check_46_zonk_type_2(v_su, v_a));
      }
      l1351 = u_field(l1351, 1);
    }
    u_value t1352 = &o_list_46_Nil;
    u_value *e1353 = &t1352;
    u_value l1354 = v_os;
    while (u_is_con(l1354, &n_list_46_Cons)) {
      u_value v_a = u_field(l1354, 0);
      if (u_truth(u_mk_bool(1))) {
        e1353 = u_append(e1353, f_check_46_zonk_type_2(v_su, v_a));
      }
      l1354 = u_field(l1354, 1);
    }
    u_value t1355 = &o_list_46_Nil;
    u_value *e1356 = &t1355;
    u_value l1357 = v_rs;
    while (u_is_con(l1357, &n_list_46_Cons)) {
      u_value v_a = u_field(l1357, 0);
      if (u_truth(u_mk_bool(1))) {
        e1356 = u_append(e1356, f_check_46_zonk_type_2(v_su, v_a));
      }
      l1357 = u_field(l1357, 1);
    }
    t1342 = u_mk_con(&n_syntax_46_TProc, 3, t1349, t1352, t1355);
  }
  if ((!t1342) && u_is_con(s1341, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1341, 0);
    u_value t1358 = &o_list_46_Nil;
    u_value *e1359 = &t1358;
    u_value l1360 = v_ts;
    while (u_is_con(l1360, &n_list_46_Cons)) {
      u_value v_a = u_field(l1360, 0);
      if (u_truth(u_mk_bool(1))) {
        e1359 = u_append(e1359, f_check_46_zonk_type_2(v_su, v_a));
      }
      l1360 = u_field(l1360, 1);
    }
    t1342 = u_mk_con(&n_syntax_46_TTuple, 1, t1358);
  }
  if (!t1342) {
    u_value v_u = s1341;
    t1342 = v_u;
  }
  if (!t1342) {
    u_no_case(s1341);
  }
  return t1342;
}

u_value f_check_46_zonk_var_2(u_value v_su, u_value v_x) {
  return f_syntax_46_Var_46_typ_58__61__2(v_x, f_check_46_zonk_type_2(v_su, f_syntax_46_Var_46_typ_1(v_x)));
}

u_value f_check_46_zonk_vars_2(u_value v_su, u_value v_xs) {
  u_value t1361 = &o_list_46_Nil;
  u_value *e1362 = &t1361;
  u_value l1363 = v_xs;
  while (u_is_con(l1363, &n_list_46_Cons)) {
    u_value v_x = u_field(l1363, 0);
    if (u_truth(u_mk_bool(1))) {
      e1362 = u_append(e1362, f_check_46_zonk_var_2(v_su, v_x));
    }
    l1363 = u_field(l1363, 1);
  }
  return t1361;
}

u_value f_check_46_zonk_expr_2(u_value v_su, u_value v_e) {
  u_value s1364 = v_e;
  u_value t1365 = NULL;
  if ((!t1365) && u_is_con(s1364, &n_syntax_46_ELit)) {
    t1365 = v_e;
  }
  if ((!t1365) && u_is_con(s1364, &n_syntax_46_EVar)) {
    u_value v_x = u_field(s1364, 0);
    t1365 = u_mk_con(&n_syntax_46_EVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t1365) && u_is_con(s1364, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s1364, 0);
    u_value v_args = u_field(s1364, 1);
    u_value t1366 = &o_list_46_Nil;
    u_value *e1367 = &t1366;
    u_value l1368 = v_args;
    while (u_is_con(l1368, &n_list_46_Cons)) {
      u_value v_a = u_field(l1368, 0);
      if (u_truth(u_mk_bool(1))) {
        e1367 = u_append(e1367, f_check_46_zonk_expr_2(v_su, v_a));
      }
      l1368 = u_field(l1368, 1);
    }
    t1365 = u_mk_con(&n_syntax_46_EApp, 2, f_check_46_zonk_expr_2(v_su, v_f), t1366);
  }
  if ((!t1365) && u_is_con(s1364, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s1364, 0);
    u_value v_b = u_field(s1364, 1);
    t1365 = u_mk_con(&n_syntax_46_ELam, 2, f_check_46_zonk_vars_2(v_su, v_xs), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t1365) && u_is_con(s1364, &n_syntax_46_EProc)) {
    u_value v_is = u_field(s1364, 0);
    u_value v_os = u_field(s1364, 1);
    u_value v_rs = u_field(s1364, 2);
    u_value v_b = u_field(s1364, 3);
    t1365 = u_mk_con(&n_syntax_46_EProc, 4, f_check_46_zonk_vars_2(v_su, v_is), f_check_46_zonk_vars_2(v_su, v_os), f_check_46_zonk_vars_2(v_su, v_rs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t1365) && u_is_con(s1364, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s1364, 0);
    u_value v_b = u_field(s1364, 1);
    u_value t1369 = &o_list_46_Nil;
    u_value *e1370 = &t1369;
    u_value l1371 = v_ps;
    while (u_is_con(l1371, &n_list_46_Cons)) {
      u_value v_p = u_field(l1371, 0);
      if (u_truth(u_mk_bool(1))) {
        e1370 = u_append(e1370, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l1371 = u_field(l1371, 1);
    }
    t1365 = u_mk_con(&n_syntax_46_ELet, 2, t1369, f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t1365) && u_is_con(s1364, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s1364, 0);
    u_value v_l = u_field(s1364, 1);
    u_value v_r = u_field(s1364, 2);
    t1365 = u_mk_con(&n_syntax_46_EIte, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_expr_2(v_su, v_l), f_check_46_zonk_expr_2(v_su, v_r));
  }
  if ((!t1365) && u_is_con(s1364, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s1364, 0);
    u_value v_bs = u_field(s1364, 1);
    u_value t1372 = &o_list_46_Nil;
    u_value *e1373 = &t1372;
    u_value l1374 = v_bs;
    while (u_is_con(l1374, &n_list_46_Cons)) {
      u_value v_b = u_field(l1374, 0);
      if (u_truth(u_mk_bool(1))) {
        e1373 = u_append(e1373, f_check_46_zonk_expr_branch_2(v_su, v_b));
      }
      l1374 = u_field(l1374, 1);
    }
    t1365 = u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t1372);
  }
  if ((!t1365) && u_is_con(s1364, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s1364, 0);
    u_value v_xs = u_field(s1364, 1);
    u_value v_ls = u_field(s1364, 2);
    u_value v_c = u_field(s1364, 3);
    u_value t1375 = &o_list_46_Nil;
    u_value *e1376 = &t1375;
    u_value l1377 = v_ls;
    while (u_is_con(l1377, &n_list_46_Cons)) {
      u_value v_l = u_field(l1377, 0);
      if (u_truth(u_mk_bool(1))) {
        e1376 = u_append(e1376, f_check_46_zonk_expr_2(v_su, v_l));
      }
      l1377 = u_field(l1377, 1);
    }
    t1365 = u_mk_con(&n_syntax_46_EComp, 4, f_check_46_zonk_expr_2(v_su, v_r), f_check_46_zonk_vars_2(v_su, v_xs), t1375, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t1365) && u_is_con(s1364, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s1364, 0);
    u_value t1378 = &o_list_46_Nil;
    u_value *e1379 = &t1378;
    u_value l1380 = v_es;
    while (u_is_con(l1380, &n_list_46_Cons)) {
      u_value v_e = u_field(l1380, 0);
      if (u_truth(u_mk_bool(1))) {
        e1379 = u_append(e1379, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l1380 = u_field(l1380, 1);
    }
    t1365 = u_mk_con(&n_syntax_46_ETuple, 1, t1378);
  }
  if ((!t1365) && u_is_con(s1364, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s1364, 0);
    u_value v_t = u_field(s1364, 1);
    t1365 = u_mk_con(&n_syntax_46_ERaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t1365) {
    u_no_case(s1364);
  }
  return t1365;
}

u_value f_check_46_zonk_binding_2(u_value v_su, u_value v_p) {
  u_value s1381 = v_p;
  u_value t1382 = NULL;
  if (!t1382) {
    u_value v_x = u_field(s1381, 0);
    u_value v_e = u_field(s1381, 1);
    t1382 = u_mk_tuple(2, f_check_46_zonk_var_2(v_su, v_x), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t1382) {
    u_no_case(s1381);
  }
  return t1382;
}

u_value f_check_46_zonk_expr_branch_2(u_value v_su, u_value v_b) {
  u_value s1383 = v_b;
  u_value t1384 = NULL;
  if ((!t1384) && u_is_con(s1383, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s1383, 0);
    u_value v_g = u_field(s1383, 1);
    u_value v_e = u_field(s1383, 2);
    t1384 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t1384) {
    u_no_case(s1383);
  }
  return t1384;
}

u_value f_check_46_zonk_pat_2(u_value v_su, u_value v_p) {
  u_value s1385 = v_p;
  u_value t1386 = NULL;
  if ((!t1386) && u_is_con(s1385, &n_syntax_46_PVar)) {
    u_value v_x = u_field(s1385, 0);
    t1386 = u_mk_con(&n_syntax_46_PVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t1386) && u_is_con(s1385, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s1385, 0);
    u_value v_args = u_field(s1385, 1);
    u_value t1387 = &o_list_46_Nil;
    u_value *e1388 = &t1387;
    u_value l1389 = v_args;
    while (u_is_con(l1389, &n_list_46_Cons)) {
      u_value v_a = u_field(l1389, 0);
      if (u_truth(u_mk_bool(1))) {
        e1388 = u_append(e1388, f_check_46_zonk_pat_2(v_su, v_a));
      }
      l1389 = u_field(l1389, 1);
    }
    t1386 = u_mk_con(&n_syntax_46_PApp, 2, v_n, t1387);
  }
  if ((!t1386) && u_is_con(s1385, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1385, 0);
    u_value t1390 = &o_list_46_Nil;
    u_value *e1391 = &t1390;
    u_value l1392 = v_ps;
    while (u_is_con(l1392, &n_list_46_Cons)) {
      u_value v_q = u_field(l1392, 0);
      if (u_truth(u_mk_bool(1))) {
        e1391 = u_append(e1391, f_check_46_zonk_pat_2(v_su, v_q));
      }
      l1392 = u_field(l1392, 1);
    }
    t1386 = u_mk_con(&n_syntax_46_PTuple, 1, t1390);
  }
  if (!t1386) {
    t1386 = v_p;
  }
  if (!t1386) {
    u_no_case(s1385);
  }
  return t1386;
}

u_value f_check_46_zonk_stmt_2(u_value v_su, u_value v_x) {
  u_value s1393 = v_x;
  u_value t1394 = NULL;
  if ((!t1394) && u_is_con(s1393, &n_syntax_46_SSkip)) {
    t1394 = v_x;
  }
  if ((!t1394) && u_is_con(s1393, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s1393, 0);
    u_value t1395 = &o_list_46_Nil;
    u_value *e1396 = &t1395;
    u_value l1397 = v_ps;
    while (u_is_con(l1397, &n_list_46_Cons)) {
      u_value v_p = u_field(l1397, 0);
      if (u_truth(u_mk_bool(1))) {
        e1396 = u_append(e1396, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l1397 = u_field(l1397, 1);
    }
    t1394 = u_mk_con(&n_syntax_46_SAssign, 1, t1395);
  }
  if ((!t1394) && u_is_con(s1393, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s1393, 0);
    u_value v_ys = u_field(s1393, 1);
    u_value t1398 = &o_list_46_Nil;
    u_value *e1399 = &t1398;
    u_value l1400 = v_ys;
    while (u_is_con(l1400, &n_list_46_Cons)) {
      u_value v_y = u_field(l1400, 0);
      if (u_truth(u_mk_bool(1))) {
        e1399 = u_append(e1399, f_check_46_zonk_stmt_2(v_su, v_y));
      }
      l1400 = u_field(l1400, 1);
    }
    t1394 = u_mk_con(&n_syntax_46_SBlock, 2, f_check_46_zonk_vars_2(v_su, v_xs), t1398);
  }
  if ((!t1394) && u_is_con(s1393, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s1393, 0);
    u_value v_l = u_field(s1393, 1);
    u_value v_r = u_field(s1393, 2);
    t1394 = u_mk_con(&n_syntax_46_SIf, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_l), f_check_46_zonk_stmt_2(v_su, v_r));
  }
  if ((!t1394) && u_is_con(s1393, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s1393, 0);
    u_value v_b = u_field(s1393, 1);
    t1394 = u_mk_con(&n_syntax_46_SWhile, 2, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t1394) && u_is_con(s1393, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s1393, 0);
    u_value v_ins = u_field(s1393, 1);
    u_value v_outs = u_field(s1393, 2);
    u_value v_refs = u_field(s1393, 3);
    u_value t1401 = &o_list_46_Nil;
    u_value *e1402 = &t1401;
    u_value l1403 = v_ins;
    while (u_is_con(l1403, &n_list_46_Cons)) {
      u_value v_e = u_field(l1403, 0);
      if (u_truth(u_mk_bool(1))) {
        e1402 = u_append(e1402, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l1403 = u_field(l1403, 1);
    }
    t1394 = u_mk_con(&n_syntax_46_SCall, 4, v_p, t1401, f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs));
  }
  if ((!t1394) && u_is_con(s1393, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s1393, 0);
    u_value v_bs = u_field(s1393, 1);
    u_value t1404 = &o_list_46_Nil;
    u_value *e1405 = &t1404;
    u_value l1406 = v_bs;
    while (u_is_con(l1406, &n_list_46_Cons)) {
      u_value v_b = u_field(l1406, 0);
      if (u_truth(u_mk_bool(1))) {
        e1405 = u_append(e1405, f_check_46_zonk_stmt_branch_2(v_su, v_b));
      }
      l1406 = u_field(l1406, 1);
    }
    t1394 = u_mk_con(&n_syntax_46_SMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t1404);
  }
  if ((!t1394) && u_is_con(s1393, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s1393, 0);
    t1394 = u_mk_con(&n_syntax_46_SAssert, 1, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t1394) && u_is_con(s1393, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s1393, 0);
    u_value v_t = u_field(s1393, 1);
    t1394 = u_mk_con(&n_syntax_46_SRaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if ((!t1394) && u_is_con(s1393, &n_syntax_46_STry)) {
    u_value v_b = u_field(s1393, 0);
    u_value v_hs = u_field(s1393, 1);
    u_value t1407 = &o_list_46_Nil;
    u_value *e1408 = &t1407;
    u_value l1409 = v_hs;
    while (u_is_con(l1409, &n_list_46_Cons)) {
      u_value v_h = u_field(l1409, 0);
      if (u_truth(u_mk_bool(1))) {
        e1408 = u_append(e1408, f_check_46_zonk_handler_2(v_su, v_h));
      }
      l1409 = u_field(l1409, 1);
    }
    t1394 = u_mk_con(&n_syntax_46_STry, 2, f_check_46_zonk_stmt_2(v_su, v_b), t1407);
  }
  if (!t1394) {
    u_no_case(s1393);
  }
  return t1394;
}

u_value f_check_46_zonk_stmt_branch_2(u_value v_su, u_value v_b) {
  u_value s1410 = v_b;
  u_value t1411 = NULL;
  if ((!t1411) && u_is_con(s1410, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s1410, 0);
    u_value v_g = u_field(s1410, 1);
    u_value v_y = u_field(s1410, 2);
    t1411 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_y));
  }
  if (!t1411) {
    u_no_case(s1410);
  }
  return t1411;
}

u_value f_check_46_zonk_handler_2(u_value v_su, u_value v_h) {
  u_value s1412 = v_h;
  u_value t1413 = NULL;
  if ((!t1413) && u_is_con(s1412, &n_syntax_46_Handler)) {
    u_value v_t = u_field(s1412, 0);
    u_value v_p = u_field(s1412, 1);
    u_value v_g = u_field(s1412, 2);
    u_value v_b = u_field(s1412, 3);
    t1413 = u_mk_con(&n_syntax_46_Handler, 4, f_check_46_zonk_type_2(v_su, v_t), f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t1413) {
    u_no_case(s1412);
  }
  return t1413;
}

u_value f_check_46_zonk_cmd_2(u_value v_su, u_value v_c) {
  u_value s1414 = v_c;
  u_value t1415 = NULL;
  if ((!t1415) && u_is_con(s1414, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s1414, 0);
    u_value v_ps = u_field(s1414, 1);
    u_value v_xss = u_field(s1414, 2);
    u_value v_r = u_field(s1414, 3);
    u_value v_b = u_field(s1414, 4);
    u_value t1416 = &o_list_46_Nil;
    u_value *e1417 = &t1416;
    u_value l1418 = v_xss;
    while (u_is_con(l1418, &n_list_46_Cons)) {
      u_value v_xs = u_field(l1418, 0);
      if (u_truth(u_mk_bool(1))) {
        e1417 = u_append(e1417, f_check_46_zonk_vars_2(v_su, v_xs));
      }
      l1418 = u_field(l1418, 1);
    }
    t1415 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, t1416, f_check_46_zonk_type_2(v_su, v_r), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t1415) && u_is_con(s1414, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s1414, 0);
    u_value v_ps = u_field(s1414, 1);
    u_value v_ins = u_field(s1414, 2);
    u_value v_outs = u_field(s1414, 3);
    u_value v_refs = u_field(s1414, 4);
    u_value v_b = u_field(s1414, 5);
    t1415 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, f_check_46_zonk_vars_2(v_su, v_ins), f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t1415) && u_is_con(s1414, &n_syntax_46_CTest)) {
    u_value v_n = u_field(s1414, 0);
    u_value v_xs = u_field(s1414, 1);
    u_value v_b = u_field(s1414, 2);
    t1415 = u_mk_con(&n_syntax_46_CTest, 3, v_n, v_xs, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t1415) {
    t1415 = v_c;
  }
  if (!t1415) {
    u_no_case(s1414);
  }
  return t1415;
}

u_value f_check_46_unknown_type_1(u_value v_t) {
  u_value t1419;
  if (u_truth(u_eq(f_check_46_free_1(v_t), &o_list_46_Nil))) {
    t1419 = &o_option_46_None;
  } else {
    t1419 = u_mk_con(&n_option_46_Some, 1, v_t);
  }
  return t1419;
}

u_value f_check_46_unknown_in_1(u_value v_x) {
  u_value s1420 = v_x;
  u_value t1421 = NULL;
  if ((!t1421) && u_is_con(s1420, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s1420, 0);
    t1421 = f_check_46_unknown_in_exprs_1(f_list_46_seconds_1(v_ps));
  }
  if ((!t1421) && u_is_con(s1420, &n_syntax_46_SBlock)) {
    u_value v_ys = u_field(s1420, 1);
    t1421 = f_check_46_unknown_in_all_1(v_ys);
  }
  if ((!t1421) && u_is_con(s1420, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s1420, 0);
    u_value v_l = u_field(s1420, 1);
    u_value v_r = u_field(s1420, 2);
    t1421 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_option_46_or_else_2(f_check_46_unknown_in_1(v_l), f_check_46_unknown_in_1(v_r)));
  }
  if ((!t1421) && u_is_con(s1420, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s1420, 0);
    u_value v_b = u_field(s1420, 1);
    t1421 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_check_46_unknown_in_1(v_b));
  }
  if ((!t1421) && u_is_con(s1420, &n_syntax_46_SCall)) {
    u_value v_ins = u_field(s1420, 1);
    t1421 = f_check_46_unknown_in_exprs_1(v_ins);
  }
  if ((!t1421) && u_is_con(s1420, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s1420, 0);
    u_value v_bs = u_field(s1420, 1);
    t1421 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_a), f_check_46_unknown_in_branches_1(v_bs));
  }
  if ((!t1421) && u_is_con(s1420, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s1420, 0);
    t1421 = f_check_46_unknown_in_expr_1(v_c);
  }
  if ((!t1421) && u_is_con(s1420, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s1420, 0);
    u_value v_t = u_field(s1420, 1);
    t1421 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t1421) && u_is_con(s1420, &n_syntax_46_STry)) {
    u_value v_b = u_field(s1420, 0);
    u_value v_hs = u_field(s1420, 1);
    t1421 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_hs));
  }
  if (!t1421) {
    t1421 = &o_option_46_None;
  }
  if (!t1421) {
    u_no_case(s1420);
  }
  return t1421;
}

u_value f_check_46_unknown_in_expr_1(u_value v_e) {
  u_value s1422 = v_e;
  u_value t1423 = NULL;
  if ((!t1423) && u_is_con(s1422, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s1422, 0);
    u_value v_t = u_field(s1422, 1);
    t1423 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t1423) && u_is_con(s1422, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s1422, 0);
    u_value v_args = u_field(s1422, 1);
    t1423 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_f, v_args));
  }
  if ((!t1423) && u_is_con(s1422, &n_syntax_46_ELam)) {
    u_value v_b = u_field(s1422, 1);
    t1423 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t1423) && u_is_con(s1422, &n_syntax_46_EProc)) {
    u_value v_b = u_field(s1422, 3);
    t1423 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t1423) && u_is_con(s1422, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s1422, 0);
    u_value v_b = u_field(s1422, 1);
    t1423 = f_check_46_unknown_in_exprs_1(f_list_46__43__43__2(f_list_46_seconds_1(v_ps), u_mk_con(&n_list_46_Cons, 2, v_b, &o_list_46_Nil)));
  }
  if ((!t1423) && u_is_con(s1422, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s1422, 0);
    u_value v_l = u_field(s1422, 1);
    u_value v_r = u_field(s1422, 2);
    t1423 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, &o_list_46_Nil))));
  }
  if ((!t1423) && u_is_con(s1422, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s1422, 0);
    u_value v_bs = u_field(s1422, 1);
    u_value t1424 = &o_list_46_Nil;
    u_value *e1425 = &t1424;
    u_value l1426 = v_bs;
    while (u_is_con(l1426, &n_list_46_Cons)) {
      u_value v_b = u_field(l1426, 0);
      if (u_truth(u_mk_bool(1))) {
        e1425 = u_append(e1425, f_check_46_branch_exprs_1(v_b));
      }
      l1426 = u_field(l1426, 1);
    }
    t1423 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_a, f_list_46_concat_1(t1424)));
  }
  if ((!t1423) && u_is_con(s1422, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s1422, 0);
    u_value v_ls = u_field(s1422, 2);
    u_value v_c = u_field(s1422, 3);
    t1423 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Cons, 2, v_c, v_ls)));
  }
  if ((!t1423) && u_is_con(s1422, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s1422, 0);
    t1423 = f_check_46_unknown_in_exprs_1(v_es);
  }
  if (!t1423) {
    t1423 = &o_option_46_None;
  }
  if (!t1423) {
    u_no_case(s1422);
  }
  return t1423;
}

u_value f_check_46_unknown_in_exprs_1(u_value v_es) {
  u_value s1427 = v_es;
  u_value t1428 = NULL;
  if ((!t1428) && u_is_con(s1427, &n_list_46_Nil)) {
    t1428 = &o_option_46_None;
  }
  if ((!t1428) && u_is_con(s1427, &n_list_46_Cons)) {
    u_value v_e = u_field(s1427, 0);
    u_value v_rest = u_field(s1427, 1);
    t1428 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_e), f_check_46_unknown_in_exprs_1(v_rest));
  }
  if (!t1428) {
    u_no_case(s1427);
  }
  return t1428;
}

u_value f_check_46_branch_exprs_1(u_value v_b) {
  u_value s1429 = v_b;
  u_value t1430 = NULL;
  if ((!t1430) && (u_is_con(s1429, &n_syntax_46_Branch) && u_is_con(u_field(s1429, 1), &n_option_46_Some))) {
    u_value v_g = u_field(u_field(s1429, 1), 0);
    u_value v_e = u_field(s1429, 2);
    t1430 = u_mk_con(&n_list_46_Cons, 2, v_g, u_mk_con(&n_list_46_Cons, 2, v_e, &o_list_46_Nil));
  }
  if ((!t1430) && (u_is_con(s1429, &n_syntax_46_Branch) && u_is_con(u_field(s1429, 1), &n_option_46_None))) {
    u_value v_e = u_field(s1429, 2);
    t1430 = u_mk_con(&n_list_46_Cons, 2, v_e, &o_list_46_Nil);
  }
  if (!t1430) {
    u_no_case(s1429);
  }
  return t1430;
}

u_value f_check_46_unknown_in_all_1(u_value v_xs) {
  u_value s1431 = v_xs;
  u_value t1432 = NULL;
  if ((!t1432) && u_is_con(s1431, &n_list_46_Nil)) {
    t1432 = &o_option_46_None;
  }
  if ((!t1432) && u_is_con(s1431, &n_list_46_Cons)) {
    u_value v_y = u_field(s1431, 0);
    u_value v_ys = u_field(s1431, 1);
    t1432 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_all_1(v_ys));
  }
  if (!t1432) {
    u_no_case(s1431);
  }
  return t1432;
}

u_value f_check_46_unknown_in_branches_1(u_value v_bs) {
  u_value s1433 = v_bs;
  u_value t1434 = NULL;
  if ((!t1434) && u_is_con(s1433, &n_list_46_Nil)) {
    t1434 = &o_option_46_None;
  }
  if ((!t1434) && (u_is_con(s1433, &n_list_46_Cons) && (u_is_con(u_field(s1433, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s1433, 0), 1), &n_option_46_Some)))) {
    u_value v_g = u_field(u_field(u_field(s1433, 0), 1), 0);
    u_value v_y = u_field(u_field(s1433, 0), 2);
    u_value v_rest = u_field(s1433, 1);
    t1434 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_g), f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest)));
  }
  if ((!t1434) && (u_is_con(s1433, &n_list_46_Cons) && (u_is_con(u_field(s1433, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s1433, 0), 1), &n_option_46_None)))) {
    u_value v_y = u_field(u_field(s1433, 0), 2);
    u_value v_rest = u_field(s1433, 1);
    t1434 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest));
  }
  if (!t1434) {
    u_no_case(s1433);
  }
  return t1434;
}

u_value f_check_46_unknown_in_handlers_1(u_value v_hs) {
  u_value s1435 = v_hs;
  u_value t1436 = NULL;
  if ((!t1436) && u_is_con(s1435, &n_list_46_Nil)) {
    t1436 = &o_option_46_None;
  }
  if ((!t1436) && (u_is_con(s1435, &n_list_46_Cons) && (u_is_con(u_field(s1435, 0), &n_syntax_46_Handler) && u_is_con(u_field(u_field(s1435, 0), 1), &n_syntax_46_PWild)))) {
    u_value v_b = u_field(u_field(s1435, 0), 3);
    u_value v_rest = u_field(s1435, 1);
    t1436 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest));
  }
  if ((!t1436) && (u_is_con(s1435, &n_list_46_Cons) && u_is_con(u_field(s1435, 0), &n_syntax_46_Handler))) {
    u_value v_t = u_field(u_field(s1435, 0), 0);
    u_value v_b = u_field(u_field(s1435, 0), 3);
    u_value v_rest = u_field(s1435, 1);
    t1436 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest)));
  }
  if (!t1436) {
    u_no_case(s1435);
  }
  return t1436;
}

u_value f_check_46_unknown_in_cmd_1(u_value v_c) {
  u_value s1437 = v_c;
  u_value t1438 = NULL;
  if ((!t1438) && u_is_con(s1437, &n_syntax_46_CFun)) {
    u_value v_b = u_field(s1437, 4);
    t1438 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t1438) && u_is_con(s1437, &n_syntax_46_CProc)) {
    u_value v_b = u_field(s1437, 5);
    t1438 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t1438) && u_is_con(s1437, &n_syntax_46_CTest)) {
    u_value v_b = u_field(s1437, 2);
    t1438 = f_check_46_unknown_in_1(v_b);
  }
  if (!t1438) {
    t1438 = &o_option_46_None;
  }
  if (!t1438) {
    u_no_case(s1437);
  }
  return t1438;
}

u_value f_check_46_head_of_1(u_value v_p) {
  u_value s1439 = v_p;
  u_value t1440 = NULL;
  if ((!t1440) && u_is_con(s1439, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s1439, 0);
    u_value v_args = u_field(s1439, 1);
    t1440 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_n), v_args));
  }
  if ((!t1440) && u_is_con(s1439, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1439, 0);
    t1440 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ps));
  }
  if ((!t1440) && u_is_con(s1439, &n_syntax_46_PLit)) {
    u_value v_v = u_field(s1439, 0);
    t1440 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, v_v), &o_list_46_Nil));
  }
  if (!t1440) {
    t1440 = &o_option_46_None;
  }
  if (!t1440) {
    u_no_case(s1439);
  }
  return t1440;
}

u_value f_check_46_heads_1(u_value v_rows) {
  u_value s1441 = v_rows;
  u_value t1442 = NULL;
  if ((!t1442) && u_is_con(s1441, &n_list_46_Nil)) {
    t1442 = &o_list_46_Nil;
  }
  if ((!t1442) && (u_is_con(s1441, &n_list_46_Cons) && u_is_con(u_field(s1441, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s1441, 0), 0);
    u_value v_rest = u_field(s1441, 1);
    u_value s1443 = f_check_46_head_of_1(v_p);
    u_value t1444 = NULL;
    if ((!t1444) && u_is_con(s1443, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s1443, 0), 0);
      t1444 = u_mk_con(&n_list_46_Cons, 2, v_h, f_check_46_heads_1(v_rest));
    }
    if ((!t1444) && u_is_con(s1443, &n_option_46_None)) {
      t1444 = f_check_46_heads_1(v_rest);
    }
    if (!t1444) {
      u_no_case(s1443);
    }
    t1442 = t1444;
  }
  if ((!t1442) && (u_is_con(s1441, &n_list_46_Cons) && u_is_con(u_field(s1441, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s1441, 1);
    t1442 = f_check_46_heads_1(v_rest);
  }
  if (!t1442) {
    u_no_case(s1441);
  }
  return t1442;
}

u_value f_check_46_field_types_3(u_value v_c, u_value v_k, u_value v_args) {
  u_value s1445 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t1446 = NULL;
  if ((!t1446) && u_is_con(s1445, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s1445, 0), 0);
    u_value v_ks = u_field(u_field(s1445, 0), 1);
    u_value t1447 = &o_list_46_Nil;
    u_value *e1448 = &t1447;
    u_value l1449 = v_ks;
    while (u_is_con(l1449, &n_list_46_Cons)) {
      u_value v_a = u_field(l1449, 0);
      if (u_truth(u_mk_bool(1))) {
        e1448 = u_append(e1448, f_check_46_subst_2(v_a, f_list_46_zip_2(v_ps, v_args)));
      }
      l1449 = u_field(l1449, 1);
    }
    t1446 = t1447;
  }
  if ((!t1446) && u_is_con(s1445, &n_option_46_None)) {
    t1446 = &o_list_46_Nil;
  }
  if (!t1446) {
    u_no_case(s1445);
  }
  return t1446;
}

u_value f_check_46_constructors_of_3(u_value v_c, u_value v_su, u_value v_t) {
  u_value s1450 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t1451 = NULL;
  if ((!t1451) && (u_is_con(s1450, &n_syntax_46_TSort) && (u_is_con(u_field(s1450, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1450, 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(s1450, 0), 1), u_mk_lit("Bool")) && u_is_con(u_field(s1450, 1), &n_list_46_Nil)))))) {
    t1451 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1))), &o_list_46_Nil), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0))), &o_list_46_Nil), &o_list_46_Nil)));
  }
  if ((!t1451) && u_is_con(s1450, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1450, 0);
    t1451 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ts), &o_list_46_Nil));
  }
  if ((!t1451) && u_is_con(s1450, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s1450, 0);
    u_value v_args = u_field(s1450, 1);
    u_value s1452 = f_ctx_46_members_of_2(v_c, v_n);
    u_value t1453 = NULL;
    if ((!t1453) && u_is_con(s1452, &n_option_46_Some)) {
      u_value v_ks = u_field(s1452, 0);
      u_value t1454 = &o_list_46_Nil;
      u_value *e1455 = &t1454;
      u_value l1456 = v_ks;
      while (u_is_con(l1456, &n_list_46_Cons)) {
        u_value v_k = u_field(l1456, 0);
        if (u_truth(u_mk_bool(1))) {
          e1455 = u_append(e1455, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_k), f_check_46_field_types_3(v_c, v_k, v_args)));
        }
        l1456 = u_field(l1456, 1);
      }
      t1453 = u_mk_con(&n_option_46_Some, 1, t1454);
    }
    if ((!t1453) && u_is_con(s1452, &n_option_46_None)) {
      t1453 = &o_option_46_None;
    }
    if (!t1453) {
      u_no_case(s1452);
    }
    t1451 = t1453;
  }
  if (!t1451) {
    t1451 = &o_option_46_None;
  }
  if (!t1451) {
    u_no_case(s1450);
  }
  return t1451;
}

u_value f_check_46_fields_of_4(u_value v_c, u_value v_su, u_value v_t, u_value v_h) {
  u_value s1457 = f_check_46_constructors_of_3(v_c, v_su, v_t);
  u_value t1458 = NULL;
  if ((!t1458) && u_is_con(s1457, &n_option_46_Some)) {
    u_value v_ks = u_field(s1457, 0);
    u_value s1459 = f_list_46_lookup_2(v_h, v_ks);
    u_value t1460 = NULL;
    if ((!t1460) && u_is_con(s1459, &n_option_46_Some)) {
      u_value v_ts = u_field(s1459, 0);
      t1460 = v_ts;
    }
    if ((!t1460) && u_is_con(s1459, &n_option_46_None)) {
      t1460 = &o_list_46_Nil;
    }
    if (!t1460) {
      u_no_case(s1459);
    }
    t1458 = t1460;
  }
  if ((!t1458) && u_is_con(s1457, &n_option_46_None)) {
    t1458 = &o_list_46_Nil;
  }
  if (!t1458) {
    u_no_case(s1457);
  }
  return t1458;
}

u_value f_check_46_covers_2(u_value v_hs, u_value v_ks) {
  u_value s1461 = v_ks;
  u_value t1462 = NULL;
  if ((!t1462) && u_is_con(s1461, &n_list_46_Nil)) {
    t1462 = u_mk_bool(1);
  }
  if ((!t1462) && u_is_con(s1461, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s1461, 0), 0);
    u_value v_rest = u_field(s1461, 1);
    u_value t1463 = f_list_46_contains_2(v_h, v_hs);
    if (u_truth(t1463)) {
      t1463 = f_check_46_covers_2(v_hs, v_rest);
    }
    t1462 = t1463;
  }
  if (!t1462) {
    u_no_case(s1461);
  }
  return t1462;
}

u_value f_check_46_defaults_1(u_value v_rows) {
  u_value s1464 = v_rows;
  u_value t1465 = NULL;
  if ((!t1465) && u_is_con(s1464, &n_list_46_Nil)) {
    t1465 = &o_list_46_Nil;
  }
  if ((!t1465) && (u_is_con(s1464, &n_list_46_Cons) && u_is_con(u_field(s1464, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s1464, 0), 0);
    u_value v_ps = u_field(u_field(s1464, 0), 1);
    u_value v_rest = u_field(s1464, 1);
    u_value s1466 = f_check_46_head_of_1(v_p);
    u_value t1467 = NULL;
    if ((!t1467) && u_is_con(s1466, &n_option_46_Some)) {
      t1467 = f_check_46_defaults_1(v_rest);
    }
    if ((!t1467) && u_is_con(s1466, &n_option_46_None)) {
      t1467 = u_mk_con(&n_list_46_Cons, 2, v_ps, f_check_46_defaults_1(v_rest));
    }
    if (!t1467) {
      u_no_case(s1466);
    }
    t1465 = t1467;
  }
  if ((!t1465) && (u_is_con(s1464, &n_list_46_Cons) && u_is_con(u_field(s1464, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s1464, 1);
    t1465 = f_check_46_defaults_1(v_rest);
  }
  if (!t1465) {
    u_no_case(s1464);
  }
  return t1465;
}

u_value f_check_46_specialize_3(u_value v_rows, u_value v_h, u_value v_n) {
  u_value s1468 = v_rows;
  u_value t1469 = NULL;
  if ((!t1469) && u_is_con(s1468, &n_list_46_Nil)) {
    t1469 = &o_list_46_Nil;
  }
  if ((!t1469) && (u_is_con(s1468, &n_list_46_Cons) && u_is_con(u_field(s1468, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s1468, 0), 0);
    u_value v_ps = u_field(u_field(s1468, 0), 1);
    u_value v_rest = u_field(s1468, 1);
    u_value s1470 = f_check_46_head_of_1(v_p);
    u_value t1471 = NULL;
    if ((!t1471) && u_is_con(s1470, &n_option_46_Some)) {
      u_value v_g = u_field(u_field(s1470, 0), 0);
      u_value v_args = u_field(u_field(s1470, 0), 1);
      u_value t1472;
      if (u_truth(u_eq(v_g, v_h))) {
        t1472 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(v_args, v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
      } else {
        t1472 = f_check_46_specialize_3(v_rest, v_h, v_n);
      }
      t1471 = t1472;
    }
    if ((!t1471) && u_is_con(s1470, &n_option_46_None)) {
      t1471 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(f_list_46_replicate_2(v_n, &o_syntax_46_PWild), v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
    }
    if (!t1471) {
      u_no_case(s1470);
    }
    t1469 = t1471;
  }
  if ((!t1469) && (u_is_con(s1468, &n_list_46_Cons) && u_is_con(u_field(s1468, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s1468, 1);
    t1469 = f_check_46_specialize_3(v_rest, v_h, v_n);
  }
  if (!t1469) {
    u_no_case(s1468);
  }
  return t1469;
}

u_value f_check_46_useful_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_q, u_value v_ts) {
  u_value s1473 = u_mk_tuple(2, v_q, v_ts);
  u_value t1474 = NULL;
  if ((!t1474) && u_is_con(u_field(s1473, 0), &n_list_46_Nil)) {
    t1474 = u_eq(v_rows, &o_list_46_Nil);
  }
  if ((!t1474) && (u_is_con(u_field(s1473, 0), &n_list_46_Cons) && u_is_con(u_field(s1473, 1), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s1473, 0), 0);
    u_value v_ps = u_field(u_field(s1473, 0), 1);
    u_value v_t = u_field(u_field(s1473, 1), 0);
    u_value v_rest = u_field(u_field(s1473, 1), 1);
    u_value s1475 = f_check_46_head_of_1(v_p);
    u_value t1476 = NULL;
    if ((!t1476) && u_is_con(s1475, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s1475, 0), 0);
      u_value v_args = u_field(u_field(s1475, 0), 1);
      t1476 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_ps), f_list_46__43__43__2(f_check_46_fields_of_4(v_c, v_su, v_t, v_h), v_rest));
    }
    if ((!t1476) && u_is_con(s1475, &n_option_46_None)) {
      u_value s1477 = f_check_46_constructors_of_3(v_c, v_su, v_t);
      u_value t1478 = NULL;
      if ((!t1478) && u_is_con(s1477, &n_option_46_Some)) {
        u_value v_ks = u_field(s1477, 0);
        u_value t1479;
        if (u_truth(f_check_46_covers_2(f_check_46_heads_1(v_rows), v_ks))) {
          t1479 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_ks, v_ps, v_rest);
        } else {
          t1479 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
        }
        t1478 = t1479;
      }
      if ((!t1478) && u_is_con(s1477, &n_option_46_None)) {
        t1478 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
      }
      if (!t1478) {
        u_no_case(s1477);
      }
      t1476 = t1478;
    }
    if (!t1476) {
      u_no_case(s1475);
    }
    t1474 = t1476;
  }
  if ((!t1474) && (u_is_con(u_field(s1473, 0), &n_list_46_Cons) && u_is_con(u_field(s1473, 1), &n_list_46_Nil))) {
    t1474 = u_mk_bool(0);
  }
  if (!t1474) {
    u_no_case(s1473);
  }
  return t1474;
}

u_value f_check_46_useful_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_ps, u_value v_rest) {
  u_value s1480 = v_ks;
  u_value t1481 = NULL;
  if ((!t1481) && u_is_con(s1480, &n_list_46_Nil)) {
    t1481 = u_mk_bool(0);
  }
  if ((!t1481) && u_is_con(s1480, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s1480, 0), 0);
    u_value v_args = u_field(u_field(s1480, 0), 1);
    u_value v_more = u_field(s1480, 1);
    u_value t1482 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(f_list_46_replicate_2(f_list_46_length_1(v_args), &o_syntax_46_PWild), v_ps), f_list_46__43__43__2(v_args, v_rest));
    if (!u_truth(t1482)) {
      t1482 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_more, v_ps, v_rest);
    }
    t1481 = t1482;
  }
  if (!t1481) {
    u_no_case(s1480);
  }
  return t1481;
}

u_value f_check_46_witness_2(u_value v_h, u_value v_args) {
  u_value s1483 = v_h;
  u_value t1484 = NULL;
  if ((!t1484) && u_is_con(s1483, &n_check_46_HCon)) {
    u_value v_n = u_field(s1483, 0);
    u_value t1485;
    if (u_truth(u_eq(v_n, f_syntax_46_tuple_name_0()))) {
      t1485 = u_mk_con(&n_syntax_46_PTuple, 1, v_args);
    } else {
      t1485 = u_mk_con(&n_syntax_46_PApp, 2, v_n, v_args);
    }
    t1484 = t1485;
  }
  if ((!t1484) && u_is_con(s1483, &n_check_46_HLit)) {
    u_value v_v = u_field(s1483, 0);
    t1484 = u_mk_con(&n_syntax_46_PLit, 1, v_v);
  }
  if (!t1484) {
    u_no_case(s1483);
  }
  return t1484;
}

u_value f_check_46_missing_4(u_value v_c, u_value v_su, u_value v_rows, u_value v_ts) {
  u_value s1486 = v_ts;
  u_value t1487 = NULL;
  if ((!t1487) && u_is_con(s1486, &n_list_46_Nil)) {
    u_value t1488;
    if (u_truth(u_eq(v_rows, &o_list_46_Nil))) {
      t1488 = u_mk_con(&n_option_46_Some, 1, &o_list_46_Nil);
    } else {
      t1488 = &o_option_46_None;
    }
    t1487 = t1488;
  }
  if ((!t1487) && u_is_con(s1486, &n_list_46_Cons)) {
    u_value v_t = u_field(s1486, 0);
    u_value v_rest = u_field(s1486, 1);
    t1487 = f_check_46_missing_in_6(v_c, v_su, v_rows, f_check_46_heads_1(v_rows), f_check_46_constructors_of_3(v_c, v_su, v_t), v_rest);
  }
  if (!t1487) {
    u_no_case(s1486);
  }
  return t1487;
}

u_value f_check_46_missing_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s1489 = v_cs;
  u_value t1490 = NULL;
  if ((!t1490) && u_is_con(s1489, &n_option_46_Some)) {
    u_value v_ks = u_field(s1489, 0);
    u_value t1491;
    if (u_truth(f_check_46_covers_2(v_hs, v_ks))) {
      t1491 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_ks, v_rest);
    } else {
      t1491 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
    }
    t1490 = t1491;
  }
  if ((!t1490) && u_is_con(s1489, &n_option_46_None)) {
    t1490 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
  }
  if (!t1490) {
    u_no_case(s1489);
  }
  return t1490;
}

u_value f_check_46_first_missing_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_rest) {
  u_value s1492 = v_ks;
  u_value t1493 = NULL;
  if ((!t1493) && u_is_con(s1492, &n_list_46_Nil)) {
    t1493 = &o_option_46_None;
  }
  if ((!t1493) && u_is_con(s1492, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s1492, 0), 0);
    u_value v_args = u_field(u_field(s1492, 0), 1);
    u_value v_more = u_field(s1492, 1);
    u_value s1494 = f_check_46_missing_4(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_rest));
    u_value t1495 = NULL;
    if ((!t1495) && u_is_con(s1494, &n_option_46_Some)) {
      u_value v_ws = u_field(s1494, 0);
      t1495 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_witness_2(v_h, f_list_46_take_2(f_list_46_length_1(v_args), v_ws)), f_list_46_drop_2(f_list_46_length_1(v_args), v_ws)));
    }
    if ((!t1495) && u_is_con(s1494, &n_option_46_None)) {
      t1495 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_more, v_rest);
    }
    if (!t1495) {
      u_no_case(s1494);
    }
    t1493 = t1495;
  }
  if (!t1493) {
    u_no_case(s1492);
  }
  return t1493;
}

u_value f_check_46_other_missing_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s1496 = f_check_46_missing_4(v_c, v_su, f_check_46_defaults_1(v_rows), v_rest);
  u_value t1497 = NULL;
  if ((!t1497) && u_is_con(s1496, &n_option_46_Some)) {
    u_value v_ws = u_field(s1496, 0);
    t1497 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_uncovered_2(v_hs, v_cs), v_ws));
  }
  if ((!t1497) && u_is_con(s1496, &n_option_46_None)) {
    t1497 = &o_option_46_None;
  }
  if (!t1497) {
    u_no_case(s1496);
  }
  return t1497;
}

u_value f_check_46_uncovered_2(u_value v_hs, u_value v_cs) {
  u_value s1498 = v_cs;
  u_value t1499 = NULL;
  if ((!t1499) && u_is_con(s1498, &n_option_46_Some)) {
    u_value v_ks = u_field(s1498, 0);
    u_value t1500;
    if (u_truth(u_eq(v_hs, &o_list_46_Nil))) {
      t1500 = &o_syntax_46_PWild;
    } else {
      t1500 = f_check_46_first_uncovered_2(v_hs, v_ks);
    }
    t1499 = t1500;
  }
  if ((!t1499) && u_is_con(s1498, &n_option_46_None)) {
    t1499 = &o_syntax_46_PWild;
  }
  if (!t1499) {
    u_no_case(s1498);
  }
  return t1499;
}

u_value f_check_46_first_uncovered_2(u_value v_hs, u_value v_ks) {
  u_value s1501 = v_ks;
  u_value t1502 = NULL;
  if ((!t1502) && u_is_con(s1501, &n_list_46_Nil)) {
    t1502 = &o_syntax_46_PWild;
  }
  if ((!t1502) && u_is_con(s1501, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s1501, 0), 0);
    u_value v_args = u_field(u_field(s1501, 0), 1);
    u_value v_rest = u_field(s1501, 1);
    u_value t1503;
    if (u_truth(f_list_46_contains_2(v_h, v_hs))) {
      t1503 = f_check_46_first_uncovered_2(v_hs, v_rest);
    } else {
      t1503 = f_check_46_witness_2(v_h, f_list_46_replicate_2(f_list_46_length_1(v_args), &o_syntax_46_PWild));
    }
    t1502 = t1503;
  }
  if (!t1502) {
    u_no_case(s1501);
  }
  return t1502;
}

void p_check_46_fresh(u_value *r_t, u_value *r_s) {
  {
    u_value t1504 = u_mk_con(&n_syntax_46_TMeta, 1, u_add(f_check_46_State_46_count_1(*r_s), u_mk_int(1)));
    u_value t1505 = f_check_46_State_46_count_58__61__2(*r_s, u_add(f_check_46_State_46_count_1(*r_s), u_mk_int(1)));
    *r_t = t1504;
    *r_s = t1505;
  }
}

void p_check_46_bind_meta(u_value v_i, u_value v_t, u_value *r_s) {
  {
    if (u_truth(f_check_46_occurs_3(f_check_46_State_46_sub_1(*r_s), v_i, v_t))) {
      {
        u_raise(u_mk_con(&n_check_46_InfiniteType, 2, u_mk_con(&n_syntax_46_TMeta, 1, v_i), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t)), "check.TypeError");
      }
    }
    u_value t1506 = f_check_46_State_46_sub_58__61__2(*r_s, f_map_46_put_3(v_i, v_t, f_check_46_State_46_sub_1(*r_s)));
    *r_s = t1506;
  }
}

void p_check_46_unify(u_value v_t1, u_value v_t2, u_value *r_s) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value t1507 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t1);
    v_a = t1507;
    u_value t1508 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t2);
    v_b = t1508;
    u_value s1509 = u_mk_tuple(2, v_a, v_b);
    int d1510 = 0;
    if ((!d1510) && (u_is_con(u_field(s1509, 0), &n_syntax_46_TMeta) && u_is_con(u_field(s1509, 1), &n_syntax_46_TMeta))) {
      u_value v_i = u_field(u_field(s1509, 0), 0);
      u_value v_j = u_field(u_field(s1509, 1), 0);
      d1510 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_i, v_j))) {
          {
            p_check_46_bind_meta(v_i, v_b, r_s);
          }
        }
      }
    }
    if ((!d1510) && u_is_con(u_field(s1509, 0), &n_syntax_46_TMeta)) {
      u_value v_i = u_field(u_field(s1509, 0), 0);
      d1510 = 1;
      {
        p_check_46_bind_meta(v_i, v_b, r_s);
      }
    }
    if ((!d1510) && u_is_con(u_field(s1509, 1), &n_syntax_46_TMeta)) {
      u_value v_j = u_field(u_field(s1509, 1), 0);
      d1510 = 1;
      {
        p_check_46_bind_meta(v_j, v_a, r_s);
      }
    }
    if ((!d1510) && (u_is_con(u_field(s1509, 0), &n_syntax_46_TParam) && u_is_con(u_field(s1509, 1), &n_syntax_46_TParam))) {
      u_value v_x = u_field(u_field(s1509, 0), 0);
      u_value v_y = u_field(u_field(s1509, 1), 0);
      d1510 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_x, v_y))) {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, v_a, v_b), "check.TypeError");
          }
        }
      }
    }
    if ((!d1510) && (u_is_con(u_field(s1509, 0), &n_syntax_46_TSort) && u_is_con(u_field(s1509, 1), &n_syntax_46_TSort))) {
      u_value v_f = u_field(u_field(s1509, 0), 0);
      u_value v_xs = u_field(u_field(s1509, 0), 1);
      u_value v_g = u_field(u_field(s1509, 1), 0);
      u_value v_ys = u_field(u_field(s1509, 1), 1);
      d1510 = 1;
      {
        u_value t1511 = u_eq(v_f, v_g);
        if (u_truth(t1511)) {
          t1511 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys));
        }
        if (u_truth(t1511)) {
          {
            p_check_46_unifies(v_xs, v_ys, r_s);
          }
        } else {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t2)), "check.TypeError");
          }
        }
      }
    }
    if ((!d1510) && (u_is_con(u_field(s1509, 0), &n_syntax_46_TFun) && u_is_con(u_field(s1509, 1), &n_syntax_46_TFun))) {
      u_value v_xs = u_field(u_field(s1509, 0), 0);
      u_value v_r1 = u_field(u_field(s1509, 0), 1);
      u_value v_ys = u_field(u_field(s1509, 1), 0);
      u_value v_r2 = u_field(u_field(s1509, 1), 1);
      d1510 = 1;
      {
        if (u_truth(u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)))) {
          {
            p_check_46_unifies(f_list_46__43__43__2(v_xs, u_mk_con(&n_list_46_Cons, 2, v_r1, &o_list_46_Nil)), f_list_46__43__43__2(v_ys, u_mk_con(&n_list_46_Cons, 2, v_r2, &o_list_46_Nil)), r_s);
          }
        } else {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t2)), "check.TypeError");
          }
        }
      }
    }
    if ((!d1510) && (u_is_con(u_field(s1509, 0), &n_syntax_46_TTuple) && u_is_con(u_field(s1509, 1), &n_syntax_46_TTuple))) {
      u_value v_xs = u_field(u_field(s1509, 0), 0);
      u_value v_ys = u_field(u_field(s1509, 1), 0);
      d1510 = 1;
      {
        if (u_truth(u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)))) {
          {
            p_check_46_unifies(v_xs, v_ys, r_s);
          }
        } else {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t2)), "check.TypeError");
          }
        }
      }
    }
    if ((!d1510) && (u_is_con(u_field(s1509, 0), &n_syntax_46_TProc) && u_is_con(u_field(s1509, 1), &n_syntax_46_TProc))) {
      u_value v_xs = u_field(u_field(s1509, 0), 0);
      u_value v_ys = u_field(u_field(s1509, 0), 1);
      u_value v_zs = u_field(u_field(s1509, 0), 2);
      u_value v_us = u_field(u_field(s1509, 1), 0);
      u_value v_vs = u_field(u_field(s1509, 1), 1);
      u_value v_ws = u_field(u_field(s1509, 1), 2);
      d1510 = 1;
      {
        u_value t1512 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_us));
        if (u_truth(t1512)) {
          t1512 = u_eq(f_list_46_length_1(v_ys), f_list_46_length_1(v_vs));
        }
        u_value t1513 = t1512;
        if (u_truth(t1513)) {
          t1513 = u_eq(f_list_46_length_1(v_zs), f_list_46_length_1(v_ws));
        }
        if (u_truth(t1513)) {
          {
            p_check_46_unifies(f_list_46__43__43__2(v_xs, f_list_46__43__43__2(v_ys, v_zs)), f_list_46__43__43__2(v_us, f_list_46__43__43__2(v_vs, v_ws)), r_s);
          }
        } else {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t2)), "check.TypeError");
          }
        }
      }
    }
    if (!d1510) {
      d1510 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t2)), "check.TypeError");
      }
    }
    if (!d1510) {
      u_no_case(s1509);
    }
  }
}

void p_check_46_unifies(u_value v_xs, u_value v_ys, u_value *r_s) {
  {
    u_value s1514 = u_mk_tuple(2, v_xs, v_ys);
    int d1515 = 0;
    if ((!d1515) && (u_is_con(u_field(s1514, 0), &n_list_46_Cons) && u_is_con(u_field(s1514, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s1514, 0), 0);
      u_value v_xs1 = u_field(u_field(s1514, 0), 1);
      u_value v_y = u_field(u_field(s1514, 1), 0);
      u_value v_ys1 = u_field(u_field(s1514, 1), 1);
      d1515 = 1;
      {
        p_check_46_unify(v_x, v_y, r_s);
        p_check_46_unifies(v_xs1, v_ys1, r_s);
      }
    }
    if ((!d1515) && (u_is_con(u_field(s1514, 0), &n_list_46_Nil) && u_is_con(u_field(s1514, 1), &n_list_46_Nil))) {
      d1515 = 1;
      {
      }
    }
    if (!d1515) {
      d1515 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)), "check.TypeError");
      }
    }
    if (!d1515) {
      u_no_case(s1514);
    }
  }
}

void p_check_46_fresh_list(u_value v_xs, u_value *r_ts, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s1516 = v_xs;
    int d1517 = 0;
    if ((!d1517) && u_is_con(s1516, &n_list_46_Nil)) {
      d1517 = 1;
      {
        u_value t1518 = &o_list_46_Nil;
        *r_ts = t1518;
      }
    }
    if ((!d1517) && u_is_con(s1516, &n_list_46_Cons)) {
      u_value v_more = u_field(s1516, 1);
      d1517 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_fresh_list(v_more, &v_rest, r_s);
        u_value t1519 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rest);
        *r_ts = t1519;
      }
    }
    if (!d1517) {
      u_no_case(s1516);
    }
  }
}

void p_check_46_instance(u_value v_ps, u_value *r_m, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s1520 = v_ps;
    int d1521 = 0;
    if ((!d1521) && u_is_con(s1520, &n_list_46_Nil)) {
      d1521 = 1;
      {
        u_value t1522 = &o_list_46_Nil;
        *r_m = t1522;
      }
    }
    if ((!d1521) && u_is_con(s1520, &n_list_46_Cons)) {
      u_value v_p = u_field(s1520, 0);
      u_value v_qs = u_field(s1520, 1);
      d1521 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_instance(v_qs, &v_rest, r_s);
        u_value t1523 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_p, v_t), v_rest);
        *r_m = t1523;
      }
    }
    if (!d1521) {
      u_no_case(s1520);
    }
  }
}

void p_check_46_instantiate(u_value v_sc, u_value *r_t, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value s1524 = v_sc;
    int d1525 = 0;
    if (!d1525) {
      u_value v_ps = u_field(s1524, 0);
      u_value v_u = u_field(s1524, 1);
      d1525 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        u_value t1526 = f_check_46_subst_2(f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u), v_m);
        *r_t = t1526;
      }
    }
    if (!d1525) {
      u_no_case(s1524);
    }
  }
}

void p_check_46_unique(u_value v_ns) {
  {
    u_value s1527 = f_list_46_repeated_1(v_ns);
    int d1528 = 0;
    if ((!d1528) && u_is_con(s1527, &n_option_46_Some)) {
      u_value v_n = u_field(s1527, 0);
      d1528 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, f_syntax_46_qualified_1(v_n)), "check.TypeError");
      }
    }
    if ((!d1528) && u_is_con(s1527, &n_option_46_None)) {
      d1528 = 1;
      {
      }
    }
    if (!d1528) {
      u_no_case(s1527);
    }
  }
}

void p_check_46_unique_ids(u_value v_ns) {
  {
    u_value s1529 = f_list_46_repeated_1(v_ns);
    int d1530 = 0;
    if ((!d1530) && u_is_con(s1529, &n_option_46_Some)) {
      u_value v_n = u_field(s1529, 0);
      d1530 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, v_n), "check.TypeError");
      }
    }
    if ((!d1530) && u_is_con(s1529, &n_option_46_None)) {
      d1530 = 1;
      {
      }
    }
    if (!d1530) {
      u_no_case(s1529);
    }
  }
}

void p_check_46_same_length(u_value v_m, u_value v_n) {
  {
    if (u_truth(f_prelude_46__33__61__2(v_m, v_n))) {
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, v_m, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_arguments(u_value v_n, u_value v_k, u_value v_args) {
  {
    if (u_truth(f_prelude_46__33__61__2(f_list_46_length_1(v_args), v_k))) {
      {
        u_raise(u_mk_con(&n_check_46_SortArity, 3, v_n, v_k, f_list_46_length_1(v_args)), "check.TypeError");
      }
    }
  }
}

void p_check_46_normalize(u_value v_t, u_value *r_u, u_value *r_s) {
  {
    u_value v_us = NULL;
    u_value v_v = NULL;
    u_value s1531 = v_t;
    int d1532 = 0;
    if ((!d1532) && u_is_con(s1531, &n_syntax_46_THole)) {
      d1532 = 1;
      {
        p_check_46_fresh(r_u, r_s);
      }
    }
    if ((!d1532) && u_is_con(s1531, &n_syntax_46_TParam)) {
      d1532 = 1;
      {
        u_value t1533 = v_t;
        *r_u = t1533;
      }
    }
    if ((!d1532) && u_is_con(s1531, &n_syntax_46_TMeta)) {
      d1532 = 1;
      {
        u_value t1534 = v_t;
        *r_u = t1534;
      }
    }
    if ((!d1532) && u_is_con(s1531, &n_syntax_46_TFun)) {
      u_value v_args = u_field(s1531, 0);
      u_value v_r = u_field(s1531, 1);
      d1532 = 1;
      {
        p_check_46_normalizes(v_args, &v_us, r_s);
        p_check_46_normalize(v_r, &v_v, r_s);
        u_value t1535 = u_mk_con(&n_syntax_46_TFun, 2, v_us, v_v);
        *r_u = t1535;
      }
    }
    if ((!d1532) && u_is_con(s1531, &n_syntax_46_TTuple)) {
      u_value v_ts = u_field(s1531, 0);
      d1532 = 1;
      {
        p_check_46_normalizes(v_ts, &v_us, r_s);
        u_value t1536 = u_mk_con(&n_syntax_46_TTuple, 1, v_us);
        *r_u = t1536;
      }
    }
    if ((!d1532) && u_is_con(s1531, &n_syntax_46_TProc)) {
      u_value v_is = u_field(s1531, 0);
      u_value v_os = u_field(s1531, 1);
      u_value v_rs = u_field(s1531, 2);
      d1532 = 1;
      {
        p_check_46_normalizes(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs)), &v_us, r_s);
        u_value t1537 = u_mk_con(&n_syntax_46_TProc, 3, f_list_46_take_2(f_list_46_length_1(v_is), v_us), f_list_46_take_2(f_list_46_length_1(v_os), f_list_46_drop_2(f_list_46_length_1(v_is), v_us)), f_list_46_drop_2(u_add(f_list_46_length_1(v_is), f_list_46_length_1(v_os)), v_us));
        *r_u = t1537;
      }
    }
    if ((!d1532) && u_is_con(s1531, &n_syntax_46_TSort)) {
      u_value v_n = u_field(s1531, 0);
      u_value v_args = u_field(s1531, 1);
      d1532 = 1;
      {
        if (u_truth(f_check_46_basic_1(v_n))) {
          {
            p_check_46_arguments(v_n, u_mk_int(0), v_args);
            u_value t1538 = v_t;
            *r_u = t1538;
          }
        } else {
          {
            u_value s1540 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam1539, 1, *r_s));
            int d1541 = 0;
            if ((!d1541) && (u_is_con(s1540, &n_list_46_Cons) && u_is_con(u_field(s1540, 1), &n_list_46_Nil))) {
              u_value v_m = u_field(s1540, 0);
              d1541 = 1;
              {
                p_check_46_normalize_sort(v_m, v_args, r_u, r_s);
              }
            }
            if ((!d1541) && u_is_con(s1540, &n_list_46_Nil)) {
              d1541 = 1;
              {
                u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
              }
            }
            if (!d1541) {
              u_value v_ms = s1540;
              d1541 = 1;
              {
                u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ms), "check.TypeError");
              }
            }
            if (!d1541) {
              u_no_case(s1540);
            }
          }
        }
      }
    }
    if (!d1532) {
      u_no_case(s1531);
    }
  }
}

void p_check_46_normalize_sort(u_value v_n, u_value v_args, u_value *r_u, u_value *r_s) {
  {
    u_value v_us = NULL;
    u_value s1542 = f_ctx_46_data_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
    int d1543 = 0;
    if ((!d1543) && u_is_con(s1542, &n_option_46_Some)) {
      u_value v_k = u_field(s1542, 0);
      d1543 = 1;
      {
        p_check_46_arguments(v_n, v_k, v_args);
        p_check_46_normalizes(v_args, &v_us, r_s);
        u_value t1544 = u_mk_con(&n_syntax_46_TSort, 2, v_n, v_us);
        *r_u = t1544;
      }
    }
    if ((!d1543) && u_is_con(s1542, &n_option_46_None)) {
      d1543 = 1;
      {
        u_value s1545 = f_ctx_46_alias_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
        int d1546 = 0;
        if ((!d1546) && u_is_con(s1545, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s1545, 0), 0);
          u_value v_rhs = u_field(u_field(s1545, 0), 1);
          d1546 = 1;
          {
            p_check_46_arguments(v_n, f_list_46_length_1(v_ps), v_args);
            p_check_46_normalizes(v_args, &v_us, r_s);
            u_value t1547 = f_check_46_subst_2(v_rhs, f_list_46_zip_2(f_check_46_params_1(v_ps), v_us));
            *r_u = t1547;
          }
        }
        if ((!d1546) && u_is_con(s1545, &n_option_46_None)) {
          d1546 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
          }
        }
        if (!d1546) {
          u_no_case(s1545);
        }
      }
    }
    if (!d1543) {
      u_no_case(s1542);
    }
  }
}

void p_check_46_normalizes(u_value v_ts, u_value *r_us, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_rest = NULL;
    u_value s1548 = v_ts;
    int d1549 = 0;
    if ((!d1549) && u_is_con(s1548, &n_list_46_Nil)) {
      d1549 = 1;
      {
        u_value t1550 = &o_list_46_Nil;
        *r_us = t1550;
      }
    }
    if ((!d1549) && u_is_con(s1548, &n_list_46_Cons)) {
      u_value v_t = u_field(s1548, 0);
      u_value v_more = u_field(s1548, 1);
      d1549 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalizes(v_more, &v_rest, r_s);
        u_value t1551 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t1551;
      }
    }
    if (!d1549) {
      u_no_case(s1548);
    }
  }
}

void p_check_46_known(u_value v_t, u_value v_what, u_value *r_u, u_value *r_s) {
  {
    if (u_truth(f_check_46_holes_1(v_t))) {
      {
        u_raise(u_mk_con(&n_check_46_MissingType, 1, v_what), "check.TypeError");
      }
    }
    p_check_46_normalize(v_t, r_u, r_s);
  }
}

void p_check_46_knowns(u_value v_ts, u_value v_what, u_value *r_us, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_rest = NULL;
    u_value s1552 = v_ts;
    int d1553 = 0;
    if ((!d1553) && u_is_con(s1552, &n_list_46_Nil)) {
      d1553 = 1;
      {
        u_value t1554 = &o_list_46_Nil;
        *r_us = t1554;
      }
    }
    if ((!d1553) && u_is_con(s1552, &n_list_46_Cons)) {
      u_value v_t = u_field(s1552, 0);
      u_value v_more = u_field(s1552, 1);
      d1553 = 1;
      {
        p_check_46_known(v_t, v_what, &v_u, r_s);
        p_check_46_knowns(v_more, v_what, &v_rest, r_s);
        u_value t1555 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t1555;
      }
    }
    if (!d1553) {
      u_no_case(s1552);
    }
  }
}

void p_check_46_formals(u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    p_check_46_unique(f_syntax_46_names_1(v_xs));
    p_check_46_normalize_vars(v_xs, r_ys, r_s);
  }
}

void p_check_46_normalize_vars(u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_rest = NULL;
    u_value s1556 = v_xs;
    int d1557 = 0;
    if ((!d1557) && u_is_con(s1556, &n_list_46_Nil)) {
      d1557 = 1;
      {
        u_value t1558 = &o_list_46_Nil;
        *r_ys = t1558;
      }
    }
    if ((!d1557) && (u_is_con(s1556, &n_list_46_Cons) && u_is_con(u_field(s1556, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s1556, 0), 0);
      u_value v_t = u_field(u_field(s1556, 0), 1);
      u_value v_more = u_field(s1556, 1);
      d1557 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalize_vars(v_more, &v_rest, r_s);
        u_value t1559 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t1559;
      }
    }
    if (!d1557) {
      u_no_case(s1556);
    }
  }
}

void p_check_46_signature(u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    p_check_46_unique(f_syntax_46_names_1(v_xs));
    p_check_46_known_vars(v_xs, r_ys, r_s);
  }
}

void p_check_46_known_vars(u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_rest = NULL;
    u_value s1560 = v_xs;
    int d1561 = 0;
    if ((!d1561) && u_is_con(s1560, &n_list_46_Nil)) {
      d1561 = 1;
      {
        u_value t1562 = &o_list_46_Nil;
        *r_ys = t1562;
      }
    }
    if ((!d1561) && (u_is_con(s1560, &n_list_46_Cons) && u_is_con(u_field(s1560, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s1560, 0), 0);
      u_value v_t = u_field(u_field(s1560, 0), 1);
      u_value v_more = u_field(s1560, 1);
      d1561 = 1;
      {
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_known_vars(v_more, &v_rest, r_s);
        u_value t1563 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t1563;
      }
    }
    if (!d1561) {
      u_no_case(s1560);
    }
  }
}

void p_check_46_resolve_name(u_value v_s, u_value v_env, u_value v_n, u_value v_k, u_value *r_m, u_value *r_sc) {
  {
    u_value s1564 = f_check_46_candidates_3(v_s, v_env, v_n);
    int d1565 = 0;
    if ((!d1565) && u_is_con(s1564, &n_list_46_Nil)) {
      d1565 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if ((!d1565) && (u_is_con(s1564, &n_list_46_Cons) && u_is_con(u_field(s1564, 1), &n_list_46_Nil))) {
      u_value v_m1 = u_field(u_field(s1564, 0), 0);
      u_value v_one = u_field(u_field(s1564, 0), 1);
      d1565 = 1;
      {
        u_value t1566 = v_m1;
        u_value t1567 = v_one;
        *r_m = t1566;
        *r_sc = t1567;
      }
    }
    if (!d1565) {
      u_value v_cs = s1564;
      d1565 = 1;
      {
        if (u_truth(u_lt(v_k, u_mk_int(0)))) {
          {
            u_raise(u_mk_con(&n_check_46_OverloadedUnapplied, 1, v_n), "check.TypeError");
          }
        }
        u_value s1568 = f_check_46_with_arity_2(v_cs, v_k);
        int d1569 = 0;
        if ((!d1569) && (u_is_con(s1568, &n_list_46_Cons) && u_is_con(u_field(s1568, 1), &n_list_46_Nil))) {
          u_value v_m1 = u_field(u_field(s1568, 0), 0);
          u_value v_one = u_field(u_field(s1568, 0), 1);
          d1569 = 1;
          {
            u_value t1570 = v_m1;
            u_value t1571 = v_one;
            *r_m = t1570;
            *r_sc = t1571;
          }
        }
        if ((!d1569) && u_is_con(s1568, &n_list_46_Nil)) {
          d1569 = 1;
          {
            u_raise(u_mk_con(&n_check_46_WrongArity, 2, v_n, v_k), "check.TypeError");
          }
        }
        if (!d1569) {
          u_value v_more = s1568;
          d1569 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, f_list_46_firsts_1(v_more)), "check.TypeError");
          }
        }
        if (!d1569) {
          u_no_case(s1568);
        }
      }
    }
    if (!d1565) {
      u_no_case(s1564);
    }
  }
}

void p_check_46_use(u_value v_n, u_value v_a, u_value v_sc, u_value *r_x, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_w = NULL;
    p_check_46_instantiate(v_sc, &v_u, r_s);
    p_check_46_normalize(v_a, &v_w, r_s);
    p_check_46_unify(v_u, v_w, r_s);
    u_value t1572 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_u);
    *r_x = t1572;
  }
}

void p_check_46_infer(u_value v_env, u_value v_e, u_value *r_e1, u_value *r_t, u_value *r_s) {
  {
    u_value v_sc = NULL;
    u_value v_m = NULL;
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_ys = NULL;
    u_value v_zs = NULL;
    u_value v_b = NULL;
    u_value v_c = NULL;
    u_value v_l = NULL;
    u_value v_u = NULL;
    u_value v_w = NULL;
    u_value v_es = NULL;
    u_value v_ts = NULL;
    u_value v_bs = NULL;
    u_value v_y = NULL;
    u_value s1573 = v_e;
    int d1574 = 0;
    if ((!d1574) && u_is_con(s1573, &n_syntax_46_ELit)) {
      u_value v_v = u_field(s1573, 0);
      d1574 = 1;
      {
        u_value t1575 = v_e;
        u_value t1576 = f_check_46_literal_1(v_v);
        *r_e1 = t1575;
        *r_t = t1576;
      }
    }
    if ((!d1574) && (u_is_con(s1573, &n_syntax_46_EVar) && u_is_con(u_field(s1573, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s1573, 0), 0);
      u_value v_a = u_field(u_field(s1573, 0), 1);
      d1574 = 1;
      {
        p_check_46_resolve_name(*r_s, v_env, v_n, u_mk_int(-1), &v_m, &v_sc);
        p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
        u_value t1577 = u_mk_con(&n_syntax_46_EVar, 1, v_x);
        u_value t1578 = f_syntax_46_Var_46_typ_1(v_x);
        *r_e1 = t1577;
        *r_t = t1578;
      }
    }
    if ((!d1574) && u_is_con(s1573, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s1573, 0);
      u_value v_args = u_field(s1573, 1);
      d1574 = 1;
      {
        p_check_46_application(v_env, v_f, v_args, r_e1, r_t, r_s);
      }
    }
    if ((!d1574) && u_is_con(s1573, &n_syntax_46_ELam)) {
      u_value v_bound = u_field(s1573, 0);
      u_value v_body = u_field(s1573, 1);
      d1574 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t1579 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, v_b);
        u_value t1580 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), v_u);
        *r_e1 = t1579;
        *r_t = t1580;
      }
    }
    if ((!d1574) && u_is_con(s1573, &n_syntax_46_EProc)) {
      u_value v_is = u_field(s1573, 0);
      u_value v_os = u_field(s1573, 1);
      u_value v_rs = u_field(s1573, 2);
      u_value v_body = u_field(s1573, 3);
      d1574 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))));
        p_check_46_formals(v_is, &v_xs, r_s);
        p_check_46_formals(v_os, &v_ys, r_s);
        p_check_46_formals(v_rs, &v_zs, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(f_list_46__43__43__2(v_xs, f_list_46__43__43__2(v_ys, v_zs))), v_env), f_syntax_46_names_1(f_list_46__43__43__2(v_ys, v_zs)), v_body, &v_y, r_s);
        u_value t1581 = u_mk_con(&n_syntax_46_EProc, 4, v_xs, v_ys, v_zs, v_y);
        u_value t1582 = u_mk_con(&n_syntax_46_TProc, 3, f_syntax_46_types_1(v_xs), f_syntax_46_types_1(v_ys), f_syntax_46_types_1(v_zs));
        *r_e1 = t1581;
        *r_t = t1582;
      }
    }
    if ((!d1574) && u_is_con(s1573, &n_syntax_46_ELet)) {
      u_value v_bound = u_field(s1573, 0);
      u_value v_body = u_field(s1573, 1);
      d1574 = 1;
      {
        p_check_46_formals(f_list_46_firsts_1(v_bound), &v_xs, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_bound), f_syntax_46_types_1(v_xs), &v_es, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_generalized_3(f_check_46_State_46_sub_1(*r_s), v_env, v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t1583 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        u_value t1584 = v_u;
        *r_e1 = t1583;
        *r_t = t1584;
      }
    }
    if ((!d1574) && u_is_con(s1573, &n_syntax_46_EIte)) {
      u_value v_cond = u_field(s1573, 0);
      u_value v_left = u_field(s1573, 1);
      u_value v_right = u_field(s1573, 2);
      d1574 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(v_env, v_left, &v_l, &v_u, r_s);
        p_check_46_check(v_env, v_right, v_u, &v_b, r_s);
        u_value t1585 = u_mk_con(&n_syntax_46_EIte, 3, v_c, v_l, v_b);
        u_value t1586 = v_u;
        *r_e1 = t1585;
        *r_t = t1586;
      }
    }
    if ((!d1574) && u_is_con(s1573, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s1573, 0);
      u_value v_bound = u_field(s1573, 1);
      u_value v_lists = u_field(s1573, 2);
      u_value v_cond = u_field(s1573, 3);
      d1574 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_same_length(f_list_46_length_1(v_xs), f_list_46_length_1(v_lists));
        p_check_46_generators(v_env, v_xs, v_lists, &v_es, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_result, &v_b, &v_u, r_s);
        u_value t1587 = u_mk_con(&n_syntax_46_EComp, 4, v_b, v_xs, v_es, v_c);
        u_value t1588 = f_check_46_tlist_1(v_u);
        *r_e1 = t1587;
        *r_t = t1588;
      }
    }
    if ((!d1574) && u_is_con(s1573, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s1573, 0);
      d1574 = 1;
      {
        p_check_46_infers(v_env, v_items, &v_es, &v_ts, r_s);
        u_value t1589 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
        u_value t1590 = u_mk_con(&n_syntax_46_TTuple, 1, v_ts);
        *r_e1 = t1589;
        *r_t = t1590;
      }
    }
    if ((!d1574) && u_is_con(s1573, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s1573, 0);
      u_value v_a = u_field(s1573, 1);
      d1574 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_normalize(v_a, &v_w, r_s);
        p_check_46_unify(v_u, v_w, r_s);
        p_check_46_fresh(r_t, r_s);
        u_value t1591 = u_mk_con(&n_syntax_46_ERaise, 2, v_c, v_u);
        *r_e1 = t1591;
      }
    }
    if ((!d1574) && u_is_con(s1573, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s1573, 0);
      u_value v_cases = u_field(s1573, 1);
      d1574 = 1;
      {
        p_check_46_fresh(r_t, r_s);
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_expr_branches(v_env, v_cases, v_u, *r_t, &v_bs, r_s);
        u_value t1592 = &o_list_46_Nil;
        u_value *e1593 = &t1592;
        u_value l1594 = v_bs;
        while (u_is_con(l1594, &n_list_46_Cons)) {
          u_value v_br = u_field(l1594, 0);
          if (u_truth(u_mk_bool(1))) {
            e1593 = u_append(e1593, f_check_46_case_of_1(v_br));
          }
          l1594 = u_field(l1594, 1);
        }
        p_check_46_coverage(*r_s, v_u, t1592);
        u_value t1595 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, v_bs);
        *r_e1 = t1595;
      }
    }
    if (!d1574) {
      u_no_case(s1573);
    }
  }
}

void p_check_46_application(u_value v_env, u_value v_f, u_value v_args, u_value *r_e1, u_value *r_t, u_value *r_s) {
  {
    u_value v_sc = NULL;
    u_value v_m = NULL;
    u_value v_x = NULL;
    u_value v_f1 = NULL;
    u_value v_u = NULL;
    u_value v_args1 = NULL;
    u_value s1596 = v_f;
    int d1597 = 0;
    if ((!d1597) && (u_is_con(s1596, &n_syntax_46_EVar) && (u_is_con(u_field(s1596, 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(s1596, 0), 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(u_field(s1596, 0), 0), 0), &n_option_46_None))))) {
      u_value v_b = u_field(u_field(u_field(s1596, 0), 0), 1);
      u_value v_a = u_field(u_field(s1596, 0), 1);
      u_value t1598 = f_prelude_46__62__2(u_str_size(v_b), u_mk_int(1));
      if (u_truth(t1598)) {
        t1598 = u_eq(u_str_get(v_b, u_mk_int(0)), u_mk_char(46));
      }
      if (u_truth(t1598)) {
        d1597 = 1;
        {
          p_check_46_field_access(v_env, v_b, v_a, v_args, r_e1, r_t, r_s);
        }
      }
    }
    if ((!d1597) && (u_is_con(s1596, &n_syntax_46_EVar) && u_is_con(u_field(s1596, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s1596, 0), 0);
      u_value v_a = u_field(u_field(s1596, 0), 1);
      d1597 = 1;
      {
        if (u_truth(f_prelude_46__62__2(f_list_46_length_1(f_check_46_candidates_3(*r_s, v_env, v_n)), u_mk_int(1)))) {
          {
            p_check_46_resolve_name(*r_s, v_env, v_n, f_list_46_length_1(v_args), &v_m, &v_sc);
            p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
            p_check_46_apply(v_env, f_syntax_46_Var_46_typ_1(v_x), v_args, &v_args1, r_t, r_s);
            u_value t1599 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), v_args1);
            *r_e1 = t1599;
          }
        } else {
          {
            p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
            p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
            u_value t1600 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
            *r_e1 = t1600;
          }
        }
      }
    }
    if (!d1597) {
      d1597 = 1;
      {
        p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
        p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
        u_value t1601 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
        *r_e1 = t1601;
      }
    }
    if (!d1597) {
      u_no_case(s1596);
    }
  }
}

void p_check_46_field_access(u_value v_env, u_value v_f, u_value v_a, u_value v_args, u_value *r_e1, u_value *r_t, u_value *r_s) {
  {
    u_value v_arg1 = NULL;
    u_value v_u = NULL;
    u_value v_rest1 = NULL;
    u_value v_us = NULL;
    u_value v_acc = NULL;
    u_value v_x = NULL;
    u_value s1602 = v_args;
    int d1603 = 0;
    if ((!d1603) && u_is_con(s1602, &n_list_46_Cons)) {
      u_value v_arg = u_field(s1602, 0);
      u_value v_rest = u_field(s1602, 1);
      d1603 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_arg1, &v_u, r_s);
        u_value s1604 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u);
        int d1605 = 0;
        if ((!d1605) && (u_is_con(s1604, &n_syntax_46_TSort) && (u_is_con(u_field(s1604, 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(s1604, 0), 0), &n_option_46_Some)))) {
          u_value v_m = u_field(u_field(u_field(s1604, 0), 0), 0);
          u_value v_tn = u_field(u_field(s1604, 0), 1);
          d1605 = 1;
          {
            u_value t1606 = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), u_concat(v_tn, v_f));
            v_acc = t1606;
          }
        }
        if (!d1605) {
          u_value v_v = s1604;
          d1605 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownFieldType, 2, v_f, v_v), "check.TypeError");
          }
        }
        if (!d1605) {
          u_no_case(s1604);
        }
        u_value s1607 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(*r_s), v_acc);
        int d1608 = 0;
        if ((!d1608) && (u_is_con(s1607, &n_list_46_Cons) && u_is_con(u_field(s1607, 1), &n_list_46_Nil))) {
          u_value v_sc = u_field(s1607, 0);
          d1608 = 1;
          {
            p_check_46_use(v_acc, v_a, v_sc, &v_x, r_s);
          }
        }
        if (!d1608) {
          d1608 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownField, 1, v_acc), "check.TypeError");
          }
        }
        if (!d1608) {
          u_no_case(s1607);
        }
        p_check_46_infers(v_env, v_rest, &v_rest1, &v_us, r_s);
        p_check_46_fresh(r_t, r_s);
        p_check_46_unify(f_syntax_46_Var_46_typ_1(v_x), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_u, v_us), *r_t), r_s);
        u_value t1609 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), u_mk_con(&n_list_46_Cons, 2, v_arg1, v_rest1));
        *r_e1 = t1609;
      }
    }
    if (!d1603) {
      d1603 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, u_mk_int(1), f_list_46_length_1(v_args)), "check.TypeError");
      }
    }
    if (!d1603) {
      u_no_case(s1602);
    }
  }
}

void p_check_46_apply(u_value v_env, u_value v_f, u_value v_args, u_value *r_args1, u_value *r_r, u_value *r_s) {
  {
    u_value v_ts = NULL;
    p_check_46_fresh(r_r, r_s);
    p_check_46_infers(v_env, v_args, r_args1, &v_ts, r_s);
    p_check_46_unify(v_f, f_check_46_fun_2(v_ts, *r_r), r_s);
  }
}

void p_check_46_check(u_value v_env, u_value v_e, u_value v_t, u_value *r_e1, u_value *r_s) {
  {
    u_value v_u = NULL;
    p_check_46_infer(v_env, v_e, r_e1, &v_u, r_s);
    p_check_46_unify(v_u, v_t, r_s);
  }
}

void p_check_46_checks(u_value v_env, u_value v_es, u_value v_ts, u_value *r_es1, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value v_rest = NULL;
    u_value s1610 = u_mk_tuple(2, v_es, v_ts);
    int d1611 = 0;
    if ((!d1611) && (u_is_con(u_field(s1610, 0), &n_list_46_Cons) && u_is_con(u_field(s1610, 1), &n_list_46_Cons))) {
      u_value v_e = u_field(u_field(s1610, 0), 0);
      u_value v_more = u_field(u_field(s1610, 0), 1);
      u_value v_t = u_field(u_field(s1610, 1), 0);
      u_value v_ts1 = u_field(u_field(s1610, 1), 1);
      d1611 = 1;
      {
        p_check_46_check(v_env, v_e, v_t, &v_e1, r_s);
        p_check_46_checks(v_env, v_more, v_ts1, &v_rest, r_s);
        u_value t1612 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        *r_es1 = t1612;
      }
    }
    if ((!d1611) && (u_is_con(u_field(s1610, 0), &n_list_46_Nil) && u_is_con(u_field(s1610, 1), &n_list_46_Nil))) {
      d1611 = 1;
      {
        u_value t1613 = &o_list_46_Nil;
        *r_es1 = t1613;
      }
    }
    if (!d1611) {
      d1611 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_es)), "check.TypeError");
      }
    }
    if (!d1611) {
      u_no_case(s1610);
    }
  }
}

void p_check_46_infers(u_value v_env, u_value v_es, u_value *r_es1, u_value *r_ts, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value v_us = NULL;
    u_value s1614 = v_es;
    int d1615 = 0;
    if ((!d1615) && u_is_con(s1614, &n_list_46_Nil)) {
      d1615 = 1;
      {
        u_value t1616 = &o_list_46_Nil;
        u_value t1617 = &o_list_46_Nil;
        *r_es1 = t1616;
        *r_ts = t1617;
      }
    }
    if ((!d1615) && u_is_con(s1614, &n_list_46_Cons)) {
      u_value v_e = u_field(s1614, 0);
      u_value v_more = u_field(s1614, 1);
      d1615 = 1;
      {
        p_check_46_infer(v_env, v_e, &v_e1, &v_t, r_s);
        p_check_46_infers(v_env, v_more, &v_rest, &v_us, r_s);
        u_value t1618 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        u_value t1619 = u_mk_con(&n_list_46_Cons, 2, v_t, v_us);
        *r_es1 = t1618;
        *r_ts = t1619;
      }
    }
    if (!d1615) {
      u_no_case(s1614);
    }
  }
}

void p_check_46_generators(u_value v_env, u_value v_xs, u_value v_lists, u_value *r_lists1, u_value *r_s) {
  {
    u_value v_l = NULL;
    u_value v_rest = NULL;
    u_value s1620 = u_mk_tuple(2, v_xs, v_lists);
    int d1621 = 0;
    if ((!d1621) && (u_is_con(u_field(s1620, 0), &n_list_46_Cons) && u_is_con(u_field(s1620, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s1620, 0), 0);
      u_value v_ys = u_field(u_field(s1620, 0), 1);
      u_value v_e = u_field(u_field(s1620, 1), 0);
      u_value v_es = u_field(u_field(s1620, 1), 1);
      d1621 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tlist_1(f_syntax_46_Var_46_typ_1(v_x)), &v_l, r_s);
        p_check_46_generators(f_list_46__43__43__2(f_check_46_bind_1(u_mk_con(&n_list_46_Cons, 2, v_x, &o_list_46_Nil)), v_env), v_ys, v_es, &v_rest, r_s);
        u_value t1622 = u_mk_con(&n_list_46_Cons, 2, v_l, v_rest);
        *r_lists1 = t1622;
      }
    }
    if (!d1621) {
      d1621 = 1;
      {
        u_value t1623 = &o_list_46_Nil;
        *r_lists1 = t1623;
      }
    }
    if (!d1621) {
      u_no_case(s1620);
    }
  }
}

void p_check_46_expr_branches(u_value v_env, u_value v_cases, u_value v_t, u_value v_r, u_value *r_cases1, u_value *r_s) {
  {
    u_value v_p1 = NULL;
    u_value v_xs = NULL;
    u_value v_g1 = NULL;
    u_value v_b1 = NULL;
    u_value v_rest = NULL;
    u_value s1624 = v_cases;
    int d1625 = 0;
    if ((!d1625) && u_is_con(s1624, &n_list_46_Nil)) {
      d1625 = 1;
      {
        u_value t1626 = &o_list_46_Nil;
        *r_cases1 = t1626;
      }
    }
    if ((!d1625) && (u_is_con(s1624, &n_list_46_Cons) && u_is_con(u_field(s1624, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s1624, 0), 0);
      u_value v_g = u_field(u_field(s1624, 0), 1);
      u_value v_b = u_field(u_field(s1624, 0), 2);
      u_value v_more = u_field(s1624, 1);
      d1625 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_b, v_r, &v_b1, r_s);
        p_check_46_expr_branches(v_env, v_more, v_t, v_r, &v_rest, r_s);
        u_value t1627 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t1627;
      }
    }
    if (!d1625) {
      u_no_case(s1624);
    }
  }
}

void p_check_46_guard(u_value v_env, u_value v_g, u_value *r_g1, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value s1628 = v_g;
    int d1629 = 0;
    if ((!d1629) && u_is_con(s1628, &n_option_46_Some)) {
      u_value v_e = u_field(s1628, 0);
      d1629 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tbool_0(), &v_e1, r_s);
        u_value t1630 = u_mk_con(&n_option_46_Some, 1, v_e1);
        *r_g1 = t1630;
      }
    }
    if ((!d1629) && u_is_con(s1628, &n_option_46_None)) {
      d1629 = 1;
      {
        u_value t1631 = &o_option_46_None;
        *r_g1 = t1631;
      }
    }
    if (!d1629) {
      u_no_case(s1628);
    }
  }
}

void p_check_46_coverage(u_value v_s, u_value v_t, u_value v_cs) {
  {
    p_check_46_reachable(v_s, v_t, &o_list_46_Nil, v_cs);
    u_value t1632 = &o_list_46_Nil;
    u_value *e1633 = &t1632;
    u_value l1634 = f_check_46_unguarded_1(v_cs);
    while (u_is_con(l1634, &n_list_46_Cons)) {
      u_value v_p = u_field(l1634, 0);
      if (u_truth(u_mk_bool(1))) {
        e1633 = u_append(e1633, u_mk_con(&n_list_46_Cons, 2, v_p, &o_list_46_Nil));
      }
      l1634 = u_field(l1634, 1);
    }
    u_value s1635 = f_check_46_missing_4(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t1632, u_mk_con(&n_list_46_Cons, 2, v_t, &o_list_46_Nil));
    int d1636 = 0;
    if ((!d1636) && (u_is_con(s1635, &n_option_46_Some) && u_is_con(u_field(s1635, 0), &n_list_46_Cons))) {
      u_value v_w = u_field(u_field(s1635, 0), 0);
      d1636 = 1;
      {
        u_raise(u_mk_con(&n_check_46_NotExhaustive, 1, v_w), "check.TypeError");
      }
    }
    if (!d1636) {
      d1636 = 1;
      {
      }
    }
    if (!d1636) {
      u_no_case(s1635);
    }
  }
}

u_value f_check_46_unguarded_1(u_value v_cs) {
  u_value s1637 = v_cs;
  u_value t1638 = NULL;
  if ((!t1638) && u_is_con(s1637, &n_list_46_Nil)) {
    t1638 = &o_list_46_Nil;
  }
  if ((!t1638) && (u_is_con(s1637, &n_list_46_Cons) && u_equals(u_field(u_field(s1637, 0), 1), u_mk_bool(1)))) {
    u_value v_p = u_field(u_field(s1637, 0), 0);
    u_value v_rest = u_field(s1637, 1);
    t1638 = u_mk_con(&n_list_46_Cons, 2, v_p, f_check_46_unguarded_1(v_rest));
  }
  if ((!t1638) && (u_is_con(s1637, &n_list_46_Cons) && u_equals(u_field(u_field(s1637, 0), 1), u_mk_bool(0)))) {
    u_value v_rest = u_field(s1637, 1);
    t1638 = f_check_46_unguarded_1(v_rest);
  }
  if (!t1638) {
    u_no_case(s1637);
  }
  return t1638;
}

void p_check_46_reachable(u_value v_s, u_value v_t, u_value v_before, u_value v_cs) {
  {
    u_value s1639 = v_cs;
    int d1640 = 0;
    if ((!d1640) && u_is_con(s1639, &n_list_46_Nil)) {
      d1640 = 1;
      {
      }
    }
    if ((!d1640) && u_is_con(s1639, &n_list_46_Cons)) {
      u_value v_p = u_field(u_field(s1639, 0), 0);
      u_value v_open = u_field(u_field(s1639, 0), 1);
      u_value v_rest = u_field(s1639, 1);
      d1640 = 1;
      {
        u_value t1641 = &o_list_46_Nil;
        u_value *e1642 = &t1641;
        u_value l1643 = v_before;
        while (u_is_con(l1643, &n_list_46_Cons)) {
          u_value v_q = u_field(l1643, 0);
          if (u_truth(u_mk_bool(1))) {
            e1642 = u_append(e1642, u_mk_con(&n_list_46_Cons, 2, v_q, &o_list_46_Nil));
          }
          l1643 = u_field(l1643, 1);
        }
        if (u_truth(f_prelude_46_not_1(f_check_46_useful_5(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t1641, u_mk_con(&n_list_46_Cons, 2, v_p, &o_list_46_Nil), u_mk_con(&n_list_46_Cons, 2, v_t, &o_list_46_Nil))))) {
          {
            u_raise(u_mk_con(&n_check_46_Unreachable, 1, v_p), "check.TypeError");
          }
        }
        if (u_truth(v_open)) {
          {
            p_check_46_reachable(v_s, v_t, f_list_46__43__43__2(v_before, u_mk_con(&n_list_46_Cons, 2, v_p, &o_list_46_Nil)), v_rest);
          }
        } else {
          {
            p_check_46_reachable(v_s, v_t, v_before, v_rest);
          }
        }
      }
    }
    if (!d1640) {
      u_no_case(s1639);
    }
  }
}

void p_check_46_pattern(u_value v_p, u_value v_t, u_value *r_p1, u_value *r_xs, u_value *r_s) {
  {
    p_check_46_binders(v_p, v_t, r_p1, r_xs, r_s);
    p_check_46_unique(f_syntax_46_names_1(*r_xs));
  }
}

void p_check_46_binders(u_value v_p, u_value v_t, u_value *r_p1, u_value *r_xs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_m = NULL;
    u_value v_qs = NULL;
    u_value v_ts = NULL;
    u_value v_k = NULL;
    u_value s1644 = v_p;
    int d1645 = 0;
    if ((!d1645) && u_is_con(s1644, &n_syntax_46_PWild)) {
      d1645 = 1;
      {
        u_value t1646 = v_p;
        u_value t1647 = &o_list_46_Nil;
        *r_p1 = t1646;
        *r_xs = t1647;
      }
    }
    if ((!d1645) && u_is_con(s1644, &n_syntax_46_PLit)) {
      u_value v_v = u_field(s1644, 0);
      d1645 = 1;
      {
        p_check_46_unify(f_check_46_literal_1(v_v), v_t, r_s);
        u_value t1648 = v_p;
        u_value t1649 = &o_list_46_Nil;
        *r_p1 = t1648;
        *r_xs = t1649;
      }
    }
    if ((!d1645) && (u_is_con(s1644, &n_syntax_46_PVar) && u_is_con(u_field(s1644, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s1644, 0), 0);
      u_value v_a = u_field(u_field(s1644, 0), 1);
      d1645 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam1650, 1, *r_s)), &o_list_46_Nil))) {
          {
            p_check_46_binders(u_mk_con(&n_syntax_46_PApp, 2, v_n, &o_list_46_Nil), v_t, r_p1, r_xs, r_s);
          }
        } else {
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_u, v_t, r_s);
            u_value t1651 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u));
            u_value t1652 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), &o_list_46_Nil);
            *r_p1 = t1651;
            *r_xs = t1652;
          }
        }
      }
    }
    if ((!d1645) && u_is_con(s1644, &n_syntax_46_PApp)) {
      u_value v_n = u_field(s1644, 0);
      u_value v_args = u_field(s1644, 1);
      d1645 = 1;
      {
        u_value s1654 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam1653, 1, *r_s));
        int d1655 = 0;
        if ((!d1655) && (u_is_con(s1654, &n_list_46_Cons) && u_is_con(u_field(s1654, 1), &n_list_46_Nil))) {
          u_value v_k1 = u_field(s1654, 0);
          d1655 = 1;
          {
            u_value t1656 = v_k1;
            v_k = t1656;
          }
        }
        if ((!d1655) && u_is_con(s1654, &n_list_46_Nil)) {
          d1655 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d1655) {
          u_value v_ks = s1654;
          d1655 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ks), "check.TypeError");
          }
        }
        if (!d1655) {
          u_no_case(s1654);
        }
        u_value s1657 = f_ctx_46_constr_of_2(f_check_46_State_46_ctx_1(*r_s), v_k);
        int d1658 = 0;
        if ((!d1658) && u_is_con(s1657, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s1657, 0), 0);
          u_value v_ks = u_field(u_field(s1657, 0), 1);
          u_value v_r = u_field(u_field(s1657, 0), 2);
          d1658 = 1;
          {
            p_check_46_same_length(f_list_46_length_1(v_ks), f_list_46_length_1(v_args));
            p_check_46_instance(v_ps, &v_m, r_s);
            p_check_46_unify(f_check_46_subst_2(v_r, v_m), v_t, r_s);
            u_value t1659 = &o_list_46_Nil;
            u_value *e1660 = &t1659;
            u_value l1661 = v_ks;
            while (u_is_con(l1661, &n_list_46_Cons)) {
              u_value v_kt = u_field(l1661, 0);
              if (u_truth(u_mk_bool(1))) {
                e1660 = u_append(e1660, f_check_46_subst_2(v_kt, v_m));
              }
              l1661 = u_field(l1661, 1);
            }
            p_check_46_binder_list(v_args, t1659, &v_qs, r_xs, r_s);
            u_value t1662 = u_mk_con(&n_syntax_46_PApp, 2, v_k, v_qs);
            *r_p1 = t1662;
          }
        }
        if ((!d1658) && u_is_con(s1657, &n_option_46_None)) {
          d1658 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d1658) {
          u_no_case(s1657);
        }
      }
    }
    if ((!d1645) && u_is_con(s1644, &n_syntax_46_PTuple)) {
      u_value v_items = u_field(s1644, 0);
      d1645 = 1;
      {
        p_check_46_fresh_list(v_items, &v_ts, r_s);
        p_check_46_unify(u_mk_con(&n_syntax_46_TTuple, 1, v_ts), v_t, r_s);
        p_check_46_binder_list(v_items, v_ts, &v_qs, r_xs, r_s);
        u_value t1663 = u_mk_con(&n_syntax_46_PTuple, 1, v_qs);
        *r_p1 = t1663;
      }
    }
    if (!d1645) {
      u_no_case(s1644);
    }
  }
}

void p_check_46_binder_list(u_value v_ps, u_value v_ts, u_value *r_qs, u_value *r_xs, u_value *r_s) {
  {
    u_value v_q = NULL;
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value v_zs = NULL;
    u_value s1664 = u_mk_tuple(2, v_ps, v_ts);
    int d1665 = 0;
    if ((!d1665) && (u_is_con(u_field(s1664, 0), &n_list_46_Cons) && u_is_con(u_field(s1664, 1), &n_list_46_Cons))) {
      u_value v_p = u_field(u_field(s1664, 0), 0);
      u_value v_more = u_field(u_field(s1664, 0), 1);
      u_value v_t = u_field(u_field(s1664, 1), 0);
      u_value v_ts1 = u_field(u_field(s1664, 1), 1);
      d1665 = 1;
      {
        p_check_46_binders(v_p, v_t, &v_q, &v_ys, r_s);
        p_check_46_binder_list(v_more, v_ts1, &v_rest, &v_zs, r_s);
        u_value t1666 = u_mk_con(&n_list_46_Cons, 2, v_q, v_rest);
        u_value t1667 = f_list_46__43__43__2(v_ys, v_zs);
        *r_qs = t1666;
        *r_xs = t1667;
      }
    }
    if ((!d1665) && (u_is_con(u_field(s1664, 0), &n_list_46_Nil) && u_is_con(u_field(s1664, 1), &n_list_46_Nil))) {
      d1665 = 1;
      {
        u_value t1668 = &o_list_46_Nil;
        u_value t1669 = &o_list_46_Nil;
        *r_qs = t1668;
        *r_xs = t1669;
      }
    }
    if (!d1665) {
      d1665 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_ps)), "check.TypeError");
      }
    }
    if (!d1665) {
      u_no_case(s1664);
    }
  }
}

void p_check_46_variable(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s1670 = v_x;
    int d1671 = 0;
    if ((!d1671) && u_is_con(s1670, &n_syntax_46_Var)) {
      u_value v_n = u_field(s1670, 0);
      u_value v_a = u_field(s1670, 1);
      d1671 = 1;
      {
        if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, v_assignable)))) {
          {
            u_raise(u_mk_con(&n_check_46_NotAssignable, 1, v_n), "check.TypeError");
          }
        }
        u_value s1672 = f_list_46_lookup_2(v_n, v_env);
        int d1673 = 0;
        if ((!d1673) && u_is_con(s1672, &n_option_46_Some)) {
          u_value v_t = u_field(u_field(s1672, 0), 1);
          d1673 = 1;
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_t, v_u, r_s);
            u_value t1674 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_t);
            *r_y = t1674;
          }
        }
        if ((!d1673) && u_is_con(s1672, &n_option_46_None)) {
          d1673 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
          }
        }
        if (!d1673) {
          u_no_case(s1672);
        }
      }
    }
    if (!d1671) {
      u_no_case(s1670);
    }
  }
}

void p_check_46_variables(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s1675 = v_xs;
    int d1676 = 0;
    if ((!d1676) && u_is_con(s1675, &n_list_46_Nil)) {
      d1676 = 1;
      {
        u_value t1677 = &o_list_46_Nil;
        *r_ys = t1677;
      }
    }
    if ((!d1676) && u_is_con(s1675, &n_list_46_Cons)) {
      u_value v_x = u_field(s1675, 0);
      u_value v_more = u_field(s1675, 1);
      d1676 = 1;
      {
        p_check_46_variable(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_variables(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t1678 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t1678;
      }
    }
    if (!d1676) {
      u_no_case(s1675);
    }
  }
}

void p_check_46_statement(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_es = NULL;
    u_value v_c = NULL;
    u_value v_t = NULL;
    u_value v_u = NULL;
    u_value v_l = NULL;
    u_value v_r = NULL;
    u_value v_xs = NULL;
    u_value v_bs = NULL;
    u_value v_hs = NULL;
    u_value s1679 = v_x;
    int d1680 = 0;
    if ((!d1680) && u_is_con(s1679, &n_syntax_46_SSkip)) {
      d1680 = 1;
      {
        u_value t1681 = v_x;
        *r_y = t1681;
      }
    }
    if ((!d1680) && u_is_con(s1679, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s1679, 0);
      d1680 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)));
        p_check_46_variables(v_env, v_assignable, f_list_46_firsts_1(v_pairs), &v_ys, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_pairs), f_syntax_46_types_1(v_ys), &v_es, r_s);
        u_value t1682 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_zip_2(v_ys, v_es));
        *r_y = t1682;
      }
    }
    if ((!d1680) && u_is_con(s1679, &n_syntax_46_SBlock)) {
      u_value v_locals = u_field(s1679, 0);
      u_value v_body = u_field(s1679, 1);
      d1680 = 1;
      {
        p_check_46_untyped(v_locals, f_check_46_initialized_1(v_body));
        p_check_46_formals(v_locals, &v_ys, r_s);
        p_check_46_statements(f_list_46__43__43__2(f_check_46_bind_1(v_ys), v_env), f_list_46__43__43__2(f_syntax_46_names_1(v_ys), v_assignable), v_body, &v_xs, r_s);
        u_value t1683 = u_mk_con(&n_syntax_46_SBlock, 2, v_ys, v_xs);
        *r_y = t1683;
      }
    }
    if ((!d1680) && u_is_con(s1679, &n_syntax_46_SIf)) {
      u_value v_cond = u_field(s1679, 0);
      u_value v_left = u_field(s1679, 1);
      u_value v_right = u_field(s1679, 2);
      d1680 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_left, &v_l, r_s);
        p_check_46_statement(v_env, v_assignable, v_right, &v_r, r_s);
        u_value t1684 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
        *r_y = t1684;
      }
    }
    if ((!d1680) && u_is_con(s1679, &n_syntax_46_SWhile)) {
      u_value v_cond = u_field(s1679, 0);
      u_value v_body = u_field(s1679, 1);
      d1680 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        u_value t1685 = u_mk_con(&n_syntax_46_SWhile, 2, v_c, v_l);
        *r_y = t1685;
      }
    }
    if ((!d1680) && u_is_con(s1679, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s1679, 0);
      u_value v_ins = u_field(s1679, 1);
      u_value v_outs = u_field(s1679, 2);
      u_value v_refs = u_field(s1679, 3);
      d1680 = 1;
      {
        p_check_46_call_proc(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d1680) && u_is_con(s1679, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s1679, 0);
      u_value v_cases = u_field(s1679, 1);
      d1680 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_cases, v_t, &v_bs, r_s);
        u_value t1686 = &o_list_46_Nil;
        u_value *e1687 = &t1686;
        u_value l1688 = v_bs;
        while (u_is_con(l1688, &n_list_46_Cons)) {
          u_value v_br = u_field(l1688, 0);
          if (u_truth(u_mk_bool(1))) {
            e1687 = u_append(e1687, f_check_46_case_of_1(v_br));
          }
          l1688 = u_field(l1688, 1);
        }
        p_check_46_coverage(*r_s, v_t, t1686);
        u_value t1689 = u_mk_con(&n_syntax_46_SMatch, 2, v_c, v_bs);
        *r_y = t1689;
      }
    }
    if ((!d1680) && u_is_con(s1679, &n_syntax_46_SAssert)) {
      u_value v_cond = u_field(s1679, 0);
      d1680 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        u_value t1690 = u_mk_con(&n_syntax_46_SAssert, 1, v_c);
        *r_y = t1690;
      }
    }
    if ((!d1680) && u_is_con(s1679, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s1679, 0);
      u_value v_a = u_field(s1679, 1);
      d1680 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_unify(v_t, v_u, r_s);
        u_value t1691 = u_mk_con(&n_syntax_46_SRaise, 2, v_c, v_t);
        *r_y = t1691;
      }
    }
    if ((!d1680) && u_is_con(s1679, &n_syntax_46_STry)) {
      u_value v_body = u_field(s1679, 0);
      u_value v_handlers = u_field(s1679, 1);
      d1680 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_handlers, &v_hs, r_s);
        u_value t1692 = u_mk_con(&n_syntax_46_STry, 2, v_l, v_hs);
        *r_y = t1692;
      }
    }
    if (!d1680) {
      u_no_case(s1679);
    }
  }
}

u_value f_check_46_initialized_1(u_value v_xs) {
  u_value s1693 = v_xs;
  u_value t1694 = NULL;
  if ((!t1694) && (u_is_con(s1693, &n_list_46_Cons) && (u_is_con(u_field(s1693, 0), &n_syntax_46_SAssign) && (u_is_con(u_field(u_field(s1693, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s1693, 0), 0), 1), &n_list_46_Nil))))) {
    u_value v_x = u_field(u_field(u_field(u_field(s1693, 0), 0), 0), 0);
    u_value v_rest = u_field(s1693, 1);
    t1694 = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), f_check_46_initialized_1(v_rest));
  }
  if (!t1694) {
    t1694 = &o_list_46_Nil;
  }
  if (!t1694) {
    u_no_case(s1693);
  }
  return t1694;
}

void p_check_46_untyped(u_value v_xs, u_value v_inits) {
  {
    u_value s1695 = v_xs;
    int d1696 = 0;
    if ((!d1696) && u_is_con(s1695, &n_list_46_Nil)) {
      d1696 = 1;
      {
      }
    }
    if ((!d1696) && (u_is_con(s1695, &n_list_46_Cons) && u_is_con(u_field(s1695, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s1695, 0), 0);
      u_value v_t = u_field(u_field(s1695, 0), 1);
      u_value v_rest = u_field(s1695, 1);
      d1696 = 1;
      {
        u_value t1697 = f_check_46_holes_1(v_t);
        if (u_truth(t1697)) {
          t1697 = f_prelude_46_not_1(f_list_46_contains_2(v_n, v_inits));
        }
        if (u_truth(t1697)) {
          {
            u_raise(u_mk_con(&n_check_46_MissingType, 1, v_n), "check.TypeError");
          }
        }
        p_check_46_untyped(v_rest, v_inits);
      }
    }
    if (!d1696) {
      u_no_case(s1695);
    }
  }
}

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s1698 = v_xs;
    int d1699 = 0;
    if ((!d1699) && u_is_con(s1698, &n_list_46_Nil)) {
      d1699 = 1;
      {
        u_value t1700 = &o_list_46_Nil;
        *r_ys = t1700;
      }
    }
    if ((!d1699) && u_is_con(s1698, &n_list_46_Cons)) {
      u_value v_x = u_field(s1698, 0);
      u_value v_more = u_field(s1698, 1);
      d1699 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_statements(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t1701 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t1701;
      }
    }
    if (!d1699) {
      u_no_case(s1698);
    }
  }
}

u_value f_check_46_os_procs_1(u_value v_c) {
  u_value t1712;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_1702 = U_LIT("os_args");
    static struct u_object l_os_read_1703 = U_LIT("os_read");
    static struct u_object l_os_write_1704 = U_LIT("os_write");
    static struct u_object l_os_show_1705 = U_LIT("os_show");
    static struct u_object l_A_1706 = U_LIT("A");
    static struct u_object l_A_1707 = U_LIT("A");
    static struct u_object l_os_print_1708 = U_LIT("os_print");
    static struct u_object l_os_print_error_1709 = U_LIT("os_print_error");
    static struct u_object l_os_exit_1710 = U_LIT("os_exit");
    static struct u_object l_os_library_1711 = U_LIT("os_library");
    t1712 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_1702), u_mk_tuple(4, &o_list_46_Nil, &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), &o_list_46_Nil), &o_list_46_Nil)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_1703), u_mk_tuple(4, &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), &o_list_46_Nil), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), &o_list_46_Nil), &o_list_46_Nil)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_1704), u_mk_tuple(4, &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), &o_list_46_Nil)), &o_list_46_Nil, &o_list_46_Nil)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_show_1705), u_mk_tuple(4, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_1706), &o_list_46_Nil), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_1707), &o_list_46_Nil), &o_list_46_Nil, &o_list_46_Nil)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_print_1708), u_mk_tuple(4, &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), &o_list_46_Nil), &o_list_46_Nil, &o_list_46_Nil)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_print_error_1709), u_mk_tuple(4, &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), &o_list_46_Nil), &o_list_46_Nil, &o_list_46_Nil)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_exit_1710), u_mk_tuple(4, &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), &o_list_46_Nil), &o_list_46_Nil, &o_list_46_Nil)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_1711), u_mk_tuple(4, &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), &o_list_46_Nil), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), &o_list_46_Nil), &o_list_46_Nil)), &o_list_46_Nil))))))));
  } else {
    t1712 = &o_list_46_Nil;
  }
  return t1712;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t1713 = &o_list_46_Nil;
  u_value *e1714 = &t1713;
  static struct u_object l_Program_1715 = U_LIT("Program");
  u_value l1716 = f_ctx_46_names_of_2(v_c, &l_Program_1715);
  while (u_is_con(l1716, &n_list_46_Cons)) {
    u_value v_d = u_field(l1716, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e1714 = u_append(e1714, v_d);
    }
    l1716 = u_field(l1716, 1);
  }
  u_value s1717 = t1713;
  u_value t1718 = NULL;
  if ((!t1718) && u_is_con(s1717, &n_list_46_Cons)) {
    u_value v_d = u_field(s1717, 0);
    static struct u_object l_read_1719 = U_LIT("read");
    static struct u_object l_library_1720 = U_LIT("library");
    t1718 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_1719), u_mk_tuple(4, &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), &o_list_46_Nil), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, &o_list_46_Nil), &o_list_46_Nil), &o_list_46_Nil)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_1720), u_mk_tuple(4, &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), &o_list_46_Nil), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, &o_list_46_Nil), &o_list_46_Nil), &o_list_46_Nil)), &o_list_46_Nil));
  }
  if (!t1718) {
    t1718 = &o_list_46_Nil;
  }
  if (!t1718) {
    u_no_case(s1717);
  }
  return t1718;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value s1721 = f_list_46_lookup_2(v_p, v_env);
    int d1722 = 0;
    if ((!d1722) && u_is_con(s1721, &n_option_46_Some)) {
      u_value v_sc = u_field(s1721, 0);
      d1722 = 1;
      {
        p_check_46_call_variable(v_env, v_assignable, v_p, v_sc, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d1722) && u_is_con(s1721, &n_option_46_None)) {
      d1722 = 1;
      {
        p_check_46_call_declared(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if (!d1722) {
      u_no_case(s1721);
    }
  }
}

void p_check_46_call_variable(u_value v_env, u_value v_assignable, u_value v_p, u_value v_sc, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_x = NULL;
    u_value v_is = NULL;
    u_value v_os = NULL;
    u_value v_rs = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    p_check_46_use(v_p, &o_syntax_46_THole, v_sc, &v_x, r_s);
    p_check_46_fresh_list(v_ins, &v_is, r_s);
    p_check_46_fresh_list(v_outs, &v_os, r_s);
    p_check_46_fresh_list(v_refs, &v_rs, r_s);
    p_check_46_unify(f_syntax_46_Var_46_typ_1(v_x), u_mk_con(&n_syntax_46_TProc, 3, v_is, v_os, v_rs), r_s);
    p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
    p_check_46_checks(v_env, v_ins, v_is, &v_ins1, r_s);
    p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
    p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
    p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), f_list_46__43__43__2(v_os, v_rs), r_s);
    u_value t1723 = u_mk_con(&n_syntax_46_SCall, 4, v_p, v_ins1, v_outs1, v_refs1);
    *r_y = t1723;
  }
}

void p_check_46_call_declared(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s1725 = f_check_46_visible_names_3(*r_s, v_p, u_mk_closure(lam1724, 1, *r_s));
    int d1726 = 0;
    if ((!d1726) && (u_is_con(s1725, &n_list_46_Cons) && u_is_con(u_field(s1725, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s1725, 0);
      d1726 = 1;
      {
        u_value t1727 = v_q1;
        v_q = t1727;
      }
    }
    if ((!d1726) && u_is_con(s1725, &n_list_46_Nil)) {
      d1726 = 1;
      {
        u_value t1728 = v_p;
        v_q = t1728;
      }
    }
    if (!d1726) {
      u_value v_qs = s1725;
      d1726 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d1726) {
      u_no_case(s1725);
    }
    u_value s1729 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_State_46_ctx_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_State_46_ctx_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_State_46_ctx_1(*r_s)))));
    int d1730 = 0;
    if ((!d1730) && u_is_con(s1729, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s1729, 0), 0);
      u_value v_is = u_field(u_field(s1729, 0), 1);
      u_value v_os = u_field(u_field(s1729, 0), 2);
      u_value v_rs = u_field(u_field(s1729, 0), 3);
      d1730 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t1731 = &o_list_46_Nil;
        u_value *e1732 = &t1731;
        u_value l1733 = v_is;
        while (u_is_con(l1733, &n_list_46_Cons)) {
          u_value v_t = u_field(l1733, 0);
          if (u_truth(u_mk_bool(1))) {
            e1732 = u_append(e1732, f_check_46_subst_2(v_t, v_m));
          }
          l1733 = u_field(l1733, 1);
        }
        p_check_46_checks(v_env, v_ins, t1731, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t1734 = &o_list_46_Nil;
        u_value *e1735 = &t1734;
        u_value l1736 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l1736, &n_list_46_Cons)) {
          u_value v_t = u_field(l1736, 0);
          if (u_truth(u_mk_bool(1))) {
            e1735 = u_append(e1735, f_check_46_subst_2(v_t, v_m));
          }
          l1736 = u_field(l1736, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t1734, r_s);
        u_value t1737 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t1737;
      }
    }
    if ((!d1730) && u_is_con(s1729, &n_option_46_None)) {
      d1730 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d1730) {
      u_no_case(s1729);
    }
  }
}

void p_check_46_stmt_branches(u_value v_env, u_value v_assignable, u_value v_cases, u_value v_t, u_value *r_cases1, u_value *r_s) {
  {
    u_value v_p1 = NULL;
    u_value v_xs = NULL;
    u_value v_g1 = NULL;
    u_value v_b1 = NULL;
    u_value v_rest = NULL;
    u_value s1738 = v_cases;
    int d1739 = 0;
    if ((!d1739) && u_is_con(s1738, &n_list_46_Nil)) {
      d1739 = 1;
      {
        u_value t1740 = &o_list_46_Nil;
        *r_cases1 = t1740;
      }
    }
    if ((!d1739) && (u_is_con(s1738, &n_list_46_Cons) && u_is_con(u_field(s1738, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s1738, 0), 0);
      u_value v_g = u_field(u_field(s1738, 0), 1);
      u_value v_b = u_field(u_field(s1738, 0), 2);
      u_value v_more = u_field(s1738, 1);
      d1739 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t1741 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t1741;
      }
    }
    if (!d1739) {
      u_no_case(s1738);
    }
  }
}

void p_check_46_handler_list(u_value v_env, u_value v_assignable, u_value v_hs, u_value *r_hs1, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_p1 = NULL;
    u_value v_xs = NULL;
    u_value v_g1 = NULL;
    u_value v_b1 = NULL;
    u_value v_rest = NULL;
    u_value s1742 = v_hs;
    int d1743 = 0;
    if ((!d1743) && u_is_con(s1742, &n_list_46_Nil)) {
      d1743 = 1;
      {
        u_value t1744 = &o_list_46_Nil;
        *r_hs1 = t1744;
      }
    }
    if ((!d1743) && (u_is_con(s1742, &n_list_46_Cons) && u_is_con(u_field(s1742, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s1742, 0), 0);
      u_value v_p = u_field(u_field(s1742, 0), 1);
      u_value v_g = u_field(u_field(s1742, 0), 2);
      u_value v_b = u_field(u_field(s1742, 0), 3);
      u_value v_more = u_field(s1742, 1);
      d1743 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t1745 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t1745;
      }
    }
    if (!d1743) {
      u_no_case(s1742);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t1746 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t1746)) {
      t1746 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t1747 = t1746;
    if (!u_truth(t1747)) {
      t1747 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t1748 = t1747;
    if (!u_truth(t1748)) {
      t1748 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n))), &o_list_46_Nil);
    }
    if (u_truth(t1748)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t1749 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t1749)) {
      t1749 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t1750 = t1749;
    if (!u_truth(t1750)) {
      t1750 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t1750)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_1751 = U_LIT("Bool");
    static struct u_object l_Int_1752 = U_LIT("Int");
    static struct u_object l_Str_1753 = U_LIT("Str");
    static struct u_object l_Char_1754 = U_LIT("Char");
    static struct u_object l__45__62__1755 = U_LIT("->");
    u_value t1756 = f_list_46_contains_2(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_1751, u_mk_con(&n_list_46_Cons, 2, &l_Int_1752, u_mk_con(&n_list_46_Cons, 2, &l_Str_1753, u_mk_con(&n_list_46_Cons, 2, &l_Char_1754, u_mk_con(&n_list_46_Cons, 2, &l__45__62__1755, &o_list_46_Nil))))));
    if (!u_truth(t1756)) {
      t1756 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t1757 = t1756;
    if (!u_truth(t1757)) {
      t1757 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t1757)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t1758 = &o_list_46_Nil;
    u_value *e1759 = &t1758;
    u_value l1760 = v_ts;
    while (u_is_con(l1760, &n_list_46_Cons)) {
      u_value v_t = u_field(l1760, 0);
      if (u_truth(u_mk_bool(1))) {
        e1759 = u_append(e1759, f_check_46_free_1(v_t));
      }
      l1760 = u_field(l1760, 1);
    }
    u_value t1761 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t1758)), f_check_46_params_1(v_ps));
    v_undeclared = t1761;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, &o_list_46_Nil))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
  {
    u_value s1762 = v_cs;
    int d1763 = 0;
    if ((!d1763) && u_is_con(s1762, &n_list_46_Nil)) {
      d1763 = 1;
      {
      }
    }
    if ((!d1763) && (u_is_con(s1762, &n_list_46_Cons) && u_is_con(u_field(u_field(s1762, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s1762, 0), 0);
      u_value v_n = u_field(u_field(u_field(s1762, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s1762, 0), 1), 1);
      u_value v_rest = u_field(s1762, 1);
      d1763 = 1;
      {
        u_value t1764 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t1764;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        u_value t1765 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t1765;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d1763) && u_is_con(s1762, &n_list_46_Cons)) {
      u_value v_rest = u_field(s1762, 1);
      d1763 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d1763) {
      u_no_case(s1762);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s1766 = v_cs;
    int d1767 = 0;
    if ((!d1767) && u_is_con(s1766, &n_list_46_Nil)) {
      d1767 = 1;
      {
      }
    }
    if ((!d1767) && (u_is_con(s1766, &n_list_46_Cons) && u_is_con(u_field(u_field(s1766, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s1766, 0), 0);
      u_value v_n = u_field(u_field(u_field(s1766, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s1766, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s1766, 0), 1), 2);
      u_value v_rest = u_field(s1766, 1);
      d1767 = 1;
      {
        u_value t1768 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t1768;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, &o_list_46_Nil));
        u_value t1769 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_State_46_ctx_1(*r_s), v_n, v_ps, v_u));
        *r_s = t1769;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d1767) && u_is_con(s1766, &n_list_46_Cons)) {
      u_value v_rest = u_field(s1766, 1);
      d1767 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d1767) {
      u_no_case(s1766);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
  {
    u_value s1770 = v_cs;
    int d1771 = 0;
    if ((!d1771) && u_is_con(s1770, &n_list_46_Nil)) {
      d1771 = 1;
      {
      }
    }
    if ((!d1771) && (u_is_con(s1770, &n_list_46_Cons) && u_is_con(u_field(u_field(s1770, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s1770, 0), 0);
      u_value v_n = u_field(u_field(u_field(s1770, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s1770, 0), 1), 1);
      u_value v_ks = u_field(u_field(u_field(s1770, 0), 1), 2);
      u_value v_rest = u_field(s1770, 1);
      d1771 = 1;
      {
        u_value t1772 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t1772;
        p_check_46_unique_ids(v_ps);
        u_value t1773 = &o_list_46_Nil;
        u_value *e1774 = &t1773;
        u_value l1775 = v_ks;
        while (u_is_con(l1775, &n_list_46_Cons)) {
          u_value v_k = u_field(l1775, 0);
          if (u_truth(u_mk_bool(1))) {
            e1774 = u_append(e1774, f_syntax_46_Constr_46_name_1(v_k));
          }
          l1775 = u_field(l1775, 1);
        }
        u_value t1776 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_State_46_ctx_1(*r_s), v_n, t1773));
        *r_s = t1776;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d1771) && u_is_con(s1770, &n_list_46_Cons)) {
      u_value v_rest = u_field(s1770, 1);
      d1771 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d1771) {
      u_no_case(s1770);
    }
  }
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s1777 = v_ks;
    int d1778 = 0;
    if ((!d1778) && u_is_con(s1777, &n_list_46_Nil)) {
      d1778 = 1;
      {
      }
    }
    if ((!d1778) && (u_is_con(s1777, &n_list_46_Cons) && u_is_con(u_field(s1777, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s1777, 0), 0);
      u_value v_args = u_field(u_field(s1777, 0), 1);
      u_value v_rest = u_field(s1777, 1);
      d1778 = 1;
      {
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t1779 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_State_46_ctx_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t1779;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d1778) {
      u_no_case(s1777);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s1780 = v_xss;
  u_value t1781 = NULL;
  if ((!t1781) && u_is_con(s1780, &n_list_46_Nil)) {
    t1781 = v_r;
  }
  if ((!t1781) && u_is_con(s1780, &n_list_46_Cons)) {
    u_value v_xs = u_field(s1780, 0);
    u_value v_rest = u_field(s1780, 1);
    t1781 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t1781) {
    u_no_case(s1780);
  }
  return t1781;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s1782 = v_xss;
  u_value t1783 = NULL;
  if ((!t1783) && u_is_con(s1782, &n_list_46_Nil)) {
    t1783 = &o_list_46_Nil;
  }
  if ((!t1783) && u_is_con(s1782, &n_list_46_Cons)) {
    u_value v_xs = u_field(s1782, 0);
    t1783 = v_xs;
  }
  if (!t1783) {
    u_no_case(s1782);
  }
  return t1783;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s1784 = v_xss;
    int d1785 = 0;
    if ((!d1785) && u_is_con(s1784, &n_list_46_Nil)) {
      d1785 = 1;
      {
        u_value t1786 = &o_list_46_Nil;
        *r_yss = t1786;
      }
    }
    if ((!d1785) && u_is_con(s1784, &n_list_46_Cons)) {
      u_value v_xs = u_field(s1784, 0);
      u_value v_more = u_field(s1784, 1);
      d1785 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t1787 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t1787;
      }
    }
    if (!d1785) {
      u_no_case(s1784);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s1788 = v_cs;
    int d1789 = 0;
    if ((!d1789) && u_is_con(s1788, &n_list_46_Nil)) {
      d1789 = 1;
      {
      }
    }
    if ((!d1789) && (u_is_con(s1788, &n_list_46_Cons) && u_is_con(u_field(u_field(s1788, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s1788, 0), 0);
      u_value v_n = u_field(u_field(u_field(s1788, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s1788, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s1788, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s1788, 0), 1), 3);
      u_value v_rest = u_field(s1788, 1);
      d1789 = 1;
      {
        u_value t1790 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t1790;
        p_check_46_overload(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), &o_list_46_Nil));
        u_value t1791 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t1791;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d1789) && u_is_con(s1788, &n_list_46_Cons)) {
      u_value v_rest = u_field(s1788, 1);
      d1789 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d1789) {
      u_no_case(s1788);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s1792 = v_cs;
    int d1793 = 0;
    if ((!d1793) && u_is_con(s1792, &n_list_46_Nil)) {
      d1793 = 1;
      {
      }
    }
    if ((!d1793) && (u_is_con(s1792, &n_list_46_Cons) && u_is_con(u_field(u_field(s1792, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s1792, 0), 0);
      u_value v_n = u_field(u_field(u_field(s1792, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s1792, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s1792, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s1792, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s1792, 0), 1), 4);
      u_value v_rest = u_field(s1792, 1);
      d1793 = 1;
      {
        u_value t1794 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t1794;
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t1795 = f_syntax_46_types_1(v_xs);
        v_ts = t1795;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t1796 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t1796;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d1793) && u_is_con(s1792, &n_list_46_Cons)) {
      u_value v_rest = u_field(s1792, 1);
      d1793 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d1793) {
      u_no_case(s1792);
    }
  }
}

void p_check_46_command(u_value v_c, u_value *r_c1, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value v_b = NULL;
    u_value v_t = NULL;
    u_value v_is = NULL;
    u_value v_os = NULL;
    u_value v_rs = NULL;
    u_value v_y = NULL;
    u_value s1797 = v_c;
    int d1798 = 0;
    if ((!d1798) && u_is_con(s1797, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s1797, 0);
      u_value v_ps = u_field(s1797, 1);
      u_value v_bound = u_field(s1797, 2);
      u_value v_res = u_field(s1797, 3);
      u_value v_body = u_field(s1797, 4);
      d1798 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t1799 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t1799;
      }
    }
    if ((!d1798) && u_is_con(s1797, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s1797, 0);
      u_value v_ps = u_field(s1797, 1);
      u_value v_ins = u_field(s1797, 2);
      u_value v_outs = u_field(s1797, 3);
      u_value v_refs = u_field(s1797, 4);
      u_value v_body = u_field(s1797, 5);
      d1798 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t1800 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t1800;
      }
    }
    if ((!d1798) && (u_is_con(s1797, &n_syntax_46_CTest) && u_is_con(u_field(s1797, 1), &n_list_46_Nil))) {
      u_value v_n = u_field(s1797, 0);
      u_value v_body = u_field(s1797, 2);
      d1798 = 1;
      {
        p_check_46_statement(&o_list_46_Nil, &o_list_46_Nil, v_body, &v_y, r_s);
        u_value t1801 = u_mk_con(&n_syntax_46_CTest, 3, v_n, &o_list_46_Nil, v_y);
        *r_c1 = t1801;
      }
    }
    if ((!d1798) && u_is_con(s1797, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s1797, 0);
      d1798 = 1;
      {
        u_raise(u_mk_con(&n_check_46_TestParams, 1, v_n), "check.TypeError");
      }
    }
    if (!d1798) {
      d1798 = 1;
      {
        u_value t1802 = v_c;
        *r_c1 = t1802;
      }
    }
    if (!d1798) {
      u_no_case(s1797);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s1803 = v_cs;
    int d1804 = 0;
    if ((!d1804) && u_is_con(s1803, &n_list_46_Nil)) {
      d1804 = 1;
      {
        u_value t1805 = &o_list_46_Nil;
        *r_cs1 = t1805;
      }
    }
    if ((!d1804) && u_is_con(s1803, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s1803, 0), 0);
      u_value v_c = u_field(u_field(s1803, 0), 1);
      u_value v_more = u_field(s1803, 1);
      d1804 = 1;
      {
        u_value t1806 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t1806;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t1807 = f_check_46_zonk_cmd_2(f_check_46_State_46_sub_1(*r_s), v_c1);
        v_c2 = t1807;
        u_value s1808 = f_check_46_unknown_in_cmd_1(v_c2);
        int d1809 = 0;
        if ((!d1809) && u_is_con(s1808, &n_option_46_Some)) {
          u_value v_t = u_field(s1808, 0);
          d1809 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d1809) && u_is_con(s1808, &n_option_46_None)) {
          d1809 = 1;
          {
          }
        }
        if (!d1809) {
          u_no_case(s1808);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t1810 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t1810;
      }
    }
    if (!d1804) {
      u_no_case(s1803);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s1811 = v_cs;
  u_value t1812 = NULL;
  if ((!t1812) && u_is_con(s1811, &n_list_46_Nil)) {
    t1812 = &o_list_46_Nil;
  }
  if ((!t1812) && (u_is_con(s1811, &n_list_46_Cons) && u_is_con(u_field(u_field(s1811, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s1811, 0), 1), 0);
    u_value v_more = u_field(s1811, 1);
    t1812 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t1812) && u_is_con(s1811, &n_list_46_Cons)) {
    u_value v_more = u_field(s1811, 1);
    t1812 = f_check_46_block_names_1(v_more);
  }
  if (!t1812) {
    u_no_case(s1811);
  }
  return t1812;
}

u_value f_check_46_Checked_46_cmds_1(u_value v_x) {
  u_value s1813 = v_x;
  u_value t1814 = NULL;
  if ((!t1814) && u_is_con(s1813, &n_check_46_Checked)) {
    u_value v_y = u_field(s1813, 0);
    t1814 = v_y;
  }
  if (!t1814) {
    u_no_case(s1813);
  }
  return t1814;
}

u_value f_check_46_Checked_46_ctx_1(u_value v_x) {
  u_value s1815 = v_x;
  u_value t1816 = NULL;
  if ((!t1816) && u_is_con(s1815, &n_check_46_Checked)) {
    u_value v_y = u_field(s1815, 1);
    t1816 = v_y;
  }
  if (!t1816) {
    u_no_case(s1815);
  }
  return t1816;
}

u_value f_check_46_Checked_46_imports_1(u_value v_x) {
  u_value s1817 = v_x;
  u_value t1818 = NULL;
  if ((!t1818) && u_is_con(s1817, &n_check_46_Checked)) {
    u_value v_y = u_field(s1817, 2);
    t1818 = v_y;
  }
  if (!t1818) {
    u_no_case(s1817);
  }
  return t1818;
}

u_value f_check_46_Checked_46_cmds_58__61__2(u_value v_x, u_value v_y) {
  u_value s1819 = v_x;
  u_value t1820 = NULL;
  if ((!t1820) && u_is_con(s1819, &n_check_46_Checked)) {
    u_value v_a1 = u_field(s1819, 1);
    u_value v_a2 = u_field(s1819, 2);
    t1820 = u_mk_con(&n_check_46_Checked, 3, v_y, v_a1, v_a2);
  }
  if (!t1820) {
    u_no_case(s1819);
  }
  return t1820;
}

u_value f_check_46_Checked_46_ctx_58__61__2(u_value v_x, u_value v_y) {
  u_value s1821 = v_x;
  u_value t1822 = NULL;
  if ((!t1822) && u_is_con(s1821, &n_check_46_Checked)) {
    u_value v_a0 = u_field(s1821, 0);
    u_value v_a2 = u_field(s1821, 2);
    t1822 = u_mk_con(&n_check_46_Checked, 3, v_a0, v_y, v_a2);
  }
  if (!t1822) {
    u_no_case(s1821);
  }
  return t1822;
}

u_value f_check_46_Checked_46_imports_58__61__2(u_value v_x, u_value v_y) {
  u_value s1823 = v_x;
  u_value t1824 = NULL;
  if ((!t1824) && u_is_con(s1823, &n_check_46_Checked)) {
    u_value v_a0 = u_field(s1823, 0);
    u_value v_a1 = u_field(s1823, 1);
    t1824 = u_mk_con(&n_check_46_Checked, 3, v_a0, v_a1, v_y);
  }
  if (!t1824) {
    u_no_case(s1823);
  }
  return t1824;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_cache = NULL;
    u_value t1825 = &o_list_46_Nil;
    v_cache = t1825;
    p_check_46_check_modules(v_p, v_main, r_q, &v_cache);
  }
}

void p_check_46_check_modules(u_value v_p, u_value v_main, u_value *r_q, u_value *r_cache) {
  {
    u_value v_c = NULL;
    u_value v_imports = NULL;
    u_value v_seen = NULL;
    u_value v_cs = NULL;
    u_value v_own = NULL;
    u_value t1826 = f_ctx_46_empty_ctx_0();
    v_c = t1826;
    u_value t1827 = &o_list_46_Nil;
    v_imports = t1827;
    u_value t1828 = &o_list_46_Nil;
    v_seen = t1828;
    u_value t1829 = &o_list_46_Nil;
    v_cs = t1829;
    p_check_46_check_units(f_check_46_modules_1(f_syntax_46_Program_46_cmds_1(v_p)), &v_c, &v_imports, &v_seen, &v_cs, r_cache);
    p_check_46_check_unit(f_check_46_own_decls_2(v_main, f_syntax_46_Program_46_cmds_1(v_p)), v_main, v_c, v_imports, &v_own);
    u_value t1830 = u_mk_con(&n_syntax_46_Program, 1, f_list_46__43__43__2(v_cs, f_check_46_Checked_46_cmds_1(v_own)));
    *r_q = t1830;
  }
}

void p_check_46_check_units(u_value v_us, u_value *r_c, u_value *r_imports, u_value *r_seen, u_value *r_cs, u_value *r_cache) {
  {
    u_value v_key = NULL;
    u_value v_checked = NULL;
    u_value v_i = NULL;
    u_value s1831 = v_us;
    int d1832 = 0;
    if ((!d1832) && u_is_con(s1831, &n_list_46_Nil)) {
      d1832 = 1;
      {
      }
    }
    if ((!d1832) && u_is_con(s1831, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s1831, 0), 0);
      u_value v_ds = u_field(u_field(s1831, 0), 1);
      u_value v_rest = u_field(s1831, 1);
      d1832 = 1;
      {
        static struct u_object l_prelude_1833 = U_LIT("prelude");
        u_value t1834 = u_mk_tuple(3, v_m, v_ds, f_check_46_positions_of_2(u_mk_con(&n_list_46_Cons, 2, &l_prelude_1833, f_check_46_imports_in_2(v_ds, v_m)), *r_seen));
        v_key = t1834;
        u_value s1835 = f_check_46_position_in_3(v_key, *r_cache, u_mk_int(0));
        int d1836 = 0;
        if ((!d1836) && u_is_con(s1835, &n_option_46_Some)) {
          u_value v_k = u_field(u_field(s1835, 0), 0);
          u_value v_done = u_field(u_field(s1835, 0), 1);
          d1836 = 1;
          {
            u_value t1837 = v_k;
            u_value t1838 = v_done;
            v_i = t1837;
            v_checked = t1838;
          }
        }
        if ((!d1836) && u_is_con(s1835, &n_option_46_None)) {
          d1836 = 1;
          {
            p_check_46_check_unit(v_ds, v_m, *r_c, *r_imports, &v_checked);
            u_value t1839 = f_list_46_length_1(*r_cache);
            u_value t1840 = f_list_46__43__43__2(*r_cache, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_key, v_checked), &o_list_46_Nil));
            v_i = t1839;
            *r_cache = t1840;
          }
        }
        if (!d1836) {
          u_no_case(s1835);
        }
        u_value t1841 = f_ctx_46_ctx_union_2(*r_c, f_check_46_Checked_46_ctx_1(v_checked));
        u_value t1842 = f_list_46__43__43__2(*r_imports, f_check_46_Checked_46_imports_1(v_checked));
        u_value t1843 = f_list_46__43__43__2(*r_cs, f_check_46_Checked_46_cmds_1(v_checked));
        *r_c = t1841;
        *r_imports = t1842;
        *r_cs = t1843;
        u_value t1844 = &o_list_46_Nil;
        u_value *e1845 = &t1844;
        u_value l1846 = f_list_46_firsts_1(f_check_46_Checked_46_imports_1(v_checked));
        while (u_is_con(l1846, &n_list_46_Cons)) {
          u_value v_n = u_field(l1846, 0);
          if (u_truth(u_mk_bool(1))) {
            e1845 = u_append(e1845, u_mk_tuple(2, v_n, v_i));
          }
          l1846 = u_field(l1846, 1);
        }
        u_value t1847 = f_list_46__43__43__2(t1844, *r_seen);
        *r_seen = t1847;
        p_check_46_check_units(v_rest, r_c, r_imports, r_seen, r_cs, r_cache);
      }
    }
    if (!d1832) {
      u_no_case(s1831);
    }
  }
}

u_value f_check_46_positions_of_2(u_value v_ms, u_value v_seen) {
  u_value s1848 = v_ms;
  u_value t1849 = NULL;
  if ((!t1849) && u_is_con(s1848, &n_list_46_Nil)) {
    t1849 = &o_list_46_Nil;
  }
  if ((!t1849) && u_is_con(s1848, &n_list_46_Cons)) {
    u_value v_m = u_field(s1848, 0);
    u_value v_rest = u_field(s1848, 1);
    u_value s1850 = f_list_46_lookup_2(v_m, v_seen);
    u_value t1851 = NULL;
    if ((!t1851) && u_is_con(s1850, &n_option_46_Some)) {
      u_value v_i = u_field(s1850, 0);
      t1851 = u_mk_con(&n_list_46_Cons, 2, v_i, f_check_46_positions_of_2(v_rest, v_seen));
    }
    if ((!t1851) && u_is_con(s1850, &n_option_46_None)) {
      t1851 = f_check_46_positions_of_2(v_rest, v_seen);
    }
    if (!t1851) {
      u_no_case(s1850);
    }
    t1849 = t1851;
  }
  if (!t1849) {
    u_no_case(s1848);
  }
  return t1849;
}

u_value f_check_46_position_in_3(u_value v_key, u_value v_cache, u_value v_i) {
  u_value s1852 = v_cache;
  u_value t1853 = NULL;
  if ((!t1853) && u_is_con(s1852, &n_list_46_Nil)) {
    t1853 = &o_option_46_None;
  }
  if ((!t1853) && u_is_con(s1852, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s1852, 0), 0);
    u_value v_checked = u_field(u_field(s1852, 0), 1);
    u_value v_rest = u_field(s1852, 1);
    u_value t1854;
    if (u_truth(u_eq(v_k, v_key))) {
      t1854 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_i, v_checked));
    } else {
      t1854 = f_check_46_position_in_3(v_key, v_rest, u_add(v_i, u_mk_int(1)));
    }
    t1853 = t1854;
  }
  if (!t1853) {
    u_no_case(s1852);
  }
  return t1853;
}

void p_check_46_check_unit(u_value v_ds, u_value v_main, u_value v_before, u_value v_imports, u_value *r_checked) {
  {
    u_value v_own = NULL;
    u_value v_s = NULL;
    u_value v_cs = NULL;
    u_value t1855 = &o_list_46_Nil;
    u_value *e1856 = &t1855;
    u_value l1857 = f_list_46_distinct_1(f_list_46_firsts_1(v_ds));
    while (u_is_con(l1857, &n_list_46_Cons)) {
      u_value v_m = u_field(l1857, 0);
      if (u_truth(u_mk_bool(1))) {
        e1856 = u_append(e1856, u_mk_tuple(2, v_m, f_check_46_imports_in_2(v_ds, v_m)));
      }
      l1857 = u_field(l1857, 1);
    }
    u_value t1858 = f_list_46__43__43__2(v_imports, t1855);
    v_own = t1858;
    u_value t1859 = u_mk_con(&n_check_46_State, 6, v_before, f_map_46_empty_0(), u_mk_int(0), v_main, &o_list_46_Nil, f_check_46_visibility_2(v_own, v_main));
    v_s = t1859;
    p_check_46_declare_datas(v_ds, &v_s);
    p_check_46_declare_sorts(v_ds, &v_s);
    p_check_46_declare_constrs(v_ds, &v_s);
    p_check_46_declare_funs(v_ds, &v_s);
    p_check_46_declare_procs(v_ds, &v_s);
    p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
    p_check_46_commands(v_ds, &v_cs, &v_s);
    u_value t1860 = &o_list_46_Nil;
    u_value *e1861 = &t1860;
    u_value l1862 = f_list_46_distinct_1(f_list_46_firsts_1(v_ds));
    while (u_is_con(l1862, &n_list_46_Cons)) {
      u_value v_m = u_field(l1862, 0);
      if (u_truth(u_mk_bool(1))) {
        e1861 = u_append(e1861, u_mk_tuple(2, v_m, f_check_46_imports_in_2(v_ds, v_m)));
      }
      l1862 = u_field(l1862, 1);
    }
    u_value t1863 = u_mk_con(&n_check_46_Checked, 3, v_cs, f_ctx_46_ctx_of_2(f_check_46_State_46_ctx_1(v_s), f_list_46_distinct_1(f_list_46_firsts_1(v_ds))), t1860);
    *r_checked = t1863;
  }
}

u_value f_check_46_modules_1(u_value v_cs) {
  u_value s1864 = v_cs;
  u_value t1865 = NULL;
  if ((!t1865) && u_is_con(s1864, &n_list_46_Nil)) {
    t1865 = &o_list_46_Nil;
  }
  if ((!t1865) && (u_is_con(s1864, &n_list_46_Cons) && (u_is_con(u_field(s1864, 0), &n_syntax_46_CModule) && u_equals(u_field(u_field(s1864, 0), 0), u_mk_lit("prelude"))))) {
    u_value v_ds = u_field(u_field(s1864, 0), 1);
    u_value v_rest = u_field(s1864, 1);
    static struct u_object l_prelude_1866 = U_LIT("prelude");
    static struct u_object l_prelude_1867 = U_LIT("prelude");
    t1865 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &l_prelude_1866, f_check_46_flatten_2(&l_prelude_1867, v_ds)), f_check_46_modules_1(v_rest));
  }
  if ((!t1865) && (u_is_con(s1864, &n_list_46_Cons) && u_is_con(u_field(s1864, 0), &n_syntax_46_CModule))) {
    u_value v_n = u_field(u_field(s1864, 0), 0);
    u_value v_ds = u_field(u_field(s1864, 0), 1);
    u_value v_rest = u_field(s1864, 1);
    t1865 = f_list_46__43__43__2(f_check_46_modules_1(v_ds), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_check_46_own_decls_2(v_n, v_ds)), f_check_46_modules_1(v_rest)));
  }
  if ((!t1865) && u_is_con(s1864, &n_list_46_Cons)) {
    u_value v_rest = u_field(s1864, 1);
    t1865 = f_check_46_modules_1(v_rest);
  }
  if (!t1865) {
    u_no_case(s1864);
  }
  return t1865;
}

u_value f_check_46_own_decls_2(u_value v_m, u_value v_cs) {
  u_value t1868 = &o_list_46_Nil;
  u_value *e1869 = &t1868;
  u_value l1870 = v_cs;
  while (u_is_con(l1870, &n_list_46_Cons)) {
    u_value v_c = u_field(l1870, 0);
    if (u_truth(f_prelude_46_not_1(f_check_46_is_module_1(v_c)))) {
      e1869 = u_append(e1869, u_mk_tuple(2, v_m, f_check_46_qualify_2(v_m, v_c)));
    }
    l1870 = u_field(l1870, 1);
  }
  return t1868;
}

u_value f_check_46_is_module_1(u_value v_c) {
  u_value s1871 = v_c;
  u_value t1872 = NULL;
  if ((!t1872) && u_is_con(s1871, &n_syntax_46_CModule)) {
    t1872 = u_mk_bool(1);
  }
  if (!t1872) {
    t1872 = u_mk_bool(0);
  }
  if (!t1872) {
    u_no_case(s1871);
  }
  return t1872;
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_1873 = U_LIT("main");
    u_value t1874 = f_check_46_start_2(&l_main_1873, &o_list_46_Nil);
    v_s = t1874;
    p_check_46_infer(&o_list_46_Nil, v_e, &v_e1, &v_u, &v_s);
    u_value t1875 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(v_s), v_u);
    *r_t = t1875;
  }
}

u_value f_check_46_ev_1(u_value v_n) {
  return u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), &o_syntax_46_THole));
}

u_value f_check_46_pv_1(u_value v_n) {
  return u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), &o_syntax_46_THole));
}

u_value f_check_46_num_1(u_value v_n) {
  return u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
}

u_value f_check_46_ap_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_syntax_46_EApp, 2, f_check_46_ev_1(v_f), v_args);
}

u_value f_check_46_tl_1(u_value v_t) {
  static struct u_object l_List_1876 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_1876), u_mk_con(&n_list_46_Cons, 2, v_t, &o_list_46_Nil));
}

u_value f_check_46_lists_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of check.lists");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_list_1877 = U_LIT("list");
  static struct u_object l_List_1878 = U_LIT("List");
  static struct u_object l_A_1879 = U_LIT("A");
  static struct u_object l_Nil_1880 = U_LIT("Nil");
  static struct u_object l_Cons_1881 = U_LIT("Cons");
  static struct u_object l_A_1882 = U_LIT("A");
  static struct u_object l_A_1883 = U_LIT("A");
  static struct u_object l_list_1884 = U_LIT("list");
  static struct u_object l_len_1885 = U_LIT("len");
  static struct u_object l_A_1886 = U_LIT("A");
  static struct u_object l_xs_1887 = U_LIT("xs");
  static struct u_object l_A_1888 = U_LIT("A");
  static struct u_object l_xs_1889 = U_LIT("xs");
  static struct u_object l_Nil_1890 = U_LIT("Nil");
  static struct u_object l_Cons_1891 = U_LIT("Cons");
  static struct u_object l_ys_1892 = U_LIT("ys");
  static struct u_object l__43__1893 = U_LIT("+");
  static struct u_object l_len_1894 = U_LIT("len");
  static struct u_object l_ys_1895 = U_LIT("ys");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_1877, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_1878), u_mk_con(&n_list_46_Cons, 2, &l_A_1879, &o_list_46_Nil), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_1880), &o_list_46_Nil), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_1881), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_1882), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_1883)), &o_list_46_Nil))), &o_list_46_Nil))), &o_list_46_Nil)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_1884), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_1885), u_mk_con(&n_list_46_Cons, 2, &l_A_1886, &o_list_46_Nil), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_1887), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_1888))), &o_list_46_Nil), &o_list_46_Nil), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_1889), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_1890), &o_option_46_None, f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1891), u_mk_con(&n_list_46_Cons, 2, &o_syntax_46_PWild, u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_1892), &o_list_46_Nil))), &o_option_46_None, f_check_46_ap_2(&l__43__1893, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_1894, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_1895), &o_list_46_Nil)), &o_list_46_Nil)))), &o_list_46_Nil)))), &o_list_46_Nil)));
  return cache;
}

u_value f_value_46_list_value_1(u_value v_vs) {
  u_value s1896 = v_vs;
  u_value t1897 = NULL;
  if ((!t1897) && u_is_con(s1896, &n_list_46_Nil)) {
    t1897 = u_mk_con(&n_value_46_Obj, 2, f_syntax_46_nil_name_0(), &o_list_46_Nil);
  }
  if ((!t1897) && u_is_con(s1896, &n_list_46_Cons)) {
    u_value v_v = u_field(s1896, 0);
    u_value v_rest = u_field(s1896, 1);
    t1897 = u_mk_con(&n_value_46_Obj, 2, f_syntax_46_cons_name_0(), u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_value_46_list_value_1(v_rest), &o_list_46_Nil)));
  }
  if (!t1897) {
    u_no_case(s1896);
  }
  return t1897;
}

u_value f_value_46_list_elements_1(u_value v_v) {
  u_value s1898 = v_v;
  u_value t1899 = NULL;
  if ((!t1899) && (u_is_con(s1898, &n_value_46_Obj) && u_is_con(u_field(s1898, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s1898, 0);
    if (u_truth(u_eq(v_n, f_syntax_46_nil_name_0()))) {
      t1899 = u_mk_con(&n_option_46_Some, 1, &o_list_46_Nil);
    }
  }
  if ((!t1899) && (u_is_con(s1898, &n_value_46_Obj) && (u_is_con(u_field(s1898, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1898, 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s1898, 1), 1), 1), &n_list_46_Nil))))) {
    u_value v_n = u_field(s1898, 0);
    u_value v_w = u_field(u_field(s1898, 1), 0);
    u_value v_ws = u_field(u_field(u_field(s1898, 1), 1), 0);
    if (u_truth(u_eq(v_n, f_syntax_46_cons_name_0()))) {
      u_value s1900 = f_value_46_list_elements_1(v_ws);
      u_value t1901 = NULL;
      if ((!t1901) && u_is_con(s1900, &n_option_46_Some)) {
        u_value v_us = u_field(s1900, 0);
        t1901 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_w, v_us));
      }
      if ((!t1901) && u_is_con(s1900, &n_option_46_None)) {
        t1901 = &o_option_46_None;
      }
      if (!t1901) {
        u_no_case(s1900);
      }
      t1899 = t1901;
    }
  }
  if (!t1899) {
    t1899 = &o_option_46_None;
  }
  if (!t1899) {
    u_no_case(s1898);
  }
  return t1899;
}

u_value f_print_46_show_value_1(u_value v_v) {
  u_value s1902 = v_v;
  u_value t1903 = NULL;
  if ((!t1903) && (u_is_con(s1902, &n_value_46_Lit) && u_is_con(u_field(s1902, 0), &n_syntax_46_VUnit))) {
    static struct u_object l__40__41__1904 = U_LIT("()");
    t1903 = &l__40__41__1904;
  }
  if ((!t1903) && (u_is_con(s1902, &n_value_46_Lit) && u_is_con(u_field(s1902, 0), &n_syntax_46_VBool))) {
    u_value v_b = u_field(u_field(s1902, 0), 0);
    u_value t1907;
    if (u_truth(v_b)) {
      static struct u_object l_true_1905 = U_LIT("true");
      t1907 = &l_true_1905;
    } else {
      static struct u_object l_false_1906 = U_LIT("false");
      t1907 = &l_false_1906;
    }
    t1903 = t1907;
  }
  if ((!t1903) && (u_is_con(s1902, &n_value_46_Lit) && u_is_con(u_field(s1902, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1902, 0), 0);
    t1903 = u_str_show(v_n);
  }
  if ((!t1903) && (u_is_con(s1902, &n_value_46_Lit) && u_is_con(u_field(s1902, 0), &n_syntax_46_VStr))) {
    u_value v_s = u_field(u_field(s1902, 0), 0);
    t1903 = u_str_quote(v_s);
  }
  if ((!t1903) && (u_is_con(s1902, &n_value_46_Lit) && u_is_con(u_field(s1902, 0), &n_syntax_46_VChar))) {
    u_value v_c = u_field(u_field(s1902, 0), 0);
    static struct u_object l__39__1908 = U_LIT("'");
    static struct u_object l__39__1909 = U_LIT("'");
    t1903 = u_concat(&l__39__1908, u_concat(f_print_46_escape_char_1(v_c), &l__39__1909));
  }
  if ((!t1903) && u_is_con(s1902, &n_value_46_Obj)) {
    u_value v_n = u_field(s1902, 0);
    u_value v_vs = u_field(s1902, 1);
    if (u_truth(u_eq(v_n, f_syntax_46_tuple_name_0()))) {
      static struct u_object l__40__1910 = U_LIT("(");
      static struct u_object l__41__1911 = U_LIT(")");
      t1903 = u_concat(&l__40__1910, u_concat(f_print_46_show_values_1(v_vs), &l__41__1911));
    }
  }
  if ((!t1903) && u_is_con(s1902, &n_value_46_Obj)) {
    u_value v_n = u_field(s1902, 0);
    u_value t1914 = u_eq(v_n, f_syntax_46_cons_name_0());
    if (u_truth(t1914)) {
      t1914 = f_prelude_46__33__61__2(f_value_46_list_elements_1(v_v), &o_option_46_None);
    }
    if (u_truth(t1914)) {
      static struct u_object l__91__1912 = U_LIT("[");
      static struct u_object l__93__1913 = U_LIT("]");
      t1903 = u_concat(&l__91__1912, u_concat(f_print_46_show_values_1(f_print_46_list_items_1(v_v)), &l__93__1913));
    }
  }
  if ((!t1903) && (u_is_con(s1902, &n_value_46_Obj) && u_is_con(u_field(s1902, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s1902, 0);
    t1903 = f_syntax_46_Name_46_base_1(v_n);
  }
  if ((!t1903) && u_is_con(s1902, &n_value_46_Obj)) {
    u_value v_n = u_field(s1902, 0);
    u_value v_vs = u_field(s1902, 1);
    static struct u_object l__40__1915 = U_LIT("(");
    static struct u_object l__41__1916 = U_LIT(")");
    t1903 = u_concat(f_syntax_46_Name_46_base_1(v_n), u_concat(&l__40__1915, u_concat(f_print_46_show_values_1(v_vs), &l__41__1916)));
  }
  if ((!t1903) && u_is_con(s1902, &n_value_46_Proc)) {
    static struct u_object l__60_procedure_62__1917 = U_LIT("<procedure>");
    t1903 = &l__60_procedure_62__1917;
  }
  if ((!t1903) && u_is_con(s1902, &n_value_46_ProcClos)) {
    static struct u_object l__60_procedure_62__1918 = U_LIT("<procedure>");
    t1903 = &l__60_procedure_62__1918;
  }
  if (!t1903) {
    static struct u_object l__60_function_62__1919 = U_LIT("<function>");
    t1903 = &l__60_function_62__1919;
  }
  if (!t1903) {
    u_no_case(s1902);
  }
  return t1903;
}

u_value f_print_46_show_values_1(u_value v_vs) {
  static struct u_object l__44__32__1920 = U_LIT(", ");
  u_value t1921 = &o_list_46_Nil;
  u_value *e1922 = &t1921;
  u_value l1923 = v_vs;
  while (u_is_con(l1923, &n_list_46_Cons)) {
    u_value v_v = u_field(l1923, 0);
    if (u_truth(u_mk_bool(1))) {
      e1922 = u_append(e1922, f_print_46_show_value_1(v_v));
    }
    l1923 = u_field(l1923, 1);
  }
  return u_str_join(&l__44__32__1920, t1921);
}

u_value f_print_46_list_items_1(u_value v_v) {
  u_value s1924 = f_value_46_list_elements_1(v_v);
  u_value t1925 = NULL;
  if ((!t1925) && u_is_con(s1924, &n_option_46_Some)) {
    u_value v_vs = u_field(s1924, 0);
    t1925 = v_vs;
  }
  if ((!t1925) && u_is_con(s1924, &n_option_46_None)) {
    t1925 = &o_list_46_Nil;
  }
  if (!t1925) {
    u_no_case(s1924);
  }
  return t1925;
}

u_value f_print_46_escape_char_1(u_value v_c) {
  u_value s1926 = v_c;
  u_value t1927 = NULL;
  if ((!t1927) && u_equals(s1926, u_mk_char(92))) {
    static struct u_object l__92__92__1928 = U_LIT("\\\\");
    t1927 = &l__92__92__1928;
  }
  if ((!t1927) && u_equals(s1926, u_mk_char(10))) {
    static struct u_object l__92_n_1929 = U_LIT("\\n");
    t1927 = &l__92_n_1929;
  }
  if ((!t1927) && u_equals(s1926, u_mk_char(9))) {
    static struct u_object l__92_t_1930 = U_LIT("\\t");
    t1927 = &l__92_t_1930;
  }
  if ((!t1927) && u_equals(s1926, u_mk_char(13))) {
    static struct u_object l__92_r_1931 = U_LIT("\\r");
    t1927 = &l__92_r_1931;
  }
  if ((!t1927) && u_equals(s1926, u_mk_char(39))) {
    static struct u_object l__92__39__1932 = U_LIT("\\'");
    t1927 = &l__92__39__1932;
  }
  if (!t1927) {
    t1927 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, &o_list_46_Nil));
  }
  if (!t1927) {
    u_no_case(s1926);
  }
  return t1927;
}

u_value f_print_46_show_type_1(u_value v_t) {
  u_value s1933 = v_t;
  u_value t1934 = NULL;
  if ((!t1934) && u_is_con(s1933, &n_syntax_46_TParam)) {
    u_value v_n = u_field(s1933, 0);
    t1934 = v_n;
  }
  if ((!t1934) && u_is_con(s1933, &n_syntax_46_THole)) {
    static struct u_object l___1935 = U_LIT("_");
    t1934 = &l___1935;
  }
  if ((!t1934) && u_is_con(s1933, &n_syntax_46_TMeta)) {
    u_value v_i = u_field(s1933, 0);
    static struct u_object l__63__1936 = U_LIT("?");
    t1934 = u_concat(&l__63__1936, u_str_show(v_i));
  }
  if ((!t1934) && (u_is_con(s1933, &n_syntax_46_TFun) && (u_is_con(u_field(s1933, 0), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1933, 0), 0), &n_syntax_46_TTuple) && (u_is_con(u_field(u_field(u_field(s1933, 0), 0), 0), &n_list_46_Nil) && u_is_con(u_field(u_field(s1933, 0), 1), &n_list_46_Nil)))))) {
    u_value v_r = u_field(s1933, 1);
    static struct u_object l__40__40__41__41__32__45__62__32__1937 = U_LIT("(()) -> ");
    t1934 = u_concat(&l__40__40__41__41__32__45__62__32__1937, f_print_46_show_type_1(v_r));
  }
  if ((!t1934) && (u_is_con(s1933, &n_syntax_46_TFun) && (u_is_con(u_field(s1933, 0), &n_list_46_Cons) && u_is_con(u_field(u_field(s1933, 0), 1), &n_list_46_Nil)))) {
    u_value v_a = u_field(u_field(s1933, 0), 0);
    u_value v_r = u_field(s1933, 1);
    static struct u_object l__32__45__62__32__1938 = U_LIT(" -> ");
    t1934 = u_concat(f_print_46_type_operand_1(v_a), u_concat(&l__32__45__62__32__1938, f_print_46_show_type_1(v_r)));
  }
  if ((!t1934) && u_is_con(s1933, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1933, 0);
    u_value v_r = u_field(s1933, 1);
    static struct u_object l__40__1939 = U_LIT("(");
    static struct u_object l__41__32__45__62__32__1940 = U_LIT(") -> ");
    t1934 = u_concat(&l__40__1939, u_concat(f_print_46_show_types_1(v_args), u_concat(&l__41__32__45__62__32__1940, f_print_46_show_type_1(v_r))));
  }
  if ((!t1934) && u_is_con(s1933, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s1933, 0);
    u_value v_os = u_field(s1933, 1);
    u_value v_rs = u_field(s1933, 2);
    static struct u_object l__40__1941 = U_LIT("(");
    static struct u_object l__44__32__1942 = U_LIT(", ");
    static struct u_object l_in_1943 = U_LIT("in");
    static struct u_object l_out_1944 = U_LIT("out");
    static struct u_object l_ref_1945 = U_LIT("ref");
    static struct u_object l__41__1946 = U_LIT(")");
    t1934 = u_concat(&l__40__1941, u_concat(u_str_join(&l__44__32__1942, f_list_46__43__43__2(f_print_46_type_section_2(&l_in_1943, v_is), f_list_46__43__43__2(f_print_46_type_section_2(&l_out_1944, v_os), f_print_46_type_section_2(&l_ref_1945, v_rs)))), &l__41__1946));
  }
  if ((!t1934) && (u_is_con(s1933, &n_syntax_46_TTuple) && u_is_con(u_field(s1933, 0), &n_list_46_Nil))) {
    static struct u_object l__40__41__1947 = U_LIT("()");
    t1934 = &l__40__41__1947;
  }
  if ((!t1934) && u_is_con(s1933, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1933, 0);
    static struct u_object l__32__42__32__1948 = U_LIT(" * ");
    u_value t1949 = &o_list_46_Nil;
    u_value *e1950 = &t1949;
    u_value l1951 = v_ts;
    while (u_is_con(l1951, &n_list_46_Cons)) {
      u_value v_a = u_field(l1951, 0);
      if (u_truth(u_mk_bool(1))) {
        e1950 = u_append(e1950, f_print_46_type_component_1(v_a));
      }
      l1951 = u_field(l1951, 1);
    }
    t1934 = u_str_join(&l__32__42__32__1948, t1949);
  }
  if ((!t1934) && (u_is_con(s1933, &n_syntax_46_TSort) && u_is_con(u_field(s1933, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s1933, 0);
    t1934 = f_syntax_46_Name_46_base_1(v_n);
  }
  if ((!t1934) && u_is_con(s1933, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s1933, 0);
    u_value v_args = u_field(s1933, 1);
    static struct u_object l__91__1952 = U_LIT("[");
    static struct u_object l__93__1953 = U_LIT("]");
    t1934 = u_concat(f_syntax_46_Name_46_base_1(v_n), u_concat(&l__91__1952, u_concat(f_print_46_show_types_1(v_args), &l__93__1953)));
  }
  if (!t1934) {
    u_no_case(s1933);
  }
  return t1934;
}

u_value f_print_46_show_types_1(u_value v_ts) {
  static struct u_object l__44__32__1954 = U_LIT(", ");
  u_value t1955 = &o_list_46_Nil;
  u_value *e1956 = &t1955;
  u_value l1957 = v_ts;
  while (u_is_con(l1957, &n_list_46_Cons)) {
    u_value v_t = u_field(l1957, 0);
    if (u_truth(u_mk_bool(1))) {
      e1956 = u_append(e1956, f_print_46_show_type_1(v_t));
    }
    l1957 = u_field(l1957, 1);
  }
  return u_str_join(&l__44__32__1954, t1955);
}

u_value f_print_46_type_section_2(u_value v_mode, u_value v_ts) {
  u_value t1959;
  if (u_truth(u_eq(v_ts, &o_list_46_Nil))) {
    t1959 = &o_list_46_Nil;
  } else {
    static struct u_object l__32__1958 = U_LIT(" ");
    t1959 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_mode, u_concat(&l__32__1958, f_print_46_show_types_1(v_ts))), &o_list_46_Nil);
  }
  return t1959;
}

u_value f_print_46_type_operand_1(u_value v_t) {
  u_value s1960 = v_t;
  u_value t1961 = NULL;
  if ((!t1961) && u_is_con(s1960, &n_syntax_46_TFun)) {
    static struct u_object l__40__1962 = U_LIT("(");
    static struct u_object l__41__1963 = U_LIT(")");
    t1961 = u_concat(&l__40__1962, u_concat(f_print_46_show_type_1(v_t), &l__41__1963));
  }
  if (!t1961) {
    t1961 = f_print_46_show_type_1(v_t);
  }
  if (!t1961) {
    u_no_case(s1960);
  }
  return t1961;
}

u_value f_print_46_type_component_1(u_value v_t) {
  u_value s1964 = v_t;
  u_value t1965 = NULL;
  if ((!t1965) && u_is_con(s1964, &n_syntax_46_TFun)) {
    static struct u_object l__40__1966 = U_LIT("(");
    static struct u_object l__41__1967 = U_LIT(")");
    t1965 = u_concat(&l__40__1966, u_concat(f_print_46_show_type_1(v_t), &l__41__1967));
  }
  if ((!t1965) && (u_is_con(s1964, &n_syntax_46_TTuple) && (u_is_con(u_field(s1964, 0), &n_list_46_Cons) && u_is_con(u_field(u_field(s1964, 0), 1), &n_list_46_Cons)))) {
    static struct u_object l__40__1968 = U_LIT("(");
    static struct u_object l__41__1969 = U_LIT(")");
    t1965 = u_concat(&l__40__1968, u_concat(f_print_46_show_type_1(v_t), &l__41__1969));
  }
  if (!t1965) {
    t1965 = f_print_46_show_type_1(v_t);
  }
  if (!t1965) {
    u_no_case(s1964);
  }
  return t1965;
}

u_value f_print_46_show_pat_1(u_value v_p) {
  u_value s1970 = v_p;
  u_value t1971 = NULL;
  if ((!t1971) && u_is_con(s1970, &n_syntax_46_PWild)) {
    static struct u_object l___1972 = U_LIT("_");
    t1971 = &l___1972;
  }
  if ((!t1971) && u_is_con(s1970, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s1970, 0);
    t1971 = f_print_46_show_value_1(u_mk_con(&n_value_46_Lit, 1, v_l));
  }
  if ((!t1971) && u_is_con(s1970, &n_syntax_46_PVar)) {
    u_value v_x = u_field(s1970, 0);
    t1971 = f_syntax_46_Name_46_base_1(f_syntax_46_Var_46_name_1(v_x));
  }
  if ((!t1971) && u_is_con(s1970, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1970, 0);
    static struct u_object l__40__1973 = U_LIT("(");
    static struct u_object l__41__1974 = U_LIT(")");
    t1971 = u_concat(&l__40__1973, u_concat(f_print_46_show_pats_1(v_ps), &l__41__1974));
  }
  if ((!t1971) && (u_is_con(s1970, &n_syntax_46_PApp) && (u_is_con(u_field(s1970, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1970, 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s1970, 1), 1), 1), &n_list_46_Nil))))) {
    u_value v_n = u_field(s1970, 0);
    u_value v_x = u_field(u_field(s1970, 1), 0);
    u_value v_xs = u_field(u_field(u_field(s1970, 1), 1), 0);
    u_value t1979 = u_eq(v_n, f_syntax_46_cons_name_0());
    if (!u_truth(t1979)) {
      static struct u_object l_Cons_1978 = U_LIT("Cons");
      t1979 = u_eq(v_n, f_syntax_46_unqualified_1(&l_Cons_1978));
    }
    if (u_truth(t1979)) {
      static struct u_object l__91__1975 = U_LIT("[");
      static struct u_object l__59__32__1976 = U_LIT("; ");
      static struct u_object l__93__1977 = U_LIT("]");
      t1971 = u_concat(&l__91__1975, u_concat(f_print_46_show_pat_1(v_x), u_concat(&l__59__32__1976, u_concat(f_print_46_show_pat_1(v_xs), &l__93__1977))));
    }
  }
  if ((!t1971) && (u_is_con(s1970, &n_syntax_46_PApp) && (u_is_con(u_field(s1970, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1970, 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s1970, 1), 1), 1), &n_list_46_Nil))))) {
    u_value v_n = u_field(s1970, 0);
    u_value v_l = u_field(u_field(s1970, 1), 0);
    u_value v_r = u_field(u_field(u_field(s1970, 1), 1), 0);
    if (u_truth(f_print_46_symbolic_1(f_syntax_46_Name_46_base_1(v_n)))) {
      t1971 = f_print_46_infix_text_3(f_syntax_46_Name_46_base_1(v_n), f_print_46_show_pat_1(v_l), f_print_46_show_pat_1(v_r));
    }
  }
  if ((!t1971) && (u_is_con(s1970, &n_syntax_46_PApp) && u_is_con(u_field(s1970, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s1970, 0);
    t1971 = f_syntax_46_Name_46_base_1(v_n);
  }
  if ((!t1971) && u_is_con(s1970, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s1970, 0);
    u_value v_ps = u_field(s1970, 1);
    static struct u_object l__40__1980 = U_LIT("(");
    static struct u_object l__41__1981 = U_LIT(")");
    t1971 = u_concat(f_syntax_46_Name_46_base_1(v_n), u_concat(&l__40__1980, u_concat(f_print_46_show_pats_1(v_ps), &l__41__1981)));
  }
  if (!t1971) {
    u_no_case(s1970);
  }
  return t1971;
}

u_value f_print_46_show_pats_1(u_value v_ps) {
  static struct u_object l__44__32__1982 = U_LIT(", ");
  u_value t1983 = &o_list_46_Nil;
  u_value *e1984 = &t1983;
  u_value l1985 = v_ps;
  while (u_is_con(l1985, &n_list_46_Cons)) {
    u_value v_p = u_field(l1985, 0);
    if (u_truth(u_mk_bool(1))) {
      e1984 = u_append(e1984, f_print_46_show_pat_1(v_p));
    }
    l1985 = u_field(l1985, 1);
  }
  return u_str_join(&l__44__32__1982, t1983);
}

u_value f_print_46_symbolic_1(u_value v_n) {
  u_value t1990 = u_str_get(v_n, u_mk_int(0));
  u_value t1989;
  {
    u_value v_c = t1990;
    u_value t1986 = f_prelude_46_not_1(u_is_letter(v_c));
    if (u_truth(t1986)) {
      t1986 = f_prelude_46__33__61__2(v_c, u_mk_char(95));
    }
    u_value t1988 = t1986;
    if (u_truth(t1988)) {
      static struct u_object l__44__1987 = U_LIT(",");
      t1988 = f_prelude_46__33__61__2(v_n, &l__44__1987);
    }
    t1989 = t1988;
  }
  return t1989;
}

u_value f_print_46_infix_text_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__32__1991 = U_LIT(" ");
  static struct u_object l__32__1992 = U_LIT(" ");
  return u_concat(f_print_46_pat_operand_1(v_l), u_concat(&l__32__1991, u_concat(v_op, u_concat(&l__32__1992, f_print_46_pat_operand_1(v_r)))));
}

u_value f_print_46_pat_operand_1(u_value v_a) {
  u_value t1993 = u_str_contains(v_a, u_mk_char(32));
  if (u_truth(t1993)) {
    t1993 = f_prelude_46__33__61__2(u_str_get(v_a, u_mk_int(0)), u_mk_char(40));
  }
  u_value t1996;
  if (u_truth(t1993)) {
    static struct u_object l__40__1994 = U_LIT("(");
    static struct u_object l__41__1995 = U_LIT(")");
    t1996 = u_concat(&l__40__1994, u_concat(v_a, &l__41__1995));
  } else {
    t1996 = v_a;
  }
  return t1996;
}

u_value f_print_46_text_of_1(u_value v_v) {
  u_value s1997 = v_v;
  u_value t1998 = NULL;
  if ((!t1998) && (u_is_con(s1997, &n_value_46_Lit) && u_is_con(u_field(s1997, 0), &n_syntax_46_VStr))) {
    u_value v_s = u_field(u_field(s1997, 0), 0);
    t1998 = v_s;
  }
  if (!t1998) {
    t1998 = f_print_46_show_value_1(v_v);
  }
  if (!t1998) {
    u_no_case(s1997);
  }
  return t1998;
}

u_value f_run_46_Prog_46_funs_1(u_value v_x) {
  u_value s1999 = v_x;
  u_value t2000 = NULL;
  if ((!t2000) && u_is_con(s1999, &n_run_46_Prog)) {
    u_value v_y = u_field(s1999, 0);
    t2000 = v_y;
  }
  if (!t2000) {
    u_no_case(s1999);
  }
  return t2000;
}

u_value f_run_46_Prog_46_procs_1(u_value v_x) {
  u_value s2001 = v_x;
  u_value t2002 = NULL;
  if ((!t2002) && u_is_con(s2001, &n_run_46_Prog)) {
    u_value v_y = u_field(s2001, 1);
    t2002 = v_y;
  }
  if (!t2002) {
    u_no_case(s2001);
  }
  return t2002;
}

u_value f_run_46_Prog_46_constrs_1(u_value v_x) {
  u_value s2003 = v_x;
  u_value t2004 = NULL;
  if ((!t2004) && u_is_con(s2003, &n_run_46_Prog)) {
    u_value v_y = u_field(s2003, 2);
    t2004 = v_y;
  }
  if (!t2004) {
    u_no_case(s2003);
  }
  return t2004;
}

u_value f_run_46_Prog_46_positions_1(u_value v_x) {
  u_value s2005 = v_x;
  u_value t2006 = NULL;
  if ((!t2006) && u_is_con(s2005, &n_run_46_Prog)) {
    u_value v_y = u_field(s2005, 3);
    t2006 = v_y;
  }
  if (!t2006) {
    u_no_case(s2005);
  }
  return t2006;
}

u_value f_run_46_Prog_46_computing_1(u_value v_x) {
  u_value s2007 = v_x;
  u_value t2008 = NULL;
  if ((!t2008) && u_is_con(s2007, &n_run_46_Prog)) {
    u_value v_y = u_field(s2007, 4);
    t2008 = v_y;
  }
  if (!t2008) {
    u_no_case(s2007);
  }
  return t2008;
}

u_value f_run_46_Prog_46_args_1(u_value v_x) {
  u_value s2009 = v_x;
  u_value t2010 = NULL;
  if ((!t2010) && u_is_con(s2009, &n_run_46_Prog)) {
    u_value v_y = u_field(s2009, 5);
    t2010 = v_y;
  }
  if (!t2010) {
    u_no_case(s2009);
  }
  return t2010;
}

u_value f_run_46_Prog_46_funs_58__61__2(u_value v_x, u_value v_y) {
  u_value s2011 = v_x;
  u_value t2012 = NULL;
  if ((!t2012) && u_is_con(s2011, &n_run_46_Prog)) {
    u_value v_a1 = u_field(s2011, 1);
    u_value v_a2 = u_field(s2011, 2);
    u_value v_a3 = u_field(s2011, 3);
    u_value v_a4 = u_field(s2011, 4);
    u_value v_a5 = u_field(s2011, 5);
    t2012 = u_mk_con(&n_run_46_Prog, 6, v_y, v_a1, v_a2, v_a3, v_a4, v_a5);
  }
  if (!t2012) {
    u_no_case(s2011);
  }
  return t2012;
}

u_value f_run_46_Prog_46_procs_58__61__2(u_value v_x, u_value v_y) {
  u_value s2013 = v_x;
  u_value t2014 = NULL;
  if ((!t2014) && u_is_con(s2013, &n_run_46_Prog)) {
    u_value v_a0 = u_field(s2013, 0);
    u_value v_a2 = u_field(s2013, 2);
    u_value v_a3 = u_field(s2013, 3);
    u_value v_a4 = u_field(s2013, 4);
    u_value v_a5 = u_field(s2013, 5);
    t2014 = u_mk_con(&n_run_46_Prog, 6, v_a0, v_y, v_a2, v_a3, v_a4, v_a5);
  }
  if (!t2014) {
    u_no_case(s2013);
  }
  return t2014;
}

u_value f_run_46_Prog_46_constrs_58__61__2(u_value v_x, u_value v_y) {
  u_value s2015 = v_x;
  u_value t2016 = NULL;
  if ((!t2016) && u_is_con(s2015, &n_run_46_Prog)) {
    u_value v_a0 = u_field(s2015, 0);
    u_value v_a1 = u_field(s2015, 1);
    u_value v_a3 = u_field(s2015, 3);
    u_value v_a4 = u_field(s2015, 4);
    u_value v_a5 = u_field(s2015, 5);
    t2016 = u_mk_con(&n_run_46_Prog, 6, v_a0, v_a1, v_y, v_a3, v_a4, v_a5);
  }
  if (!t2016) {
    u_no_case(s2015);
  }
  return t2016;
}

u_value f_run_46_Prog_46_positions_58__61__2(u_value v_x, u_value v_y) {
  u_value s2017 = v_x;
  u_value t2018 = NULL;
  if ((!t2018) && u_is_con(s2017, &n_run_46_Prog)) {
    u_value v_a0 = u_field(s2017, 0);
    u_value v_a1 = u_field(s2017, 1);
    u_value v_a2 = u_field(s2017, 2);
    u_value v_a4 = u_field(s2017, 4);
    u_value v_a5 = u_field(s2017, 5);
    t2018 = u_mk_con(&n_run_46_Prog, 6, v_a0, v_a1, v_a2, v_y, v_a4, v_a5);
  }
  if (!t2018) {
    u_no_case(s2017);
  }
  return t2018;
}

u_value f_run_46_Prog_46_computing_58__61__2(u_value v_x, u_value v_y) {
  u_value s2019 = v_x;
  u_value t2020 = NULL;
  if ((!t2020) && u_is_con(s2019, &n_run_46_Prog)) {
    u_value v_a0 = u_field(s2019, 0);
    u_value v_a1 = u_field(s2019, 1);
    u_value v_a2 = u_field(s2019, 2);
    u_value v_a3 = u_field(s2019, 3);
    u_value v_a5 = u_field(s2019, 5);
    t2020 = u_mk_con(&n_run_46_Prog, 6, v_a0, v_a1, v_a2, v_a3, v_y, v_a5);
  }
  if (!t2020) {
    u_no_case(s2019);
  }
  return t2020;
}

u_value f_run_46_Prog_46_args_58__61__2(u_value v_x, u_value v_y) {
  u_value s2021 = v_x;
  u_value t2022 = NULL;
  if ((!t2022) && u_is_con(s2021, &n_run_46_Prog)) {
    u_value v_a0 = u_field(s2021, 0);
    u_value v_a1 = u_field(s2021, 1);
    u_value v_a2 = u_field(s2021, 2);
    u_value v_a3 = u_field(s2021, 3);
    u_value v_a4 = u_field(s2021, 4);
    t2022 = u_mk_con(&n_run_46_Prog, 6, v_a0, v_a1, v_a2, v_a3, v_a4, v_y);
  }
  if (!t2022) {
    u_no_case(s2021);
  }
  return t2022;
}

u_value f_run_46_position_of_2(u_value v_p, u_value v_k) {
  u_value s2023 = f_map_46_get_2(v_k, f_run_46_Prog_46_positions_1(v_p));
  u_value t2024 = NULL;
  if ((!t2024) && u_is_con(s2023, &n_option_46_Some)) {
    u_value v_i = u_field(s2023, 0);
    t2024 = v_i;
  }
  if ((!t2024) && u_is_con(s2023, &n_option_46_None)) {
    t2024 = u_mk_int(0);
  }
  if (!t2024) {
    u_no_case(s2023);
  }
  return t2024;
}

u_value f_run_46_load_1(u_value v_q) {
  u_value s2025 = v_q;
  u_value t2026 = NULL;
  if ((!t2026) && u_is_con(s2025, &n_syntax_46_Program)) {
    u_value v_cs = u_field(s2025, 0);
    t2026 = u_mk_con(&n_run_46_Prog, 6, f_map_46_put_all_2(f_run_46_fun_defs_1(v_cs), f_map_46_empty_0()), f_map_46_put_all_2(f_run_46_proc_defs_1(v_cs), f_map_46_empty_0()), f_map_46_put_all_2(f_run_46_constr_defs_1(v_cs), f_map_46_empty_0()), f_map_46_put_all_2(f_run_46_constr_positions_1(v_cs), f_map_46_empty_0()), &o_list_46_Nil, &o_list_46_Nil);
  }
  if (!t2026) {
    u_no_case(s2025);
  }
  return t2026;
}

u_value f_run_46_fun_defs_1(u_value v_cs) {
  u_value s2027 = v_cs;
  u_value t2028 = NULL;
  if ((!t2028) && u_is_con(s2027, &n_list_46_Nil)) {
    t2028 = &o_list_46_Nil;
  }
  if ((!t2028) && (u_is_con(s2027, &n_list_46_Cons) && u_is_con(u_field(s2027, 0), &n_syntax_46_CFun))) {
    u_value v_n = u_field(u_field(s2027, 0), 0);
    u_value v_xss = u_field(u_field(s2027, 0), 2);
    u_value v_b = u_field(u_field(s2027, 0), 4);
    u_value v_rest = u_field(s2027, 1);
    t2028 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_tuple(2, v_xss, v_b)), f_run_46_fun_defs_1(v_rest));
  }
  if ((!t2028) && u_is_con(s2027, &n_list_46_Cons)) {
    u_value v_rest = u_field(s2027, 1);
    t2028 = f_run_46_fun_defs_1(v_rest);
  }
  if (!t2028) {
    u_no_case(s2027);
  }
  return t2028;
}

u_value f_run_46_proc_defs_1(u_value v_cs) {
  u_value s2029 = v_cs;
  u_value t2030 = NULL;
  if ((!t2030) && u_is_con(s2029, &n_list_46_Nil)) {
    t2030 = &o_list_46_Nil;
  }
  if ((!t2030) && (u_is_con(s2029, &n_list_46_Cons) && u_is_con(u_field(s2029, 0), &n_syntax_46_CProc))) {
    u_value v_n = u_field(u_field(s2029, 0), 0);
    u_value v_ins = u_field(u_field(s2029, 0), 2);
    u_value v_outs = u_field(u_field(s2029, 0), 3);
    u_value v_refs = u_field(u_field(s2029, 0), 4);
    u_value v_b = u_field(u_field(s2029, 0), 5);
    u_value v_rest = u_field(s2029, 1);
    t2030 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_con(&n_run_46_ProcDef, 4, v_ins, v_outs, v_refs, v_b)), f_run_46_proc_defs_1(v_rest));
  }
  if ((!t2030) && u_is_con(s2029, &n_list_46_Cons)) {
    u_value v_rest = u_field(s2029, 1);
    t2030 = f_run_46_proc_defs_1(v_rest);
  }
  if (!t2030) {
    u_no_case(s2029);
  }
  return t2030;
}

u_value f_run_46_constr_positions_1(u_value v_cs) {
  u_value s2031 = v_cs;
  u_value t2032 = NULL;
  if ((!t2032) && u_is_con(s2031, &n_list_46_Nil)) {
    t2032 = &o_list_46_Nil;
  }
  if ((!t2032) && (u_is_con(s2031, &n_list_46_Cons) && u_is_con(u_field(s2031, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s2031, 0), 2);
    u_value v_rest = u_field(s2031, 1);
    u_value t2033 = &o_list_46_Nil;
    u_value *e2034 = &t2033;
    u_value l2035 = v_ks;
    while (u_is_con(l2035, &n_list_46_Cons)) {
      u_value v_k = u_field(l2035, 0);
      if (u_truth(u_mk_bool(1))) {
        e2034 = u_append(e2034, f_syntax_46_Constr_46_name_1(v_k));
      }
      l2035 = u_field(l2035, 1);
    }
    t2032 = f_list_46__43__43__2(f_list_46_zip_2(t2033, f_run_46_numbers_from_2(u_mk_int(0), f_list_46_length_1(v_ks))), f_run_46_constr_positions_1(v_rest));
  }
  if ((!t2032) && u_is_con(s2031, &n_list_46_Cons)) {
    u_value v_rest = u_field(s2031, 1);
    t2032 = f_run_46_constr_positions_1(v_rest);
  }
  if (!t2032) {
    u_no_case(s2031);
  }
  return t2032;
}

u_value f_run_46_numbers_from_2(u_value v_i, u_value v_n) {
  u_value t2036;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t2036 = &o_list_46_Nil;
  } else {
    t2036 = u_mk_con(&n_list_46_Cons, 2, v_i, f_run_46_numbers_from_2(u_add(v_i, u_mk_int(1)), u_sub(v_n, u_mk_int(1))));
  }
  return t2036;
}

u_value f_run_46_constr_defs_1(u_value v_cs) {
  u_value s2037 = v_cs;
  u_value t2038 = NULL;
  if ((!t2038) && u_is_con(s2037, &n_list_46_Nil)) {
    t2038 = &o_list_46_Nil;
  }
  if ((!t2038) && (u_is_con(s2037, &n_list_46_Cons) && u_is_con(u_field(s2037, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s2037, 0), 2);
    u_value v_rest = u_field(s2037, 1);
    u_value t2039 = &o_list_46_Nil;
    u_value *e2040 = &t2039;
    u_value l2041 = v_ks;
    while (u_is_con(l2041, &n_list_46_Cons)) {
      u_value v_k = u_field(l2041, 0);
      if (u_truth(u_mk_bool(1))) {
        e2040 = u_append(e2040, f_run_46_constr_arity_1(v_k));
      }
      l2041 = u_field(l2041, 1);
    }
    t2038 = f_list_46__43__43__2(t2039, f_run_46_constr_defs_1(v_rest));
  }
  if ((!t2038) && u_is_con(s2037, &n_list_46_Cons)) {
    u_value v_rest = u_field(s2037, 1);
    t2038 = f_run_46_constr_defs_1(v_rest);
  }
  if (!t2038) {
    u_no_case(s2037);
  }
  return t2038;
}

u_value f_run_46_constr_arity_1(u_value v_k) {
  u_value s2042 = v_k;
  u_value t2043 = NULL;
  if ((!t2043) && u_is_con(s2042, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s2042, 0);
    u_value v_ts = u_field(s2042, 1);
    t2043 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t2043) {
    u_no_case(s2042);
  }
  return t2043;
}

u_value f_run_46_builtin_names_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of run.builtin_names");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l__43__2044 = U_LIT("+");
  static struct u_object l__45__2045 = U_LIT("-");
  static struct u_object l__42__2046 = U_LIT("*");
  static struct u_object l__47__2047 = U_LIT("/");
  static struct u_object l__37__2048 = U_LIT("%");
  static struct u_object l__60__2049 = U_LIT("<");
  static struct u_object l__61__61__2050 = U_LIT("==");
  static struct u_object l_chars_2051 = U_LIT("chars");
  static struct u_object l_string_2052 = U_LIT("string");
  static struct u_object l_ord_2053 = U_LIT("ord");
  static struct u_object l_chr_2054 = U_LIT("chr");
  static struct u_object l__94__2055 = U_LIT("^");
  static struct u_object l_str_size_2056 = U_LIT("str_size");
  static struct u_object l_str_join_2057 = U_LIT("str_join");
  static struct u_object l_str_slice_2058 = U_LIT("str_slice");
  static struct u_object l_str_split_2059 = U_LIT("str_split");
  static struct u_object l_compare_2060 = U_LIT("compare");
  static struct u_object l_str_get_2061 = U_LIT("str_get");
  static struct u_object l_str_contains_2062 = U_LIT("str_contains");
  static struct u_object l_str_compare_2063 = U_LIT("str_compare");
  static struct u_object l_str_show_2064 = U_LIT("str_show");
  static struct u_object l_str_quote_2065 = U_LIT("str_quote");
  static struct u_object l_str_number_2066 = U_LIT("str_number");
  static struct u_object l_is_digit_2067 = U_LIT("is_digit");
  static struct u_object l_is_letter_2068 = U_LIT("is_letter");
  static struct u_object l_is_space_2069 = U_LIT("is_space");
  u_pop(handler);
  cache = f_set_46_from_list_1(u_mk_con(&n_list_46_Cons, 2, &l__43__2044, u_mk_con(&n_list_46_Cons, 2, &l__45__2045, u_mk_con(&n_list_46_Cons, 2, &l__42__2046, u_mk_con(&n_list_46_Cons, 2, &l__47__2047, u_mk_con(&n_list_46_Cons, 2, &l__37__2048, u_mk_con(&n_list_46_Cons, 2, &l__60__2049, u_mk_con(&n_list_46_Cons, 2, &l__61__61__2050, u_mk_con(&n_list_46_Cons, 2, &l_chars_2051, u_mk_con(&n_list_46_Cons, 2, &l_string_2052, u_mk_con(&n_list_46_Cons, 2, &l_ord_2053, u_mk_con(&n_list_46_Cons, 2, &l_chr_2054, u_mk_con(&n_list_46_Cons, 2, &l__94__2055, u_mk_con(&n_list_46_Cons, 2, &l_str_size_2056, u_mk_con(&n_list_46_Cons, 2, &l_str_join_2057, u_mk_con(&n_list_46_Cons, 2, &l_str_slice_2058, u_mk_con(&n_list_46_Cons, 2, &l_str_split_2059, u_mk_con(&n_list_46_Cons, 2, &l_compare_2060, u_mk_con(&n_list_46_Cons, 2, &l_str_get_2061, u_mk_con(&n_list_46_Cons, 2, &l_str_contains_2062, u_mk_con(&n_list_46_Cons, 2, &l_str_compare_2063, u_mk_con(&n_list_46_Cons, 2, &l_str_show_2064, u_mk_con(&n_list_46_Cons, 2, &l_str_quote_2065, u_mk_con(&n_list_46_Cons, 2, &l_str_number_2066, u_mk_con(&n_list_46_Cons, 2, &l_is_digit_2067, u_mk_con(&n_list_46_Cons, 2, &l_is_letter_2068, u_mk_con(&n_list_46_Cons, 2, &l_is_space_2069, &o_list_46_Nil)))))))))))))))))))))))))));
  return cache;
}

u_value f_run_46_is_builtin_1(u_value v_n) {
  u_value s2070 = v_n;
  u_value t2071 = NULL;
  if ((!t2071) && (u_is_con(s2070, &n_syntax_46_Name) && u_is_con(u_field(s2070, 0), &n_option_46_None))) {
    u_value v_b = u_field(s2070, 1);
    t2071 = f_set_46_member_2(v_b, f_run_46_builtin_names_0());
  }
  if (!t2071) {
    t2071 = u_mk_bool(0);
  }
  if (!t2071) {
    u_no_case(s2070);
  }
  return t2071;
}

u_value f_run_46_is_function_1(u_value v_v) {
  u_value s2072 = v_v;
  u_value t2073 = NULL;
  if ((!t2073) && u_is_con(s2072, &n_value_46_Lit)) {
    t2073 = u_mk_bool(0);
  }
  if ((!t2073) && u_is_con(s2072, &n_value_46_Obj)) {
    t2073 = u_mk_bool(0);
  }
  if (!t2073) {
    t2073 = u_mk_bool(1);
  }
  if (!t2073) {
    u_no_case(s2072);
  }
  return t2073;
}

u_value f_run_46_truth_1(u_value v_v) {
  return u_eq(v_v, u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1))));
}

u_value f_run_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s2074 = v_xss;
  u_value t2075 = NULL;
  if ((!t2075) && u_is_con(s2074, &n_list_46_Nil)) {
    t2075 = v_b;
  }
  if ((!t2075) && u_is_con(s2074, &n_list_46_Cons)) {
    u_value v_xs = u_field(s2074, 0);
    u_value v_rest = u_field(s2074, 1);
    t2075 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_run_46_lambdas_2(v_rest, v_b));
  }
  if (!t2075) {
    u_no_case(s2074);
  }
  return t2075;
}

u_value f_run_46_str_values_1(u_value v_vs) {
  u_value s2076 = v_vs;
  u_value t2077 = NULL;
  if ((!t2077) && (u_is_con(s2076, &n_list_46_Cons) && (u_is_con(u_field(s2076, 0), &n_value_46_Lit) && u_is_con(u_field(u_field(s2076, 0), 0), &n_syntax_46_VStr)))) {
    u_value v_s = u_field(u_field(u_field(s2076, 0), 0), 0);
    u_value v_rest = u_field(s2076, 1);
    t2077 = u_mk_con(&n_list_46_Cons, 2, v_s, f_run_46_str_values_1(v_rest));
  }
  if (!t2077) {
    t2077 = &o_list_46_Nil;
  }
  if (!t2077) {
    u_no_case(s2076);
  }
  return t2077;
}

u_value f_run_46_char_values_1(u_value v_vs) {
  u_value s2078 = v_vs;
  u_value t2079 = NULL;
  if ((!t2079) && (u_is_con(s2078, &n_list_46_Cons) && (u_is_con(u_field(s2078, 0), &n_value_46_Lit) && u_is_con(u_field(u_field(s2078, 0), 0), &n_syntax_46_VChar)))) {
    u_value v_c = u_field(u_field(u_field(s2078, 0), 0), 0);
    u_value v_rest = u_field(s2078, 1);
    t2079 = u_mk_con(&n_list_46_Cons, 2, v_c, f_run_46_char_values_1(v_rest));
  }
  if (!t2079) {
    t2079 = &o_list_46_Nil;
  }
  if (!t2079) {
    u_no_case(s2078);
  }
  return t2079;
}

u_value f_run_46_bind_all_3(u_value v_xs, u_value v_vs, u_value v_env) {
  u_value s2080 = u_mk_tuple(2, v_xs, v_vs);
  u_value t2081 = NULL;
  if ((!t2081) && (u_is_con(u_field(s2080, 0), &n_list_46_Cons) && u_is_con(u_field(s2080, 1), &n_list_46_Cons))) {
    u_value v_x = u_field(u_field(s2080, 0), 0);
    u_value v_xs1 = u_field(u_field(s2080, 0), 1);
    u_value v_v = u_field(u_field(s2080, 1), 0);
    u_value v_vs1 = u_field(u_field(s2080, 1), 1);
    t2081 = f_run_46_bind_all_3(v_xs1, v_vs1, f_map_46_put_3(f_syntax_46_Var_46_name_1(v_x), v_v, v_env));
  }
  if (!t2081) {
    t2081 = v_env;
  }
  if (!t2081) {
    u_no_case(s2080);
  }
  return t2081;
}

u_value f_run_46_match_into_3(u_value v_p, u_value v_v, u_value v_env) {
  u_value s2082 = u_mk_tuple(2, v_p, v_v);
  u_value t2083 = NULL;
  if ((!t2083) && u_is_con(u_field(s2082, 0), &n_syntax_46_PWild)) {
    t2083 = u_mk_con(&n_option_46_Some, 1, v_env);
  }
  if ((!t2083) && u_is_con(u_field(s2082, 0), &n_syntax_46_PLit)) {
    u_value v_l = u_field(u_field(s2082, 0), 0);
    u_value t2084;
    if (u_truth(u_eq(v_v, u_mk_con(&n_value_46_Lit, 1, v_l)))) {
      t2084 = u_mk_con(&n_option_46_Some, 1, v_env);
    } else {
      t2084 = &o_option_46_None;
    }
    t2083 = t2084;
  }
  if ((!t2083) && (u_is_con(u_field(s2082, 0), &n_syntax_46_PVar) && u_is_con(u_field(u_field(s2082, 0), 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(u_field(s2082, 0), 0), 0);
    t2083 = u_mk_con(&n_option_46_Some, 1, f_map_46_put_3(v_n, v_v, v_env));
  }
  if ((!t2083) && (u_is_con(u_field(s2082, 0), &n_syntax_46_PApp) && u_is_con(u_field(s2082, 1), &n_value_46_Obj))) {
    u_value v_c = u_field(u_field(s2082, 0), 0);
    u_value v_ps = u_field(u_field(s2082, 0), 1);
    u_value v_d = u_field(u_field(s2082, 1), 0);
    u_value v_vs = u_field(u_field(s2082, 1), 1);
    u_value t2085;
    if (u_truth(u_eq(v_c, v_d))) {
      t2085 = f_run_46_match_all_into_3(v_ps, v_vs, v_env);
    } else {
      t2085 = &o_option_46_None;
    }
    t2083 = t2085;
  }
  if ((!t2083) && u_is_con(u_field(s2082, 0), &n_syntax_46_PApp)) {
    t2083 = &o_option_46_None;
  }
  if ((!t2083) && (u_is_con(u_field(s2082, 0), &n_syntax_46_PTuple) && u_is_con(u_field(s2082, 1), &n_value_46_Obj))) {
    u_value v_ps = u_field(u_field(s2082, 0), 0);
    u_value v_n = u_field(u_field(s2082, 1), 0);
    u_value v_vs = u_field(u_field(s2082, 1), 1);
    if (u_truth(u_eq(v_n, f_syntax_46_tuple_name_0()))) {
      t2083 = f_run_46_match_all_into_3(v_ps, v_vs, v_env);
    }
  }
  if ((!t2083) && u_is_con(u_field(s2082, 0), &n_syntax_46_PTuple)) {
    t2083 = &o_option_46_None;
  }
  if (!t2083) {
    u_no_case(s2082);
  }
  return t2083;
}

u_value f_run_46_match_all_into_3(u_value v_ps, u_value v_vs, u_value v_env) {
  u_value s2086 = u_mk_tuple(2, v_ps, v_vs);
  u_value t2087 = NULL;
  if ((!t2087) && (u_is_con(u_field(s2086, 0), &n_list_46_Nil) && u_is_con(u_field(s2086, 1), &n_list_46_Nil))) {
    t2087 = u_mk_con(&n_option_46_Some, 1, v_env);
  }
  if ((!t2087) && (u_is_con(u_field(s2086, 0), &n_list_46_Cons) && u_is_con(u_field(s2086, 1), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s2086, 0), 0);
    u_value v_ps1 = u_field(u_field(s2086, 0), 1);
    u_value v_v = u_field(u_field(s2086, 1), 0);
    u_value v_vs1 = u_field(u_field(s2086, 1), 1);
    u_value s2088 = f_run_46_match_into_3(v_p, v_v, v_env);
    u_value t2089 = NULL;
    if ((!t2089) && u_is_con(s2088, &n_option_46_Some)) {
      u_value v_inner = u_field(s2088, 0);
      t2089 = f_run_46_match_all_into_3(v_ps1, v_vs1, v_inner);
    }
    if ((!t2089) && u_is_con(s2088, &n_option_46_None)) {
      t2089 = &o_option_46_None;
    }
    if (!t2089) {
      u_no_case(s2088);
    }
    t2087 = t2089;
  }
  if (!t2087) {
    t2087 = &o_option_46_None;
  }
  if (!t2087) {
    u_no_case(s2086);
  }
  return t2087;
}

u_value f_run_46_pattern_vars_1(u_value v_p) {
  u_value s2090 = v_p;
  u_value t2091 = NULL;
  if ((!t2091) && (u_is_con(s2090, &n_syntax_46_PVar) && u_is_con(u_field(s2090, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2090, 0), 0);
    t2091 = u_mk_con(&n_list_46_Cons, 2, v_n, &o_list_46_Nil);
  }
  if ((!t2091) && u_is_con(s2090, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2090, 1);
    u_value t2092 = &o_list_46_Nil;
    u_value *e2093 = &t2092;
    u_value l2094 = v_ps;
    while (u_is_con(l2094, &n_list_46_Cons)) {
      u_value v_q = u_field(l2094, 0);
      if (u_truth(u_mk_bool(1))) {
        e2093 = u_append(e2093, f_run_46_pattern_vars_1(v_q));
      }
      l2094 = u_field(l2094, 1);
    }
    t2091 = f_list_46_concat_1(t2092);
  }
  if ((!t2091) && u_is_con(s2090, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2090, 0);
    u_value t2095 = &o_list_46_Nil;
    u_value *e2096 = &t2095;
    u_value l2097 = v_ps;
    while (u_is_con(l2097, &n_list_46_Cons)) {
      u_value v_q = u_field(l2097, 0);
      if (u_truth(u_mk_bool(1))) {
        e2096 = u_append(e2096, f_run_46_pattern_vars_1(v_q));
      }
      l2097 = u_field(l2097, 1);
    }
    t2091 = f_list_46_concat_1(t2095);
  }
  if (!t2091) {
    t2091 = &o_list_46_Nil;
  }
  if (!t2091) {
    u_no_case(s2090);
  }
  return t2091;
}

void p_run_46_evaluate(u_value v_p, u_value v_env, u_value v_e, u_value *r_v, u_value *r_cache) {
  {
    u_value v_w = NULL;
    u_value v_vs = NULL;
    u_value v_envs = NULL;
    u_value v_sel = NULL;
    u_value s2098 = v_e;
    int d2099 = 0;
    if ((!d2099) && u_is_con(s2098, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s2098, 0);
      d2099 = 1;
      {
        u_value t2100 = u_mk_con(&n_value_46_Lit, 1, v_l);
        *r_v = t2100;
      }
    }
    if ((!d2099) && (u_is_con(s2098, &n_syntax_46_EVar) && u_is_con(u_field(s2098, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s2098, 0), 0);
      u_value v_t = u_field(u_field(s2098, 0), 1);
      d2099 = 1;
      {
        u_value t2101 = f_run_46_variable_value_4(v_p, v_env, v_n, v_t);
        *r_v = t2101;
      }
    }
    if ((!d2099) && u_is_con(s2098, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s2098, 0);
      u_value v_args = u_field(s2098, 1);
      d2099 = 1;
      {
        p_run_46_apply_expr(v_p, v_env, v_f, v_args, r_v, r_cache);
      }
    }
    if ((!d2099) && u_is_con(s2098, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s2098, 0);
      u_value v_body = u_field(s2098, 1);
      d2099 = 1;
      {
        u_value t2102 = u_mk_con(&n_value_46_Clos, 3, v_env, v_xs, v_body);
        *r_v = t2102;
      }
    }
    if ((!d2099) && u_is_con(s2098, &n_syntax_46_EProc)) {
      u_value v_xs = u_field(s2098, 0);
      u_value v_ys = u_field(s2098, 1);
      u_value v_zs = u_field(s2098, 2);
      u_value v_body = u_field(s2098, 3);
      d2099 = 1;
      {
        u_value t2103 = u_mk_con(&n_value_46_ProcClos, 5, v_env, v_xs, v_ys, v_zs, v_body);
        *r_v = t2103;
      }
    }
    if ((!d2099) && u_is_con(s2098, &n_syntax_46_ELet)) {
      u_value v_bound = u_field(s2098, 0);
      u_value v_body = u_field(s2098, 1);
      d2099 = 1;
      {
        p_run_46_evaluate_all(v_p, v_env, f_list_46_seconds_1(v_bound), &v_vs, r_cache);
        p_run_46_evaluate(v_p, f_run_46_bind_all_3(f_list_46_firsts_1(v_bound), v_vs, v_env), v_body, r_v, r_cache);
      }
    }
    if ((!d2099) && u_is_con(s2098, &n_syntax_46_EIte)) {
      u_value v_cond = u_field(s2098, 0);
      u_value v_left = u_field(s2098, 1);
      u_value v_right = u_field(s2098, 2);
      d2099 = 1;
      {
        p_run_46_evaluate(v_p, v_env, v_cond, &v_w, r_cache);
        if (u_truth(f_run_46_truth_1(v_w))) {
          {
            p_run_46_evaluate(v_p, v_env, v_left, r_v, r_cache);
          }
        } else {
          {
            p_run_46_evaluate(v_p, v_env, v_right, r_v, r_cache);
          }
        }
      }
    }
    if ((!d2099) && u_is_con(s2098, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s2098, 0);
      u_value v_cases = u_field(s2098, 1);
      d2099 = 1;
      {
        p_run_46_evaluate(v_p, v_env, v_arg, &v_w, r_cache);
        p_run_46_select(v_p, v_env, v_w, v_cases, &v_sel, r_cache);
        u_value s2104 = v_sel;
        int d2105 = 0;
        if ((!d2105) && u_is_con(s2104, &n_option_46_Some)) {
          u_value v_body = u_field(u_field(s2104, 0), 0);
          u_value v_inner = u_field(u_field(s2104, 0), 1);
          d2105 = 1;
          {
            p_run_46_evaluate(v_p, v_inner, v_body, r_v, r_cache);
          }
        }
        if ((!d2105) && u_is_con(s2104, &n_option_46_None)) {
          d2105 = 1;
          {
            u_raise(u_mk_con(&n_run_46_NoCase, 1, v_w), "run.RuntimeError");
          }
        }
        if (!d2105) {
          u_no_case(s2104);
        }
      }
    }
    if ((!d2099) && u_is_con(s2098, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s2098, 0);
      u_value v_xs = u_field(s2098, 1);
      u_value v_lists = u_field(s2098, 2);
      u_value v_cond = u_field(s2098, 3);
      d2099 = 1;
      {
        p_run_46_combinations(v_p, v_env, v_xs, v_lists, &v_envs, r_cache);
        p_run_46_comprehension(v_p, v_envs, v_result, v_cond, &v_vs, r_cache);
        u_value t2106 = f_value_46_list_value_1(v_vs);
        *r_v = t2106;
      }
    }
    if ((!d2099) && u_is_con(s2098, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s2098, 0);
      d2099 = 1;
      {
        p_run_46_evaluate_all(v_p, v_env, v_items, &v_vs, r_cache);
        u_value t2107 = u_mk_con(&n_value_46_Obj, 2, f_syntax_46_tuple_name_0(), v_vs);
        *r_v = t2107;
      }
    }
    if ((!d2099) && u_is_con(s2098, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s2098, 0);
      u_value v_t = u_field(s2098, 1);
      d2099 = 1;
      {
        p_run_46_evaluate(v_p, v_env, v_arg, r_v, r_cache);
        u_raise(u_mk_con(&n_run_46_Thrown, 2, *r_v, v_t), "run.Thrown");
      }
    }
    if (!d2099) {
      u_no_case(s2098);
    }
  }
}

void p_run_46_evaluate_all(u_value v_p, u_value v_env, u_value v_es, u_value *r_vs, u_value *r_cache) {
  {
    u_value v_v = NULL;
    u_value v_rest = NULL;
    u_value s2108 = v_es;
    int d2109 = 0;
    if ((!d2109) && u_is_con(s2108, &n_list_46_Nil)) {
      d2109 = 1;
      {
        u_value t2110 = &o_list_46_Nil;
        *r_vs = t2110;
      }
    }
    if ((!d2109) && u_is_con(s2108, &n_list_46_Cons)) {
      u_value v_e = u_field(s2108, 0);
      u_value v_more = u_field(s2108, 1);
      d2109 = 1;
      {
        p_run_46_evaluate(v_p, v_env, v_e, &v_v, r_cache);
        p_run_46_evaluate_all(v_p, v_env, v_more, &v_rest, r_cache);
        u_value t2111 = u_mk_con(&n_list_46_Cons, 2, v_v, v_rest);
        *r_vs = t2111;
      }
    }
    if (!d2109) {
      u_no_case(s2108);
    }
  }
}

u_value f_run_46_variable_value_4(u_value v_p, u_value v_env, u_value v_n, u_value v_t) {
  u_value s2112 = u_mk_tuple(2, f_map_46_get_2(v_n, v_env), f_syntax_46_Name_46_module_1(v_n));
  u_value t2113 = NULL;
  if ((!t2113) && u_is_con(u_field(s2112, 0), &n_option_46_Some)) {
    u_value v_w = u_field(u_field(s2112, 0), 0);
    t2113 = v_w;
  }
  if ((!t2113) && (u_is_con(u_field(s2112, 0), &n_option_46_None) && u_is_con(u_field(s2112, 1), &n_option_46_None))) {
    u_value t2114;
    if (u_truth(f_run_46_is_builtin_1(v_n))) {
      t2114 = u_mk_con(&n_value_46_Prim, 1, v_n);
    } else {
      u_raise(u_mk_con(&n_run_46_Uninitialized, 1, v_n), "run.RuntimeError");
      t2114 = NULL;
    }
    t2113 = t2114;
  }
  if ((!t2113) && (u_is_con(u_field(s2112, 0), &n_option_46_None) && u_is_con(u_field(s2112, 1), &n_option_46_Some))) {
    u_value s2115 = f_map_46_get_2(u_mk_tuple(2, v_n, f_syntax_46_arity_1(v_t)), f_run_46_Prog_46_funs_1(v_p));
    u_value t2116 = NULL;
    if ((!t2116) && u_is_con(s2115, &n_option_46_Some)) {
      t2116 = u_mk_con(&n_value_46_Fn, 2, v_n, f_syntax_46_arity_1(v_t));
    }
    if ((!t2116) && u_is_con(s2115, &n_option_46_None)) {
      u_value s2117 = f_map_46_get_2(v_n, f_run_46_Prog_46_constrs_1(v_p));
      u_value t2118 = NULL;
      if ((!t2118) && u_is_con(s2117, &n_option_46_Some)) {
        u_value v_k = u_field(s2117, 0);
        u_value t2119;
        if (u_truth(u_eq(v_k, u_mk_int(0)))) {
          t2119 = u_mk_con(&n_value_46_Obj, 2, v_n, &o_list_46_Nil);
        } else {
          t2119 = u_mk_con(&n_value_46_Con, 1, v_n);
        }
        t2118 = t2119;
      }
      if ((!t2118) && u_is_con(s2117, &n_option_46_None)) {
        if (u_truth(f_map_46_has_2(v_n, f_run_46_Prog_46_procs_1(v_p)))) {
          t2118 = u_mk_con(&n_value_46_Proc, 1, v_n);
        }
      }
      if ((!t2118) && u_is_con(s2117, &n_option_46_None)) {
        u_raise(u_mk_con(&n_run_46_Uninitialized, 1, v_n), "run.RuntimeError");
        t2118 = NULL;
      }
      if (!t2118) {
        u_no_case(s2117);
      }
      t2116 = t2118;
    }
    if (!t2116) {
      u_no_case(s2115);
    }
    t2113 = t2116;
  }
  if (!t2113) {
    u_no_case(s2112);
  }
  return t2113;
}

void p_run_46_apply_expr(u_value v_p, u_value v_env, u_value v_f, u_value v_args, u_value *r_v, u_value *r_cache) {
  {
    u_value v_a = NULL;
    u_value v_g = NULL;
    u_value v_vs = NULL;
    u_value s2120 = u_mk_tuple(2, v_f, v_args);
    int d2121 = 0;
    if ((!d2121) && (u_is_con(u_field(s2120, 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(s2120, 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(s2120, 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(s2120, 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(s2120, 0), 0), 0), 1), u_mk_lit("and")) && (u_is_con(u_field(s2120, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2120, 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s2120, 1), 1), 1), &n_list_46_Nil))))))))) {
      u_value v_l = u_field(u_field(s2120, 1), 0);
      u_value v_r = u_field(u_field(u_field(s2120, 1), 1), 0);
      d2121 = 1;
      {
        p_run_46_evaluate(v_p, v_env, v_l, &v_a, r_cache);
        if (u_truth(f_run_46_truth_1(v_a))) {
          {
            p_run_46_evaluate(v_p, v_env, v_r, r_v, r_cache);
          }
        } else {
          {
            u_value t2122 = v_a;
            *r_v = t2122;
          }
        }
      }
    }
    if ((!d2121) && (u_is_con(u_field(s2120, 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(s2120, 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(s2120, 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(s2120, 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(s2120, 0), 0), 0), 1), u_mk_lit("or")) && (u_is_con(u_field(s2120, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2120, 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s2120, 1), 1), 1), &n_list_46_Nil))))))))) {
      u_value v_l = u_field(u_field(s2120, 1), 0);
      u_value v_r = u_field(u_field(u_field(s2120, 1), 1), 0);
      d2121 = 1;
      {
        p_run_46_evaluate(v_p, v_env, v_l, &v_a, r_cache);
        if (u_truth(f_run_46_truth_1(v_a))) {
          {
            u_value t2123 = v_a;
            *r_v = t2123;
          }
        } else {
          {
            p_run_46_evaluate(v_p, v_env, v_r, r_v, r_cache);
          }
        }
      }
    }
    if (!d2121) {
      d2121 = 1;
      {
        p_run_46_evaluate(v_p, v_env, v_f, &v_g, r_cache);
        p_run_46_evaluate_all(v_p, v_env, v_args, &v_vs, r_cache);
        p_run_46_apply_value(v_p, v_g, v_vs, r_v, r_cache);
      }
    }
    if (!d2121) {
      u_no_case(s2120);
    }
  }
}

void p_run_46_apply_value(u_value v_p, u_value v_f, u_value v_vs, u_value *r_v, u_value *r_cache) {
  {
    u_value v_k = NULL;
    u_value s2124 = v_f;
    int d2125 = 0;
    if ((!d2125) && u_is_con(s2124, &n_value_46_Clos)) {
      u_value v_env = u_field(s2124, 0);
      u_value v_xs = u_field(s2124, 1);
      u_value v_body = u_field(s2124, 2);
      d2125 = 1;
      {
        p_run_46_evaluate(v_p, f_run_46_bind_all_3(v_xs, v_vs, v_env), v_body, r_v, r_cache);
      }
    }
    if ((!d2125) && u_is_con(s2124, &n_value_46_Fn)) {
      u_value v_n = u_field(s2124, 0);
      u_value v_k = u_field(s2124, 1);
      d2125 = 1;
      {
        u_value s2126 = f_map_46_get_2(u_mk_tuple(2, v_n, v_k), f_run_46_Prog_46_funs_1(v_p));
        int d2127 = 0;
        if ((!d2127) && (u_is_con(s2126, &n_option_46_Some) && (u_is_con(u_field(u_field(s2126, 0), 0), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2126, 0), 0), 0), &n_list_46_Nil) && u_is_con(u_field(u_field(u_field(s2126, 0), 0), 1), &n_list_46_Nil))))) {
          u_value v_body = u_field(u_field(s2126, 0), 1);
          d2127 = 1;
          {
            p_run_46_cached(v_p, v_n, v_body, r_v, r_cache);
          }
        }
        if ((!d2127) && (u_is_con(s2126, &n_option_46_Some) && (u_is_con(u_field(u_field(s2126, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s2126, 0), 0), 1), &n_list_46_Nil)))) {
          u_value v_xs = u_field(u_field(u_field(s2126, 0), 0), 0);
          u_value v_body = u_field(u_field(s2126, 0), 1);
          d2127 = 1;
          {
            p_run_46_evaluate(v_p, f_run_46_bind_all_3(v_xs, v_vs, f_map_46_empty_0()), v_body, r_v, r_cache);
          }
        }
        if ((!d2127) && (u_is_con(s2126, &n_option_46_Some) && (u_is_con(u_field(u_field(s2126, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s2126, 0), 0), 1), &n_list_46_Cons)))) {
          u_value v_xs = u_field(u_field(u_field(s2126, 0), 0), 0);
          u_value v_ys = u_field(u_field(u_field(u_field(s2126, 0), 0), 1), 0);
          u_value v_rest = u_field(u_field(u_field(u_field(s2126, 0), 0), 1), 1);
          u_value v_body = u_field(u_field(s2126, 0), 1);
          d2127 = 1;
          {
            u_value t2128 = u_mk_con(&n_value_46_Clos, 3, f_run_46_bind_all_3(v_xs, v_vs, f_map_46_empty_0()), v_ys, f_run_46_lambdas_2(v_rest, v_body));
            *r_v = t2128;
          }
        }
        if ((!d2127) && (u_is_con(s2126, &n_option_46_Some) && u_is_con(u_field(u_field(s2126, 0), 0), &n_list_46_Nil))) {
          d2127 = 1;
          {
            u_raise(u_mk_con(&n_run_46_Uninitialized, 1, v_n), "run.RuntimeError");
          }
        }
        if ((!d2127) && u_is_con(s2126, &n_option_46_None)) {
          d2127 = 1;
          {
            u_raise(u_mk_con(&n_run_46_Uninitialized, 1, v_n), "run.RuntimeError");
          }
        }
        if (!d2127) {
          u_no_case(s2126);
        }
      }
    }
    if ((!d2125) && u_is_con(s2124, &n_value_46_Con)) {
      u_value v_n = u_field(s2124, 0);
      d2125 = 1;
      {
        u_value t2129 = u_mk_con(&n_value_46_Obj, 2, v_n, v_vs);
        *r_v = t2129;
      }
    }
    if ((!d2125) && u_is_con(s2124, &n_value_46_Prim)) {
      u_value v_n = u_field(s2124, 0);
      d2125 = 1;
      {
        u_value s2130 = u_mk_tuple(2, f_syntax_46_Name_46_base_1(v_n), v_vs);
        int d2131 = 0;
        if ((!d2131) && (u_equals(u_field(s2130, 0), u_mk_lit("compare")) && (u_is_con(u_field(s2130, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2130, 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s2130, 1), 1), 1), &n_list_46_Nil))))) {
          u_value v_a = u_field(u_field(s2130, 1), 0);
          u_value v_b = u_field(u_field(u_field(s2130, 1), 1), 0);
          d2131 = 1;
          {
            p_run_46_compare_values(v_p, v_a, v_b, &v_k);
            u_value t2132 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
            *r_v = t2132;
          }
        }
        if (!d2131) {
          d2131 = 1;
          {
            p_run_46_primitive(v_n, v_vs, r_v);
          }
        }
        if (!d2131) {
          u_no_case(s2130);
        }
      }
    }
    if (!d2125) {
      d2125 = 1;
      {
        u_raise(u_mk_con(&n_run_46_NotFunction, 1, v_f), "run.RuntimeError");
      }
    }
    if (!d2125) {
      u_no_case(s2124);
    }
  }
}

void p_run_46_cached(u_value v_p, u_value v_n, u_value v_body, u_value *r_v, u_value *r_cache) {
  {
    u_value s2133 = f_map_46_get_2(v_n, *r_cache);
    int d2134 = 0;
    if ((!d2134) && u_is_con(s2133, &n_option_46_Some)) {
      u_value v_w = u_field(s2133, 0);
      d2134 = 1;
      {
        u_value t2135 = v_w;
        *r_v = t2135;
      }
    }
    if ((!d2134) && u_is_con(s2133, &n_option_46_None)) {
      if (u_truth(f_list_46_contains_2(v_n, f_run_46_Prog_46_computing_1(v_p)))) {
        d2134 = 1;
        {
          u_raise(u_mk_con(&n_run_46_RecursiveInitialization, 1, v_n), "run.RuntimeError");
        }
      }
    }
    if ((!d2134) && u_is_con(s2133, &n_option_46_None)) {
      d2134 = 1;
      {
        p_run_46_evaluate(f_run_46_Prog_46_computing_58__61__2(v_p, u_mk_con(&n_list_46_Cons, 2, v_n, f_run_46_Prog_46_computing_1(v_p))), f_map_46_empty_0(), v_body, r_v, r_cache);
        u_value t2136 = f_map_46_put_3(v_n, *r_v, *r_cache);
        *r_cache = t2136;
      }
    }
    if (!d2134) {
      u_no_case(s2133);
    }
  }
}

u_value f_run_46_failure_2(u_value v_n, u_value v_args) {
  return u_mk_con(&n_run_46_Thrown, 2, u_mk_con(&n_value_46_Obj, 2, v_n, v_args), u_mk_con(&n_syntax_46_TSort, 2, v_n, &o_list_46_Nil));
}

void p_run_46_primitive(u_value v_n, u_value v_vs, u_value *r_v) {
  {
    u_value v_r = NULL;
    u_value s2137 = u_mk_tuple(2, f_syntax_46_Name_46_base_1(v_n), v_vs);
    int d2138 = 0;
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("+")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VInt) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2137, 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), &n_syntax_46_VInt) && u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Nil))))))))) {
      u_value v_a = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      u_value v_b = u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2139 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_add(v_a, v_b)));
        *r_v = t2139;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("-")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VInt) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2137, 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), &n_syntax_46_VInt) && u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Nil))))))))) {
      u_value v_a = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      u_value v_b = u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2140 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(v_a, v_b)));
        *r_v = t2140;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("*")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VInt) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2137, 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), &n_syntax_46_VInt) && u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Nil))))))))) {
      u_value v_a = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      u_value v_b = u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2141 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_mul(v_a, v_b)));
        *r_v = t2141;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("/")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VInt) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2137, 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), &n_syntax_46_VInt) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), 0), u_mk_int(0)) && u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Nil)))))))))) {
      d2138 = 1;
      {
        u_raise(f_run_46_failure_2(f_syntax_46_division_by_zero_name_0(), &o_list_46_Nil), "run.Thrown");
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("/")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VInt) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2137, 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), &n_syntax_46_VInt) && u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Nil))))))))) {
      u_value v_a = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      u_value v_b = u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2142 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_div(v_a, v_b)));
        *r_v = t2142;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("%")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VInt) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2137, 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), &n_syntax_46_VInt) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), 0), u_mk_int(0)) && u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Nil)))))))))) {
      d2138 = 1;
      {
        u_raise(f_run_46_failure_2(f_syntax_46_division_by_zero_name_0(), &o_list_46_Nil), "run.Thrown");
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("%")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VInt) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2137, 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), &n_syntax_46_VInt) && u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Nil))))))))) {
      u_value v_a = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      u_value v_b = u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2143 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_mod(v_a, v_b)));
        *r_v = t2143;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("<")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VInt) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2137, 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), &n_syntax_46_VInt) && u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Nil))))))))) {
      u_value v_a = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      u_value v_b = u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2144 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_lt(v_a, v_b)));
        *r_v = t2144;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("==")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Nil))))) {
      u_value v_a = u_field(u_field(s2137, 1), 0);
      u_value v_b = u_field(u_field(u_field(s2137, 1), 1), 0);
      d2138 = 1;
      {
        p_run_46_equal(v_a, v_b, &v_r);
        u_value t2145 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VBool, 1, v_r));
        *r_v = t2145;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("chars")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VStr) && u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Nil)))))) {
      u_value v_s = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2146 = &o_list_46_Nil;
        u_value *e2147 = &t2146;
        u_value l2148 = u_chars(v_s);
        while (u_is_con(l2148, &n_list_46_Cons)) {
          u_value v_c = u_field(l2148, 0);
          if (u_truth(u_mk_bool(1))) {
            e2147 = u_append(e2147, u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c)));
          }
          l2148 = u_field(l2148, 1);
        }
        u_value t2149 = f_value_46_list_value_1(t2146);
        *r_v = t2149;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("string")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Nil)))) {
      u_value v_w = u_field(u_field(s2137, 1), 0);
      d2138 = 1;
      {
        u_value s2150 = f_value_46_list_elements_1(v_w);
        int d2151 = 0;
        if ((!d2151) && u_is_con(s2150, &n_option_46_Some)) {
          u_value v_ws = u_field(s2150, 0);
          d2151 = 1;
          {
            u_value t2152 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_string(f_run_46_char_values_1(v_ws))));
            *r_v = t2152;
          }
        }
        if (!d2151) {
          d2151 = 1;
          {
            u_raise(u_mk_con(&n_run_46_NotList, 1, v_w), "run.RuntimeError");
          }
        }
        if (!d2151) {
          u_no_case(s2150);
        }
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("ord")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VChar) && u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Nil)))))) {
      u_value v_c = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2153 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_ord(v_c)));
        *r_v = t2153;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("chr")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VInt) && u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Nil)))))) {
      u_value v_k = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2154 = u_lt(v_k, u_mk_int(0));
        if (!u_truth(t2154)) {
          t2154 = f_prelude_46__62__2(v_k, u_mk_int(65535));
        }
        if (u_truth(t2154)) {
          {
            u_raise(f_run_46_failure_2(f_syntax_46_no_character_name_0(), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k)), &o_list_46_Nil)), "run.Thrown");
          }
        }
        u_value t2155 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VChar, 1, u_chr(v_k)));
        *r_v = t2155;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("^")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VStr) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2137, 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), &n_syntax_46_VStr) && u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Nil))))))))) {
      u_value v_s = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      u_value v_t = u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2156 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_s, v_t)));
        *r_v = t2156;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("str_size")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VStr) && u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Nil)))))) {
      u_value v_s = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2157 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_str_size(v_s)));
        *r_v = t2157;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("str_join")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VStr) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Nil))))))) {
      u_value v_sep = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      u_value v_w = u_field(u_field(u_field(s2137, 1), 1), 0);
      d2138 = 1;
      {
        u_value s2158 = f_value_46_list_elements_1(v_w);
        int d2159 = 0;
        if ((!d2159) && u_is_con(s2158, &n_option_46_Some)) {
          u_value v_ws = u_field(s2158, 0);
          d2159 = 1;
          {
            u_value t2160 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_str_join(v_sep, f_run_46_str_values_1(v_ws))));
            *r_v = t2160;
          }
        }
        if (!d2159) {
          d2159 = 1;
          {
            u_raise(u_mk_con(&n_run_46_NotList, 1, v_w), "run.RuntimeError");
          }
        }
        if (!d2159) {
          u_no_case(s2158);
        }
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("str_slice")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VStr) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2137, 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), &n_syntax_46_VInt) && (u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 1), 0), 0), &n_syntax_46_VInt) && u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 1), 1), &n_list_46_Nil)))))))))))) {
      u_value v_s = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      u_value v_i = u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), 0);
      u_value v_j = u_field(u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2161 = u_lt(v_i, u_mk_int(0));
        if (!u_truth(t2161)) {
          t2161 = f_prelude_46__62__2(v_i, u_str_size(v_s));
        }
        u_value t2162 = t2161;
        if (!u_truth(t2162)) {
          t2162 = f_prelude_46__62__2(v_i, v_j);
        }
        if (u_truth(t2162)) {
          {
            u_raise(f_run_46_failure_2(f_syntax_46_no_index_name_0(), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_i)), &o_list_46_Nil))), "run.Thrown");
          }
        } else {
          if (u_truth(f_prelude_46__62__2(v_j, u_str_size(v_s)))) {
            {
              u_raise(f_run_46_failure_2(f_syntax_46_no_index_name_0(), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_j)), &o_list_46_Nil))), "run.Thrown");
            }
          }
        }
        u_value t2163 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_str_slice(v_s, v_i, v_j)));
        *r_v = t2163;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("str_split")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VStr) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2137, 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), &n_syntax_46_VChar) && u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Nil))))))))) {
      u_value v_s = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      u_value v_c = u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2164 = &o_list_46_Nil;
        u_value *e2165 = &t2164;
        u_value l2166 = u_str_split(v_s, v_c);
        while (u_is_con(l2166, &n_list_46_Cons)) {
          u_value v_part = u_field(l2166, 0);
          if (u_truth(u_mk_bool(1))) {
            e2165 = u_append(e2165, u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_part)));
          }
          l2166 = u_field(l2166, 1);
        }
        u_value t2167 = f_value_46_list_value_1(t2164);
        *r_v = t2167;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("str_get")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VStr) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2137, 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), &n_syntax_46_VInt) && u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Nil))))))))) {
      u_value v_s = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      u_value v_i = u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2168 = u_lt(v_i, u_mk_int(0));
        if (!u_truth(t2168)) {
          t2168 = f_prelude_46__62__61__2(v_i, u_str_size(v_s));
        }
        if (u_truth(t2168)) {
          {
            u_raise(f_run_46_failure_2(f_syntax_46_no_index_name_0(), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_i)), &o_list_46_Nil))), "run.Thrown");
          }
        }
        u_value t2169 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VChar, 1, u_str_get(v_s, v_i)));
        *r_v = t2169;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("str_contains")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VStr) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2137, 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), &n_syntax_46_VChar) && u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Nil))))))))) {
      u_value v_s = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      u_value v_c = u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2170 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_str_contains(v_s, v_c)));
        *r_v = t2170;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("str_compare")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VStr) && (u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2137, 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), &n_syntax_46_VStr) && u_is_con(u_field(u_field(u_field(s2137, 1), 1), 1), &n_list_46_Nil))))))))) {
      u_value v_s = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      u_value v_t = u_field(u_field(u_field(u_field(u_field(s2137, 1), 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2171 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_str_compare(v_s, v_t)));
        *r_v = t2171;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("str_show")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VInt) && u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Nil)))))) {
      u_value v_k = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2172 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_str_show(v_k)));
        *r_v = t2172;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("str_quote")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VStr) && u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Nil)))))) {
      u_value v_s = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2173 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_str_quote(v_s)));
        *r_v = t2173;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("str_number")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VStr) && u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Nil)))))) {
      u_value v_s = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value s2174 = u_str_number(v_s);
        int d2175 = 0;
        if ((!d2175) && u_is_con(s2174, &n_option_46_Some)) {
          u_value v_k = u_field(s2174, 0);
          d2175 = 1;
          {
            u_value t2176 = u_mk_con(&n_value_46_Obj, 2, f_syntax_46_some_name_0(), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k)), &o_list_46_Nil));
            *r_v = t2176;
          }
        }
        if ((!d2175) && u_is_con(s2174, &n_option_46_None)) {
          d2175 = 1;
          {
            u_value t2177 = u_mk_con(&n_value_46_Obj, 2, f_syntax_46_none_name_0(), &o_list_46_Nil);
            *r_v = t2177;
          }
        }
        if (!d2175) {
          u_no_case(s2174);
        }
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("is_digit")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VChar) && u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Nil)))))) {
      u_value v_c = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2178 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_is_digit(v_c)));
        *r_v = t2178;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("is_letter")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VChar) && u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Nil)))))) {
      u_value v_c = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2179 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_is_letter(v_c)));
        *r_v = t2179;
      }
    }
    if ((!d2138) && (u_equals(u_field(s2137, 0), u_mk_lit("is_space")) && (u_is_con(u_field(s2137, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2137, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2137, 1), 0), 0), &n_syntax_46_VChar) && u_is_con(u_field(u_field(s2137, 1), 1), &n_list_46_Nil)))))) {
      u_value v_c = u_field(u_field(u_field(u_field(s2137, 1), 0), 0), 0);
      d2138 = 1;
      {
        u_value t2180 = u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_is_space(v_c)));
        *r_v = t2180;
      }
    }
    if (!d2138) {
      d2138 = 1;
      {
        u_raise(u_mk_con(&n_run_46_Unspecified, 2, v_n, v_vs), "run.RuntimeError");
      }
    }
    if (!d2138) {
      u_no_case(s2137);
    }
  }
}

void p_run_46_equal(u_value v_a, u_value v_b, u_value *r_r) {
  {
    u_value t2181 = f_run_46_is_function_1(v_a);
    if (!u_truth(t2181)) {
      t2181 = f_run_46_is_function_1(v_b);
    }
    if (u_truth(t2181)) {
      {
        u_raise(&o_run_46_CompareFunctions, "run.RuntimeError");
      }
    }
    u_value s2182 = u_mk_tuple(2, v_a, v_b);
    int d2183 = 0;
    if ((!d2183) && (u_is_con(u_field(s2182, 0), &n_value_46_Obj) && u_is_con(u_field(s2182, 1), &n_value_46_Obj))) {
      u_value v_c = u_field(u_field(s2182, 0), 0);
      u_value v_xs = u_field(u_field(s2182, 0), 1);
      u_value v_d = u_field(u_field(s2182, 1), 0);
      u_value v_ys = u_field(u_field(s2182, 1), 1);
      d2183 = 1;
      {
        u_value t2184 = u_eq(v_c, v_d);
        if (u_truth(t2184)) {
          t2184 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys));
        }
        if (u_truth(t2184)) {
          {
            p_run_46_equal_all(v_xs, v_ys, r_r);
          }
        } else {
          {
            u_value t2185 = u_mk_bool(0);
            *r_r = t2185;
          }
        }
      }
    }
    if (!d2183) {
      d2183 = 1;
      {
        u_value t2186 = u_eq(v_a, v_b);
        *r_r = t2186;
      }
    }
    if (!d2183) {
      u_no_case(s2182);
    }
  }
}

void p_run_46_compare_values(u_value v_p, u_value v_a, u_value v_b, u_value *r_r) {
  {
    u_value t2187 = f_run_46_is_function_1(v_a);
    if (!u_truth(t2187)) {
      t2187 = f_run_46_is_function_1(v_b);
    }
    if (u_truth(t2187)) {
      {
        u_raise(&o_run_46_CompareFunctions, "run.RuntimeError");
      }
    }
    u_value s2188 = u_mk_tuple(2, v_a, v_b);
    int d2189 = 0;
    if ((!d2189) && (u_is_con(u_field(s2188, 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(s2188, 0), 0), &n_syntax_46_VInt) && (u_is_con(u_field(s2188, 1), &n_value_46_Lit) && u_is_con(u_field(u_field(s2188, 1), 0), &n_syntax_46_VInt))))) {
      u_value v_x = u_field(u_field(u_field(s2188, 0), 0), 0);
      u_value v_y = u_field(u_field(u_field(s2188, 1), 0), 0);
      d2189 = 1;
      {
        u_value t2190 = f_run_46_order_2(v_x, v_y);
        *r_r = t2190;
      }
    }
    if ((!d2189) && (u_is_con(u_field(s2188, 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(s2188, 0), 0), &n_syntax_46_VChar) && (u_is_con(u_field(s2188, 1), &n_value_46_Lit) && u_is_con(u_field(u_field(s2188, 1), 0), &n_syntax_46_VChar))))) {
      u_value v_x = u_field(u_field(u_field(s2188, 0), 0), 0);
      u_value v_y = u_field(u_field(u_field(s2188, 1), 0), 0);
      d2189 = 1;
      {
        u_value t2191 = f_run_46_order_2(u_ord(v_x), u_ord(v_y));
        *r_r = t2191;
      }
    }
    if ((!d2189) && (u_is_con(u_field(s2188, 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(s2188, 0), 0), &n_syntax_46_VBool) && (u_is_con(u_field(s2188, 1), &n_value_46_Lit) && u_is_con(u_field(u_field(s2188, 1), 0), &n_syntax_46_VBool))))) {
      u_value v_x = u_field(u_field(u_field(s2188, 0), 0), 0);
      u_value v_y = u_field(u_field(u_field(s2188, 1), 0), 0);
      d2189 = 1;
      {
        u_value t2192;
        if (u_truth(v_x)) {
          t2192 = u_mk_int(1);
        } else {
          t2192 = u_mk_int(0);
        }
        u_value t2193;
        if (u_truth(v_y)) {
          t2193 = u_mk_int(1);
        } else {
          t2193 = u_mk_int(0);
        }
        u_value t2194 = f_run_46_order_2(t2192, t2193);
        *r_r = t2194;
      }
    }
    if ((!d2189) && (u_is_con(u_field(s2188, 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(s2188, 0), 0), &n_syntax_46_VStr) && (u_is_con(u_field(s2188, 1), &n_value_46_Lit) && u_is_con(u_field(u_field(s2188, 1), 0), &n_syntax_46_VStr))))) {
      u_value v_x = u_field(u_field(u_field(s2188, 0), 0), 0);
      u_value v_y = u_field(u_field(u_field(s2188, 1), 0), 0);
      d2189 = 1;
      {
        u_value t2195 = u_str_compare(v_x, v_y);
        *r_r = t2195;
      }
    }
    if ((!d2189) && (u_is_con(u_field(s2188, 0), &n_value_46_Obj) && u_is_con(u_field(s2188, 1), &n_value_46_Obj))) {
      u_value v_c = u_field(u_field(s2188, 0), 0);
      u_value v_xs = u_field(u_field(s2188, 0), 1);
      u_value v_d = u_field(u_field(s2188, 1), 0);
      u_value v_ys = u_field(u_field(s2188, 1), 1);
      d2189 = 1;
      {
        if (u_truth(u_eq(v_c, v_d))) {
          {
            p_run_46_compare_all(v_p, v_xs, v_ys, r_r);
          }
        } else {
          {
            u_value t2196 = f_run_46_order_2(f_run_46_position_of_2(v_p, v_c), f_run_46_position_of_2(v_p, v_d));
            *r_r = t2196;
          }
        }
      }
    }
    if (!d2189) {
      d2189 = 1;
      {
        u_value t2197 = u_mk_int(0);
        *r_r = t2197;
      }
    }
    if (!d2189) {
      u_no_case(s2188);
    }
  }
}

void p_run_46_compare_all(u_value v_p, u_value v_xs, u_value v_ys, u_value *r_r) {
  {
    u_value s2198 = u_mk_tuple(2, v_xs, v_ys);
    int d2199 = 0;
    if ((!d2199) && (u_is_con(u_field(s2198, 0), &n_list_46_Cons) && u_is_con(u_field(s2198, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2198, 0), 0);
      u_value v_xs1 = u_field(u_field(s2198, 0), 1);
      u_value v_y = u_field(u_field(s2198, 1), 0);
      u_value v_ys1 = u_field(u_field(s2198, 1), 1);
      d2199 = 1;
      {
        p_run_46_compare_values(v_p, v_x, v_y, r_r);
        if (u_truth(u_eq(*r_r, u_mk_int(0)))) {
          {
            p_run_46_compare_all(v_p, v_xs1, v_ys1, r_r);
          }
        }
      }
    }
    if (!d2199) {
      d2199 = 1;
      {
        u_value t2200 = f_run_46_order_2(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys));
        *r_r = t2200;
      }
    }
    if (!d2199) {
      u_no_case(s2198);
    }
  }
}

u_value f_run_46_order_2(u_value v_x, u_value v_y) {
  u_value t2202;
  if (u_truth(u_lt(v_x, v_y))) {
    t2202 = u_mk_int(-1);
  } else {
    u_value t2201;
    if (u_truth(u_lt(v_y, v_x))) {
      t2201 = u_mk_int(1);
    } else {
      t2201 = u_mk_int(0);
    }
    t2202 = t2201;
  }
  return t2202;
}

void p_run_46_equal_all(u_value v_xs, u_value v_ys, u_value *r_r) {
  {
    u_value s2203 = u_mk_tuple(2, v_xs, v_ys);
    int d2204 = 0;
    if ((!d2204) && (u_is_con(u_field(s2203, 0), &n_list_46_Cons) && u_is_con(u_field(s2203, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2203, 0), 0);
      u_value v_xs1 = u_field(u_field(s2203, 0), 1);
      u_value v_y = u_field(u_field(s2203, 1), 0);
      u_value v_ys1 = u_field(u_field(s2203, 1), 1);
      d2204 = 1;
      {
        p_run_46_equal(v_x, v_y, r_r);
        if (u_truth(*r_r)) {
          {
            p_run_46_equal_all(v_xs1, v_ys1, r_r);
          }
        }
      }
    }
    if (!d2204) {
      d2204 = 1;
      {
        u_value t2205 = u_mk_bool(1);
        *r_r = t2205;
      }
    }
    if (!d2204) {
      u_no_case(s2203);
    }
  }
}

void p_run_46_combinations(u_value v_p, u_value v_env, u_value v_xs, u_value v_lists, u_value *r_envs, u_value *r_cache) {
  {
    u_value v_l = NULL;
    u_value s2206 = u_mk_tuple(2, v_xs, v_lists);
    int d2207 = 0;
    if ((!d2207) && (u_is_con(u_field(s2206, 0), &n_list_46_Cons) && u_is_con(u_field(s2206, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2206, 0), 0);
      u_value v_ys = u_field(u_field(s2206, 0), 1);
      u_value v_e = u_field(u_field(s2206, 1), 0);
      u_value v_more = u_field(u_field(s2206, 1), 1);
      d2207 = 1;
      {
        p_run_46_evaluate(v_p, v_env, v_e, &v_l, r_cache);
        u_value s2208 = f_value_46_list_elements_1(v_l);
        int d2209 = 0;
        if ((!d2209) && u_is_con(s2208, &n_option_46_Some)) {
          u_value v_vs = u_field(s2208, 0);
          d2209 = 1;
          {
            p_run_46_extend(v_p, v_env, v_x, v_vs, v_ys, v_more, r_envs, r_cache);
          }
        }
        if ((!d2209) && u_is_con(s2208, &n_option_46_None)) {
          d2209 = 1;
          {
            u_raise(u_mk_con(&n_run_46_NotList, 1, v_l), "run.RuntimeError");
          }
        }
        if (!d2209) {
          u_no_case(s2208);
        }
      }
    }
    if (!d2207) {
      d2207 = 1;
      {
        u_value t2210 = u_mk_con(&n_list_46_Cons, 2, v_env, &o_list_46_Nil);
        *r_envs = t2210;
      }
    }
    if (!d2207) {
      u_no_case(s2206);
    }
  }
}

void p_run_46_extend(u_value v_p, u_value v_env, u_value v_x, u_value v_vs, u_value v_ys, u_value v_lists, u_value *r_envs, u_value *r_cache) {
  {
    u_value v_first = NULL;
    u_value v_rest = NULL;
    u_value s2211 = v_vs;
    int d2212 = 0;
    if ((!d2212) && u_is_con(s2211, &n_list_46_Nil)) {
      d2212 = 1;
      {
        u_value t2213 = &o_list_46_Nil;
        *r_envs = t2213;
      }
    }
    if ((!d2212) && u_is_con(s2211, &n_list_46_Cons)) {
      u_value v_v = u_field(s2211, 0);
      u_value v_more = u_field(s2211, 1);
      d2212 = 1;
      {
        p_run_46_combinations(v_p, f_map_46_put_3(f_syntax_46_Var_46_name_1(v_x), v_v, v_env), v_ys, v_lists, &v_first, r_cache);
        p_run_46_extend(v_p, v_env, v_x, v_more, v_ys, v_lists, &v_rest, r_cache);
        u_value t2214 = f_list_46__43__43__2(v_first, v_rest);
        *r_envs = t2214;
      }
    }
    if (!d2212) {
      u_no_case(s2211);
    }
  }
}

void p_run_46_comprehension(u_value v_p, u_value v_envs, u_value v_result, u_value v_cond, u_value *r_vs, u_value *r_cache) {
  {
    u_value v_b = NULL;
    u_value v_r = NULL;
    u_value v_rest = NULL;
    u_value s2215 = v_envs;
    int d2216 = 0;
    if ((!d2216) && u_is_con(s2215, &n_list_46_Nil)) {
      d2216 = 1;
      {
        u_value t2217 = &o_list_46_Nil;
        *r_vs = t2217;
      }
    }
    if ((!d2216) && u_is_con(s2215, &n_list_46_Cons)) {
      u_value v_env = u_field(s2215, 0);
      u_value v_more = u_field(s2215, 1);
      d2216 = 1;
      {
        p_run_46_evaluate(v_p, v_env, v_cond, &v_b, r_cache);
        if (u_truth(f_run_46_truth_1(v_b))) {
          {
            p_run_46_evaluate(v_p, v_env, v_result, &v_r, r_cache);
            p_run_46_comprehension(v_p, v_more, v_result, v_cond, &v_rest, r_cache);
            u_value t2218 = u_mk_con(&n_list_46_Cons, 2, v_r, v_rest);
            *r_vs = t2218;
          }
        } else {
          {
            p_run_46_comprehension(v_p, v_more, v_result, v_cond, r_vs, r_cache);
          }
        }
      }
    }
    if (!d2216) {
      u_no_case(s2215);
    }
  }
}

void p_run_46_exec(u_value v_p, u_value v_x, u_value *r_env, u_value *r_cache) {
  {
    u_value v_v = NULL;
    u_value v_w = NULL;
    u_value v_vs = NULL;
    u_value v_r = NULL;
    u_value v_sel = NULL;
    u_value s2219 = v_x;
    int d2220 = 0;
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_SSkip)) {
      d2220 = 1;
      {
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2219, 0);
      d2220 = 1;
      {
        p_run_46_evaluate_all(v_p, *r_env, f_list_46_seconds_1(v_pairs), &v_vs, r_cache);
        u_value t2221 = f_map_46_put_all_2(f_list_46_zip_2(f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_vs), *r_env);
        *r_env = t2221;
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_SBlock)) {
      u_value v_locals = u_field(s2219, 0);
      u_value v_body = u_field(s2219, 1);
      d2220 = 1;
      {
        p_run_46_scoped(v_p, f_syntax_46_names_1(v_locals), f_map_46_delete_all_2(f_syntax_46_names_1(v_locals), *r_env), v_body, r_env, r_cache);
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_SIf)) {
      u_value v_cond = u_field(s2219, 0);
      u_value v_left = u_field(s2219, 1);
      u_value v_right = u_field(s2219, 2);
      d2220 = 1;
      {
        p_run_46_evaluate(v_p, *r_env, v_cond, &v_v, r_cache);
        if (u_truth(f_run_46_truth_1(v_v))) {
          {
            p_run_46_exec(v_p, v_left, r_env, r_cache);
          }
        } else {
          {
            p_run_46_exec(v_p, v_right, r_env, r_cache);
          }
        }
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_SWhile)) {
      u_value v_cond = u_field(s2219, 0);
      u_value v_body = u_field(s2219, 1);
      d2220 = 1;
      {
        p_run_46_evaluate(v_p, *r_env, v_cond, &v_v, r_cache);
        u_value t2222 = f_run_46_truth_1(v_v);
        while (u_truth(t2222)) {
          {
            p_run_46_exec(v_p, v_body, r_env, r_cache);
            p_run_46_evaluate(v_p, *r_env, v_cond, &v_v, r_cache);
          }
          t2222 = f_run_46_truth_1(v_v);
        }
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_SCall)) {
      u_value v_n = u_field(s2219, 0);
      u_value v_ins = u_field(s2219, 1);
      u_value v_outs = u_field(s2219, 2);
      u_value v_refs = u_field(s2219, 3);
      d2220 = 1;
      {
        p_run_46_evaluate_all(v_p, *r_env, v_ins, &v_vs, r_cache);
        u_value s2223 = f_map_46_get_2(v_n, *r_env);
        int d2224 = 0;
        if ((!d2224) && u_is_con(s2223, &n_option_46_Some)) {
          u_value v_f = u_field(s2223, 0);
          d2224 = 1;
          {
            p_run_46_invoke_value(v_p, v_f, v_vs, f_syntax_46_names_1(v_refs), &v_vs, r_env, r_cache);
          }
        }
        if ((!d2224) && u_is_con(s2223, &n_option_46_None)) {
          d2224 = 1;
          {
            p_run_46_invoke(v_p, v_n, v_vs, f_syntax_46_names_1(v_refs), &v_vs, r_env, r_cache);
          }
        }
        if (!d2224) {
          u_no_case(s2223);
        }
        u_value t2225 = f_map_46_put_all_2(f_list_46_zip_2(f_syntax_46_names_1(v_outs), v_vs), *r_env);
        *r_env = t2225;
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2219, 0);
      u_value v_cases = u_field(s2219, 1);
      d2220 = 1;
      {
        p_run_46_evaluate(v_p, *r_env, v_arg, &v_v, r_cache);
        p_run_46_select(v_p, *r_env, v_v, v_cases, &v_sel, r_cache);
        u_value s2226 = v_sel;
        int d2227 = 0;
        if ((!d2227) && u_is_con(s2226, &n_option_46_Some)) {
          u_value v_body = u_field(u_field(s2226, 0), 0);
          u_value v_inner = u_field(u_field(s2226, 0), 1);
          u_value v_ns = u_field(u_field(s2226, 0), 2);
          d2227 = 1;
          {
            p_run_46_scoped(v_p, v_ns, v_inner, u_mk_con(&n_list_46_Cons, 2, v_body, &o_list_46_Nil), r_env, r_cache);
          }
        }
        if ((!d2227) && u_is_con(s2226, &n_option_46_None)) {
          d2227 = 1;
          {
            u_raise(u_mk_con(&n_run_46_NoCase, 1, v_v), "run.RuntimeError");
          }
        }
        if (!d2227) {
          u_no_case(s2226);
        }
      }
    }
    if ((!d2220) && (u_is_con(s2219, &n_syntax_46_SAssert) && (u_is_con(u_field(s2219, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2219, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2219, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2219, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2219, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2219, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2219, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2219, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2219, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_lhs = u_field(u_field(u_field(s2219, 0), 1), 0);
      u_value v_rhs = u_field(u_field(u_field(u_field(s2219, 0), 1), 1), 0);
      d2220 = 1;
      {
        p_run_46_evaluate(v_p, *r_env, v_lhs, &v_v, r_cache);
        p_run_46_evaluate(v_p, *r_env, v_rhs, &v_w, r_cache);
        p_run_46_equal(v_v, v_w, &v_r);
        if (u_truth(f_prelude_46_not_1(v_r))) {
          {
            u_raise(u_mk_con(&n_run_46_AssertionUnequal, 2, v_v, v_w), "run.RuntimeError");
          }
        }
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_SAssert)) {
      u_value v_cond = u_field(s2219, 0);
      d2220 = 1;
      {
        p_run_46_evaluate(v_p, *r_env, v_cond, &v_v, r_cache);
        if (u_truth(f_prelude_46_not_1(f_run_46_truth_1(v_v)))) {
          {
            u_raise(&o_run_46_AssertionFailed, "run.RuntimeError");
          }
        }
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2219, 0);
      u_value v_t = u_field(s2219, 1);
      d2220 = 1;
      {
        p_run_46_evaluate(v_p, *r_env, v_arg, &v_v, r_cache);
        u_raise(u_mk_con(&n_run_46_Thrown, 2, v_v, v_t), "run.Thrown");
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2219, 0);
      u_value v_handlers = u_field(s2219, 1);
      d2220 = 1;
      {
        {
          struct u_handler *h2228 = u_push();
          if (setjmp(h2228->env) == 0) {
            {
              p_run_46_exec(v_p, v_body, r_env, r_cache);
            }
            u_pop(h2228);
          } else {
            u_pop(h2228);
            int d2229 = 0;
            if ((!d2229) && (u_has_type(h2228, "run.Thrown") && u_is_con(u_caught(h2228), &n_run_46_Thrown))) {
              u_value v_u = u_field(u_caught(h2228), 0);
              u_value v_t = u_field(u_caught(h2228), 1);
              d2229 = 1;
              {
                p_run_46_handle(v_p, v_handlers, v_u, v_t, r_env, r_cache);
              }
            }
            if (!d2229) {
              u_reraise(h2228);
            }
          }
        }
      }
    }
    if (!d2220) {
      u_no_case(s2219);
    }
  }
}

void p_run_46_exec_all(u_value v_p, u_value v_xs, u_value *r_env, u_value *r_cache) {
  {
    u_value s2230 = v_xs;
    int d2231 = 0;
    if ((!d2231) && u_is_con(s2230, &n_list_46_Nil)) {
      d2231 = 1;
      {
      }
    }
    if ((!d2231) && u_is_con(s2230, &n_list_46_Cons)) {
      u_value v_x = u_field(s2230, 0);
      u_value v_more = u_field(s2230, 1);
      d2231 = 1;
      {
        p_run_46_exec(v_p, v_x, r_env, r_cache);
        p_run_46_exec_all(v_p, v_more, r_env, r_cache);
      }
    }
    if (!d2231) {
      u_no_case(s2230);
    }
  }
}

void p_run_46_scoped(u_value v_p, u_value v_names, u_value v_inner, u_value v_xs, u_value *r_env, u_value *r_cache) {
  {
    u_value v_outer = NULL;
    u_value t2232 = *r_env;
    v_outer = t2232;
    u_value t2233 = v_inner;
    *r_env = t2233;
    {
      struct u_handler *h2234 = u_push();
      if (setjmp(h2234->env) == 0) {
        {
          p_run_46_exec_all(v_p, v_xs, r_env, r_cache);
        }
        u_pop(h2234);
      } else {
        u_pop(h2234);
        int d2235 = 0;
        if ((!d2235) && (u_has_type(h2234, "run.Thrown") && u_is_con(u_caught(h2234), &n_run_46_Thrown))) {
          u_value v_v = u_field(u_caught(h2234), 0);
          u_value v_t = u_field(u_caught(h2234), 1);
          d2235 = 1;
          {
            u_value t2236 = f_run_46_restored_3(v_names, v_outer, *r_env);
            *r_env = t2236;
            u_raise(u_mk_con(&n_run_46_Thrown, 2, v_v, v_t), "run.Thrown");
          }
        }
        if (!d2235) {
          u_reraise(h2234);
        }
      }
    }
    u_value t2237 = f_run_46_restored_3(v_names, v_outer, *r_env);
    *r_env = t2237;
  }
}

u_value f_run_46_restored_3(u_value v_names, u_value v_outer, u_value v_env) {
  u_value s2238 = v_names;
  u_value t2239 = NULL;
  if ((!t2239) && u_is_con(s2238, &n_list_46_Nil)) {
    t2239 = v_env;
  }
  if ((!t2239) && u_is_con(s2238, &n_list_46_Cons)) {
    u_value v_n = u_field(s2238, 0);
    u_value v_rest = u_field(s2238, 1);
    u_value s2240 = f_map_46_get_2(v_n, v_outer);
    u_value t2241 = NULL;
    if ((!t2241) && u_is_con(s2240, &n_option_46_Some)) {
      u_value v_v = u_field(s2240, 0);
      t2241 = f_run_46_restored_3(v_rest, v_outer, f_map_46_put_3(v_n, v_v, v_env));
    }
    if ((!t2241) && u_is_con(s2240, &n_option_46_None)) {
      t2241 = f_run_46_restored_3(v_rest, v_outer, f_map_46_delete_2(v_n, v_env));
    }
    if (!t2241) {
      u_no_case(s2240);
    }
    t2239 = t2241;
  }
  if (!t2239) {
    u_no_case(s2238);
  }
  return t2239;
}

void p_run_46_select(u_value v_p, u_value v_env, u_value v_v, u_value v_cases, u_value *r_sel, u_value *r_cache) {
  {
    u_value v_ok = NULL;
    u_value s2242 = v_cases;
    int d2243 = 0;
    if ((!d2243) && u_is_con(s2242, &n_list_46_Nil)) {
      d2243 = 1;
      {
        u_value t2244 = &o_option_46_None;
        *r_sel = t2244;
      }
    }
    if ((!d2243) && (u_is_con(s2242, &n_list_46_Cons) && u_is_con(u_field(s2242, 0), &n_syntax_46_Branch))) {
      u_value v_q = u_field(u_field(s2242, 0), 0);
      u_value v_g = u_field(u_field(s2242, 0), 1);
      u_value v_b = u_field(u_field(s2242, 0), 2);
      u_value v_rest = u_field(s2242, 1);
      d2243 = 1;
      {
        u_value s2245 = f_run_46_match_into_3(v_q, v_v, v_env);
        int d2246 = 0;
        if ((!d2246) && u_is_con(s2245, &n_option_46_Some)) {
          u_value v_inner = u_field(s2245, 0);
          d2246 = 1;
          {
            p_run_46_holds(v_p, v_inner, v_g, &v_ok, r_cache);
            if (u_truth(v_ok)) {
              {
                u_value t2247 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(3, v_b, v_inner, f_run_46_pattern_vars_1(v_q)));
                *r_sel = t2247;
              }
            } else {
              {
                p_run_46_select(v_p, v_env, v_v, v_rest, r_sel, r_cache);
              }
            }
          }
        }
        if ((!d2246) && u_is_con(s2245, &n_option_46_None)) {
          d2246 = 1;
          {
            p_run_46_select(v_p, v_env, v_v, v_rest, r_sel, r_cache);
          }
        }
        if (!d2246) {
          u_no_case(s2245);
        }
      }
    }
    if (!d2243) {
      u_no_case(s2242);
    }
  }
}

void p_run_46_holds(u_value v_p, u_value v_env, u_value v_g, u_value *r_ok, u_value *r_cache) {
  {
    u_value v_w = NULL;
    u_value s2248 = v_g;
    int d2249 = 0;
    if ((!d2249) && u_is_con(s2248, &n_option_46_Some)) {
      u_value v_e = u_field(s2248, 0);
      d2249 = 1;
      {
        p_run_46_evaluate(v_p, v_env, v_e, &v_w, r_cache);
        u_value t2250 = f_run_46_truth_1(v_w);
        *r_ok = t2250;
      }
    }
    if ((!d2249) && u_is_con(s2248, &n_option_46_None)) {
      d2249 = 1;
      {
        u_value t2251 = u_mk_bool(1);
        *r_ok = t2251;
      }
    }
    if (!d2249) {
      u_no_case(s2248);
    }
  }
}

void p_run_46_handle(u_value v_p, u_value v_hs, u_value v_v, u_value v_t, u_value *r_env, u_value *r_cache) {
  {
    u_value v_ok = NULL;
    u_value s2252 = v_hs;
    int d2253 = 0;
    if ((!d2253) && u_is_con(s2252, &n_list_46_Nil)) {
      d2253 = 1;
      {
        u_raise(u_mk_con(&n_run_46_Thrown, 2, v_v, v_t), "run.Thrown");
      }
    }
    if ((!d2253) && (u_is_con(s2252, &n_list_46_Cons) && (u_is_con(u_field(s2252, 0), &n_syntax_46_Handler) && u_is_con(u_field(u_field(s2252, 0), 1), &n_syntax_46_PWild)))) {
      u_value v_g = u_field(u_field(s2252, 0), 2);
      u_value v_body = u_field(u_field(s2252, 0), 3);
      u_value v_rest = u_field(s2252, 1);
      d2253 = 1;
      {
        p_run_46_holds(v_p, *r_env, v_g, &v_ok, r_cache);
        if (u_truth(v_ok)) {
          {
            p_run_46_exec(v_p, v_body, r_env, r_cache);
          }
        } else {
          {
            p_run_46_handle(v_p, v_rest, v_v, v_t, r_env, r_cache);
          }
        }
      }
    }
    if ((!d2253) && (u_is_con(s2252, &n_list_46_Cons) && u_is_con(u_field(s2252, 0), &n_syntax_46_Handler))) {
      u_value v_u = u_field(u_field(s2252, 0), 0);
      u_value v_q = u_field(u_field(s2252, 0), 1);
      u_value v_g = u_field(u_field(s2252, 0), 2);
      u_value v_body = u_field(u_field(s2252, 0), 3);
      u_value v_rest = u_field(s2252, 1);
      d2253 = 1;
      {
        u_value t2254 = u_mk_bool(0);
        v_ok = t2254;
        if (u_truth(u_eq(v_u, v_t))) {
          {
            u_value s2255 = f_run_46_match_into_3(v_q, v_v, *r_env);
            int d2256 = 0;
            if ((!d2256) && u_is_con(s2255, &n_option_46_Some)) {
              u_value v_inner = u_field(s2255, 0);
              d2256 = 1;
              {
                p_run_46_holds(v_p, v_inner, v_g, &v_ok, r_cache);
                if (u_truth(v_ok)) {
                  {
                    p_run_46_scoped(v_p, f_run_46_pattern_vars_1(v_q), v_inner, u_mk_con(&n_list_46_Cons, 2, v_body, &o_list_46_Nil), r_env, r_cache);
                  }
                }
              }
            }
            if (!d2256) {
              d2256 = 1;
              {
              }
            }
            if (!d2256) {
              u_no_case(s2255);
            }
          }
        }
        if (u_truth(f_prelude_46_not_1(v_ok))) {
          {
            p_run_46_handle(v_p, v_rest, v_v, v_t, r_env, r_cache);
          }
        }
      }
    }
    if (!d2253) {
      u_no_case(s2252);
    }
  }
}

void p_run_46_invoke(u_value v_p, u_value v_n, u_value v_ins, u_value v_refs, u_value *r_outs, u_value *r_env, u_value *r_cache) {
  {
    u_value s2257 = f_map_46_get_2(v_n, f_run_46_Prog_46_procs_1(v_p));
    int d2258 = 0;
    if ((!d2258) && (u_is_con(s2257, &n_option_46_Some) && u_is_con(u_field(s2257, 0), &n_run_46_ProcDef))) {
      u_value v_xs = u_field(u_field(s2257, 0), 0);
      u_value v_ys = u_field(u_field(s2257, 0), 1);
      u_value v_zs = u_field(u_field(s2257, 0), 2);
      u_value v_body = u_field(u_field(s2257, 0), 3);
      d2258 = 1;
      {
        p_run_46_run_body(v_p, v_n, f_map_46_empty_0(), v_xs, v_ys, v_zs, v_body, v_ins, v_refs, r_outs, r_env, r_cache);
      }
    }
    if ((!d2258) && u_is_con(s2257, &n_option_46_None)) {
      static struct u_object l_os_show_2263 = U_LIT("os_show");
      if (u_truth(u_eq(f_syntax_46_Name_46_base_1(v_n), &l_os_show_2263))) {
        d2258 = 1;
        {
          u_value t2259 = &o_list_46_Nil;
          *r_outs = t2259;
          u_value t2260 = &o_list_46_Nil;
          u_value *e2261 = &t2260;
          u_value l2262 = v_ins;
          while (u_is_con(l2262, &n_list_46_Cons)) {
            u_value v_v = u_field(l2262, 0);
            if (u_truth(u_mk_bool(1))) {
              e2261 = u_append(e2261, f_print_46_show_value_1(v_v));
            }
            l2262 = u_field(l2262, 1);
          }
          p_run_46_print_all(t2260);
        }
      }
    }
    if ((!d2258) && u_is_con(s2257, &n_option_46_None)) {
      static struct u_object l_os_print_2268 = U_LIT("os_print");
      if (u_truth(u_eq(f_syntax_46_Name_46_base_1(v_n), &l_os_print_2268))) {
        d2258 = 1;
        {
          u_value t2264 = &o_list_46_Nil;
          *r_outs = t2264;
          u_value t2265 = &o_list_46_Nil;
          u_value *e2266 = &t2265;
          u_value l2267 = v_ins;
          while (u_is_con(l2267, &n_list_46_Cons)) {
            u_value v_v = u_field(l2267, 0);
            if (u_truth(u_mk_bool(1))) {
              e2266 = u_append(e2266, f_print_46_text_of_1(v_v));
            }
            l2267 = u_field(l2267, 1);
          }
          p_run_46_print_all(t2265);
        }
      }
    }
    if ((!d2258) && u_is_con(s2257, &n_option_46_None)) {
      static struct u_object l_os_args_2273 = U_LIT("os_args");
      if (u_truth(u_eq(f_syntax_46_Name_46_base_1(v_n), &l_os_args_2273))) {
        d2258 = 1;
        {
          u_value t2269 = &o_list_46_Nil;
          u_value *e2270 = &t2269;
          u_value l2271 = f_run_46_Prog_46_args_1(v_p);
          while (u_is_con(l2271, &n_list_46_Cons)) {
            u_value v_a = u_field(l2271, 0);
            if (u_truth(u_mk_bool(1))) {
              e2270 = u_append(e2270, u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_a)));
            }
            l2271 = u_field(l2271, 1);
          }
          u_value t2272 = u_mk_con(&n_list_46_Cons, 2, f_value_46_list_value_1(t2269), &o_list_46_Nil);
          *r_outs = t2272;
        }
      }
    }
    if ((!d2258) && u_is_con(s2257, &n_option_46_None)) {
      d2258 = 1;
      {
        p_run_46_os_proc(v_n, v_ins, r_outs);
      }
    }
    if (!d2258) {
      u_no_case(s2257);
    }
  }
}

void p_run_46_invoke_value(u_value v_p, u_value v_f, u_value v_ins, u_value v_refs, u_value *r_outs, u_value *r_env, u_value *r_cache) {
  {
    u_value s2274 = v_f;
    int d2275 = 0;
    if ((!d2275) && u_is_con(s2274, &n_value_46_Proc)) {
      u_value v_n = u_field(s2274, 0);
      d2275 = 1;
      {
        p_run_46_invoke(v_p, v_n, v_ins, v_refs, r_outs, r_env, r_cache);
      }
    }
    if ((!d2275) && u_is_con(s2274, &n_value_46_ProcClos)) {
      u_value v_around = u_field(s2274, 0);
      u_value v_xs = u_field(s2274, 1);
      u_value v_ys = u_field(s2274, 2);
      u_value v_zs = u_field(s2274, 3);
      u_value v_body = u_field(s2274, 4);
      d2275 = 1;
      {
        static struct u_object l_lambda_2276 = U_LIT("lambda");
        p_run_46_run_body(v_p, f_syntax_46_unqualified_1(&l_lambda_2276), v_around, v_xs, v_ys, v_zs, v_body, v_ins, v_refs, r_outs, r_env, r_cache);
      }
    }
    if (!d2275) {
      d2275 = 1;
      {
        u_raise(u_mk_con(&n_run_46_NotProc, 1, v_f), "run.RuntimeError");
      }
    }
    if (!d2275) {
      u_no_case(s2274);
    }
  }
}

void p_run_46_run_body(u_value v_p, u_value v_n, u_value v_around, u_value v_xs, u_value v_ys, u_value v_zs, u_value v_body, u_value v_ins, u_value v_refs, u_value *r_outs, u_value *r_env, u_value *r_cache) {
  {
    u_value v_inner = NULL;
    u_value t2277 = f_run_46_bind_all_3(v_xs, v_ins, f_map_46_put_all_2(f_run_46_values_in_2(f_list_46_zip_2(f_syntax_46_names_1(v_zs), v_refs), *r_env), f_map_46_delete_all_2(f_syntax_46_names_1(f_list_46__43__43__2(v_ys, v_zs)), v_around)));
    v_inner = t2277;
    {
      struct u_handler *h2278 = u_push();
      if (setjmp(h2278->env) == 0) {
        {
          p_run_46_exec(v_p, v_body, &v_inner, r_cache);
        }
        u_pop(h2278);
      } else {
        u_pop(h2278);
        int d2279 = 0;
        if ((!d2279) && (u_has_type(h2278, "run.Thrown") && u_is_con(u_caught(h2278), &n_run_46_Thrown))) {
          u_value v_v = u_field(u_caught(h2278), 0);
          u_value v_t = u_field(u_caught(h2278), 1);
          d2279 = 1;
          {
            u_value t2280 = f_map_46_put_all_2(f_run_46_values_in_2(f_list_46_zip_2(v_refs, f_syntax_46_names_1(v_zs)), v_inner), *r_env);
            *r_env = t2280;
            u_raise(u_mk_con(&n_run_46_Thrown, 2, v_v, v_t), "run.Thrown");
          }
        }
        if (!d2279) {
          u_reraise(h2278);
        }
      }
    }
    u_value t2281 = f_run_46_read_outs_3(v_n, f_syntax_46_names_1(v_ys), v_inner);
    *r_outs = t2281;
    u_value t2282 = f_map_46_put_all_2(f_run_46_values_in_2(f_list_46_zip_2(v_refs, f_syntax_46_names_1(v_zs)), v_inner), *r_env);
    *r_env = t2282;
  }
}

u_value f_run_46_values_in_2(u_value v_ps, u_value v_env) {
  u_value s2283 = v_ps;
  u_value t2284 = NULL;
  if ((!t2284) && u_is_con(s2283, &n_list_46_Nil)) {
    t2284 = &o_list_46_Nil;
  }
  if ((!t2284) && u_is_con(s2283, &n_list_46_Cons)) {
    u_value v_a = u_field(u_field(s2283, 0), 0);
    u_value v_b = u_field(u_field(s2283, 0), 1);
    u_value v_rest = u_field(s2283, 1);
    u_value s2285 = f_map_46_get_2(v_b, v_env);
    u_value t2286 = NULL;
    if ((!t2286) && u_is_con(s2285, &n_option_46_Some)) {
      u_value v_v = u_field(s2285, 0);
      t2286 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_a, v_v), f_run_46_values_in_2(v_rest, v_env));
    }
    if ((!t2286) && u_is_con(s2285, &n_option_46_None)) {
      t2286 = f_run_46_values_in_2(v_rest, v_env);
    }
    if (!t2286) {
      u_no_case(s2285);
    }
    t2284 = t2286;
  }
  if (!t2284) {
    u_no_case(s2283);
  }
  return t2284;
}

u_value f_run_46_read_outs_3(u_value v_p, u_value v_ys, u_value v_env) {
  u_value s2287 = v_ys;
  u_value t2288 = NULL;
  if ((!t2288) && u_is_con(s2287, &n_list_46_Nil)) {
    t2288 = &o_list_46_Nil;
  }
  if ((!t2288) && u_is_con(s2287, &n_list_46_Cons)) {
    u_value v_y = u_field(s2287, 0);
    u_value v_rest = u_field(s2287, 1);
    u_value s2289 = f_map_46_get_2(v_y, v_env);
    u_value t2290 = NULL;
    if ((!t2290) && u_is_con(s2289, &n_option_46_Some)) {
      u_value v_v = u_field(s2289, 0);
      t2290 = u_mk_con(&n_list_46_Cons, 2, v_v, f_run_46_read_outs_3(v_p, v_rest, v_env));
    }
    if ((!t2290) && u_is_con(s2289, &n_option_46_None)) {
      u_raise(u_mk_con(&n_run_46_UnassignedOutput, 2, v_y, v_p), "run.RuntimeError");
      t2290 = NULL;
    }
    if (!t2290) {
      u_no_case(s2289);
    }
    t2288 = t2290;
  }
  if (!t2288) {
    u_no_case(s2287);
  }
  return t2288;
}

void p_run_46_print_all(u_value v_lines) {
  {
    u_value s2291 = v_lines;
    int d2292 = 0;
    if ((!d2292) && u_is_con(s2291, &n_list_46_Nil)) {
      d2292 = 1;
      {
      }
    }
    if ((!d2292) && u_is_con(s2291, &n_list_46_Cons)) {
      u_value v_line = u_field(s2291, 0);
      u_value v_rest = u_field(s2291, 1);
      d2292 = 1;
      {
        p_os_print(v_line);
        p_run_46_print_all(v_rest);
      }
    }
    if (!d2292) {
      u_no_case(s2291);
    }
  }
}

void p_run_46_os_proc(u_value v_n, u_value v_ins, u_value *r_outs) {
  {
    u_value v_text = NULL;
    {
      struct u_handler *h2299 = u_push();
      if (setjmp(h2299->env) == 0) {
        {
          u_value s2293 = u_mk_tuple(2, f_syntax_46_Name_46_base_1(v_n), v_ins);
          int d2294 = 0;
          if ((!d2294) && (u_equals(u_field(s2293, 0), u_mk_lit("os_read")) && (u_is_con(u_field(s2293, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2293, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2293, 1), 0), 0), &n_syntax_46_VStr) && u_is_con(u_field(u_field(s2293, 1), 1), &n_list_46_Nil)))))) {
            u_value v_path = u_field(u_field(u_field(u_field(s2293, 1), 0), 0), 0);
            d2294 = 1;
            {
              p_os_read(v_path, &v_text);
              u_value t2295 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_text)), &o_list_46_Nil);
              *r_outs = t2295;
            }
          }
          if ((!d2294) && (u_equals(u_field(s2293, 0), u_mk_lit("os_write")) && (u_is_con(u_field(s2293, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2293, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2293, 1), 0), 0), &n_syntax_46_VStr) && (u_is_con(u_field(u_field(s2293, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2293, 1), 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(u_field(s2293, 1), 1), 0), 0), &n_syntax_46_VStr) && u_is_con(u_field(u_field(u_field(s2293, 1), 1), 1), &n_list_46_Nil))))))))) {
            u_value v_path = u_field(u_field(u_field(u_field(s2293, 1), 0), 0), 0);
            u_value v_t = u_field(u_field(u_field(u_field(u_field(s2293, 1), 1), 0), 0), 0);
            d2294 = 1;
            {
              p_os_write(v_path, v_t);
              u_value t2296 = &o_list_46_Nil;
              *r_outs = t2296;
            }
          }
          if ((!d2294) && (u_equals(u_field(s2293, 0), u_mk_lit("os_library")) && (u_is_con(u_field(s2293, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2293, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2293, 1), 0), 0), &n_syntax_46_VStr) && u_is_con(u_field(u_field(s2293, 1), 1), &n_list_46_Nil)))))) {
            u_value v_name = u_field(u_field(u_field(u_field(s2293, 1), 0), 0), 0);
            d2294 = 1;
            {
              p_os_library(v_name, &v_text);
              u_value t2297 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_text)), &o_list_46_Nil);
              *r_outs = t2297;
            }
          }
          if ((!d2294) && (u_equals(u_field(s2293, 0), u_mk_lit("os_print_error")) && (u_is_con(u_field(s2293, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2293, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2293, 1), 0), 0), &n_syntax_46_VStr) && u_is_con(u_field(u_field(s2293, 1), 1), &n_list_46_Nil)))))) {
            u_value v_t = u_field(u_field(u_field(u_field(s2293, 1), 0), 0), 0);
            d2294 = 1;
            {
              p_os_print_error(v_t);
              u_value t2298 = &o_list_46_Nil;
              *r_outs = t2298;
            }
          }
          if ((!d2294) && (u_equals(u_field(s2293, 0), u_mk_lit("os_exit")) && (u_is_con(u_field(s2293, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2293, 1), 0), &n_value_46_Lit) && (u_is_con(u_field(u_field(u_field(s2293, 1), 0), 0), &n_syntax_46_VInt) && u_is_con(u_field(u_field(s2293, 1), 1), &n_list_46_Nil)))))) {
            u_value v_k = u_field(u_field(u_field(u_field(s2293, 1), 0), 0), 0);
            d2294 = 1;
            {
              p_os_exit(v_k);
            }
          }
          if (!d2294) {
            d2294 = 1;
            {
              u_raise(u_mk_con(&n_run_46_UnknownProc, 1, v_n), "run.RuntimeError");
            }
          }
          if (!d2294) {
            u_no_case(s2293);
          }
        }
        u_pop(h2299);
      } else {
        u_pop(h2299);
        int d2300 = 0;
        if ((!d2300) && (u_has_type(h2299, "os.OsError") && u_is_con(u_caught(h2299), &n_os_46_OsError))) {
          u_value v_m = u_field(u_caught(h2299), 0);
          d2300 = 1;
          {
            u_raise(u_mk_con(&n_run_46_Thrown, 2, u_mk_con(&n_value_46_Obj, 2, f_syntax_46_os_error_name_0(), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_m)), &o_list_46_Nil)), u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_os_error_name_0(), &o_list_46_Nil)), "run.Thrown");
          }
        }
        if (!d2300) {
          u_reraise(h2299);
        }
      }
    }
  }
}

void p_run_46_run_proc(u_value v_p, u_value v_n, u_value *r_cache) {
  {
    u_value v_env = NULL;
    u_value v_outs = NULL;
    u_value t2301 = f_map_46_empty_0();
    v_env = t2301;
    p_run_46_invoke(v_p, v_n, &o_list_46_Nil, &o_list_46_Nil, &v_outs, &v_env, r_cache);
  }
}

void p_run_46_run_cmd(u_value v_p, u_value v_c, u_value *r_cache) {
  {
    u_value v_env = NULL;
    u_value t2302 = f_map_46_empty_0();
    v_env = t2302;
    u_value s2303 = v_c;
    int d2304 = 0;
    if ((!d2304) && u_is_con(s2303, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s2303, 2);
      d2304 = 1;
      {
        p_run_46_exec(v_p, v_body, &v_env, r_cache);
      }
    }
    if (!d2304) {
      d2304 = 1;
      {
      }
    }
    if (!d2304) {
      u_no_case(s2303);
    }
  }
}

void p_run_46_run_all(u_value v_p, u_value v_cs, u_value *r_cache) {
  {
    u_value s2305 = v_cs;
    int d2306 = 0;
    if ((!d2306) && u_is_con(s2305, &n_list_46_Nil)) {
      d2306 = 1;
      {
      }
    }
    if ((!d2306) && u_is_con(s2305, &n_list_46_Cons)) {
      u_value v_c = u_field(s2305, 0);
      u_value v_more = u_field(s2305, 1);
      d2306 = 1;
      {
        p_run_46_run_cmd(v_p, v_c, r_cache);
        p_run_46_run_all(v_p, v_more, r_cache);
      }
    }
    if (!d2306) {
      u_no_case(s2305);
    }
  }
}

void p_run_46_run_program(u_value v_q) {
  {
    u_value v_cache = NULL;
    u_value t2307 = f_map_46_empty_0();
    v_cache = t2307;
    p_run_46_run_all(f_run_46_load_1(v_q), f_syntax_46_Program_46_cmds_1(v_q), &v_cache);
  }
}

void p_run_46_check_and_run(u_value v_cs) {
  {
    u_value v_q = NULL;
    static struct u_object l_main_2308 = U_LIT("main");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, v_cs), &l_main_2308, &v_q);
    p_run_46_run_program(v_q);
  }
}

u_value f_run_46_asserting_3(u_value v_n, u_value v_e, u_value v_expected) {
  static struct u_object l__61__61__2309 = U_LIT("==");
  return u_mk_con(&n_syntax_46_CTest, 3, v_n, &o_list_46_Nil, u_mk_con(&n_syntax_46_SBlock, 2, &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssert, 1, f_check_46_ap_2(&l__61__61__2309, u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Cons, 2, v_expected, &o_list_46_Nil)))), &o_list_46_Nil)));
}

u_value f_run_46_assigning_1(u_value v_e) {
  static struct u_object l_a_2310 = U_LIT("a");
  static struct u_object l_v_2311 = U_LIT("v");
  static struct u_object l_v_2312 = U_LIT("v");
  return u_mk_con(&n_syntax_46_CTest, 3, &l_a_2310, &o_list_46_Nil, u_mk_con(&n_syntax_46_SBlock, 2, u_mk_con(&n_list_46_Cons, 2, f_run_46_hole_var_1(&l_v_2311), &o_list_46_Nil), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_run_46_hole_var_1(&l_v_2312), v_e), &o_list_46_Nil)), &o_list_46_Nil)));
}

u_value f_run_46_int_1(u_value v_n) {
  return u_mk_con(&n_value_46_Lit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
}

u_value f_run_46_procedures_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of run.procedures");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_Fail_2313 = U_LIT("Fail");
  static struct u_object l_Fail_2314 = U_LIT("Fail");
  static struct u_object l_sum_2315 = U_LIT("sum");
  static struct u_object l_n_2316 = U_LIT("n");
  static struct u_object l_s_2317 = U_LIT("s");
  static struct u_object l_i_2318 = U_LIT("i");
  static struct u_object l_s_2319 = U_LIT("s");
  static struct u_object l_i_2320 = U_LIT("i");
  static struct u_object l__60__2321 = U_LIT("<");
  static struct u_object l_i_2322 = U_LIT("i");
  static struct u_object l_n_2323 = U_LIT("n");
  static struct u_object l_i_2324 = U_LIT("i");
  static struct u_object l__43__2325 = U_LIT("+");
  static struct u_object l_i_2326 = U_LIT("i");
  static struct u_object l_s_2327 = U_LIT("s");
  static struct u_object l__43__2328 = U_LIT("+");
  static struct u_object l_s_2329 = U_LIT("s");
  static struct u_object l__43__2330 = U_LIT("+");
  static struct u_object l_i_2331 = U_LIT("i");
  static struct u_object l_count_2332 = U_LIT("count");
  static struct u_object l_k_2333 = U_LIT("k");
  static struct u_object l_k_2334 = U_LIT("k");
  static struct u_object l__43__2335 = U_LIT("+");
  static struct u_object l_k_2336 = U_LIT("k");
  static struct u_object l_Fail_2337 = U_LIT("Fail");
  static struct u_object l_k_2338 = U_LIT("k");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_Fail_2313), &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Fail_2314), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), &o_list_46_Nil)), &o_list_46_Nil)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(&l_sum_2315), &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_n_2316), f_check_46_tint_0()), &o_list_46_Nil), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_s_2317), f_check_46_tint_0()), &o_list_46_Nil), &o_list_46_Nil, u_mk_con(&n_syntax_46_SBlock, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_i_2318), f_check_46_tint_0()), &o_list_46_Nil), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_s_2319), &o_syntax_46_THole), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_i_2320), &o_syntax_46_THole), f_check_46_num_1(u_mk_int(0))), &o_list_46_Nil))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SWhile, 2, f_check_46_ap_2(&l__60__2321, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_i_2322), u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_n_2323), &o_list_46_Nil))), u_mk_con(&n_syntax_46_SBlock, 2, &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_i_2324), &o_syntax_46_THole), f_check_46_ap_2(&l__43__2325, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_i_2326), u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), &o_list_46_Nil)))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_s_2327), &o_syntax_46_THole), f_check_46_ap_2(&l__43__2328, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_s_2329), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l__43__2330, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_i_2331), u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), &o_list_46_Nil))), &o_list_46_Nil)))), &o_list_46_Nil))), &o_list_46_Nil))), &o_list_46_Nil)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(&l_count_2332), &o_list_46_Nil, &o_list_46_Nil, &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_k_2333), f_check_46_tint_0()), &o_list_46_Nil), u_mk_con(&n_syntax_46_SBlock, 2, &o_list_46_Nil, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_k_2334), &o_syntax_46_THole), f_check_46_ap_2(&l__43__2335, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_k_2336), u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), &o_list_46_Nil)))), &o_list_46_Nil)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SRaise, 2, f_check_46_ap_2(&l_Fail_2337, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_k_2338), &o_list_46_Nil)), &o_syntax_46_THole), &o_list_46_Nil)))), &o_list_46_Nil)));
  return cache;
}

u_value f_run_46_hole_var_1(u_value v_n) {
  return u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), &o_syntax_46_THole);
}

u_value f_run_46_equals_2(u_value v_n, u_value v_k) {
  static struct u_object l__61__61__2339 = U_LIT("==");
  return u_mk_con(&n_syntax_46_SAssert, 1, f_check_46_ap_2(&l__61__61__2339, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(v_n), u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(v_k), &o_list_46_Nil))));
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s2340 = v_t;
  u_value t2341 = NULL;
  if ((!t2341) && u_is_con(s2340, &n_c_46_CtVoid)) {
    static struct u_object l_void_2342 = U_LIT("void");
    t2341 = &l_void_2342;
  }
  if ((!t2341) && u_is_con(s2340, &n_c_46_CtInt)) {
    static struct u_object l_int_2343 = U_LIT("int");
    t2341 = &l_int_2343;
  }
  if ((!t2341) && u_is_con(s2340, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s2340, 0);
    t2341 = v_n;
  }
  if ((!t2341) && (u_is_con(s2340, &n_c_46_CtPtr) && u_is_con(u_field(s2340, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s2340, 0), 0);
    static struct u_object l__42__2344 = U_LIT("*");
    t2341 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__2344);
  }
  if ((!t2341) && u_is_con(s2340, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s2340, 0);
    static struct u_object l__32__42__2345 = U_LIT(" *");
    t2341 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__2345);
  }
  if (!t2341) {
    u_no_case(s2340);
  }
  return t2341;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s2346 = v_t;
  u_value t2347 = NULL;
  if ((!t2347) && u_is_con(s2346, &n_c_46_CtPtr)) {
    t2347 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t2347) {
    static struct u_object l__32__2348 = U_LIT(" ");
    t2347 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__2348, v_x));
  }
  if (!t2347) {
    u_no_case(s2346);
  }
  return t2347;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s2349 = v_e;
  u_value t2350 = NULL;
  if ((!t2350) && u_is_con(s2349, &n_c_46_CeVar)) {
    u_value v_x = u_field(s2349, 0);
    t2350 = v_x;
  }
  if ((!t2350) && u_is_con(s2349, &n_c_46_CeNum)) {
    u_value v_n = u_field(s2349, 0);
    t2350 = u_str_show(v_n);
  }
  if ((!t2350) && u_is_con(s2349, &n_c_46_CeStr)) {
    u_value v_s = u_field(s2349, 0);
    t2350 = u_str_quote(v_s);
  }
  if ((!t2350) && u_is_con(s2349, &n_c_46_CeCall)) {
    u_value v_f = u_field(s2349, 0);
    u_value v_args = u_field(s2349, 1);
    static struct u_object l__40__2351 = U_LIT("(");
    static struct u_object l__44__32__2352 = U_LIT(", ");
    u_value t2353 = &o_list_46_Nil;
    u_value *e2354 = &t2353;
    u_value l2355 = v_args;
    while (u_is_con(l2355, &n_list_46_Cons)) {
      u_value v_a = u_field(l2355, 0);
      if (u_truth(u_mk_bool(1))) {
        e2354 = u_append(e2354, f_c_46_c_expr_1(v_a));
      }
      l2355 = u_field(l2355, 1);
    }
    static struct u_object l__41__2356 = U_LIT(")");
    t2350 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__2351, u_concat(u_str_join(&l__44__32__2352, t2353), &l__41__2356)));
  }
  if ((!t2350) && u_is_con(s2349, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s2349, 0);
    u_value v_a = u_field(s2349, 1);
    t2350 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t2350) && u_is_con(s2349, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s2349, 0);
    u_value v_a = u_field(s2349, 1);
    u_value v_b = u_field(s2349, 2);
    static struct u_object l__32__2357 = U_LIT(" ");
    static struct u_object l__32__2358 = U_LIT(" ");
    t2350 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__2357, u_concat(v_o, u_concat(&l__32__2358, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t2350) && u_is_con(s2349, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s2349, 0);
    u_value v_i = u_field(s2349, 1);
    static struct u_object l__91__2359 = U_LIT("[");
    static struct u_object l__93__2360 = U_LIT("]");
    t2350 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__2359, u_concat(f_c_46_c_expr_1(v_i), &l__93__2360)));
  }
  if ((!t2350) && u_is_con(s2349, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s2349, 0);
    u_value v_f = u_field(s2349, 1);
    static struct u_object l__45__62__2361 = U_LIT("->");
    t2350 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__2361, v_f));
  }
  if ((!t2350) && u_is_con(s2349, &n_c_46_CeCast)) {
    u_value v_t = u_field(s2349, 0);
    u_value v_a = u_field(s2349, 1);
    static struct u_object l__40__2362 = U_LIT("(");
    static struct u_object l__41__2363 = U_LIT(")");
    t2350 = u_concat(&l__40__2362, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__2363, f_c_46_c_operand_1(v_a))));
  }
  if ((!t2350) && u_is_con(s2349, &n_c_46_CeCond)) {
    u_value v_c = u_field(s2349, 0);
    u_value v_a = u_field(s2349, 1);
    u_value v_b = u_field(s2349, 2);
    static struct u_object l__32__63__32__2364 = U_LIT(" ? ");
    static struct u_object l__32__58__32__2365 = U_LIT(" : ");
    t2350 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__2364, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__2365, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t2350) && u_is_con(s2349, &n_c_46_CeInit)) {
    u_value v_es = u_field(s2349, 0);
    static struct u_object l__123__2366 = U_LIT("{");
    static struct u_object l__44__32__2367 = U_LIT(", ");
    u_value t2368 = &o_list_46_Nil;
    u_value *e2369 = &t2368;
    u_value l2370 = v_es;
    while (u_is_con(l2370, &n_list_46_Cons)) {
      u_value v_a = u_field(l2370, 0);
      if (u_truth(u_mk_bool(1))) {
        e2369 = u_append(e2369, f_c_46_c_expr_1(v_a));
      }
      l2370 = u_field(l2370, 1);
    }
    static struct u_object l__125__2371 = U_LIT("}");
    t2350 = u_concat(&l__123__2366, u_concat(u_str_join(&l__44__32__2367, t2368), &l__125__2371));
  }
  if (!t2350) {
    u_no_case(s2349);
  }
  return t2350;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s2372 = v_e;
  u_value t2373 = NULL;
  if ((!t2373) && u_is_con(s2372, &n_c_46_CeVar)) {
    t2373 = f_c_46_c_expr_1(v_e);
  }
  if ((!t2373) && u_is_con(s2372, &n_c_46_CeNum)) {
    u_value v_n = u_field(s2372, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t2373 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t2373) && u_is_con(s2372, &n_c_46_CeStr)) {
    t2373 = f_c_46_c_expr_1(v_e);
  }
  if ((!t2373) && u_is_con(s2372, &n_c_46_CeCall)) {
    t2373 = f_c_46_c_expr_1(v_e);
  }
  if ((!t2373) && u_is_con(s2372, &n_c_46_CeIndex)) {
    t2373 = f_c_46_c_expr_1(v_e);
  }
  if ((!t2373) && u_is_con(s2372, &n_c_46_CeArrow)) {
    t2373 = f_c_46_c_expr_1(v_e);
  }
  if (!t2373) {
    static struct u_object l__40__2374 = U_LIT("(");
    static struct u_object l__41__2375 = U_LIT(")");
    t2373 = u_concat(&l__40__2374, u_concat(f_c_46_c_expr_1(v_e), &l__41__2375));
  }
  if (!t2373) {
    u_no_case(s2372);
  }
  return t2373;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s2376 = v_s;
  u_value t2377 = NULL;
  if ((!t2377) && u_is_con(s2376, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s2376, 0);
    static struct u_object l__59__2378 = U_LIT(";");
    t2377 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__2378)), &o_list_46_Nil);
  }
  if ((!t2377) && (u_is_con(s2376, &n_c_46_CsDecl) && u_is_con(u_field(s2376, 2), &n_option_46_None))) {
    u_value v_t = u_field(s2376, 0);
    u_value v_x = u_field(s2376, 1);
    static struct u_object l__59__2379 = U_LIT(";");
    t2377 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__2379)), &o_list_46_Nil);
  }
  if ((!t2377) && (u_is_con(s2376, &n_c_46_CsDecl) && u_is_con(u_field(s2376, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s2376, 0);
    u_value v_x = u_field(s2376, 1);
    u_value v_e = u_field(u_field(s2376, 2), 0);
    static struct u_object l__32__61__32__2380 = U_LIT(" = ");
    static struct u_object l__59__2381 = U_LIT(";");
    t2377 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__2380, u_concat(f_c_46_c_expr_1(v_e), &l__59__2381)))), &o_list_46_Nil);
  }
  if ((!t2377) && u_is_con(s2376, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s2376, 0);
    u_value v_e = u_field(s2376, 1);
    static struct u_object l__32__61__32__2382 = U_LIT(" = ");
    static struct u_object l__59__2383 = U_LIT(";");
    t2377 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__2382, u_concat(f_c_46_c_expr_1(v_e), &l__59__2383)))), &o_list_46_Nil);
  }
  if ((!t2377) && (u_is_con(s2376, &n_c_46_CsIf) && u_is_con(u_field(s2376, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s2376, 0);
    u_value v_l = u_field(s2376, 1);
    static struct u_object l_if_32__40__2384 = U_LIT("if (");
    static struct u_object l__41__32__123__2385 = U_LIT(") {");
    static struct u_object l__32__32__2386 = U_LIT("  ");
    static struct u_object l__125__2387 = U_LIT("}");
    t2377 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__2384, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__2385))), &o_list_46_Nil), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__2386), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__2387), &o_list_46_Nil)));
  }
  if ((!t2377) && u_is_con(s2376, &n_c_46_CsIf)) {
    u_value v_c = u_field(s2376, 0);
    u_value v_l = u_field(s2376, 1);
    u_value v_r = u_field(s2376, 2);
    static struct u_object l_if_32__40__2388 = U_LIT("if (");
    static struct u_object l__41__32__123__2389 = U_LIT(") {");
    static struct u_object l__32__32__2390 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__2391 = U_LIT("} else {");
    static struct u_object l__32__32__2392 = U_LIT("  ");
    static struct u_object l__125__2393 = U_LIT("}");
    t2377 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__2388, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__2389))), &o_list_46_Nil), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__2390), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__2391), &o_list_46_Nil), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__2392), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__2393), &o_list_46_Nil)))));
  }
  if ((!t2377) && u_is_con(s2376, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s2376, 0);
    u_value v_b = u_field(s2376, 1);
    static struct u_object l_while_32__40__2394 = U_LIT("while (");
    static struct u_object l__41__32__123__2395 = U_LIT(") {");
    static struct u_object l__32__32__2396 = U_LIT("  ");
    static struct u_object l__125__2397 = U_LIT("}");
    t2377 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__2394, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__2395))), &o_list_46_Nil), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__2396), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__2397), &o_list_46_Nil)));
  }
  if ((!t2377) && (u_is_con(s2376, &n_c_46_CsReturn) && u_is_con(u_field(s2376, 0), &n_option_46_None))) {
    static struct u_object l_return_59__2398 = U_LIT("return;");
    t2377 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__2398), &o_list_46_Nil);
  }
  if ((!t2377) && (u_is_con(s2376, &n_c_46_CsReturn) && u_is_con(u_field(s2376, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s2376, 0), 0);
    static struct u_object l_return_32__2399 = U_LIT("return ");
    static struct u_object l__59__2400 = U_LIT(";");
    t2377 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__2399, u_concat(f_c_46_c_expr_1(v_e), &l__59__2400))), &o_list_46_Nil);
  }
  if ((!t2377) && u_is_con(s2376, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s2376, 0);
    static struct u_object l__123__2401 = U_LIT("{");
    static struct u_object l__32__32__2402 = U_LIT("  ");
    static struct u_object l__125__2403 = U_LIT("}");
    t2377 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__2401), &o_list_46_Nil), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__2402), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__2403), &o_list_46_Nil)));
  }
  if (!t2377) {
    u_no_case(s2376);
  }
  return t2377;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t2404 = &o_list_46_Nil;
  u_value *e2405 = &t2404;
  u_value l2406 = v_ss;
  while (u_is_con(l2406, &n_list_46_Cons)) {
    u_value v_s = u_field(l2406, 0);
    if (u_truth(u_mk_bool(1))) {
      e2405 = u_append(e2405, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l2406 = u_field(l2406, 1);
  }
  return f_list_46_concat_1(t2404);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s2407 = v_ps;
  u_value t2408 = NULL;
  if ((!t2408) && u_is_con(s2407, &n_list_46_Nil)) {
    static struct u_object l_void_2409 = U_LIT("void");
    t2408 = &l_void_2409;
  }
  if (!t2408) {
    static struct u_object l__44__32__2410 = U_LIT(", ");
    u_value t2411 = &o_list_46_Nil;
    u_value *e2412 = &t2411;
    u_value l2413 = v_ps;
    while (u_is_con(l2413, &n_list_46_Cons)) {
      u_value v_p = u_field(l2413, 0);
      if (u_truth(u_mk_bool(1))) {
        e2412 = u_append(e2412, f_c_46_c_param_1(v_p));
      }
      l2413 = u_field(l2413, 1);
    }
    t2408 = u_str_join(&l__44__32__2410, t2411);
  }
  if (!t2408) {
    u_no_case(s2407);
  }
  return t2408;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s2414 = v_p;
  u_value t2415 = NULL;
  if (!t2415) {
    u_value v_t = u_field(s2414, 0);
    u_value v_x = u_field(s2414, 1);
    t2415 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t2415) {
    u_no_case(s2414);
  }
  return t2415;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s2416 = v_d;
  u_value t2417 = NULL;
  if ((!t2417) && u_is_con(s2416, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s2416, 0);
    static struct u_object l__35_include_32__2418 = U_LIT("#include ");
    t2417 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__2418, u_str_quote(v_h)), &o_list_46_Nil);
  }
  if ((!t2417) && u_is_con(s2416, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s2416, 0);
    u_value v_f = u_field(s2416, 1);
    u_value v_ps = u_field(s2416, 2);
    static struct u_object l__40__2419 = U_LIT("(");
    static struct u_object l__41__59__2420 = U_LIT(");");
    t2417 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__2419, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__2420))), &o_list_46_Nil);
  }
  if ((!t2417) && u_is_con(s2416, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2416, 0);
    u_value v_f = u_field(s2416, 1);
    u_value v_ps = u_field(s2416, 2);
    u_value v_b = u_field(s2416, 3);
    static struct u_object l__40__2421 = U_LIT("(");
    static struct u_object l__41__32__123__2422 = U_LIT(") {");
    static struct u_object l__32__32__2423 = U_LIT("  ");
    static struct u_object l__125__2424 = U_LIT("}");
    t2417 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__2421, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__2422))), &o_list_46_Nil), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__2423, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__2424, &o_list_46_Nil)));
  }
  if ((!t2417) && (u_is_con(s2416, &n_c_46_CdGlobal) && u_is_con(u_field(s2416, 2), &n_option_46_None))) {
    u_value v_t = u_field(s2416, 0);
    u_value v_x = u_field(s2416, 1);
    static struct u_object l__59__2425 = U_LIT(";");
    t2417 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__2425), &o_list_46_Nil);
  }
  if ((!t2417) && (u_is_con(s2416, &n_c_46_CdGlobal) && u_is_con(u_field(s2416, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s2416, 0);
    u_value v_x = u_field(s2416, 1);
    u_value v_e = u_field(u_field(s2416, 2), 0);
    static struct u_object l__32__61__32__2426 = U_LIT(" = ");
    static struct u_object l__59__2427 = U_LIT(";");
    t2417 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__2426, u_concat(f_c_46_c_expr_1(v_e), &l__59__2427))), &o_list_46_Nil);
  }
  if (!t2417) {
    u_no_case(s2416);
  }
  return t2417;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s2428 = v_f;
  u_value t2429 = NULL;
  if ((!t2429) && u_is_con(s2428, &n_c_46_CFile)) {
    u_value v_ds = u_field(s2428, 0);
    static struct u_object l__10__10__2430 = U_LIT("\n\n");
    u_value t2431 = &o_list_46_Nil;
    u_value *e2432 = &t2431;
    u_value l2433 = v_ds;
    while (u_is_con(l2433, &n_list_46_Cons)) {
      u_value v_d = u_field(l2433, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__2434 = U_LIT("\n");
        e2432 = u_append(e2432, u_str_join(&l__10__2434, f_c_46_c_decl_1(v_d)));
      }
      l2433 = u_field(l2433, 1);
    }
    static struct u_object l__10__2435 = U_LIT("\n");
    t2429 = u_concat(u_str_join(&l__10__10__2430, t2431), &l__10__2435);
  }
  if (!t2429) {
    u_no_case(s2428);
  }
  return t2429;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s2436 = v_x;
  u_value t2437 = NULL;
  if ((!t2437) && u_is_con(s2436, &n_compile_46_Globals)) {
    u_value v_y = u_field(s2436, 0);
    t2437 = v_y;
  }
  if (!t2437) {
    u_no_case(s2436);
  }
  return t2437;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s2438 = v_x;
  u_value t2439 = NULL;
  if ((!t2439) && u_is_con(s2438, &n_compile_46_Globals)) {
    u_value v_y = u_field(s2438, 1);
    t2439 = v_y;
  }
  if (!t2439) {
    u_no_case(s2438);
  }
  return t2439;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s2440 = v_x;
  u_value t2441 = NULL;
  if ((!t2441) && u_is_con(s2440, &n_compile_46_Globals)) {
    u_value v_y = u_field(s2440, 2);
    t2441 = v_y;
  }
  if (!t2441) {
    u_no_case(s2440);
  }
  return t2441;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s2442 = v_x;
  u_value t2443 = NULL;
  if ((!t2443) && u_is_con(s2442, &n_compile_46_Globals)) {
    u_value v_y = u_field(s2442, 3);
    t2443 = v_y;
  }
  if (!t2443) {
    u_no_case(s2442);
  }
  return t2443;
}

u_value f_compile_46_Globals_46_procs_1(u_value v_x) {
  u_value s2444 = v_x;
  u_value t2445 = NULL;
  if ((!t2445) && u_is_con(s2444, &n_compile_46_Globals)) {
    u_value v_y = u_field(s2444, 4);
    t2445 = v_y;
  }
  if (!t2445) {
    u_no_case(s2444);
  }
  return t2445;
}

u_value f_compile_46_Globals_46_funs_58__61__2(u_value v_x, u_value v_y) {
  u_value s2446 = v_x;
  u_value t2447 = NULL;
  if ((!t2447) && u_is_con(s2446, &n_compile_46_Globals)) {
    u_value v_a1 = u_field(s2446, 1);
    u_value v_a2 = u_field(s2446, 2);
    u_value v_a3 = u_field(s2446, 3);
    u_value v_a4 = u_field(s2446, 4);
    t2447 = u_mk_con(&n_compile_46_Globals, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t2447) {
    u_no_case(s2446);
  }
  return t2447;
}

u_value f_compile_46_Globals_46_constrs_58__61__2(u_value v_x, u_value v_y) {
  u_value s2448 = v_x;
  u_value t2449 = NULL;
  if ((!t2449) && u_is_con(s2448, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s2448, 0);
    u_value v_a2 = u_field(s2448, 2);
    u_value v_a3 = u_field(s2448, 3);
    u_value v_a4 = u_field(s2448, 4);
    t2449 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t2449) {
    u_no_case(s2448);
  }
  return t2449;
}

u_value f_compile_46_Globals_46_arities_58__61__2(u_value v_x, u_value v_y) {
  u_value s2450 = v_x;
  u_value t2451 = NULL;
  if ((!t2451) && u_is_con(s2450, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s2450, 0);
    u_value v_a1 = u_field(s2450, 1);
    u_value v_a3 = u_field(s2450, 3);
    u_value v_a4 = u_field(s2450, 4);
    t2451 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t2451) {
    u_no_case(s2450);
  }
  return t2451;
}

u_value f_compile_46_Globals_46_fields_58__61__2(u_value v_x, u_value v_y) {
  u_value s2452 = v_x;
  u_value t2453 = NULL;
  if ((!t2453) && u_is_con(s2452, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s2452, 0);
    u_value v_a1 = u_field(s2452, 1);
    u_value v_a2 = u_field(s2452, 2);
    u_value v_a4 = u_field(s2452, 4);
    t2453 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t2453) {
    u_no_case(s2452);
  }
  return t2453;
}

u_value f_compile_46_Globals_46_procs_58__61__2(u_value v_x, u_value v_y) {
  u_value s2454 = v_x;
  u_value t2455 = NULL;
  if ((!t2455) && u_is_con(s2454, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s2454, 0);
    u_value v_a1 = u_field(s2454, 1);
    u_value v_a2 = u_field(s2454, 2);
    u_value v_a3 = u_field(s2454, 3);
    t2455 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t2455) {
    u_no_case(s2454);
  }
  return t2455;
}

u_value f_compile_46_Gen_46_count_1(u_value v_x) {
  u_value s2456 = v_x;
  u_value t2457 = NULL;
  if ((!t2457) && u_is_con(s2456, &n_compile_46_Gen)) {
    u_value v_y = u_field(s2456, 0);
    t2457 = v_y;
  }
  if (!t2457) {
    u_no_case(s2456);
  }
  return t2457;
}

u_value f_compile_46_Gen_46_lifted_1(u_value v_x) {
  u_value s2458 = v_x;
  u_value t2459 = NULL;
  if ((!t2459) && u_is_con(s2458, &n_compile_46_Gen)) {
    u_value v_y = u_field(s2458, 1);
    t2459 = v_y;
  }
  if (!t2459) {
    u_no_case(s2458);
  }
  return t2459;
}

u_value f_compile_46_Gen_46_count_58__61__2(u_value v_x, u_value v_y) {
  u_value s2460 = v_x;
  u_value t2461 = NULL;
  if ((!t2461) && u_is_con(s2460, &n_compile_46_Gen)) {
    u_value v_a1 = u_field(s2460, 1);
    t2461 = u_mk_con(&n_compile_46_Gen, 2, v_y, v_a1);
  }
  if (!t2461) {
    u_no_case(s2460);
  }
  return t2461;
}

u_value f_compile_46_Gen_46_lifted_58__61__2(u_value v_x, u_value v_y) {
  u_value s2462 = v_x;
  u_value t2463 = NULL;
  if ((!t2463) && u_is_con(s2462, &n_compile_46_Gen)) {
    u_value v_a0 = u_field(s2462, 0);
    t2463 = u_mk_con(&n_compile_46_Gen, 2, v_a0, v_y);
  }
  if (!t2463) {
    u_no_case(s2462);
  }
  return t2463;
}

u_value f_compile_46_value_type_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of compile.value_type");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_u_value_2464 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_2464);
  return cache;
}

u_value f_compile_46_value_ptr_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of compile.value_ptr");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_u_value_2465 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_2465));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t2466 = &o_list_46_Nil;
  u_value *e2467 = &t2466;
  u_value l2468 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l2468, &n_list_46_Cons)) {
    u_value v_c = u_field(l2468, 0);
    if (u_truth(u_mk_bool(1))) {
      e2467 = u_append(e2467, f_compile_46_mangle_char_1(v_c));
    }
    l2468 = u_field(l2468, 1);
  }
  return u_string(f_list_46_concat_1(t2466));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t2469 = u_is_letter(v_c);
  if (!u_truth(t2469)) {
    t2469 = u_is_digit(v_c);
  }
  u_value t2470 = t2469;
  if (!u_truth(t2470)) {
    t2470 = u_eq(v_c, u_mk_char(95));
  }
  u_value t2473;
  if (u_truth(t2470)) {
    t2473 = u_mk_con(&n_list_46_Cons, 2, v_c, &o_list_46_Nil);
  } else {
    static struct u_object l___2471 = U_LIT("_");
    static struct u_object l___2472 = U_LIT("_");
    t2473 = u_chars(u_concat(&l___2471, u_concat(u_str_show(u_ord(v_c)), &l___2472)));
  }
  return t2473;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__2474 = U_LIT("f_");
  static struct u_object l___2475 = U_LIT("_");
  return u_concat(&l_f__2474, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___2475, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__2476 = U_LIT("p_");
  return u_concat(&l_p__2476, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__2477 = U_LIT("v_");
  return u_concat(&l_v__2477, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__2478 = U_LIT("r_");
  return u_concat(&l_r__2478, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_builtin_functions_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of compile.builtin_functions");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l__43__2479 = U_LIT("+");
  static struct u_object l_u_add_2480 = U_LIT("u_add");
  static struct u_object l__45__2481 = U_LIT("-");
  static struct u_object l_u_sub_2482 = U_LIT("u_sub");
  static struct u_object l__42__2483 = U_LIT("*");
  static struct u_object l_u_mul_2484 = U_LIT("u_mul");
  static struct u_object l__47__2485 = U_LIT("/");
  static struct u_object l_u_div_2486 = U_LIT("u_div");
  static struct u_object l__37__2487 = U_LIT("%");
  static struct u_object l_u_mod_2488 = U_LIT("u_mod");
  static struct u_object l__60__2489 = U_LIT("<");
  static struct u_object l_u_lt_2490 = U_LIT("u_lt");
  static struct u_object l__61__61__2491 = U_LIT("==");
  static struct u_object l_u_eq_2492 = U_LIT("u_eq");
  static struct u_object l_compare_2493 = U_LIT("compare");
  static struct u_object l_u_compare_2494 = U_LIT("u_compare");
  static struct u_object l_chars_2495 = U_LIT("chars");
  static struct u_object l_u_chars_2496 = U_LIT("u_chars");
  static struct u_object l_string_2497 = U_LIT("string");
  static struct u_object l_u_string_2498 = U_LIT("u_string");
  static struct u_object l_ord_2499 = U_LIT("ord");
  static struct u_object l_u_ord_2500 = U_LIT("u_ord");
  static struct u_object l_chr_2501 = U_LIT("chr");
  static struct u_object l_u_chr_2502 = U_LIT("u_chr");
  static struct u_object l__94__2503 = U_LIT("^");
  static struct u_object l_u_concat_2504 = U_LIT("u_concat");
  static struct u_object l_str_size_2505 = U_LIT("str_size");
  static struct u_object l_u_str_size_2506 = U_LIT("u_str_size");
  static struct u_object l_str_join_2507 = U_LIT("str_join");
  static struct u_object l_u_str_join_2508 = U_LIT("u_str_join");
  static struct u_object l_str_slice_2509 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_2510 = U_LIT("u_str_slice");
  static struct u_object l_str_split_2511 = U_LIT("str_split");
  static struct u_object l_u_str_split_2512 = U_LIT("u_str_split");
  static struct u_object l_str_get_2513 = U_LIT("str_get");
  static struct u_object l_u_str_get_2514 = U_LIT("u_str_get");
  static struct u_object l_str_contains_2515 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_2516 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_2517 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_2518 = U_LIT("u_str_compare");
  static struct u_object l_str_show_2519 = U_LIT("str_show");
  static struct u_object l_u_str_show_2520 = U_LIT("u_str_show");
  static struct u_object l_str_quote_2521 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_2522 = U_LIT("u_str_quote");
  static struct u_object l_str_number_2523 = U_LIT("str_number");
  static struct u_object l_u_str_number_2524 = U_LIT("u_str_number");
  static struct u_object l_is_digit_2525 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_2526 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_2527 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_2528 = U_LIT("u_is_letter");
  static struct u_object l_is_space_2529 = U_LIT("is_space");
  static struct u_object l_u_is_space_2530 = U_LIT("u_is_space");
  u_pop(handler);
  cache = f_map_46_put_all_2(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__2479), u_mk_tuple(2, &l_u_add_2480, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__2481), u_mk_tuple(2, &l_u_sub_2482, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__2483), u_mk_tuple(2, &l_u_mul_2484, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__2485), u_mk_tuple(2, &l_u_div_2486, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__2487), u_mk_tuple(2, &l_u_mod_2488, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__2489), u_mk_tuple(2, &l_u_lt_2490, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__2491), u_mk_tuple(2, &l_u_eq_2492, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_2493), u_mk_tuple(2, &l_u_compare_2494, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_2495), u_mk_tuple(2, &l_u_chars_2496, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_2497), u_mk_tuple(2, &l_u_string_2498, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_2499), u_mk_tuple(2, &l_u_ord_2500, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_2501), u_mk_tuple(2, &l_u_chr_2502, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__2503), u_mk_tuple(2, &l_u_concat_2504, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_2505), u_mk_tuple(2, &l_u_str_size_2506, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_2507), u_mk_tuple(2, &l_u_str_join_2508, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_2509), u_mk_tuple(2, &l_u_str_slice_2510, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_2511), u_mk_tuple(2, &l_u_str_split_2512, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_2513), u_mk_tuple(2, &l_u_str_get_2514, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_2515), u_mk_tuple(2, &l_u_str_contains_2516, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_2517), u_mk_tuple(2, &l_u_str_compare_2518, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_2519), u_mk_tuple(2, &l_u_str_show_2520, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_2521), u_mk_tuple(2, &l_u_str_quote_2522, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_2523), u_mk_tuple(2, &l_u_str_number_2524, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_2525), u_mk_tuple(2, &l_u_is_digit_2526, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_2527), u_mk_tuple(2, &l_u_is_letter_2528, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_2529), u_mk_tuple(2, &l_u_is_space_2530, u_mk_int(1))), &o_list_46_Nil)))))))))))))))))))))))))), f_map_46_empty_0());
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__2531 = U_LIT("&");
  static struct u_object l_n__2532 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__2531, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__2532, f_compile_46_mangle_1(v_n))));
}

u_value f_compile_46_unit_value_1(u_value v_n) {
  static struct u_object l__38__2533 = U_LIT("&");
  static struct u_object l_o__2534 = U_LIT("o_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__2533, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_o__2534, f_compile_46_mangle_1(v_n))));
}

u_value f_compile_46_runtime_constructors_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of compile.runtime_constructors");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  u_pop(handler);
  cache = f_set_46_from_list_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_nil_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_cons_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_none_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_some_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_os_error_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_division_by_zero_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_no_index_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_no_character_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_missing_field_name_0(), &o_list_46_Nil))))))))));
  return cache;
}

u_value f_compile_46_descriptor_decl_2(u_value v_n, u_value v_i) {
  u_value s2535 = v_n;
  u_value t2536 = NULL;
  if ((!t2536) && (u_is_con(s2535, &n_syntax_46_Name) && u_is_con(u_field(s2535, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s2535, 0), 0);
    u_value v_b = u_field(s2535, 1);
    static struct u_object l_const_32_struct_32_u_n_2537 = U_LIT("const struct u_name");
    static struct u_object l_n__2538 = U_LIT("n_");
    t2536 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2537), u_concat(&l_n__2538, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), &o_list_46_Nil))))));
  }
  if ((!t2536) && (u_is_con(s2535, &n_syntax_46_Name) && u_is_con(u_field(s2535, 0), &n_option_46_None))) {
    u_value v_b = u_field(s2535, 1);
    static struct u_object l_const_32_struct_32_u_n_2539 = U_LIT("const struct u_name");
    static struct u_object l_n__2540 = U_LIT("n_");
    static struct u_object l_NULL_2541 = U_LIT("NULL");
    t2536 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2539), u_concat(&l_n__2540, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2541), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), &o_list_46_Nil))))));
  }
  if (!t2536) {
    u_no_case(s2535);
  }
  return t2536;
}

u_value f_compile_46_unit_decl_1(u_value v_n) {
  static struct u_object l_struct_32_u_object_2542 = U_LIT("struct u_object");
  static struct u_object l_o__2543 = U_LIT("o_");
  static struct u_object l_U_UNIT_2544 = U_LIT("U_UNIT");
  static struct u_object l_n__2545 = U_LIT("n_");
  return u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_object_2542), u_concat(&l_o__2543, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_UNIT_2544, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__2545, f_compile_46_mangle_1(v_n))), &o_list_46_Nil))));
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s2546 = v_cs;
  u_value t2547 = NULL;
  if ((!t2547) && u_is_con(s2546, &n_list_46_Nil)) {
    t2547 = &o_list_46_Nil;
  }
  if ((!t2547) && (u_is_con(s2546, &n_list_46_Cons) && u_is_con(u_field(s2546, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s2546, 0), 2);
    u_value v_rest = u_field(s2546, 1);
    t2547 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(v_ks, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t2547) && u_is_con(s2546, &n_list_46_Cons)) {
    u_value v_rest = u_field(s2546, 1);
    t2547 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t2547) {
    u_no_case(s2546);
  }
  return t2547;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s2548 = v_ks;
  u_value t2549 = NULL;
  if ((!t2549) && u_is_con(s2548, &n_list_46_Nil)) {
    t2549 = &o_list_46_Nil;
  }
  if ((!t2549) && u_is_con(s2548, &n_list_46_Cons)) {
    u_value v_k = u_field(s2548, 0);
    u_value v_rest = u_field(s2548, 1);
    if (u_truth(f_set_46_member_2(f_syntax_46_Constr_46_name_1(v_k), f_compile_46_runtime_constructors_0()))) {
      t2549 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    }
  }
  if ((!t2549) && (u_is_con(s2548, &n_list_46_Cons) && (u_is_con(u_field(s2548, 0), &n_syntax_46_Constr) && u_is_con(u_field(u_field(s2548, 0), 1), &n_list_46_Nil)))) {
    u_value v_n = u_field(u_field(s2548, 0), 0);
    u_value v_rest = u_field(s2548, 1);
    t2549 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_n, v_i), u_mk_con(&n_list_46_Cons, 2, f_compile_46_unit_decl_1(v_n), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)))));
  }
  if ((!t2549) && (u_is_con(s2548, &n_list_46_Cons) && u_is_con(u_field(s2548, 0), &n_syntax_46_Constr))) {
    u_value v_n = u_field(u_field(s2548, 0), 0);
    u_value v_rest = u_field(s2548, 1);
    t2549 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_n, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t2549) {
    u_no_case(s2548);
  }
  return t2549;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t2550 = &o_list_46_Nil;
  u_value *e2551 = &t2550;
  u_value l2552 = v_xs;
  while (u_is_con(l2552, &n_list_46_Cons)) {
    u_value v_x = u_field(l2552, 0);
    if (u_truth(u_mk_bool(1))) {
      e2551 = u_append(e2551, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l2552 = u_field(l2552, 1);
  }
  return t2550;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__2553 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__2553, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s2554 = f_list_46_lookup_2(v_x, v_env);
  u_value t2555 = NULL;
  if ((!t2555) && u_is_con(s2554, &n_option_46_Some)) {
    u_value v_c = u_field(s2554, 0);
    t2555 = v_c;
  }
  if (!t2555) {
    t2555 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t2555) {
    u_no_case(s2554);
  }
  return t2555;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s2556 = v_c;
  u_value t2557 = NULL;
  if ((!t2557) && (u_is_con(s2556, &n_c_46_CeUnary) && u_equals(u_field(s2556, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s2556, 1);
    t2557 = v_p;
  }
  if (!t2557) {
    static struct u_object l__38__2558 = U_LIT("&");
    t2557 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2558, v_c);
  }
  if (!t2557) {
    u_no_case(s2556);
  }
  return t2557;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s2559 = v_t;
  u_value t2560 = NULL;
  if ((!t2560) && (u_is_con(s2559, &n_syntax_46_TSort) && u_is_con(u_field(s2559, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s2559, 0);
    t2560 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t2560) && u_is_con(s2559, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s2559, 0);
    u_value v_args = u_field(s2559, 1);
    static struct u_object l__91__2561 = U_LIT("[");
    static struct u_object l__44__32__2562 = U_LIT(", ");
    u_value t2563 = &o_list_46_Nil;
    u_value *e2564 = &t2563;
    u_value l2565 = v_args;
    while (u_is_con(l2565, &n_list_46_Cons)) {
      u_value v_a = u_field(l2565, 0);
      if (u_truth(u_mk_bool(1))) {
        e2564 = u_append(e2564, f_compile_46_type_key_1(v_a));
      }
      l2565 = u_field(l2565, 1);
    }
    static struct u_object l__93__2566 = U_LIT("]");
    t2560 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__2561, u_concat(u_str_join(&l__44__32__2562, t2563), &l__93__2566)));
  }
  if ((!t2560) && u_is_con(s2559, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s2559, 0);
    u_value v_r = u_field(s2559, 1);
    static struct u_object l__40__2567 = U_LIT("(");
    static struct u_object l__44__32__2568 = U_LIT(", ");
    u_value t2569 = &o_list_46_Nil;
    u_value *e2570 = &t2569;
    u_value l2571 = v_args;
    while (u_is_con(l2571, &n_list_46_Cons)) {
      u_value v_a = u_field(l2571, 0);
      if (u_truth(u_mk_bool(1))) {
        e2570 = u_append(e2570, f_compile_46_type_key_1(v_a));
      }
      l2571 = u_field(l2571, 1);
    }
    static struct u_object l__41__32__45__62__32__2572 = U_LIT(") -> ");
    t2560 = u_concat(&l__40__2567, u_concat(u_str_join(&l__44__32__2568, t2569), u_concat(&l__41__32__45__62__32__2572, f_compile_46_type_key_1(v_r))));
  }
  if ((!t2560) && u_is_con(s2559, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s2559, 0);
    u_value v_os = u_field(s2559, 1);
    u_value v_rs = u_field(s2559, 2);
    static struct u_object l__40_in_32__2573 = U_LIT("(in ");
    static struct u_object l__44__32__2574 = U_LIT(", ");
    u_value t2575 = &o_list_46_Nil;
    u_value *e2576 = &t2575;
    u_value l2577 = v_is;
    while (u_is_con(l2577, &n_list_46_Cons)) {
      u_value v_a = u_field(l2577, 0);
      if (u_truth(u_mk_bool(1))) {
        e2576 = u_append(e2576, f_compile_46_type_key_1(v_a));
      }
      l2577 = u_field(l2577, 1);
    }
    static struct u_object l__44__32_out_32__2578 = U_LIT(", out ");
    static struct u_object l__44__32__2579 = U_LIT(", ");
    u_value t2580 = &o_list_46_Nil;
    u_value *e2581 = &t2580;
    u_value l2582 = v_os;
    while (u_is_con(l2582, &n_list_46_Cons)) {
      u_value v_a = u_field(l2582, 0);
      if (u_truth(u_mk_bool(1))) {
        e2581 = u_append(e2581, f_compile_46_type_key_1(v_a));
      }
      l2582 = u_field(l2582, 1);
    }
    static struct u_object l__44__32_ref_32__2583 = U_LIT(", ref ");
    static struct u_object l__44__32__2584 = U_LIT(", ");
    u_value t2585 = &o_list_46_Nil;
    u_value *e2586 = &t2585;
    u_value l2587 = v_rs;
    while (u_is_con(l2587, &n_list_46_Cons)) {
      u_value v_a = u_field(l2587, 0);
      if (u_truth(u_mk_bool(1))) {
        e2586 = u_append(e2586, f_compile_46_type_key_1(v_a));
      }
      l2587 = u_field(l2587, 1);
    }
    static struct u_object l__41__2588 = U_LIT(")");
    t2560 = u_concat(&l__40_in_32__2573, u_concat(u_str_join(&l__44__32__2574, t2575), u_concat(&l__44__32_out_32__2578, u_concat(u_str_join(&l__44__32__2579, t2580), u_concat(&l__44__32_ref_32__2583, u_concat(u_str_join(&l__44__32__2584, t2585), &l__41__2588))))));
  }
  if ((!t2560) && u_is_con(s2559, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s2559, 0);
    static struct u_object l__40__2589 = U_LIT("(");
    static struct u_object l__32__42__32__2590 = U_LIT(" * ");
    u_value t2591 = &o_list_46_Nil;
    u_value *e2592 = &t2591;
    u_value l2593 = v_ts;
    while (u_is_con(l2593, &n_list_46_Cons)) {
      u_value v_a = u_field(l2593, 0);
      if (u_truth(u_mk_bool(1))) {
        e2592 = u_append(e2592, f_compile_46_type_key_1(v_a));
      }
      l2593 = u_field(l2593, 1);
    }
    static struct u_object l__41__2594 = U_LIT(")");
    t2560 = u_concat(&l__40__2589, u_concat(u_str_join(&l__32__42__32__2590, t2591), &l__41__2594));
  }
  if (!t2560) {
    static struct u_object l__63__2595 = U_LIT("?");
    t2560 = &l__63__2595;
  }
  if (!t2560) {
    u_no_case(s2559);
  }
  return t2560;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t2596 = &o_list_46_Nil;
  u_value *e2597 = &t2596;
  u_value l2598 = v_cs;
  while (u_is_con(l2598, &n_list_46_Cons)) {
    u_value v_c = u_field(l2598, 0);
    if (u_truth(u_mk_bool(1))) {
      e2597 = u_append(e2597, f_compile_46_fun_arities_1(v_c));
    }
    l2598 = u_field(l2598, 1);
  }
  u_value t2599 = &o_list_46_Nil;
  u_value *e2600 = &t2599;
  u_value l2601 = v_cs;
  while (u_is_con(l2601, &n_list_46_Cons)) {
    u_value v_c = u_field(l2601, 0);
    if (u_truth(u_mk_bool(1))) {
      e2600 = u_append(e2600, f_compile_46_constr_arities_1(v_c));
    }
    l2601 = u_field(l2601, 1);
  }
  u_value t2606 = f_list_46_concat_1(t2596);
  u_value t2607 = f_list_46_concat_1(t2599);
  u_value t2605;
  {
    u_value v_fs = t2606;
    u_value v_ks = t2607;
    u_value t2602 = &o_list_46_Nil;
    u_value *e2603 = &t2602;
    u_value l2604 = v_cs;
    while (u_is_con(l2604, &n_list_46_Cons)) {
      u_value v_c = u_field(l2604, 0);
      if (u_truth(u_mk_bool(1))) {
        e2603 = u_append(e2603, f_compile_46_proc_arities_1(v_c));
      }
      l2604 = u_field(l2604, 1);
    }
    t2605 = u_mk_con(&n_compile_46_Globals, 5, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()), f_map_46_put_all_2(f_list_46_concat_1(t2602), f_map_46_empty_0()));
  }
  return t2605;
}

u_value f_compile_46_proc_arities_1(u_value v_c) {
  u_value s2608 = v_c;
  u_value t2609 = NULL;
  if ((!t2609) && u_is_con(s2608, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s2608, 0);
    u_value v_ins = u_field(s2608, 2);
    u_value v_outs = u_field(s2608, 3);
    u_value v_refs = u_field(s2608, 4);
    t2609 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, f_list_46_length_1(v_ins), u_add(f_list_46_length_1(v_outs), f_list_46_length_1(v_refs)))), &o_list_46_Nil);
  }
  if (!t2609) {
    t2609 = &o_list_46_Nil;
  }
  if (!t2609) {
    u_no_case(s2608);
  }
  return t2609;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s2610 = v_fs;
  u_value t2611 = NULL;
  if ((!t2611) && u_is_con(s2610, &n_list_46_Nil)) {
    t2611 = v_m;
  }
  if ((!t2611) && u_is_con(s2610, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s2610, 0), 0);
    u_value v_k = u_field(u_field(s2610, 0), 1);
    u_value v_rest = u_field(s2610, 1);
    u_value s2612 = f_map_46_get_2(v_f, v_m);
    u_value t2613 = NULL;
    if ((!t2613) && u_is_con(s2612, &n_option_46_Some)) {
      u_value v_ks = u_field(s2612, 0);
      t2613 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t2613) && u_is_con(s2612, &n_option_46_None)) {
      t2613 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, &o_list_46_Nil), v_m));
    }
    if (!t2613) {
      u_no_case(s2612);
    }
    t2611 = t2613;
  }
  if (!t2611) {
    u_no_case(s2610);
  }
  return t2611;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s2614 = v_gl;
  u_value t2615 = NULL;
  if ((!t2615) && u_is_con(s2614, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s2614, 2);
    u_value s2616 = f_map_46_get_2(v_n, v_fm);
    u_value t2617 = NULL;
    if ((!t2617) && u_is_con(s2616, &n_option_46_Some)) {
      u_value v_ks = u_field(s2616, 0);
      t2617 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t2617) && u_is_con(s2616, &n_option_46_None)) {
      t2617 = u_mk_bool(0);
    }
    if (!t2617) {
      u_no_case(s2616);
    }
    t2615 = t2617;
  }
  if (!t2615) {
    u_no_case(s2614);
  }
  return t2615;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s2618 = v_gl;
  u_value t2619 = NULL;
  if ((!t2619) && u_is_con(s2618, &n_compile_46_Globals)) {
    u_value v_km = u_field(s2618, 3);
    t2619 = f_map_46_get_2(v_n, v_km);
  }
  if (!t2619) {
    u_no_case(s2618);
  }
  return t2619;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s2620 = v_c;
  u_value t2621 = NULL;
  if ((!t2621) && u_is_con(s2620, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s2620, 0);
    u_value v_xss = u_field(s2620, 2);
    t2621 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), &o_list_46_Nil);
  }
  if (!t2621) {
    t2621 = &o_list_46_Nil;
  }
  if (!t2621) {
    u_no_case(s2620);
  }
  return t2621;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s2622 = v_c;
  u_value t2623 = NULL;
  if ((!t2623) && u_is_con(s2622, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s2622, 2);
    u_value t2624 = &o_list_46_Nil;
    u_value *e2625 = &t2624;
    u_value l2626 = v_ks;
    while (u_is_con(l2626, &n_list_46_Cons)) {
      u_value v_k = u_field(l2626, 0);
      if (u_truth(u_mk_bool(1))) {
        e2625 = u_append(e2625, f_compile_46_constr_arity_1(v_k));
      }
      l2626 = u_field(l2626, 1);
    }
    t2623 = t2624;
  }
  if (!t2623) {
    t2623 = &o_list_46_Nil;
  }
  if (!t2623) {
    u_no_case(s2622);
  }
  return t2623;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s2627 = v_k;
  u_value t2628 = NULL;
  if ((!t2628) && u_is_con(s2627, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s2627, 0);
    u_value v_ts = u_field(s2627, 1);
    t2628 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t2628) {
    u_no_case(s2627);
  }
  return t2628;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s2629 = v_xs;
  u_value t2630 = NULL;
  if ((!t2630) && u_is_con(s2629, &n_list_46_Nil)) {
    t2630 = &o_list_46_Nil;
  }
  if ((!t2630) && u_is_con(s2629, &n_list_46_Cons)) {
    u_value v_x = u_field(s2629, 0);
    u_value v_rest = u_field(s2629, 1);
    t2630 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t2630) {
    u_no_case(s2629);
  }
  return t2630;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_2631 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_2631, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), &o_list_46_Nil)));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_2632 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_2632, u_mk_con(&n_list_46_Cons, 2, v_v, &o_list_46_Nil));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s2633 = v_cs;
  u_value t2634 = NULL;
  if ((!t2634) && u_is_con(s2633, &n_list_46_Nil)) {
    t2634 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t2634) && (u_is_con(s2633, &n_list_46_Cons) && u_is_con(u_field(s2633, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s2633, 0);
    t2634 = v_d;
  }
  if ((!t2634) && u_is_con(s2633, &n_list_46_Cons)) {
    u_value v_d = u_field(s2633, 0);
    u_value v_rest = u_field(s2633, 1);
    static struct u_object l__38__38__2635 = U_LIT("&&");
    t2634 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__2635, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t2634) {
    u_no_case(s2633);
  }
  return t2634;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s2636 = v_l;
  u_value t2637 = NULL;
  if ((!t2637) && u_is_con(s2636, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_2638 = U_LIT("u_mk_tuple");
    t2637 = f_c_46_c_call_2(&l_u_mk_tuple_2638, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), &o_list_46_Nil));
  }
  if ((!t2637) && u_is_con(s2636, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s2636, 0);
    static struct u_object l_u_mk_bool_2639 = U_LIT("u_mk_bool");
    u_value t2640;
    if (u_truth(v_b)) {
      t2640 = u_mk_int(1);
    } else {
      t2640 = u_mk_int(0);
    }
    t2637 = f_c_46_c_call_2(&l_u_mk_bool_2639, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t2640), &o_list_46_Nil));
  }
  if ((!t2637) && u_is_con(s2636, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s2636, 0);
    u_value t2641 = u_lt(u_mk_int_str("-1000000000000000000"), v_n);
    if (u_truth(t2641)) {
      t2641 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t2644;
    if (u_truth(t2641)) {
      static struct u_object l_u_mk_int_2642 = U_LIT("u_mk_int");
      t2644 = f_c_46_c_call_2(&l_u_mk_int_2642, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), &o_list_46_Nil));
    } else {
      static struct u_object l_u_mk_int_str_2643 = U_LIT("u_mk_int_str");
      t2644 = f_c_46_c_call_2(&l_u_mk_int_str_2643, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), &o_list_46_Nil));
    }
    t2637 = t2644;
  }
  if ((!t2637) && u_is_con(s2636, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s2636, 0);
    static struct u_object l_u_mk_lit_2645 = U_LIT("u_mk_lit");
    t2637 = f_c_46_c_call_2(&l_u_mk_lit_2645, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), &o_list_46_Nil));
  }
  if ((!t2637) && u_is_con(s2636, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s2636, 0);
    static struct u_object l_u_mk_char_2646 = U_LIT("u_mk_char");
    t2637 = f_c_46_c_call_2(&l_u_mk_char_2646, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), &o_list_46_Nil));
  }
  if (!t2637) {
    u_no_case(s2636);
  }
  return t2637;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s2647 = v_p;
  u_value t2648 = NULL;
  if ((!t2648) && u_is_con(s2647, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s2647, 0);
    static struct u_object l_u_equals_2649 = U_LIT("u_equals");
    t2648 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_2649, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), &o_list_46_Nil))), &o_list_46_Nil);
  }
  if ((!t2648) && u_is_con(s2647, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s2647, 0);
    u_value v_ps = u_field(s2647, 1);
    static struct u_object l_u_is_con_2650 = U_LIT("u_is_con");
    t2648 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_2650, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), &o_list_46_Nil))), &o_list_46_Nil), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t2648) && u_is_con(s2647, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2647, 0);
    t2648 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2648) {
    t2648 = &o_list_46_Nil;
  }
  if (!t2648) {
    u_no_case(s2647);
  }
  return t2648;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s2651 = v_ps;
  u_value t2652 = NULL;
  if ((!t2652) && u_is_con(s2651, &n_list_46_Nil)) {
    t2652 = &o_list_46_Nil;
  }
  if ((!t2652) && u_is_con(s2651, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2651, 0), 0);
    u_value v_i = u_field(u_field(s2651, 0), 1);
    u_value v_rest = u_field(s2651, 1);
    t2652 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t2652) {
    u_no_case(s2651);
  }
  return t2652;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s2653 = v_p;
  u_value t2654 = NULL;
  if ((!t2654) && (u_is_con(s2653, &n_syntax_46_PVar) && u_is_con(u_field(s2653, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2653, 0), 0);
    t2654 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), &o_list_46_Nil);
  }
  if ((!t2654) && u_is_con(s2653, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2653, 1);
    t2654 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t2654) && u_is_con(s2653, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2653, 0);
    t2654 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2654) {
    t2654 = &o_list_46_Nil;
  }
  if (!t2654) {
    u_no_case(s2653);
  }
  return t2654;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s2655 = v_ps;
  u_value t2656 = NULL;
  if ((!t2656) && u_is_con(s2655, &n_list_46_Nil)) {
    t2656 = &o_list_46_Nil;
  }
  if ((!t2656) && u_is_con(s2655, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2655, 0), 0);
    u_value v_i = u_field(u_field(s2655, 0), 1);
    u_value v_rest = u_field(s2655, 1);
    t2656 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t2656) {
    u_no_case(s2655);
  }
  return t2656;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s2657 = v_p;
  u_value t2658 = NULL;
  if ((!t2658) && (u_is_con(s2657, &n_syntax_46_PVar) && u_is_con(u_field(s2657, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2657, 0), 0);
    t2658 = u_mk_con(&n_list_46_Cons, 2, v_n, &o_list_46_Nil);
  }
  if ((!t2658) && u_is_con(s2657, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2657, 1);
    u_value t2659 = &o_list_46_Nil;
    u_value *e2660 = &t2659;
    u_value l2661 = v_ps;
    while (u_is_con(l2661, &n_list_46_Cons)) {
      u_value v_q = u_field(l2661, 0);
      if (u_truth(u_mk_bool(1))) {
        e2660 = u_append(e2660, f_compile_46_pattern_names_1(v_q));
      }
      l2661 = u_field(l2661, 1);
    }
    t2658 = f_list_46_concat_1(t2659);
  }
  if ((!t2658) && u_is_con(s2657, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2657, 0);
    u_value t2662 = &o_list_46_Nil;
    u_value *e2663 = &t2662;
    u_value l2664 = v_ps;
    while (u_is_con(l2664, &n_list_46_Cons)) {
      u_value v_q = u_field(l2664, 0);
      if (u_truth(u_mk_bool(1))) {
        e2663 = u_append(e2663, f_compile_46_pattern_names_1(v_q));
      }
      l2664 = u_field(l2664, 1);
    }
    t2658 = f_list_46_concat_1(t2662);
  }
  if (!t2658) {
    t2658 = &o_list_46_Nil;
  }
  if (!t2658) {
    u_no_case(s2657);
  }
  return t2658;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s2665 = v_e;
  u_value t2666 = NULL;
  if ((!t2666) && u_is_con(s2665, &n_syntax_46_ELit)) {
    t2666 = &o_list_46_Nil;
  }
  if ((!t2666) && (u_is_con(s2665, &n_syntax_46_EVar) && u_is_con(u_field(s2665, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2665, 0), 0);
    u_value t2667;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t2667 = &o_list_46_Nil;
    } else {
      t2667 = u_mk_con(&n_list_46_Cons, 2, v_n, &o_list_46_Nil);
    }
    t2666 = t2667;
  }
  if ((!t2666) && u_is_con(s2665, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s2665, 0);
    u_value v_args = u_field(s2665, 1);
    u_value t2668 = &o_list_46_Nil;
    u_value *e2669 = &t2668;
    u_value l2670 = v_args;
    while (u_is_con(l2670, &n_list_46_Cons)) {
      u_value v_a = u_field(l2670, 0);
      if (u_truth(u_mk_bool(1))) {
        e2669 = u_append(e2669, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2670 = u_field(l2670, 1);
    }
    t2666 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t2668));
  }
  if ((!t2666) && u_is_con(s2665, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s2665, 0);
    u_value v_b = u_field(s2665, 1);
    t2666 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t2666) && u_is_con(s2665, &n_syntax_46_EProc)) {
    u_value v_is = u_field(s2665, 0);
    u_value v_os = u_field(s2665, 1);
    u_value v_rs = u_field(s2665, 2);
    u_value v_b = u_field(s2665, 3);
    t2666 = f_compile_46_free_in_stmt_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), v_bound));
  }
  if ((!t2666) && u_is_con(s2665, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s2665, 0);
    u_value v_b = u_field(s2665, 1);
    u_value t2671 = &o_list_46_Nil;
    u_value *e2672 = &t2671;
    u_value l2673 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l2673, &n_list_46_Cons)) {
      u_value v_a = u_field(l2673, 0);
      if (u_truth(u_mk_bool(1))) {
        e2672 = u_append(e2672, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2673 = u_field(l2673, 1);
    }
    t2666 = f_list_46__43__43__2(f_list_46_concat_1(t2671), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t2666) && u_is_con(s2665, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s2665, 0);
    u_value v_l = u_field(s2665, 1);
    u_value v_r = u_field(s2665, 2);
    t2666 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t2666) && u_is_con(s2665, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s2665, 0);
    u_value v_cs = u_field(s2665, 1);
    u_value t2674 = &o_list_46_Nil;
    u_value *e2675 = &t2674;
    u_value l2676 = v_cs;
    while (u_is_con(l2676, &n_list_46_Cons)) {
      u_value v_b = u_field(l2676, 0);
      if (u_truth(u_mk_bool(1))) {
        e2675 = u_append(e2675, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l2676 = u_field(l2676, 1);
    }
    t2666 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2674));
  }
  if ((!t2666) && u_is_con(s2665, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s2665, 0);
    u_value v_xs = u_field(s2665, 1);
    u_value v_ls = u_field(s2665, 2);
    u_value v_c = u_field(s2665, 3);
    u_value t2677 = &o_list_46_Nil;
    u_value *e2678 = &t2677;
    u_value l2679 = v_ls;
    while (u_is_con(l2679, &n_list_46_Cons)) {
      u_value v_l = u_field(l2679, 0);
      if (u_truth(u_mk_bool(1))) {
        e2678 = u_append(e2678, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2679 = u_field(l2679, 1);
    }
    t2666 = f_list_46__43__43__2(f_list_46_concat_1(t2677), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t2666) && u_is_con(s2665, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s2665, 0);
    u_value t2680 = &o_list_46_Nil;
    u_value *e2681 = &t2680;
    u_value l2682 = v_es;
    while (u_is_con(l2682, &n_list_46_Cons)) {
      u_value v_a = u_field(l2682, 0);
      if (u_truth(u_mk_bool(1))) {
        e2681 = u_append(e2681, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2682 = u_field(l2682, 1);
    }
    t2666 = f_list_46_concat_1(t2680);
  }
  if ((!t2666) && u_is_con(s2665, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s2665, 0);
    t2666 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if (!t2666) {
    u_no_case(s2665);
  }
  return t2666;
}

u_value f_compile_46_free_in_stmt_2(u_value v_x, u_value v_bound) {
  u_value s2683 = v_x;
  u_value t2684 = NULL;
  if ((!t2684) && u_is_con(s2683, &n_syntax_46_SSkip)) {
    t2684 = &o_list_46_Nil;
  }
  if ((!t2684) && u_is_con(s2683, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s2683, 0);
    u_value t2685 = &o_list_46_Nil;
    u_value *e2686 = &t2685;
    u_value l2687 = f_syntax_46_names_1(f_list_46_firsts_1(v_ps));
    while (u_is_con(l2687, &n_list_46_Cons)) {
      u_value v_y = u_field(l2687, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2686 = u_append(e2686, v_y);
      }
      l2687 = u_field(l2687, 1);
    }
    u_value t2688 = &o_list_46_Nil;
    u_value *e2689 = &t2688;
    u_value l2690 = f_list_46_seconds_1(v_ps);
    while (u_is_con(l2690, &n_list_46_Cons)) {
      u_value v_a = u_field(l2690, 0);
      if (u_truth(u_mk_bool(1))) {
        e2689 = u_append(e2689, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2690 = u_field(l2690, 1);
    }
    t2684 = f_list_46__43__43__2(t2685, f_list_46_concat_1(t2688));
  }
  if ((!t2684) && u_is_con(s2683, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s2683, 0);
    u_value v_ys = u_field(s2683, 1);
    u_value t2691 = &o_list_46_Nil;
    u_value *e2692 = &t2691;
    u_value l2693 = v_ys;
    while (u_is_con(l2693, &n_list_46_Cons)) {
      u_value v_y = u_field(l2693, 0);
      if (u_truth(u_mk_bool(1))) {
        e2692 = u_append(e2692, f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2693 = u_field(l2693, 1);
    }
    t2684 = f_list_46_concat_1(t2691);
  }
  if ((!t2684) && u_is_con(s2683, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s2683, 0);
    u_value v_l = u_field(s2683, 1);
    u_value v_r = u_field(s2683, 2);
    t2684 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_l, v_bound), f_compile_46_free_in_stmt_2(v_r, v_bound)));
  }
  if ((!t2684) && u_is_con(s2683, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s2683, 0);
    u_value v_b = u_field(s2683, 1);
    t2684 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_compile_46_free_in_stmt_2(v_b, v_bound));
  }
  if ((!t2684) && u_is_con(s2683, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s2683, 0);
    u_value v_ins = u_field(s2683, 1);
    u_value v_outs = u_field(s2683, 2);
    u_value v_refs = u_field(s2683, 3);
    u_value t2694 = &o_list_46_Nil;
    u_value *e2695 = &t2694;
    u_value l2696 = u_mk_con(&n_list_46_Cons, 2, v_p, f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
    while (u_is_con(l2696, &n_list_46_Cons)) {
      u_value v_y = u_field(l2696, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2695 = u_append(e2695, v_y);
      }
      l2696 = u_field(l2696, 1);
    }
    u_value t2697 = &o_list_46_Nil;
    u_value *e2698 = &t2697;
    u_value l2699 = v_ins;
    while (u_is_con(l2699, &n_list_46_Cons)) {
      u_value v_a = u_field(l2699, 0);
      if (u_truth(u_mk_bool(1))) {
        e2698 = u_append(e2698, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2699 = u_field(l2699, 1);
    }
    t2684 = f_list_46__43__43__2(t2694, f_list_46_concat_1(t2697));
  }
  if ((!t2684) && u_is_con(s2683, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s2683, 0);
    u_value v_bs = u_field(s2683, 1);
    u_value t2700 = &o_list_46_Nil;
    u_value *e2701 = &t2700;
    u_value l2702 = v_bs;
    while (u_is_con(l2702, &n_list_46_Cons)) {
      u_value v_b = u_field(l2702, 0);
      if (u_truth(u_mk_bool(1))) {
        e2701 = u_append(e2701, f_compile_46_free_in_stmt_branch_2(v_b, v_bound));
      }
      l2702 = u_field(l2702, 1);
    }
    t2684 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2700));
  }
  if ((!t2684) && u_is_con(s2683, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s2683, 0);
    t2684 = f_compile_46_free_vars_2(v_c, v_bound);
  }
  if ((!t2684) && u_is_con(s2683, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s2683, 0);
    t2684 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if ((!t2684) && u_is_con(s2683, &n_syntax_46_STry)) {
    u_value v_b = u_field(s2683, 0);
    u_value v_hs = u_field(s2683, 1);
    u_value t2703 = &o_list_46_Nil;
    u_value *e2704 = &t2703;
    u_value l2705 = v_hs;
    while (u_is_con(l2705, &n_list_46_Cons)) {
      u_value v_h = u_field(l2705, 0);
      if (u_truth(u_mk_bool(1))) {
        e2704 = u_append(e2704, f_compile_46_free_in_handler_2(v_h, v_bound));
      }
      l2705 = u_field(l2705, 1);
    }
    t2684 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_b, v_bound), f_list_46_concat_1(t2703));
  }
  if (!t2684) {
    u_no_case(s2683);
  }
  return t2684;
}

u_value f_compile_46_free_in_stmt_branch_2(u_value v_b, u_value v_bound) {
  u_value s2706 = v_b;
  u_value t2707 = NULL;
  if ((!t2707) && (u_is_con(s2706, &n_syntax_46_Branch) && u_is_con(u_field(s2706, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2706, 0);
    u_value v_g = u_field(u_field(s2706, 1), 0);
    u_value v_y = u_field(s2706, 2);
    t2707 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2707) && (u_is_con(s2706, &n_syntax_46_Branch) && u_is_con(u_field(s2706, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2706, 0);
    u_value v_y = u_field(s2706, 2);
    t2707 = f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2707) {
    u_no_case(s2706);
  }
  return t2707;
}

u_value f_compile_46_free_in_handler_2(u_value v_h, u_value v_bound) {
  u_value t2711 = f_list_46__43__43__2(f_compile_46_pattern_names_1(f_syntax_46_Handler_46_pat_1(v_h)), v_bound);
  u_value t2710;
  {
    u_value v_inner = t2711;
    u_value s2708 = f_syntax_46_Handler_46_guard_1(v_h);
    u_value t2709 = NULL;
    if ((!t2709) && u_is_con(s2708, &n_option_46_Some)) {
      u_value v_g = u_field(s2708, 0);
      t2709 = f_compile_46_free_vars_2(v_g, v_inner);
    }
    if ((!t2709) && u_is_con(s2708, &n_option_46_None)) {
      t2709 = &o_list_46_Nil;
    }
    if (!t2709) {
      u_no_case(s2708);
    }
    t2710 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(f_syntax_46_Handler_46_body_1(v_h), v_inner), t2709);
  }
  return t2710;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s2712 = v_b;
  u_value t2713 = NULL;
  if ((!t2713) && (u_is_con(s2712, &n_syntax_46_Branch) && u_is_con(u_field(s2712, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2712, 0);
    u_value v_g = u_field(u_field(s2712, 1), 0);
    u_value v_e = u_field(s2712, 2);
    t2713 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2713) && (u_is_con(s2712, &n_syntax_46_Branch) && u_is_con(u_field(s2712, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2712, 0);
    u_value v_e = u_field(s2712, 2);
    t2713 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2713) {
    u_no_case(s2712);
  }
  return t2713;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s2714 = v_xss;
  u_value t2715 = NULL;
  if ((!t2715) && u_is_con(s2714, &n_list_46_Nil)) {
    t2715 = v_b;
  }
  if ((!t2715) && u_is_con(s2714, &n_list_46_Cons)) {
    u_value v_xs = u_field(s2714, 0);
    u_value v_rest = u_field(s2714, 1);
    t2715 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t2715) {
    u_no_case(s2714);
  }
  return t2715;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value t2716 = u_concat(v_stem, u_str_show(f_compile_46_Gen_46_count_1(*r_g)));
    u_value t2717 = f_compile_46_Gen_46_count_58__61__2(*r_g, u_add(f_compile_46_Gen_46_count_1(*r_g), u_mk_int(1)));
    *r_x = t2716;
    *r_g = t2717;
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value t2718 = f_compile_46_Gen_46_lifted_58__61__2(*r_g, f_list_46__43__43__2(f_compile_46_Gen_46_lifted_1(*r_g), u_mk_con(&n_list_46_Cons, 2, v_d, &o_list_46_Nil)));
    *r_g = t2718;
  }
}

u_value f_compile_46_global_value_3(u_value v_gl, u_value v_n, u_value v_t) {
  u_value t2732;
  if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
    static struct u_object l_u_mk_closure_2719 = U_LIT("u_mk_closure");
    static struct u_object l_w__2720 = U_LIT("w_");
    t2732 = f_c_46_c_call_2(&l_u_mk_closure_2719, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__2720, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), &o_list_46_Nil)));
  } else {
    u_value s2721 = f_compile_46_fields_count_2(v_gl, v_n);
    u_value t2722 = NULL;
    if ((!t2722) && (u_is_con(s2721, &n_option_46_Some) && u_equals(u_field(s2721, 0), u_mk_int(0)))) {
      t2722 = f_compile_46_unit_value_1(v_n);
    }
    if ((!t2722) && u_is_con(s2721, &n_option_46_Some)) {
      static struct u_object l_u_mk_closure_2723 = U_LIT("u_mk_closure");
      static struct u_object l_wc__2724 = U_LIT("wc_");
      t2722 = f_c_46_c_call_2(&l_u_mk_closure_2723, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__2724, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), &o_list_46_Nil)));
    }
    if ((!t2722) && u_is_con(s2721, &n_option_46_None)) {
      u_value s2725 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
      u_value t2726 = NULL;
      if ((!t2726) && u_is_con(s2725, &n_option_46_Some)) {
        u_value v_f = u_field(u_field(s2725, 0), 0);
        static struct u_object l_u_mk_closure_2727 = U_LIT("u_mk_closure");
        static struct u_object l_w_2728 = U_LIT("w");
        t2726 = f_c_46_c_call_2(&l_u_mk_closure_2727, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_2728, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), &o_list_46_Nil)));
      }
      if ((!t2726) && u_is_con(s2725, &n_option_46_None)) {
        if (u_truth(f_map_46_has_2(v_n, f_compile_46_Globals_46_procs_1(v_gl)))) {
          static struct u_object l_u_mk_proc_2729 = U_LIT("u_mk_proc");
          static struct u_object l_wp__2730 = U_LIT("wp_");
          t2726 = f_c_46_c_call_2(&l_u_mk_proc_2729, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wp__2730, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), &o_list_46_Nil)));
        }
      }
      if ((!t2726) && u_is_con(s2725, &n_option_46_None)) {
        static struct u_object l_the_32_variable_32__2731 = U_LIT("the variable ");
        u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__2731, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
        t2726 = NULL;
      }
      if (!t2726) {
        u_no_case(s2725);
      }
      t2722 = t2726;
    }
    if (!t2722) {
      u_no_case(s2721);
    }
    t2732 = t2722;
  }
  return t2732;
}

void p_compile_46_compile_expr(u_value v_gl, u_value v_env, u_value v_e, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value v_cs = NULL;
    u_value v_ra = NULL;
    u_value v_rb = NULL;
    u_value v_rc = NULL;
    u_value v_rs = NULL;
    u_value v_t = NULL;
    u_value v_d = NULL;
    u_value s2733 = v_e;
    int d2734 = 0;
    if ((!d2734) && (u_is_con(s2733, &n_syntax_46_ELit) && u_is_con(u_field(s2733, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s2733, 0), 0);
      d2734 = 1;
      {
        static struct u_object l_l__2735 = U_LIT("l_");
        u_value t2736;
        if (u_truth(u_lt(u_str_size(v_s), u_mk_int(16)))) {
          t2736 = u_str_size(v_s);
        } else {
          t2736 = u_mk_int(16);
        }
        static struct u_object l___2737 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__2735, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), t2736))), &l___2737)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__2738 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_2739 = U_LIT("U_LIT");
        static struct u_object l__38__2740 = U_LIT("&");
        u_value t2741 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__2738), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_2739, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), &o_list_46_Nil)))), &o_list_46_Nil);
        u_value t2742 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2740, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t2741;
        *r_r = t2742;
      }
    }
    if ((!d2734) && u_is_con(s2733, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s2733, 0);
      d2734 = 1;
      {
        u_value t2743 = &o_list_46_Nil;
        u_value t2744 = f_compile_46_literal_value_1(v_l);
        *r_ss = t2743;
        *r_r = t2744;
      }
    }
    if ((!d2734) && (u_is_con(s2733, &n_syntax_46_EVar) && u_is_con(u_field(s2733, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s2733, 0), 0);
      u_value v_ty = u_field(u_field(s2733, 0), 1);
      d2734 = 1;
      {
        u_value s2745 = f_list_46_lookup_2(v_n, v_env);
        int d2746 = 0;
        if ((!d2746) && u_is_con(s2745, &n_option_46_Some)) {
          u_value v_c = u_field(s2745, 0);
          d2746 = 1;
          {
            u_value t2747 = &o_list_46_Nil;
            u_value t2748 = v_c;
            *r_ss = t2747;
            *r_r = t2748;
          }
        }
        if ((!d2746) && u_is_con(s2745, &n_option_46_None)) {
          d2746 = 1;
          {
            u_value t2749 = &o_list_46_Nil;
            u_value t2750 = f_compile_46_global_value_3(v_gl, v_n, v_ty);
            *r_ss = t2749;
            *r_r = t2750;
          }
        }
        if (!d2746) {
          u_no_case(s2745);
        }
      }
    }
    if ((!d2734) && (u_is_con(s2733, &n_syntax_46_EApp) && (u_is_con(u_field(s2733, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s2733, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s2733, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s2733, 0), 0), 1);
      u_value v_args = u_field(s2733, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d2734 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d2734) && u_is_con(s2733, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s2733, 0);
      u_value v_args = u_field(s2733, 1);
      d2734 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_2751 = U_LIT("u_apply");
        u_value t2752 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2753 = f_c_46_c_call_2(&l_u_apply_2751, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), &o_list_46_Nil)), v_rs));
        *r_ss = t2752;
        *r_r = t2753;
      }
    }
    if ((!d2734) && u_is_con(s2733, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s2733, 0);
      u_value v_b = u_field(s2733, 1);
      d2734 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2734) && u_is_con(s2733, &n_syntax_46_EProc)) {
      u_value v_is = u_field(s2733, 0);
      u_value v_os = u_field(s2733, 1);
      u_value v_rs = u_field(s2733, 2);
      u_value v_b = u_field(s2733, 3);
      d2734 = 1;
      {
        p_compile_46_compile_proc_lambda(v_gl, v_env, v_is, f_list_46__43__43__2(v_os, v_rs), v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2734) && u_is_con(s2733, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s2733, 0);
      u_value v_b = u_field(s2733, 1);
      d2734 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2754 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2754, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2755 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, &o_option_46_None), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), &o_list_46_Nil)))), &o_list_46_Nil))));
        *r_ss = t2755;
        u_value t2756 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2756;
      }
    }
    if ((!d2734) && u_is_con(s2733, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s2733, 0);
      u_value v_l = u_field(s2733, 1);
      u_value v_rr = u_field(s2733, 2);
      d2734 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2757 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2757, &v_t, r_g);
        u_value t2758 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, &o_option_46_None), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), &o_list_46_Nil)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), &o_list_46_Nil))), &o_list_46_Nil)));
        *r_ss = t2758;
        u_value t2759 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2759;
      }
    }
    if ((!d2734) && u_is_con(s2733, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s2733, 0);
      u_value v_cases = u_field(s2733, 1);
      d2734 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2760 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2760, &v_d, r_g);
        static struct u_object l_t_2761 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2761, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_2762 = U_LIT("NULL");
        u_value t2763 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2762))), &o_list_46_Nil)), v_bs));
        *r_ss = t2763;
        static struct u_object l__33__2764 = U_LIT("!");
        static struct u_object l_u_no_case_2765 = U_LIT("u_no_case");
        u_value t2766 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2764, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2765, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &o_list_46_Nil))), &o_list_46_Nil), &o_list_46_Nil), &o_list_46_Nil));
        *r_ss = t2766;
        u_value t2767 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2767;
      }
    }
    if ((!d2734) && u_is_con(s2733, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s2733, 0);
      u_value v_xs = u_field(s2733, 1);
      u_value v_lists = u_field(s2733, 2);
      u_value v_c = u_field(s2733, 3);
      d2734 = 1;
      {
        static struct u_object l_t_2768 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2768, &v_t, r_g);
        static struct u_object l_e_2769 = U_LIT("e");
        p_compile_46_gen_name(&l_e_2769, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l__38__2770 = U_LIT("&");
        u_value t2771 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_compile_46_unit_value_1(f_syntax_46_nil_name_0()))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__2770, u_mk_con(&n_c_46_CeVar, 1, v_t)))), &o_list_46_Nil)), v_bs);
        *r_ss = t2771;
        u_value t2772 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2772;
      }
    }
    if ((!d2734) && u_is_con(s2733, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s2733, 0);
      d2734 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_2773 = U_LIT("u_mk_tuple");
        u_value t2774 = f_c_46_c_call_2(&l_u_mk_tuple_2773, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), &o_list_46_Nil), v_rs));
        *r_r = t2774;
      }
    }
    if ((!d2734) && u_is_con(s2733, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s2733, 0);
      u_value v_ty = u_field(s2733, 1);
      d2734 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2775 = U_LIT("u_raise");
        static struct u_object l_NULL_2776 = U_LIT("NULL");
        u_value t2777 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2775, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), &o_list_46_Nil)))), &o_list_46_Nil));
        u_value t2778 = u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2776);
        *r_ss = t2777;
        *r_r = t2778;
      }
    }
    if (!d2734) {
      u_no_case(s2733);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s2779 = v_es;
    int d2780 = 0;
    if ((!d2780) && u_is_con(s2779, &n_list_46_Nil)) {
      d2780 = 1;
      {
        u_value t2781 = &o_list_46_Nil;
        u_value t2782 = &o_list_46_Nil;
        *r_ss = t2781;
        *r_rs = t2782;
      }
    }
    if ((!d2780) && u_is_con(s2779, &n_list_46_Cons)) {
      u_value v_e = u_field(s2779, 0);
      u_value v_rest = u_field(s2779, 1);
      d2780 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t2783 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2784 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t2783;
        *r_rs = t2784;
      }
    }
    if (!d2780) {
      u_no_case(s2779);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s2785 = v_rs;
    int d2786 = 0;
    if ((!d2786) && u_is_con(s2785, &n_list_46_Nil)) {
      d2786 = 1;
      {
        u_value t2787 = &o_list_46_Nil;
        *r_ss = t2787;
      }
    }
    if ((!d2786) && u_is_con(s2785, &n_list_46_Cons)) {
      u_value v_r = u_field(s2785, 0);
      u_value v_rest = u_field(s2785, 1);
      d2786 = 1;
      {
        static struct u_object l_t_2788 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2788, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t2789 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t2789;
      }
    }
    if (!d2786) {
      u_no_case(s2785);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s2790 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2791 = NULL;
  if ((!t2791) && (u_is_con(u_field(s2790, 0), &n_list_46_Cons) && (u_is_con(u_field(s2790, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2790, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2790, 0), 0);
    u_value v_more = u_field(u_field(s2790, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2790, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2790, 1), 1);
    t2791 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t2791) {
    t2791 = &o_list_46_Nil;
  }
  if (!t2791) {
    u_no_case(s2790);
  }
  return t2791;
}

void p_compile_46_compile_named(u_value v_gl, u_value v_env, u_value v_n, u_value v_ty, u_value v_args, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value v_ra = NULL;
    u_value v_rb = NULL;
    u_value v_rs = NULL;
    u_value v_t = NULL;
    u_value s2792 = v_args;
    int d2793 = 0;
    if ((!d2793) && (u_is_con(s2792, &n_list_46_Cons) && (u_is_con(u_field(s2792, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2792, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s2792, 0);
      u_value v_b = u_field(u_field(s2792, 1), 0);
      static struct u_object l_and_2800 = U_LIT("and");
      u_value t2802 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2800));
      if (!u_truth(t2802)) {
        static struct u_object l_or_2801 = U_LIT("or");
        t2802 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_2801));
      }
      if (u_truth(t2802)) {
        d2793 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_2794 = U_LIT("t");
          p_compile_46_gen_name(&l_t_2794, &v_t, r_g);
          static struct u_object l_and_2795 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2795)))) {
            {
              u_value t2796 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), &o_list_46_Nil)), &o_list_46_Nil), &o_list_46_Nil)));
              *r_ss = t2796;
            }
          } else {
            {
              static struct u_object l__33__2797 = U_LIT("!");
              u_value t2798 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2797, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), &o_list_46_Nil)), &o_list_46_Nil), &o_list_46_Nil)));
              *r_ss = t2798;
            }
          }
          u_value t2799 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t2799;
        }
      }
    }
    if (!d2793) {
      d2793 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t2803 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t2803)) {
          t2803 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t2803)) {
          {
            u_value t2804 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t2804;
          }
        } else {
          u_value t2805 = u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, u_mk_int(0)));
          if (u_truth(t2805)) {
            t2805 = u_eq(v_args, &o_list_46_Nil);
          }
          if (u_truth(t2805)) {
            {
              u_value t2806 = f_compile_46_unit_value_1(v_n);
              *r_r = t2806;
            }
          } else {
            if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
              {
                static struct u_object l_u_mk_con_2807 = U_LIT("u_mk_con");
                u_value t2808 = f_c_46_c_call_2(&l_u_mk_con_2807, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), &o_list_46_Nil)), v_rs));
                *r_r = t2808;
              }
            } else {
              {
                u_value s2809 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
                int d2810 = 0;
                if ((!d2810) && u_is_con(s2809, &n_option_46_Some)) {
                  u_value v_b = u_field(u_field(s2809, 0), 0);
                  u_value v_k = u_field(u_field(s2809, 0), 1);
                  if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                    d2810 = 1;
                    {
                      u_value t2811 = f_c_46_c_call_2(v_b, v_rs);
                      *r_r = t2811;
                    }
                  }
                }
                if (!d2810) {
                  d2810 = 1;
                  {
                    static struct u_object l_u_apply_2812 = U_LIT("u_apply");
                    u_value t2813 = f_c_46_c_call_2(&l_u_apply_2812, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_global_value_3(v_gl, v_n, v_ty), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), &o_list_46_Nil)), v_rs));
                    *r_r = t2813;
                  }
                }
                if (!d2810) {
                  u_no_case(s2809);
                }
              }
            }
          }
        }
      }
    }
    if (!d2793) {
      u_no_case(s2792);
    }
  }
}

void p_compile_46_compile_lambda(u_value v_gl, u_value v_env, u_value v_xs, u_value v_b, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_caps = NULL;
    u_value v_f = NULL;
    u_value v_bs = NULL;
    u_value v_rb = NULL;
    u_value v_decls = NULL;
    u_value t2814 = &o_list_46_Nil;
    u_value *e2815 = &t2814;
    u_value l2816 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l2816, &n_list_46_Cons)) {
      u_value v_x = u_field(l2816, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2815 = u_append(e2815, v_x);
      }
      l2816 = u_field(l2816, 1);
    }
    u_value t2817 = t2814;
    v_caps = t2817;
    static struct u_object l_lam_2818 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_2818, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_2819 = U_LIT("env");
    static struct u_object l_args_2820 = U_LIT("args");
    u_value t2821 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2819), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_2820));
    v_decls = t2821;
    static struct u_object l_env_2822 = U_LIT("env");
    static struct u_object l_args_2823 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2822), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2823), &o_list_46_Nil)), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), &o_list_46_Nil)))), r_g);
    static struct u_object l_u_mk_closure_2824 = U_LIT("u_mk_closure");
    u_value t2825 = &o_list_46_Nil;
    u_value *e2826 = &t2825;
    u_value l2827 = v_caps;
    while (u_is_con(l2827, &n_list_46_Cons)) {
      u_value v_x = u_field(l2827, 0);
      if (u_truth(u_mk_bool(1))) {
        e2826 = u_append(e2826, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2827 = u_field(l2827, 1);
    }
    u_value t2828 = &o_list_46_Nil;
    u_value t2829 = f_c_46_c_call_2(&l_u_mk_closure_2824, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), &o_list_46_Nil)), t2825));
    *r_ss = t2828;
    *r_r = t2829;
  }
}

void p_compile_46_compile_proc_lambda(u_value v_gl, u_value v_env, u_value v_ins, u_value v_outs, u_value v_b, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_caps = NULL;
    u_value v_f = NULL;
    u_value v_bs = NULL;
    u_value v_decls = NULL;
    u_value t2830 = &o_list_46_Nil;
    u_value *e2831 = &t2830;
    u_value l2832 = f_list_46_distinct_1(f_compile_46_free_in_stmt_2(v_b, f_syntax_46_names_1(f_list_46__43__43__2(v_ins, v_outs))));
    while (u_is_con(l2832, &n_list_46_Cons)) {
      u_value v_x = u_field(l2832, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2831 = u_append(e2831, v_x);
      }
      l2832 = u_field(l2832, 1);
    }
    u_value t2833 = t2830;
    v_caps = t2833;
    static struct u_object l_proc_2834 = U_LIT("proc");
    p_compile_46_gen_name(&l_proc_2834, &v_f, r_g);
    u_value t2835 = &o_list_46_Nil;
    u_value *e2836 = &t2835;
    u_value l2837 = f_syntax_46_names_1(v_outs);
    while (u_is_con(l2837, &n_list_46_Cons)) {
      u_value v_x = u_field(l2837, 0);
      if (u_truth(u_mk_bool(1))) {
        e2836 = u_append(e2836, f_compile_46_pointer_1(v_x));
      }
      l2837 = u_field(l2837, 1);
    }
    p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_ins))), t2835), v_b, &v_bs, r_g);
    static struct u_object l_env_2838 = U_LIT("env");
    static struct u_object l_ins_2839 = U_LIT("ins");
    u_value t2840 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2838), f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_ins), u_mk_int(0)), &l_ins_2839), f_compile_46_unpack_pointers_1(f_compile_46_numbered_2(f_syntax_46_names_1(v_outs), u_mk_int(0)))));
    v_decls = t2840;
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, &o_c_46_CtVoid, v_f, f_compile_46_proc_params_0(), f_list_46__43__43__2(v_decls, v_bs)), r_g);
    static struct u_object l_u_mk_proc_2841 = U_LIT("u_mk_proc");
    u_value t2842 = &o_list_46_Nil;
    u_value *e2843 = &t2842;
    u_value l2844 = v_caps;
    while (u_is_con(l2844, &n_list_46_Cons)) {
      u_value v_x = u_field(l2844, 0);
      if (u_truth(u_mk_bool(1))) {
        e2843 = u_append(e2843, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2844 = u_field(l2844, 1);
    }
    u_value t2845 = &o_list_46_Nil;
    u_value t2846 = f_c_46_c_call_2(&l_u_mk_proc_2841, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), &o_list_46_Nil)), t2842));
    *r_ss = t2845;
    *r_r = t2846;
  }
}

u_value f_compile_46_proc_params_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of compile.proc_params");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_env_2847 = U_LIT("env");
  static struct u_object l_ins_2848 = U_LIT("ins");
  static struct u_object l_outs_2849 = U_LIT("outs");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2847), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_ins_2848), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, f_compile_46_value_ptr_0()), &l_outs_2849), &o_list_46_Nil)));
  return cache;
}

u_value f_compile_46_unpack_pointers_1(u_value v_xs) {
  u_value s2850 = v_xs;
  u_value t2851 = NULL;
  if ((!t2851) && u_is_con(s2850, &n_list_46_Nil)) {
    t2851 = &o_list_46_Nil;
  }
  if ((!t2851) && u_is_con(s2850, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2850, 0), 0);
    u_value v_i = u_field(u_field(s2850, 0), 1);
    u_value v_rest = u_field(s2850, 1);
    static struct u_object l_outs_2852 = U_LIT("outs");
    t2851 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(&l_outs_2852, v_i))), f_compile_46_unpack_pointers_1(v_rest));
  }
  if (!t2851) {
    u_no_case(s2850);
  }
  return t2851;
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s2853 = v_xs;
  u_value t2854 = NULL;
  if ((!t2854) && u_is_con(s2853, &n_list_46_Nil)) {
    t2854 = &o_list_46_Nil;
  }
  if ((!t2854) && u_is_con(s2853, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2853, 0), 0);
    u_value v_i = u_field(u_field(s2853, 0), 1);
    u_value v_rest = u_field(s2853, 1);
    t2854 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t2854) {
    u_no_case(s2853);
  }
  return t2854;
}

void p_compile_46_expr_cases(u_value v_gl, u_value v_env, u_value v_cases, u_value v_v, u_value v_t, u_value *r_ss, u_value *r_g) {
  {
    u_value v_bs = NULL;
    u_value v_rb = NULL;
    u_value v_gs = NULL;
    u_value v_rg = NULL;
    u_value v_body = NULL;
    u_value v_rest = NULL;
    u_value v_env1 = NULL;
    u_value s2855 = v_cases;
    int d2856 = 0;
    if ((!d2856) && u_is_con(s2855, &n_list_46_Nil)) {
      d2856 = 1;
      {
        u_value t2857 = &o_list_46_Nil;
        *r_ss = t2857;
      }
    }
    if ((!d2856) && (u_is_con(s2855, &n_list_46_Cons) && u_is_con(u_field(s2855, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2855, 0), 0);
      u_value v_guard = u_field(u_field(s2855, 0), 1);
      u_value v_e = u_field(u_field(s2855, 0), 2);
      u_value v_more = u_field(s2855, 1);
      d2856 = 1;
      {
        u_value t2858 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2858;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2859 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), &o_list_46_Nil));
        v_body = t2859;
        u_value s2860 = v_guard;
        int d2861 = 0;
        if ((!d2861) && u_is_con(s2860, &n_option_46_Some)) {
          u_value v_c = u_field(s2860, 0);
          d2861 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2862 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, &o_list_46_Nil), &o_list_46_Nil));
            v_body = t2862;
          }
        }
        if ((!d2861) && u_is_con(s2860, &n_option_46_None)) {
          d2861 = 1;
          {
          }
        }
        if (!d2861) {
          u_no_case(s2860);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2863 = U_LIT("!");
        u_value t2864 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2863, u_mk_con(&n_c_46_CeVar, 1, v_t)), &o_list_46_Nil), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), &o_list_46_Nil), &o_list_46_Nil), v_rest);
        *r_ss = t2864;
      }
    }
    if (!d2856) {
      u_no_case(s2855);
    }
  }
}

void p_compile_46_compile_loops(u_value v_gl, u_value v_env, u_value v_xs, u_value v_lists, u_value v_result, u_value v_c, u_value v_t, u_value *r_ss, u_value *r_g) {
  {
    u_value v_cs = NULL;
    u_value v_rc = NULL;
    u_value v_bs = NULL;
    u_value v_rb = NULL;
    u_value v_ls = NULL;
    u_value v_rl = NULL;
    u_value v_l = NULL;
    u_value v_inner = NULL;
    u_value s2865 = u_mk_tuple(2, v_xs, v_lists);
    int d2866 = 0;
    if ((!d2866) && (u_is_con(u_field(s2865, 0), &n_list_46_Cons) && u_is_con(u_field(s2865, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2865, 0), 0);
      u_value v_more = u_field(u_field(s2865, 0), 1);
      u_value v_list = u_field(u_field(s2865, 1), 0);
      u_value v_others = u_field(u_field(s2865, 1), 1);
      d2866 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2867 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2867, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), &o_list_46_Nil)), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2868 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), &o_list_46_Nil));
        *r_ss = t2868;
        static struct u_object l_u_is_con_2869 = U_LIT("u_is_con");
        u_value t2870 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2869, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), &o_list_46_Nil))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), &o_list_46_Nil), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), &o_list_46_Nil)))), &o_list_46_Nil));
        *r_ss = t2870;
      }
    }
    if (!d2866) {
      d2866 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2871 = U_LIT("u_append");
        u_value t2872 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2871, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, &o_list_46_Nil)))), &o_list_46_Nil)), &o_list_46_Nil), &o_list_46_Nil));
        *r_ss = t2872;
      }
    }
    if (!d2866) {
      u_no_case(s2865);
    }
  }
}

void p_compile_46_compile_stmt(u_value v_gl, u_value v_env, u_value v_s, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value v_cs = NULL;
    u_value v_ra = NULL;
    u_value v_rb = NULL;
    u_value v_rc = NULL;
    u_value v_rs = NULL;
    u_value v_t = NULL;
    u_value v_d = NULL;
    u_value s2873 = v_s;
    int d2874 = 0;
    if ((!d2874) && u_is_con(s2873, &n_syntax_46_SSkip)) {
      d2874 = 1;
      {
        u_value t2875 = &o_list_46_Nil;
        *r_ss = t2875;
      }
    }
    if ((!d2874) && u_is_con(s2873, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2873, 0);
      d2874 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2876 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2876;
      }
    }
    if ((!d2874) && u_is_con(s2873, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2873, 0);
      u_value v_body = u_field(s2873, 1);
      d2874 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t2877 = &o_list_46_Nil;
        u_value *e2878 = &t2877;
        u_value l2879 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2879, &n_list_46_Cons)) {
          u_value v_x = u_field(l2879, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2880 = U_LIT("NULL");
            e2878 = u_append(e2878, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2880))));
          }
          l2879 = u_field(l2879, 1);
        }
        u_value t2881 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2877, v_bs)), &o_list_46_Nil);
        *r_ss = t2881;
      }
    }
    if ((!d2874) && u_is_con(s2873, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2873, 0);
      u_value v_l = u_field(s2873, 1);
      u_value v_r = u_field(s2873, 2);
      d2874 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2882 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), &o_list_46_Nil));
        *r_ss = t2882;
      }
    }
    if ((!d2874) && u_is_con(s2873, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2873, 0);
      u_value v_b = u_field(s2873, 1);
      d2874 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2883 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2883, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2884 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), &o_list_46_Nil)))), &o_list_46_Nil)));
        *r_ss = t2884;
      }
    }
    if ((!d2874) && u_is_con(s2873, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2873, 0);
      u_value v_ins = u_field(s2873, 1);
      u_value v_outs = u_field(s2873, 2);
      u_value v_refs = u_field(s2873, 3);
      d2874 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        if (u_truth(f_list_46_has_key_2(v_p, v_env))) {
          {
            static struct u_object l_u_call_proc_2885 = U_LIT("u_call_proc");
            u_value t2886 = &o_list_46_Nil;
            u_value *e2887 = &t2886;
            u_value l2888 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
            while (u_is_con(l2888, &n_list_46_Cons)) {
              u_value v_x = u_field(l2888, 0);
              if (u_truth(u_mk_bool(1))) {
                e2887 = u_append(e2887, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
              }
              l2888 = u_field(l2888, 1);
            }
            u_value t2889 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_call_proc_2885, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_var_expr_2(v_env, v_p), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_ins)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(f_list_46__43__43__2(v_outs, v_refs))), &o_list_46_Nil))), f_list_46__43__43__2(v_rs, t2886)))), &o_list_46_Nil));
            *r_ss = t2889;
          }
        } else {
          {
            u_value t2890 = &o_list_46_Nil;
            u_value *e2891 = &t2890;
            u_value l2892 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
            while (u_is_con(l2892, &n_list_46_Cons)) {
              u_value v_x = u_field(l2892, 0);
              if (u_truth(u_mk_bool(1))) {
                e2891 = u_append(e2891, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
              }
              l2892 = u_field(l2892, 1);
            }
            u_value t2893 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2890))), &o_list_46_Nil));
            *r_ss = t2893;
          }
        }
      }
    }
    if ((!d2874) && u_is_con(s2873, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2873, 0);
      u_value v_cases = u_field(s2873, 1);
      d2874 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2894 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2894, &v_d, r_g);
        static struct u_object l_d_2895 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2895, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2896 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, &o_c_46_CtInt, v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), &o_list_46_Nil)), v_bs));
        *r_ss = t2896;
        static struct u_object l__33__2897 = U_LIT("!");
        static struct u_object l_u_no_case_2898 = U_LIT("u_no_case");
        u_value t2899 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2897, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2898, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &o_list_46_Nil))), &o_list_46_Nil), &o_list_46_Nil), &o_list_46_Nil));
        *r_ss = t2899;
      }
    }
    if ((!d2874) && (u_is_con(s2873, &n_syntax_46_SAssert) && (u_is_con(u_field(s2873, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2873, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2873, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2873, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2873, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2873, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2873, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2873, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2873, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2873, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2873, 0), 1), 1), 0);
      d2874 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2900 = U_LIT("u_assert_equal");
        u_value t2901 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2900, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, &o_list_46_Nil)))), &o_list_46_Nil)));
        *r_ss = t2901;
      }
    }
    if ((!d2874) && u_is_con(s2873, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2873, 0);
      d2874 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2902 = U_LIT("u_assert_true");
        u_value t2903 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2902, u_mk_con(&n_list_46_Cons, 2, v_rc, &o_list_46_Nil))), &o_list_46_Nil));
        *r_ss = t2903;
      }
    }
    if ((!d2874) && u_is_con(s2873, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2873, 0);
      u_value v_ty = u_field(s2873, 1);
      d2874 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2904 = U_LIT("u_raise");
        u_value t2905 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2904, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), &o_list_46_Nil)))), &o_list_46_Nil));
        *r_ss = t2905;
      }
    }
    if ((!d2874) && u_is_con(s2873, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2873, 0);
      u_value v_hs = u_field(s2873, 1);
      d2874 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2906 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2906, &v_d, r_g);
        static struct u_object l_d_2907 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2907, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2908 = U_LIT("u_pop");
        static struct u_object l__33__2909 = U_LIT("!");
        static struct u_object l_u_reraise_2910 = U_LIT("u_reraise");
        u_value t2911 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2908, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &o_list_46_Nil))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, &o_c_46_CtInt, v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), &o_list_46_Nil)), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2909, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2910, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &o_list_46_Nil))), &o_list_46_Nil), &o_list_46_Nil), &o_list_46_Nil)));
        v_as = t2911;
        static struct u_object l_struct_32_u_handler_2912 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2913 = U_LIT("u_push");
        static struct u_object l__61__61__2914 = U_LIT("==");
        static struct u_object l_setjmp_2915 = U_LIT("setjmp");
        static struct u_object l_env_2916 = U_LIT("env");
        static struct u_object l_u_pop_2917 = U_LIT("u_pop");
        u_value t2918 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2912)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2913, &o_list_46_Nil))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2914, f_c_46_c_call_2(&l_setjmp_2915, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2916), &o_list_46_Nil)), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2917, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &o_list_46_Nil))), &o_list_46_Nil)), v_as), &o_list_46_Nil))), &o_list_46_Nil);
        *r_ss = t2918;
      }
    }
    if (!d2874) {
      u_no_case(s2873);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2919 = v_xs;
    int d2920 = 0;
    if ((!d2920) && u_is_con(s2919, &n_list_46_Nil)) {
      d2920 = 1;
      {
        u_value t2921 = &o_list_46_Nil;
        *r_ss = t2921;
      }
    }
    if ((!d2920) && u_is_con(s2919, &n_list_46_Cons)) {
      u_value v_x = u_field(s2919, 0);
      u_value v_rest = u_field(s2919, 1);
      d2920 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2922 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2922;
      }
    }
    if (!d2920) {
      u_no_case(s2919);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2923 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2924 = NULL;
  if ((!t2924) && (u_is_con(u_field(s2923, 0), &n_list_46_Cons) && (u_is_con(u_field(s2923, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2923, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2923, 0), 0);
    u_value v_more = u_field(u_field(s2923, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2923, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2923, 1), 1);
    t2924 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2924) {
    t2924 = &o_list_46_Nil;
  }
  if (!t2924) {
    u_no_case(s2923);
  }
  return t2924;
}

void p_compile_46_handler_cases(u_value v_gl, u_value v_env, u_value v_hs, u_value v_h, u_value v_d, u_value *r_ss, u_value *r_g) {
  {
    u_value v_v = NULL;
    u_value v_bs = NULL;
    u_value v_gs = NULL;
    u_value v_rg = NULL;
    u_value v_body = NULL;
    u_value v_rest = NULL;
    u_value v_env1 = NULL;
    u_value v_tests = NULL;
    static struct u_object l_u_caught_2925 = U_LIT("u_caught");
    u_value t2926 = f_c_46_c_call_2(&l_u_caught_2925, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), &o_list_46_Nil));
    v_v = t2926;
    u_value s2927 = v_hs;
    int d2928 = 0;
    if ((!d2928) && u_is_con(s2927, &n_list_46_Nil)) {
      d2928 = 1;
      {
        u_value t2929 = &o_list_46_Nil;
        *r_ss = t2929;
      }
    }
    if ((!d2928) && (u_is_con(s2927, &n_list_46_Cons) && u_is_con(u_field(s2927, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2927, 0), 0);
      u_value v_p = u_field(u_field(s2927, 0), 1);
      u_value v_guard = u_field(u_field(s2927, 0), 2);
      u_value v_s = u_field(u_field(s2927, 0), 3);
      u_value v_more = u_field(s2927, 1);
      d2928 = 1;
      {
        u_value t2930 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2930;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2931 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), &o_list_46_Nil), v_bs);
        v_body = t2931;
        u_value s2932 = v_guard;
        int d2933 = 0;
        if ((!d2933) && u_is_con(s2932, &n_option_46_Some)) {
          u_value v_c = u_field(s2932, 0);
          d2933 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2934 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, &o_list_46_Nil), &o_list_46_Nil));
            v_body = t2934;
          }
        }
        if ((!d2933) && u_is_con(s2932, &n_option_46_None)) {
          d2933 = 1;
          {
          }
        }
        if (!d2933) {
          u_no_case(s2932);
        }
        u_value s2935 = v_p;
        int d2936 = 0;
        if ((!d2936) && u_is_con(s2935, &n_syntax_46_PWild)) {
          d2936 = 1;
          {
            u_value t2937 = &o_list_46_Nil;
            v_tests = t2937;
          }
        }
        if (!d2936) {
          d2936 = 1;
          {
            static struct u_object l_u_has_type_2938 = U_LIT("u_has_type");
            u_value t2939 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2938, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), &o_list_46_Nil))), &o_list_46_Nil), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2939;
          }
        }
        if (!d2936) {
          u_no_case(s2935);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2940 = U_LIT("!");
        u_value t2941 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2940, u_mk_con(&n_c_46_CeVar, 1, v_d)), &o_list_46_Nil), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), &o_list_46_Nil), &o_list_46_Nil), v_rest);
        *r_ss = t2941;
      }
    }
    if (!d2928) {
      u_no_case(s2927);
    }
  }
}

void p_compile_46_stmt_cases(u_value v_gl, u_value v_env, u_value v_cases, u_value v_v, u_value v_d, u_value *r_ss, u_value *r_g) {
  {
    u_value v_bs = NULL;
    u_value v_gs = NULL;
    u_value v_rg = NULL;
    u_value v_body = NULL;
    u_value v_rest = NULL;
    u_value v_env1 = NULL;
    u_value s2942 = v_cases;
    int d2943 = 0;
    if ((!d2943) && u_is_con(s2942, &n_list_46_Nil)) {
      d2943 = 1;
      {
        u_value t2944 = &o_list_46_Nil;
        *r_ss = t2944;
      }
    }
    if ((!d2943) && (u_is_con(s2942, &n_list_46_Cons) && u_is_con(u_field(s2942, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2942, 0), 0);
      u_value v_guard = u_field(u_field(s2942, 0), 1);
      u_value v_s = u_field(u_field(s2942, 0), 2);
      u_value v_more = u_field(s2942, 1);
      d2943 = 1;
      {
        u_value t2945 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2945;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2946 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), &o_list_46_Nil), v_bs);
        v_body = t2946;
        u_value s2947 = v_guard;
        int d2948 = 0;
        if ((!d2948) && u_is_con(s2947, &n_option_46_Some)) {
          u_value v_c = u_field(s2947, 0);
          d2948 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2949 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, &o_list_46_Nil), &o_list_46_Nil));
            v_body = t2949;
          }
        }
        if ((!d2948) && u_is_con(s2947, &n_option_46_None)) {
          d2948 = 1;
          {
          }
        }
        if (!d2948) {
          u_no_case(s2947);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2950 = U_LIT("!");
        u_value t2951 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2950, u_mk_con(&n_c_46_CeVar, 1, v_d)), &o_list_46_Nil), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), &o_list_46_Nil), &o_list_46_Nil), v_rest);
        *r_ss = t2951;
      }
    }
    if (!d2943) {
      u_no_case(s2942);
    }
  }
}

void p_compile_46_compile_cmd(u_value v_gl, u_value v_entry, u_value v_c, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_bs = NULL;
    u_value v_rb = NULL;
    u_value v_params = NULL;
    u_value s2952 = v_c;
    int d2953 = 0;
    if ((!d2953) && u_is_con(s2952, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2952, 0);
      u_value v_xss = u_field(s2952, 2);
      u_value v_b = u_field(s2952, 4);
      d2953 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2954 = &o_list_46_Nil;
        u_value *e2955 = &t2954;
        u_value l2956 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2956, &n_list_46_Cons)) {
          u_value v_x = u_field(l2956, 0);
          if (u_truth(u_mk_bool(1))) {
            e2955 = u_append(e2955, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2956 = u_field(l2956, 1);
        }
        u_value t2957 = t2954;
        v_params = t2957;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), &o_list_46_Nil))) {
          {
            u_value t2958 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2958;
          }
        } else {
          {
            u_value t2959 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), &o_list_46_Nil));
            v_bs = t2959;
          }
        }
        u_value t2960 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), &o_list_46_Nil);
        u_value t2961 = &o_list_46_Nil;
        *r_ds = t2960;
        *r_ms = t2961;
      }
    }
    if ((!d2953) && u_is_con(s2952, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2952, 0);
      u_value v_ins = u_field(s2952, 2);
      u_value v_outs = u_field(s2952, 3);
      u_value v_refs = u_field(s2952, 4);
      u_value v_body = u_field(s2952, 5);
      d2953 = 1;
      {
        u_value t2962 = &o_list_46_Nil;
        u_value *e2963 = &t2962;
        u_value l2964 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2964, &n_list_46_Cons)) {
          u_value v_x = u_field(l2964, 0);
          if (u_truth(u_mk_bool(1))) {
            e2963 = u_append(e2963, f_compile_46_pointer_1(v_x));
          }
          l2964 = u_field(l2964, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2962), v_body, &v_bs, r_g);
        u_value t2965 = &o_list_46_Nil;
        u_value *e2966 = &t2965;
        u_value l2967 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2967, &n_list_46_Cons)) {
          u_value v_x = u_field(l2967, 0);
          if (u_truth(u_mk_bool(1))) {
            e2966 = u_append(e2966, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2967 = u_field(l2967, 1);
        }
        u_value t2968 = &o_list_46_Nil;
        u_value *e2969 = &t2968;
        u_value l2970 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2970, &n_list_46_Cons)) {
          u_value v_x = u_field(l2970, 0);
          if (u_truth(u_mk_bool(1))) {
            e2969 = u_append(e2969, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2970 = u_field(l2970, 1);
        }
        u_value t2971 = f_list_46__43__43__2(t2965, t2968);
        v_params = t2971;
        u_value t2972 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, &o_c_46_CtVoid, f_compile_46_proc_name_1(v_n), v_params, v_bs), &o_list_46_Nil);
        u_value t2973 = &o_list_46_Nil;
        *r_ds = t2972;
        *r_ms = t2973;
      }
    }
    if ((!d2953) && u_is_con(s2952, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s2952, 0);
      u_value v_body = u_field(s2952, 2);
      static struct u_object l_test_2977 = U_LIT("test");
      if (u_truth(u_eq(v_entry, &l_test_2977))) {
        d2953 = 1;
        {
          p_compile_46_compile_stmt(v_gl, &o_list_46_Nil, v_body, &v_bs, r_g);
          static struct u_object l_u_passed_2974 = U_LIT("u_passed");
          u_value t2975 = &o_list_46_Nil;
          u_value t2976 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_passed_2974, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_n), &o_list_46_Nil))), &o_list_46_Nil))), &o_list_46_Nil);
          *r_ds = t2975;
          *r_ms = t2976;
        }
      }
    }
    if (!d2953) {
      d2953 = 1;
      {
        u_value t2978 = &o_list_46_Nil;
        u_value t2979 = &o_list_46_Nil;
        *r_ds = t2978;
        *r_ms = t2979;
      }
    }
    if (!d2953) {
      u_no_case(s2952);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_entry, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2980 = v_cs;
    int d2981 = 0;
    if ((!d2981) && u_is_con(s2980, &n_list_46_Nil)) {
      d2981 = 1;
      {
        u_value t2982 = &o_list_46_Nil;
        u_value t2983 = &o_list_46_Nil;
        *r_ds = t2982;
        *r_ms = t2983;
      }
    }
    if ((!d2981) && u_is_con(s2980, &n_list_46_Cons)) {
      u_value v_c = u_field(s2980, 0);
      u_value v_rest = u_field(s2980, 1);
      d2981 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_entry, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_entry, v_rest, &v_d2, &v_m2, r_g);
        u_value t2984 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2985 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2984;
        *r_ms = t2985;
      }
    }
    if (!d2981) {
      u_no_case(s2980);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_handler_2986 = U_LIT("handler");
  u_value t3013 = u_mk_con(&n_c_46_CeVar, 1, &l_handler_2986);
  u_value t3012;
  {
    u_value v_h = t3013;
    static struct u_object l_static_32_u_value_2987 = U_LIT("static u_value");
    static struct u_object l_cache_2988 = U_LIT("cache");
    static struct u_object l_NULL_2989 = U_LIT("NULL");
    static struct u_object l__61__61__2990 = U_LIT("==");
    static struct u_object l_cache_2991 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2992 = U_LIT("U_COMPUTING");
    static struct u_object l_u_fail_2993 = U_LIT("u_fail");
    static struct u_object l_recursive_32_initia_2994 = U_LIT("recursive initialization of ");
    static struct u_object l_cache_2995 = U_LIT("cache");
    static struct u_object l_cache_2996 = U_LIT("cache");
    static struct u_object l_cache_2997 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2998 = U_LIT("U_COMPUTING");
    static struct u_object l_struct_32_u_handler_2999 = U_LIT("struct u_handler");
    static struct u_object l_handler_3000 = U_LIT("handler");
    static struct u_object l_u_push_3001 = U_LIT("u_push");
    static struct u_object l__33__61__3002 = U_LIT("!=");
    static struct u_object l_setjmp_3003 = U_LIT("setjmp");
    static struct u_object l_env_3004 = U_LIT("env");
    static struct u_object l_u_pop_3005 = U_LIT("u_pop");
    static struct u_object l_cache_3006 = U_LIT("cache");
    static struct u_object l_NULL_3007 = U_LIT("NULL");
    static struct u_object l_u_reraise_3008 = U_LIT("u_reraise");
    static struct u_object l_u_pop_3009 = U_LIT("u_pop");
    static struct u_object l_cache_3010 = U_LIT("cache");
    static struct u_object l_cache_3011 = U_LIT("cache");
    t3012 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2987), &l_cache_2988, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2989))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2990, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2991), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2992)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2993, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2994, v_n)), &o_list_46_Nil))), &o_list_46_Nil), &o_list_46_Nil), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2995), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2996))), &o_list_46_Nil), &o_list_46_Nil), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2997), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2998)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2999)), &l_handler_3000, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_3001, &o_list_46_Nil))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__33__61__3002, f_c_46_c_call_2(&l_setjmp_3003, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, v_h, &l_env_3004), &o_list_46_Nil)), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_3005, u_mk_con(&n_list_46_Cons, 2, v_h, &o_list_46_Nil))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_3006), u_mk_con(&n_c_46_CeVar, 1, &l_NULL_3007)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_3008, u_mk_con(&n_list_46_Cons, 2, v_h, &o_list_46_Nil))), &o_list_46_Nil))), &o_list_46_Nil), &o_list_46_Nil)))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_3009, u_mk_con(&n_list_46_Cons, 2, v_h, &o_list_46_Nil))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_3010), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_3011))), &o_list_46_Nil)))));
  }
  return t3012;
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t3014 = &o_list_46_Nil;
  u_value *e3015 = &t3014;
  u_value l3016 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l3016, &n_list_46_Cons)) {
    u_value v_p = u_field(l3016, 0);
    if (u_truth(u_mk_bool(1))) {
      e3015 = u_append(e3015, f_compile_46_fun_wrapper_1(v_p));
    }
    l3016 = u_field(l3016, 1);
  }
  u_value t3017 = &o_list_46_Nil;
  u_value *e3018 = &t3017;
  u_value l3019 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l3019, &n_list_46_Cons)) {
    u_value v_p = u_field(l3019, 0);
    if (u_truth(u_mk_bool(1))) {
      e3018 = u_append(e3018, f_compile_46_constr_wrapper_1(v_p));
    }
    l3019 = u_field(l3019, 1);
  }
  u_value t3020 = &o_list_46_Nil;
  u_value *e3021 = &t3020;
  u_value l3022 = f_list_46_seconds_1(f_map_46_entries_1(f_compile_46_builtin_functions_0()));
  while (u_is_con(l3022, &n_list_46_Cons)) {
    u_value v_p = u_field(l3022, 0);
    if (u_truth(u_mk_bool(1))) {
      e3021 = u_append(e3021, f_compile_46_builtin_wrapper_1(v_p));
    }
    l3022 = u_field(l3022, 1);
  }
  u_value t3023 = &o_list_46_Nil;
  u_value *e3024 = &t3023;
  u_value l3025 = f_map_46_entries_1(f_compile_46_Globals_46_procs_1(v_gl));
  while (u_is_con(l3025, &n_list_46_Cons)) {
    u_value v_p = u_field(l3025, 0);
    if (u_truth(u_mk_bool(1))) {
      e3024 = u_append(e3024, f_compile_46_proc_wrapper_1(v_p));
    }
    l3025 = u_field(l3025, 1);
  }
  return f_list_46__43__43__2(t3014, f_list_46__43__43__2(f_list_46_concat_1(t3017), f_list_46__43__43__2(t3020, t3023)));
}

u_value f_compile_46_proc_wrapper_1(u_value v_p) {
  u_value s3026 = v_p;
  u_value t3027 = NULL;
  if (!t3027) {
    u_value v_n = u_field(s3026, 0);
    u_value v_k = u_field(u_field(s3026, 1), 0);
    u_value v_m = u_field(u_field(s3026, 1), 1);
    static struct u_object l_wp__3028 = U_LIT("wp_");
    u_value t3029 = &o_list_46_Nil;
    u_value *e3030 = &t3029;
    u_value l3031 = f_compile_46_range_to_1(v_k);
    while (u_is_con(l3031, &n_list_46_Cons)) {
      u_value v_i = u_field(l3031, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_ins_3032 = U_LIT("ins");
        e3030 = u_append(e3030, f_compile_46_index_2(&l_ins_3032, v_i));
      }
      l3031 = u_field(l3031, 1);
    }
    u_value t3033 = &o_list_46_Nil;
    u_value *e3034 = &t3033;
    u_value l3035 = f_compile_46_range_to_1(v_m);
    while (u_is_con(l3035, &n_list_46_Cons)) {
      u_value v_i = u_field(l3035, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_outs_3036 = U_LIT("outs");
        e3034 = u_append(e3034, f_compile_46_index_2(&l_outs_3036, v_i));
      }
      l3035 = u_field(l3035, 1);
    }
    t3027 = u_mk_con(&n_c_46_CdFunction, 4, &o_c_46_CtVoid, u_concat(&l_wp__3028, f_compile_46_mangle_1(v_n)), f_compile_46_proc_params_0(), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_n), f_list_46__43__43__2(t3029, t3033))), &o_list_46_Nil));
  }
  if (!t3027) {
    u_no_case(s3026);
  }
  return t3027;
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s3037 = v_p;
  u_value t3038 = NULL;
  if (!t3038) {
    u_value v_n = u_field(s3037, 0);
    u_value v_k = u_field(s3037, 1);
    static struct u_object l_w__3039 = U_LIT("w_");
    t3038 = f_compile_46_wrapper_3(u_concat(&l_w__3039, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t3038) {
    u_no_case(s3037);
  }
  return t3038;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s3040 = v_p;
  u_value t3041 = NULL;
  if (!t3041) {
    u_value v_f = u_field(s3040, 0);
    u_value v_k = u_field(s3040, 1);
    static struct u_object l_w_3042 = U_LIT("w");
    t3041 = f_compile_46_wrapper_3(u_concat(&l_w_3042, v_f), v_f, v_k);
  }
  if (!t3041) {
    u_no_case(s3040);
  }
  return t3041;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_3043 = U_LIT("env");
  static struct u_object l_args_3044 = U_LIT("args");
  u_value t3045 = &o_list_46_Nil;
  u_value *e3046 = &t3045;
  u_value l3047 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l3047, &n_list_46_Cons)) {
    u_value v_i = u_field(l3047, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_3048 = U_LIT("args");
      e3046 = u_append(e3046, f_compile_46_index_2(&l_args_3048, v_i));
    }
    l3047 = u_field(l3047, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_3043), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_3044), &o_list_46_Nil)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t3045))), &o_list_46_Nil));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s3049 = v_p;
  u_value t3050 = NULL;
  if ((!t3050) && u_equals(u_field(s3049, 1), u_mk_int(0))) {
    t3050 = &o_list_46_Nil;
  }
  if (!t3050) {
    u_value v_n = u_field(s3049, 0);
    u_value v_k = u_field(s3049, 1);
    static struct u_object l_wc__3051 = U_LIT("wc_");
    static struct u_object l_env_3052 = U_LIT("env");
    static struct u_object l_args_3053 = U_LIT("args");
    static struct u_object l_u_mk_con_array_3054 = U_LIT("u_mk_con_array");
    static struct u_object l_args_3055 = U_LIT("args");
    t3050 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__3051, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_3052), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_3053), &o_list_46_Nil)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_3054, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_3055), &o_list_46_Nil)))))), &o_list_46_Nil)), &o_list_46_Nil);
  }
  if (!t3050) {
    u_no_case(s3049);
  }
  return t3050;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t3056;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t3056 = &o_list_46_Nil;
  } else {
    t3056 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), &o_list_46_Nil));
  }
  return t3056;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s3057 = v_d;
  u_value t3058 = NULL;
  if ((!t3058) && u_is_con(s3057, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s3057, 0);
    u_value v_f = u_field(s3057, 1);
    u_value v_ps = u_field(s3057, 2);
    t3058 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), &o_list_46_Nil);
  }
  if (!t3058) {
    t3058 = &o_list_46_Nil;
  }
  if (!t3058) {
    u_no_case(s3057);
  }
  return t3058;
}

void p_compile_46_compile_program(u_value v_q, u_value v_m, u_value v_entry, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t3059 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), &o_list_46_Nil);
    v_g = t3059;
    u_value t3060 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t3060;
    static struct u_object l_test_3061 = U_LIT("test");
    u_value t3063 = f_prelude_46__33__61__2(v_entry, &l_test_3061);
    if (u_truth(t3063)) {
      t3063 = f_prelude_46_not_1(f_list_46_exists_2(f_syntax_46_Program_46_cmds_1(v_q), u_mk_closure(lam3062, 2, v_m, v_entry)));
    }
    if (u_truth(t3063)) {
      {
        u_raise(u_mk_con(&n_compile_46_UnknownEntry, 1, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), v_entry)), "compile.CompileError");
      }
    }
    p_compile_46_compile_cmds(v_gl, v_entry, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    static struct u_object l_test_3064 = U_LIT("test");
    if (u_truth(f_prelude_46__33__61__2(v_entry, &l_test_3064))) {
      {
        u_value t3065 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), v_entry)), &o_list_46_Nil)), &o_list_46_Nil);
        v_ms = t3065;
      }
    }
    u_value t3066 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, f_compile_46_Gen_46_lifted_1(v_g)));
    v_fs = t3066;
    static struct u_object l_ulang_46_h_3067 = U_LIT("ulang.h");
    u_value t3068 = &o_list_46_Nil;
    u_value *e3069 = &t3068;
    u_value l3070 = v_fs;
    while (u_is_con(l3070, &n_list_46_Cons)) {
      u_value v_d = u_field(l3070, 0);
      if (u_truth(u_mk_bool(1))) {
        e3069 = u_append(e3069, f_compile_46_prototype_1(v_d));
      }
      l3070 = u_field(l3070, 1);
    }
    static struct u_object l_main_3071 = U_LIT("main");
    static struct u_object l_argc_3072 = U_LIT("argc");
    static struct u_object l_char_3073 = U_LIT("char");
    static struct u_object l_argv_3074 = U_LIT("argv");
    static struct u_object l_u_args_3075 = U_LIT("u_args");
    static struct u_object l_argc_3076 = U_LIT("argc");
    static struct u_object l_argv_3077 = U_LIT("argv");
    u_value t3078 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_3067), &o_list_46_Nil), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t3068), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, &o_c_46_CtInt, &l_main_3071, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &o_c_46_CtInt, &l_argc_3072), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_3073))), &l_argv_3074), &o_list_46_Nil)), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_3075, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_3076), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_3077), &o_list_46_Nil)))), &o_list_46_Nil), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), &o_list_46_Nil)))), &o_list_46_Nil))))));
    *r_f = t3078;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_entry, u_value v_target) {
  {
    u_value v_cache = NULL;
    u_value t3079 = &o_list_46_Nil;
    v_cache = t3079;
    p_compile_46_compile_with(v_source, v_entry, v_target, &v_cache);
  }
}

u_value f_compile_46_is_entry_2(u_value v_c, u_value v_n) {
  u_value s3080 = v_c;
  u_value t3081 = NULL;
  if ((!t3081) && (u_is_con(s3080, &n_syntax_46_CProc) && (u_is_con(u_field(s3080, 2), &n_list_46_Nil) && (u_is_con(u_field(s3080, 3), &n_list_46_Nil) && u_is_con(u_field(s3080, 4), &n_list_46_Nil))))) {
    u_value v_k = u_field(s3080, 0);
    t3081 = u_eq(v_k, v_n);
  }
  if (!t3081) {
    t3081 = u_mk_bool(0);
  }
  if (!t3081) {
    u_no_case(s3080);
  }
  return t3081;
}

void p_compile_46_compile_with(u_value v_source, u_value v_entry, u_value v_target, u_value *r_cache) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_3082 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_3082, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_3083 = U_LIT("prelude");
    p_check_46_check_modules(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_3083, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q, r_cache);
    p_compile_46_compile_program(v_q, f_parse_46_base_name_1(v_source), v_entry, &v_f);
    p_os_write(v_target, f_c_46_c_file_1(v_f));
  }
}

void p_ulang_46_main(void) {
  {
    u_value v_args = NULL;
    u_value v_command = NULL;
    u_value v_rest = NULL;
    u_value v_ok = NULL;
    u_value t3084 = u_mk_bool(1);
    v_ok = t3084;
    p_os_args(&v_args);
    u_value t3085 = f_ulang_46_before_dashes_1(v_args);
    u_value t3086 = f_ulang_46_after_dashes_1(v_args);
    v_command = t3085;
    v_rest = t3086;
    u_value s3087 = v_command;
    int d3088 = 0;
    if ((!d3088) && u_is_con(s3087, &n_list_46_Nil)) {
      d3088 = 1;
      {
      }
    }
    if ((!d3088) && (u_is_con(s3087, &n_list_46_Cons) && (u_equals(u_field(s3087, 0), u_mk_lit("run")) && (u_is_con(u_field(s3087, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s3087, 1), 1), &n_list_46_Nil))))) {
      u_value v_file = u_field(u_field(s3087, 1), 0);
      d3088 = 1;
      {
        p_ulang_46_attempt(v_file, u_mk_proc(proc3089, 2, v_file, v_rest), &v_ok);
      }
    }
    if ((!d3088) && (u_is_con(s3087, &n_list_46_Cons) && (u_equals(u_field(s3087, 0), u_mk_lit("run")) && (u_is_con(u_field(s3087, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s3087, 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s3087, 1), 1), 1), &n_list_46_Nil)))))) {
      u_value v_file = u_field(u_field(s3087, 1), 0);
      u_value v_name = u_field(u_field(u_field(s3087, 1), 1), 0);
      d3088 = 1;
      {
        p_ulang_46_attempt(v_file, u_mk_proc(proc3091, 3, v_file, v_name, v_rest), &v_ok);
      }
    }
    if ((!d3088) && (u_is_con(s3087, &n_list_46_Cons) && (u_equals(u_field(s3087, 0), u_mk_lit("test")) && u_is_con(u_field(s3087, 1), &n_list_46_Cons)))) {
      u_value v_file = u_field(u_field(s3087, 1), 0);
      u_value v_names = u_field(u_field(s3087, 1), 1);
      d3088 = 1;
      {
        p_ulang_46_attempt(v_file, u_mk_proc(proc3092, 3, v_file, v_names, v_rest), &v_ok);
      }
    }
    if ((!d3088) && (u_is_con(s3087, &n_list_46_Cons) && (u_equals(u_field(s3087, 0), u_mk_lit("check")) && (u_is_con(u_field(s3087, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s3087, 1), 1), &n_list_46_Nil))))) {
      u_value v_file = u_field(u_field(s3087, 1), 0);
      d3088 = 1;
      {
        p_ulang_46_attempt(v_file, u_mk_proc(proc3093, 1, v_file), &v_ok);
      }
    }
    if ((!d3088) && (u_is_con(s3087, &n_list_46_Cons) && (u_equals(u_field(s3087, 0), u_mk_lit("compile")) && (u_is_con(u_field(s3087, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s3087, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s3087, 1), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s3087, 1), 1), 1), 1), &n_list_46_Nil))))))) {
      u_value v_file = u_field(u_field(s3087, 1), 0);
      u_value v_entry = u_field(u_field(u_field(s3087, 1), 1), 0);
      u_value v_target = u_field(u_field(u_field(u_field(s3087, 1), 1), 1), 0);
      d3088 = 1;
      {
        p_ulang_46_attempt(v_file, u_mk_proc(proc3094, 3, v_file, v_entry, v_target), &v_ok);
      }
    }
    if (!d3088) {
      d3088 = 1;
      {
        p_os_print_error(f_ulang_46_usage_0());
        u_value t3095 = u_mk_bool(0);
        v_ok = t3095;
      }
    }
    if (!d3088) {
      u_no_case(s3087);
    }
    if (u_truth(f_prelude_46_not_1(v_ok))) {
      {
        p_os_exit(u_mk_int(1));
      }
    }
  }
}

u_value f_ulang_46_usage_0(void) {
  static u_value cache = NULL;
  if (cache == U_COMPUTING) {
    u_fail("recursive initialization of ulang.usage");
  }
  if (cache) {
    return cache;
  }
  cache = U_COMPUTING;
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_usage_58__32_ulang_32_run_3096 = U_LIT("usage: ulang run <file> [<procedure>] [-- args]\n");
  static struct u_object l__32__32__32__32__32__32__32_ulang_32_tes_3097 = U_LIT("       ulang test <file> [<name> ...] [-- args]\n");
  static struct u_object l__32__32__32__32__32__32__32_ulang_32_che_3098 = U_LIT("       ulang check <file>\n");
  static struct u_object l__32__32__32__32__32__32__32_ulang_32_com_3099 = U_LIT("       ulang compile <file> <entry> <target.c>");
  u_pop(handler);
  cache = u_concat(&l_usage_58__32_ulang_32_run_3096, u_concat(&l__32__32__32__32__32__32__32_ulang_32_tes_3097, u_concat(&l__32__32__32__32__32__32__32_ulang_32_che_3098, &l__32__32__32__32__32__32__32_ulang_32_com_3099)));
  return cache;
}

u_value f_ulang_46_before_dashes_1(u_value v_args) {
  u_value s3100 = v_args;
  u_value t3101 = NULL;
  if ((!t3101) && u_is_con(s3100, &n_list_46_Nil)) {
    t3101 = &o_list_46_Nil;
  }
  if ((!t3101) && (u_is_con(s3100, &n_list_46_Cons) && u_equals(u_field(s3100, 0), u_mk_lit("--")))) {
    t3101 = &o_list_46_Nil;
  }
  if ((!t3101) && u_is_con(s3100, &n_list_46_Cons)) {
    u_value v_a = u_field(s3100, 0);
    u_value v_more = u_field(s3100, 1);
    t3101 = u_mk_con(&n_list_46_Cons, 2, v_a, f_ulang_46_before_dashes_1(v_more));
  }
  if (!t3101) {
    u_no_case(s3100);
  }
  return t3101;
}

u_value f_ulang_46_after_dashes_1(u_value v_args) {
  u_value s3102 = v_args;
  u_value t3103 = NULL;
  if ((!t3103) && u_is_con(s3102, &n_list_46_Nil)) {
    t3103 = &o_list_46_Nil;
  }
  if ((!t3103) && (u_is_con(s3102, &n_list_46_Cons) && u_equals(u_field(s3102, 0), u_mk_lit("--")))) {
    u_value v_more = u_field(s3102, 1);
    t3103 = v_more;
  }
  if ((!t3103) && u_is_con(s3102, &n_list_46_Cons)) {
    u_value v_more = u_field(s3102, 1);
    t3103 = f_ulang_46_after_dashes_1(v_more);
  }
  if (!t3103) {
    u_no_case(s3102);
  }
  return t3103;
}

void p_ulang_46_attempt(u_value v_file, u_value v_command, u_value *r_ok) {
  {
    u_value v_message = NULL;
    static struct u_object l__3104 = U_LIT("");
    u_value t3105 = &l__3104;
    v_message = t3105;
    {
      struct u_handler *h3106 = u_push();
      if (setjmp(h3106->env) == 0) {
        {
          u_call_proc(v_command, 0, 1, r_ok);
        }
        u_pop(h3106);
      } else {
        u_pop(h3106);
        int d3107 = 0;
        if ((!d3107) && (u_has_type(h3106, "lex.LexError") && u_is_con(u_caught(h3106), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h3106), 0);
          u_value v_i = u_field(u_caught(h3106), 1);
          d3107 = 1;
          {
            p_ulang_46_located(v_file, v_m, v_i, &v_message);
          }
        }
        if ((!d3107) && (u_has_type(h3106, "parse.ParseError") && u_is_con(u_caught(h3106), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h3106), 0);
          u_value v_i = u_field(u_caught(h3106), 1);
          d3107 = 1;
          {
            p_ulang_46_located(v_file, v_m, v_i, &v_message);
          }
        }
        if ((!d3107) && (u_has_type(h3106, "os.OsError") && u_is_con(u_caught(h3106), &n_os_46_OsError))) {
          u_value v_m = u_field(u_caught(h3106), 0);
          d3107 = 1;
          {
            static struct u_object l_runtime_32_error_58__32__3108 = U_LIT("runtime error: ");
            u_value t3109 = u_concat(&l_runtime_32_error_58__32__3108, v_m);
            v_message = t3109;
          }
        }
        if ((!d3107) && (u_has_type(h3106, "run.Thrown") && u_is_con(u_caught(h3106), &n_run_46_Thrown))) {
          u_value v_v = u_field(u_caught(h3106), 0);
          d3107 = 1;
          {
            static struct u_object l_runtime_32_error_58__32_u_3110 = U_LIT("runtime error: uncaught exception ");
            u_value t3111 = u_concat(&l_runtime_32_error_58__32_u_3110, f_print_46_show_value_1(v_v));
            v_message = t3111;
          }
        }
        if ((!d3107) && (u_has_type(h3106, "compile.CompileError") && u_is_con(u_caught(h3106), &n_compile_46_Unsupported))) {
          u_value v_m = u_field(u_caught(h3106), 0);
          d3107 = 1;
          {
            static struct u_object l_compile_32_error_58__32__3112 = U_LIT("compile error: ");
            u_value t3113 = u_concat(&l_compile_32_error_58__32__3112, v_m);
            v_message = t3113;
          }
        }
        if ((!d3107) && (u_has_type(h3106, "compile.CompileError") && u_is_con(u_caught(h3106), &n_compile_46_UnknownEntry))) {
          u_value v_n = u_field(u_caught(h3106), 0);
          d3107 = 1;
          {
            static struct u_object l_compile_32_error_58__32_t_3114 = U_LIT("compile error: there is no procedure ");
            static struct u_object l__32_without_32_paramet_3115 = U_LIT(" without parameters");
            u_value t3116 = u_concat(&l_compile_32_error_58__32_t_3114, u_concat(f_syntax_46_Name_46_base_1(v_n), &l__32_without_32_paramet_3115));
            v_message = t3116;
          }
        }
        if ((!d3107) && u_has_type(h3106, "check.TypeError")) {
          u_value v_e = u_caught(h3106);
          d3107 = 1;
          {
            static struct u_object l_type_32_error_58__32__3117 = U_LIT("type error: ");
            u_value t3118 = u_concat(&l_type_32_error_58__32__3117, f_ulang_46_type_message_1(v_e));
            v_message = t3118;
          }
        }
        if ((!d3107) && u_has_type(h3106, "run.RuntimeError")) {
          u_value v_e = u_caught(h3106);
          d3107 = 1;
          {
            static struct u_object l_runtime_32_error_58__32__3119 = U_LIT("runtime error: ");
            u_value t3120 = u_concat(&l_runtime_32_error_58__32__3119, f_ulang_46_runtime_message_1(v_e));
            v_message = t3120;
          }
        }
        if (!d3107) {
          u_reraise(h3106);
        }
      }
    }
    static struct u_object l__3121 = U_LIT("");
    if (u_truth(f_prelude_46__33__61__2(v_message, &l__3121))) {
      {
        static struct u_object l__58__32__3122 = U_LIT(": ");
        p_os_print_error(u_concat(v_file, u_concat(&l__58__32__3122, v_message)));
        u_value t3123 = u_mk_bool(0);
        *r_ok = t3123;
      }
    }
  }
}

void p_ulang_46_located(u_value v_file, u_value v_m, u_value v_i, u_value *r_message) {
  {
    u_value v_text = NULL;
    u_value v_line = NULL;
    u_value v_column = NULL;
    if (u_truth(f_ulang_46_in_module_1(u_str_split(v_m, u_mk_char(32))))) {
      {
        static struct u_object l_parse_32_error_58__32__3124 = U_LIT("parse error: ");
        u_value t3125 = u_concat(&l_parse_32_error_58__32__3124, v_m);
        *r_message = t3125;
      }
    } else {
      {
        p_os_read(v_file, &v_text);
        p_lex_46_position(v_text, v_i, &v_line, &v_column);
        static struct u_object l_parse_32_error_58__32__3126 = U_LIT("parse error: ");
        static struct u_object l__32_at_32__3127 = U_LIT(" at ");
        static struct u_object l__46__3128 = U_LIT(".");
        u_value t3129 = u_concat(&l_parse_32_error_58__32__3126, u_concat(v_m, u_concat(&l__32_at_32__3127, u_concat(u_str_show(v_line), u_concat(&l__46__3128, u_str_show(v_column))))));
        *r_message = t3129;
      }
    }
  }
}

u_value f_ulang_46_in_module_1(u_value v_words) {
  u_value s3130 = v_words;
  u_value t3131 = NULL;
  if ((!t3131) && (u_is_con(s3130, &n_list_46_Cons) && (u_is_con(u_field(s3130, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s3130, 1), 1), &n_list_46_Nil)))) {
    u_value v_w = u_field(s3130, 0);
    static struct u_object l_in_3132 = U_LIT("in");
    t3131 = u_eq(v_w, &l_in_3132);
  }
  if ((!t3131) && u_is_con(s3130, &n_list_46_Cons)) {
    u_value v_more = u_field(s3130, 1);
    t3131 = f_ulang_46_in_module_1(v_more);
  }
  if (!t3131) {
    t3131 = u_mk_bool(0);
  }
  if (!t3131) {
    u_no_case(s3130);
  }
  return t3131;
}

void p_ulang_46_load_file(u_value v_file, u_value *r_q) {
  {
    u_value v_name = NULL;
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_text = NULL;
    u_value v_library = NULL;
    u_value v_own = NULL;
    u_value t3133 = f_parse_46_base_name_1(v_file);
    v_name = t3133;
    static struct u_object l_prelude_3134 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_3134, &v_pre);
    p_parse_46_parse_file_after(v_file, v_pre, &v_p);
    u_value t3135 = f_syntax_46_Program_46_cmds_1(v_p);
    v_own = t3135;
    static struct u_object l_prelude_3136 = U_LIT("prelude");
    if (u_truth(f_list_46_contains_2(v_name, u_mk_con(&n_list_46_Cons, 2, &l_prelude_3136, f_ulang_46_module_names_1(f_syntax_46_Program_46_cmds_1(v_pre)))))) {
      {
        p_os_read(v_file, &v_text);
        p_os_library(v_name, &v_library);
        if (u_truth(u_eq(v_text, v_library))) {
          {
            u_value t3137 = &o_list_46_Nil;
            u_value *e3138 = &t3137;
            u_value l3139 = f_syntax_46_Program_46_cmds_1(v_p);
            while (u_is_con(l3139, &n_list_46_Cons)) {
              u_value v_c = u_field(l3139, 0);
              if (u_truth(f_ulang_46_is_any_test_1(v_c))) {
                e3138 = u_append(e3138, v_c);
              }
              l3139 = u_field(l3139, 1);
            }
            u_value t3140 = t3137;
            v_own = t3140;
          }
        }
      }
    }
    static struct u_object l_prelude_3141 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_3141, f_syntax_46_Program_46_cmds_1(v_pre)), v_own)), v_name, r_q);
  }
}

u_value f_ulang_46_module_names_1(u_value v_cs) {
  u_value s3142 = v_cs;
  u_value t3143 = NULL;
  if ((!t3143) && u_is_con(s3142, &n_list_46_Nil)) {
    t3143 = &o_list_46_Nil;
  }
  if ((!t3143) && (u_is_con(s3142, &n_list_46_Cons) && u_is_con(u_field(s3142, 0), &n_syntax_46_CModule))) {
    u_value v_n = u_field(u_field(s3142, 0), 0);
    u_value v_rest = u_field(s3142, 1);
    t3143 = u_mk_con(&n_list_46_Cons, 2, v_n, f_ulang_46_module_names_1(v_rest));
  }
  if ((!t3143) && u_is_con(s3142, &n_list_46_Cons)) {
    u_value v_rest = u_field(s3142, 1);
    t3143 = f_ulang_46_module_names_1(v_rest);
  }
  if (!t3143) {
    u_no_case(s3142);
  }
  return t3143;
}

u_value f_ulang_46_is_any_test_1(u_value v_c) {
  u_value s3144 = v_c;
  u_value t3145 = NULL;
  if ((!t3145) && u_is_con(s3144, &n_syntax_46_CTest)) {
    t3145 = u_mk_bool(1);
  }
  if (!t3145) {
    t3145 = u_mk_bool(0);
  }
  if (!t3145) {
    u_no_case(s3144);
  }
  return t3145;
}

void p_ulang_46_check_file(u_value v_file, u_value *r_ok) {
  {
    u_value v_q = NULL;
    p_ulang_46_load_file(v_file, &v_q);
    u_value t3146 = u_mk_bool(1);
    *r_ok = t3146;
  }
}

void p_ulang_46_compile_to(u_value v_file, u_value v_entry, u_value v_target, u_value *r_ok) {
  {
    p_compile_46_compile_file(v_file, v_entry, v_target);
    u_value t3147 = u_mk_bool(1);
    *r_ok = t3147;
  }
}

void p_ulang_46_run_file(u_value v_file, u_value v_name, u_value v_args, u_value *r_ok) {
  {
    u_value v_q = NULL;
    u_value v_p = NULL;
    u_value v_cache = NULL;
    u_value v_n = NULL;
    u_value t3148 = f_map_46_empty_0();
    v_cache = t3148;
    u_value t3149 = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_parse_46_base_name_1(v_file)), v_name);
    v_n = t3149;
    p_ulang_46_load_file(v_file, &v_q);
    u_value t3150 = f_run_46_Prog_46_args_58__61__2(f_run_46_load_1(v_q), v_args);
    v_p = t3150;
    if (u_truth(f_list_46_exists_2(f_syntax_46_Program_46_cmds_1(v_q), u_mk_closure(lam3151, 1, v_n)))) {
      {
        p_run_46_run_proc(v_p, v_n, &v_cache);
        u_value t3152 = u_mk_bool(1);
        *r_ok = t3152;
      }
    } else {
      {
        static struct u_object l__58__32_runtime_32_error_58__3153 = U_LIT(": runtime error: there is no procedure ");
        static struct u_object l__32_without_32_paramet_3154 = U_LIT(" without parameters");
        p_os_print_error(u_concat(v_file, u_concat(&l__58__32_runtime_32_error_58__3153, u_concat(v_name, &l__32_without_32_paramet_3154))));
        u_value t3155 = u_mk_bool(0);
        *r_ok = t3155;
      }
    }
  }
}

void p_ulang_46_test_file(u_value v_file, u_value v_names, u_value v_args, u_value *r_ok) {
  {
    u_value v_q = NULL;
    u_value v_p = NULL;
    u_value v_tests = NULL;
    u_value v_missing = NULL;
    p_ulang_46_load_file(v_file, &v_q);
    u_value t3156 = f_run_46_Prog_46_args_58__61__2(f_run_46_load_1(v_q), v_args);
    v_p = t3156;
    u_value t3157 = &o_list_46_Nil;
    u_value *e3158 = &t3157;
    u_value l3159 = f_syntax_46_Program_46_cmds_1(v_q);
    while (u_is_con(l3159, &n_list_46_Cons)) {
      u_value v_c = u_field(l3159, 0);
      if (u_truth(f_ulang_46_is_selected_test_2(v_c, v_names))) {
        e3158 = u_append(e3158, v_c);
      }
      l3159 = u_field(l3159, 1);
    }
    u_value t3160 = t3157;
    v_tests = t3160;
    u_value t3161 = &o_list_46_Nil;
    u_value *e3162 = &t3161;
    u_value l3163 = v_names;
    while (u_is_con(l3163, &n_list_46_Cons)) {
      u_value v_n = u_field(l3163, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_exists_2(v_tests, u_mk_closure(lam3164, 1, v_n))))) {
        e3162 = u_append(e3162, v_n);
      }
      l3163 = u_field(l3163, 1);
    }
    u_value t3165 = t3161;
    v_missing = t3165;
    u_value s3166 = v_missing;
    int d3167 = 0;
    if ((!d3167) && u_is_con(s3166, &n_list_46_Cons)) {
      u_value v_n = u_field(s3166, 0);
      d3167 = 1;
      {
        static struct u_object l__58__32_runtime_32_error_58__3168 = U_LIT(": runtime error: there is no test ");
        p_os_print_error(u_concat(v_file, u_concat(&l__58__32_runtime_32_error_58__3168, v_n)));
        u_value t3169 = u_mk_bool(0);
        *r_ok = t3169;
      }
    }
    if (!d3167) {
      d3167 = 1;
      {
        p_ulang_46_run_tests(v_p, v_tests, r_ok);
      }
    }
    if (!d3167) {
      u_no_case(s3166);
    }
  }
}

void p_ulang_46_run_tests(u_value v_p, u_value v_tests, u_value *r_ok) {
  {
    u_value v_passed = NULL;
    u_value v_rest = NULL;
    u_value s3170 = v_tests;
    int d3171 = 0;
    if ((!d3171) && u_is_con(s3170, &n_list_46_Nil)) {
      d3171 = 1;
      {
        u_value t3172 = u_mk_bool(1);
        *r_ok = t3172;
      }
    }
    if ((!d3171) && u_is_con(s3170, &n_list_46_Cons)) {
      u_value v_c = u_field(s3170, 0);
      u_value v_more = u_field(s3170, 1);
      d3171 = 1;
      {
        p_ulang_46_run_test(v_p, v_c, &v_passed);
        p_ulang_46_run_tests(v_p, v_more, &v_rest);
        u_value t3173 = v_passed;
        if (u_truth(t3173)) {
          t3173 = v_rest;
        }
        u_value t3174 = t3173;
        *r_ok = t3174;
      }
    }
    if (!d3171) {
      u_no_case(s3170);
    }
  }
}

void p_ulang_46_run_test(u_value v_p, u_value v_c, u_value *r_passed) {
  {
    u_value v_cache = NULL;
    u_value v_message = NULL;
    u_value t3175 = f_map_46_empty_0();
    v_cache = t3175;
    static struct u_object l__3176 = U_LIT("");
    u_value t3177 = &l__3176;
    v_message = t3177;
    {
      struct u_handler *h3178 = u_push();
      if (setjmp(h3178->env) == 0) {
        {
          p_run_46_run_cmd(v_p, v_c, &v_cache);
        }
        u_pop(h3178);
      } else {
        u_pop(h3178);
        int d3179 = 0;
        if ((!d3179) && (u_has_type(h3178, "run.Thrown") && u_is_con(u_caught(h3178), &n_run_46_Thrown))) {
          u_value v_v = u_field(u_caught(h3178), 0);
          d3179 = 1;
          {
            static struct u_object l_runtime_32_error_58__32_u_3180 = U_LIT("runtime error: uncaught exception ");
            u_value t3181 = u_concat(&l_runtime_32_error_58__32_u_3180, f_print_46_show_value_1(v_v));
            v_message = t3181;
          }
        }
        if ((!d3179) && u_has_type(h3178, "run.RuntimeError")) {
          u_value v_e = u_caught(h3178);
          d3179 = 1;
          {
            static struct u_object l_runtime_32_error_58__32__3182 = U_LIT("runtime error: ");
            u_value t3183 = u_concat(&l_runtime_32_error_58__32__3182, f_ulang_46_runtime_message_1(v_e));
            v_message = t3183;
          }
        }
        if (!d3179) {
          u_reraise(h3178);
        }
      }
    }
    static struct u_object l__3184 = U_LIT("");
    if (u_truth(u_eq(v_message, &l__3184))) {
      {
        static struct u_object l_test_32__3185 = U_LIT("test ");
        static struct u_object l__32_passed_3186 = U_LIT(" passed");
        p_os_print(u_concat(&l_test_32__3185, u_concat(f_ulang_46_cmd_name_1(v_c), &l__32_passed_3186)));
        u_value t3187 = u_mk_bool(1);
        *r_passed = t3187;
      }
    } else {
      {
        static struct u_object l_test_32__3188 = U_LIT("test ");
        static struct u_object l__32_failed_58__32__3189 = U_LIT(" failed: ");
        p_os_print(u_concat(&l_test_32__3188, u_concat(f_ulang_46_cmd_name_1(v_c), u_concat(&l__32_failed_58__32__3189, v_message))));
        u_value t3190 = u_mk_bool(0);
        *r_passed = t3190;
      }
    }
  }
}

u_value f_ulang_46_is_selected_test_2(u_value v_c, u_value v_names) {
  u_value s3191 = v_c;
  u_value t3192 = NULL;
  if ((!t3192) && u_is_con(s3191, &n_syntax_46_CTest)) {
    u_value v_n = u_field(s3191, 0);
    u_value t3193 = u_eq(v_names, &o_list_46_Nil);
    if (!u_truth(t3193)) {
      t3193 = f_list_46_contains_2(v_n, v_names);
    }
    t3192 = t3193;
  }
  if (!t3192) {
    t3192 = u_mk_bool(0);
  }
  if (!t3192) {
    u_no_case(s3191);
  }
  return t3192;
}

u_value f_ulang_46_is_test_2(u_value v_c, u_value v_n) {
  u_value s3194 = v_c;
  u_value t3195 = NULL;
  if ((!t3195) && u_is_con(s3194, &n_syntax_46_CTest)) {
    u_value v_m = u_field(s3194, 0);
    t3195 = u_eq(v_m, v_n);
  }
  if (!t3195) {
    t3195 = u_mk_bool(0);
  }
  if (!t3195) {
    u_no_case(s3194);
  }
  return t3195;
}

u_value f_ulang_46_cmd_name_1(u_value v_c) {
  u_value s3196 = v_c;
  u_value t3197 = NULL;
  if ((!t3197) && u_is_con(s3196, &n_syntax_46_CTest)) {
    u_value v_n = u_field(s3196, 0);
    t3197 = v_n;
  }
  if (!t3197) {
    static struct u_object l__3198 = U_LIT("");
    t3197 = &l__3198;
  }
  if (!t3197) {
    u_no_case(s3196);
  }
  return t3197;
}

u_value f_ulang_46_type_message_1(u_value v_e) {
  u_value s3199 = v_e;
  u_value t3200 = NULL;
  if ((!t3200) && u_is_con(s3199, &n_check_46_UnknownSort)) {
    u_value v_n = u_field(s3199, 0);
    static struct u_object l_unknown_32_sort_32__3201 = U_LIT("unknown sort ");
    t3200 = u_concat(&l_unknown_32_sort_32__3201, f_syntax_46_Name_46_base_1(v_n));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_SortArity)) {
    u_value v_n = u_field(s3199, 0);
    u_value v_k = u_field(s3199, 1);
    u_value v_m = u_field(s3199, 2);
    static struct u_object l_sort_32__3202 = U_LIT("sort ");
    static struct u_object l__32_expects_32__3203 = U_LIT(" expects ");
    static struct u_object l__32_arguments_44__32_got_32__3204 = U_LIT(" arguments, got ");
    t3200 = u_concat(&l_sort_32__3202, u_concat(f_syntax_46_Name_46_base_1(v_n), u_concat(&l__32_expects_32__3203, u_concat(u_str_show(v_k), u_concat(&l__32_arguments_44__32_got_32__3204, u_str_show(v_m))))));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_SortDefined)) {
    u_value v_n = u_field(s3199, 0);
    static struct u_object l_sort_32__3205 = U_LIT("sort ");
    static struct u_object l__32_is_32_already_32_defi_3206 = U_LIT(" is already defined");
    t3200 = u_concat(&l_sort_32__3205, u_concat(f_syntax_46_Name_46_base_1(v_n), &l__32_is_32_already_32_defi_3206));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_MissingType)) {
    u_value v_n = u_field(s3199, 0);
    static struct u_object l_missing_32_type_32_for_3207 = U_LIT("missing type for ");
    t3200 = u_concat(&l_missing_32_type_32_for_3207, f_syntax_46_Name_46_base_1(v_n));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_UndeclaredParams)) {
    u_value v_n = u_field(s3199, 0);
    u_value v_ts = u_field(s3199, 1);
    static struct u_object l__32_has_32_undeclared_32__3208 = U_LIT(" has undeclared type parameters ");
    t3200 = u_concat(f_syntax_46_Name_46_base_1(v_n), u_concat(&l__32_has_32_undeclared_32__3208, f_print_46_show_types_1(v_ts)));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_CannotUnify)) {
    u_value v_a = u_field(s3199, 0);
    u_value v_b = u_field(s3199, 1);
    static struct u_object l_cannot_32_unify_32__3209 = U_LIT("cannot unify ");
    static struct u_object l__32_and_32__3210 = U_LIT(" and ");
    t3200 = u_concat(&l_cannot_32_unify_32__3209, u_concat(f_print_46_show_type_1(v_a), u_concat(&l__32_and_32__3210, f_print_46_show_type_1(v_b))));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_InfiniteType)) {
    u_value v_a = u_field(s3199, 0);
    u_value v_b = u_field(s3199, 1);
    static struct u_object l_infinite_32_type_32__3211 = U_LIT("infinite type ");
    static struct u_object l__32__61__32__3212 = U_LIT(" = ");
    t3200 = u_concat(&l_infinite_32_type_32__3211, u_concat(f_print_46_show_type_1(v_a), u_concat(&l__32__61__32__3212, f_print_46_show_type_1(v_b))));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_UnknownVariable)) {
    u_value v_n = u_field(s3199, 0);
    static struct u_object l_unknown_32_variable_3213 = U_LIT("unknown variable ");
    t3200 = u_concat(&l_unknown_32_variable_3213, f_syntax_46_Name_46_base_1(v_n));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_OverloadedUnapplied)) {
    u_value v_n = u_field(s3199, 0);
    static struct u_object l_overloaded_32__3214 = U_LIT("overloaded ");
    static struct u_object l__32_must_32_be_32_applied_3215 = U_LIT(" must be applied");
    t3200 = u_concat(&l_overloaded_32__3214, u_concat(f_syntax_46_Name_46_base_1(v_n), &l__32_must_32_be_32_applied_3215));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_WrongArity)) {
    u_value v_n = u_field(s3199, 0);
    u_value v_k = u_field(s3199, 1);
    static struct u_object l__32_does_32_not_32_take_32__3216 = U_LIT(" does not take ");
    static struct u_object l__32_arguments_3217 = U_LIT(" arguments");
    t3200 = u_concat(f_syntax_46_Name_46_base_1(v_n), u_concat(&l__32_does_32_not_32_take_32__3216, u_concat(u_str_show(v_k), &l__32_arguments_3217)));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_AlreadyDefined)) {
    u_value v_n = u_field(s3199, 0);
    static struct u_object l__32_is_32_already_32_defi_3218 = U_LIT(" is already defined");
    t3200 = u_concat(f_syntax_46_Name_46_base_1(v_n), &l__32_is_32_already_32_defi_3218);
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_Duplicate)) {
    u_value v_s = u_field(s3199, 0);
    static struct u_object l_duplicate_32__3219 = U_LIT("duplicate ");
    t3200 = u_concat(&l_duplicate_32__3219, v_s);
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_Ambiguous)) {
    u_value v_n = u_field(s3199, 0);
    u_value v_ns = u_field(s3199, 1);
    static struct u_object l__32_is_32_ambiguous_58__32__3220 = U_LIT(" is ambiguous: ");
    static struct u_object l__44__32__3221 = U_LIT(", ");
    u_value t3222 = &o_list_46_Nil;
    u_value *e3223 = &t3222;
    u_value l3224 = v_ns;
    while (u_is_con(l3224, &n_list_46_Cons)) {
      u_value v_m = u_field(l3224, 0);
      if (u_truth(u_mk_bool(1))) {
        e3223 = u_append(e3223, f_syntax_46_qualified_1(v_m));
      }
      l3224 = u_field(l3224, 1);
    }
    t3200 = u_concat(f_syntax_46_Name_46_base_1(v_n), u_concat(&l__32_is_32_ambiguous_58__32__3220, u_str_join(&l__44__32__3221, t3222)));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_UnknownField)) {
    u_value v_n = u_field(s3199, 0);
    t3200 = f_ulang_46_field_message_1(u_str_split(f_syntax_46_Name_46_base_1(v_n), u_mk_char(46)));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_UnknownFieldType)) {
    u_value v_f = u_field(s3199, 0);
    u_value v_t = u_field(s3199, 1);
    static struct u_object l_the_32_type_32__3225 = U_LIT("the type ");
    static struct u_object l__32_of_32_the_32_value_32_of_3226 = U_LIT(" of the value of ");
    static struct u_object l__32_is_32_not_32_known_3227 = U_LIT(" is not known");
    t3200 = u_concat(&l_the_32_type_32__3225, u_concat(f_print_46_show_type_1(v_t), u_concat(&l__32_of_32_the_32_value_32_of_3226, u_concat(v_f, &l__32_is_32_not_32_known_3227))));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_CountMismatch)) {
    u_value v_k = u_field(s3199, 0);
    u_value v_m = u_field(s3199, 1);
    static struct u_object l_expected_32__3228 = U_LIT("expected ");
    static struct u_object l__32_values_44__32_got_32__3229 = U_LIT(" values, got ");
    t3200 = u_concat(&l_expected_32__3228, u_concat(u_str_show(v_k), u_concat(&l__32_values_44__32_got_32__3229, u_str_show(v_m))));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_UnknownConstructor)) {
    u_value v_n = u_field(s3199, 0);
    static struct u_object l_unknown_32_construc_3230 = U_LIT("unknown constructor ");
    t3200 = u_concat(&l_unknown_32_construc_3230, f_syntax_46_Name_46_base_1(v_n));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_NotExhaustive)) {
    u_value v_p = u_field(s3199, 0);
    static struct u_object l_match_32_is_32_not_32_exh_3231 = U_LIT("match is not exhaustive, missing ");
    t3200 = u_concat(&l_match_32_is_32_not_32_exh_3231, f_print_46_show_pat_1(v_p));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_Unreachable)) {
    u_value v_p = u_field(s3199, 0);
    static struct u_object l_unreachable_32_case_3232 = U_LIT("unreachable case ");
    t3200 = u_concat(&l_unreachable_32_case_3232, f_print_46_show_pat_1(v_p));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_NotAssignable)) {
    u_value v_n = u_field(s3199, 0);
    static struct u_object l__32_is_32_not_32_assignab_3233 = U_LIT(" is not assignable");
    t3200 = u_concat(f_syntax_46_Name_46_base_1(v_n), &l__32_is_32_not_32_assignab_3233);
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_UnknownProcedure)) {
    u_value v_n = u_field(s3199, 0);
    static struct u_object l_unknown_32_procedur_3234 = U_LIT("unknown procedure ");
    t3200 = u_concat(&l_unknown_32_procedur_3234, f_syntax_46_Name_46_base_1(v_n));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_UnknownException)) {
    u_value v_t = u_field(s3199, 0);
    static struct u_object l_the_32_type_32__3235 = U_LIT("the type ");
    static struct u_object l__32_of_32_an_32_exception_3236 = U_LIT(" of an exception is not fully known");
    t3200 = u_concat(&l_the_32_type_32__3235, u_concat(f_print_46_show_type_1(v_t), &l__32_of_32_an_32_exception_3236));
  }
  if ((!t3200) && u_is_con(s3199, &n_check_46_TestParams)) {
    u_value v_n = u_field(s3199, 0);
    static struct u_object l_test_32__3237 = U_LIT("test ");
    static struct u_object l__32_takes_32_parameter_3238 = U_LIT(" takes parameters");
    t3200 = u_concat(&l_test_32__3237, u_concat(v_n, &l__32_takes_32_parameter_3238));
  }
  if (!t3200) {
    u_no_case(s3199);
  }
  return t3200;
}

u_value f_ulang_46_field_message_1(u_value v_parts) {
  u_value s3239 = v_parts;
  u_value t3240 = NULL;
  if ((!t3240) && (u_is_con(s3239, &n_list_46_Cons) && (u_is_con(u_field(s3239, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s3239, 1), 1), &n_list_46_Nil)))) {
    u_value v_t = u_field(s3239, 0);
    u_value v_f = u_field(u_field(s3239, 1), 0);
    static struct u_object l_unknown_32_field_32__3241 = U_LIT("unknown field ");
    static struct u_object l__32_of_32__3242 = U_LIT(" of ");
    t3240 = u_concat(&l_unknown_32_field_32__3241, u_concat(f_ulang_46_without_update_1(v_f), u_concat(&l__32_of_32__3242, v_t)));
  }
  if (!t3240) {
    static struct u_object l_unknown_32_field_32__3243 = U_LIT("unknown field ");
    static struct u_object l__46__3244 = U_LIT(".");
    t3240 = u_concat(&l_unknown_32_field_32__3243, u_str_join(&l__46__3244, v_parts));
  }
  if (!t3240) {
    u_no_case(s3239);
  }
  return t3240;
}

u_value f_ulang_46_without_update_1(u_value v_f) {
  u_value t3249 = u_str_size(v_f);
  u_value t3248;
  {
    u_value v_n = t3249;
    u_value t3246 = f_prelude_46__62__2(v_n, u_mk_int(2));
    if (u_truth(t3246)) {
      static struct u_object l__58__61__3245 = U_LIT(":=");
      t3246 = u_eq(u_str_slice(v_f, u_sub(v_n, u_mk_int(2)), v_n), &l__58__61__3245);
    }
    u_value t3247;
    if (u_truth(t3246)) {
      t3247 = u_str_slice(v_f, u_mk_int(0), u_sub(v_n, u_mk_int(2)));
    } else {
      t3247 = v_f;
    }
    t3248 = t3247;
  }
  return t3248;
}

u_value f_ulang_46_runtime_message_1(u_value v_e) {
  u_value s3250 = v_e;
  u_value t3251 = NULL;
  if ((!t3251) && u_is_con(s3250, &n_run_46_Uninitialized)) {
    u_value v_n = u_field(s3250, 0);
    static struct u_object l__32_is_32_not_32_initiali_3252 = U_LIT(" is not initialized");
    t3251 = u_concat(f_syntax_46_Name_46_base_1(v_n), &l__32_is_32_not_32_initiali_3252);
  }
  if ((!t3251) && u_is_con(s3250, &n_run_46_NotFunction)) {
    u_value v_v = u_field(s3250, 0);
    static struct u_object l_cannot_32_apply_32__3253 = U_LIT("cannot apply ");
    t3251 = u_concat(&l_cannot_32_apply_32__3253, f_print_46_show_value_1(v_v));
  }
  if ((!t3251) && u_is_con(s3250, &n_run_46_Unspecified)) {
    u_value v_n = u_field(s3250, 0);
    u_value v_vs = u_field(s3250, 1);
    static struct u_object l__32_is_32_not_32_specifie_3254 = U_LIT(" is not specified for ");
    static struct u_object l__44__32__3255 = U_LIT(", ");
    u_value t3256 = &o_list_46_Nil;
    u_value *e3257 = &t3256;
    u_value l3258 = v_vs;
    while (u_is_con(l3258, &n_list_46_Cons)) {
      u_value v_v = u_field(l3258, 0);
      if (u_truth(u_mk_bool(1))) {
        e3257 = u_append(e3257, f_print_46_show_value_1(v_v));
      }
      l3258 = u_field(l3258, 1);
    }
    t3251 = u_concat(f_syntax_46_Name_46_base_1(v_n), u_concat(&l__32_is_32_not_32_specifie_3254, u_str_join(&l__44__32__3255, t3256)));
  }
  if ((!t3251) && u_is_con(s3250, &n_run_46_NoCase)) {
    u_value v_v = u_field(s3250, 0);
    static struct u_object l_no_32_case_32_matches_32__3259 = U_LIT("no case matches ");
    t3251 = u_concat(&l_no_32_case_32_matches_32__3259, f_print_46_show_value_1(v_v));
  }
  if ((!t3251) && u_is_con(s3250, &n_run_46_CompareFunctions)) {
    static struct u_object l_cannot_32_compare_32_f_3260 = U_LIT("cannot compare functions");
    t3251 = &l_cannot_32_compare_32_f_3260;
  }
  if ((!t3251) && u_is_con(s3250, &n_run_46_NotList)) {
    u_value v_v = u_field(s3250, 0);
    static struct u_object l_expected_32_a_32_list_44__3261 = U_LIT("expected a list, got ");
    t3251 = u_concat(&l_expected_32_a_32_list_44__3261, f_print_46_show_value_1(v_v));
  }
  if ((!t3251) && u_is_con(s3250, &n_run_46_AssertionFailed)) {
    static struct u_object l_assertion_32_failed_3262 = U_LIT("assertion failed");
    t3251 = &l_assertion_32_failed_3262;
  }
  if ((!t3251) && u_is_con(s3250, &n_run_46_AssertionUnequal)) {
    u_value v_a = u_field(s3250, 0);
    u_value v_b = u_field(s3250, 1);
    static struct u_object l_assertion_32_failed_3263 = U_LIT("assertion failed: ");
    static struct u_object l__32__33__61__32__3264 = U_LIT(" != ");
    t3251 = u_concat(&l_assertion_32_failed_3263, u_concat(f_print_46_show_value_1(v_a), u_concat(&l__32__33__61__32__3264, f_print_46_show_value_1(v_b))));
  }
  if ((!t3251) && u_is_con(s3250, &n_run_46_UnassignedOutput)) {
    u_value v_x = u_field(s3250, 0);
    u_value v_p = u_field(s3250, 1);
    static struct u_object l_output_32__3265 = U_LIT("output ");
    static struct u_object l__32_of_32__3266 = U_LIT(" of ");
    static struct u_object l__32_is_32_not_32_assigned_3267 = U_LIT(" is not assigned");
    t3251 = u_concat(&l_output_32__3265, u_concat(f_syntax_46_Name_46_base_1(v_x), u_concat(&l__32_of_32__3266, u_concat(f_syntax_46_Name_46_base_1(v_p), &l__32_is_32_not_32_assigned_3267))));
  }
  if ((!t3251) && u_is_con(s3250, &n_run_46_UnknownProc)) {
    u_value v_n = u_field(s3250, 0);
    static struct u_object l_unknown_32_procedur_3268 = U_LIT("unknown procedure ");
    t3251 = u_concat(&l_unknown_32_procedur_3268, f_syntax_46_Name_46_base_1(v_n));
  }
  if ((!t3251) && u_is_con(s3250, &n_run_46_NotProc)) {
    u_value v_v = u_field(s3250, 0);
    static struct u_object l_cannot_32_call_32__3269 = U_LIT("cannot call ");
    t3251 = u_concat(&l_cannot_32_call_32__3269, f_print_46_show_value_1(v_v));
  }
  if ((!t3251) && u_is_con(s3250, &n_run_46_RecursiveInitialization)) {
    u_value v_n = u_field(s3250, 0);
    static struct u_object l_recursive_32_initia_3270 = U_LIT("recursive initialization of ");
    t3251 = u_concat(&l_recursive_32_initia_3270, f_syntax_46_Name_46_base_1(v_n));
  }
  if (!t3251) {
    u_no_case(s3250);
  }
  return t3251;
}

u_value lam215(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

u_value lam899(u_value *env, u_value *args) {
  u_value v_t = env[0];
  u_value v_e = args[0];
  return f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t);
}

u_value lam1284(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam1285(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam1287(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam1288(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam1298(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_f = args[0];
  return f_ctx_46_is_fun_2(f_check_46_State_46_ctx_1(v_s), v_f);
}

u_value lam1305(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam1309(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_p = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_p);
}

u_value lam1539(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_t = args[0];
  return f_ctx_46_is_sort_2(f_check_46_State_46_ctx_1(v_s), v_t);
}

u_value lam1650(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam1653(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam1724(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_q = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_q);
}

u_value lam3062(u_value *env, u_value *args) {
  u_value v_m = env[0];
  u_value v_entry = env[1];
  u_value v_c = args[0];
  return f_compile_46_is_entry_2(v_c, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), v_entry));
}

void proc3089(u_value *env, u_value *ins, u_value **outs) {
  u_value v_file = env[0];
  u_value v_rest = env[1];
  u_value *r_r = outs[0];
  {
    static struct u_object l_main_3090 = U_LIT("main");
    p_ulang_46_run_file(v_file, &l_main_3090, v_rest, r_r);
  }
}

void proc3091(u_value *env, u_value *ins, u_value **outs) {
  u_value v_file = env[0];
  u_value v_name = env[1];
  u_value v_rest = env[2];
  u_value *r_r = outs[0];
  {
    p_ulang_46_run_file(v_file, v_name, v_rest, r_r);
  }
}

void proc3092(u_value *env, u_value *ins, u_value **outs) {
  u_value v_file = env[0];
  u_value v_names = env[1];
  u_value v_rest = env[2];
  u_value *r_r = outs[0];
  {
    p_ulang_46_test_file(v_file, v_names, v_rest, r_r);
  }
}

void proc3093(u_value *env, u_value *ins, u_value **outs) {
  u_value v_file = env[0];
  u_value *r_r = outs[0];
  {
    p_ulang_46_check_file(v_file, r_r);
  }
}

void proc3094(u_value *env, u_value *ins, u_value **outs) {
  u_value v_file = env[0];
  u_value v_entry = env[1];
  u_value v_target = env[2];
  u_value *r_r = outs[0];
  {
    p_ulang_46_compile_to(v_file, v_entry, v_target, r_r);
  }
}

u_value lam3151(u_value *env, u_value *args) {
  u_value v_n = env[0];
  u_value v_c = args[0];
  return f_compile_46_is_entry_2(v_c, v_n);
}

u_value lam3164(u_value *env, u_value *args) {
  u_value v_n = env[0];
  u_value v_c = args[0];
  return f_ulang_46_is_test_2(v_c, v_n);
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  p_ulang_46_main();
  return 0;
}
