#include "ulang.h"

const struct u_name n_nat_46_Zero = {"nat", "Zero", 0};

const struct u_name n_nat_46_Succ = {"nat", "Succ", 1};

const struct u_name n_syntax_46_Name = {"syntax", "Name", 0};

const struct u_name n_syntax_46_THole = {"syntax", "THole", 0};

const struct u_name n_syntax_46_TMeta = {"syntax", "TMeta", 1};

const struct u_name n_syntax_46_TParam = {"syntax", "TParam", 2};

const struct u_name n_syntax_46_TSort = {"syntax", "TSort", 3};

const struct u_name n_syntax_46_TFun = {"syntax", "TFun", 4};

const struct u_name n_syntax_46_TTuple = {"syntax", "TTuple", 5};

const struct u_name n_syntax_46_VUnit = {"syntax", "VUnit", 0};

const struct u_name n_syntax_46_VBool = {"syntax", "VBool", 1};

const struct u_name n_syntax_46_VInt = {"syntax", "VInt", 2};

const struct u_name n_syntax_46_VStr = {"syntax", "VStr", 3};

const struct u_name n_syntax_46_VChar = {"syntax", "VChar", 4};

const struct u_name n_syntax_46_Var = {"syntax", "Var", 0};

const struct u_name n_syntax_46_ELit = {"syntax", "ELit", 0};

const struct u_name n_syntax_46_EVar = {"syntax", "EVar", 1};

const struct u_name n_syntax_46_EApp = {"syntax", "EApp", 2};

const struct u_name n_syntax_46_ELam = {"syntax", "ELam", 3};

const struct u_name n_syntax_46_ELet = {"syntax", "ELet", 4};

const struct u_name n_syntax_46_EIte = {"syntax", "EIte", 5};

const struct u_name n_syntax_46_EMatch = {"syntax", "EMatch", 6};

const struct u_name n_syntax_46_EComp = {"syntax", "EComp", 7};

const struct u_name n_syntax_46_ETuple = {"syntax", "ETuple", 8};

const struct u_name n_syntax_46_ERaise = {"syntax", "ERaise", 9};

const struct u_name n_syntax_46_PWild = {"syntax", "PWild", 0};

const struct u_name n_syntax_46_PLit = {"syntax", "PLit", 1};

const struct u_name n_syntax_46_PVar = {"syntax", "PVar", 2};

const struct u_name n_syntax_46_PApp = {"syntax", "PApp", 3};

const struct u_name n_syntax_46_PTuple = {"syntax", "PTuple", 4};

const struct u_name n_syntax_46_Branch = {"syntax", "Branch", 0};

const struct u_name n_syntax_46_SSkip = {"syntax", "SSkip", 0};

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

const struct u_name n_syntax_46_FPostfix = {"syntax", "FPostfix", 1};

const struct u_name n_syntax_46_FInfix = {"syntax", "FInfix", 2};

const struct u_name n_syntax_46_FLeft = {"syntax", "FLeft", 3};

const struct u_name n_syntax_46_FRight = {"syntax", "FRight", 4};

const struct u_name n_syntax_46_CSort = {"syntax", "CSort", 0};

const struct u_name n_syntax_46_CData = {"syntax", "CData", 1};

const struct u_name n_syntax_46_CFun = {"syntax", "CFun", 2};

const struct u_name n_syntax_46_CProc = {"syntax", "CProc", 3};

const struct u_name n_syntax_46_CModule = {"syntax", "CModule", 4};

const struct u_name n_syntax_46_CImport = {"syntax", "CImport", 5};

const struct u_name n_syntax_46_CNotation = {"syntax", "CNotation", 6};

const struct u_name n_syntax_46_CEval = {"syntax", "CEval", 7};

const struct u_name n_syntax_46_CRun = {"syntax", "CRun", 8};

const struct u_name n_syntax_46_CTest = {"syntax", "CTest", 9};

const struct u_name n_syntax_46_Program = {"syntax", "Program", 0};

const struct u_name n_map_46_MLeaf = {"map", "MLeaf", 0};

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

const struct u_name n_check_46_HCon = {"check", "HCon", 0};

const struct u_name n_check_46_HLit = {"check", "HLit", 1};

const struct u_name n_c_46_CtVoid = {"c", "CtVoid", 0};

const struct u_name n_c_46_CtInt = {"c", "CtInt", 1};

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

const struct u_name n_lex_46_TName = {"lex", "TName", 0};

const struct u_name n_lex_46_TSym = {"lex", "TSym", 1};

const struct u_name n_lex_46_TPunct = {"lex", "TPunct", 2};

const struct u_name n_lex_46_TNum = {"lex", "TNum", 3};

const struct u_name n_lex_46_TStr = {"lex", "TStr", 4};

const struct u_name n_lex_46_TChar = {"lex", "TChar", 5};

const struct u_name n_lex_46_TEnd = {"lex", "TEnd", 6};

const struct u_name n_lex_46_LexError = {"lex", "LexError", 0};

const struct u_name n_set_46_SLeaf = {"set", "SLeaf", 0};

const struct u_name n_set_46_SNode = {"set", "SNode", 1};

const struct u_name n_parse_46_ParseError = {"parse", "ParseError", 0};

const struct u_name n_parse_46_Notations = {"parse", "Notations", 0};

const struct u_name n_parse_46_Scope = {"parse", "Scope", 0};

const struct u_name n_parse_46_Operand = {"parse", "Operand", 0};

const struct u_name n_parse_46_Operator = {"parse", "Operator", 1};

const struct u_name n_parse_46_Ops = {"parse", "Ops", 0};

const struct u_name n_parse_46_Context = {"parse", "Context", 0};

const struct u_name n_compile_46_Unsupported = {"compile", "Unsupported", 0};

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

u_value w_f_check_46_ev_1(u_value *env, u_value *args);

u_value w_f_check_46_pv_1(u_value *env, u_value *args);

u_value w_f_check_46_num_1(u_value *env, u_value *args);

u_value w_f_check_46_ap_2(u_value *env, u_value *args);

u_value w_f_check_46_tl_1(u_value *env, u_value *args);

u_value w_f_check_46_lists_0(u_value *env, u_value *args);

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

u_value w_f_parse_46_expr_op2_3(u_value *env, u_value *args);

u_value w_f_parse_46_cons_exprs_2(u_value *env, u_value *args);

u_value w_f_parse_46_starts_expr_2(u_value *env, u_value *args);

u_value w_f_parse_46_at_update_1(u_value *env, u_value *args);

u_value w_f_parse_46_pattern_vars_1(u_value *env, u_value *args);

u_value w_f_parse_46_pat_op1_2(u_value *env, u_value *args);

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

u_value w_f_compile_46_Globals_46_funs_1(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_constrs_1(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_arities_1(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_fields_1(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_funs_58__61__2(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_constrs_58__61__2(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_arities_58__61__2(u_value *env, u_value *args);

u_value w_f_compile_46_Globals_46_fields_58__61__2(u_value *env, u_value *args);

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

u_value w_f_compile_46_runtime_constructors_0(u_value *env, u_value *args);

u_value w_f_compile_46_descriptor_decl_2(u_value *env, u_value *args);

u_value w_f_compile_46_descriptor_decls_1(u_value *env, u_value *args);

u_value w_f_compile_46_constructor_decls_2(u_value *env, u_value *args);

u_value w_f_compile_46_in_scope_1(u_value *env, u_value *args);

u_value w_f_compile_46_pointer_1(u_value *env, u_value *args);

u_value w_f_compile_46_var_expr_2(u_value *env, u_value *args);

u_value w_f_compile_46_address_1(u_value *env, u_value *args);

u_value w_f_compile_46_type_key_1(u_value *env, u_value *args);

u_value w_f_compile_46_globals_1(u_value *env, u_value *args);

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

u_value w_f_compile_46_free_in_branch_2(u_value *env, u_value *args);

u_value w_f_compile_46_lambdas_2(u_value *env, u_value *args);

u_value w_f_compile_46_rebind_2(u_value *env, u_value *args);

u_value w_f_compile_46_unpack_2(u_value *env, u_value *args);

u_value w_f_compile_46_reassign_3(u_value *env, u_value *args);

u_value w_f_compile_46_cached_3(u_value *env, u_value *args);

u_value w_f_compile_46_wrappers_1(u_value *env, u_value *args);

u_value w_f_compile_46_fun_wrapper_1(u_value *env, u_value *args);

u_value w_f_compile_46_builtin_wrapper_1(u_value *env, u_value *args);

u_value w_f_compile_46_wrapper_3(u_value *env, u_value *args);

u_value w_f_compile_46_constr_wrapper_1(u_value *env, u_value *args);

u_value w_f_compile_46_range_to_1(u_value *env, u_value *args);

u_value w_f_compile_46_prototype_1(u_value *env, u_value *args);

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

u_value wc_syntax_46_CEval(u_value *env, u_value *args);

u_value wc_syntax_46_CRun(u_value *env, u_value *args);

u_value wc_syntax_46_CTest(u_value *env, u_value *args);

u_value wc_syntax_46_Program(u_value *env, u_value *args);

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

u_value wc_check_46_HCon(u_value *env, u_value *args);

u_value wc_check_46_HLit(u_value *env, u_value *args);

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

u_value wc_compile_46_Unsupported(u_value *env, u_value *args);

u_value wc_compile_46_Globals(u_value *env, u_value *args);

u_value wc_compile_46_Gen(u_value *env, u_value *args);

u_value wu_add(u_value *env, u_value *args);

u_value wu_sub(u_value *env, u_value *args);

u_value wu_mul(u_value *env, u_value *args);

u_value wu_div(u_value *env, u_value *args);

u_value wu_mod(u_value *env, u_value *args);

u_value wu_lt(u_value *env, u_value *args);

u_value wu_eq(u_value *env, u_value *args);

u_value wu_compare(u_value *env, u_value *args);

u_value wu_chars(u_value *env, u_value *args);

u_value wu_string(u_value *env, u_value *args);

u_value wu_ord(u_value *env, u_value *args);

u_value wu_chr(u_value *env, u_value *args);

u_value wu_concat(u_value *env, u_value *args);

u_value wu_str_size(u_value *env, u_value *args);

u_value wu_str_join(u_value *env, u_value *args);

u_value wu_str_slice(u_value *env, u_value *args);

u_value wu_str_split(u_value *env, u_value *args);

u_value wu_str_get(u_value *env, u_value *args);

u_value wu_str_contains(u_value *env, u_value *args);

u_value wu_str_compare(u_value *env, u_value *args);

u_value wu_str_show(u_value *env, u_value *args);

u_value wu_str_quote(u_value *env, u_value *args);

u_value wu_str_number(u_value *env, u_value *args);

u_value wu_is_digit(u_value *env, u_value *args);

u_value wu_is_letter(u_value *env, u_value *args);

u_value wu_is_space(u_value *env, u_value *args);

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

u_value f_check_46_visibility_2(u_value v_ds, u_value v_main);

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

void p_check_46_one_of(u_value v_n, u_value v_ns, u_value *r_m);

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

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q);

void p_check_46_type_of(u_value v_e, u_value *r_t);

u_value f_check_46_ev_1(u_value v_n);

u_value f_check_46_pv_1(u_value v_n);

u_value f_check_46_num_1(u_value v_n);

u_value f_check_46_ap_2(u_value v_f, u_value v_args);

u_value f_check_46_tl_1(u_value v_t);

u_value f_check_46_lists_0(void);

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

void p_parse_46_parse_brackets(u_value v_sc, u_value *r_e, u_value *r_ts);

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts);

void p_parse_46_parse_expr_cases(u_value v_sc, u_value *r_cs, u_value *r_ts);

void p_parse_46_last_case(u_value *r_ts);

u_value f_parse_46_pat_op1_2(u_value v_op, u_value v_p);

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

void p_parse_46_skip_comma(u_value v_first, u_value *r_ts);

void p_parse_46_parse_variables(u_value v_sc, u_value *r_xs, u_value *r_ts);

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts);

void p_parse_46_parse_formals1(u_value v_sc, u_value *r_xs, u_value *r_ts);

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

u_value f_compile_46_Globals_46_funs_1(u_value v_x);

u_value f_compile_46_Globals_46_constrs_1(u_value v_x);

u_value f_compile_46_Globals_46_arities_1(u_value v_x);

u_value f_compile_46_Globals_46_fields_1(u_value v_x);

u_value f_compile_46_Globals_46_funs_58__61__2(u_value v_x, u_value v_y);

u_value f_compile_46_Globals_46_constrs_58__61__2(u_value v_x, u_value v_y);

u_value f_compile_46_Globals_46_arities_58__61__2(u_value v_x, u_value v_y);

u_value f_compile_46_Globals_46_fields_58__61__2(u_value v_x, u_value v_y);

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

u_value f_compile_46_runtime_constructors_0(void);

u_value f_compile_46_descriptor_decl_2(u_value v_n, u_value v_i);

u_value f_compile_46_descriptor_decls_1(u_value v_cs);

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i);

u_value f_compile_46_in_scope_1(u_value v_xs);

u_value f_compile_46_pointer_1(u_value v_x);

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x);

u_value f_compile_46_address_1(u_value v_c);

u_value f_compile_46_type_key_1(u_value v_t);

u_value f_compile_46_globals_1(u_value v_cs);

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

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound);

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b);

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g);

void p_compile_46_lift(u_value v_d, u_value *r_g);

void p_compile_46_global_value(u_value v_gl, u_value v_n, u_value v_t, u_value *r_r);

void p_compile_46_compile_expr(u_value v_gl, u_value v_env, u_value v_e, u_value *r_ss, u_value *r_r, u_value *r_g);

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g);

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g);

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts);

void p_compile_46_compile_named(u_value v_gl, u_value v_env, u_value v_n, u_value v_ty, u_value v_args, u_value *r_ss, u_value *r_r, u_value *r_g);

void p_compile_46_compile_lambda(u_value v_gl, u_value v_env, u_value v_xs, u_value v_b, u_value *r_ss, u_value *r_r, u_value *r_g);

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a);

void p_compile_46_expr_cases(u_value v_gl, u_value v_env, u_value v_cases, u_value v_v, u_value v_t, u_value *r_ss, u_value *r_g);

void p_compile_46_compile_loops(u_value v_gl, u_value v_env, u_value v_xs, u_value v_lists, u_value v_result, u_value v_c, u_value v_t, u_value *r_ss, u_value *r_g);

void p_compile_46_compile_stmt(u_value v_gl, u_value v_env, u_value v_s, u_value *r_ss, u_value *r_g);

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g);

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts);

void p_compile_46_handler_cases(u_value v_gl, u_value v_env, u_value v_hs, u_value v_h, u_value v_d, u_value *r_ss, u_value *r_g);

void p_compile_46_stmt_cases(u_value v_gl, u_value v_env, u_value v_cases, u_value v_v, u_value v_d, u_value *r_ss, u_value *r_g);

void p_compile_46_compile_cmd(u_value v_gl, u_value v_c, u_value *r_ds, u_value *r_ms, u_value *r_g);

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g);

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r);

u_value f_compile_46_wrappers_1(u_value v_gl);

u_value f_compile_46_fun_wrapper_1(u_value v_p);

u_value f_compile_46_builtin_wrapper_1(u_value v_p);

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k);

u_value f_compile_46_constr_wrapper_1(u_value v_p);

u_value f_compile_46_range_to_1(u_value v_k);

u_value f_compile_46_prototype_1(u_value v_d);

void p_compile_46_compile_program(u_value v_q, u_value *r_f);

void p_compile_46_compile_file(u_value v_source, u_value v_target);

u_value lam366(u_value *env, u_value *args);

u_value lam367(u_value *env, u_value *args);

u_value lam369(u_value *env, u_value *args);

u_value lam379(u_value *env, u_value *args);

u_value lam386(u_value *env, u_value *args);

u_value lam611(u_value *env, u_value *args);

u_value lam720(u_value *env, u_value *args);

u_value lam723(u_value *env, u_value *args);

u_value lam785(u_value *env, u_value *args);

u_value lam1062(u_value *env, u_value *args);

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

u_value w_f_parse_46_pat_op1_2(u_value *env, u_value *args) {
  return f_parse_46_pat_op1_2(args[0], args[1]);
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

u_value w_f_compile_46_runtime_constructors_0(u_value *env, u_value *args) {
  return f_compile_46_runtime_constructors_0();
}

u_value w_f_compile_46_descriptor_decl_2(u_value *env, u_value *args) {
  return f_compile_46_descriptor_decl_2(args[0], args[1]);
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

u_value w_f_compile_46_free_in_branch_2(u_value *env, u_value *args) {
  return f_compile_46_free_in_branch_2(args[0], args[1]);
}

u_value w_f_compile_46_lambdas_2(u_value *env, u_value *args) {
  return f_compile_46_lambdas_2(args[0], args[1]);
}

u_value w_f_compile_46_rebind_2(u_value *env, u_value *args) {
  return f_compile_46_rebind_2(args[0], args[1]);
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

u_value wc_syntax_46_CEval(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_CEval, 1, args);
}

u_value wc_syntax_46_CRun(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_CRun, 2, args);
}

u_value wc_syntax_46_CTest(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_CTest, 2, args);
}

u_value wc_syntax_46_Program(u_value *env, u_value *args) {
  return u_mk_con_array(&n_syntax_46_Program, 1, args);
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

u_value wc_check_46_HCon(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_HCon, 1, args);
}

u_value wc_check_46_HLit(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_HLit, 1, args);
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

u_value wc_compile_46_Unsupported(u_value *env, u_value *args) {
  return u_mk_con_array(&n_compile_46_Unsupported, 1, args);
}

u_value wc_compile_46_Globals(u_value *env, u_value *args) {
  return u_mk_con_array(&n_compile_46_Globals, 4, args);
}

u_value wc_compile_46_Gen(u_value *env, u_value *args) {
  return u_mk_con_array(&n_compile_46_Gen, 2, args);
}

u_value wu_add(u_value *env, u_value *args) {
  return u_add(args[0], args[1]);
}

u_value wu_sub(u_value *env, u_value *args) {
  return u_sub(args[0], args[1]);
}

u_value wu_mul(u_value *env, u_value *args) {
  return u_mul(args[0], args[1]);
}

u_value wu_div(u_value *env, u_value *args) {
  return u_div(args[0], args[1]);
}

u_value wu_mod(u_value *env, u_value *args) {
  return u_mod(args[0], args[1]);
}

u_value wu_lt(u_value *env, u_value *args) {
  return u_lt(args[0], args[1]);
}

u_value wu_eq(u_value *env, u_value *args) {
  return u_eq(args[0], args[1]);
}

u_value wu_compare(u_value *env, u_value *args) {
  return u_compare(args[0], args[1]);
}

u_value wu_chars(u_value *env, u_value *args) {
  return u_chars(args[0]);
}

u_value wu_string(u_value *env, u_value *args) {
  return u_string(args[0]);
}

u_value wu_ord(u_value *env, u_value *args) {
  return u_ord(args[0]);
}

u_value wu_chr(u_value *env, u_value *args) {
  return u_chr(args[0]);
}

u_value wu_concat(u_value *env, u_value *args) {
  return u_concat(args[0], args[1]);
}

u_value wu_str_size(u_value *env, u_value *args) {
  return u_str_size(args[0]);
}

u_value wu_str_join(u_value *env, u_value *args) {
  return u_str_join(args[0], args[1]);
}

u_value wu_str_slice(u_value *env, u_value *args) {
  return u_str_slice(args[0], args[1], args[2]);
}

u_value wu_str_split(u_value *env, u_value *args) {
  return u_str_split(args[0], args[1]);
}

u_value wu_str_get(u_value *env, u_value *args) {
  return u_str_get(args[0], args[1]);
}

u_value wu_str_contains(u_value *env, u_value *args) {
  return u_str_contains(args[0], args[1]);
}

u_value wu_str_compare(u_value *env, u_value *args) {
  return u_str_compare(args[0], args[1]);
}

u_value wu_str_show(u_value *env, u_value *args) {
  return u_str_show(args[0]);
}

u_value wu_str_quote(u_value *env, u_value *args) {
  return u_str_quote(args[0]);
}

u_value wu_str_number(u_value *env, u_value *args) {
  return u_str_number(args[0]);
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
    t10 = u_mk_con(&n_option_46_None, 0);
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
    t14 = u_mk_con(&n_option_46_None, 0);
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
      t16 = u_mk_con(&n_option_46_None, 0);
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
    t18 = u_mk_con(&n_list_46_Nil, 0);
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
    t23 = u_mk_con(&n_list_46_Nil, 0);
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
    t25 = u_mk_con(&n_list_46_Nil, 0);
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
    t36 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t36) && u_is_con(s35, &n_list_46_Cons)) {
    u_value v_y = u_field(s35, 0);
    u_value v_ys = u_field(s35, 1);
    u_value t37 = u_mk_con(&n_list_46_Nil, 0);
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
    t41 = u_mk_con(&n_option_46_None, 0);
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
  u_value t43 = u_mk_con(&n_list_46_Nil, 0);
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
    t47 = u_mk_con(&n_list_46_Nil, 0);
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
  return f_list_46_reverse_onto_2(v_xs, u_mk_con(&n_list_46_Nil, 0));
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
    t51 = u_mk_con(&n_list_46_Nil, 0);
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
    t53 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t53) && u_is_con(s52, &n_list_46_Cons)) {
    u_value v_y = u_field(s52, 0);
    u_value v_ys = u_field(s52, 1);
    u_value t54;
    if (u_truth(u_eq(v_n, u_mk_int(0)))) {
      t54 = u_mk_con(&n_list_46_Nil, 0);
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
    t56 = u_mk_con(&n_list_46_Nil, 0);
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
    t58 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t58 = u_mk_con(&n_list_46_Cons, 2, v_x, f_list_46_replicate_2(u_sub(v_n, u_mk_int(1)), v_x));
  }
  return t58;
}

u_value f_list_46_firsts_1(u_value v_ps) {
  u_value s59 = v_ps;
  u_value t60 = NULL;
  if ((!t60) && u_is_con(s59, &n_list_46_Nil)) {
    t60 = u_mk_con(&n_list_46_Nil, 0);
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
    t62 = u_mk_con(&n_list_46_Nil, 0);
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
  return u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_None, 0), v_x);
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
  u_value t145 = u_mk_con(&n_list_46_Nil, 0);
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
  u_value t148 = u_mk_con(&n_list_46_Nil, 0);
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
    t152 = f_prelude_46__45__1(u_mk_int(1));
  }
  if (!t152) {
    u_no_case(s151);
  }
  return t152;
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
  cache = u_mk_con(&n_map_46_MLeaf, 0);
  return cache;
}

u_value f_map_46_get_2(u_value v_k, u_value v_m) {
  u_value s153 = v_m;
  u_value t154 = NULL;
  if ((!t154) && u_is_con(s153, &n_map_46_MLeaf)) {
    t154 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t154) && u_is_con(s153, &n_map_46_MNode)) {
    u_value v_l = u_field(s153, 0);
    u_value v_j = u_field(s153, 1);
    u_value v_v = u_field(s153, 2);
    u_value v_r = u_field(s153, 3);
    u_value t158 = u_compare(v_k, v_j);
    u_value t157;
    {
      u_value v_c = t158;
      u_value t156;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t156 = f_map_46_get_2(v_k, v_l);
      } else {
        u_value t155;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t155 = f_map_46_get_2(v_k, v_r);
        } else {
          t155 = u_mk_con(&n_option_46_Some, 1, v_v);
        }
        t156 = t155;
      }
      t157 = t156;
    }
    t154 = t157;
  }
  if (!t154) {
    u_no_case(s153);
  }
  return t154;
}

u_value f_map_46_has_2(u_value v_k, u_value v_m) {
  return f_prelude_46__33__61__2(f_map_46_get_2(v_k, v_m), u_mk_con(&n_option_46_None, 0));
}

u_value f_map_46_put_3(u_value v_k, u_value v_v, u_value v_m) {
  u_value s159 = v_m;
  u_value t160 = NULL;
  if ((!t160) && u_is_con(s159, &n_map_46_MLeaf)) {
    t160 = u_mk_con(&n_map_46_MNode, 5, u_mk_con(&n_map_46_MLeaf, 0), v_k, v_v, u_mk_con(&n_map_46_MLeaf, 0), u_mk_int(1));
  }
  if ((!t160) && u_is_con(s159, &n_map_46_MNode)) {
    u_value v_l = u_field(s159, 0);
    u_value v_j = u_field(s159, 1);
    u_value v_w = u_field(s159, 2);
    u_value v_r = u_field(s159, 3);
    u_value v_h = u_field(s159, 4);
    u_value t164 = u_compare(v_k, v_j);
    u_value t163;
    {
      u_value v_c = t164;
      u_value t162;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t162 = f_map_46_balance_4(f_map_46_put_3(v_k, v_v, v_l), v_j, v_w, v_r);
      } else {
        u_value t161;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t161 = f_map_46_balance_4(v_l, v_j, v_w, f_map_46_put_3(v_k, v_v, v_r));
        } else {
          t161 = u_mk_con(&n_map_46_MNode, 5, v_l, v_k, v_v, v_r, v_h);
        }
        t162 = t161;
      }
      t163 = t162;
    }
    t160 = t163;
  }
  if (!t160) {
    u_no_case(s159);
  }
  return t160;
}

u_value f_map_46_entries_1(u_value v_m) {
  return f_map_46_entries_onto_2(v_m, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_map_46_entries_onto_2(u_value v_m, u_value v_rest) {
  u_value s165 = v_m;
  u_value t166 = NULL;
  if ((!t166) && u_is_con(s165, &n_map_46_MLeaf)) {
    t166 = v_rest;
  }
  if ((!t166) && u_is_con(s165, &n_map_46_MNode)) {
    u_value v_l = u_field(s165, 0);
    u_value v_k = u_field(s165, 1);
    u_value v_v = u_field(s165, 2);
    u_value v_r = u_field(s165, 3);
    t166 = f_map_46_entries_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_v), f_map_46_entries_onto_2(v_r, v_rest)));
  }
  if (!t166) {
    u_no_case(s165);
  }
  return t166;
}

u_value f_map_46_size_1(u_value v_m) {
  u_value s167 = v_m;
  u_value t168 = NULL;
  if ((!t168) && u_is_con(s167, &n_map_46_MLeaf)) {
    t168 = u_mk_int(0);
  }
  if ((!t168) && u_is_con(s167, &n_map_46_MNode)) {
    u_value v_l = u_field(s167, 0);
    u_value v_r = u_field(s167, 3);
    t168 = u_add(u_add(f_map_46_size_1(v_l), u_mk_int(1)), f_map_46_size_1(v_r));
  }
  if (!t168) {
    u_no_case(s167);
  }
  return t168;
}

u_value f_map_46_height_1(u_value v_m) {
  u_value s169 = v_m;
  u_value t170 = NULL;
  if ((!t170) && u_is_con(s169, &n_map_46_MLeaf)) {
    t170 = u_mk_int(0);
  }
  if ((!t170) && u_is_con(s169, &n_map_46_MNode)) {
    u_value v_h = u_field(s169, 4);
    t170 = v_h;
  }
  if (!t170) {
    u_no_case(s169);
  }
  return t170;
}

u_value f_map_46_mnode_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r) {
  return u_mk_con(&n_map_46_MNode, 5, v_l, v_k, v_v, v_r, f_map_46_mnode_height_2(v_l, v_r));
}

u_value f_map_46_rotate_left_1(u_value v_m) {
  u_value s171 = v_m;
  u_value t172 = NULL;
  if ((!t172) && (u_is_con(s171, &n_map_46_MNode) && u_is_con(u_field(s171, 3), &n_map_46_MNode))) {
    u_value v_a = u_field(s171, 0);
    u_value v_j = u_field(s171, 1);
    u_value v_v = u_field(s171, 2);
    u_value v_b = u_field(u_field(s171, 3), 0);
    u_value v_k = u_field(u_field(s171, 3), 1);
    u_value v_w = u_field(u_field(s171, 3), 2);
    u_value v_c = u_field(u_field(s171, 3), 3);
    t172 = f_map_46_mnode_4(f_map_46_mnode_4(v_a, v_j, v_v, v_b), v_k, v_w, v_c);
  }
  if (!t172) {
    t172 = v_m;
  }
  if (!t172) {
    u_no_case(s171);
  }
  return t172;
}

u_value f_map_46_rotate_right_1(u_value v_m) {
  u_value s173 = v_m;
  u_value t174 = NULL;
  if ((!t174) && (u_is_con(s173, &n_map_46_MNode) && u_is_con(u_field(s173, 0), &n_map_46_MNode))) {
    u_value v_a = u_field(u_field(s173, 0), 0);
    u_value v_j = u_field(u_field(s173, 0), 1);
    u_value v_v = u_field(u_field(s173, 0), 2);
    u_value v_b = u_field(u_field(s173, 0), 3);
    u_value v_k = u_field(s173, 1);
    u_value v_w = u_field(s173, 2);
    u_value v_c = u_field(s173, 3);
    t174 = f_map_46_mnode_4(v_a, v_j, v_v, f_map_46_mnode_4(v_b, v_k, v_w, v_c));
  }
  if (!t174) {
    t174 = v_m;
  }
  if (!t174) {
    u_no_case(s173);
  }
  return t174;
}

u_value f_map_46_left_of_1(u_value v_m) {
  u_value s175 = v_m;
  u_value t176 = NULL;
  if ((!t176) && u_is_con(s175, &n_map_46_MNode)) {
    u_value v_l = u_field(s175, 0);
    t176 = v_l;
  }
  if (!t176) {
    t176 = v_m;
  }
  if (!t176) {
    u_no_case(s175);
  }
  return t176;
}

u_value f_map_46_right_of_1(u_value v_m) {
  u_value s177 = v_m;
  u_value t178 = NULL;
  if ((!t178) && u_is_con(s177, &n_map_46_MNode)) {
    u_value v_r = u_field(s177, 3);
    t178 = v_r;
  }
  if (!t178) {
    t178 = v_m;
  }
  if (!t178) {
    u_no_case(s177);
  }
  return t178;
}

u_value f_map_46_balance_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r) {
  u_value t182;
  if (u_truth(f_prelude_46__62__2(f_map_46_height_1(v_l), u_add(f_map_46_height_1(v_r), u_mk_int(1))))) {
    u_value t179;
    if (u_truth(u_lt(f_map_46_height_1(f_map_46_left_of_1(v_l)), f_map_46_height_1(f_map_46_right_of_1(v_l))))) {
      t179 = f_map_46_rotate_left_1(v_l);
    } else {
      t179 = v_l;
    }
    t182 = f_map_46_rotate_right_1(f_map_46_mnode_4(t179, v_k, v_v, v_r));
  } else {
    u_value t181;
    if (u_truth(f_prelude_46__62__2(f_map_46_height_1(v_r), u_add(f_map_46_height_1(v_l), u_mk_int(1))))) {
      u_value t180;
      if (u_truth(u_lt(f_map_46_height_1(f_map_46_right_of_1(v_r)), f_map_46_height_1(f_map_46_left_of_1(v_r))))) {
        t180 = f_map_46_rotate_right_1(v_r);
      } else {
        t180 = v_r;
      }
      t181 = f_map_46_rotate_left_1(f_map_46_mnode_4(v_l, v_k, v_v, t180));
    } else {
      t181 = f_map_46_mnode_4(v_l, v_k, v_v, v_r);
    }
    t182 = t181;
  }
  return t182;
}

u_value f_map_46_balanced_1(u_value v_m) {
  u_value s183 = v_m;
  u_value t184 = NULL;
  if ((!t184) && u_is_con(s183, &n_map_46_MLeaf)) {
    t184 = u_mk_bool(1);
  }
  if ((!t184) && u_is_con(s183, &n_map_46_MNode)) {
    u_value v_l = u_field(s183, 0);
    u_value v_r = u_field(s183, 3);
    u_value v_h = u_field(s183, 4);
    u_value t185 = f_map_46_balanced_1(v_l);
    if (u_truth(t185)) {
      t185 = f_map_46_balanced_1(v_r);
    }
    u_value t186 = t185;
    if (u_truth(t186)) {
      t186 = u_eq(v_h, f_map_46_mnode_height_2(v_l, v_r));
    }
    u_value t187 = t186;
    if (u_truth(t187)) {
      t187 = f_prelude_46__60__61__2(f_map_46_height_1(v_l), u_add(f_map_46_height_1(v_r), u_mk_int(1)));
    }
    u_value t188 = t187;
    if (u_truth(t188)) {
      t188 = f_prelude_46__60__61__2(f_map_46_height_1(v_r), u_add(f_map_46_height_1(v_l), u_mk_int(1)));
    }
    t184 = t188;
  }
  if (!t184) {
    u_no_case(s183);
  }
  return t184;
}

u_value f_map_46_mnode_height_2(u_value v_l, u_value v_r) {
  u_value t189;
  if (u_truth(u_lt(f_map_46_height_1(v_l), f_map_46_height_1(v_r)))) {
    t189 = f_map_46_height_1(v_r);
  } else {
    t189 = f_map_46_height_1(v_l);
  }
  return u_add(u_mk_int(1), t189);
}

u_value f_map_46_put_all_2(u_value v_ps, u_value v_m) {
  u_value s190 = v_ps;
  u_value t191 = NULL;
  if ((!t191) && u_is_con(s190, &n_list_46_Nil)) {
    t191 = v_m;
  }
  if ((!t191) && u_is_con(s190, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s190, 0), 0);
    u_value v_v = u_field(u_field(s190, 0), 1);
    u_value v_rest = u_field(s190, 1);
    t191 = f_map_46_put_all_2(v_rest, f_map_46_put_3(v_k, v_v, v_m));
  }
  if (!t191) {
    u_no_case(s190);
  }
  return t191;
}

u_value f_map_46_numbers_1(u_value v_n) {
  u_value t192;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t192 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t192 = f_list_46__43__43__2(f_map_46_numbers_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t192;
}

u_value f_ctx_46_Ctx_46_aliases_1(u_value v_x) {
  u_value s193 = v_x;
  u_value t194 = NULL;
  if ((!t194) && u_is_con(s193, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s193, 0);
    t194 = v_y;
  }
  if (!t194) {
    u_no_case(s193);
  }
  return t194;
}

u_value f_ctx_46_Ctx_46_funs_1(u_value v_x) {
  u_value s195 = v_x;
  u_value t196 = NULL;
  if ((!t196) && u_is_con(s195, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s195, 1);
    t196 = v_y;
  }
  if (!t196) {
    u_no_case(s195);
  }
  return t196;
}

u_value f_ctx_46_Ctx_46_procs_1(u_value v_x) {
  u_value s197 = v_x;
  u_value t198 = NULL;
  if ((!t198) && u_is_con(s197, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s197, 2);
    t198 = v_y;
  }
  if (!t198) {
    u_no_case(s197);
  }
  return t198;
}

u_value f_ctx_46_Ctx_46_datas_1(u_value v_x) {
  u_value s199 = v_x;
  u_value t200 = NULL;
  if ((!t200) && u_is_con(s199, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s199, 3);
    t200 = v_y;
  }
  if (!t200) {
    u_no_case(s199);
  }
  return t200;
}

u_value f_ctx_46_Ctx_46_members_1(u_value v_x) {
  u_value s201 = v_x;
  u_value t202 = NULL;
  if ((!t202) && u_is_con(s201, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s201, 4);
    t202 = v_y;
  }
  if (!t202) {
    u_no_case(s201);
  }
  return t202;
}

u_value f_ctx_46_Ctx_46_constrs_1(u_value v_x) {
  u_value s203 = v_x;
  u_value t204 = NULL;
  if ((!t204) && u_is_con(s203, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s203, 5);
    t204 = v_y;
  }
  if (!t204) {
    u_no_case(s203);
  }
  return t204;
}

u_value f_ctx_46_Ctx_46_named_1(u_value v_x) {
  u_value s205 = v_x;
  u_value t206 = NULL;
  if ((!t206) && u_is_con(s205, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s205, 6);
    t206 = v_y;
  }
  if (!t206) {
    u_no_case(s205);
  }
  return t206;
}

u_value f_ctx_46_Ctx_46_aliases_58__61__2(u_value v_x, u_value v_y) {
  u_value s207 = v_x;
  u_value t208 = NULL;
  if ((!t208) && u_is_con(s207, &n_ctx_46_Ctx)) {
    u_value v_a1 = u_field(s207, 1);
    u_value v_a2 = u_field(s207, 2);
    u_value v_a3 = u_field(s207, 3);
    u_value v_a4 = u_field(s207, 4);
    u_value v_a5 = u_field(s207, 5);
    u_value v_a6 = u_field(s207, 6);
    t208 = u_mk_con(&n_ctx_46_Ctx, 7, v_y, v_a1, v_a2, v_a3, v_a4, v_a5, v_a6);
  }
  if (!t208) {
    u_no_case(s207);
  }
  return t208;
}

u_value f_ctx_46_Ctx_46_funs_58__61__2(u_value v_x, u_value v_y) {
  u_value s209 = v_x;
  u_value t210 = NULL;
  if ((!t210) && u_is_con(s209, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s209, 0);
    u_value v_a2 = u_field(s209, 2);
    u_value v_a3 = u_field(s209, 3);
    u_value v_a4 = u_field(s209, 4);
    u_value v_a5 = u_field(s209, 5);
    u_value v_a6 = u_field(s209, 6);
    t210 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_y, v_a2, v_a3, v_a4, v_a5, v_a6);
  }
  if (!t210) {
    u_no_case(s209);
  }
  return t210;
}

u_value f_ctx_46_Ctx_46_procs_58__61__2(u_value v_x, u_value v_y) {
  u_value s211 = v_x;
  u_value t212 = NULL;
  if ((!t212) && u_is_con(s211, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s211, 0);
    u_value v_a1 = u_field(s211, 1);
    u_value v_a3 = u_field(s211, 3);
    u_value v_a4 = u_field(s211, 4);
    u_value v_a5 = u_field(s211, 5);
    u_value v_a6 = u_field(s211, 6);
    t212 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_a1, v_y, v_a3, v_a4, v_a5, v_a6);
  }
  if (!t212) {
    u_no_case(s211);
  }
  return t212;
}

u_value f_ctx_46_Ctx_46_datas_58__61__2(u_value v_x, u_value v_y) {
  u_value s213 = v_x;
  u_value t214 = NULL;
  if ((!t214) && u_is_con(s213, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s213, 0);
    u_value v_a1 = u_field(s213, 1);
    u_value v_a2 = u_field(s213, 2);
    u_value v_a4 = u_field(s213, 4);
    u_value v_a5 = u_field(s213, 5);
    u_value v_a6 = u_field(s213, 6);
    t214 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_a1, v_a2, v_y, v_a4, v_a5, v_a6);
  }
  if (!t214) {
    u_no_case(s213);
  }
  return t214;
}

u_value f_ctx_46_Ctx_46_members_58__61__2(u_value v_x, u_value v_y) {
  u_value s215 = v_x;
  u_value t216 = NULL;
  if ((!t216) && u_is_con(s215, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s215, 0);
    u_value v_a1 = u_field(s215, 1);
    u_value v_a2 = u_field(s215, 2);
    u_value v_a3 = u_field(s215, 3);
    u_value v_a5 = u_field(s215, 5);
    u_value v_a6 = u_field(s215, 6);
    t216 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_a1, v_a2, v_a3, v_y, v_a5, v_a6);
  }
  if (!t216) {
    u_no_case(s215);
  }
  return t216;
}

u_value f_ctx_46_Ctx_46_constrs_58__61__2(u_value v_x, u_value v_y) {
  u_value s217 = v_x;
  u_value t218 = NULL;
  if ((!t218) && u_is_con(s217, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s217, 0);
    u_value v_a1 = u_field(s217, 1);
    u_value v_a2 = u_field(s217, 2);
    u_value v_a3 = u_field(s217, 3);
    u_value v_a4 = u_field(s217, 4);
    u_value v_a6 = u_field(s217, 6);
    t218 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_a1, v_a2, v_a3, v_a4, v_y, v_a6);
  }
  if (!t218) {
    u_no_case(s217);
  }
  return t218;
}

u_value f_ctx_46_Ctx_46_named_58__61__2(u_value v_x, u_value v_y) {
  u_value s219 = v_x;
  u_value t220 = NULL;
  if ((!t220) && u_is_con(s219, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s219, 0);
    u_value v_a1 = u_field(s219, 1);
    u_value v_a2 = u_field(s219, 2);
    u_value v_a3 = u_field(s219, 3);
    u_value v_a4 = u_field(s219, 4);
    u_value v_a5 = u_field(s219, 5);
    t220 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_a1, v_a2, v_a3, v_a4, v_a5, v_y);
  }
  if (!t220) {
    u_no_case(s219);
  }
  return t220;
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

u_value f_ctx_46_alias_of_2(u_value v_c, u_value v_n) {
  return f_map_46_get_2(v_n, f_ctx_46_Ctx_46_aliases_1(v_c));
}

u_value f_ctx_46_schemes_of_2(u_value v_c, u_value v_n) {
  u_value s221 = f_map_46_get_2(v_n, f_ctx_46_Ctx_46_funs_1(v_c));
  u_value t222 = NULL;
  if ((!t222) && u_is_con(s221, &n_option_46_Some)) {
    u_value v_ss = u_field(s221, 0);
    t222 = v_ss;
  }
  if ((!t222) && u_is_con(s221, &n_option_46_None)) {
    t222 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t222) {
    u_no_case(s221);
  }
  return t222;
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
  u_value t223 = f_ctx_46_is_data_2(v_c, v_n);
  if (!u_truth(t223)) {
    t223 = f_ctx_46_is_alias_2(v_c, v_n);
  }
  return t223;
}

u_value f_ctx_46_names_of_2(u_value v_c, u_value v_b) {
  u_value s224 = f_map_46_get_2(v_b, f_ctx_46_Ctx_46_named_1(v_c));
  u_value t225 = NULL;
  if ((!t225) && u_is_con(s224, &n_option_46_Some)) {
    u_value v_ns = u_field(s224, 0);
    t225 = v_ns;
  }
  if ((!t225) && u_is_con(s224, &n_option_46_None)) {
    t225 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t225) {
    u_no_case(s224);
  }
  return t225;
}

u_value f_ctx_46_with_named_2(u_value v_bs, u_value v_n) {
  u_value s226 = f_map_46_get_2(f_syntax_46_Name_46_base_1(v_n), v_bs);
  u_value t227 = NULL;
  if ((!t227) && u_is_con(s226, &n_option_46_Some)) {
    u_value v_ns = u_field(s226, 0);
    u_value t228;
    if (u_truth(f_list_46_contains_2(v_n, v_ns))) {
      t228 = v_bs;
    } else {
      t228 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), f_list_46__43__43__2(v_ns, u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0))), v_bs);
    }
    t227 = t228;
  }
  if ((!t227) && u_is_con(s226, &n_option_46_None)) {
    t227 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_bs);
  }
  if (!t227) {
    u_no_case(s226);
  }
  return t227;
}

u_value f_ctx_46_with_alias_4(u_value v_c, u_value v_n, u_value v_ps, u_value v_t) {
  u_value s229 = v_c;
  u_value t230 = NULL;
  if ((!t230) && u_is_con(s229, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s229, 0);
    u_value v_f = u_field(s229, 1);
    u_value v_p = u_field(s229, 2);
    u_value v_d = u_field(s229, 3);
    u_value v_m = u_field(s229, 4);
    u_value v_k = u_field(s229, 5);
    u_value v_bs = u_field(s229, 6);
    t230 = u_mk_con(&n_ctx_46_Ctx, 7, f_map_46_put_3(v_n, u_mk_tuple(2, v_ps, v_t), v_a), v_f, v_p, v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t230) {
    u_no_case(s229);
  }
  return t230;
}

u_value f_ctx_46_with_fun_3(u_value v_c, u_value v_n, u_value v_sc) {
  u_value s231 = v_c;
  u_value t232 = NULL;
  if ((!t232) && u_is_con(s231, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s231, 0);
    u_value v_f = u_field(s231, 1);
    u_value v_p = u_field(s231, 2);
    u_value v_d = u_field(s231, 3);
    u_value v_m = u_field(s231, 4);
    u_value v_k = u_field(s231, 5);
    u_value v_bs = u_field(s231, 6);
    t232 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, f_map_46_put_3(v_n, f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), u_mk_con(&n_list_46_Cons, 2, v_sc, u_mk_con(&n_list_46_Nil, 0))), v_f), v_p, v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t232) {
    u_no_case(s231);
  }
  return t232;
}

u_value f_ctx_46_with_proc_3(u_value v_c, u_value v_n, u_value v_g) {
  u_value s233 = v_c;
  u_value t234 = NULL;
  if ((!t234) && u_is_con(s233, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s233, 0);
    u_value v_f = u_field(s233, 1);
    u_value v_p = u_field(s233, 2);
    u_value v_d = u_field(s233, 3);
    u_value v_m = u_field(s233, 4);
    u_value v_k = u_field(s233, 5);
    u_value v_bs = u_field(s233, 6);
    t234 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, f_map_46_put_3(v_n, v_g, v_p), v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t234) {
    u_no_case(s233);
  }
  return t234;
}

u_value f_ctx_46_with_data_3(u_value v_c, u_value v_n, u_value v_arity) {
  u_value s235 = v_c;
  u_value t236 = NULL;
  if ((!t236) && u_is_con(s235, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s235, 0);
    u_value v_f = u_field(s235, 1);
    u_value v_p = u_field(s235, 2);
    u_value v_d = u_field(s235, 3);
    u_value v_m = u_field(s235, 4);
    u_value v_k = u_field(s235, 5);
    u_value v_bs = u_field(s235, 6);
    t236 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, f_map_46_put_3(v_n, v_arity, v_d), v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t236) {
    u_no_case(s235);
  }
  return t236;
}

u_value f_ctx_46_with_members_3(u_value v_c, u_value v_n, u_value v_cs) {
  u_value s237 = v_c;
  u_value t238 = NULL;
  if ((!t238) && u_is_con(s237, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s237, 0);
    u_value v_f = u_field(s237, 1);
    u_value v_p = u_field(s237, 2);
    u_value v_d = u_field(s237, 3);
    u_value v_m = u_field(s237, 4);
    u_value v_k = u_field(s237, 5);
    u_value v_bs = u_field(s237, 6);
    t238 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, v_d, f_map_46_put_3(v_n, v_cs, v_m), v_k, v_bs);
  }
  if (!t238) {
    u_no_case(s237);
  }
  return t238;
}

u_value f_ctx_46_with_constr_3(u_value v_c, u_value v_n, u_value v_kc) {
  u_value s239 = v_c;
  u_value t240 = NULL;
  if ((!t240) && u_is_con(s239, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s239, 0);
    u_value v_f = u_field(s239, 1);
    u_value v_p = u_field(s239, 2);
    u_value v_d = u_field(s239, 3);
    u_value v_m = u_field(s239, 4);
    u_value v_k = u_field(s239, 5);
    u_value v_bs = u_field(s239, 6);
    t240 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, v_d, v_m, f_map_46_put_3(v_n, v_kc, v_k), f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t240) {
    u_no_case(s239);
  }
  return t240;
}

u_value f_check_46_State_46_ctx_1(u_value v_x) {
  u_value s241 = v_x;
  u_value t242 = NULL;
  if ((!t242) && u_is_con(s241, &n_check_46_State)) {
    u_value v_y = u_field(s241, 0);
    t242 = v_y;
  }
  if (!t242) {
    u_no_case(s241);
  }
  return t242;
}

u_value f_check_46_State_46_sub_1(u_value v_x) {
  u_value s243 = v_x;
  u_value t244 = NULL;
  if ((!t244) && u_is_con(s243, &n_check_46_State)) {
    u_value v_y = u_field(s243, 1);
    t244 = v_y;
  }
  if (!t244) {
    u_no_case(s243);
  }
  return t244;
}

u_value f_check_46_State_46_count_1(u_value v_x) {
  u_value s245 = v_x;
  u_value t246 = NULL;
  if ((!t246) && u_is_con(s245, &n_check_46_State)) {
    u_value v_y = u_field(s245, 2);
    t246 = v_y;
  }
  if (!t246) {
    u_no_case(s245);
  }
  return t246;
}

u_value f_check_46_State_46_module_1(u_value v_x) {
  u_value s247 = v_x;
  u_value t248 = NULL;
  if ((!t248) && u_is_con(s247, &n_check_46_State)) {
    u_value v_y = u_field(s247, 3);
    t248 = v_y;
  }
  if (!t248) {
    u_no_case(s247);
  }
  return t248;
}

u_value f_check_46_State_46_visible_1(u_value v_x) {
  u_value s249 = v_x;
  u_value t250 = NULL;
  if ((!t250) && u_is_con(s249, &n_check_46_State)) {
    u_value v_y = u_field(s249, 4);
    t250 = v_y;
  }
  if (!t250) {
    u_no_case(s249);
  }
  return t250;
}

u_value f_check_46_State_46_table_1(u_value v_x) {
  u_value s251 = v_x;
  u_value t252 = NULL;
  if ((!t252) && u_is_con(s251, &n_check_46_State)) {
    u_value v_y = u_field(s251, 5);
    t252 = v_y;
  }
  if (!t252) {
    u_no_case(s251);
  }
  return t252;
}

u_value f_check_46_State_46_ctx_58__61__2(u_value v_x, u_value v_y) {
  u_value s253 = v_x;
  u_value t254 = NULL;
  if ((!t254) && u_is_con(s253, &n_check_46_State)) {
    u_value v_a1 = u_field(s253, 1);
    u_value v_a2 = u_field(s253, 2);
    u_value v_a3 = u_field(s253, 3);
    u_value v_a4 = u_field(s253, 4);
    u_value v_a5 = u_field(s253, 5);
    t254 = u_mk_con(&n_check_46_State, 6, v_y, v_a1, v_a2, v_a3, v_a4, v_a5);
  }
  if (!t254) {
    u_no_case(s253);
  }
  return t254;
}

u_value f_check_46_State_46_sub_58__61__2(u_value v_x, u_value v_y) {
  u_value s255 = v_x;
  u_value t256 = NULL;
  if ((!t256) && u_is_con(s255, &n_check_46_State)) {
    u_value v_a0 = u_field(s255, 0);
    u_value v_a2 = u_field(s255, 2);
    u_value v_a3 = u_field(s255, 3);
    u_value v_a4 = u_field(s255, 4);
    u_value v_a5 = u_field(s255, 5);
    t256 = u_mk_con(&n_check_46_State, 6, v_a0, v_y, v_a2, v_a3, v_a4, v_a5);
  }
  if (!t256) {
    u_no_case(s255);
  }
  return t256;
}

u_value f_check_46_State_46_count_58__61__2(u_value v_x, u_value v_y) {
  u_value s257 = v_x;
  u_value t258 = NULL;
  if ((!t258) && u_is_con(s257, &n_check_46_State)) {
    u_value v_a0 = u_field(s257, 0);
    u_value v_a1 = u_field(s257, 1);
    u_value v_a3 = u_field(s257, 3);
    u_value v_a4 = u_field(s257, 4);
    u_value v_a5 = u_field(s257, 5);
    t258 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_y, v_a3, v_a4, v_a5);
  }
  if (!t258) {
    u_no_case(s257);
  }
  return t258;
}

u_value f_check_46_State_46_module_58__61__2(u_value v_x, u_value v_y) {
  u_value s259 = v_x;
  u_value t260 = NULL;
  if ((!t260) && u_is_con(s259, &n_check_46_State)) {
    u_value v_a0 = u_field(s259, 0);
    u_value v_a1 = u_field(s259, 1);
    u_value v_a2 = u_field(s259, 2);
    u_value v_a4 = u_field(s259, 4);
    u_value v_a5 = u_field(s259, 5);
    t260 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_a2, v_y, v_a4, v_a5);
  }
  if (!t260) {
    u_no_case(s259);
  }
  return t260;
}

u_value f_check_46_State_46_visible_58__61__2(u_value v_x, u_value v_y) {
  u_value s261 = v_x;
  u_value t262 = NULL;
  if ((!t262) && u_is_con(s261, &n_check_46_State)) {
    u_value v_a0 = u_field(s261, 0);
    u_value v_a1 = u_field(s261, 1);
    u_value v_a2 = u_field(s261, 2);
    u_value v_a3 = u_field(s261, 3);
    u_value v_a5 = u_field(s261, 5);
    t262 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_a2, v_a3, v_y, v_a5);
  }
  if (!t262) {
    u_no_case(s261);
  }
  return t262;
}

u_value f_check_46_State_46_table_58__61__2(u_value v_x, u_value v_y) {
  u_value s263 = v_x;
  u_value t264 = NULL;
  if ((!t264) && u_is_con(s263, &n_check_46_State)) {
    u_value v_a0 = u_field(s263, 0);
    u_value v_a1 = u_field(s263, 1);
    u_value v_a2 = u_field(s263, 2);
    u_value v_a3 = u_field(s263, 3);
    u_value v_a4 = u_field(s263, 4);
    t264 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_a2, v_a3, v_a4, v_y);
  }
  if (!t264) {
    u_no_case(s263);
  }
  return t264;
}

u_value f_check_46_with_ctx_2(u_value v_s, u_value v_c) {
  u_value s265 = v_s;
  u_value t266 = NULL;
  if ((!t266) && u_is_con(s265, &n_check_46_State)) {
    u_value v_su = u_field(s265, 1);
    u_value v_n = u_field(s265, 2);
    u_value v_m = u_field(s265, 3);
    u_value v_ms = u_field(s265, 4);
    u_value v_vs = u_field(s265, 5);
    t266 = u_mk_con(&n_check_46_State, 6, v_c, v_su, v_n, v_m, v_ms, v_vs);
  }
  if (!t266) {
    u_no_case(s265);
  }
  return t266;
}

u_value f_check_46_enter_2(u_value v_s, u_value v_m) {
  u_value s267 = v_s;
  u_value t268 = NULL;
  if ((!t268) && u_is_con(s267, &n_check_46_State)) {
    u_value v_c = u_field(s267, 0);
    u_value v_vs = u_field(s267, 5);
    t268 = u_mk_con(&n_check_46_State, 6, v_c, f_map_46_empty_0(), u_mk_int(0), v_m, f_check_46_sees_2(v_m, v_vs), v_vs);
  }
  if (!t268) {
    u_no_case(s267);
  }
  return t268;
}

u_value f_check_46_sees_2(u_value v_m, u_value v_vs) {
  u_value s269 = f_list_46_lookup_2(v_m, v_vs);
  u_value t270 = NULL;
  if ((!t270) && u_is_con(s269, &n_option_46_Some)) {
    u_value v_ms = u_field(s269, 0);
    t270 = v_ms;
  }
  if ((!t270) && u_is_con(s269, &n_option_46_None)) {
    t270 = u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t270) {
    u_no_case(s269);
  }
  return t270;
}

u_value f_check_46_start_2(u_value v_main, u_value v_vs) {
  return u_mk_con(&n_check_46_State, 6, f_check_46_initial_0(), f_map_46_empty_0(), u_mk_int(0), v_main, f_check_46_sees_2(v_main, v_vs), v_vs);
}

u_value f_check_46_flatten_2(u_value v_m, u_value v_cs) {
  u_value s271 = v_cs;
  u_value t272 = NULL;
  if ((!t272) && u_is_con(s271, &n_list_46_Nil)) {
    t272 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t272) && (u_is_con(s271, &n_list_46_Cons) && u_is_con(u_field(s271, 0), &n_syntax_46_CModule))) {
    u_value v_n = u_field(u_field(s271, 0), 0);
    u_value v_ds = u_field(u_field(s271, 0), 1);
    u_value v_rest = u_field(s271, 1);
    t272 = f_list_46__43__43__2(f_check_46_flatten_2(v_n, v_ds), f_check_46_flatten_2(v_m, v_rest));
  }
  if ((!t272) && u_is_con(s271, &n_list_46_Cons)) {
    u_value v_c = u_field(s271, 0);
    u_value v_rest = u_field(s271, 1);
    t272 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_m, f_check_46_qualify_2(v_m, v_c)), f_check_46_flatten_2(v_m, v_rest));
  }
  if (!t272) {
    u_no_case(s271);
  }
  return t272;
}

u_value f_check_46_in_module_2(u_value v_m, u_value v_n) {
  return u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), f_syntax_46_Name_46_base_1(v_n));
}

u_value f_check_46_qualify_2(u_value v_m, u_value v_c) {
  u_value s273 = v_c;
  u_value t274 = NULL;
  if ((!t274) && u_is_con(s273, &n_syntax_46_CSort)) {
    u_value v_n = u_field(s273, 0);
    u_value v_ps = u_field(s273, 1);
    u_value v_t = u_field(s273, 2);
    t274 = u_mk_con(&n_syntax_46_CSort, 3, f_check_46_in_module_2(v_m, v_n), v_ps, v_t);
  }
  if ((!t274) && u_is_con(s273, &n_syntax_46_CData)) {
    u_value v_n = u_field(s273, 0);
    u_value v_ps = u_field(s273, 1);
    u_value v_ks = u_field(s273, 2);
    u_value t275 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e276 = &t275;
    u_value l277 = v_ks;
    while (u_is_con(l277, &n_list_46_Cons)) {
      u_value v_k = u_field(l277, 0);
      if (u_truth(u_mk_bool(1))) {
        e276 = u_append(e276, f_check_46_qualify_constr_2(v_m, v_k));
      }
      l277 = u_field(l277, 1);
    }
    t274 = u_mk_con(&n_syntax_46_CData, 3, f_check_46_in_module_2(v_m, v_n), v_ps, t275);
  }
  if ((!t274) && u_is_con(s273, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s273, 0);
    u_value v_ps = u_field(s273, 1);
    u_value v_xss = u_field(s273, 2);
    u_value v_r = u_field(s273, 3);
    u_value v_b = u_field(s273, 4);
    t274 = u_mk_con(&n_syntax_46_CFun, 5, f_check_46_in_module_2(v_m, v_n), v_ps, v_xss, v_r, v_b);
  }
  if ((!t274) && u_is_con(s273, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s273, 0);
    u_value v_ps = u_field(s273, 1);
    u_value v_ins = u_field(s273, 2);
    u_value v_outs = u_field(s273, 3);
    u_value v_refs = u_field(s273, 4);
    u_value v_b = u_field(s273, 5);
    t274 = u_mk_con(&n_syntax_46_CProc, 6, f_check_46_in_module_2(v_m, v_n), v_ps, v_ins, v_outs, v_refs, v_b);
  }
  if (!t274) {
    t274 = v_c;
  }
  if (!t274) {
    u_no_case(s273);
  }
  return t274;
}

u_value f_check_46_qualify_constr_2(u_value v_m, u_value v_k) {
  u_value s278 = v_k;
  u_value t279 = NULL;
  if ((!t279) && u_is_con(s278, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s278, 0);
    u_value v_ts = u_field(s278, 1);
    t279 = u_mk_con(&n_syntax_46_Constr, 2, f_check_46_in_module_2(v_m, v_n), v_ts);
  }
  if (!t279) {
    u_no_case(s278);
  }
  return t279;
}

u_value f_check_46_imports_in_2(u_value v_ds, u_value v_m) {
  u_value s280 = v_ds;
  u_value t281 = NULL;
  if ((!t281) && u_is_con(s280, &n_list_46_Nil)) {
    t281 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t281) && (u_is_con(s280, &n_list_46_Cons) && u_is_con(u_field(u_field(s280, 0), 1), &n_syntax_46_CImport))) {
    u_value v_k = u_field(u_field(s280, 0), 0);
    u_value v_n = u_field(u_field(u_field(s280, 0), 1), 0);
    u_value v_rest = u_field(s280, 1);
    if (u_truth(u_eq(v_k, v_m))) {
      t281 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_imports_in_2(v_rest, v_m));
    }
  }
  if ((!t281) && u_is_con(s280, &n_list_46_Cons)) {
    u_value v_rest = u_field(s280, 1);
    t281 = f_check_46_imports_in_2(v_rest, v_m);
  }
  if (!t281) {
    u_no_case(s280);
  }
  return t281;
}

u_value f_check_46_visibility_2(u_value v_ds, u_value v_main) {
  u_value t282 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e283 = &t282;
  u_value l284 = f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_main, u_mk_con(&n_list_46_Nil, 0)), f_list_46_firsts_1(v_ds)));
  while (u_is_con(l284, &n_list_46_Cons)) {
    u_value v_m = u_field(l284, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_prelude_285 = U_LIT("prelude");
      static struct u_object l_prelude_286 = U_LIT("prelude");
      e283 = u_append(e283, u_mk_tuple(2, v_m, f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_check_46_imports_in_2(v_ds, v_m), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, &l_prelude_285, u_mk_con(&n_list_46_Nil, 0)), f_check_46_imports_in_2(v_ds, &l_prelude_286)))))));
    }
    l284 = u_field(l284, 1);
  }
  return t282;
}

u_value f_check_46_visible_names_3(u_value v_s, u_value v_n, u_value v_declared) {
  u_value t287 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e288 = &t287;
  u_value l289 = f_ctx_46_names_of_2(f_check_46_State_46_ctx_1(v_s), f_syntax_46_Name_46_base_1(v_n));
  while (u_is_con(l289, &n_list_46_Cons)) {
    u_value v_d = u_field(l289, 0);
    if (u_truth(u_apply(v_declared, 1, v_d))) {
      e288 = u_append(e288, v_d);
    }
    l289 = u_field(l289, 1);
  }
  u_value t298 = t287;
  u_value t297;
  {
    u_value v_ds = t298;
    u_value s290 = v_n;
    u_value t291 = NULL;
    if ((!t291) && (u_is_con(s290, &n_syntax_46_Name) && u_is_con(u_field(s290, 0), &n_option_46_Some))) {
      u_value t292;
      if (u_truth(f_list_46_contains_2(v_n, v_ds))) {
        t292 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
      } else {
        t292 = u_mk_con(&n_list_46_Nil, 0);
      }
      t291 = t292;
    }
    if ((!t291) && (u_is_con(s290, &n_syntax_46_Name) && u_is_con(u_field(s290, 0), &n_option_46_None))) {
      u_value v_b = u_field(s290, 1);
      u_value t296;
      if (u_truth(f_list_46_contains_2(u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), v_ds))) {
        t296 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), u_mk_con(&n_list_46_Nil, 0));
      } else {
        u_value t293 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e294 = &t293;
        u_value l295 = v_ds;
        while (u_is_con(l295, &n_list_46_Cons)) {
          u_value v_d = u_field(l295, 0);
          if (u_truth(f_check_46_is_visible_2(v_s, v_d))) {
            e294 = u_append(e294, v_d);
          }
          l295 = u_field(l295, 1);
        }
        t296 = t293;
      }
      t291 = t296;
    }
    if (!t291) {
      u_no_case(s290);
    }
    t297 = t291;
  }
  return t297;
}

u_value f_check_46_is_visible_2(u_value v_s, u_value v_d) {
  u_value s299 = f_syntax_46_Name_46_module_1(v_d);
  u_value t300 = NULL;
  if ((!t300) && u_is_con(s299, &n_option_46_Some)) {
    u_value v_m = u_field(s299, 0);
    t300 = f_list_46_contains_2(v_m, f_check_46_State_46_visible_1(v_s));
  }
  if ((!t300) && u_is_con(s299, &n_option_46_None)) {
    t300 = u_mk_bool(0);
  }
  if (!t300) {
    u_no_case(s299);
  }
  return t300;
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
  static struct u_object l_Bool_301 = U_LIT("Bool");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Bool_301), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Int_302 = U_LIT("Int");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Int_302), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Str_303 = U_LIT("Str");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Str_303), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Char_304 = U_LIT("Char");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Char_304), u_mk_con(&n_list_46_Nil, 0));
  return cache;
}

u_value f_check_46_tlist_1(u_value v_t) {
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_list_name_0(), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_check_46_fun_2(u_value v_args, u_value v_res) {
  return u_mk_con(&n_syntax_46_TFun, 2, v_args, v_res);
}

u_value f_check_46_params_1(u_value v_ps) {
  u_value t305 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e306 = &t305;
  u_value l307 = v_ps;
  while (u_is_con(l307, &n_list_46_Cons)) {
    u_value v_p = u_field(l307, 0);
    if (u_truth(u_mk_bool(1))) {
      e306 = u_append(e306, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l307 = u_field(l307, 1);
  }
  return t305;
}

u_value f_check_46_basic_1(u_value v_n) {
  u_value s308 = v_n;
  u_value t309 = NULL;
  if ((!t309) && (u_is_con(s308, &n_syntax_46_Name) && u_is_con(u_field(s308, 0), &n_option_46_None))) {
    u_value v_b = u_field(s308, 1);
    static struct u_object l_Bool_310 = U_LIT("Bool");
    static struct u_object l_Int_311 = U_LIT("Int");
    static struct u_object l_Str_312 = U_LIT("Str");
    static struct u_object l_Char_313 = U_LIT("Char");
    t309 = f_list_46_contains_2(v_b, u_mk_con(&n_list_46_Cons, 2, &l_Bool_310, u_mk_con(&n_list_46_Cons, 2, &l_Int_311, u_mk_con(&n_list_46_Cons, 2, &l_Str_312, u_mk_con(&n_list_46_Cons, 2, &l_Char_313, u_mk_con(&n_list_46_Nil, 0))))));
  }
  if (!t309) {
    t309 = u_mk_bool(0);
  }
  if (!t309) {
    u_no_case(s308);
  }
  return t309;
}

u_value f_check_46_literal_1(u_value v_v) {
  u_value s314 = v_v;
  u_value t315 = NULL;
  if ((!t315) && u_is_con(s314, &n_syntax_46_VUnit)) {
    t315 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t315) && u_is_con(s314, &n_syntax_46_VBool)) {
    t315 = f_check_46_tbool_0();
  }
  if ((!t315) && u_is_con(s314, &n_syntax_46_VInt)) {
    t315 = f_check_46_tint_0();
  }
  if ((!t315) && u_is_con(s314, &n_syntax_46_VStr)) {
    t315 = f_check_46_tstr_0();
  }
  if ((!t315) && u_is_con(s314, &n_syntax_46_VChar)) {
    t315 = f_check_46_tchar_0();
  }
  if (!t315) {
    u_no_case(s314);
  }
  return t315;
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
  cache = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0()));
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
  cache = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tbool_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tbool_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0()));
  return cache;
}

u_value f_check_46_builtin_1(u_value v_n) {
  u_value s316 = v_n;
  u_value t317 = NULL;
  if ((!t317) && (u_is_con(s316, &n_syntax_46_Name) && u_is_con(u_field(s316, 0), &n_option_46_None))) {
    u_value v_b = u_field(s316, 1);
    t317 = f_check_46_builtin_scheme_1(v_b);
  }
  if (!t317) {
    t317 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t317) {
    u_no_case(s316);
  }
  return t317;
}

u_value f_check_46_builtin_scheme_1(u_value v_b) {
  static struct u_object l_A_318 = U_LIT("A");
  u_value t322 = u_mk_con(&n_syntax_46_TParam, 1, &l_A_318);
  u_value t321;
  {
    u_value v_a = t322;
    u_value s319 = v_b;
    u_value t320 = NULL;
    if ((!t320) && u_equals(s319, u_mk_lit("+"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("-"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("*"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("/"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("%"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("<"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("=="))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("compare"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("and"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("or"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("chars"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tlist_1(f_check_46_tchar_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("string"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tchar_0()), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("ord"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("chr"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("^"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("str_size"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("str_join"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("str_slice"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("str_split"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tlist_1(f_check_46_tstr_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("str_get"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("str_contains"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("str_compare"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("str_show"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("str_quote"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("str_number"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_option_name_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("is_digit"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("is_letter"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t320) && u_equals(s319, u_mk_lit("is_space"))) {
      t320 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if (!t320) {
      t320 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t320) {
      u_no_case(s319);
    }
    t321 = t320;
  }
  return t321;
}

u_value f_check_46_is_meta_1(u_value v_t) {
  u_value s323 = v_t;
  u_value t324 = NULL;
  if ((!t324) && u_is_con(s323, &n_syntax_46_TMeta)) {
    t324 = u_mk_bool(1);
  }
  if (!t324) {
    t324 = u_mk_bool(0);
  }
  if (!t324) {
    u_no_case(s323);
  }
  return t324;
}

u_value f_check_46_free_1(u_value v_t) {
  u_value s325 = v_t;
  u_value t326 = NULL;
  if ((!t326) && u_is_con(s325, &n_syntax_46_TParam)) {
    t326 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t326) && u_is_con(s325, &n_syntax_46_TMeta)) {
    t326 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t326) && u_is_con(s325, &n_syntax_46_THole)) {
    t326 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t326) && u_is_con(s325, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s325, 1);
    u_value t327 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e328 = &t327;
    u_value l329 = v_args;
    while (u_is_con(l329, &n_list_46_Cons)) {
      u_value v_a = u_field(l329, 0);
      if (u_truth(u_mk_bool(1))) {
        e328 = u_append(e328, f_check_46_free_1(v_a));
      }
      l329 = u_field(l329, 1);
    }
    t326 = f_list_46_distinct_1(f_list_46_concat_1(t327));
  }
  if ((!t326) && u_is_con(s325, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s325, 0);
    u_value v_r = u_field(s325, 1);
    u_value t330 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e331 = &t330;
    u_value l332 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l332, &n_list_46_Cons)) {
      u_value v_a = u_field(l332, 0);
      if (u_truth(u_mk_bool(1))) {
        e331 = u_append(e331, f_check_46_free_1(v_a));
      }
      l332 = u_field(l332, 1);
    }
    t326 = f_list_46_distinct_1(f_list_46_concat_1(t330));
  }
  if ((!t326) && u_is_con(s325, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s325, 0);
    u_value t333 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e334 = &t333;
    u_value l335 = v_ts;
    while (u_is_con(l335, &n_list_46_Cons)) {
      u_value v_a = u_field(l335, 0);
      if (u_truth(u_mk_bool(1))) {
        e334 = u_append(e334, f_check_46_free_1(v_a));
      }
      l335 = u_field(l335, 1);
    }
    t326 = f_list_46_distinct_1(f_list_46_concat_1(t333));
  }
  if (!t326) {
    u_no_case(s325);
  }
  return t326;
}

u_value f_check_46_holes_1(u_value v_t) {
  u_value s336 = v_t;
  u_value t337 = NULL;
  if ((!t337) && u_is_con(s336, &n_syntax_46_THole)) {
    t337 = u_mk_bool(1);
  }
  if ((!t337) && u_is_con(s336, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s336, 1);
    u_value t338 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e339 = &t338;
    u_value l340 = v_args;
    while (u_is_con(l340, &n_list_46_Cons)) {
      u_value v_a = u_field(l340, 0);
      if (u_truth(u_mk_bool(1))) {
        e339 = u_append(e339, f_check_46_holes_1(v_a));
      }
      l340 = u_field(l340, 1);
    }
    t337 = f_list_46_contains_2(u_mk_bool(1), t338);
  }
  if ((!t337) && u_is_con(s336, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s336, 0);
    u_value v_r = u_field(s336, 1);
    u_value t341 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e342 = &t341;
    u_value l343 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l343, &n_list_46_Cons)) {
      u_value v_a = u_field(l343, 0);
      if (u_truth(u_mk_bool(1))) {
        e342 = u_append(e342, f_check_46_holes_1(v_a));
      }
      l343 = u_field(l343, 1);
    }
    t337 = f_list_46_contains_2(u_mk_bool(1), t341);
  }
  if ((!t337) && u_is_con(s336, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s336, 0);
    u_value t344 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e345 = &t344;
    u_value l346 = v_ts;
    while (u_is_con(l346, &n_list_46_Cons)) {
      u_value v_a = u_field(l346, 0);
      if (u_truth(u_mk_bool(1))) {
        e345 = u_append(e345, f_check_46_holes_1(v_a));
      }
      l346 = u_field(l346, 1);
    }
    t337 = f_list_46_contains_2(u_mk_bool(1), t344);
  }
  if (!t337) {
    t337 = u_mk_bool(0);
  }
  if (!t337) {
    u_no_case(s336);
  }
  return t337;
}

u_value f_check_46_subst_2(u_value v_t, u_value v_m) {
  u_value s347 = v_t;
  u_value t348 = NULL;
  if ((!t348) && u_is_con(s347, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s347, 0);
    u_value v_args = u_field(s347, 1);
    u_value t349 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e350 = &t349;
    u_value l351 = v_args;
    while (u_is_con(l351, &n_list_46_Cons)) {
      u_value v_a = u_field(l351, 0);
      if (u_truth(u_mk_bool(1))) {
        e350 = u_append(e350, f_check_46_subst_2(v_a, v_m));
      }
      l351 = u_field(l351, 1);
    }
    t348 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t349);
  }
  if ((!t348) && u_is_con(s347, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s347, 0);
    u_value v_r = u_field(s347, 1);
    u_value t352 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e353 = &t352;
    u_value l354 = v_args;
    while (u_is_con(l354, &n_list_46_Cons)) {
      u_value v_a = u_field(l354, 0);
      if (u_truth(u_mk_bool(1))) {
        e353 = u_append(e353, f_check_46_subst_2(v_a, v_m));
      }
      l354 = u_field(l354, 1);
    }
    t348 = u_mk_con(&n_syntax_46_TFun, 2, t352, f_check_46_subst_2(v_r, v_m));
  }
  if ((!t348) && u_is_con(s347, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s347, 0);
    u_value t355 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e356 = &t355;
    u_value l357 = v_ts;
    while (u_is_con(l357, &n_list_46_Cons)) {
      u_value v_a = u_field(l357, 0);
      if (u_truth(u_mk_bool(1))) {
        e356 = u_append(e356, f_check_46_subst_2(v_a, v_m));
      }
      l357 = u_field(l357, 1);
    }
    t348 = u_mk_con(&n_syntax_46_TTuple, 1, t355);
  }
  if (!t348) {
    u_value s358 = f_list_46_lookup_2(v_t, v_m);
    u_value t359 = NULL;
    if ((!t359) && u_is_con(s358, &n_option_46_Some)) {
      u_value v_u = u_field(s358, 0);
      t359 = v_u;
    }
    if ((!t359) && u_is_con(s358, &n_option_46_None)) {
      t359 = v_t;
    }
    if (!t359) {
      u_no_case(s358);
    }
    t348 = t359;
  }
  if (!t348) {
    u_no_case(s347);
  }
  return t348;
}

u_value f_check_46_resolve_2(u_value v_su, u_value v_t) {
  u_value s360 = v_t;
  u_value t361 = NULL;
  if ((!t361) && u_is_con(s360, &n_syntax_46_TMeta)) {
    u_value v_i = u_field(s360, 0);
    u_value s362 = f_map_46_get_2(v_i, v_su);
    u_value t363 = NULL;
    if ((!t363) && u_is_con(s362, &n_option_46_Some)) {
      u_value v_u = u_field(s362, 0);
      t363 = f_check_46_resolve_2(v_su, v_u);
    }
    if ((!t363) && u_is_con(s362, &n_option_46_None)) {
      t363 = v_t;
    }
    if (!t363) {
      u_no_case(s362);
    }
    t361 = t363;
  }
  if (!t361) {
    t361 = v_t;
  }
  if (!t361) {
    u_no_case(s360);
  }
  return t361;
}

u_value f_check_46_occurs_3(u_value v_su, u_value v_i, u_value v_t) {
  u_value s364 = f_check_46_resolve_2(v_su, v_t);
  u_value t365 = NULL;
  if ((!t365) && u_is_con(s364, &n_syntax_46_TMeta)) {
    u_value v_j = u_field(s364, 0);
    t365 = u_eq(v_i, v_j);
  }
  if ((!t365) && u_is_con(s364, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s364, 1);
    t365 = f_list_46_exists_2(v_args, u_mk_closure(lam366, 2, v_su, v_i));
  }
  if ((!t365) && u_is_con(s364, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s364, 0);
    u_value v_r = u_field(s364, 1);
    u_value t368 = f_list_46_exists_2(v_args, u_mk_closure(lam367, 2, v_su, v_i));
    if (!u_truth(t368)) {
      t368 = f_check_46_occurs_3(v_su, v_i, v_r);
    }
    t365 = t368;
  }
  if ((!t365) && u_is_con(s364, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s364, 0);
    t365 = f_list_46_exists_2(v_ts, u_mk_closure(lam369, 2, v_su, v_i));
  }
  if (!t365) {
    t365 = u_mk_bool(0);
  }
  if (!t365) {
    u_no_case(s364);
  }
  return t365;
}

u_value f_check_46_constructor_scheme_1(u_value v_k) {
  u_value s370 = v_k;
  u_value t371 = NULL;
  if (!t371) {
    u_value v_ps = u_field(s370, 0);
    u_value v_ks = u_field(s370, 1);
    u_value v_r = u_field(s370, 2);
    u_value t372;
    if (u_truth(u_eq(v_ks, u_mk_con(&n_list_46_Nil, 0)))) {
      t372 = v_r;
    } else {
      t372 = f_check_46_fun_2(v_ks, v_r);
    }
    t371 = u_mk_tuple(2, v_ps, t372);
  }
  if (!t371) {
    u_no_case(s370);
  }
  return t371;
}

u_value f_check_46_constructor_of_2(u_value v_c, u_value v_k) {
  u_value s373 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t374 = NULL;
  if ((!t374) && u_is_con(s373, &n_option_46_Some)) {
    u_value v_kc = u_field(s373, 0);
    t374 = f_check_46_constructor_scheme_1(v_kc);
  }
  if ((!t374) && u_is_con(s373, &n_option_46_None)) {
    t374 = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t374) {
    u_no_case(s373);
  }
  return t374;
}

u_value f_check_46_candidates_3(u_value v_s, u_value v_env, u_value v_n) {
  u_value s375 = f_list_46_lookup_2(v_n, v_env);
  u_value t376 = NULL;
  if ((!t376) && u_is_con(s375, &n_option_46_Some)) {
    u_value v_sc = u_field(s375, 0);
    t376 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, v_sc), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t376) && u_is_con(s375, &n_option_46_None)) {
    u_value t377 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e378 = &t377;
    u_value l380 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam379, 1, v_s));
    while (u_is_con(l380, &n_list_46_Cons)) {
      u_value v_f = u_field(l380, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t381 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e382 = &t381;
        u_value l383 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(v_s), v_f);
        while (u_is_con(l383, &n_list_46_Cons)) {
          u_value v_sc = u_field(l383, 0);
          if (u_truth(u_mk_bool(1))) {
            e382 = u_append(e382, u_mk_tuple(2, v_f, v_sc));
          }
          l383 = u_field(l383, 1);
        }
        e378 = u_append(e378, t381);
      }
      l380 = u_field(l380, 1);
    }
    u_value t384 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e385 = &t384;
    u_value l387 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam386, 1, v_s));
    while (u_is_con(l387, &n_list_46_Cons)) {
      u_value v_k = u_field(l387, 0);
      if (u_truth(u_mk_bool(1))) {
        e385 = u_append(e385, u_mk_tuple(2, v_k, f_check_46_constructor_of_2(f_check_46_State_46_ctx_1(v_s), v_k)));
      }
      l387 = u_field(l387, 1);
    }
    u_value t388 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e389 = &t388;
    u_value l390 = f_check_46_builtin_1(v_n);
    while (u_is_con(l390, &n_list_46_Cons)) {
      u_value v_sc = u_field(l390, 0);
      if (u_truth(u_mk_bool(1))) {
        e389 = u_append(e389, u_mk_tuple(2, v_n, v_sc));
      }
      l390 = u_field(l390, 1);
    }
    t376 = f_list_46__43__43__2(f_list_46_concat_1(t377), f_list_46__43__43__2(t384, t388));
  }
  if (!t376) {
    u_no_case(s375);
  }
  return t376;
}

u_value f_check_46_with_arity_2(u_value v_cs, u_value v_k) {
  u_value s391 = v_cs;
  u_value t392 = NULL;
  if ((!t392) && u_is_con(s391, &n_list_46_Nil)) {
    t392 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t392) && u_is_con(s391, &n_list_46_Cons)) {
    u_value v_n = u_field(u_field(s391, 0), 0);
    u_value v_ps = u_field(u_field(u_field(s391, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s391, 0), 1), 1);
    u_value v_rest = u_field(s391, 1);
    u_value t393;
    if (u_truth(u_eq(f_syntax_46_arity_1(v_t), v_k))) {
      t393 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, v_ps, v_t)), f_check_46_with_arity_2(v_rest, v_k));
    } else {
      t393 = f_check_46_with_arity_2(v_rest, v_k);
    }
    t392 = t393;
  }
  if (!t392) {
    u_no_case(s391);
  }
  return t392;
}

u_value f_check_46_has_arity_2(u_value v_scs, u_value v_k) {
  u_value s394 = v_scs;
  u_value t395 = NULL;
  if ((!t395) && u_is_con(s394, &n_list_46_Nil)) {
    t395 = u_mk_bool(0);
  }
  if ((!t395) && u_is_con(s394, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s394, 0), 1);
    u_value v_rest = u_field(s394, 1);
    u_value t396 = u_eq(f_syntax_46_arity_1(v_t), v_k);
    if (!u_truth(t396)) {
      t396 = f_check_46_has_arity_2(v_rest, v_k);
    }
    t395 = t396;
  }
  if (!t395) {
    u_no_case(s394);
  }
  return t395;
}

u_value f_check_46_bind_1(u_value v_xs) {
  u_value t397 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e398 = &t397;
  u_value l399 = v_xs;
  while (u_is_con(l399, &n_list_46_Cons)) {
    u_value v_x = u_field(l399, 0);
    if (u_truth(u_mk_bool(1))) {
      e398 = u_append(e398, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_syntax_46_Var_46_typ_1(v_x))));
    }
    l399 = u_field(l399, 1);
  }
  return t397;
}

u_value f_check_46_fixed_2(u_value v_su, u_value v_env) {
  u_value s400 = v_env;
  u_value t401 = NULL;
  if ((!t401) && u_is_con(s400, &n_list_46_Nil)) {
    t401 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t401) && u_is_con(s400, &n_list_46_Cons)) {
    u_value v_ps = u_field(u_field(u_field(s400, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s400, 0), 1), 1);
    u_value v_rest = u_field(s400, 1);
    t401 = f_list_46__43__43__2(f_list_46_diff_2(f_check_46_free_1(f_check_46_zonk_type_2(v_su, v_t)), v_ps), f_check_46_fixed_2(v_su, v_rest));
  }
  if (!t401) {
    u_no_case(s400);
  }
  return t401;
}

u_value f_check_46_generalize_3(u_value v_su, u_value v_env, u_value v_t) {
  u_value t407 = f_check_46_fixed_2(v_su, v_env);
  u_value t408 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t406;
  {
    u_value v_fx = t407;
    u_value v_u = t408;
    u_value t402 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e403 = &t402;
    u_value l404 = f_check_46_free_1(v_u);
    while (u_is_con(l404, &n_list_46_Cons)) {
      u_value v_m = u_field(l404, 0);
      u_value t405 = f_check_46_is_meta_1(v_m);
      if (u_truth(t405)) {
        t405 = f_prelude_46_not_1(f_list_46_contains_2(v_m, v_fx));
      }
      if (u_truth(t405)) {
        e403 = u_append(e403, v_m);
      }
      l404 = u_field(l404, 1);
    }
    t406 = u_mk_tuple(2, t402, v_u);
  }
  return t406;
}

u_value f_check_46_generalized_3(u_value v_su, u_value v_env, u_value v_xs) {
  u_value t409 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e410 = &t409;
  u_value l411 = v_xs;
  while (u_is_con(l411, &n_list_46_Cons)) {
    u_value v_x = u_field(l411, 0);
    if (u_truth(u_mk_bool(1))) {
      e410 = u_append(e410, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), f_check_46_generalize_3(v_su, v_env, f_syntax_46_Var_46_typ_1(v_x))));
    }
    l411 = u_field(l411, 1);
  }
  return t409;
}

u_value f_check_46_case_of_1(u_value v_b) {
  u_value s412 = v_b;
  u_value t413 = NULL;
  if ((!t413) && u_is_con(s412, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s412, 0);
    u_value v_g = u_field(s412, 1);
    t413 = u_mk_tuple(2, v_p, u_eq(v_g, u_mk_con(&n_option_46_None, 0)));
  }
  if (!t413) {
    u_no_case(s412);
  }
  return t413;
}

u_value f_check_46_zonk_guard_2(u_value v_su, u_value v_g) {
  u_value s414 = v_g;
  u_value t415 = NULL;
  if ((!t415) && u_is_con(s414, &n_option_46_Some)) {
    u_value v_e = u_field(s414, 0);
    t415 = u_mk_con(&n_option_46_Some, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t415) && u_is_con(s414, &n_option_46_None)) {
    t415 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t415) {
    u_no_case(s414);
  }
  return t415;
}

u_value f_check_46_zonk_type_2(u_value v_su, u_value v_t) {
  u_value s416 = f_check_46_resolve_2(v_su, v_t);
  u_value t417 = NULL;
  if ((!t417) && u_is_con(s416, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s416, 0);
    u_value v_args = u_field(s416, 1);
    u_value t418 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e419 = &t418;
    u_value l420 = v_args;
    while (u_is_con(l420, &n_list_46_Cons)) {
      u_value v_a = u_field(l420, 0);
      if (u_truth(u_mk_bool(1))) {
        e419 = u_append(e419, f_check_46_zonk_type_2(v_su, v_a));
      }
      l420 = u_field(l420, 1);
    }
    t417 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t418);
  }
  if ((!t417) && u_is_con(s416, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s416, 0);
    u_value v_r = u_field(s416, 1);
    u_value t421 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e422 = &t421;
    u_value l423 = v_args;
    while (u_is_con(l423, &n_list_46_Cons)) {
      u_value v_a = u_field(l423, 0);
      if (u_truth(u_mk_bool(1))) {
        e422 = u_append(e422, f_check_46_zonk_type_2(v_su, v_a));
      }
      l423 = u_field(l423, 1);
    }
    t417 = u_mk_con(&n_syntax_46_TFun, 2, t421, f_check_46_zonk_type_2(v_su, v_r));
  }
  if ((!t417) && u_is_con(s416, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s416, 0);
    u_value t424 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e425 = &t424;
    u_value l426 = v_ts;
    while (u_is_con(l426, &n_list_46_Cons)) {
      u_value v_a = u_field(l426, 0);
      if (u_truth(u_mk_bool(1))) {
        e425 = u_append(e425, f_check_46_zonk_type_2(v_su, v_a));
      }
      l426 = u_field(l426, 1);
    }
    t417 = u_mk_con(&n_syntax_46_TTuple, 1, t424);
  }
  if (!t417) {
    u_value v_u = s416;
    t417 = v_u;
  }
  if (!t417) {
    u_no_case(s416);
  }
  return t417;
}

u_value f_check_46_zonk_var_2(u_value v_su, u_value v_x) {
  u_value s427 = v_x;
  u_value t428 = NULL;
  if ((!t428) && u_is_con(s427, &n_syntax_46_Var)) {
    u_value v_n = u_field(s427, 0);
    u_value v_t = u_field(s427, 1);
    t428 = u_mk_con(&n_syntax_46_Var, 2, v_n, f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t428) {
    u_no_case(s427);
  }
  return t428;
}

u_value f_check_46_zonk_vars_2(u_value v_su, u_value v_xs) {
  u_value t429 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e430 = &t429;
  u_value l431 = v_xs;
  while (u_is_con(l431, &n_list_46_Cons)) {
    u_value v_x = u_field(l431, 0);
    if (u_truth(u_mk_bool(1))) {
      e430 = u_append(e430, f_check_46_zonk_var_2(v_su, v_x));
    }
    l431 = u_field(l431, 1);
  }
  return t429;
}

u_value f_check_46_zonk_expr_2(u_value v_su, u_value v_e) {
  u_value s432 = v_e;
  u_value t433 = NULL;
  if ((!t433) && u_is_con(s432, &n_syntax_46_ELit)) {
    t433 = v_e;
  }
  if ((!t433) && u_is_con(s432, &n_syntax_46_EVar)) {
    u_value v_x = u_field(s432, 0);
    t433 = u_mk_con(&n_syntax_46_EVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t433) && u_is_con(s432, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s432, 0);
    u_value v_args = u_field(s432, 1);
    u_value t434 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e435 = &t434;
    u_value l436 = v_args;
    while (u_is_con(l436, &n_list_46_Cons)) {
      u_value v_a = u_field(l436, 0);
      if (u_truth(u_mk_bool(1))) {
        e435 = u_append(e435, f_check_46_zonk_expr_2(v_su, v_a));
      }
      l436 = u_field(l436, 1);
    }
    t433 = u_mk_con(&n_syntax_46_EApp, 2, f_check_46_zonk_expr_2(v_su, v_f), t434);
  }
  if ((!t433) && u_is_con(s432, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s432, 0);
    u_value v_b = u_field(s432, 1);
    t433 = u_mk_con(&n_syntax_46_ELam, 2, f_check_46_zonk_vars_2(v_su, v_xs), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t433) && u_is_con(s432, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s432, 0);
    u_value v_b = u_field(s432, 1);
    u_value t437 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e438 = &t437;
    u_value l439 = v_ps;
    while (u_is_con(l439, &n_list_46_Cons)) {
      u_value v_p = u_field(l439, 0);
      if (u_truth(u_mk_bool(1))) {
        e438 = u_append(e438, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l439 = u_field(l439, 1);
    }
    t433 = u_mk_con(&n_syntax_46_ELet, 2, t437, f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t433) && u_is_con(s432, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s432, 0);
    u_value v_l = u_field(s432, 1);
    u_value v_r = u_field(s432, 2);
    t433 = u_mk_con(&n_syntax_46_EIte, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_expr_2(v_su, v_l), f_check_46_zonk_expr_2(v_su, v_r));
  }
  if ((!t433) && u_is_con(s432, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s432, 0);
    u_value v_bs = u_field(s432, 1);
    u_value t440 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e441 = &t440;
    u_value l442 = v_bs;
    while (u_is_con(l442, &n_list_46_Cons)) {
      u_value v_b = u_field(l442, 0);
      if (u_truth(u_mk_bool(1))) {
        e441 = u_append(e441, f_check_46_zonk_expr_branch_2(v_su, v_b));
      }
      l442 = u_field(l442, 1);
    }
    t433 = u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t440);
  }
  if ((!t433) && u_is_con(s432, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s432, 0);
    u_value v_xs = u_field(s432, 1);
    u_value v_ls = u_field(s432, 2);
    u_value v_c = u_field(s432, 3);
    u_value t443 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e444 = &t443;
    u_value l445 = v_ls;
    while (u_is_con(l445, &n_list_46_Cons)) {
      u_value v_l = u_field(l445, 0);
      if (u_truth(u_mk_bool(1))) {
        e444 = u_append(e444, f_check_46_zonk_expr_2(v_su, v_l));
      }
      l445 = u_field(l445, 1);
    }
    t433 = u_mk_con(&n_syntax_46_EComp, 4, f_check_46_zonk_expr_2(v_su, v_r), f_check_46_zonk_vars_2(v_su, v_xs), t443, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t433) && u_is_con(s432, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s432, 0);
    u_value t446 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e447 = &t446;
    u_value l448 = v_es;
    while (u_is_con(l448, &n_list_46_Cons)) {
      u_value v_e = u_field(l448, 0);
      if (u_truth(u_mk_bool(1))) {
        e447 = u_append(e447, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l448 = u_field(l448, 1);
    }
    t433 = u_mk_con(&n_syntax_46_ETuple, 1, t446);
  }
  if ((!t433) && u_is_con(s432, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s432, 0);
    u_value v_t = u_field(s432, 1);
    t433 = u_mk_con(&n_syntax_46_ERaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t433) {
    u_no_case(s432);
  }
  return t433;
}

u_value f_check_46_zonk_binding_2(u_value v_su, u_value v_p) {
  u_value s449 = v_p;
  u_value t450 = NULL;
  if (!t450) {
    u_value v_x = u_field(s449, 0);
    u_value v_e = u_field(s449, 1);
    t450 = u_mk_tuple(2, f_check_46_zonk_var_2(v_su, v_x), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t450) {
    u_no_case(s449);
  }
  return t450;
}

u_value f_check_46_zonk_expr_branch_2(u_value v_su, u_value v_b) {
  u_value s451 = v_b;
  u_value t452 = NULL;
  if ((!t452) && u_is_con(s451, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s451, 0);
    u_value v_g = u_field(s451, 1);
    u_value v_e = u_field(s451, 2);
    t452 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t452) {
    u_no_case(s451);
  }
  return t452;
}

u_value f_check_46_zonk_pat_2(u_value v_su, u_value v_p) {
  u_value s453 = v_p;
  u_value t454 = NULL;
  if ((!t454) && u_is_con(s453, &n_syntax_46_PVar)) {
    u_value v_x = u_field(s453, 0);
    t454 = u_mk_con(&n_syntax_46_PVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t454) && u_is_con(s453, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s453, 0);
    u_value v_args = u_field(s453, 1);
    u_value t455 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e456 = &t455;
    u_value l457 = v_args;
    while (u_is_con(l457, &n_list_46_Cons)) {
      u_value v_a = u_field(l457, 0);
      if (u_truth(u_mk_bool(1))) {
        e456 = u_append(e456, f_check_46_zonk_pat_2(v_su, v_a));
      }
      l457 = u_field(l457, 1);
    }
    t454 = u_mk_con(&n_syntax_46_PApp, 2, v_n, t455);
  }
  if ((!t454) && u_is_con(s453, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s453, 0);
    u_value t458 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e459 = &t458;
    u_value l460 = v_ps;
    while (u_is_con(l460, &n_list_46_Cons)) {
      u_value v_q = u_field(l460, 0);
      if (u_truth(u_mk_bool(1))) {
        e459 = u_append(e459, f_check_46_zonk_pat_2(v_su, v_q));
      }
      l460 = u_field(l460, 1);
    }
    t454 = u_mk_con(&n_syntax_46_PTuple, 1, t458);
  }
  if (!t454) {
    t454 = v_p;
  }
  if (!t454) {
    u_no_case(s453);
  }
  return t454;
}

u_value f_check_46_zonk_stmt_2(u_value v_su, u_value v_x) {
  u_value s461 = v_x;
  u_value t462 = NULL;
  if ((!t462) && u_is_con(s461, &n_syntax_46_SSkip)) {
    t462 = v_x;
  }
  if ((!t462) && u_is_con(s461, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s461, 0);
    u_value t463 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e464 = &t463;
    u_value l465 = v_ps;
    while (u_is_con(l465, &n_list_46_Cons)) {
      u_value v_p = u_field(l465, 0);
      if (u_truth(u_mk_bool(1))) {
        e464 = u_append(e464, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l465 = u_field(l465, 1);
    }
    t462 = u_mk_con(&n_syntax_46_SAssign, 1, t463);
  }
  if ((!t462) && u_is_con(s461, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s461, 0);
    u_value v_ys = u_field(s461, 1);
    u_value t466 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e467 = &t466;
    u_value l468 = v_ys;
    while (u_is_con(l468, &n_list_46_Cons)) {
      u_value v_y = u_field(l468, 0);
      if (u_truth(u_mk_bool(1))) {
        e467 = u_append(e467, f_check_46_zonk_stmt_2(v_su, v_y));
      }
      l468 = u_field(l468, 1);
    }
    t462 = u_mk_con(&n_syntax_46_SBlock, 2, f_check_46_zonk_vars_2(v_su, v_xs), t466);
  }
  if ((!t462) && u_is_con(s461, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s461, 0);
    u_value v_l = u_field(s461, 1);
    u_value v_r = u_field(s461, 2);
    t462 = u_mk_con(&n_syntax_46_SIf, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_l), f_check_46_zonk_stmt_2(v_su, v_r));
  }
  if ((!t462) && u_is_con(s461, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s461, 0);
    u_value v_b = u_field(s461, 1);
    t462 = u_mk_con(&n_syntax_46_SWhile, 2, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t462) && u_is_con(s461, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s461, 0);
    u_value v_ins = u_field(s461, 1);
    u_value v_outs = u_field(s461, 2);
    u_value v_refs = u_field(s461, 3);
    u_value t469 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e470 = &t469;
    u_value l471 = v_ins;
    while (u_is_con(l471, &n_list_46_Cons)) {
      u_value v_e = u_field(l471, 0);
      if (u_truth(u_mk_bool(1))) {
        e470 = u_append(e470, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l471 = u_field(l471, 1);
    }
    t462 = u_mk_con(&n_syntax_46_SCall, 4, v_p, t469, f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs));
  }
  if ((!t462) && u_is_con(s461, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s461, 0);
    u_value v_bs = u_field(s461, 1);
    u_value t472 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e473 = &t472;
    u_value l474 = v_bs;
    while (u_is_con(l474, &n_list_46_Cons)) {
      u_value v_b = u_field(l474, 0);
      if (u_truth(u_mk_bool(1))) {
        e473 = u_append(e473, f_check_46_zonk_stmt_branch_2(v_su, v_b));
      }
      l474 = u_field(l474, 1);
    }
    t462 = u_mk_con(&n_syntax_46_SMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t472);
  }
  if ((!t462) && u_is_con(s461, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s461, 0);
    t462 = u_mk_con(&n_syntax_46_SAssert, 1, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t462) && u_is_con(s461, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s461, 0);
    u_value v_t = u_field(s461, 1);
    t462 = u_mk_con(&n_syntax_46_SRaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if ((!t462) && u_is_con(s461, &n_syntax_46_STry)) {
    u_value v_b = u_field(s461, 0);
    u_value v_hs = u_field(s461, 1);
    u_value t475 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e476 = &t475;
    u_value l477 = v_hs;
    while (u_is_con(l477, &n_list_46_Cons)) {
      u_value v_h = u_field(l477, 0);
      if (u_truth(u_mk_bool(1))) {
        e476 = u_append(e476, f_check_46_zonk_handler_2(v_su, v_h));
      }
      l477 = u_field(l477, 1);
    }
    t462 = u_mk_con(&n_syntax_46_STry, 2, f_check_46_zonk_stmt_2(v_su, v_b), t475);
  }
  if (!t462) {
    u_no_case(s461);
  }
  return t462;
}

u_value f_check_46_zonk_stmt_branch_2(u_value v_su, u_value v_b) {
  u_value s478 = v_b;
  u_value t479 = NULL;
  if ((!t479) && u_is_con(s478, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s478, 0);
    u_value v_g = u_field(s478, 1);
    u_value v_y = u_field(s478, 2);
    t479 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_y));
  }
  if (!t479) {
    u_no_case(s478);
  }
  return t479;
}

u_value f_check_46_zonk_handler_2(u_value v_su, u_value v_h) {
  u_value s480 = v_h;
  u_value t481 = NULL;
  if ((!t481) && u_is_con(s480, &n_syntax_46_Handler)) {
    u_value v_t = u_field(s480, 0);
    u_value v_p = u_field(s480, 1);
    u_value v_g = u_field(s480, 2);
    u_value v_b = u_field(s480, 3);
    t481 = u_mk_con(&n_syntax_46_Handler, 4, f_check_46_zonk_type_2(v_su, v_t), f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t481) {
    u_no_case(s480);
  }
  return t481;
}

u_value f_check_46_zonk_cmd_2(u_value v_su, u_value v_c) {
  u_value s482 = v_c;
  u_value t483 = NULL;
  if ((!t483) && u_is_con(s482, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s482, 0);
    u_value v_ps = u_field(s482, 1);
    u_value v_xss = u_field(s482, 2);
    u_value v_r = u_field(s482, 3);
    u_value v_b = u_field(s482, 4);
    u_value t484 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e485 = &t484;
    u_value l486 = v_xss;
    while (u_is_con(l486, &n_list_46_Cons)) {
      u_value v_xs = u_field(l486, 0);
      if (u_truth(u_mk_bool(1))) {
        e485 = u_append(e485, f_check_46_zonk_vars_2(v_su, v_xs));
      }
      l486 = u_field(l486, 1);
    }
    t483 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, t484, f_check_46_zonk_type_2(v_su, v_r), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t483) && u_is_con(s482, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s482, 0);
    u_value v_ps = u_field(s482, 1);
    u_value v_ins = u_field(s482, 2);
    u_value v_outs = u_field(s482, 3);
    u_value v_refs = u_field(s482, 4);
    u_value v_b = u_field(s482, 5);
    t483 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, f_check_46_zonk_vars_2(v_su, v_ins), f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t483) && u_is_con(s482, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s482, 0);
    t483 = u_mk_con(&n_syntax_46_CEval, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t483) && u_is_con(s482, &n_syntax_46_CRun)) {
    u_value v_n = u_field(s482, 0);
    u_value v_b = u_field(s482, 1);
    t483 = u_mk_con(&n_syntax_46_CRun, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t483) && u_is_con(s482, &n_syntax_46_CTest)) {
    u_value v_n = u_field(s482, 0);
    u_value v_b = u_field(s482, 1);
    t483 = u_mk_con(&n_syntax_46_CTest, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t483) {
    t483 = v_c;
  }
  if (!t483) {
    u_no_case(s482);
  }
  return t483;
}

u_value f_check_46_unknown_type_1(u_value v_t) {
  u_value t487;
  if (u_truth(u_eq(f_check_46_free_1(v_t), u_mk_con(&n_list_46_Nil, 0)))) {
    t487 = u_mk_con(&n_option_46_None, 0);
  } else {
    t487 = u_mk_con(&n_option_46_Some, 1, v_t);
  }
  return t487;
}

u_value f_check_46_unknown_in_1(u_value v_x) {
  u_value s488 = v_x;
  u_value t489 = NULL;
  if ((!t489) && u_is_con(s488, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s488, 0);
    t489 = f_check_46_unknown_in_exprs_1(f_list_46_seconds_1(v_ps));
  }
  if ((!t489) && u_is_con(s488, &n_syntax_46_SBlock)) {
    u_value v_ys = u_field(s488, 1);
    t489 = f_check_46_unknown_in_all_1(v_ys);
  }
  if ((!t489) && u_is_con(s488, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s488, 0);
    u_value v_l = u_field(s488, 1);
    u_value v_r = u_field(s488, 2);
    t489 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_option_46_or_else_2(f_check_46_unknown_in_1(v_l), f_check_46_unknown_in_1(v_r)));
  }
  if ((!t489) && u_is_con(s488, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s488, 0);
    u_value v_b = u_field(s488, 1);
    t489 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_check_46_unknown_in_1(v_b));
  }
  if ((!t489) && u_is_con(s488, &n_syntax_46_SCall)) {
    u_value v_ins = u_field(s488, 1);
    t489 = f_check_46_unknown_in_exprs_1(v_ins);
  }
  if ((!t489) && u_is_con(s488, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s488, 0);
    u_value v_bs = u_field(s488, 1);
    t489 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_a), f_check_46_unknown_in_branches_1(v_bs));
  }
  if ((!t489) && u_is_con(s488, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s488, 0);
    t489 = f_check_46_unknown_in_expr_1(v_c);
  }
  if ((!t489) && u_is_con(s488, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s488, 0);
    u_value v_t = u_field(s488, 1);
    t489 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t489) && u_is_con(s488, &n_syntax_46_STry)) {
    u_value v_b = u_field(s488, 0);
    u_value v_hs = u_field(s488, 1);
    t489 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_hs));
  }
  if (!t489) {
    t489 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t489) {
    u_no_case(s488);
  }
  return t489;
}

u_value f_check_46_unknown_in_expr_1(u_value v_e) {
  u_value s490 = v_e;
  u_value t491 = NULL;
  if ((!t491) && u_is_con(s490, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s490, 0);
    u_value v_t = u_field(s490, 1);
    t491 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t491) && u_is_con(s490, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s490, 0);
    u_value v_args = u_field(s490, 1);
    t491 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_f, v_args));
  }
  if ((!t491) && u_is_con(s490, &n_syntax_46_ELam)) {
    u_value v_b = u_field(s490, 1);
    t491 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t491) && u_is_con(s490, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s490, 0);
    u_value v_b = u_field(s490, 1);
    t491 = f_check_46_unknown_in_exprs_1(f_list_46__43__43__2(f_list_46_seconds_1(v_ps), u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t491) && u_is_con(s490, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s490, 0);
    u_value v_l = u_field(s490, 1);
    u_value v_r = u_field(s490, 2);
    t491 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)))));
  }
  if ((!t491) && u_is_con(s490, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s490, 0);
    u_value v_bs = u_field(s490, 1);
    u_value t492 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e493 = &t492;
    u_value l494 = v_bs;
    while (u_is_con(l494, &n_list_46_Cons)) {
      u_value v_b = u_field(l494, 0);
      if (u_truth(u_mk_bool(1))) {
        e493 = u_append(e493, f_check_46_branch_exprs_1(v_b));
      }
      l494 = u_field(l494, 1);
    }
    t491 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_a, f_list_46_concat_1(t492)));
  }
  if ((!t491) && u_is_con(s490, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s490, 0);
    u_value v_ls = u_field(s490, 2);
    u_value v_c = u_field(s490, 3);
    t491 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Cons, 2, v_c, v_ls)));
  }
  if ((!t491) && u_is_con(s490, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s490, 0);
    t491 = f_check_46_unknown_in_exprs_1(v_es);
  }
  if (!t491) {
    t491 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t491) {
    u_no_case(s490);
  }
  return t491;
}

u_value f_check_46_unknown_in_exprs_1(u_value v_es) {
  u_value s495 = v_es;
  u_value t496 = NULL;
  if ((!t496) && u_is_con(s495, &n_list_46_Nil)) {
    t496 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t496) && u_is_con(s495, &n_list_46_Cons)) {
    u_value v_e = u_field(s495, 0);
    u_value v_rest = u_field(s495, 1);
    t496 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_e), f_check_46_unknown_in_exprs_1(v_rest));
  }
  if (!t496) {
    u_no_case(s495);
  }
  return t496;
}

u_value f_check_46_branch_exprs_1(u_value v_b) {
  u_value s497 = v_b;
  u_value t498 = NULL;
  if ((!t498) && (u_is_con(s497, &n_syntax_46_Branch) && u_is_con(u_field(s497, 1), &n_option_46_Some))) {
    u_value v_g = u_field(u_field(s497, 1), 0);
    u_value v_e = u_field(s497, 2);
    t498 = u_mk_con(&n_list_46_Cons, 2, v_g, u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t498) && (u_is_con(s497, &n_syntax_46_Branch) && u_is_con(u_field(s497, 1), &n_option_46_None))) {
    u_value v_e = u_field(s497, 2);
    t498 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t498) {
    u_no_case(s497);
  }
  return t498;
}

u_value f_check_46_unknown_in_all_1(u_value v_xs) {
  u_value s499 = v_xs;
  u_value t500 = NULL;
  if ((!t500) && u_is_con(s499, &n_list_46_Nil)) {
    t500 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t500) && u_is_con(s499, &n_list_46_Cons)) {
    u_value v_y = u_field(s499, 0);
    u_value v_ys = u_field(s499, 1);
    t500 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_all_1(v_ys));
  }
  if (!t500) {
    u_no_case(s499);
  }
  return t500;
}

u_value f_check_46_unknown_in_branches_1(u_value v_bs) {
  u_value s501 = v_bs;
  u_value t502 = NULL;
  if ((!t502) && u_is_con(s501, &n_list_46_Nil)) {
    t502 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t502) && (u_is_con(s501, &n_list_46_Cons) && (u_is_con(u_field(s501, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s501, 0), 1), &n_option_46_Some)))) {
    u_value v_g = u_field(u_field(u_field(s501, 0), 1), 0);
    u_value v_y = u_field(u_field(s501, 0), 2);
    u_value v_rest = u_field(s501, 1);
    t502 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_g), f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest)));
  }
  if ((!t502) && (u_is_con(s501, &n_list_46_Cons) && (u_is_con(u_field(s501, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s501, 0), 1), &n_option_46_None)))) {
    u_value v_y = u_field(u_field(s501, 0), 2);
    u_value v_rest = u_field(s501, 1);
    t502 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest));
  }
  if (!t502) {
    u_no_case(s501);
  }
  return t502;
}

u_value f_check_46_unknown_in_handlers_1(u_value v_hs) {
  u_value s503 = v_hs;
  u_value t504 = NULL;
  if ((!t504) && u_is_con(s503, &n_list_46_Nil)) {
    t504 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t504) && (u_is_con(s503, &n_list_46_Cons) && (u_is_con(u_field(s503, 0), &n_syntax_46_Handler) && u_is_con(u_field(u_field(s503, 0), 1), &n_syntax_46_PWild)))) {
    u_value v_b = u_field(u_field(s503, 0), 3);
    u_value v_rest = u_field(s503, 1);
    t504 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest));
  }
  if ((!t504) && (u_is_con(s503, &n_list_46_Cons) && u_is_con(u_field(s503, 0), &n_syntax_46_Handler))) {
    u_value v_t = u_field(u_field(s503, 0), 0);
    u_value v_b = u_field(u_field(s503, 0), 3);
    u_value v_rest = u_field(s503, 1);
    t504 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest)));
  }
  if (!t504) {
    u_no_case(s503);
  }
  return t504;
}

u_value f_check_46_unknown_in_cmd_1(u_value v_c) {
  u_value s505 = v_c;
  u_value t506 = NULL;
  if ((!t506) && u_is_con(s505, &n_syntax_46_CFun)) {
    u_value v_b = u_field(s505, 4);
    t506 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t506) && u_is_con(s505, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s505, 0);
    t506 = f_check_46_unknown_in_expr_1(v_e);
  }
  if ((!t506) && u_is_con(s505, &n_syntax_46_CProc)) {
    u_value v_b = u_field(s505, 5);
    t506 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t506) && u_is_con(s505, &n_syntax_46_CRun)) {
    u_value v_b = u_field(s505, 1);
    t506 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t506) && u_is_con(s505, &n_syntax_46_CTest)) {
    u_value v_b = u_field(s505, 1);
    t506 = f_check_46_unknown_in_1(v_b);
  }
  if (!t506) {
    t506 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t506) {
    u_no_case(s505);
  }
  return t506;
}

u_value f_check_46_head_of_1(u_value v_p) {
  u_value s507 = v_p;
  u_value t508 = NULL;
  if ((!t508) && u_is_con(s507, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s507, 0);
    u_value v_args = u_field(s507, 1);
    t508 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_n), v_args));
  }
  if ((!t508) && u_is_con(s507, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s507, 0);
    t508 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ps));
  }
  if ((!t508) && u_is_con(s507, &n_syntax_46_PLit)) {
    u_value v_v = u_field(s507, 0);
    t508 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, v_v), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t508) {
    t508 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t508) {
    u_no_case(s507);
  }
  return t508;
}

u_value f_check_46_heads_1(u_value v_rows) {
  u_value s509 = v_rows;
  u_value t510 = NULL;
  if ((!t510) && u_is_con(s509, &n_list_46_Nil)) {
    t510 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t510) && (u_is_con(s509, &n_list_46_Cons) && u_is_con(u_field(s509, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s509, 0), 0);
    u_value v_rest = u_field(s509, 1);
    u_value s511 = f_check_46_head_of_1(v_p);
    u_value t512 = NULL;
    if ((!t512) && u_is_con(s511, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s511, 0), 0);
      t512 = u_mk_con(&n_list_46_Cons, 2, v_h, f_check_46_heads_1(v_rest));
    }
    if ((!t512) && u_is_con(s511, &n_option_46_None)) {
      t512 = f_check_46_heads_1(v_rest);
    }
    if (!t512) {
      u_no_case(s511);
    }
    t510 = t512;
  }
  if ((!t510) && (u_is_con(s509, &n_list_46_Cons) && u_is_con(u_field(s509, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s509, 1);
    t510 = f_check_46_heads_1(v_rest);
  }
  if (!t510) {
    u_no_case(s509);
  }
  return t510;
}

u_value f_check_46_field_types_3(u_value v_c, u_value v_k, u_value v_args) {
  u_value s513 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t514 = NULL;
  if ((!t514) && u_is_con(s513, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s513, 0), 0);
    u_value v_ks = u_field(u_field(s513, 0), 1);
    u_value t515 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e516 = &t515;
    u_value l517 = v_ks;
    while (u_is_con(l517, &n_list_46_Cons)) {
      u_value v_a = u_field(l517, 0);
      if (u_truth(u_mk_bool(1))) {
        e516 = u_append(e516, f_check_46_subst_2(v_a, f_list_46_zip_2(v_ps, v_args)));
      }
      l517 = u_field(l517, 1);
    }
    t514 = t515;
  }
  if ((!t514) && u_is_con(s513, &n_option_46_None)) {
    t514 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t514) {
    u_no_case(s513);
  }
  return t514;
}

u_value f_check_46_constructors_of_3(u_value v_c, u_value v_su, u_value v_t) {
  u_value s518 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t519 = NULL;
  if ((!t519) && (u_is_con(s518, &n_syntax_46_TSort) && (u_is_con(u_field(s518, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s518, 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(s518, 0), 1), u_mk_lit("Bool")) && u_is_con(u_field(s518, 1), &n_list_46_Nil)))))) {
    t519 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t519) && u_is_con(s518, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s518, 0);
    t519 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ts), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t519) && u_is_con(s518, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s518, 0);
    u_value v_args = u_field(s518, 1);
    u_value s520 = f_ctx_46_members_of_2(v_c, v_n);
    u_value t521 = NULL;
    if ((!t521) && u_is_con(s520, &n_option_46_Some)) {
      u_value v_ks = u_field(s520, 0);
      u_value t522 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e523 = &t522;
      u_value l524 = v_ks;
      while (u_is_con(l524, &n_list_46_Cons)) {
        u_value v_k = u_field(l524, 0);
        if (u_truth(u_mk_bool(1))) {
          e523 = u_append(e523, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_k), f_check_46_field_types_3(v_c, v_k, v_args)));
        }
        l524 = u_field(l524, 1);
      }
      t521 = u_mk_con(&n_option_46_Some, 1, t522);
    }
    if ((!t521) && u_is_con(s520, &n_option_46_None)) {
      t521 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t521) {
      u_no_case(s520);
    }
    t519 = t521;
  }
  if (!t519) {
    t519 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t519) {
    u_no_case(s518);
  }
  return t519;
}

u_value f_check_46_fields_of_4(u_value v_c, u_value v_su, u_value v_t, u_value v_h) {
  u_value s525 = f_check_46_constructors_of_3(v_c, v_su, v_t);
  u_value t526 = NULL;
  if ((!t526) && u_is_con(s525, &n_option_46_Some)) {
    u_value v_ks = u_field(s525, 0);
    u_value s527 = f_list_46_lookup_2(v_h, v_ks);
    u_value t528 = NULL;
    if ((!t528) && u_is_con(s527, &n_option_46_Some)) {
      u_value v_ts = u_field(s527, 0);
      t528 = v_ts;
    }
    if ((!t528) && u_is_con(s527, &n_option_46_None)) {
      t528 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t528) {
      u_no_case(s527);
    }
    t526 = t528;
  }
  if ((!t526) && u_is_con(s525, &n_option_46_None)) {
    t526 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t526) {
    u_no_case(s525);
  }
  return t526;
}

u_value f_check_46_covers_2(u_value v_hs, u_value v_ks) {
  u_value s529 = v_ks;
  u_value t530 = NULL;
  if ((!t530) && u_is_con(s529, &n_list_46_Nil)) {
    t530 = u_mk_bool(1);
  }
  if ((!t530) && u_is_con(s529, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s529, 0), 0);
    u_value v_rest = u_field(s529, 1);
    u_value t531 = f_list_46_contains_2(v_h, v_hs);
    if (u_truth(t531)) {
      t531 = f_check_46_covers_2(v_hs, v_rest);
    }
    t530 = t531;
  }
  if (!t530) {
    u_no_case(s529);
  }
  return t530;
}

u_value f_check_46_defaults_1(u_value v_rows) {
  u_value s532 = v_rows;
  u_value t533 = NULL;
  if ((!t533) && u_is_con(s532, &n_list_46_Nil)) {
    t533 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t533) && (u_is_con(s532, &n_list_46_Cons) && u_is_con(u_field(s532, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s532, 0), 0);
    u_value v_ps = u_field(u_field(s532, 0), 1);
    u_value v_rest = u_field(s532, 1);
    u_value s534 = f_check_46_head_of_1(v_p);
    u_value t535 = NULL;
    if ((!t535) && u_is_con(s534, &n_option_46_Some)) {
      t535 = f_check_46_defaults_1(v_rest);
    }
    if ((!t535) && u_is_con(s534, &n_option_46_None)) {
      t535 = u_mk_con(&n_list_46_Cons, 2, v_ps, f_check_46_defaults_1(v_rest));
    }
    if (!t535) {
      u_no_case(s534);
    }
    t533 = t535;
  }
  if ((!t533) && (u_is_con(s532, &n_list_46_Cons) && u_is_con(u_field(s532, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s532, 1);
    t533 = f_check_46_defaults_1(v_rest);
  }
  if (!t533) {
    u_no_case(s532);
  }
  return t533;
}

u_value f_check_46_specialize_3(u_value v_rows, u_value v_h, u_value v_n) {
  u_value s536 = v_rows;
  u_value t537 = NULL;
  if ((!t537) && u_is_con(s536, &n_list_46_Nil)) {
    t537 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t537) && (u_is_con(s536, &n_list_46_Cons) && u_is_con(u_field(s536, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s536, 0), 0);
    u_value v_ps = u_field(u_field(s536, 0), 1);
    u_value v_rest = u_field(s536, 1);
    u_value s538 = f_check_46_head_of_1(v_p);
    u_value t539 = NULL;
    if ((!t539) && u_is_con(s538, &n_option_46_Some)) {
      u_value v_g = u_field(u_field(s538, 0), 0);
      u_value v_args = u_field(u_field(s538, 0), 1);
      u_value t540;
      if (u_truth(u_eq(v_g, v_h))) {
        t540 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(v_args, v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
      } else {
        t540 = f_check_46_specialize_3(v_rest, v_h, v_n);
      }
      t539 = t540;
    }
    if ((!t539) && u_is_con(s538, &n_option_46_None)) {
      t539 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(f_list_46_replicate_2(v_n, u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
    }
    if (!t539) {
      u_no_case(s538);
    }
    t537 = t539;
  }
  if ((!t537) && (u_is_con(s536, &n_list_46_Cons) && u_is_con(u_field(s536, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s536, 1);
    t537 = f_check_46_specialize_3(v_rest, v_h, v_n);
  }
  if (!t537) {
    u_no_case(s536);
  }
  return t537;
}

u_value f_check_46_useful_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_q, u_value v_ts) {
  u_value s541 = u_mk_tuple(2, v_q, v_ts);
  u_value t542 = NULL;
  if ((!t542) && u_is_con(u_field(s541, 0), &n_list_46_Nil)) {
    t542 = u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t542) && (u_is_con(u_field(s541, 0), &n_list_46_Cons) && u_is_con(u_field(s541, 1), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s541, 0), 0);
    u_value v_ps = u_field(u_field(s541, 0), 1);
    u_value v_t = u_field(u_field(s541, 1), 0);
    u_value v_rest = u_field(u_field(s541, 1), 1);
    u_value s543 = f_check_46_head_of_1(v_p);
    u_value t544 = NULL;
    if ((!t544) && u_is_con(s543, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s543, 0), 0);
      u_value v_args = u_field(u_field(s543, 0), 1);
      t544 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_ps), f_list_46__43__43__2(f_check_46_fields_of_4(v_c, v_su, v_t, v_h), v_rest));
    }
    if ((!t544) && u_is_con(s543, &n_option_46_None)) {
      u_value s545 = f_check_46_constructors_of_3(v_c, v_su, v_t);
      u_value t546 = NULL;
      if ((!t546) && u_is_con(s545, &n_option_46_Some)) {
        u_value v_ks = u_field(s545, 0);
        u_value t547;
        if (u_truth(f_check_46_covers_2(f_check_46_heads_1(v_rows), v_ks))) {
          t547 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_ks, v_ps, v_rest);
        } else {
          t547 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
        }
        t546 = t547;
      }
      if ((!t546) && u_is_con(s545, &n_option_46_None)) {
        t546 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
      }
      if (!t546) {
        u_no_case(s545);
      }
      t544 = t546;
    }
    if (!t544) {
      u_no_case(s543);
    }
    t542 = t544;
  }
  if ((!t542) && (u_is_con(u_field(s541, 0), &n_list_46_Cons) && u_is_con(u_field(s541, 1), &n_list_46_Nil))) {
    t542 = u_mk_bool(0);
  }
  if (!t542) {
    u_no_case(s541);
  }
  return t542;
}

u_value f_check_46_useful_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_ps, u_value v_rest) {
  u_value s548 = v_ks;
  u_value t549 = NULL;
  if ((!t549) && u_is_con(s548, &n_list_46_Nil)) {
    t549 = u_mk_bool(0);
  }
  if ((!t549) && u_is_con(s548, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s548, 0), 0);
    u_value v_args = u_field(u_field(s548, 0), 1);
    u_value v_more = u_field(s548, 1);
    u_value t550 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_list_46__43__43__2(v_args, v_rest));
    if (!u_truth(t550)) {
      t550 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_more, v_ps, v_rest);
    }
    t549 = t550;
  }
  if (!t549) {
    u_no_case(s548);
  }
  return t549;
}

u_value f_check_46_witness_2(u_value v_h, u_value v_args) {
  u_value s551 = v_h;
  u_value t552 = NULL;
  if ((!t552) && u_is_con(s551, &n_check_46_HCon)) {
    u_value v_n = u_field(s551, 0);
    u_value t553;
    if (u_truth(u_eq(v_n, f_syntax_46_tuple_name_0()))) {
      t553 = u_mk_con(&n_syntax_46_PTuple, 1, v_args);
    } else {
      t553 = u_mk_con(&n_syntax_46_PApp, 2, v_n, v_args);
    }
    t552 = t553;
  }
  if ((!t552) && u_is_con(s551, &n_check_46_HLit)) {
    u_value v_v = u_field(s551, 0);
    t552 = u_mk_con(&n_syntax_46_PLit, 1, v_v);
  }
  if (!t552) {
    u_no_case(s551);
  }
  return t552;
}

u_value f_check_46_missing_4(u_value v_c, u_value v_su, u_value v_rows, u_value v_ts) {
  u_value s554 = v_ts;
  u_value t555 = NULL;
  if ((!t555) && u_is_con(s554, &n_list_46_Nil)) {
    u_value t556;
    if (u_truth(u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0)))) {
      t556 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
    } else {
      t556 = u_mk_con(&n_option_46_None, 0);
    }
    t555 = t556;
  }
  if ((!t555) && u_is_con(s554, &n_list_46_Cons)) {
    u_value v_t = u_field(s554, 0);
    u_value v_rest = u_field(s554, 1);
    t555 = f_check_46_missing_in_6(v_c, v_su, v_rows, f_check_46_heads_1(v_rows), f_check_46_constructors_of_3(v_c, v_su, v_t), v_rest);
  }
  if (!t555) {
    u_no_case(s554);
  }
  return t555;
}

u_value f_check_46_missing_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s557 = v_cs;
  u_value t558 = NULL;
  if ((!t558) && u_is_con(s557, &n_option_46_Some)) {
    u_value v_ks = u_field(s557, 0);
    u_value t559;
    if (u_truth(f_check_46_covers_2(v_hs, v_ks))) {
      t559 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_ks, v_rest);
    } else {
      t559 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
    }
    t558 = t559;
  }
  if ((!t558) && u_is_con(s557, &n_option_46_None)) {
    t558 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
  }
  if (!t558) {
    u_no_case(s557);
  }
  return t558;
}

u_value f_check_46_first_missing_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_rest) {
  u_value s560 = v_ks;
  u_value t561 = NULL;
  if ((!t561) && u_is_con(s560, &n_list_46_Nil)) {
    t561 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t561) && u_is_con(s560, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s560, 0), 0);
    u_value v_args = u_field(u_field(s560, 0), 1);
    u_value v_more = u_field(s560, 1);
    u_value s562 = f_check_46_missing_4(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_rest));
    u_value t563 = NULL;
    if ((!t563) && u_is_con(s562, &n_option_46_Some)) {
      u_value v_ws = u_field(s562, 0);
      t563 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_witness_2(v_h, f_list_46_take_2(f_list_46_length_1(v_args), v_ws)), f_list_46_drop_2(f_list_46_length_1(v_args), v_ws)));
    }
    if ((!t563) && u_is_con(s562, &n_option_46_None)) {
      t563 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_more, v_rest);
    }
    if (!t563) {
      u_no_case(s562);
    }
    t561 = t563;
  }
  if (!t561) {
    u_no_case(s560);
  }
  return t561;
}

u_value f_check_46_other_missing_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s564 = f_check_46_missing_4(v_c, v_su, f_check_46_defaults_1(v_rows), v_rest);
  u_value t565 = NULL;
  if ((!t565) && u_is_con(s564, &n_option_46_Some)) {
    u_value v_ws = u_field(s564, 0);
    t565 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_uncovered_2(v_hs, v_cs), v_ws));
  }
  if ((!t565) && u_is_con(s564, &n_option_46_None)) {
    t565 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t565) {
    u_no_case(s564);
  }
  return t565;
}

u_value f_check_46_uncovered_2(u_value v_hs, u_value v_cs) {
  u_value s566 = v_cs;
  u_value t567 = NULL;
  if ((!t567) && u_is_con(s566, &n_option_46_Some)) {
    u_value v_ks = u_field(s566, 0);
    u_value t568;
    if (u_truth(u_eq(v_hs, u_mk_con(&n_list_46_Nil, 0)))) {
      t568 = u_mk_con(&n_syntax_46_PWild, 0);
    } else {
      t568 = f_check_46_first_uncovered_2(v_hs, v_ks);
    }
    t567 = t568;
  }
  if ((!t567) && u_is_con(s566, &n_option_46_None)) {
    t567 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if (!t567) {
    u_no_case(s566);
  }
  return t567;
}

u_value f_check_46_first_uncovered_2(u_value v_hs, u_value v_ks) {
  u_value s569 = v_ks;
  u_value t570 = NULL;
  if ((!t570) && u_is_con(s569, &n_list_46_Nil)) {
    t570 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if ((!t570) && u_is_con(s569, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s569, 0), 0);
    u_value v_args = u_field(u_field(s569, 0), 1);
    u_value v_rest = u_field(s569, 1);
    u_value t571;
    if (u_truth(f_list_46_contains_2(v_h, v_hs))) {
      t571 = f_check_46_first_uncovered_2(v_hs, v_rest);
    } else {
      t571 = f_check_46_witness_2(v_h, f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)));
    }
    t570 = t571;
  }
  if (!t570) {
    u_no_case(s569);
  }
  return t570;
}

void p_check_46_fresh(u_value *r_t, u_value *r_s) {
  {
    u_value s572 = *r_s;
    int d573 = 0;
    if ((!d573) && u_is_con(s572, &n_check_46_State)) {
      u_value v_c = u_field(s572, 0);
      u_value v_su = u_field(s572, 1);
      u_value v_n = u_field(s572, 2);
      u_value v_m = u_field(s572, 3);
      u_value v_ms = u_field(s572, 4);
      u_value v_vs = u_field(s572, 5);
      d573 = 1;
      {
        u_value t574 = u_mk_con(&n_syntax_46_TMeta, 1, u_add(v_n, u_mk_int(1)));
        u_value t575 = u_mk_con(&n_check_46_State, 6, v_c, v_su, u_add(v_n, u_mk_int(1)), v_m, v_ms, v_vs);
        *r_t = t574;
        *r_s = t575;
      }
    }
    if (!d573) {
      u_no_case(s572);
    }
  }
}

void p_check_46_bind_meta(u_value v_i, u_value v_t, u_value *r_s) {
  {
    if (u_truth(f_check_46_occurs_3(f_check_46_State_46_sub_1(*r_s), v_i, v_t))) {
      {
        u_raise(u_mk_con(&n_check_46_InfiniteType, 2, u_mk_con(&n_syntax_46_TMeta, 1, v_i), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t)), "check.TypeError");
      }
    }
    u_value s576 = *r_s;
    int d577 = 0;
    if ((!d577) && u_is_con(s576, &n_check_46_State)) {
      u_value v_c = u_field(s576, 0);
      u_value v_su = u_field(s576, 1);
      u_value v_n = u_field(s576, 2);
      u_value v_m = u_field(s576, 3);
      u_value v_ms = u_field(s576, 4);
      u_value v_vs = u_field(s576, 5);
      d577 = 1;
      {
        u_value t578 = u_mk_con(&n_check_46_State, 6, v_c, f_map_46_put_3(v_i, v_t, v_su), v_n, v_m, v_ms, v_vs);
        *r_s = t578;
      }
    }
    if (!d577) {
      u_no_case(s576);
    }
  }
}

void p_check_46_unify(u_value v_t1, u_value v_t2, u_value *r_s) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value t579 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t1);
    v_a = t579;
    u_value t580 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t2);
    v_b = t580;
    u_value s581 = u_mk_tuple(2, v_a, v_b);
    int d582 = 0;
    if ((!d582) && (u_is_con(u_field(s581, 0), &n_syntax_46_TMeta) && u_is_con(u_field(s581, 1), &n_syntax_46_TMeta))) {
      u_value v_i = u_field(u_field(s581, 0), 0);
      u_value v_j = u_field(u_field(s581, 1), 0);
      d582 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_i, v_j))) {
          {
            p_check_46_bind_meta(v_i, v_b, r_s);
          }
        }
      }
    }
    if ((!d582) && u_is_con(u_field(s581, 0), &n_syntax_46_TMeta)) {
      u_value v_i = u_field(u_field(s581, 0), 0);
      d582 = 1;
      {
        p_check_46_bind_meta(v_i, v_b, r_s);
      }
    }
    if ((!d582) && u_is_con(u_field(s581, 1), &n_syntax_46_TMeta)) {
      u_value v_j = u_field(u_field(s581, 1), 0);
      d582 = 1;
      {
        p_check_46_bind_meta(v_j, v_a, r_s);
      }
    }
    if ((!d582) && (u_is_con(u_field(s581, 0), &n_syntax_46_TParam) && u_is_con(u_field(s581, 1), &n_syntax_46_TParam))) {
      u_value v_x = u_field(u_field(s581, 0), 0);
      u_value v_y = u_field(u_field(s581, 1), 0);
      d582 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_x, v_y))) {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, v_a, v_b), "check.TypeError");
          }
        }
      }
    }
    if ((!d582) && (u_is_con(u_field(s581, 0), &n_syntax_46_TSort) && u_is_con(u_field(s581, 1), &n_syntax_46_TSort))) {
      u_value v_f = u_field(u_field(s581, 0), 0);
      u_value v_xs = u_field(u_field(s581, 0), 1);
      u_value v_g = u_field(u_field(s581, 1), 0);
      u_value v_ys = u_field(u_field(s581, 1), 1);
      d582 = 1;
      {
        u_value t583 = u_eq(v_f, v_g);
        if (u_truth(t583)) {
          t583 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys));
        }
        if (u_truth(t583)) {
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
    if ((!d582) && (u_is_con(u_field(s581, 0), &n_syntax_46_TFun) && u_is_con(u_field(s581, 1), &n_syntax_46_TFun))) {
      u_value v_xs = u_field(u_field(s581, 0), 0);
      u_value v_r1 = u_field(u_field(s581, 0), 1);
      u_value v_ys = u_field(u_field(s581, 1), 0);
      u_value v_r2 = u_field(u_field(s581, 1), 1);
      d582 = 1;
      {
        if (u_truth(u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)))) {
          {
            p_check_46_unifies(f_list_46__43__43__2(v_xs, u_mk_con(&n_list_46_Cons, 2, v_r1, u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_ys, u_mk_con(&n_list_46_Cons, 2, v_r2, u_mk_con(&n_list_46_Nil, 0))), r_s);
          }
        } else {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t2)), "check.TypeError");
          }
        }
      }
    }
    if ((!d582) && (u_is_con(u_field(s581, 0), &n_syntax_46_TTuple) && u_is_con(u_field(s581, 1), &n_syntax_46_TTuple))) {
      u_value v_xs = u_field(u_field(s581, 0), 0);
      u_value v_ys = u_field(u_field(s581, 1), 0);
      d582 = 1;
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
    if (!d582) {
      d582 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t2)), "check.TypeError");
      }
    }
    if (!d582) {
      u_no_case(s581);
    }
  }
}

void p_check_46_unifies(u_value v_xs, u_value v_ys, u_value *r_s) {
  {
    u_value s584 = u_mk_tuple(2, v_xs, v_ys);
    int d585 = 0;
    if ((!d585) && (u_is_con(u_field(s584, 0), &n_list_46_Cons) && u_is_con(u_field(s584, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s584, 0), 0);
      u_value v_xs1 = u_field(u_field(s584, 0), 1);
      u_value v_y = u_field(u_field(s584, 1), 0);
      u_value v_ys1 = u_field(u_field(s584, 1), 1);
      d585 = 1;
      {
        p_check_46_unify(v_x, v_y, r_s);
        p_check_46_unifies(v_xs1, v_ys1, r_s);
      }
    }
    if ((!d585) && (u_is_con(u_field(s584, 0), &n_list_46_Nil) && u_is_con(u_field(s584, 1), &n_list_46_Nil))) {
      d585 = 1;
      {
      }
    }
    if (!d585) {
      d585 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)), "check.TypeError");
      }
    }
    if (!d585) {
      u_no_case(s584);
    }
  }
}

void p_check_46_fresh_list(u_value v_xs, u_value *r_ts, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s586 = v_xs;
    int d587 = 0;
    if ((!d587) && u_is_con(s586, &n_list_46_Nil)) {
      d587 = 1;
      {
        u_value t588 = u_mk_con(&n_list_46_Nil, 0);
        *r_ts = t588;
      }
    }
    if ((!d587) && u_is_con(s586, &n_list_46_Cons)) {
      u_value v_more = u_field(s586, 1);
      d587 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_fresh_list(v_more, &v_rest, r_s);
        u_value t589 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rest);
        *r_ts = t589;
      }
    }
    if (!d587) {
      u_no_case(s586);
    }
  }
}

void p_check_46_instance(u_value v_ps, u_value *r_m, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s590 = v_ps;
    int d591 = 0;
    if ((!d591) && u_is_con(s590, &n_list_46_Nil)) {
      d591 = 1;
      {
        u_value t592 = u_mk_con(&n_list_46_Nil, 0);
        *r_m = t592;
      }
    }
    if ((!d591) && u_is_con(s590, &n_list_46_Cons)) {
      u_value v_p = u_field(s590, 0);
      u_value v_qs = u_field(s590, 1);
      d591 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_instance(v_qs, &v_rest, r_s);
        u_value t593 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_p, v_t), v_rest);
        *r_m = t593;
      }
    }
    if (!d591) {
      u_no_case(s590);
    }
  }
}

void p_check_46_instantiate(u_value v_sc, u_value *r_t, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value s594 = v_sc;
    int d595 = 0;
    if (!d595) {
      u_value v_ps = u_field(s594, 0);
      u_value v_u = u_field(s594, 1);
      d595 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        u_value t596 = f_check_46_subst_2(f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u), v_m);
        *r_t = t596;
      }
    }
    if (!d595) {
      u_no_case(s594);
    }
  }
}

void p_check_46_unique(u_value v_ns) {
  {
    u_value s597 = f_list_46_repeated_1(v_ns);
    int d598 = 0;
    if ((!d598) && u_is_con(s597, &n_option_46_Some)) {
      u_value v_n = u_field(s597, 0);
      d598 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, f_syntax_46_qualified_1(v_n)), "check.TypeError");
      }
    }
    if ((!d598) && u_is_con(s597, &n_option_46_None)) {
      d598 = 1;
      {
      }
    }
    if (!d598) {
      u_no_case(s597);
    }
  }
}

void p_check_46_unique_ids(u_value v_ns) {
  {
    u_value s599 = f_list_46_repeated_1(v_ns);
    int d600 = 0;
    if ((!d600) && u_is_con(s599, &n_option_46_Some)) {
      u_value v_n = u_field(s599, 0);
      d600 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, v_n), "check.TypeError");
      }
    }
    if ((!d600) && u_is_con(s599, &n_option_46_None)) {
      d600 = 1;
      {
      }
    }
    if (!d600) {
      u_no_case(s599);
    }
  }
}

void p_check_46_one_of(u_value v_n, u_value v_ns, u_value *r_m) {
  {
    u_value s601 = v_ns;
    int d602 = 0;
    if ((!d602) && (u_is_con(s601, &n_list_46_Cons) && u_is_con(u_field(s601, 1), &n_list_46_Nil))) {
      u_value v_k = u_field(s601, 0);
      d602 = 1;
      {
        u_value t603 = v_k;
        *r_m = t603;
      }
    }
    if ((!d602) && u_is_con(s601, &n_list_46_Nil)) {
      d602 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if (!d602) {
      d602 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ns), "check.TypeError");
      }
    }
    if (!d602) {
      u_no_case(s601);
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
    u_value s604 = v_t;
    int d605 = 0;
    if ((!d605) && u_is_con(s604, &n_syntax_46_THole)) {
      d605 = 1;
      {
        p_check_46_fresh(r_u, r_s);
      }
    }
    if ((!d605) && u_is_con(s604, &n_syntax_46_TParam)) {
      d605 = 1;
      {
        u_value t606 = v_t;
        *r_u = t606;
      }
    }
    if ((!d605) && u_is_con(s604, &n_syntax_46_TMeta)) {
      d605 = 1;
      {
        u_value t607 = v_t;
        *r_u = t607;
      }
    }
    if ((!d605) && u_is_con(s604, &n_syntax_46_TFun)) {
      u_value v_args = u_field(s604, 0);
      u_value v_r = u_field(s604, 1);
      d605 = 1;
      {
        p_check_46_normalizes(v_args, &v_us, r_s);
        p_check_46_normalize(v_r, &v_v, r_s);
        u_value t608 = u_mk_con(&n_syntax_46_TFun, 2, v_us, v_v);
        *r_u = t608;
      }
    }
    if ((!d605) && u_is_con(s604, &n_syntax_46_TTuple)) {
      u_value v_ts = u_field(s604, 0);
      d605 = 1;
      {
        p_check_46_normalizes(v_ts, &v_us, r_s);
        u_value t609 = u_mk_con(&n_syntax_46_TTuple, 1, v_us);
        *r_u = t609;
      }
    }
    if ((!d605) && u_is_con(s604, &n_syntax_46_TSort)) {
      u_value v_n = u_field(s604, 0);
      u_value v_args = u_field(s604, 1);
      d605 = 1;
      {
        if (u_truth(f_check_46_basic_1(v_n))) {
          {
            p_check_46_arguments(v_n, u_mk_int(0), v_args);
            u_value t610 = v_t;
            *r_u = t610;
          }
        } else {
          {
            u_value s612 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam611, 1, *r_s));
            int d613 = 0;
            if ((!d613) && (u_is_con(s612, &n_list_46_Cons) && u_is_con(u_field(s612, 1), &n_list_46_Nil))) {
              u_value v_m = u_field(s612, 0);
              d613 = 1;
              {
                p_check_46_normalize_sort(v_m, v_args, r_u, r_s);
              }
            }
            if ((!d613) && u_is_con(s612, &n_list_46_Nil)) {
              d613 = 1;
              {
                u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
              }
            }
            if (!d613) {
              u_value v_ms = s612;
              d613 = 1;
              {
                u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ms), "check.TypeError");
              }
            }
            if (!d613) {
              u_no_case(s612);
            }
          }
        }
      }
    }
    if (!d605) {
      u_no_case(s604);
    }
  }
}

void p_check_46_normalize_sort(u_value v_n, u_value v_args, u_value *r_u, u_value *r_s) {
  {
    u_value v_us = NULL;
    u_value s614 = f_ctx_46_data_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
    int d615 = 0;
    if ((!d615) && u_is_con(s614, &n_option_46_Some)) {
      u_value v_k = u_field(s614, 0);
      d615 = 1;
      {
        p_check_46_arguments(v_n, v_k, v_args);
        p_check_46_normalizes(v_args, &v_us, r_s);
        u_value t616 = u_mk_con(&n_syntax_46_TSort, 2, v_n, v_us);
        *r_u = t616;
      }
    }
    if ((!d615) && u_is_con(s614, &n_option_46_None)) {
      d615 = 1;
      {
        u_value s617 = f_ctx_46_alias_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
        int d618 = 0;
        if ((!d618) && u_is_con(s617, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s617, 0), 0);
          u_value v_rhs = u_field(u_field(s617, 0), 1);
          d618 = 1;
          {
            p_check_46_arguments(v_n, f_list_46_length_1(v_ps), v_args);
            p_check_46_normalizes(v_args, &v_us, r_s);
            u_value t619 = f_check_46_subst_2(v_rhs, f_list_46_zip_2(f_check_46_params_1(v_ps), v_us));
            *r_u = t619;
          }
        }
        if ((!d618) && u_is_con(s617, &n_option_46_None)) {
          d618 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
          }
        }
        if (!d618) {
          u_no_case(s617);
        }
      }
    }
    if (!d615) {
      u_no_case(s614);
    }
  }
}

void p_check_46_normalizes(u_value v_ts, u_value *r_us, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_rest = NULL;
    u_value s620 = v_ts;
    int d621 = 0;
    if ((!d621) && u_is_con(s620, &n_list_46_Nil)) {
      d621 = 1;
      {
        u_value t622 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t622;
      }
    }
    if ((!d621) && u_is_con(s620, &n_list_46_Cons)) {
      u_value v_t = u_field(s620, 0);
      u_value v_more = u_field(s620, 1);
      d621 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalizes(v_more, &v_rest, r_s);
        u_value t623 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t623;
      }
    }
    if (!d621) {
      u_no_case(s620);
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
    u_value s624 = v_ts;
    int d625 = 0;
    if ((!d625) && u_is_con(s624, &n_list_46_Nil)) {
      d625 = 1;
      {
        u_value t626 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t626;
      }
    }
    if ((!d625) && u_is_con(s624, &n_list_46_Cons)) {
      u_value v_t = u_field(s624, 0);
      u_value v_more = u_field(s624, 1);
      d625 = 1;
      {
        p_check_46_known(v_t, v_what, &v_u, r_s);
        p_check_46_knowns(v_more, v_what, &v_rest, r_s);
        u_value t627 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t627;
      }
    }
    if (!d625) {
      u_no_case(s624);
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
    u_value s628 = v_xs;
    int d629 = 0;
    if ((!d629) && u_is_con(s628, &n_list_46_Nil)) {
      d629 = 1;
      {
        u_value t630 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t630;
      }
    }
    if ((!d629) && (u_is_con(s628, &n_list_46_Cons) && u_is_con(u_field(s628, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s628, 0), 0);
      u_value v_t = u_field(u_field(s628, 0), 1);
      u_value v_more = u_field(s628, 1);
      d629 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalize_vars(v_more, &v_rest, r_s);
        u_value t631 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t631;
      }
    }
    if (!d629) {
      u_no_case(s628);
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
    u_value s632 = v_xs;
    int d633 = 0;
    if ((!d633) && u_is_con(s632, &n_list_46_Nil)) {
      d633 = 1;
      {
        u_value t634 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t634;
      }
    }
    if ((!d633) && (u_is_con(s632, &n_list_46_Cons) && u_is_con(u_field(s632, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s632, 0), 0);
      u_value v_t = u_field(u_field(s632, 0), 1);
      u_value v_more = u_field(s632, 1);
      d633 = 1;
      {
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_known_vars(v_more, &v_rest, r_s);
        u_value t635 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t635;
      }
    }
    if (!d633) {
      u_no_case(s632);
    }
  }
}

void p_check_46_resolve_name(u_value v_s, u_value v_env, u_value v_n, u_value v_k, u_value *r_m, u_value *r_sc) {
  {
    u_value s636 = f_check_46_candidates_3(v_s, v_env, v_n);
    int d637 = 0;
    if ((!d637) && u_is_con(s636, &n_list_46_Nil)) {
      d637 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if ((!d637) && (u_is_con(s636, &n_list_46_Cons) && u_is_con(u_field(s636, 1), &n_list_46_Nil))) {
      u_value v_m1 = u_field(u_field(s636, 0), 0);
      u_value v_one = u_field(u_field(s636, 0), 1);
      d637 = 1;
      {
        u_value t638 = v_m1;
        u_value t639 = v_one;
        *r_m = t638;
        *r_sc = t639;
      }
    }
    if (!d637) {
      u_value v_cs = s636;
      d637 = 1;
      {
        if (u_truth(u_lt(v_k, u_mk_int(0)))) {
          {
            u_raise(u_mk_con(&n_check_46_OverloadedUnapplied, 1, v_n), "check.TypeError");
          }
        }
        u_value s640 = f_check_46_with_arity_2(v_cs, v_k);
        int d641 = 0;
        if ((!d641) && (u_is_con(s640, &n_list_46_Cons) && u_is_con(u_field(s640, 1), &n_list_46_Nil))) {
          u_value v_m1 = u_field(u_field(s640, 0), 0);
          u_value v_one = u_field(u_field(s640, 0), 1);
          d641 = 1;
          {
            u_value t642 = v_m1;
            u_value t643 = v_one;
            *r_m = t642;
            *r_sc = t643;
          }
        }
        if ((!d641) && u_is_con(s640, &n_list_46_Nil)) {
          d641 = 1;
          {
            u_raise(u_mk_con(&n_check_46_WrongArity, 2, v_n, v_k), "check.TypeError");
          }
        }
        if (!d641) {
          u_value v_more = s640;
          d641 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, f_list_46_firsts_1(v_more)), "check.TypeError");
          }
        }
        if (!d641) {
          u_no_case(s640);
        }
      }
    }
    if (!d637) {
      u_no_case(s636);
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
    u_value t644 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_u);
    *r_x = t644;
  }
}

void p_check_46_infer(u_value v_env, u_value v_e, u_value *r_e1, u_value *r_t, u_value *r_s) {
  {
    u_value v_sc = NULL;
    u_value v_m = NULL;
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    u_value v_c = NULL;
    u_value v_l = NULL;
    u_value v_u = NULL;
    u_value v_w = NULL;
    u_value v_es = NULL;
    u_value v_ts = NULL;
    u_value v_bs = NULL;
    u_value s645 = v_e;
    int d646 = 0;
    if ((!d646) && u_is_con(s645, &n_syntax_46_ELit)) {
      u_value v_v = u_field(s645, 0);
      d646 = 1;
      {
        u_value t647 = v_e;
        u_value t648 = f_check_46_literal_1(v_v);
        *r_e1 = t647;
        *r_t = t648;
      }
    }
    if ((!d646) && (u_is_con(s645, &n_syntax_46_EVar) && u_is_con(u_field(s645, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s645, 0), 0);
      u_value v_a = u_field(u_field(s645, 0), 1);
      d646 = 1;
      {
        p_check_46_resolve_name(*r_s, v_env, v_n, f_prelude_46__45__1(u_mk_int(1)), &v_m, &v_sc);
        p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
        u_value t649 = u_mk_con(&n_syntax_46_EVar, 1, v_x);
        u_value t650 = f_syntax_46_Var_46_typ_1(v_x);
        *r_e1 = t649;
        *r_t = t650;
      }
    }
    if ((!d646) && u_is_con(s645, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s645, 0);
      u_value v_args = u_field(s645, 1);
      d646 = 1;
      {
        p_check_46_application(v_env, v_f, v_args, r_e1, r_t, r_s);
      }
    }
    if ((!d646) && u_is_con(s645, &n_syntax_46_ELam)) {
      u_value v_bound = u_field(s645, 0);
      u_value v_body = u_field(s645, 1);
      d646 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t651 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, v_b);
        u_value t652 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), v_u);
        *r_e1 = t651;
        *r_t = t652;
      }
    }
    if ((!d646) && u_is_con(s645, &n_syntax_46_ELet)) {
      u_value v_bound = u_field(s645, 0);
      u_value v_body = u_field(s645, 1);
      d646 = 1;
      {
        p_check_46_formals(f_list_46_firsts_1(v_bound), &v_xs, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_bound), f_syntax_46_types_1(v_xs), &v_es, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_generalized_3(f_check_46_State_46_sub_1(*r_s), v_env, v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t653 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        u_value t654 = v_u;
        *r_e1 = t653;
        *r_t = t654;
      }
    }
    if ((!d646) && u_is_con(s645, &n_syntax_46_EIte)) {
      u_value v_cond = u_field(s645, 0);
      u_value v_left = u_field(s645, 1);
      u_value v_right = u_field(s645, 2);
      d646 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(v_env, v_left, &v_l, &v_u, r_s);
        p_check_46_check(v_env, v_right, v_u, &v_b, r_s);
        u_value t655 = u_mk_con(&n_syntax_46_EIte, 3, v_c, v_l, v_b);
        u_value t656 = v_u;
        *r_e1 = t655;
        *r_t = t656;
      }
    }
    if ((!d646) && u_is_con(s645, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s645, 0);
      u_value v_bound = u_field(s645, 1);
      u_value v_lists = u_field(s645, 2);
      u_value v_cond = u_field(s645, 3);
      d646 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_same_length(f_list_46_length_1(v_xs), f_list_46_length_1(v_lists));
        p_check_46_generators(v_env, v_xs, v_lists, &v_es, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_result, &v_b, &v_u, r_s);
        u_value t657 = u_mk_con(&n_syntax_46_EComp, 4, v_b, v_xs, v_es, v_c);
        u_value t658 = f_check_46_tlist_1(v_u);
        *r_e1 = t657;
        *r_t = t658;
      }
    }
    if ((!d646) && u_is_con(s645, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s645, 0);
      d646 = 1;
      {
        p_check_46_infers(v_env, v_items, &v_es, &v_ts, r_s);
        u_value t659 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
        u_value t660 = u_mk_con(&n_syntax_46_TTuple, 1, v_ts);
        *r_e1 = t659;
        *r_t = t660;
      }
    }
    if ((!d646) && u_is_con(s645, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s645, 0);
      u_value v_a = u_field(s645, 1);
      d646 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_normalize(v_a, &v_w, r_s);
        p_check_46_unify(v_u, v_w, r_s);
        p_check_46_fresh(r_t, r_s);
        u_value t661 = u_mk_con(&n_syntax_46_ERaise, 2, v_c, v_u);
        *r_e1 = t661;
      }
    }
    if ((!d646) && u_is_con(s645, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s645, 0);
      u_value v_cases = u_field(s645, 1);
      d646 = 1;
      {
        p_check_46_fresh(r_t, r_s);
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_expr_branches(v_env, v_cases, v_u, *r_t, &v_bs, r_s);
        u_value t662 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e663 = &t662;
        u_value l664 = v_bs;
        while (u_is_con(l664, &n_list_46_Cons)) {
          u_value v_br = u_field(l664, 0);
          if (u_truth(u_mk_bool(1))) {
            e663 = u_append(e663, f_check_46_case_of_1(v_br));
          }
          l664 = u_field(l664, 1);
        }
        p_check_46_coverage(*r_s, v_u, t662);
        u_value t665 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, v_bs);
        *r_e1 = t665;
      }
    }
    if (!d646) {
      u_no_case(s645);
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
    u_value s666 = v_f;
    int d667 = 0;
    if ((!d667) && (u_is_con(s666, &n_syntax_46_EVar) && (u_is_con(u_field(s666, 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(s666, 0), 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(u_field(s666, 0), 0), 0), &n_option_46_None))))) {
      u_value v_b = u_field(u_field(u_field(s666, 0), 0), 1);
      u_value v_a = u_field(u_field(s666, 0), 1);
      u_value t668 = f_prelude_46__62__2(u_str_size(v_b), u_mk_int(1));
      if (u_truth(t668)) {
        t668 = u_eq(u_str_get(v_b, u_mk_int(0)), u_mk_char(46));
      }
      if (u_truth(t668)) {
        d667 = 1;
        {
          p_check_46_field_access(v_env, v_b, v_a, v_args, r_e1, r_t, r_s);
        }
      }
    }
    if ((!d667) && (u_is_con(s666, &n_syntax_46_EVar) && u_is_con(u_field(s666, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s666, 0), 0);
      u_value v_a = u_field(u_field(s666, 0), 1);
      d667 = 1;
      {
        if (u_truth(f_prelude_46__62__2(f_list_46_length_1(f_check_46_candidates_3(*r_s, v_env, v_n)), u_mk_int(1)))) {
          {
            p_check_46_resolve_name(*r_s, v_env, v_n, f_list_46_length_1(v_args), &v_m, &v_sc);
            p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
            p_check_46_apply(v_env, f_syntax_46_Var_46_typ_1(v_x), v_args, &v_args1, r_t, r_s);
            u_value t669 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), v_args1);
            *r_e1 = t669;
          }
        } else {
          {
            p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
            p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
            u_value t670 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
            *r_e1 = t670;
          }
        }
      }
    }
    if (!d667) {
      d667 = 1;
      {
        p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
        p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
        u_value t671 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
        *r_e1 = t671;
      }
    }
    if (!d667) {
      u_no_case(s666);
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
    u_value s672 = v_args;
    int d673 = 0;
    if ((!d673) && u_is_con(s672, &n_list_46_Cons)) {
      u_value v_arg = u_field(s672, 0);
      u_value v_rest = u_field(s672, 1);
      d673 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_arg1, &v_u, r_s);
        u_value s674 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u);
        int d675 = 0;
        if ((!d675) && (u_is_con(s674, &n_syntax_46_TSort) && (u_is_con(u_field(s674, 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(s674, 0), 0), &n_option_46_Some)))) {
          u_value v_m = u_field(u_field(u_field(s674, 0), 0), 0);
          u_value v_tn = u_field(u_field(s674, 0), 1);
          d675 = 1;
          {
            u_value t676 = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), u_concat(v_tn, v_f));
            v_acc = t676;
          }
        }
        if (!d675) {
          u_value v_v = s674;
          d675 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownFieldType, 2, v_f, v_v), "check.TypeError");
          }
        }
        if (!d675) {
          u_no_case(s674);
        }
        u_value s677 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(*r_s), v_acc);
        int d678 = 0;
        if ((!d678) && (u_is_con(s677, &n_list_46_Cons) && u_is_con(u_field(s677, 1), &n_list_46_Nil))) {
          u_value v_sc = u_field(s677, 0);
          d678 = 1;
          {
            p_check_46_use(v_acc, v_a, v_sc, &v_x, r_s);
          }
        }
        if (!d678) {
          d678 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownField, 1, v_acc), "check.TypeError");
          }
        }
        if (!d678) {
          u_no_case(s677);
        }
        p_check_46_infers(v_env, v_rest, &v_rest1, &v_us, r_s);
        p_check_46_fresh(r_t, r_s);
        p_check_46_unify(f_syntax_46_Var_46_typ_1(v_x), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_u, v_us), *r_t), r_s);
        u_value t679 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), u_mk_con(&n_list_46_Cons, 2, v_arg1, v_rest1));
        *r_e1 = t679;
      }
    }
    if (!d673) {
      d673 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, u_mk_int(1), f_list_46_length_1(v_args)), "check.TypeError");
      }
    }
    if (!d673) {
      u_no_case(s672);
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
    u_value s680 = u_mk_tuple(2, v_es, v_ts);
    int d681 = 0;
    if ((!d681) && (u_is_con(u_field(s680, 0), &n_list_46_Cons) && u_is_con(u_field(s680, 1), &n_list_46_Cons))) {
      u_value v_e = u_field(u_field(s680, 0), 0);
      u_value v_more = u_field(u_field(s680, 0), 1);
      u_value v_t = u_field(u_field(s680, 1), 0);
      u_value v_ts1 = u_field(u_field(s680, 1), 1);
      d681 = 1;
      {
        p_check_46_check(v_env, v_e, v_t, &v_e1, r_s);
        p_check_46_checks(v_env, v_more, v_ts1, &v_rest, r_s);
        u_value t682 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        *r_es1 = t682;
      }
    }
    if ((!d681) && (u_is_con(u_field(s680, 0), &n_list_46_Nil) && u_is_con(u_field(s680, 1), &n_list_46_Nil))) {
      d681 = 1;
      {
        u_value t683 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t683;
      }
    }
    if (!d681) {
      d681 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_es)), "check.TypeError");
      }
    }
    if (!d681) {
      u_no_case(s680);
    }
  }
}

void p_check_46_infers(u_value v_env, u_value v_es, u_value *r_es1, u_value *r_ts, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value v_us = NULL;
    u_value s684 = v_es;
    int d685 = 0;
    if ((!d685) && u_is_con(s684, &n_list_46_Nil)) {
      d685 = 1;
      {
        u_value t686 = u_mk_con(&n_list_46_Nil, 0);
        u_value t687 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t686;
        *r_ts = t687;
      }
    }
    if ((!d685) && u_is_con(s684, &n_list_46_Cons)) {
      u_value v_e = u_field(s684, 0);
      u_value v_more = u_field(s684, 1);
      d685 = 1;
      {
        p_check_46_infer(v_env, v_e, &v_e1, &v_t, r_s);
        p_check_46_infers(v_env, v_more, &v_rest, &v_us, r_s);
        u_value t688 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        u_value t689 = u_mk_con(&n_list_46_Cons, 2, v_t, v_us);
        *r_es1 = t688;
        *r_ts = t689;
      }
    }
    if (!d685) {
      u_no_case(s684);
    }
  }
}

void p_check_46_generators(u_value v_env, u_value v_xs, u_value v_lists, u_value *r_lists1, u_value *r_s) {
  {
    u_value v_l = NULL;
    u_value v_rest = NULL;
    u_value s690 = u_mk_tuple(2, v_xs, v_lists);
    int d691 = 0;
    if ((!d691) && (u_is_con(u_field(s690, 0), &n_list_46_Cons) && u_is_con(u_field(s690, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s690, 0), 0);
      u_value v_ys = u_field(u_field(s690, 0), 1);
      u_value v_e = u_field(u_field(s690, 1), 0);
      u_value v_es = u_field(u_field(s690, 1), 1);
      d691 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tlist_1(f_syntax_46_Var_46_typ_1(v_x)), &v_l, r_s);
        p_check_46_generators(f_list_46__43__43__2(f_check_46_bind_1(u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0))), v_env), v_ys, v_es, &v_rest, r_s);
        u_value t692 = u_mk_con(&n_list_46_Cons, 2, v_l, v_rest);
        *r_lists1 = t692;
      }
    }
    if (!d691) {
      d691 = 1;
      {
        u_value t693 = u_mk_con(&n_list_46_Nil, 0);
        *r_lists1 = t693;
      }
    }
    if (!d691) {
      u_no_case(s690);
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
    u_value s694 = v_cases;
    int d695 = 0;
    if ((!d695) && u_is_con(s694, &n_list_46_Nil)) {
      d695 = 1;
      {
        u_value t696 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t696;
      }
    }
    if ((!d695) && (u_is_con(s694, &n_list_46_Cons) && u_is_con(u_field(s694, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s694, 0), 0);
      u_value v_g = u_field(u_field(s694, 0), 1);
      u_value v_b = u_field(u_field(s694, 0), 2);
      u_value v_more = u_field(s694, 1);
      d695 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_b, v_r, &v_b1, r_s);
        p_check_46_expr_branches(v_env, v_more, v_t, v_r, &v_rest, r_s);
        u_value t697 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t697;
      }
    }
    if (!d695) {
      u_no_case(s694);
    }
  }
}

void p_check_46_guard(u_value v_env, u_value v_g, u_value *r_g1, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value s698 = v_g;
    int d699 = 0;
    if ((!d699) && u_is_con(s698, &n_option_46_Some)) {
      u_value v_e = u_field(s698, 0);
      d699 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tbool_0(), &v_e1, r_s);
        u_value t700 = u_mk_con(&n_option_46_Some, 1, v_e1);
        *r_g1 = t700;
      }
    }
    if ((!d699) && u_is_con(s698, &n_option_46_None)) {
      d699 = 1;
      {
        u_value t701 = u_mk_con(&n_option_46_None, 0);
        *r_g1 = t701;
      }
    }
    if (!d699) {
      u_no_case(s698);
    }
  }
}

void p_check_46_coverage(u_value v_s, u_value v_t, u_value v_cs) {
  {
    p_check_46_reachable(v_s, v_t, u_mk_con(&n_list_46_Nil, 0), v_cs);
    u_value t702 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e703 = &t702;
    u_value l704 = f_check_46_unguarded_1(v_cs);
    while (u_is_con(l704, &n_list_46_Cons)) {
      u_value v_p = u_field(l704, 0);
      if (u_truth(u_mk_bool(1))) {
        e703 = u_append(e703, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
      }
      l704 = u_field(l704, 1);
    }
    u_value s705 = f_check_46_missing_4(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t702, u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
    int d706 = 0;
    if ((!d706) && (u_is_con(s705, &n_option_46_Some) && u_is_con(u_field(s705, 0), &n_list_46_Cons))) {
      u_value v_w = u_field(u_field(s705, 0), 0);
      d706 = 1;
      {
        u_raise(u_mk_con(&n_check_46_NotExhaustive, 1, v_w), "check.TypeError");
      }
    }
    if (!d706) {
      d706 = 1;
      {
      }
    }
    if (!d706) {
      u_no_case(s705);
    }
  }
}

u_value f_check_46_unguarded_1(u_value v_cs) {
  u_value s707 = v_cs;
  u_value t708 = NULL;
  if ((!t708) && u_is_con(s707, &n_list_46_Nil)) {
    t708 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t708) && (u_is_con(s707, &n_list_46_Cons) && u_equals(u_field(u_field(s707, 0), 1), u_mk_bool(1)))) {
    u_value v_p = u_field(u_field(s707, 0), 0);
    u_value v_rest = u_field(s707, 1);
    t708 = u_mk_con(&n_list_46_Cons, 2, v_p, f_check_46_unguarded_1(v_rest));
  }
  if ((!t708) && (u_is_con(s707, &n_list_46_Cons) && u_equals(u_field(u_field(s707, 0), 1), u_mk_bool(0)))) {
    u_value v_rest = u_field(s707, 1);
    t708 = f_check_46_unguarded_1(v_rest);
  }
  if (!t708) {
    u_no_case(s707);
  }
  return t708;
}

void p_check_46_reachable(u_value v_s, u_value v_t, u_value v_before, u_value v_cs) {
  {
    u_value s709 = v_cs;
    int d710 = 0;
    if ((!d710) && u_is_con(s709, &n_list_46_Nil)) {
      d710 = 1;
      {
      }
    }
    if ((!d710) && u_is_con(s709, &n_list_46_Cons)) {
      u_value v_p = u_field(u_field(s709, 0), 0);
      u_value v_open = u_field(u_field(s709, 0), 1);
      u_value v_rest = u_field(s709, 1);
      d710 = 1;
      {
        u_value t711 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e712 = &t711;
        u_value l713 = v_before;
        while (u_is_con(l713, &n_list_46_Cons)) {
          u_value v_q = u_field(l713, 0);
          if (u_truth(u_mk_bool(1))) {
            e712 = u_append(e712, u_mk_con(&n_list_46_Cons, 2, v_q, u_mk_con(&n_list_46_Nil, 0)));
          }
          l713 = u_field(l713, 1);
        }
        if (u_truth(f_prelude_46_not_1(f_check_46_useful_5(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t711, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)))))) {
          {
            u_raise(u_mk_con(&n_check_46_Unreachable, 1, v_p), "check.TypeError");
          }
        }
        if (u_truth(v_open)) {
          {
            p_check_46_reachable(v_s, v_t, f_list_46__43__43__2(v_before, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0))), v_rest);
          }
        } else {
          {
            p_check_46_reachable(v_s, v_t, v_before, v_rest);
          }
        }
      }
    }
    if (!d710) {
      u_no_case(s709);
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
    u_value s714 = v_p;
    int d715 = 0;
    if ((!d715) && u_is_con(s714, &n_syntax_46_PWild)) {
      d715 = 1;
      {
        u_value t716 = v_p;
        u_value t717 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t716;
        *r_xs = t717;
      }
    }
    if ((!d715) && u_is_con(s714, &n_syntax_46_PLit)) {
      u_value v_v = u_field(s714, 0);
      d715 = 1;
      {
        p_check_46_unify(f_check_46_literal_1(v_v), v_t, r_s);
        u_value t718 = v_p;
        u_value t719 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t718;
        *r_xs = t719;
      }
    }
    if ((!d715) && (u_is_con(s714, &n_syntax_46_PVar) && u_is_con(u_field(s714, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s714, 0), 0);
      u_value v_a = u_field(u_field(s714, 0), 1);
      d715 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam720, 1, *r_s)), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            p_check_46_binders(u_mk_con(&n_syntax_46_PApp, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_t, r_p1, r_xs, r_s);
          }
        } else {
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_u, v_t, r_s);
            u_value t721 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u));
            u_value t722 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), u_mk_con(&n_list_46_Nil, 0));
            *r_p1 = t721;
            *r_xs = t722;
          }
        }
      }
    }
    if ((!d715) && u_is_con(s714, &n_syntax_46_PApp)) {
      u_value v_n = u_field(s714, 0);
      u_value v_args = u_field(s714, 1);
      d715 = 1;
      {
        u_value s724 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam723, 1, *r_s));
        int d725 = 0;
        if ((!d725) && (u_is_con(s724, &n_list_46_Cons) && u_is_con(u_field(s724, 1), &n_list_46_Nil))) {
          u_value v_k1 = u_field(s724, 0);
          d725 = 1;
          {
            u_value t726 = v_k1;
            v_k = t726;
          }
        }
        if ((!d725) && u_is_con(s724, &n_list_46_Nil)) {
          d725 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d725) {
          u_value v_ks = s724;
          d725 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ks), "check.TypeError");
          }
        }
        if (!d725) {
          u_no_case(s724);
        }
        u_value s727 = f_ctx_46_constr_of_2(f_check_46_State_46_ctx_1(*r_s), v_k);
        int d728 = 0;
        if ((!d728) && u_is_con(s727, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s727, 0), 0);
          u_value v_ks = u_field(u_field(s727, 0), 1);
          u_value v_r = u_field(u_field(s727, 0), 2);
          d728 = 1;
          {
            p_check_46_same_length(f_list_46_length_1(v_ks), f_list_46_length_1(v_args));
            p_check_46_instance(v_ps, &v_m, r_s);
            p_check_46_unify(f_check_46_subst_2(v_r, v_m), v_t, r_s);
            u_value t729 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e730 = &t729;
            u_value l731 = v_ks;
            while (u_is_con(l731, &n_list_46_Cons)) {
              u_value v_kt = u_field(l731, 0);
              if (u_truth(u_mk_bool(1))) {
                e730 = u_append(e730, f_check_46_subst_2(v_kt, v_m));
              }
              l731 = u_field(l731, 1);
            }
            p_check_46_binder_list(v_args, t729, &v_qs, r_xs, r_s);
            u_value t732 = u_mk_con(&n_syntax_46_PApp, 2, v_k, v_qs);
            *r_p1 = t732;
          }
        }
        if ((!d728) && u_is_con(s727, &n_option_46_None)) {
          d728 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d728) {
          u_no_case(s727);
        }
      }
    }
    if ((!d715) && u_is_con(s714, &n_syntax_46_PTuple)) {
      u_value v_items = u_field(s714, 0);
      d715 = 1;
      {
        p_check_46_fresh_list(v_items, &v_ts, r_s);
        p_check_46_unify(u_mk_con(&n_syntax_46_TTuple, 1, v_ts), v_t, r_s);
        p_check_46_binder_list(v_items, v_ts, &v_qs, r_xs, r_s);
        u_value t733 = u_mk_con(&n_syntax_46_PTuple, 1, v_qs);
        *r_p1 = t733;
      }
    }
    if (!d715) {
      u_no_case(s714);
    }
  }
}

void p_check_46_binder_list(u_value v_ps, u_value v_ts, u_value *r_qs, u_value *r_xs, u_value *r_s) {
  {
    u_value v_q = NULL;
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value v_zs = NULL;
    u_value s734 = u_mk_tuple(2, v_ps, v_ts);
    int d735 = 0;
    if ((!d735) && (u_is_con(u_field(s734, 0), &n_list_46_Cons) && u_is_con(u_field(s734, 1), &n_list_46_Cons))) {
      u_value v_p = u_field(u_field(s734, 0), 0);
      u_value v_more = u_field(u_field(s734, 0), 1);
      u_value v_t = u_field(u_field(s734, 1), 0);
      u_value v_ts1 = u_field(u_field(s734, 1), 1);
      d735 = 1;
      {
        p_check_46_binders(v_p, v_t, &v_q, &v_ys, r_s);
        p_check_46_binder_list(v_more, v_ts1, &v_rest, &v_zs, r_s);
        u_value t736 = u_mk_con(&n_list_46_Cons, 2, v_q, v_rest);
        u_value t737 = f_list_46__43__43__2(v_ys, v_zs);
        *r_qs = t736;
        *r_xs = t737;
      }
    }
    if ((!d735) && (u_is_con(u_field(s734, 0), &n_list_46_Nil) && u_is_con(u_field(s734, 1), &n_list_46_Nil))) {
      d735 = 1;
      {
        u_value t738 = u_mk_con(&n_list_46_Nil, 0);
        u_value t739 = u_mk_con(&n_list_46_Nil, 0);
        *r_qs = t738;
        *r_xs = t739;
      }
    }
    if (!d735) {
      d735 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_ps)), "check.TypeError");
      }
    }
    if (!d735) {
      u_no_case(s734);
    }
  }
}

void p_check_46_variable(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s740 = v_x;
    int d741 = 0;
    if ((!d741) && u_is_con(s740, &n_syntax_46_Var)) {
      u_value v_n = u_field(s740, 0);
      u_value v_a = u_field(s740, 1);
      d741 = 1;
      {
        if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, v_assignable)))) {
          {
            u_raise(u_mk_con(&n_check_46_NotAssignable, 1, v_n), "check.TypeError");
          }
        }
        u_value s742 = f_list_46_lookup_2(v_n, v_env);
        int d743 = 0;
        if ((!d743) && u_is_con(s742, &n_option_46_Some)) {
          u_value v_t = u_field(u_field(s742, 0), 1);
          d743 = 1;
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_t, v_u, r_s);
            u_value t744 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_t);
            *r_y = t744;
          }
        }
        if ((!d743) && u_is_con(s742, &n_option_46_None)) {
          d743 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
          }
        }
        if (!d743) {
          u_no_case(s742);
        }
      }
    }
    if (!d741) {
      u_no_case(s740);
    }
  }
}

void p_check_46_variables(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s745 = v_xs;
    int d746 = 0;
    if ((!d746) && u_is_con(s745, &n_list_46_Nil)) {
      d746 = 1;
      {
        u_value t747 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t747;
      }
    }
    if ((!d746) && u_is_con(s745, &n_list_46_Cons)) {
      u_value v_x = u_field(s745, 0);
      u_value v_more = u_field(s745, 1);
      d746 = 1;
      {
        p_check_46_variable(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_variables(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t748 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t748;
      }
    }
    if (!d746) {
      u_no_case(s745);
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
    u_value s749 = v_x;
    int d750 = 0;
    if ((!d750) && u_is_con(s749, &n_syntax_46_SSkip)) {
      d750 = 1;
      {
        u_value t751 = v_x;
        *r_y = t751;
      }
    }
    if ((!d750) && u_is_con(s749, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s749, 0);
      d750 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)));
        p_check_46_variables(v_env, v_assignable, f_list_46_firsts_1(v_pairs), &v_ys, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_pairs), f_syntax_46_types_1(v_ys), &v_es, r_s);
        u_value t752 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_zip_2(v_ys, v_es));
        *r_y = t752;
      }
    }
    if ((!d750) && u_is_con(s749, &n_syntax_46_SBlock)) {
      u_value v_locals = u_field(s749, 0);
      u_value v_body = u_field(s749, 1);
      d750 = 1;
      {
        p_check_46_untyped(v_locals, f_check_46_initialized_1(v_body));
        p_check_46_formals(v_locals, &v_ys, r_s);
        p_check_46_statements(f_list_46__43__43__2(f_check_46_bind_1(v_ys), v_env), f_list_46__43__43__2(f_syntax_46_names_1(v_ys), v_assignable), v_body, &v_xs, r_s);
        u_value t753 = u_mk_con(&n_syntax_46_SBlock, 2, v_ys, v_xs);
        *r_y = t753;
      }
    }
    if ((!d750) && u_is_con(s749, &n_syntax_46_SIf)) {
      u_value v_cond = u_field(s749, 0);
      u_value v_left = u_field(s749, 1);
      u_value v_right = u_field(s749, 2);
      d750 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_left, &v_l, r_s);
        p_check_46_statement(v_env, v_assignable, v_right, &v_r, r_s);
        u_value t754 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
        *r_y = t754;
      }
    }
    if ((!d750) && u_is_con(s749, &n_syntax_46_SWhile)) {
      u_value v_cond = u_field(s749, 0);
      u_value v_body = u_field(s749, 1);
      d750 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        u_value t755 = u_mk_con(&n_syntax_46_SWhile, 2, v_c, v_l);
        *r_y = t755;
      }
    }
    if ((!d750) && u_is_con(s749, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s749, 0);
      u_value v_ins = u_field(s749, 1);
      u_value v_outs = u_field(s749, 2);
      u_value v_refs = u_field(s749, 3);
      d750 = 1;
      {
        p_check_46_call_proc(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d750) && u_is_con(s749, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s749, 0);
      u_value v_cases = u_field(s749, 1);
      d750 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_cases, v_t, &v_bs, r_s);
        u_value t756 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e757 = &t756;
        u_value l758 = v_bs;
        while (u_is_con(l758, &n_list_46_Cons)) {
          u_value v_br = u_field(l758, 0);
          if (u_truth(u_mk_bool(1))) {
            e757 = u_append(e757, f_check_46_case_of_1(v_br));
          }
          l758 = u_field(l758, 1);
        }
        p_check_46_coverage(*r_s, v_t, t756);
        u_value t759 = u_mk_con(&n_syntax_46_SMatch, 2, v_c, v_bs);
        *r_y = t759;
      }
    }
    if ((!d750) && u_is_con(s749, &n_syntax_46_SAssert)) {
      u_value v_cond = u_field(s749, 0);
      d750 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        u_value t760 = u_mk_con(&n_syntax_46_SAssert, 1, v_c);
        *r_y = t760;
      }
    }
    if ((!d750) && u_is_con(s749, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s749, 0);
      u_value v_a = u_field(s749, 1);
      d750 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_unify(v_t, v_u, r_s);
        u_value t761 = u_mk_con(&n_syntax_46_SRaise, 2, v_c, v_t);
        *r_y = t761;
      }
    }
    if ((!d750) && u_is_con(s749, &n_syntax_46_STry)) {
      u_value v_body = u_field(s749, 0);
      u_value v_handlers = u_field(s749, 1);
      d750 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_handlers, &v_hs, r_s);
        u_value t762 = u_mk_con(&n_syntax_46_STry, 2, v_l, v_hs);
        *r_y = t762;
      }
    }
    if (!d750) {
      u_no_case(s749);
    }
  }
}

u_value f_check_46_initialized_1(u_value v_xs) {
  u_value s763 = v_xs;
  u_value t764 = NULL;
  if ((!t764) && (u_is_con(s763, &n_list_46_Cons) && (u_is_con(u_field(s763, 0), &n_syntax_46_SAssign) && (u_is_con(u_field(u_field(s763, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s763, 0), 0), 1), &n_list_46_Nil))))) {
    u_value v_x = u_field(u_field(u_field(u_field(s763, 0), 0), 0), 0);
    u_value v_rest = u_field(s763, 1);
    t764 = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), f_check_46_initialized_1(v_rest));
  }
  if (!t764) {
    t764 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t764) {
    u_no_case(s763);
  }
  return t764;
}

void p_check_46_untyped(u_value v_xs, u_value v_inits) {
  {
    u_value s765 = v_xs;
    int d766 = 0;
    if ((!d766) && u_is_con(s765, &n_list_46_Nil)) {
      d766 = 1;
      {
      }
    }
    if ((!d766) && (u_is_con(s765, &n_list_46_Cons) && u_is_con(u_field(s765, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s765, 0), 0);
      u_value v_t = u_field(u_field(s765, 0), 1);
      u_value v_rest = u_field(s765, 1);
      d766 = 1;
      {
        u_value t767 = f_check_46_holes_1(v_t);
        if (u_truth(t767)) {
          t767 = f_prelude_46_not_1(f_list_46_contains_2(v_n, v_inits));
        }
        if (u_truth(t767)) {
          {
            u_raise(u_mk_con(&n_check_46_MissingType, 1, v_n), "check.TypeError");
          }
        }
        p_check_46_untyped(v_rest, v_inits);
      }
    }
    if (!d766) {
      u_no_case(s765);
    }
  }
}

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s768 = v_xs;
    int d769 = 0;
    if ((!d769) && u_is_con(s768, &n_list_46_Nil)) {
      d769 = 1;
      {
        u_value t770 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t770;
      }
    }
    if ((!d769) && u_is_con(s768, &n_list_46_Cons)) {
      u_value v_x = u_field(s768, 0);
      u_value v_more = u_field(s768, 1);
      d769 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_statements(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t771 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t771;
      }
    }
    if (!d769) {
      u_no_case(s768);
    }
  }
}

u_value f_check_46_os_procs_1(u_value v_c) {
  u_value t776;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_772 = U_LIT("os_args");
    static struct u_object l_os_read_773 = U_LIT("os_read");
    static struct u_object l_os_write_774 = U_LIT("os_write");
    static struct u_object l_os_library_775 = U_LIT("os_library");
    t776 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_772), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_773), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_774), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_775), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)))));
  } else {
    t776 = u_mk_con(&n_list_46_Nil, 0);
  }
  return t776;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t777 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e778 = &t777;
  static struct u_object l_Program_779 = U_LIT("Program");
  u_value l780 = f_ctx_46_names_of_2(v_c, &l_Program_779);
  while (u_is_con(l780, &n_list_46_Cons)) {
    u_value v_d = u_field(l780, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e778 = u_append(e778, v_d);
    }
    l780 = u_field(l780, 1);
  }
  u_value s781 = t777;
  u_value t782 = NULL;
  if ((!t782) && u_is_con(s781, &n_list_46_Cons)) {
    u_value v_d = u_field(s781, 0);
    static struct u_object l_read_783 = U_LIT("read");
    static struct u_object l_library_784 = U_LIT("library");
    t782 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_783), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_784), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t782) {
    t782 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t782) {
    u_no_case(s781);
  }
  return t782;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s786 = f_check_46_visible_names_3(*r_s, v_p, u_mk_closure(lam785, 1, *r_s));
    int d787 = 0;
    if ((!d787) && (u_is_con(s786, &n_list_46_Cons) && u_is_con(u_field(s786, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s786, 0);
      d787 = 1;
      {
        u_value t788 = v_q1;
        v_q = t788;
      }
    }
    if ((!d787) && u_is_con(s786, &n_list_46_Nil)) {
      d787 = 1;
      {
        u_value t789 = v_p;
        v_q = t789;
      }
    }
    if (!d787) {
      u_value v_qs = s786;
      d787 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d787) {
      u_no_case(s786);
    }
    u_value s790 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_State_46_ctx_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_State_46_ctx_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_State_46_ctx_1(*r_s)))));
    int d791 = 0;
    if ((!d791) && u_is_con(s790, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s790, 0), 0);
      u_value v_is = u_field(u_field(s790, 0), 1);
      u_value v_os = u_field(u_field(s790, 0), 2);
      u_value v_rs = u_field(u_field(s790, 0), 3);
      d791 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t792 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e793 = &t792;
        u_value l794 = v_is;
        while (u_is_con(l794, &n_list_46_Cons)) {
          u_value v_t = u_field(l794, 0);
          if (u_truth(u_mk_bool(1))) {
            e793 = u_append(e793, f_check_46_subst_2(v_t, v_m));
          }
          l794 = u_field(l794, 1);
        }
        p_check_46_checks(v_env, v_ins, t792, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t795 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e796 = &t795;
        u_value l797 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l797, &n_list_46_Cons)) {
          u_value v_t = u_field(l797, 0);
          if (u_truth(u_mk_bool(1))) {
            e796 = u_append(e796, f_check_46_subst_2(v_t, v_m));
          }
          l797 = u_field(l797, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t795, r_s);
        u_value t798 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t798;
      }
    }
    if ((!d791) && u_is_con(s790, &n_option_46_None)) {
      d791 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d791) {
      u_no_case(s790);
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
    u_value s799 = v_cases;
    int d800 = 0;
    if ((!d800) && u_is_con(s799, &n_list_46_Nil)) {
      d800 = 1;
      {
        u_value t801 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t801;
      }
    }
    if ((!d800) && (u_is_con(s799, &n_list_46_Cons) && u_is_con(u_field(s799, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s799, 0), 0);
      u_value v_g = u_field(u_field(s799, 0), 1);
      u_value v_b = u_field(u_field(s799, 0), 2);
      u_value v_more = u_field(s799, 1);
      d800 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t802 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t802;
      }
    }
    if (!d800) {
      u_no_case(s799);
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
    u_value s803 = v_hs;
    int d804 = 0;
    if ((!d804) && u_is_con(s803, &n_list_46_Nil)) {
      d804 = 1;
      {
        u_value t805 = u_mk_con(&n_list_46_Nil, 0);
        *r_hs1 = t805;
      }
    }
    if ((!d804) && (u_is_con(s803, &n_list_46_Cons) && u_is_con(u_field(s803, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s803, 0), 0);
      u_value v_p = u_field(u_field(s803, 0), 1);
      u_value v_g = u_field(u_field(s803, 0), 2);
      u_value v_b = u_field(u_field(s803, 0), 3);
      u_value v_more = u_field(s803, 1);
      d804 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t806 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t806;
      }
    }
    if (!d804) {
      u_no_case(s803);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t807 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t807)) {
      t807 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t808 = t807;
    if (!u_truth(t808)) {
      t808 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t809 = t808;
    if (!u_truth(t809)) {
      t809 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n))), u_mk_con(&n_list_46_Nil, 0));
    }
    if (u_truth(t809)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t810 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t810)) {
      t810 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t811 = t810;
    if (!u_truth(t811)) {
      t811 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t811)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_812 = U_LIT("Bool");
    static struct u_object l_Int_813 = U_LIT("Int");
    static struct u_object l_Str_814 = U_LIT("Str");
    static struct u_object l_Char_815 = U_LIT("Char");
    static struct u_object l__45__62__816 = U_LIT("->");
    u_value t817 = f_list_46_contains_2(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_812, u_mk_con(&n_list_46_Cons, 2, &l_Int_813, u_mk_con(&n_list_46_Cons, 2, &l_Str_814, u_mk_con(&n_list_46_Cons, 2, &l_Char_815, u_mk_con(&n_list_46_Cons, 2, &l__45__62__816, u_mk_con(&n_list_46_Nil, 0)))))));
    if (!u_truth(t817)) {
      t817 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t818 = t817;
    if (!u_truth(t818)) {
      t818 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t818)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t819 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e820 = &t819;
    u_value l821 = v_ts;
    while (u_is_con(l821, &n_list_46_Cons)) {
      u_value v_t = u_field(l821, 0);
      if (u_truth(u_mk_bool(1))) {
        e820 = u_append(e820, f_check_46_free_1(v_t));
      }
      l821 = u_field(l821, 1);
    }
    u_value t822 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t819)), f_check_46_params_1(v_ps));
    v_undeclared = t822;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, u_mk_con(&n_list_46_Nil, 0)))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
  {
    u_value s823 = v_cs;
    int d824 = 0;
    if ((!d824) && u_is_con(s823, &n_list_46_Nil)) {
      d824 = 1;
      {
      }
    }
    if ((!d824) && (u_is_con(s823, &n_list_46_Cons) && u_is_con(u_field(u_field(s823, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s823, 0), 0);
      u_value v_n = u_field(u_field(u_field(s823, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s823, 0), 1), 1);
      u_value v_rest = u_field(s823, 1);
      d824 = 1;
      {
        u_value t825 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t825;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        u_value t826 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t826;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d824) && u_is_con(s823, &n_list_46_Cons)) {
      u_value v_rest = u_field(s823, 1);
      d824 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d824) {
      u_no_case(s823);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s827 = v_cs;
    int d828 = 0;
    if ((!d828) && u_is_con(s827, &n_list_46_Nil)) {
      d828 = 1;
      {
      }
    }
    if ((!d828) && (u_is_con(s827, &n_list_46_Cons) && u_is_con(u_field(u_field(s827, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s827, 0), 0);
      u_value v_n = u_field(u_field(u_field(s827, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s827, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s827, 0), 1), 2);
      u_value v_rest = u_field(s827, 1);
      d828 = 1;
      {
        u_value t829 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t829;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)));
        u_value t830 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_State_46_ctx_1(*r_s), v_n, v_ps, v_u));
        *r_s = t830;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d828) && u_is_con(s827, &n_list_46_Cons)) {
      u_value v_rest = u_field(s827, 1);
      d828 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d828) {
      u_no_case(s827);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
  {
    u_value s831 = v_cs;
    int d832 = 0;
    if ((!d832) && u_is_con(s831, &n_list_46_Nil)) {
      d832 = 1;
      {
      }
    }
    if ((!d832) && (u_is_con(s831, &n_list_46_Cons) && u_is_con(u_field(u_field(s831, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s831, 0), 0);
      u_value v_n = u_field(u_field(u_field(s831, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s831, 0), 1), 1);
      u_value v_ks = u_field(u_field(u_field(s831, 0), 1), 2);
      u_value v_rest = u_field(s831, 1);
      d832 = 1;
      {
        u_value t833 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t833;
        p_check_46_unique_ids(v_ps);
        u_value t834 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e835 = &t834;
        u_value l836 = v_ks;
        while (u_is_con(l836, &n_list_46_Cons)) {
          u_value v_k = u_field(l836, 0);
          if (u_truth(u_mk_bool(1))) {
            e835 = u_append(e835, f_syntax_46_Constr_46_name_1(v_k));
          }
          l836 = u_field(l836, 1);
        }
        u_value t837 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_State_46_ctx_1(*r_s), v_n, t834));
        *r_s = t837;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d832) && u_is_con(s831, &n_list_46_Cons)) {
      u_value v_rest = u_field(s831, 1);
      d832 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d832) {
      u_no_case(s831);
    }
  }
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s838 = v_ks;
    int d839 = 0;
    if ((!d839) && u_is_con(s838, &n_list_46_Nil)) {
      d839 = 1;
      {
      }
    }
    if ((!d839) && (u_is_con(s838, &n_list_46_Cons) && u_is_con(u_field(s838, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s838, 0), 0);
      u_value v_args = u_field(u_field(s838, 0), 1);
      u_value v_rest = u_field(s838, 1);
      d839 = 1;
      {
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t840 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_State_46_ctx_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t840;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d839) {
      u_no_case(s838);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s841 = v_xss;
  u_value t842 = NULL;
  if ((!t842) && u_is_con(s841, &n_list_46_Nil)) {
    t842 = v_r;
  }
  if ((!t842) && u_is_con(s841, &n_list_46_Cons)) {
    u_value v_xs = u_field(s841, 0);
    u_value v_rest = u_field(s841, 1);
    t842 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t842) {
    u_no_case(s841);
  }
  return t842;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s843 = v_xss;
  u_value t844 = NULL;
  if ((!t844) && u_is_con(s843, &n_list_46_Nil)) {
    t844 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t844) && u_is_con(s843, &n_list_46_Cons)) {
    u_value v_xs = u_field(s843, 0);
    t844 = v_xs;
  }
  if (!t844) {
    u_no_case(s843);
  }
  return t844;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s845 = v_xss;
    int d846 = 0;
    if ((!d846) && u_is_con(s845, &n_list_46_Nil)) {
      d846 = 1;
      {
        u_value t847 = u_mk_con(&n_list_46_Nil, 0);
        *r_yss = t847;
      }
    }
    if ((!d846) && u_is_con(s845, &n_list_46_Cons)) {
      u_value v_xs = u_field(s845, 0);
      u_value v_more = u_field(s845, 1);
      d846 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t848 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t848;
      }
    }
    if (!d846) {
      u_no_case(s845);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s849 = v_cs;
    int d850 = 0;
    if ((!d850) && u_is_con(s849, &n_list_46_Nil)) {
      d850 = 1;
      {
      }
    }
    if ((!d850) && (u_is_con(s849, &n_list_46_Cons) && u_is_con(u_field(u_field(s849, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s849, 0), 0);
      u_value v_n = u_field(u_field(u_field(s849, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s849, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s849, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s849, 0), 1), 3);
      u_value v_rest = u_field(s849, 1);
      d850 = 1;
      {
        u_value t851 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t851;
        p_check_46_overload(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), u_mk_con(&n_list_46_Nil, 0)));
        u_value t852 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t852;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d850) && u_is_con(s849, &n_list_46_Cons)) {
      u_value v_rest = u_field(s849, 1);
      d850 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d850) {
      u_no_case(s849);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s853 = v_cs;
    int d854 = 0;
    if ((!d854) && u_is_con(s853, &n_list_46_Nil)) {
      d854 = 1;
      {
      }
    }
    if ((!d854) && (u_is_con(s853, &n_list_46_Cons) && u_is_con(u_field(u_field(s853, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s853, 0), 0);
      u_value v_n = u_field(u_field(u_field(s853, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s853, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s853, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s853, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s853, 0), 1), 4);
      u_value v_rest = u_field(s853, 1);
      d854 = 1;
      {
        u_value t855 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t855;
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t856 = f_syntax_46_types_1(v_xs);
        v_ts = t856;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t857 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t857;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d854) && u_is_con(s853, &n_list_46_Cons)) {
      u_value v_rest = u_field(s853, 1);
      d854 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d854) {
      u_no_case(s853);
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
    u_value s858 = v_c;
    int d859 = 0;
    if ((!d859) && u_is_con(s858, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s858, 0);
      u_value v_ps = u_field(s858, 1);
      u_value v_bound = u_field(s858, 2);
      u_value v_res = u_field(s858, 3);
      u_value v_body = u_field(s858, 4);
      d859 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t860 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t860;
      }
    }
    if ((!d859) && u_is_con(s858, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s858, 0);
      u_value v_ps = u_field(s858, 1);
      u_value v_ins = u_field(s858, 2);
      u_value v_outs = u_field(s858, 3);
      u_value v_refs = u_field(s858, 4);
      u_value v_body = u_field(s858, 5);
      d859 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t861 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t861;
      }
    }
    if ((!d859) && u_is_con(s858, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s858, 0);
      d859 = 1;
      {
        p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_b, &v_t, r_s);
        u_value t862 = u_mk_con(&n_syntax_46_CEval, 1, v_b);
        *r_c1 = t862;
      }
    }
    if ((!d859) && u_is_con(s858, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s858, 0);
      u_value v_body = u_field(s858, 1);
      d859 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t863 = u_mk_con(&n_syntax_46_CRun, 2, v_n, v_y);
        *r_c1 = t863;
      }
    }
    if ((!d859) && u_is_con(s858, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s858, 0);
      u_value v_body = u_field(s858, 1);
      d859 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t864 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t864;
      }
    }
    if (!d859) {
      d859 = 1;
      {
        u_value t865 = v_c;
        *r_c1 = t865;
      }
    }
    if (!d859) {
      u_no_case(s858);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s866 = v_cs;
    int d867 = 0;
    if ((!d867) && u_is_con(s866, &n_list_46_Nil)) {
      d867 = 1;
      {
        u_value t868 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t868;
      }
    }
    if ((!d867) && u_is_con(s866, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s866, 0), 0);
      u_value v_c = u_field(u_field(s866, 0), 1);
      u_value v_more = u_field(s866, 1);
      d867 = 1;
      {
        u_value t869 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t869;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t870 = f_check_46_zonk_cmd_2(f_check_46_State_46_sub_1(*r_s), v_c1);
        v_c2 = t870;
        u_value s871 = f_check_46_unknown_in_cmd_1(v_c2);
        int d872 = 0;
        if ((!d872) && u_is_con(s871, &n_option_46_Some)) {
          u_value v_t = u_field(s871, 0);
          d872 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d872) && u_is_con(s871, &n_option_46_None)) {
          d872 = 1;
          {
          }
        }
        if (!d872) {
          u_no_case(s871);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t873 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t873;
      }
    }
    if (!d867) {
      u_no_case(s866);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s874 = v_cs;
  u_value t875 = NULL;
  if ((!t875) && u_is_con(s874, &n_list_46_Nil)) {
    t875 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t875) && (u_is_con(s874, &n_list_46_Cons) && u_is_con(u_field(u_field(s874, 0), 1), &n_syntax_46_CRun))) {
    u_value v_n = u_field(u_field(u_field(s874, 0), 1), 0);
    u_value v_more = u_field(s874, 1);
    t875 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t875) && (u_is_con(s874, &n_list_46_Cons) && u_is_con(u_field(u_field(s874, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s874, 0), 1), 0);
    u_value v_more = u_field(s874, 1);
    t875 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t875) && u_is_con(s874, &n_list_46_Cons)) {
    u_value v_more = u_field(s874, 1);
    t875 = f_check_46_block_names_1(v_more);
  }
  if (!t875) {
    u_no_case(s874);
  }
  return t875;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_s = NULL;
    u_value v_ds = NULL;
    u_value v_cs = NULL;
    u_value s876 = v_p;
    int d877 = 0;
    if ((!d877) && u_is_con(s876, &n_syntax_46_Program)) {
      u_value v_cmds = u_field(s876, 0);
      d877 = 1;
      {
        u_value t878 = f_check_46_flatten_2(v_main, v_cmds);
        v_ds = t878;
        u_value t879 = f_check_46_start_2(v_main, f_check_46_visibility_2(v_ds, v_main));
        v_s = t879;
        p_check_46_declare_datas(v_ds, &v_s);
        p_check_46_declare_sorts(v_ds, &v_s);
        p_check_46_declare_constrs(v_ds, &v_s);
        p_check_46_declare_funs(v_ds, &v_s);
        p_check_46_declare_procs(v_ds, &v_s);
        p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
        p_check_46_commands(v_ds, &v_cs, &v_s);
        u_value t880 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
        *r_q = t880;
      }
    }
    if (!d877) {
      u_no_case(s876);
    }
  }
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_881 = U_LIT("main");
    u_value t882 = f_check_46_start_2(&l_main_881, u_mk_con(&n_list_46_Nil, 0));
    v_s = t882;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t883 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(v_s), v_u);
    *r_t = t883;
  }
}

u_value f_check_46_ev_1(u_value v_n) {
  return u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_check_46_pv_1(u_value v_n) {
  return u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_check_46_num_1(u_value v_n) {
  return u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
}

u_value f_check_46_ap_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_syntax_46_EApp, 2, f_check_46_ev_1(v_f), v_args);
}

u_value f_check_46_tl_1(u_value v_t) {
  static struct u_object l_List_884 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_884), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_885 = U_LIT("list");
  static struct u_object l_List_886 = U_LIT("List");
  static struct u_object l_A_887 = U_LIT("A");
  static struct u_object l_Nil_888 = U_LIT("Nil");
  static struct u_object l_Cons_889 = U_LIT("Cons");
  static struct u_object l_A_890 = U_LIT("A");
  static struct u_object l_A_891 = U_LIT("A");
  static struct u_object l_list_892 = U_LIT("list");
  static struct u_object l_len_893 = U_LIT("len");
  static struct u_object l_A_894 = U_LIT("A");
  static struct u_object l_xs_895 = U_LIT("xs");
  static struct u_object l_A_896 = U_LIT("A");
  static struct u_object l_xs_897 = U_LIT("xs");
  static struct u_object l_Nil_898 = U_LIT("Nil");
  static struct u_object l_Cons_899 = U_LIT("Cons");
  static struct u_object l_ys_900 = U_LIT("ys");
  static struct u_object l__43__901 = U_LIT("+");
  static struct u_object l_len_902 = U_LIT("len");
  static struct u_object l_ys_903 = U_LIT("ys");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_885, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_886), u_mk_con(&n_list_46_Cons, 2, &l_A_887, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_888), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_889), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_890), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_891)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_892), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_893), u_mk_con(&n_list_46_Cons, 2, &l_A_894, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_895), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_896))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_897), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_898), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_899), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_900), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__901, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_902, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_903), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s904 = v_t;
  u_value t905 = NULL;
  if ((!t905) && u_is_con(s904, &n_c_46_CtVoid)) {
    static struct u_object l_void_906 = U_LIT("void");
    t905 = &l_void_906;
  }
  if ((!t905) && u_is_con(s904, &n_c_46_CtInt)) {
    static struct u_object l_int_907 = U_LIT("int");
    t905 = &l_int_907;
  }
  if ((!t905) && u_is_con(s904, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s904, 0);
    t905 = v_n;
  }
  if ((!t905) && (u_is_con(s904, &n_c_46_CtPtr) && u_is_con(u_field(s904, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s904, 0), 0);
    static struct u_object l__42__908 = U_LIT("*");
    t905 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__908);
  }
  if ((!t905) && u_is_con(s904, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s904, 0);
    static struct u_object l__32__42__909 = U_LIT(" *");
    t905 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__909);
  }
  if (!t905) {
    u_no_case(s904);
  }
  return t905;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s910 = v_t;
  u_value t911 = NULL;
  if ((!t911) && u_is_con(s910, &n_c_46_CtPtr)) {
    t911 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t911) {
    static struct u_object l__32__912 = U_LIT(" ");
    t911 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__912, v_x));
  }
  if (!t911) {
    u_no_case(s910);
  }
  return t911;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s913 = v_e;
  u_value t914 = NULL;
  if ((!t914) && u_is_con(s913, &n_c_46_CeVar)) {
    u_value v_x = u_field(s913, 0);
    t914 = v_x;
  }
  if ((!t914) && u_is_con(s913, &n_c_46_CeNum)) {
    u_value v_n = u_field(s913, 0);
    t914 = u_str_show(v_n);
  }
  if ((!t914) && u_is_con(s913, &n_c_46_CeStr)) {
    u_value v_s = u_field(s913, 0);
    t914 = u_str_quote(v_s);
  }
  if ((!t914) && u_is_con(s913, &n_c_46_CeCall)) {
    u_value v_f = u_field(s913, 0);
    u_value v_args = u_field(s913, 1);
    static struct u_object l__40__915 = U_LIT("(");
    static struct u_object l__44__32__916 = U_LIT(", ");
    u_value t917 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e918 = &t917;
    u_value l919 = v_args;
    while (u_is_con(l919, &n_list_46_Cons)) {
      u_value v_a = u_field(l919, 0);
      if (u_truth(u_mk_bool(1))) {
        e918 = u_append(e918, f_c_46_c_expr_1(v_a));
      }
      l919 = u_field(l919, 1);
    }
    static struct u_object l__41__920 = U_LIT(")");
    t914 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__915, u_concat(u_str_join(&l__44__32__916, t917), &l__41__920)));
  }
  if ((!t914) && u_is_con(s913, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s913, 0);
    u_value v_a = u_field(s913, 1);
    t914 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t914) && u_is_con(s913, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s913, 0);
    u_value v_a = u_field(s913, 1);
    u_value v_b = u_field(s913, 2);
    static struct u_object l__32__921 = U_LIT(" ");
    static struct u_object l__32__922 = U_LIT(" ");
    t914 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__921, u_concat(v_o, u_concat(&l__32__922, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t914) && u_is_con(s913, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s913, 0);
    u_value v_i = u_field(s913, 1);
    static struct u_object l__91__923 = U_LIT("[");
    static struct u_object l__93__924 = U_LIT("]");
    t914 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__923, u_concat(f_c_46_c_expr_1(v_i), &l__93__924)));
  }
  if ((!t914) && u_is_con(s913, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s913, 0);
    u_value v_f = u_field(s913, 1);
    static struct u_object l__45__62__925 = U_LIT("->");
    t914 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__925, v_f));
  }
  if ((!t914) && u_is_con(s913, &n_c_46_CeCast)) {
    u_value v_t = u_field(s913, 0);
    u_value v_a = u_field(s913, 1);
    static struct u_object l__40__926 = U_LIT("(");
    static struct u_object l__41__927 = U_LIT(")");
    t914 = u_concat(&l__40__926, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__927, f_c_46_c_operand_1(v_a))));
  }
  if ((!t914) && u_is_con(s913, &n_c_46_CeCond)) {
    u_value v_c = u_field(s913, 0);
    u_value v_a = u_field(s913, 1);
    u_value v_b = u_field(s913, 2);
    static struct u_object l__32__63__32__928 = U_LIT(" ? ");
    static struct u_object l__32__58__32__929 = U_LIT(" : ");
    t914 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__928, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__929, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t914) && u_is_con(s913, &n_c_46_CeInit)) {
    u_value v_es = u_field(s913, 0);
    static struct u_object l__123__930 = U_LIT("{");
    static struct u_object l__44__32__931 = U_LIT(", ");
    u_value t932 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e933 = &t932;
    u_value l934 = v_es;
    while (u_is_con(l934, &n_list_46_Cons)) {
      u_value v_a = u_field(l934, 0);
      if (u_truth(u_mk_bool(1))) {
        e933 = u_append(e933, f_c_46_c_expr_1(v_a));
      }
      l934 = u_field(l934, 1);
    }
    static struct u_object l__125__935 = U_LIT("}");
    t914 = u_concat(&l__123__930, u_concat(u_str_join(&l__44__32__931, t932), &l__125__935));
  }
  if (!t914) {
    u_no_case(s913);
  }
  return t914;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s936 = v_e;
  u_value t937 = NULL;
  if ((!t937) && u_is_con(s936, &n_c_46_CeVar)) {
    t937 = f_c_46_c_expr_1(v_e);
  }
  if ((!t937) && u_is_con(s936, &n_c_46_CeNum)) {
    u_value v_n = u_field(s936, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t937 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t937) && u_is_con(s936, &n_c_46_CeStr)) {
    t937 = f_c_46_c_expr_1(v_e);
  }
  if ((!t937) && u_is_con(s936, &n_c_46_CeCall)) {
    t937 = f_c_46_c_expr_1(v_e);
  }
  if ((!t937) && u_is_con(s936, &n_c_46_CeIndex)) {
    t937 = f_c_46_c_expr_1(v_e);
  }
  if ((!t937) && u_is_con(s936, &n_c_46_CeArrow)) {
    t937 = f_c_46_c_expr_1(v_e);
  }
  if (!t937) {
    static struct u_object l__40__938 = U_LIT("(");
    static struct u_object l__41__939 = U_LIT(")");
    t937 = u_concat(&l__40__938, u_concat(f_c_46_c_expr_1(v_e), &l__41__939));
  }
  if (!t937) {
    u_no_case(s936);
  }
  return t937;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s940 = v_s;
  u_value t941 = NULL;
  if ((!t941) && u_is_con(s940, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s940, 0);
    static struct u_object l__59__942 = U_LIT(";");
    t941 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__942)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t941) && (u_is_con(s940, &n_c_46_CsDecl) && u_is_con(u_field(s940, 2), &n_option_46_None))) {
    u_value v_t = u_field(s940, 0);
    u_value v_x = u_field(s940, 1);
    static struct u_object l__59__943 = U_LIT(";");
    t941 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__943)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t941) && (u_is_con(s940, &n_c_46_CsDecl) && u_is_con(u_field(s940, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s940, 0);
    u_value v_x = u_field(s940, 1);
    u_value v_e = u_field(u_field(s940, 2), 0);
    static struct u_object l__32__61__32__944 = U_LIT(" = ");
    static struct u_object l__59__945 = U_LIT(";");
    t941 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__944, u_concat(f_c_46_c_expr_1(v_e), &l__59__945)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t941) && u_is_con(s940, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s940, 0);
    u_value v_e = u_field(s940, 1);
    static struct u_object l__32__61__32__946 = U_LIT(" = ");
    static struct u_object l__59__947 = U_LIT(";");
    t941 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__946, u_concat(f_c_46_c_expr_1(v_e), &l__59__947)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t941) && (u_is_con(s940, &n_c_46_CsIf) && u_is_con(u_field(s940, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s940, 0);
    u_value v_l = u_field(s940, 1);
    static struct u_object l_if_32__40__948 = U_LIT("if (");
    static struct u_object l__41__32__123__949 = U_LIT(") {");
    static struct u_object l__32__32__950 = U_LIT("  ");
    static struct u_object l__125__951 = U_LIT("}");
    t941 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__948, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__949))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__950), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__951), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t941) && u_is_con(s940, &n_c_46_CsIf)) {
    u_value v_c = u_field(s940, 0);
    u_value v_l = u_field(s940, 1);
    u_value v_r = u_field(s940, 2);
    static struct u_object l_if_32__40__952 = U_LIT("if (");
    static struct u_object l__41__32__123__953 = U_LIT(") {");
    static struct u_object l__32__32__954 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__955 = U_LIT("} else {");
    static struct u_object l__32__32__956 = U_LIT("  ");
    static struct u_object l__125__957 = U_LIT("}");
    t941 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__952, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__953))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__954), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__955), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__956), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__957), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t941) && u_is_con(s940, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s940, 0);
    u_value v_b = u_field(s940, 1);
    static struct u_object l_while_32__40__958 = U_LIT("while (");
    static struct u_object l__41__32__123__959 = U_LIT(") {");
    static struct u_object l__32__32__960 = U_LIT("  ");
    static struct u_object l__125__961 = U_LIT("}");
    t941 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__958, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__959))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__960), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__961), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t941) && (u_is_con(s940, &n_c_46_CsReturn) && u_is_con(u_field(s940, 0), &n_option_46_None))) {
    static struct u_object l_return_59__962 = U_LIT("return;");
    t941 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__962), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t941) && (u_is_con(s940, &n_c_46_CsReturn) && u_is_con(u_field(s940, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s940, 0), 0);
    static struct u_object l_return_32__963 = U_LIT("return ");
    static struct u_object l__59__964 = U_LIT(";");
    t941 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__963, u_concat(f_c_46_c_expr_1(v_e), &l__59__964))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t941) && u_is_con(s940, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s940, 0);
    static struct u_object l__123__965 = U_LIT("{");
    static struct u_object l__32__32__966 = U_LIT("  ");
    static struct u_object l__125__967 = U_LIT("}");
    t941 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__965), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__966), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__967), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t941) {
    u_no_case(s940);
  }
  return t941;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t968 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e969 = &t968;
  u_value l970 = v_ss;
  while (u_is_con(l970, &n_list_46_Cons)) {
    u_value v_s = u_field(l970, 0);
    if (u_truth(u_mk_bool(1))) {
      e969 = u_append(e969, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l970 = u_field(l970, 1);
  }
  return f_list_46_concat_1(t968);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s971 = v_ps;
  u_value t972 = NULL;
  if ((!t972) && u_is_con(s971, &n_list_46_Nil)) {
    static struct u_object l_void_973 = U_LIT("void");
    t972 = &l_void_973;
  }
  if (!t972) {
    static struct u_object l__44__32__974 = U_LIT(", ");
    u_value t975 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e976 = &t975;
    u_value l977 = v_ps;
    while (u_is_con(l977, &n_list_46_Cons)) {
      u_value v_p = u_field(l977, 0);
      if (u_truth(u_mk_bool(1))) {
        e976 = u_append(e976, f_c_46_c_param_1(v_p));
      }
      l977 = u_field(l977, 1);
    }
    t972 = u_str_join(&l__44__32__974, t975);
  }
  if (!t972) {
    u_no_case(s971);
  }
  return t972;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s978 = v_p;
  u_value t979 = NULL;
  if (!t979) {
    u_value v_t = u_field(s978, 0);
    u_value v_x = u_field(s978, 1);
    t979 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t979) {
    u_no_case(s978);
  }
  return t979;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s980 = v_d;
  u_value t981 = NULL;
  if ((!t981) && u_is_con(s980, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s980, 0);
    static struct u_object l__35_include_32__982 = U_LIT("#include ");
    t981 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__982, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t981) && u_is_con(s980, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s980, 0);
    u_value v_f = u_field(s980, 1);
    u_value v_ps = u_field(s980, 2);
    static struct u_object l__40__983 = U_LIT("(");
    static struct u_object l__41__59__984 = U_LIT(");");
    t981 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__983, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__984))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t981) && u_is_con(s980, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s980, 0);
    u_value v_f = u_field(s980, 1);
    u_value v_ps = u_field(s980, 2);
    u_value v_b = u_field(s980, 3);
    static struct u_object l__40__985 = U_LIT("(");
    static struct u_object l__41__32__123__986 = U_LIT(") {");
    static struct u_object l__32__32__987 = U_LIT("  ");
    static struct u_object l__125__988 = U_LIT("}");
    t981 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__985, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__986))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__987, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__988, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t981) && (u_is_con(s980, &n_c_46_CdGlobal) && u_is_con(u_field(s980, 2), &n_option_46_None))) {
    u_value v_t = u_field(s980, 0);
    u_value v_x = u_field(s980, 1);
    static struct u_object l__59__989 = U_LIT(";");
    t981 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__989), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t981) && (u_is_con(s980, &n_c_46_CdGlobal) && u_is_con(u_field(s980, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s980, 0);
    u_value v_x = u_field(s980, 1);
    u_value v_e = u_field(u_field(s980, 2), 0);
    static struct u_object l__32__61__32__990 = U_LIT(" = ");
    static struct u_object l__59__991 = U_LIT(";");
    t981 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__990, u_concat(f_c_46_c_expr_1(v_e), &l__59__991))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t981) {
    u_no_case(s980);
  }
  return t981;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s992 = v_f;
  u_value t993 = NULL;
  if ((!t993) && u_is_con(s992, &n_c_46_CFile)) {
    u_value v_ds = u_field(s992, 0);
    static struct u_object l__10__10__994 = U_LIT("\n\n");
    u_value t995 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e996 = &t995;
    u_value l997 = v_ds;
    while (u_is_con(l997, &n_list_46_Cons)) {
      u_value v_d = u_field(l997, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__998 = U_LIT("\n");
        e996 = u_append(e996, u_str_join(&l__10__998, f_c_46_c_decl_1(v_d)));
      }
      l997 = u_field(l997, 1);
    }
    static struct u_object l__10__999 = U_LIT("\n");
    t993 = u_concat(u_str_join(&l__10__10__994, t995), &l__10__999);
  }
  if (!t993) {
    u_no_case(s992);
  }
  return t993;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__1000 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__1000, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__44__59__46__1001 = U_LIT("()[],;.");
  return u_str_contains(&l__40__41__91__93__44__59__46__1001, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t1002 = u_is_letter(v_c);
  if (!u_truth(t1002)) {
    t1002 = u_is_digit(v_c);
  }
  u_value t1003 = t1002;
  if (!u_truth(t1003)) {
    t1003 = u_eq(v_c, u_mk_char(95));
  }
  return t1003;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t1004;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t1004 = u_str_get(v_s, v_i);
  } else {
    t1004 = u_chr(u_mk_int(0));
  }
  return t1004;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s1005 = v_c;
  u_value t1006 = NULL;
  if ((!t1006) && u_equals(s1005, u_mk_char(34))) {
    t1006 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t1006) && u_equals(s1005, u_mk_char(39))) {
    t1006 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t1006) && u_equals(s1005, u_mk_char(92))) {
    t1006 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t1006) && u_equals(s1005, u_mk_char(110))) {
    t1006 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t1006) && u_equals(s1005, u_mk_char(116))) {
    t1006 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t1006) && u_equals(s1005, u_mk_char(114))) {
    t1006 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t1006) {
    t1006 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1006) {
    u_no_case(s1005);
  }
  return t1006;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t1007;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t1007 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t1007 = v_i;
  }
  return t1007;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t1008 = u_mk_int(0);
    u_value t1009 = u_mk_int(1);
    u_value t1010 = u_mk_int(1);
    v_j = t1008;
    *r_line = t1009;
    *r_column = t1010;
    u_value t1015 = u_lt(v_j, v_i);
    while (u_truth(t1015)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t1011 = u_add(*r_line, u_mk_int(1));
            u_value t1012 = u_mk_int(1);
            *r_line = t1011;
            *r_column = t1012;
          }
        } else {
          {
            u_value t1013 = u_add(*r_column, u_mk_int(1));
            *r_column = t1013;
          }
        }
        u_value t1014 = u_add(v_j, u_mk_int(1));
        v_j = t1014;
      }
      t1015 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t1016 = *r_i;
    u_value t1017 = u_add(*r_i, u_mk_int(1));
    u_value t1018 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t1016;
    *r_i = t1017;
    v_parts = t1018;
    u_value t1019 = *r_i;
    v_from = t1019;
    u_value t1030 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t1030)) {
      {
        u_value t1020 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t1020;
        u_value t1021 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t1021)) {
          t1021 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t1021)) {
          {
            static struct u_object l_unterminated_32_lit_1022 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_1022, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s1023 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d1024 = 0;
            if ((!d1024) && u_is_con(s1023, &n_option_46_Some)) {
              u_value v_d = u_field(s1023, 0);
              d1024 = 1;
              {
                u_value t1025 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t1026 = u_add(*r_i, u_mk_int(2));
                v_parts = t1025;
                *r_i = t1026;
                u_value t1027 = *r_i;
                v_from = t1027;
              }
            }
            if ((!d1024) && u_is_con(s1023, &n_option_46_None)) {
              d1024 = 1;
              {
                static struct u_object l_invalid_32_escape_1028 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_1028, *r_i), "lex.LexError");
              }
            }
            if (!d1024) {
              u_no_case(s1023);
            }
          }
        } else {
          {
            u_value t1029 = u_add(*r_i, u_mk_int(1));
            *r_i = t1029;
          }
        }
      }
      t1030 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__1031 = U_LIT("");
    u_value t1032 = u_str_join(&l__1031, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t1033 = u_add(*r_i, u_mk_int(1));
    *r_text = t1032;
    *r_i = t1033;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1034 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t1034;
    u_value t1035 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1036 = v_j;
    *r_t = t1035;
    *r_i = t1036;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1037 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t1037;
    u_value s1038 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d1039 = 0;
    if ((!d1039) && u_is_con(s1038, &n_option_46_Some)) {
      u_value v_n = u_field(s1038, 0);
      d1039 = 1;
      {
        u_value t1040 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t1041 = v_j;
        *r_t = t1040;
        *r_i = t1041;
      }
    }
    if ((!d1039) && u_is_con(s1038, &n_option_46_None)) {
      d1039 = 1;
      {
        static struct u_object l_invalid_32_number_1042 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_1042, *r_i), "lex.LexError");
      }
    }
    if (!d1039) {
      u_no_case(s1038);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1043 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t1043;
    u_value t1044 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1045 = v_j;
    *r_t = t1044;
    *r_i = t1045;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t1046 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t1046;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t1047 = *r_i;
    v_start = t1047;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_1048 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_1048, v_start), "lex.LexError");
      }
    }
    u_value t1049 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t1049;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t1050 = u_str_get(v_s, *r_i);
    v_c = t1050;
    u_value s1051 = v_c;
    int d1052 = 0;
    if (!d1052) {
      u_value t1053 = u_is_letter(v_c);
      if (!u_truth(t1053)) {
        t1053 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t1053)) {
        d1052 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d1052) {
      if (u_truth(u_is_digit(v_c))) {
        d1052 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d1052) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d1052 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d1052) && u_equals(s1051, u_mk_char(34))) {
      d1052 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d1052) && u_equals(s1051, u_mk_char(39))) {
      d1052 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d1052) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d1052 = 1;
        {
          u_value t1054 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t1055 = u_add(*r_i, u_mk_int(1));
          *r_t = t1054;
          *r_i = t1055;
        }
      }
    }
    if (!d1052) {
      d1052 = 1;
      {
        static struct u_object l_unexpected_32_chara_1056 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_1056, *r_i), "lex.LexError");
      }
    }
    if (!d1052) {
      u_no_case(s1051);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t1057 = u_mk_int(0);
    v_i = t1057;
    u_value t1058 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1058;
    u_value t1065 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t1065)) {
      {
        u_value t1059 = v_i;
        v_start = t1059;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t1060 = u_add(v_i, u_mk_int(1));
            v_i = t1060;
          }
        } else {
          u_value t1061 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t1061)) {
            t1061 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t1061)) {
            {
              u_value t1063 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam1062, 0));
              v_i = t1063;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t1064 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t1064;
            }
          }
        }
      }
      t1065 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t1066 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t1066;
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
  cache = u_mk_con(&n_set_46_SLeaf, 0);
  return cache;
}

u_value f_set_46_member_2(u_value v_x, u_value v_s) {
  u_value s1067 = v_s;
  u_value t1068 = NULL;
  if ((!t1068) && u_is_con(s1067, &n_set_46_SLeaf)) {
    t1068 = u_mk_bool(0);
  }
  if ((!t1068) && u_is_con(s1067, &n_set_46_SNode)) {
    u_value v_l = u_field(s1067, 0);
    u_value v_y = u_field(s1067, 1);
    u_value v_r = u_field(s1067, 2);
    u_value t1072 = u_compare(v_x, v_y);
    u_value t1071;
    {
      u_value v_c = t1072;
      u_value t1070;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t1070 = f_set_46_member_2(v_x, v_l);
      } else {
        u_value t1069;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t1069 = f_set_46_member_2(v_x, v_r);
        } else {
          t1069 = u_mk_bool(1);
        }
        t1070 = t1069;
      }
      t1071 = t1070;
    }
    t1068 = t1071;
  }
  if (!t1068) {
    u_no_case(s1067);
  }
  return t1068;
}

u_value f_set_46_insert_2(u_value v_x, u_value v_s) {
  u_value s1073 = v_s;
  u_value t1074 = NULL;
  if ((!t1074) && u_is_con(s1073, &n_set_46_SLeaf)) {
    t1074 = u_mk_con(&n_set_46_SNode, 4, u_mk_con(&n_set_46_SLeaf, 0), v_x, u_mk_con(&n_set_46_SLeaf, 0), u_mk_int(1));
  }
  if ((!t1074) && u_is_con(s1073, &n_set_46_SNode)) {
    u_value v_l = u_field(s1073, 0);
    u_value v_y = u_field(s1073, 1);
    u_value v_r = u_field(s1073, 2);
    u_value t1078 = u_compare(v_x, v_y);
    u_value t1077;
    {
      u_value v_c = t1078;
      u_value t1076;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t1076 = f_set_46_rebalance_3(f_set_46_insert_2(v_x, v_l), v_y, v_r);
      } else {
        u_value t1075;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t1075 = f_set_46_rebalance_3(v_l, v_y, f_set_46_insert_2(v_x, v_r));
        } else {
          t1075 = v_s;
        }
        t1076 = t1075;
      }
      t1077 = t1076;
    }
    t1074 = t1077;
  }
  if (!t1074) {
    u_no_case(s1073);
  }
  return t1074;
}

u_value f_set_46_from_list_1(u_value v_xs) {
  return f_set_46_insert_all_2(v_xs, u_mk_con(&n_set_46_SLeaf, 0));
}

u_value f_set_46_insert_all_2(u_value v_xs, u_value v_s) {
  u_value s1079 = v_xs;
  u_value t1080 = NULL;
  if ((!t1080) && u_is_con(s1079, &n_list_46_Nil)) {
    t1080 = v_s;
  }
  if ((!t1080) && u_is_con(s1079, &n_list_46_Cons)) {
    u_value v_x = u_field(s1079, 0);
    u_value v_rest = u_field(s1079, 1);
    t1080 = f_set_46_insert_all_2(v_rest, f_set_46_insert_2(v_x, v_s));
  }
  if (!t1080) {
    u_no_case(s1079);
  }
  return t1080;
}

u_value f_set_46_elements_1(u_value v_s) {
  return f_set_46_elements_onto_2(v_s, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_set_46_elements_onto_2(u_value v_s, u_value v_rest) {
  u_value s1081 = v_s;
  u_value t1082 = NULL;
  if ((!t1082) && u_is_con(s1081, &n_set_46_SLeaf)) {
    t1082 = v_rest;
  }
  if ((!t1082) && u_is_con(s1081, &n_set_46_SNode)) {
    u_value v_l = u_field(s1081, 0);
    u_value v_x = u_field(s1081, 1);
    u_value v_r = u_field(s1081, 2);
    t1082 = f_set_46_elements_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, v_x, f_set_46_elements_onto_2(v_r, v_rest)));
  }
  if (!t1082) {
    u_no_case(s1081);
  }
  return t1082;
}

u_value f_set_46_count_1(u_value v_s) {
  u_value s1083 = v_s;
  u_value t1084 = NULL;
  if ((!t1084) && u_is_con(s1083, &n_set_46_SLeaf)) {
    t1084 = u_mk_int(0);
  }
  if ((!t1084) && u_is_con(s1083, &n_set_46_SNode)) {
    u_value v_l = u_field(s1083, 0);
    u_value v_r = u_field(s1083, 2);
    t1084 = u_add(u_add(f_set_46_count_1(v_l), u_mk_int(1)), f_set_46_count_1(v_r));
  }
  if (!t1084) {
    u_no_case(s1083);
  }
  return t1084;
}

u_value f_set_46_depth_1(u_value v_s) {
  u_value s1085 = v_s;
  u_value t1086 = NULL;
  if ((!t1086) && u_is_con(s1085, &n_set_46_SLeaf)) {
    t1086 = u_mk_int(0);
  }
  if ((!t1086) && u_is_con(s1085, &n_set_46_SNode)) {
    u_value v_d = u_field(s1085, 3);
    t1086 = v_d;
  }
  if (!t1086) {
    u_no_case(s1085);
  }
  return t1086;
}

u_value f_set_46_snode_depth_2(u_value v_l, u_value v_r) {
  u_value t1087;
  if (u_truth(u_lt(f_set_46_depth_1(v_l), f_set_46_depth_1(v_r)))) {
    t1087 = f_set_46_depth_1(v_r);
  } else {
    t1087 = f_set_46_depth_1(v_l);
  }
  return u_add(u_mk_int(1), t1087);
}

u_value f_set_46_snode_3(u_value v_l, u_value v_x, u_value v_r) {
  return u_mk_con(&n_set_46_SNode, 4, v_l, v_x, v_r, f_set_46_snode_depth_2(v_l, v_r));
}

u_value f_set_46_turn_left_1(u_value v_s) {
  u_value s1088 = v_s;
  u_value t1089 = NULL;
  if ((!t1089) && (u_is_con(s1088, &n_set_46_SNode) && u_is_con(u_field(s1088, 2), &n_set_46_SNode))) {
    u_value v_a = u_field(s1088, 0);
    u_value v_x = u_field(s1088, 1);
    u_value v_b = u_field(u_field(s1088, 2), 0);
    u_value v_y = u_field(u_field(s1088, 2), 1);
    u_value v_c = u_field(u_field(s1088, 2), 2);
    t1089 = f_set_46_snode_3(f_set_46_snode_3(v_a, v_x, v_b), v_y, v_c);
  }
  if (!t1089) {
    t1089 = v_s;
  }
  if (!t1089) {
    u_no_case(s1088);
  }
  return t1089;
}

u_value f_set_46_turn_right_1(u_value v_s) {
  u_value s1090 = v_s;
  u_value t1091 = NULL;
  if ((!t1091) && (u_is_con(s1090, &n_set_46_SNode) && u_is_con(u_field(s1090, 0), &n_set_46_SNode))) {
    u_value v_a = u_field(u_field(s1090, 0), 0);
    u_value v_x = u_field(u_field(s1090, 0), 1);
    u_value v_b = u_field(u_field(s1090, 0), 2);
    u_value v_y = u_field(s1090, 1);
    u_value v_c = u_field(s1090, 2);
    t1091 = f_set_46_snode_3(v_a, v_x, f_set_46_snode_3(v_b, v_y, v_c));
  }
  if (!t1091) {
    t1091 = v_s;
  }
  if (!t1091) {
    u_no_case(s1090);
  }
  return t1091;
}

u_value f_set_46_smaller_1(u_value v_s) {
  u_value s1092 = v_s;
  u_value t1093 = NULL;
  if ((!t1093) && u_is_con(s1092, &n_set_46_SNode)) {
    u_value v_l = u_field(s1092, 0);
    t1093 = v_l;
  }
  if (!t1093) {
    t1093 = v_s;
  }
  if (!t1093) {
    u_no_case(s1092);
  }
  return t1093;
}

u_value f_set_46_greater_1(u_value v_s) {
  u_value s1094 = v_s;
  u_value t1095 = NULL;
  if ((!t1095) && u_is_con(s1094, &n_set_46_SNode)) {
    u_value v_r = u_field(s1094, 2);
    t1095 = v_r;
  }
  if (!t1095) {
    t1095 = v_s;
  }
  if (!t1095) {
    u_no_case(s1094);
  }
  return t1095;
}

u_value f_set_46_rebalance_3(u_value v_l, u_value v_x, u_value v_r) {
  u_value t1099;
  if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1))))) {
    u_value t1096;
    if (u_truth(u_lt(f_set_46_depth_1(f_set_46_smaller_1(v_l)), f_set_46_depth_1(f_set_46_greater_1(v_l))))) {
      t1096 = f_set_46_turn_left_1(v_l);
    } else {
      t1096 = v_l;
    }
    t1099 = f_set_46_turn_right_1(f_set_46_snode_3(t1096, v_x, v_r));
  } else {
    u_value t1098;
    if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1))))) {
      u_value t1097;
      if (u_truth(u_lt(f_set_46_depth_1(f_set_46_greater_1(v_r)), f_set_46_depth_1(f_set_46_smaller_1(v_r))))) {
        t1097 = f_set_46_turn_right_1(v_r);
      } else {
        t1097 = v_r;
      }
      t1098 = f_set_46_turn_left_1(f_set_46_snode_3(v_l, v_x, t1097));
    } else {
      t1098 = f_set_46_snode_3(v_l, v_x, v_r);
    }
    t1099 = t1098;
  }
  return t1099;
}

u_value f_set_46_well_formed_1(u_value v_s) {
  u_value s1100 = v_s;
  u_value t1101 = NULL;
  if ((!t1101) && u_is_con(s1100, &n_set_46_SLeaf)) {
    t1101 = u_mk_bool(1);
  }
  if ((!t1101) && u_is_con(s1100, &n_set_46_SNode)) {
    u_value v_l = u_field(s1100, 0);
    u_value v_r = u_field(s1100, 2);
    u_value v_d = u_field(s1100, 3);
    u_value t1102 = f_set_46_well_formed_1(v_l);
    if (u_truth(t1102)) {
      t1102 = f_set_46_well_formed_1(v_r);
    }
    u_value t1103 = t1102;
    if (u_truth(t1103)) {
      t1103 = u_eq(v_d, f_set_46_snode_depth_2(v_l, v_r));
    }
    u_value t1104 = t1103;
    if (u_truth(t1104)) {
      t1104 = f_prelude_46__60__61__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1)));
    }
    u_value t1105 = t1104;
    if (u_truth(t1105)) {
      t1105 = f_prelude_46__60__61__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1)));
    }
    t1101 = t1105;
  }
  if (!t1101) {
    u_no_case(s1100);
  }
  return t1101;
}

u_value f_set_46_upto_1(u_value v_n) {
  u_value t1106;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t1106 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1106 = f_list_46__43__43__2(f_set_46_upto_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t1106;
}

u_value f_parse_46_Notations_46_cmds_1(u_value v_x) {
  u_value s1107 = v_x;
  u_value t1108 = NULL;
  if ((!t1108) && u_is_con(s1107, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1107, 0);
    t1108 = v_y;
  }
  if (!t1108) {
    u_no_case(s1107);
  }
  return t1108;
}

u_value f_parse_46_Notations_46_ops_1(u_value v_x) {
  u_value s1109 = v_x;
  u_value t1110 = NULL;
  if ((!t1110) && u_is_con(s1109, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1109, 1);
    t1110 = v_y;
  }
  if (!t1110) {
    u_no_case(s1109);
  }
  return t1110;
}

u_value f_parse_46_Notations_46_cmds_58__61__2(u_value v_x, u_value v_y) {
  u_value s1111 = v_x;
  u_value t1112 = NULL;
  if ((!t1112) && u_is_con(s1111, &n_parse_46_Notations)) {
    u_value v_a1 = u_field(s1111, 1);
    t1112 = u_mk_con(&n_parse_46_Notations, 2, v_y, v_a1);
  }
  if (!t1112) {
    u_no_case(s1111);
  }
  return t1112;
}

u_value f_parse_46_Notations_46_ops_58__61__2(u_value v_x, u_value v_y) {
  u_value s1113 = v_x;
  u_value t1114 = NULL;
  if ((!t1114) && u_is_con(s1113, &n_parse_46_Notations)) {
    u_value v_a0 = u_field(s1113, 0);
    t1114 = u_mk_con(&n_parse_46_Notations, 2, v_a0, v_y);
  }
  if (!t1114) {
    u_no_case(s1113);
  }
  return t1114;
}

u_value f_parse_46_Scope_46_params_1(u_value v_x) {
  u_value s1115 = v_x;
  u_value t1116 = NULL;
  if ((!t1116) && u_is_con(s1115, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1115, 0);
    t1116 = v_y;
  }
  if (!t1116) {
    u_no_case(s1115);
  }
  return t1116;
}

u_value f_parse_46_Scope_46_notations_1(u_value v_x) {
  u_value s1117 = v_x;
  u_value t1118 = NULL;
  if ((!t1118) && u_is_con(s1117, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1117, 1);
    t1118 = v_y;
  }
  if (!t1118) {
    u_no_case(s1117);
  }
  return t1118;
}

u_value f_parse_46_Scope_46_params_58__61__2(u_value v_x, u_value v_y) {
  u_value s1119 = v_x;
  u_value t1120 = NULL;
  if ((!t1120) && u_is_con(s1119, &n_parse_46_Scope)) {
    u_value v_a1 = u_field(s1119, 1);
    t1120 = u_mk_con(&n_parse_46_Scope, 2, v_y, v_a1);
  }
  if (!t1120) {
    u_no_case(s1119);
  }
  return t1120;
}

u_value f_parse_46_Scope_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1121 = v_x;
  u_value t1122 = NULL;
  if ((!t1122) && u_is_con(s1121, &n_parse_46_Scope)) {
    u_value v_a0 = u_field(s1121, 0);
    t1122 = u_mk_con(&n_parse_46_Scope, 2, v_a0, v_y);
  }
  if (!t1122) {
    u_no_case(s1121);
  }
  return t1122;
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
  static struct u_object l___1123 = U_LIT("_");
  static struct u_object l_assert_1124 = U_LIT("assert");
  static struct u_object l_assign_1125 = U_LIT("assign");
  static struct u_object l_call_1126 = U_LIT("call");
  static struct u_object l_case_1127 = U_LIT("case");
  static struct u_object l_catch_1128 = U_LIT("catch");
  static struct u_object l_data_1129 = U_LIT("data");
  static struct u_object l_else_1130 = U_LIT("else");
  static struct u_object l_end_1131 = U_LIT("end");
  static struct u_object l_eval_1132 = U_LIT("eval");
  static struct u_object l_false_1133 = U_LIT("false");
  static struct u_object l_for_1134 = U_LIT("for");
  static struct u_object l_function_1135 = U_LIT("function");
  static struct u_object l_if_1136 = U_LIT("if");
  static struct u_object l_import_1137 = U_LIT("import");
  static struct u_object l_in_1138 = U_LIT("in");
  static struct u_object l_infix_1139 = U_LIT("infix");
  static struct u_object l_lambda_1140 = U_LIT("lambda");
  static struct u_object l_let_1141 = U_LIT("let");
  static struct u_object l_local_1142 = U_LIT("local");
  static struct u_object l_match_1143 = U_LIT("match");
  static struct u_object l_out_1144 = U_LIT("out");
  static struct u_object l_postfix_1145 = U_LIT("postfix");
  static struct u_object l_prefix_1146 = U_LIT("prefix");
  static struct u_object l_procedure_1147 = U_LIT("procedure");
  static struct u_object l_raise_1148 = U_LIT("raise");
  static struct u_object l_ref_1149 = U_LIT("ref");
  static struct u_object l_run_1150 = U_LIT("run");
  static struct u_object l_skip_1151 = U_LIT("skip");
  static struct u_object l_test_1152 = U_LIT("test");
  static struct u_object l_then_1153 = U_LIT("then");
  static struct u_object l_true_1154 = U_LIT("true");
  static struct u_object l_type_1155 = U_LIT("type");
  static struct u_object l_try_1156 = U_LIT("try");
  static struct u_object l_while_1157 = U_LIT("while");
  static struct u_object l_with_1158 = U_LIT("with");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, &l___1123, u_mk_con(&n_list_46_Cons, 2, &l_assert_1124, u_mk_con(&n_list_46_Cons, 2, &l_assign_1125, u_mk_con(&n_list_46_Cons, 2, &l_call_1126, u_mk_con(&n_list_46_Cons, 2, &l_case_1127, u_mk_con(&n_list_46_Cons, 2, &l_catch_1128, u_mk_con(&n_list_46_Cons, 2, &l_data_1129, u_mk_con(&n_list_46_Cons, 2, &l_else_1130, u_mk_con(&n_list_46_Cons, 2, &l_end_1131, u_mk_con(&n_list_46_Cons, 2, &l_eval_1132, u_mk_con(&n_list_46_Cons, 2, &l_false_1133, u_mk_con(&n_list_46_Cons, 2, &l_for_1134, u_mk_con(&n_list_46_Cons, 2, &l_function_1135, u_mk_con(&n_list_46_Cons, 2, &l_if_1136, u_mk_con(&n_list_46_Cons, 2, &l_import_1137, u_mk_con(&n_list_46_Cons, 2, &l_in_1138, u_mk_con(&n_list_46_Cons, 2, &l_infix_1139, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1140, u_mk_con(&n_list_46_Cons, 2, &l_let_1141, u_mk_con(&n_list_46_Cons, 2, &l_local_1142, u_mk_con(&n_list_46_Cons, 2, &l_match_1143, u_mk_con(&n_list_46_Cons, 2, &l_out_1144, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1145, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1146, u_mk_con(&n_list_46_Cons, 2, &l_procedure_1147, u_mk_con(&n_list_46_Cons, 2, &l_raise_1148, u_mk_con(&n_list_46_Cons, 2, &l_ref_1149, u_mk_con(&n_list_46_Cons, 2, &l_run_1150, u_mk_con(&n_list_46_Cons, 2, &l_skip_1151, u_mk_con(&n_list_46_Cons, 2, &l_test_1152, u_mk_con(&n_list_46_Cons, 2, &l_then_1153, u_mk_con(&n_list_46_Cons, 2, &l_true_1154, u_mk_con(&n_list_46_Cons, 2, &l_type_1155, u_mk_con(&n_list_46_Cons, 2, &l_try_1156, u_mk_con(&n_list_46_Cons, 2, &l_while_1157, u_mk_con(&n_list_46_Cons, 2, &l_with_1158, u_mk_con(&n_list_46_Nil, 0)))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_list_46_contains_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1159 = U_LIT("=");
  static struct u_object l__61__62__1160 = U_LIT("=>");
  static struct u_object l__58__61__1161 = U_LIT(":=");
  static struct u_object l__58__1162 = U_LIT(":");
  static struct u_object l__124__1163 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1159, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1160, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1161, u_mk_con(&n_list_46_Cons, 2, &l__58__1162, u_mk_con(&n_list_46_Cons, 2, &l__124__1163, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1164 = v_ns;
  u_value t1165 = NULL;
  if ((!t1165) && u_is_con(s1164, &n_list_46_Nil)) {
    t1165 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1165) && (u_is_con(s1164, &n_list_46_Cons) && (u_is_con(u_field(s1164, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1164, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1164, 0), 1);
    u_value v_ops = u_field(u_field(s1164, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1165 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1165) && u_is_con(s1164, &n_list_46_Cons)) {
    u_value v_more = u_field(s1164, 1);
    t1165 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1165) {
    u_no_case(s1164);
  }
  return t1165;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1166 = v_ns;
  u_value t1167 = NULL;
  if ((!t1167) && u_is_con(s1166, &n_list_46_Nil)) {
    t1167 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1167) && (u_is_con(s1166, &n_list_46_Cons) && (u_is_con(u_field(s1166, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1166, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1166, 0), 1);
    u_value v_ops = u_field(u_field(s1166, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1167 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1167) && u_is_con(s1166, &n_list_46_Cons)) {
    u_value v_more = u_field(s1166, 1);
    t1167 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1167) {
    u_no_case(s1166);
  }
  return t1167;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1168 = v_ns;
  u_value t1169 = NULL;
  if ((!t1169) && u_is_con(s1168, &n_list_46_Nil)) {
    t1169 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1169) && (u_is_con(s1168, &n_list_46_Cons) && u_is_con(u_field(s1168, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1168, 0), 0);
    u_value v_p = u_field(u_field(s1168, 0), 1);
    u_value v_ops = u_field(u_field(s1168, 0), 2);
    u_value t1170 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1170)) {
      t1170 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1171 = t1170;
    if (u_truth(t1171)) {
      t1171 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1171)) {
      t1169 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1169) && u_is_con(s1168, &n_list_46_Cons)) {
    u_value v_more = u_field(s1168, 1);
    t1169 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1169) {
    u_no_case(s1168);
  }
  return t1169;
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
  cache = u_mk_con(&n_parse_46_Notations, 2, u_mk_con(&n_list_46_Nil, 0), f_set_46_empty_set_0());
  return cache;
}

u_value f_parse_46_with_notations_2(u_value v_ns, u_value v_cs) {
  return u_mk_con(&n_parse_46_Notations, 2, f_list_46__43__43__2(f_parse_46_Notations_46_cmds_1(v_ns), v_cs), f_set_46_insert_all_2(f_parse_46_operators_of_1(v_cs), f_parse_46_Notations_46_ops_1(v_ns)));
}

u_value f_parse_46_operators_of_1(u_value v_cs) {
  u_value s1172 = v_cs;
  u_value t1173 = NULL;
  if ((!t1173) && u_is_con(s1172, &n_list_46_Nil)) {
    t1173 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1173) && (u_is_con(s1172, &n_list_46_Cons) && u_is_con(u_field(s1172, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1172, 0), 2);
    u_value v_more = u_field(s1172, 1);
    t1173 = f_list_46__43__43__2(v_ops, f_parse_46_operators_of_1(v_more));
  }
  if ((!t1173) && u_is_con(s1172, &n_list_46_Cons)) {
    u_value v_more = u_field(s1172, 1);
    t1173 = f_parse_46_operators_of_1(v_more);
  }
  if (!t1173) {
    u_no_case(s1172);
  }
  return t1173;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1174 = v_ts;
  u_value t1175 = NULL;
  if ((!t1175) && u_is_con(s1174, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1174, 0), 0);
    t1175 = v_t;
  }
  if (!t1175) {
    t1175 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1175) {
    u_no_case(s1174);
  }
  return t1175;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1176 = v_ts;
  u_value t1177 = NULL;
  if ((!t1177) && (u_is_con(s1176, &n_list_46_Cons) && u_is_con(u_field(s1176, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1176, 1), 0), 0);
    t1177 = v_t;
  }
  if (!t1177) {
    t1177 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1177) {
    u_no_case(s1176);
  }
  return t1177;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1178 = v_ts;
  u_value t1179 = NULL;
  if ((!t1179) && u_is_con(s1178, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1178, 0), 1);
    t1179 = v_i;
  }
  if (!t1179) {
    t1179 = u_mk_int(0);
  }
  if (!t1179) {
    u_no_case(s1178);
  }
  return t1179;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1180 = v_t;
  u_value t1181 = NULL;
  if ((!t1181) && u_is_con(s1180, &n_lex_46_TName)) {
    u_value v_n = u_field(s1180, 0);
    t1181 = v_n;
  }
  if ((!t1181) && u_is_con(s1180, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1180, 0);
    t1181 = v_s;
  }
  if ((!t1181) && u_is_con(s1180, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1180, 0);
    t1181 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1181) && u_is_con(s1180, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1180, 0);
    t1181 = u_str_show(v_n);
  }
  if ((!t1181) && u_is_con(s1180, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1180, 0);
    t1181 = u_str_quote(v_s);
  }
  if ((!t1181) && u_is_con(s1180, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1180, 0);
    static struct u_object l__39__1182 = U_LIT("'");
    static struct u_object l__39__1183 = U_LIT("'");
    t1181 = u_concat(&l__39__1182, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1183));
  }
  if ((!t1181) && u_is_con(s1180, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1184 = U_LIT("end of input");
    t1181 = &l_end_32_of_32_input_1184;
  }
  if (!t1181) {
    u_no_case(s1180);
  }
  return t1181;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1185 = f_parse_46_head_token_1(v_ts);
  u_value t1186 = NULL;
  if ((!t1186) && u_is_con(s1185, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1185, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1186 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1186) && u_is_con(s1185, &n_lex_46_TName)) {
    u_value v_n = u_field(s1185, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1186 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1186) {
    t1186 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1186) {
    u_no_case(s1185);
  }
  return t1186;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1187 = *r_ts;
    int d1188 = 0;
    if ((!d1188) && u_is_con(s1187, &n_list_46_Cons)) {
      u_value v_more = u_field(s1187, 1);
      d1188 = 1;
      {
        u_value t1189 = v_more;
        *r_ts = t1189;
      }
    }
    if (!d1188) {
      d1188 = 1;
      {
      }
    }
    if (!d1188) {
      u_no_case(s1187);
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
        static struct u_object l__32_expected_32_but_32__1190 = U_LIT(" expected but ");
        static struct u_object l__32_found_1191 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1190, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1191))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1192 = f_parse_46_head_token_1(*r_ts);
    int d1193 = 0;
    if ((!d1193) && u_is_con(s1192, &n_lex_46_TName)) {
      u_value v_m = u_field(s1192, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1193 = 1;
        {
          u_value t1194 = v_m;
          *r_n = t1194;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1193) {
      d1193 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1195 = U_LIT("name expected but ");
        static struct u_object l__32_found_1196 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1195, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1196)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1193) {
      u_no_case(s1192);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1197 = f_parse_46_head_token_1(*r_ts);
    int d1198 = 0;
    if ((!d1198) && u_is_con(s1197, &n_lex_46_TName)) {
      u_value v_m = u_field(s1197, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_m))) {
        d1198 = 1;
        {
          static struct u_object l_operator_32__1199 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1200 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1199, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1200)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1198) {
      d1198 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1198) {
      u_no_case(s1197);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1201 = f_parse_46_head_token_1(*r_ts);
    int d1202 = 0;
    if ((!d1202) && u_is_con(s1201, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1201, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1202 = 1;
        {
          u_value t1203 = v_s;
          *r_n = t1203;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1202) {
      d1202 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1202) {
      u_no_case(s1201);
    }
  }
}

u_value f_parse_46_Ops_46_notations_1(u_value v_x) {
  u_value s1204 = v_x;
  u_value t1205 = NULL;
  if ((!t1205) && u_is_con(s1204, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1204, 0);
    t1205 = v_y;
  }
  if (!t1205) {
    u_no_case(s1204);
  }
  return t1205;
}

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x) {
  u_value s1206 = v_x;
  u_value t1207 = NULL;
  if ((!t1207) && u_is_con(s1206, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1206, 1);
    t1207 = v_y;
  }
  if (!t1207) {
    u_no_case(s1206);
  }
  return t1207;
}

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x) {
  u_value s1208 = v_x;
  u_value t1209 = NULL;
  if ((!t1209) && u_is_con(s1208, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1208, 2);
    t1209 = v_y;
  }
  if (!t1209) {
    u_no_case(s1208);
  }
  return t1209;
}

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x) {
  u_value s1210 = v_x;
  u_value t1211 = NULL;
  if ((!t1211) && u_is_con(s1210, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1210, 3);
    t1211 = v_y;
  }
  if (!t1211) {
    u_no_case(s1210);
  }
  return t1211;
}

u_value f_parse_46_Ops_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1212 = v_x;
  u_value t1213 = NULL;
  if ((!t1213) && u_is_con(s1212, &n_parse_46_Ops)) {
    u_value v_a1 = u_field(s1212, 1);
    u_value v_a2 = u_field(s1212, 2);
    u_value v_a3 = u_field(s1212, 3);
    t1213 = u_mk_con(&n_parse_46_Ops, 4, v_y, v_a1, v_a2, v_a3);
  }
  if (!t1213) {
    u_no_case(s1212);
  }
  return t1213;
}

u_value f_parse_46_Ops_46_prefix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1214 = v_x;
  u_value t1215 = NULL;
  if ((!t1215) && u_is_con(s1214, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1214, 0);
    u_value v_a2 = u_field(s1214, 2);
    u_value v_a3 = u_field(s1214, 3);
    t1215 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_y, v_a2, v_a3);
  }
  if (!t1215) {
    u_no_case(s1214);
  }
  return t1215;
}

u_value f_parse_46_Ops_46_postfix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1216 = v_x;
  u_value t1217 = NULL;
  if ((!t1217) && u_is_con(s1216, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1216, 0);
    u_value v_a1 = u_field(s1216, 1);
    u_value v_a3 = u_field(s1216, 3);
    t1217 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_y, v_a3);
  }
  if (!t1217) {
    u_no_case(s1216);
  }
  return t1217;
}

u_value f_parse_46_Ops_46_infix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1218 = v_x;
  u_value t1219 = NULL;
  if ((!t1219) && u_is_con(s1218, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1218, 0);
    u_value v_a1 = u_field(s1218, 1);
    u_value v_a2 = u_field(s1218, 2);
    t1219 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_a2, v_y);
  }
  if (!t1219) {
    u_no_case(s1218);
  }
  return t1219;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1220 = v_rest;
    int d1221 = 0;
    if ((!d1221) && u_is_con(s1220, &n_list_46_Nil)) {
      d1221 = 1;
      {
      }
    }
    if ((!d1221) && (u_is_con(s1220, &n_list_46_Cons) && u_is_con(u_field(s1220, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1220, 0), 0);
      u_value v_i = u_field(u_field(s1220, 0), 1);
      d1221 = 1;
      {
        static struct u_object l_unexpected_32_opera_1222 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1222, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1221) {
      d1221 = 1;
      {
        static struct u_object l_missing_32_operator_1223 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1223, v_at), "parse.ParseError");
      }
    }
    if (!d1221) {
      u_no_case(s1220);
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
    u_value s1224 = v_items;
    int d1225 = 0;
    if ((!d1225) && (u_is_con(s1224, &n_list_46_Cons) && u_is_con(u_field(s1224, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1224, 0), 0);
      u_value v_more = u_field(s1224, 1);
      d1225 = 1;
      {
        u_value t1226 = v_x;
        u_value t1227 = v_more;
        *r_a = t1226;
        *r_rest = t1227;
      }
    }
    if ((!d1225) && (u_is_con(s1224, &n_list_46_Cons) && u_is_con(u_field(s1224, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1224, 0), 0);
      u_value v_i = u_field(u_field(s1224, 0), 1);
      u_value v_more = u_field(s1224, 1);
      d1225 = 1;
      {
        u_value s1228 = f_parse_46_prefix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op);
        int d1229 = 0;
        if ((!d1229) && u_is_con(s1228, &n_option_46_Some)) {
          u_value v_p = u_field(s1228, 0);
          d1229 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1230 = u_apply(f_parse_46_Ops_46_prefix_op_1(v_o), 2, v_op, v_b);
            *r_a = t1230;
          }
        }
        if ((!d1229) && u_is_con(s1228, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op))) {
            d1229 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1231 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1231), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1229) && u_is_con(s1228, &n_option_46_None)) {
          d1229 = 1;
          {
            static struct u_object l_undeclared_32_opera_1232 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1232, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1229) {
          u_no_case(s1228);
        }
      }
    }
    if ((!d1225) && u_is_con(s1224, &n_list_46_Nil)) {
      d1225 = 1;
      {
        static struct u_object l_missing_32_operand_1233 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1233, v_at), "parse.ParseError");
      }
    }
    if (!d1225) {
      u_no_case(s1224);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1234 = v_items;
    int d1235 = 0;
    if ((!d1235) && (u_is_con(s1234, &n_list_46_Cons) && u_is_con(u_field(s1234, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1234, 0), 0);
      u_value v_i = u_field(u_field(s1234, 0), 1);
      u_value v_tail = u_field(s1234, 1);
      d1235 = 1;
      {
        u_value s1236 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op), f_parse_46_infix_of_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op));
        int d1237 = 0;
        if ((!d1237) && u_is_con(u_field(s1236, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1236, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1237 = 1;
            {
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_postfix_op_1(v_o), 2, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1237) && u_is_con(u_field(s1236, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1236, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1236, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1237 = 1;
            {
              u_value t1238;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1238 = v_p;
              } else {
                t1238 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1238, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)));
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_infix_op_1(v_o), 3, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1237) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op)))) {
            d1237 = 1;
            {
              static struct u_object l_undeclared_32_opera_1239 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1239, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1237) {
          d1237 = 1;
          {
            u_value t1240 = v_lhs;
            u_value t1241 = v_items;
            *r_a = t1240;
            *r_rest = t1241;
          }
        }
        if (!d1237) {
          u_no_case(s1236);
        }
      }
    }
    if (!d1235) {
      d1235 = 1;
      {
        u_value t1242 = v_lhs;
        u_value t1243 = v_items;
        *r_a = t1242;
        *r_rest = t1243;
      }
    }
    if (!d1235) {
      u_no_case(s1234);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1244 = v_rest;
    int d1245 = 0;
    if ((!d1245) && (u_is_con(s1244, &n_list_46_Cons) && u_is_con(u_field(s1244, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1244, 0), 0);
      u_value v_i = u_field(u_field(s1244, 0), 1);
      u_value t1248 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1248)) {
        t1248 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1248)) {
        d1245 = 1;
        {
          static struct u_object l_operator_32__1246 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1247 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1246, u_concat(v_op, &l__32_is_32_not_32_associat_1247)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1245) {
      d1245 = 1;
      {
      }
    }
    if (!d1245) {
      u_no_case(s1244);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1249 = v_fp;
  u_value t1250 = NULL;
  if ((!t1250) && u_is_con(s1249, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1249, 0), 1);
    t1250 = u_eq(v_q, v_p);
  }
  if (!t1250) {
    t1250 = u_mk_bool(0);
  }
  if (!t1250) {
    u_no_case(s1249);
  }
  return t1250;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1251 = v_ts;
  u_value t1252 = NULL;
  if ((!t1252) && (u_is_con(s1251, &n_list_46_Cons) && (u_is_con(u_field(s1251, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1251, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1251, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1251, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1251, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1251, 0), 1);
    t1252 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1252) && (u_is_con(s1251, &n_list_46_Cons) && u_is_con(u_field(s1251, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1251, 0);
    t1252 = v_t;
  }
  if ((!t1252) && u_is_con(s1251, &n_list_46_Nil)) {
    t1252 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1252) {
    static struct u_object l__40_invalid_41__1253 = U_LIT("(invalid)");
    t1252 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1253), v_ts);
  }
  if (!t1252) {
    u_no_case(s1251);
  }
  return t1252;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1254 = v_ts;
  u_value t1255 = NULL;
  if ((!t1255) && (u_is_con(s1254, &n_list_46_Cons) && (u_is_con(u_field(s1254, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1254, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1254, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1254, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1254, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1254, 0), 1);
    t1255 = v_cs;
  }
  if (!t1255) {
    t1255 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1255) {
    u_no_case(s1254);
  }
  return t1255;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1256 = U_LIT("->");
  u_value t1263;
  if (u_truth(u_eq(v_op, &l__45__62__1256))) {
    u_value t1257 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1258 = &t1257;
    u_value l1259 = v_l;
    while (u_is_con(l1259, &n_list_46_Cons)) {
      u_value v_t = u_field(l1259, 0);
      if (u_truth(u_mk_bool(1))) {
        e1258 = u_append(e1258, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1259 = u_field(l1259, 1);
    }
    t1263 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1257, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1260 = U_LIT("*");
    u_value t1262;
    if (u_truth(u_eq(v_op, &l__42__1260))) {
      static struct u_object l__40_product_41__1261 = U_LIT("(product)");
      t1262 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1261), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1262 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1263 = t1262;
  }
  return t1263;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1264 = v_t;
  u_value t1265 = NULL;
  if ((!t1265) && (u_is_con(s1264, &n_syntax_46_TSort) && (u_is_con(u_field(s1264, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1264, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1264, 0), 1), u_mk_lit("(invalid)")))))) {
    t1265 = u_mk_bool(1);
  }
  if ((!t1265) && u_is_con(s1264, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1264, 1);
    t1265 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1265) && u_is_con(s1264, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1264, 0);
    u_value v_r = u_field(s1264, 1);
    u_value t1266 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1266)) {
      t1266 = f_parse_46_has_invalid_1(v_r);
    }
    t1265 = t1266;
  }
  if ((!t1265) && u_is_con(s1264, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1264, 0);
    t1265 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1265) {
    t1265 = u_mk_bool(0);
  }
  if (!t1265) {
    u_no_case(s1264);
  }
  return t1265;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1267 = v_ts;
  u_value t1268 = NULL;
  if ((!t1268) && u_is_con(s1267, &n_list_46_Nil)) {
    t1268 = u_mk_bool(0);
  }
  if ((!t1268) && u_is_con(s1267, &n_list_46_Cons)) {
    u_value v_t = u_field(s1267, 0);
    u_value v_more = u_field(s1267, 1);
    u_value t1269 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1269)) {
      t1269 = f_parse_46_any_invalid_1(v_more);
    }
    t1268 = t1269;
  }
  if (!t1268) {
    u_no_case(s1267);
  }
  return t1268;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1270 = v_t;
  u_value t1271 = NULL;
  if ((!t1271) && u_is_con(s1270, &n_lex_46_TName)) {
    u_value v_n = u_field(s1270, 0);
    u_value t1272 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1272)) {
      t1272 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1271 = t1272;
  }
  if ((!t1271) && (u_is_con(s1270, &n_lex_46_TPunct) && u_equals(u_field(s1270, 0), u_mk_char(40)))) {
    t1271 = u_mk_bool(1);
  }
  if (!t1271) {
    t1271 = u_mk_bool(0);
  }
  if (!t1271) {
    u_no_case(s1270);
  }
  return t1271;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1273 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1273;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1274 = f_parse_46_type_one_1(v_r);
    *r_t = t1274;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1275 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1275, v_start), "parse.ParseError");
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
    u_value t1276 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1276;
    u_value t1277 = u_mk_bool(1);
    v_next = t1277;
    u_value t1278 = u_mk_bool(1);
    v_go = t1278;
    u_value t1287 = v_go;
    while (u_truth(t1287)) {
      {
        u_value s1279 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1280 = 0;
        if ((!d1280) && u_is_con(s1279, &n_option_46_Some)) {
          u_value v_op = u_field(s1279, 0);
          d1280 = 1;
          {
            u_value t1281 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1282 = u_mk_bool(1);
            v_rev = t1281;
            v_next = t1282;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1280) && u_is_con(s1279, &n_option_46_None)) {
          u_value t1285 = v_next;
          if (u_truth(t1285)) {
            t1285 = f_parse_46_starts_type_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1285)) {
            d1280 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1283 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1284 = u_mk_bool(0);
              v_rev = t1283;
              v_next = t1284;
            }
          }
        }
        if (!d1280) {
          d1280 = 1;
          {
            u_value t1286 = u_mk_bool(0);
            v_go = t1286;
          }
        }
        if (!d1280) {
          u_no_case(s1279);
        }
      }
      t1287 = v_go;
    }
    u_value s1288 = v_rev;
    int d1289 = 0;
    if ((!d1289) && u_is_con(s1288, &n_list_46_Nil)) {
      d1289 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1290 = U_LIT("type expected but ");
        static struct u_object l__32_found_1291 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1290, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1291)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1289) {
      d1289 = 1;
      {
        u_value t1292 = f_list_46_reverse_1(v_rev);
        *r_items = t1292;
      }
    }
    if (!d1289) {
      u_no_case(s1288);
    }
  }
}

void p_parse_46_type_basic(u_value v_sc, u_value *r_r, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_args = NULL;
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
            u_value t1293 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
            *r_r = t1293;
          }
        } else {
          if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Scope_46_params_1(v_sc)))) {
            {
              u_value t1294 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1294;
            }
          } else {
            {
              u_value t1295 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1295;
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
    u_value t1296 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1296;
    u_value t1297 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1297)) {
      t1297 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1297)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1298 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1298;
        u_value t1300 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1300)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1299 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1299;
          }
          t1300 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1301 = f_list_46_reverse_1(v_rev);
    *r_r = t1301;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1302 = v_es;
  u_value t1303 = NULL;
  if ((!t1303) && u_is_con(s1302, &n_list_46_Nil)) {
    t1303 = v_rest;
  }
  if ((!t1303) && u_is_con(s1302, &n_list_46_Cons)) {
    u_value v_e = u_field(s1302, 0);
    u_value v_more = u_field(s1302, 1);
    static struct u_object l_Cons_1304 = U_LIT("Cons");
    t1303 = f_parse_46_expr_op2_3(&l_Cons_1304, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1303) {
    u_no_case(s1302);
  }
  return t1303;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1305 = v_t;
  u_value t1306 = NULL;
  if ((!t1306) && u_is_con(s1305, &n_lex_46_TName)) {
    u_value v_n = u_field(s1305, 0);
    static struct u_object l_let_1307 = U_LIT("let");
    static struct u_object l_if_1308 = U_LIT("if");
    static struct u_object l_lambda_1309 = U_LIT("lambda");
    static struct u_object l_match_1310 = U_LIT("match");
    static struct u_object l_true_1311 = U_LIT("true");
    static struct u_object l_false_1312 = U_LIT("false");
    u_value t1314 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1307, u_mk_con(&n_list_46_Cons, 2, &l_if_1308, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1309, u_mk_con(&n_list_46_Cons, 2, &l_match_1310, u_mk_con(&n_list_46_Cons, 2, &l_true_1311, u_mk_con(&n_list_46_Cons, 2, &l_false_1312, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1314)) {
      u_value t1313 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1313)) {
        t1313 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1314 = t1313;
    }
    t1306 = t1314;
  }
  if ((!t1306) && u_is_con(s1305, &n_lex_46_TNum)) {
    t1306 = u_mk_bool(1);
  }
  if ((!t1306) && u_is_con(s1305, &n_lex_46_TStr)) {
    t1306 = u_mk_bool(1);
  }
  if ((!t1306) && u_is_con(s1305, &n_lex_46_TChar)) {
    t1306 = u_mk_bool(1);
  }
  if ((!t1306) && u_is_con(s1305, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1305, 0);
    u_value t1315 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1315)) {
      t1315 = u_eq(v_c, u_mk_char(91));
    }
    t1306 = t1315;
  }
  if (!t1306) {
    t1306 = u_mk_bool(0);
  }
  if (!t1306) {
    u_no_case(s1305);
  }
  return t1306;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    static struct u_object l_raise_1316 = U_LIT("raise");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_raise_1316))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, r_e, r_ts);
        u_value t1317 = u_mk_con(&n_syntax_46_ERaise, 2, *r_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_e = t1317;
      }
    } else {
      {
        p_parse_46_operators(v_sc, r_e, r_ts);
        static struct u_object l_with_1318 = U_LIT("with");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_with_1318))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_update(v_sc, r_e, r_ts);
            u_value t1319 = f_parse_46_at_update_1(*r_ts);
            while (u_truth(t1319)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_update(v_sc, r_e, r_ts);
              }
              t1319 = f_parse_46_at_update_1(*r_ts);
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
    static struct u_object l__58__61__1320 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1320), r_ts);
    p_parse_46_operators(v_sc, &v_a, r_ts);
    static struct u_object l__46__1321 = U_LIT(".");
    static struct u_object l__58__61__1322 = U_LIT(":=");
    u_value t1323 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1321, u_concat(v_f, &l__58__61__1322))), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1323;
  }
}

u_value f_parse_46_at_update_1(u_value v_ts) {
  u_value s1324 = v_ts;
  u_value t1325 = NULL;
  if ((!t1325) && (u_is_con(s1324, &n_list_46_Cons) && (u_is_con(u_field(u_field(s1324, 0), 0), &n_lex_46_TPunct) && (u_equals(u_field(u_field(u_field(s1324, 0), 0), 0), u_mk_char(44)) && (u_is_con(u_field(s1324, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s1324, 1), 0), 0), &n_lex_46_TName) && (u_is_con(u_field(u_field(s1324, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(u_field(s1324, 1), 1), 0), 0), &n_lex_46_TSym) && u_equals(u_field(u_field(u_field(u_field(u_field(s1324, 1), 1), 0), 0), 0), u_mk_lit(":=")))))))))) {
    t1325 = u_mk_bool(1);
  }
  if (!t1325) {
    t1325 = u_mk_bool(0);
  }
  if (!t1325) {
    u_no_case(s1324);
  }
  return t1325;
}

void p_parse_46_operators(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_items = NULL;
    p_parse_46_expr_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_expr_op1_2, 0), u_mk_closure(w_f_parse_46_expr_op1_2, 0), u_mk_closure(w_f_parse_46_expr_op2_3, 0)), f_parse_46_head_offset_1(*r_ts), r_e);
  }
}

void p_parse_46_expr_items(u_value v_sc, u_value *r_items, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_next = NULL;
    u_value v_go = NULL;
    u_value v_e = NULL;
    u_value t1326 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1326;
    u_value t1327 = u_mk_bool(1);
    v_next = t1327;
    u_value t1328 = u_mk_bool(1);
    v_go = t1328;
    u_value t1337 = v_go;
    while (u_truth(t1337)) {
      {
        u_value s1329 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1330 = 0;
        if ((!d1330) && u_is_con(s1329, &n_option_46_Some)) {
          u_value v_op = u_field(s1329, 0);
          d1330 = 1;
          {
            u_value t1331 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1332 = u_mk_bool(1);
            v_rev = t1331;
            v_next = t1332;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1330) && u_is_con(s1329, &n_option_46_None)) {
          u_value t1335 = v_next;
          if (u_truth(t1335)) {
            t1335 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1335)) {
            d1330 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1333 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1334 = u_mk_bool(0);
              v_rev = t1333;
              v_next = t1334;
            }
          }
        }
        if (!d1330) {
          d1330 = 1;
          {
            u_value t1336 = u_mk_bool(0);
            v_go = t1336;
          }
        }
        if (!d1330) {
          u_no_case(s1329);
        }
      }
      t1337 = v_go;
    }
    u_value s1338 = v_rev;
    int d1339 = 0;
    if ((!d1339) && u_is_con(s1338, &n_list_46_Nil)) {
      d1339 = 1;
      {
        static struct u_object l_expression_32_expec_1340 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1341 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1340, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1341)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1339) {
      d1339 = 1;
      {
        u_value t1342 = f_list_46_reverse_1(v_rev);
        *r_items = t1342;
      }
    }
    if (!d1339) {
      u_no_case(s1338);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1343 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1343;
    u_value t1345 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1345)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1344 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1344;
      }
      t1345 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1346 = f_list_46_reverse_1(v_rev);
    *r_es = t1346;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1347 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1347;
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
    u_value t1348 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1348)) {
      t1348 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1352 = t1348;
    while (u_truth(t1352)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1349 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1349;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1350 = U_LIT(".");
            u_value t1351 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1350, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1351;
          }
        }
      }
      u_value t1353 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1353)) {
        t1353 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1352 = t1353;
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
    u_value s1354 = f_parse_46_head_token_1(*r_ts);
    int d1355 = 0;
    if ((!d1355) && (u_is_con(s1354, &n_lex_46_TName) && u_equals(u_field(s1354, 0), u_mk_lit("let")))) {
      d1355 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1355) && (u_is_con(s1354, &n_lex_46_TName) && u_equals(u_field(s1354, 0), u_mk_lit("if")))) {
      d1355 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1356 = U_LIT("then");
        p_parse_46_keyword(&l_then_1356, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1357 = U_LIT("else");
        p_parse_46_keyword(&l_else_1357, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1358 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1358;
      }
    }
    if ((!d1355) && (u_is_con(s1354, &n_lex_46_TName) && u_equals(u_field(s1354, 0), u_mk_lit("lambda")))) {
      d1355 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1355) && (u_is_con(s1354, &n_lex_46_TName) && u_equals(u_field(s1354, 0), u_mk_lit("match")))) {
      d1355 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1359 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1359;
      }
    }
    if ((!d1355) && (u_is_con(s1354, &n_lex_46_TName) && u_equals(u_field(s1354, 0), u_mk_lit("true")))) {
      d1355 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1360 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1360;
      }
    }
    if ((!d1355) && (u_is_con(s1354, &n_lex_46_TName) && u_equals(u_field(s1354, 0), u_mk_lit("false")))) {
      d1355 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1361 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1361;
      }
    }
    if ((!d1355) && u_is_con(s1354, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1354, 0);
      d1355 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1362 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1362;
      }
    }
    if ((!d1355) && u_is_con(s1354, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1354, 0);
      d1355 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1363 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1363;
      }
    }
    if ((!d1355) && u_is_con(s1354, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1354, 0);
      d1355 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1364 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1364;
      }
    }
    if ((!d1355) && (u_is_con(s1354, &n_lex_46_TPunct) && u_equals(u_field(s1354, 0), u_mk_char(40)))) {
      d1355 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1365 = v_es;
        int d1366 = 0;
        if ((!d1366) && (u_is_con(s1365, &n_list_46_Cons) && u_is_con(u_field(s1365, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1365, 0);
          d1366 = 1;
          {
            u_value t1367 = v_x;
            *r_e = t1367;
          }
        }
        if (!d1366) {
          d1366 = 1;
          {
            u_value t1368 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1368;
          }
        }
        if (!d1366) {
          u_no_case(s1365);
        }
      }
    }
    if ((!d1355) && (u_is_con(s1354, &n_lex_46_TPunct) && u_equals(u_field(s1354, 0), u_mk_char(91)))) {
      d1355 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1355) && u_is_con(s1354, &n_lex_46_TName)) {
      u_value v_n = u_field(s1354, 0);
      u_value t1370 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1370)) {
        t1370 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t1370)) {
        d1355 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1369 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1369;
        }
      }
    }
    if (!d1355) {
      d1355 = 1;
      {
        static struct u_object l_expression_32_expec_1371 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1372 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1371, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1372)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1355) {
      u_no_case(s1354);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1373 = v_ps;
  u_value t1374 = NULL;
  if ((!t1374) && u_is_con(s1373, &n_list_46_Nil)) {
    t1374 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1374) && (u_is_con(s1373, &n_list_46_Cons) && u_is_con(u_field(s1373, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1373, 0), 0);
    u_value v_more = u_field(s1373, 1);
    u_value s1375 = f_parse_46_pattern_vars_1(v_more);
    u_value t1376 = NULL;
    if ((!t1376) && u_is_con(s1375, &n_option_46_Some)) {
      u_value v_xs = u_field(s1375, 0);
      t1376 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1376) && u_is_con(s1375, &n_option_46_None)) {
      t1376 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1376) {
      u_no_case(s1375);
    }
    t1374 = t1376;
  }
  if (!t1374) {
    t1374 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1374) {
    u_no_case(s1373);
  }
  return t1374;
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
    static struct u_object l__58__61__1377 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1377), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1378 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1379 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1378;
    v_es = t1379;
    u_value t1383 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1383)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1380 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1380), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1381 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1382 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1381;
        v_es = t1382;
      }
      t1383 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1384 = U_LIT("in");
    p_parse_46_keyword(&l_in_1384, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1385 = f_list_46_reverse_1(v_ps);
    u_value t1386 = f_list_46_reverse_1(v_es);
    v_ps = t1385;
    v_es = t1386;
    u_value s1387 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1388 = 0;
    if ((!d1388) && u_is_con(u_field(s1387, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1387, 0), 0);
      d1388 = 1;
      {
        u_value t1389 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1389;
      }
    }
    if ((!d1388) && (u_is_con(u_field(s1387, 0), &n_option_46_None) && (u_is_con(u_field(s1387, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1387, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1387, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1387, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1387, 1), 0);
      u_value v_c = u_field(u_field(s1387, 2), 0);
      d1388 = 1;
      {
        u_value t1390 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1390;
      }
    }
    if (!d1388) {
      d1388 = 1;
      {
        u_value t1391 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1391;
      }
    }
    if (!d1388) {
      u_no_case(s1387);
    }
  }
}

void p_parse_46_parse_lambda(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    p_parse_46_advance(r_ts);
    u_value t1392 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1392;
    static struct u_object l__61__62__1393 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1393)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1394 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1394;
        u_value t1396 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1396)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1395 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1395;
          }
          t1396 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1397 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1397), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1398 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1398;
  }
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
        static struct u_object l_Nil_1399 = U_LIT("Nil");
        u_value t1400 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1399), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1400;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1401 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1401))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1402 = U_LIT("in");
            p_parse_46_keyword(&l_in_1402, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1403 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1404 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1403;
            v_lists = t1404;
            u_value t1408 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1408)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1405 = U_LIT("in");
                p_parse_46_keyword(&l_in_1405, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1406 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1407 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1406;
                v_lists = t1407;
              }
              t1408 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1409 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1409;
            static struct u_object l_if_1410 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1410))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1411 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1411;
          }
        } else {
          {
            u_value t1412 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1412;
            u_value t1414 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1414)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1413 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1413;
              }
              t1414 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1415 = U_LIT("Nil");
            u_value t1416 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1415), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1416;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1417 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1417;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1418 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1418))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1419 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1419;
      }
    } else {
      {
        u_value t1420 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1420;
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
    u_value t1421 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1421;
    static struct u_object l_case_1422 = U_LIT("case");
    u_value t1425 = f_parse_46_at_keyword_2(*r_ts, &l_case_1422);
    while (u_truth(t1425)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1423 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1423), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1424 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1424;
      }
      static struct u_object l_case_1426 = U_LIT("case");
      t1425 = f_parse_46_at_keyword_2(*r_ts, &l_case_1426);
    }
    static struct u_object l_else_1427 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1427))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1428 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1428;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1429 = U_LIT("end");
    p_parse_46_keyword(&l_end_1429, r_ts);
    u_value t1430 = f_list_46_reverse_1(v_rev);
    *r_cs = t1430;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1431 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1431))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1432 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1432, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
  }
}

u_value f_parse_46_pat_op1_2(u_value v_op, u_value v_p) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_pat_op2_3(u_value v_op, u_value v_l, u_value v_r) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_pats_2(u_value v_ps, u_value v_rest) {
  u_value s1433 = v_ps;
  u_value t1434 = NULL;
  if ((!t1434) && u_is_con(s1433, &n_list_46_Nil)) {
    t1434 = v_rest;
  }
  if ((!t1434) && u_is_con(s1433, &n_list_46_Cons)) {
    u_value v_p = u_field(s1433, 0);
    u_value v_more = u_field(s1433, 1);
    static struct u_object l_Cons_1435 = U_LIT("Cons");
    t1434 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1435), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1434) {
    u_no_case(s1433);
  }
  return t1434;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1436 = v_t;
  u_value t1437 = NULL;
  if ((!t1437) && u_is_con(s1436, &n_lex_46_TName)) {
    u_value v_n = u_field(s1436, 0);
    static struct u_object l___1438 = U_LIT("_");
    static struct u_object l_true_1439 = U_LIT("true");
    static struct u_object l_false_1440 = U_LIT("false");
    u_value t1442 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1438, u_mk_con(&n_list_46_Cons, 2, &l_true_1439, u_mk_con(&n_list_46_Cons, 2, &l_false_1440, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1442)) {
      u_value t1441 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1441)) {
        t1441 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1442 = t1441;
    }
    t1437 = t1442;
  }
  if ((!t1437) && u_is_con(s1436, &n_lex_46_TNum)) {
    t1437 = u_mk_bool(1);
  }
  if ((!t1437) && u_is_con(s1436, &n_lex_46_TStr)) {
    t1437 = u_mk_bool(1);
  }
  if ((!t1437) && u_is_con(s1436, &n_lex_46_TChar)) {
    t1437 = u_mk_bool(1);
  }
  if ((!t1437) && u_is_con(s1436, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1436, 0);
    u_value t1443 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1443)) {
      t1443 = u_eq(v_c, u_mk_char(91));
    }
    t1437 = t1443;
  }
  if (!t1437) {
    t1437 = u_mk_bool(0);
  }
  if (!t1437) {
    u_no_case(s1436);
  }
  return t1437;
}

void p_parse_46_parse_pattern(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_items = NULL;
    p_parse_46_pattern_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_pat_op1_2, 0), u_mk_closure(w_f_parse_46_pat_op1_2, 0), u_mk_closure(w_f_parse_46_pat_op2_3, 0)), f_parse_46_head_offset_1(*r_ts), r_p);
  }
}

void p_parse_46_pattern_items(u_value v_sc, u_value *r_items, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_next = NULL;
    u_value v_go = NULL;
    u_value v_p = NULL;
    u_value t1444 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1444;
    u_value t1445 = u_mk_bool(1);
    v_next = t1445;
    u_value t1446 = u_mk_bool(1);
    v_go = t1446;
    u_value t1455 = v_go;
    while (u_truth(t1455)) {
      {
        u_value s1447 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1448 = 0;
        if ((!d1448) && u_is_con(s1447, &n_option_46_Some)) {
          u_value v_op = u_field(s1447, 0);
          d1448 = 1;
          {
            u_value t1449 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1450 = u_mk_bool(1);
            v_rev = t1449;
            v_next = t1450;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1448) && u_is_con(s1447, &n_option_46_None)) {
          u_value t1453 = v_next;
          if (u_truth(t1453)) {
            t1453 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1453)) {
            d1448 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1451 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1452 = u_mk_bool(0);
              v_rev = t1451;
              v_next = t1452;
            }
          }
        }
        if (!d1448) {
          d1448 = 1;
          {
            u_value t1454 = u_mk_bool(0);
            v_go = t1454;
          }
        }
        if (!d1448) {
          u_no_case(s1447);
        }
      }
      t1455 = v_go;
    }
    u_value s1456 = v_rev;
    int d1457 = 0;
    if ((!d1457) && u_is_con(s1456, &n_list_46_Nil)) {
      d1457 = 1;
      {
        static struct u_object l_pattern_32_expected_1458 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1459 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1458, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1459)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1457) {
      d1457 = 1;
      {
        u_value t1460 = f_list_46_reverse_1(v_rev);
        *r_items = t1460;
      }
    }
    if (!d1457) {
      u_no_case(s1456);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1461 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1461;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1462 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1462;
        u_value t1464 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1464)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1463 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1463;
          }
          t1464 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1465 = f_list_46_reverse_1(v_rev);
    *r_ps = t1465;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1466 = f_parse_46_head_token_1(*r_ts);
    int d1467 = 0;
    if ((!d1467) && (u_is_con(s1466, &n_lex_46_TName) && u_equals(u_field(s1466, 0), u_mk_lit("_")))) {
      d1467 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1468 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1468;
      }
    }
    if ((!d1467) && (u_is_con(s1466, &n_lex_46_TName) && u_equals(u_field(s1466, 0), u_mk_lit("true")))) {
      d1467 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1469 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1469;
      }
    }
    if ((!d1467) && (u_is_con(s1466, &n_lex_46_TName) && u_equals(u_field(s1466, 0), u_mk_lit("false")))) {
      d1467 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1470 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1470;
      }
    }
    if ((!d1467) && u_is_con(s1466, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1466, 0);
      d1467 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1471 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1471;
      }
    }
    if ((!d1467) && u_is_con(s1466, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1466, 0);
      d1467 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1472 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1472;
      }
    }
    if ((!d1467) && u_is_con(s1466, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1466, 0);
      d1467 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1473 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1473;
      }
    }
    if ((!d1467) && (u_is_con(s1466, &n_lex_46_TPunct) && u_equals(u_field(s1466, 0), u_mk_char(40)))) {
      d1467 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1474 = v_ps;
        int d1475 = 0;
        if ((!d1475) && (u_is_con(s1474, &n_list_46_Cons) && u_is_con(u_field(s1474, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1474, 0);
          d1475 = 1;
          {
            u_value t1476 = v_q;
            *r_p = t1476;
          }
        }
        if (!d1475) {
          d1475 = 1;
          {
            u_value t1477 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1477;
          }
        }
        if (!d1475) {
          u_no_case(s1474);
        }
      }
    }
    if ((!d1467) && (u_is_con(s1466, &n_lex_46_TPunct) && u_equals(u_field(s1466, 0), u_mk_char(91)))) {
      d1467 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1478 = U_LIT("Nil");
            u_value t1479 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1478), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1479;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1480 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1480;
            u_value t1482 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1482)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1481 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1481;
              }
              t1482 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1483 = U_LIT("Nil");
            u_value t1484 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1483), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1484;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1485 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1485;
          }
        }
      }
    }
    if (!d1467) {
      d1467 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1486 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1486;
          }
        } else {
          {
            u_value t1487 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1487;
          }
        }
      }
    }
    if (!d1467) {
      u_no_case(s1466);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1488 = v_t;
  u_value t1489 = NULL;
  if ((!t1489) && u_is_con(s1488, &n_lex_46_TName)) {
    u_value v_n = u_field(s1488, 0);
    static struct u_object l_skip_1490 = U_LIT("skip");
    static struct u_object l_if_1491 = U_LIT("if");
    static struct u_object l_while_1492 = U_LIT("while");
    static struct u_object l_call_1493 = U_LIT("call");
    static struct u_object l_match_1494 = U_LIT("match");
    static struct u_object l_assert_1495 = U_LIT("assert");
    static struct u_object l_assign_1496 = U_LIT("assign");
    static struct u_object l_raise_1497 = U_LIT("raise");
    static struct u_object l_try_1498 = U_LIT("try");
    t1489 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1490, u_mk_con(&n_list_46_Cons, 2, &l_if_1491, u_mk_con(&n_list_46_Cons, 2, &l_while_1492, u_mk_con(&n_list_46_Cons, 2, &l_call_1493, u_mk_con(&n_list_46_Cons, 2, &l_match_1494, u_mk_con(&n_list_46_Cons, 2, &l_assert_1495, u_mk_con(&n_list_46_Cons, 2, &l_assign_1496, u_mk_con(&n_list_46_Cons, 2, &l_raise_1497, u_mk_con(&n_list_46_Cons, 2, &l_try_1498, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1489) {
    t1489 = u_mk_bool(0);
  }
  if (!t1489) {
    u_no_case(s1488);
  }
  return t1489;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1499 = v_cs;
  u_value t1500 = NULL;
  if ((!t1500) && u_is_con(s1499, &n_list_46_Nil)) {
    t1500 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1500) && (u_is_con(s1499, &n_list_46_Cons) && u_is_con(u_field(s1499, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1499, 0), 0);
    u_value v_g = u_field(u_field(s1499, 0), 1);
    u_value v_b = u_field(u_field(s1499, 0), 2);
    u_value v_more = u_field(s1499, 1);
    t1500 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1500) {
    u_no_case(s1499);
  }
  return t1500;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1501 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1501;
    u_value t1502 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1502;
    u_value t1503 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1503;
    static struct u_object l_local_1504 = U_LIT("local");
    u_value t1506 = f_parse_46_at_keyword_2(*r_ts, &l_local_1504);
    while (u_truth(t1506)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1505 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1505)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1505 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1507 = U_LIT("local");
      t1506 = f_parse_46_at_keyword_2(*r_ts, &l_local_1507);
    }
    u_value t1509 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1509)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1508 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1508;
      }
      t1509 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1510 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1510;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1511 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1511;
    static struct u_object l__58__1512 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1512)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__1513 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1513)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1514 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1514;
      }
    }
    u_value t1515 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1515;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1516 = f_parse_46_head_token_1(*r_ts);
    int d1517 = 0;
    if ((!d1517) && (u_is_con(s1516, &n_lex_46_TName) && u_equals(u_field(s1516, 0), u_mk_lit("skip")))) {
      d1517 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1518 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1518;
      }
    }
    if ((!d1517) && (u_is_con(s1516, &n_lex_46_TName) && u_equals(u_field(s1516, 0), u_mk_lit("if")))) {
      d1517 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1517) && (u_is_con(s1516, &n_lex_46_TName) && u_equals(u_field(s1516, 0), u_mk_lit("while")))) {
      d1517 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1519 = U_LIT("end");
        p_parse_46_keyword(&l_end_1519, r_ts);
        u_value t1520 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1520;
      }
    }
    if ((!d1517) && (u_is_con(s1516, &n_lex_46_TName) && u_equals(u_field(s1516, 0), u_mk_lit("call")))) {
      d1517 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1517) && (u_is_con(s1516, &n_lex_46_TName) && u_equals(u_field(s1516, 0), u_mk_lit("match")))) {
      d1517 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1521 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1521;
      }
    }
    if ((!d1517) && (u_is_con(s1516, &n_lex_46_TName) && u_equals(u_field(s1516, 0), u_mk_lit("assert")))) {
      d1517 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1522 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1522;
      }
    }
    if ((!d1517) && (u_is_con(s1516, &n_lex_46_TName) && u_equals(u_field(s1516, 0), u_mk_lit("assign")))) {
      d1517 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1517) && (u_is_con(s1516, &n_lex_46_TName) && u_equals(u_field(s1516, 0), u_mk_lit("raise")))) {
      d1517 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1523 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1523;
      }
    }
    if ((!d1517) && (u_is_con(s1516, &n_lex_46_TName) && u_equals(u_field(s1516, 0), u_mk_lit("try")))) {
      d1517 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1524 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1524, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1525 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1525;
      }
    }
    if (!d1517) {
      d1517 = 1;
      {
        static struct u_object l_statement_32_expect_1526 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1527 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1526, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1527)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1517) {
      u_no_case(s1516);
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
    static struct u_object l_else_1528 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1528)))) {
      {
        static struct u_object l_end_1529 = U_LIT("end");
        p_parse_46_keyword(&l_end_1529, r_ts);
        u_value t1530 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1530;
      }
    } else {
      static struct u_object l_if_1531 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1531)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1532 = U_LIT("end");
          p_parse_46_keyword(&l_end_1532, r_ts);
        }
      }
    }
    u_value t1533 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1533;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1534 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1534), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1535 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1535;
    u_value t1538 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1538)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1536 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1536), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1537 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1537;
      }
      t1538 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1539 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1539;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1540 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1540;
    static struct u_object l_case_1541 = U_LIT("case");
    u_value t1544 = f_parse_46_at_keyword_2(*r_ts, &l_case_1541);
    while (u_truth(t1544)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1542 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1542), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1543 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1543;
      }
      static struct u_object l_case_1545 = U_LIT("case");
      t1544 = f_parse_46_at_keyword_2(*r_ts, &l_case_1545);
    }
    static struct u_object l_else_1546 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1546))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1547 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1547;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1548 = U_LIT("end");
    p_parse_46_keyword(&l_end_1548, r_ts);
    u_value t1549 = f_list_46_reverse_1(v_rev);
    *r_cs = t1549;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1551;
  if (u_truth(v_first)) {
    t1551 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1550 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1550)) {
      t1550 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1551 = t1550;
  }
  return t1551;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1553 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1553)) {
    static struct u_object l_out_1552 = U_LIT("out");
    t1553 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1552));
  }
  u_value t1555 = t1553;
  if (u_truth(t1555)) {
    static struct u_object l_ref_1554 = U_LIT("ref");
    t1555 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1554));
  }
  return t1555;
}

void p_parse_46_parse_call(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_p = NULL;
    u_value v_e = NULL;
    u_value v_ins = NULL;
    u_value v_outs = NULL;
    u_value v_refs = NULL;
    u_value v_first = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_name(&v_p, r_ts);
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    u_value t1556 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1557 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1558 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1559 = u_mk_bool(1);
    v_ins = t1556;
    v_outs = t1557;
    v_refs = t1558;
    v_first = t1559;
    static struct u_object l_in_1560 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1560))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1561 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
        u_value t1562 = u_mk_bool(0);
        v_ins = t1561;
        v_first = t1562;
        u_value t1564 = f_parse_46_item_follows_1(*r_ts);
        while (u_truth(t1564)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_expr(v_sc, &v_e, r_ts);
            u_value t1563 = u_mk_con(&n_list_46_Cons, 2, v_e, v_ins);
            v_ins = t1563;
          }
          t1564 = f_parse_46_item_follows_1(*r_ts);
        }
      }
    }
    static struct u_object l_out_1565 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1565, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_outs, r_ts);
        u_value t1566 = u_mk_bool(0);
        v_first = t1566;
      }
    }
    static struct u_object l_ref_1567 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1567, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    u_value t1568 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), f_list_46_reverse_1(v_ins), v_outs, v_refs);
    *r_s = t1568;
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

void p_parse_46_parse_variables(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    u_value t1569 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1569;
    u_value t1571 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1571)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1570 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_rev);
        v_rev = t1570;
      }
      t1571 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1572 = f_list_46_reverse_1(v_rev);
    *r_xs = t1572;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1573 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1573;
    static struct u_object l__58__1574 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1574)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1575 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1575;
  }
}

void p_parse_46_parse_formals1(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_formal(v_sc, &v_x, r_ts);
    u_value t1576 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1576;
    u_value t1578 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1578)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formal(v_sc, &v_x, r_ts);
        u_value t1577 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1577;
      }
      t1578 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1579 = f_list_46_reverse_1(v_rev);
    *r_xs = t1579;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1580 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1580;
      }
    } else {
      {
        p_parse_46_parse_formals1(v_sc, r_xs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
  }
}

u_value f_parse_46_declarations_1(u_value v_cs) {
  u_value s1581 = v_cs;
  u_value t1582 = NULL;
  if ((!t1582) && u_is_con(s1581, &n_list_46_Nil)) {
    t1582 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1582) && (u_is_con(s1581, &n_list_46_Cons) && u_is_con(u_field(s1581, 0), &n_syntax_46_CEval))) {
    u_value v_more = u_field(s1581, 1);
    t1582 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1582) && (u_is_con(s1581, &n_list_46_Cons) && u_is_con(u_field(s1581, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1581, 1);
    t1582 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1582) && (u_is_con(s1581, &n_list_46_Cons) && u_is_con(u_field(s1581, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1581, 1);
    t1582 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1582) && u_is_con(s1581, &n_list_46_Cons)) {
    u_value v_c = u_field(s1581, 0);
    u_value v_more = u_field(s1581, 1);
    t1582 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1582) {
    u_no_case(s1581);
  }
  return t1582;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1583 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1584 = &t1583;
  u_value l1585 = v_cs;
  while (u_is_con(l1585, &n_list_46_Cons)) {
    u_value v_c = u_field(l1585, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1584 = u_append(e1584, v_c);
    }
    l1585 = u_field(l1585, 1);
  }
  return t1583;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1586 = v_c;
  u_value t1587 = NULL;
  if ((!t1587) && u_is_con(s1586, &n_syntax_46_CNotation)) {
    t1587 = u_mk_bool(1);
  }
  if (!t1587) {
    t1587 = u_mk_bool(0);
  }
  if (!t1587) {
    u_no_case(s1586);
  }
  return t1587;
}

u_value f_parse_46_Context_46_notations_1(u_value v_x) {
  u_value s1588 = v_x;
  u_value t1589 = NULL;
  if ((!t1589) && u_is_con(s1588, &n_parse_46_Context)) {
    u_value v_y = u_field(s1588, 0);
    t1589 = v_y;
  }
  if (!t1589) {
    u_no_case(s1588);
  }
  return t1589;
}

u_value f_parse_46_Context_46_modules_1(u_value v_x) {
  u_value s1590 = v_x;
  u_value t1591 = NULL;
  if ((!t1591) && u_is_con(s1590, &n_parse_46_Context)) {
    u_value v_y = u_field(s1590, 1);
    t1591 = v_y;
  }
  if (!t1591) {
    u_no_case(s1590);
  }
  return t1591;
}

u_value f_parse_46_Context_46_loading_1(u_value v_x) {
  u_value s1592 = v_x;
  u_value t1593 = NULL;
  if ((!t1593) && u_is_con(s1592, &n_parse_46_Context)) {
    u_value v_y = u_field(s1592, 2);
    t1593 = v_y;
  }
  if (!t1593) {
    u_no_case(s1592);
  }
  return t1593;
}

u_value f_parse_46_Context_46_base_1(u_value v_x) {
  u_value s1594 = v_x;
  u_value t1595 = NULL;
  if ((!t1595) && u_is_con(s1594, &n_parse_46_Context)) {
    u_value v_y = u_field(s1594, 3);
    t1595 = v_y;
  }
  if (!t1595) {
    u_no_case(s1594);
  }
  return t1595;
}

u_value f_parse_46_Context_46_dir_1(u_value v_x) {
  u_value s1596 = v_x;
  u_value t1597 = NULL;
  if ((!t1597) && u_is_con(s1596, &n_parse_46_Context)) {
    u_value v_y = u_field(s1596, 4);
    t1597 = v_y;
  }
  if (!t1597) {
    u_no_case(s1596);
  }
  return t1597;
}

u_value f_parse_46_Context_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1598 = v_x;
  u_value t1599 = NULL;
  if ((!t1599) && u_is_con(s1598, &n_parse_46_Context)) {
    u_value v_a1 = u_field(s1598, 1);
    u_value v_a2 = u_field(s1598, 2);
    u_value v_a3 = u_field(s1598, 3);
    u_value v_a4 = u_field(s1598, 4);
    t1599 = u_mk_con(&n_parse_46_Context, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1599) {
    u_no_case(s1598);
  }
  return t1599;
}

u_value f_parse_46_Context_46_modules_58__61__2(u_value v_x, u_value v_y) {
  u_value s1600 = v_x;
  u_value t1601 = NULL;
  if ((!t1601) && u_is_con(s1600, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1600, 0);
    u_value v_a2 = u_field(s1600, 2);
    u_value v_a3 = u_field(s1600, 3);
    u_value v_a4 = u_field(s1600, 4);
    t1601 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t1601) {
    u_no_case(s1600);
  }
  return t1601;
}

u_value f_parse_46_Context_46_loading_58__61__2(u_value v_x, u_value v_y) {
  u_value s1602 = v_x;
  u_value t1603 = NULL;
  if ((!t1603) && u_is_con(s1602, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1602, 0);
    u_value v_a1 = u_field(s1602, 1);
    u_value v_a3 = u_field(s1602, 3);
    u_value v_a4 = u_field(s1602, 4);
    t1603 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t1603) {
    u_no_case(s1602);
  }
  return t1603;
}

u_value f_parse_46_Context_46_base_58__61__2(u_value v_x, u_value v_y) {
  u_value s1604 = v_x;
  u_value t1605 = NULL;
  if ((!t1605) && u_is_con(s1604, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1604, 0);
    u_value v_a1 = u_field(s1604, 1);
    u_value v_a2 = u_field(s1604, 2);
    u_value v_a4 = u_field(s1604, 4);
    t1605 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t1605) {
    u_no_case(s1604);
  }
  return t1605;
}

u_value f_parse_46_Context_46_dir_58__61__2(u_value v_x, u_value v_y) {
  u_value s1606 = v_x;
  u_value t1607 = NULL;
  if ((!t1607) && u_is_con(s1606, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1606, 0);
    u_value v_a1 = u_field(s1606, 1);
    u_value v_a2 = u_field(s1606, 2);
    u_value v_a3 = u_field(s1606, 3);
    t1607 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t1607) {
    u_no_case(s1606);
  }
  return t1607;
}

u_value f_parse_46_with_commands_2(u_value v_cx, u_value v_cs) {
  return u_mk_con(&n_parse_46_Context, 5, f_parse_46_with_notations_2(f_parse_46_Context_46_notations_1(v_cx), v_cs), f_parse_46_Context_46_modules_1(v_cx), f_parse_46_Context_46_loading_1(v_cx), f_parse_46_Context_46_base_1(v_cx), f_parse_46_Context_46_dir_1(v_cx));
}

u_value f_parse_46_with_module_2(u_value v_cx, u_value v_c) {
  return u_mk_con(&n_parse_46_Context, 5, f_parse_46_Context_46_notations_1(v_cx), f_list_46__43__43__2(f_parse_46_modules_of_1(v_c), f_parse_46_Context_46_modules_1(v_cx)), f_parse_46_Context_46_loading_1(v_cx), f_parse_46_Context_46_base_1(v_cx), f_parse_46_Context_46_dir_1(v_cx));
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1608 = v_c;
  u_value t1609 = NULL;
  if ((!t1609) && u_is_con(s1608, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1608, 0);
    u_value v_cs = u_field(s1608, 1);
    u_value t1610 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1611 = &t1610;
    u_value l1612 = v_cs;
    while (u_is_con(l1612, &n_list_46_Cons)) {
      u_value v_d = u_field(l1612, 0);
      if (u_truth(u_mk_bool(1))) {
        e1611 = u_append(e1611, f_parse_46_modules_of_1(v_d));
      }
      l1612 = u_field(l1612, 1);
    }
    t1609 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1610));
  }
  if (!t1609) {
    t1609 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1609) {
    u_no_case(s1608);
  }
  return t1609;
}

u_value f_parse_46_own_notations_2(u_value v_cx, u_value v_n) {
  u_value s1613 = f_list_46_lookup_2(v_n, f_parse_46_Context_46_modules_1(v_cx));
  u_value t1614 = NULL;
  if ((!t1614) && u_is_con(s1613, &n_option_46_Some)) {
    u_value v_ns = u_field(s1613, 0);
    t1614 = v_ns;
  }
  if ((!t1614) && u_is_con(s1613, &n_option_46_None)) {
    t1614 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1614) {
    u_no_case(s1613);
  }
  return t1614;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1615 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1615;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1616 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1616;
        u_value t1618 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1618)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1617 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1617;
          }
          t1618 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1619 = f_list_46_reverse_1(v_rev);
    *r_ps = t1619;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1620 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1620;
    u_value s1621 = f_parse_46_head_token_1(*r_ts);
    int d1622 = 0;
    if ((!d1622) && (u_is_con(s1621, &n_lex_46_TName) && u_equals(u_field(s1621, 0), u_mk_lit("prefix")))) {
      d1622 = 1;
      {
        u_value t1623 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1623;
      }
    }
    if ((!d1622) && (u_is_con(s1621, &n_lex_46_TName) && u_equals(u_field(s1621, 0), u_mk_lit("postfix")))) {
      d1622 = 1;
      {
        u_value t1624 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1624;
      }
    }
    if (!d1622) {
      d1622 = 1;
      {
        u_value s1625 = f_parse_46_second_token_1(*r_ts);
        int d1626 = 0;
        if ((!d1626) && (u_is_con(s1625, &n_lex_46_TName) && u_equals(u_field(s1625, 0), u_mk_lit("left")))) {
          d1626 = 1;
          {
            u_value t1627 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1627;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1626) && (u_is_con(s1625, &n_lex_46_TName) && u_equals(u_field(s1625, 0), u_mk_lit("right")))) {
          d1626 = 1;
          {
            u_value t1628 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1628;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1626) {
          d1626 = 1;
          {
          }
        }
        if (!d1626) {
          u_no_case(s1625);
        }
      }
    }
    if (!d1622) {
      u_no_case(s1621);
    }
    p_parse_46_advance(r_ts);
    u_value s1629 = f_parse_46_head_token_1(*r_ts);
    int d1630 = 0;
    if ((!d1630) && u_is_con(s1629, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1629, 0);
      d1630 = 1;
      {
        u_value t1631 = v_n;
        v_p = t1631;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1630) {
      d1630 = 1;
      {
        static struct u_object l_precedence_32_expec_1632 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1633 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1632, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1633)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1630) {
      u_no_case(s1629);
    }
    u_value t1634 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1635 = u_mk_bool(1);
    v_ops = t1634;
    v_go = t1635;
    u_value t1641 = v_go;
    while (u_truth(t1641)) {
      {
        u_value s1636 = f_parse_46_head_token_1(*r_ts);
        int d1637 = 0;
        if ((!d1637) && u_is_con(s1636, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1636, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1637 = 1;
            {
              u_value t1638 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1638;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1637) && u_is_con(s1636, &n_lex_46_TName)) {
          u_value v_n = u_field(s1636, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1637 = 1;
            {
              u_value t1639 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1639;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1637) {
          d1637 = 1;
          {
            u_value t1640 = u_mk_bool(0);
            v_go = t1640;
          }
        }
        if (!d1637) {
          u_no_case(s1636);
        }
      }
      t1641 = v_go;
    }
    u_value s1642 = v_ops;
    int d1643 = 0;
    if ((!d1643) && u_is_con(s1642, &n_list_46_Nil)) {
      d1643 = 1;
      {
        static struct u_object l_operator_32_expecte_1644 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1645 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1644, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1645)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1643) {
      d1643 = 1;
      {
        u_value t1646 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1646;
      }
    }
    if (!d1643) {
      u_no_case(s1642);
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
    u_value t1647 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1648 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1647;
    *r_fs = t1648;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1649 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
            u_value t1650 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
            v_args = t1649;
            *r_fs = t1650;
            u_value t1653 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1653)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t1651 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t1652 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t1651;
                *r_fs = t1652;
              }
              t1653 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1654 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t1655 = f_list_46_reverse_1(*r_fs);
    *r_k = t1654;
    *r_fs = t1655;
    u_value s1656 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d1657 = 0;
    if ((!d1657) && u_is_con(s1656, &n_option_46_Some)) {
      u_value v_f = u_field(s1656, 0);
      d1657 = 1;
      {
        static struct u_object l_repeated_32_field_32__1658 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__1659 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__1658, u_concat(v_f, u_concat(&l__32_of_32__1659, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1657) && u_is_con(s1656, &n_option_46_None)) {
      d1657 = 1;
      {
      }
    }
    if (!d1657) {
      u_no_case(s1656);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1660 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1661 = 0;
    if ((!d1661) && (u_is_con(u_field(s1660, 0), &n_lex_46_TName) && (u_is_con(u_field(s1660, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1660, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1660, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1661 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1662 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1662;
        }
      }
    }
    if (!d1661) {
      d1661 = 1;
      {
        u_value t1663 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1663;
      }
    }
    if (!d1661) {
      u_no_case(s1660);
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
    static struct u_object l__58__61__1664 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1664), r_ts);
    static struct u_object l__124__1665 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1665)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1666 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1666;
    static struct u_object l__124__1667 = U_LIT("|");
    u_value t1669 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1667));
    while (u_truth(t1669)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1668 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1668;
      }
      static struct u_object l__124__1670 = U_LIT("|");
      t1669 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1670));
    }
    u_value s1671 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d1672 = 0;
    if ((!d1672) && u_is_con(s1671, &n_option_46_Some)) {
      u_value v_f = u_field(s1671, 0);
      d1672 = 1;
      {
        static struct u_object l_field_32__1673 = U_LIT("field ");
        static struct u_object l__32_of_32__1674 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_1675 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__1673, u_concat(v_f, u_concat(&l__32_of_32__1674, u_concat(v_n, &l__32_has_32_different_32_t_1675)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1672) && u_is_con(s1671, &n_option_46_None)) {
      d1672 = 1;
      {
      }
    }
    if (!d1672) {
      u_no_case(s1671);
    }
    u_value t1676 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_list_46__43__43__2(f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)), f_parse_46_updaters_3(v_n, v_ps, f_list_46_reverse_1(v_rev))));
    *r_cs = t1676;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s1677 = v_fs;
  u_value t1678 = NULL;
  if ((!t1678) && u_is_con(s1677, &n_list_46_Nil)) {
    t1678 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1678) && (u_is_con(s1677, &n_list_46_Cons) && u_is_con(u_field(s1677, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s1677, 0), 0);
    u_value v_rest = u_field(s1677, 1);
    t1678 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t1678) && (u_is_con(s1677, &n_list_46_Cons) && u_is_con(u_field(s1677, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1677, 1);
    t1678 = f_parse_46_field_names_1(v_rest);
  }
  if (!t1678) {
    u_no_case(s1677);
  }
  return t1678;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s1679 = v_fs;
  u_value t1680 = NULL;
  if ((!t1680) && u_is_con(s1679, &n_list_46_Nil)) {
    t1680 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1680) && u_is_con(s1679, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1679, 0), 0);
    u_value v_t = u_field(u_field(u_field(s1679, 0), 1), 3);
    u_value v_rest = u_field(s1679, 1);
    u_value t1681 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1682 = &t1681;
    u_value l1683 = f_parse_46_fields_named_2(v_f, v_rest);
    while (u_is_con(l1683, &n_list_46_Cons)) {
      u_value v_e = u_field(l1683, 0);
      if (u_truth(u_mk_bool(1))) {
        e1682 = u_append(e1682, f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t));
      }
      l1683 = u_field(l1683, 1);
    }
    u_value t1684;
    if (u_truth(f_list_46_contains_2(u_mk_bool(1), t1681))) {
      t1684 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t1684 = f_parse_46_differing_1(v_rest);
    }
    t1680 = t1684;
  }
  if (!t1680) {
    u_no_case(s1679);
  }
  return t1680;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s1685 = v_e;
  u_value t1686 = NULL;
  if (!t1686) {
    u_value v_t = u_field(u_field(s1685, 1), 3);
    t1686 = v_t;
  }
  if (!t1686) {
    u_no_case(s1685);
  }
  return t1686;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s1687 = v_fs;
  u_value t1688 = NULL;
  if ((!t1688) && u_is_con(s1687, &n_list_46_Nil)) {
    t1688 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1688) && u_is_con(s1687, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s1687, 0), 0);
    u_value v_d = u_field(u_field(s1687, 0), 1);
    u_value v_rest = u_field(s1687, 1);
    u_value t1689;
    if (u_truth(u_eq(v_g, v_f))) {
      t1689 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t1689 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t1688 = t1689;
  }
  if (!t1688) {
    u_no_case(s1687);
  }
  return t1688;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1690 = v_ks;
  u_value t1691 = NULL;
  if ((!t1691) && u_is_con(s1690, &n_list_46_Nil)) {
    t1691 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1691) && (u_is_con(s1690, &n_list_46_Cons) && u_is_con(u_field(u_field(s1690, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1690, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1690, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1690, 0), 1);
    u_value v_rest = u_field(s1690, 1);
    t1691 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1691) {
    u_no_case(s1690);
  }
  return t1691;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1692 = v_fs;
  u_value t1693 = NULL;
  if ((!t1693) && u_is_con(s1692, &n_list_46_Nil)) {
    t1693 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1693) && (u_is_con(s1692, &n_list_46_Cons) && u_is_con(u_field(u_field(s1692, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1692, 0), 0), 0);
    u_value v_t = u_field(u_field(s1692, 0), 1);
    u_value v_rest = u_field(s1692, 1);
    t1693 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1693) && (u_is_con(s1692, &n_list_46_Cons) && u_is_con(u_field(u_field(s1692, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1692, 1);
    t1693 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1693) {
    u_no_case(s1692);
  }
  return t1693;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1698 = f_parse_46_fields_of_1(v_ks);
  u_value t1697;
  {
    u_value v_fs = t1698;
    u_value t1694 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1695 = &t1694;
    u_value l1696 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1696, &n_list_46_Cons)) {
      u_value v_f = u_field(l1696, 0);
      if (u_truth(u_mk_bool(1))) {
        e1695 = u_append(e1695, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1696 = u_field(l1696, 1);
    }
    t1697 = t1694;
  }
  return t1697;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1699 = U_LIT("x");
  u_value t1700 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1701 = &t1700;
  u_value l1702 = v_ps;
  while (u_is_con(l1702, &n_list_46_Cons)) {
    u_value v_p = u_field(l1702, 0);
    if (u_truth(u_mk_bool(1))) {
      e1701 = u_append(e1701, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1702 = u_field(l1702, 1);
  }
  u_value t1710 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1699), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1711 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1700);
  u_value t1709;
  {
    u_value v_x = t1710;
    u_value v_owner = t1711;
    static struct u_object l__46__1703 = U_LIT(".");
    static struct u_object l_x_1704 = U_LIT("x");
    u_value t1705 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1706 = &t1705;
    u_value l1707 = v_es;
    while (u_is_con(l1707, &n_list_46_Cons)) {
      u_value v_e = u_field(l1707, 0);
      if (u_truth(u_mk_bool(1))) {
        e1706 = u_append(e1706, f_parse_46_field_case_1(v_e));
      }
      l1707 = u_field(l1707, 1);
    }
    u_value t1708;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1708 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1708 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1709 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1703, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1704), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1705, t1708)));
  }
  return t1709;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1712 = v_es;
  u_value t1713 = NULL;
  if ((!t1713) && u_is_con(s1712, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1712, 0), 1), 3);
    t1713 = v_t;
  }
  if (!t1713) {
    t1713 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1713) {
    u_no_case(s1712);
  }
  return t1713;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s1714 = v_e;
  u_value t1715 = NULL;
  if (!t1715) {
    u_value v_k = u_field(u_field(s1714, 1), 0);
    u_value v_i = u_field(u_field(s1714, 1), 1);
    u_value v_n = u_field(u_field(s1714, 1), 2);
    u_value t1716 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1717 = &t1716;
    u_value l1718 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1718, &n_list_46_Cons)) {
      u_value v_j = u_field(l1718, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1720;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1719 = U_LIT("y");
          t1720 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1719), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1720 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1717 = u_append(e1717, t1720);
      }
      l1718 = u_field(l1718, 1);
    }
    static struct u_object l_y_1721 = U_LIT("y");
    t1715 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1716), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1721), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1715) {
    u_no_case(s1714);
  }
  return t1715;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_MissingField_1722 = U_LIT("MissingField");
  static struct u_object l__46__1723 = U_LIT(".");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_ERaise, 2, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_MissingField_1722), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_t, u_concat(&l__46__1723, v_f)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_parse_46_updaters_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1728 = f_parse_46_fields_of_1(v_ks);
  u_value t1727;
  {
    u_value v_fs = t1728;
    u_value t1724 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1725 = &t1724;
    u_value l1726 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1726, &n_list_46_Cons)) {
      u_value v_f = u_field(l1726, 0);
      if (u_truth(u_mk_bool(1))) {
        e1725 = u_append(e1725, f_parse_46_updater_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1726 = u_field(l1726, 1);
    }
    t1727 = t1724;
  }
  return t1727;
}

u_value f_parse_46_updater_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1729 = U_LIT("x");
  u_value t1730 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1731 = &t1730;
  u_value l1732 = v_ps;
  while (u_is_con(l1732, &n_list_46_Cons)) {
    u_value v_p = u_field(l1732, 0);
    if (u_truth(u_mk_bool(1))) {
      e1731 = u_append(e1731, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1732 = u_field(l1732, 1);
  }
  u_value t1742 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1729), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1743 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1730);
  u_value t1741;
  {
    u_value v_x = t1742;
    u_value v_owner = t1743;
    static struct u_object l__46__1733 = U_LIT(".");
    static struct u_object l__58__61__1734 = U_LIT(":=");
    static struct u_object l_x_1735 = U_LIT("x");
    static struct u_object l_y_1736 = U_LIT("y");
    u_value t1737 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1738 = &t1737;
    u_value l1739 = v_es;
    while (u_is_con(l1739, &n_list_46_Cons)) {
      u_value v_e = u_field(l1739, 0);
      if (u_truth(u_mk_bool(1))) {
        e1738 = u_append(e1738, f_parse_46_update_case_1(v_e));
      }
      l1739 = u_field(l1739, 1);
    }
    u_value t1740;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1740 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1740 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1741 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1733, u_concat(v_f, &l__58__61__1734)))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1735), v_owner), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1736), f_parse_46_field_type_1(v_es)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), v_owner, u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1737, t1740)));
  }
  return t1741;
}

u_value f_parse_46_update_case_1(u_value v_e) {
  u_value s1744 = v_e;
  u_value t1745 = NULL;
  if (!t1745) {
    u_value v_k = u_field(u_field(s1744, 1), 0);
    u_value v_i = u_field(u_field(s1744, 1), 1);
    u_value v_n = u_field(u_field(s1744, 1), 2);
    u_value t1746 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1747 = &t1746;
    u_value l1748 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1748, &n_list_46_Cons)) {
      u_value v_j = u_field(l1748, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1750;
        if (u_truth(u_eq(v_j, v_i))) {
          t1750 = u_mk_con(&n_syntax_46_PWild, 0);
        } else {
          static struct u_object l_a_1749 = U_LIT("a");
          t1750 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l_a_1749, u_str_show(v_j))), u_mk_con(&n_syntax_46_THole, 0)));
        }
        e1747 = u_append(e1747, t1750);
      }
      l1748 = u_field(l1748, 1);
    }
    u_value t1751 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1752 = &t1751;
    u_value l1753 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1753, &n_list_46_Cons)) {
      u_value v_j = u_field(l1753, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1756;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1754 = U_LIT("y");
          t1756 = &l_y_1754;
        } else {
          static struct u_object l_a_1755 = U_LIT("a");
          t1756 = u_concat(&l_a_1755, u_str_show(v_j));
        }
        e1752 = u_append(e1752, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(t1756), u_mk_con(&n_syntax_46_THole, 0))));
      }
      l1753 = u_field(l1753, 1);
    }
    t1745 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1746), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_k, u_mk_con(&n_syntax_46_THole, 0))), t1751));
  }
  if (!t1745) {
    u_no_case(s1744);
  }
  return t1745;
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t1757;
  if (u_truth(u_eq(v_i, v_n))) {
    t1757 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1757 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t1757;
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
    u_value t1758 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1758;
    u_value t1760 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1760)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1759 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1759;
      }
      t1760 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1761 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1761), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1762 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1762), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1763 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1763;
  }
}

void p_parse_46_parse_procedure(u_value v_ns, u_value *r_c, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_ins = NULL;
    u_value v_outs = NULL;
    u_value v_refs = NULL;
    u_value v_first = NULL;
    u_value v_b = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_name(&v_n, r_ts);
    p_parse_46_type_params(&v_ps, r_ts);
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    u_value t1764 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1765 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1766 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1767 = u_mk_bool(1);
    v_ins = t1764;
    v_outs = t1765;
    v_refs = t1766;
    v_first = t1767;
    static struct u_object l_in_1768 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1768))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_ins, r_ts);
        u_value t1769 = u_mk_bool(0);
        v_first = t1769;
      }
    }
    static struct u_object l_out_1770 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1770, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_outs, r_ts);
        u_value t1771 = u_mk_bool(0);
        v_first = t1771;
      }
    }
    static struct u_object l_ref_1772 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1772, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    static struct u_object l__58__61__1773 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1773), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1774 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1774;
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
    u_value v_at = NULL;
    u_value t1775 = f_parse_46_Context_46_notations_1(v_cx);
    v_ns = t1775;
    u_value s1776 = f_parse_46_head_token_1(*r_ts);
    int d1777 = 0;
    if ((!d1777) && (u_is_con(s1776, &n_lex_46_TName) && u_equals(u_field(s1776, 0), u_mk_lit("import")))) {
      d1777 = 1;
      {
        u_value t1778 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1778;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Context_46_modules_1(v_cx))))) {
          {
            u_value t1779 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1779;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_cx, v_at, r_cs);
            u_value t1780 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1780;
          }
        }
      }
    }
    if ((!d1777) && u_is_con(s1776, &n_lex_46_TName)) {
      u_value v_w = u_field(s1776, 0);
      static struct u_object l_prefix_1782 = U_LIT("prefix");
      static struct u_object l_postfix_1783 = U_LIT("postfix");
      static struct u_object l_infix_1784 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1782, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1783, u_mk_con(&n_list_46_Cons, 2, &l_infix_1784, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1777 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1781 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1781;
        }
      }
    }
    if ((!d1777) && (u_is_con(s1776, &n_lex_46_TName) && u_equals(u_field(s1776, 0), u_mk_lit("type")))) {
      d1777 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1785 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1785), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1786 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1786;
      }
    }
    if ((!d1777) && (u_is_con(s1776, &n_lex_46_TName) && u_equals(u_field(s1776, 0), u_mk_lit("data")))) {
      d1777 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1777) && (u_is_con(s1776, &n_lex_46_TName) && u_equals(u_field(s1776, 0), u_mk_lit("function")))) {
      d1777 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1787 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1787;
      }
    }
    if ((!d1777) && (u_is_con(s1776, &n_lex_46_TName) && u_equals(u_field(s1776, 0), u_mk_lit("procedure")))) {
      d1777 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1788 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1788;
      }
    }
    if ((!d1777) && (u_is_con(s1776, &n_lex_46_TName) && u_equals(u_field(s1776, 0), u_mk_lit("eval")))) {
      d1777 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_e, r_ts);
        u_value t1789 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CEval, 1, v_e), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1789;
      }
    }
    if ((!d1777) && (u_is_con(s1776, &n_lex_46_TName) && u_equals(u_field(s1776, 0), u_mk_lit("run")))) {
      d1777 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1790 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1790), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1791 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1791;
      }
    }
    if ((!d1777) && (u_is_con(s1776, &n_lex_46_TName) && u_equals(u_field(s1776, 0), u_mk_lit("test")))) {
      d1777 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1792 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1792), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1793 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1793;
      }
    }
    if (!d1777) {
      d1777 = 1;
      {
        static struct u_object l_declaration_32_expe_1794 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1795 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1794, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1795)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1777) {
      u_no_case(s1776);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_cx) {
  {
    u_value s1796 = v_cs;
    int d1797 = 0;
    if ((!d1797) && u_is_con(s1796, &n_list_46_Nil)) {
      d1797 = 1;
      {
      }
    }
    if ((!d1797) && (u_is_con(s1796, &n_list_46_Cons) && u_is_con(u_field(s1796, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1796, 0), 0);
      u_value v_p = u_field(u_field(s1796, 0), 1);
      u_value v_ops = u_field(u_field(s1796, 0), 2);
      u_value v_more = u_field(s1796, 1);
      d1797 = 1;
      {
        u_value t1798 = f_parse_46_with_commands_2(*r_cx, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_cx = t1798;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1797) && (u_is_con(s1796, &n_list_46_Cons) && u_is_con(u_field(s1796, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1796, 0), 0);
      u_value v_ds = u_field(u_field(s1796, 0), 1);
      u_value v_more = u_field(s1796, 1);
      d1797 = 1;
      {
        u_value t1799 = f_parse_46_with_module_2(*r_cx, u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds));
        *r_cx = t1799;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1797) && (u_is_con(s1796, &n_list_46_Cons) && u_is_con(u_field(s1796, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1796, 0), 0);
      u_value v_more = u_field(s1796, 1);
      d1797 = 1;
      {
        u_value t1800 = f_parse_46_with_commands_2(*r_cx, f_parse_46_own_notations_2(*r_cx, v_n));
        *r_cx = t1800;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1797) && u_is_con(s1796, &n_list_46_Cons)) {
      u_value v_more = u_field(s1796, 1);
      d1797 = 1;
      {
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if (!d1797) {
      u_no_case(s1796);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_cx, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1801 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1801;
    u_value t1803 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1803)) {
      {
        p_parse_46_parse_command(*r_cx, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_cx);
        u_value t1802 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1802;
      }
      t1803 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1804 = f_list_46_reverse_1(v_rev);
    *r_cs = t1804;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1805 = v_dir;
    int d1806 = 0;
    if ((!d1806) && u_is_con(s1805, &n_option_46_Some)) {
      u_value v_d = u_field(s1805, 0);
      d1806 = 1;
      {
        {
          struct u_handler *h1809 = u_push();
          if (setjmp(h1809->env) == 0) {
            {
              static struct u_object l__47__1807 = U_LIT("/");
              static struct u_object l__46_u_1808 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1807, u_concat(v_n, &l__46_u_1808))), r_text);
            }
            u_pop(h1809);
          } else {
            u_pop(h1809);
            int d1810 = 0;
            if ((!d1810) && (u_has_type(h1809, "os.OsError") && u_is_con(u_caught(h1809), &n_os_46_OsError))) {
              d1810 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1810) {
              u_reraise(h1809);
            }
          }
        }
      }
    }
    if ((!d1806) && u_is_con(s1805, &n_option_46_None)) {
      d1806 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1806) {
      u_no_case(s1805);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1811 = u_push();
      if (setjmp(h1811->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1811);
      } else {
        u_pop(h1811);
        int d1812 = 0;
        if ((!d1812) && (u_has_type(h1811, "os.OsError") && u_is_con(u_caught(h1811), &n_os_46_OsError))) {
          d1812 = 1;
          {
            static struct u_object l_unknown_32_module_32__1813 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1813, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1812) {
          u_reraise(h1811);
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
        static struct u_object l_cyclic_32_import_32_of_1814 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1814, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Context_46_dir_1(v_cx), v_at, &v_text);
    u_value t1815 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_base_notations_1(v_cx), f_parse_46_Context_46_modules_1(v_cx), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Context_46_loading_1(v_cx)), f_parse_46_Context_46_base_1(v_cx), f_parse_46_Context_46_dir_1(v_cx));
    v_mcx = t1815;
    {
      struct u_handler *h1816 = u_push();
      if (setjmp(h1816->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mcx, &v_mts);
        }
        u_pop(h1816);
      } else {
        u_pop(h1816);
        int d1817 = 0;
        if ((!d1817) && (u_has_type(h1816, "lex.LexError") && u_is_con(u_caught(h1816), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1816), 0);
          u_value v_i = u_field(u_caught(h1816), 1);
          d1817 = 1;
          {
            static struct u_object l__32_in_32__1818 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1818, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1817) && (u_has_type(h1816, "parse.ParseError") && u_is_con(u_caught(h1816), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1816), 0);
          u_value v_i = u_field(u_caught(h1816), 1);
          d1817 = 1;
          {
            static struct u_object l__32_in_32__1819 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1819, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1817) {
          u_reraise(h1816);
        }
      }
    }
  }
}

u_value f_parse_46_base_notations_1(u_value v_cx) {
  u_value s1820 = f_parse_46_Context_46_base_1(v_cx);
  u_value t1821 = NULL;
  if ((!t1821) && u_is_con(s1820, &n_option_46_Some)) {
    u_value v_b = u_field(s1820, 0);
    t1821 = v_b;
  }
  if ((!t1821) && u_is_con(s1820, &n_option_46_None)) {
    t1821 = f_parse_46_Context_46_notations_1(v_cx);
  }
  if (!t1821) {
    u_no_case(s1820);
  }
  return t1821;
}

void p_parse_46_parse_text(u_value v_text, u_value v_cx, u_value *r_p) {
  {
    u_value v_ts = NULL;
    u_value v_cs = NULL;
    u_value v_after = NULL;
    u_value t1822 = v_cx;
    v_after = t1822;
    {
      struct u_handler *h1823 = u_push();
      if (setjmp(h1823->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1823);
      } else {
        u_pop(h1823);
        int d1824 = 0;
        if ((!d1824) && (u_has_type(h1823, "lex.LexError") && u_is_con(u_caught(h1823), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1823), 0);
          u_value v_i = u_field(u_caught(h1823), 1);
          d1824 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1824) {
          u_reraise(h1823);
        }
      }
    }
    p_parse_46_parse_commands(&v_cs, &v_after, &v_ts);
    u_value t1825 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1825;
  }
}

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p) {
  {
    u_value v_cx = NULL;
    u_value t1826 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    v_cx = t1826;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), &v_cx);
    p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_Context_46_notations_1(v_cx), f_parse_46_Context_46_modules_1(v_cx), u_mk_con(&n_list_46_Cons, 2, v_name, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_Some, 1, f_parse_46_Context_46_notations_1(v_cx)), v_dir), r_p);
  }
}

void p_parse_46_parse_library(u_value v_n, u_value *r_p) {
  {
    u_value v_text = NULL;
    u_value v_pre = NULL;
    p_parse_46_library_text(v_n, u_mk_int(0), &v_text);
    static struct u_object l_prelude_1827 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1827))) {
      {
        static struct u_object l_prelude_1828 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1828, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1829 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1829, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1830 = v_xs;
  u_value t1831 = NULL;
  if ((!t1831) && (u_is_con(s1830, &n_list_46_Cons) && u_is_con(u_field(s1830, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1830, 0);
    t1831 = v_x;
  }
  if ((!t1831) && u_is_con(s1830, &n_list_46_Cons)) {
    u_value v_more = u_field(s1830, 1);
    t1831 = f_parse_46_last_part_1(v_more);
  }
  if (!t1831) {
    static struct u_object l__1832 = U_LIT("");
    t1831 = &l__1832;
  }
  if (!t1831) {
    u_no_case(s1830);
  }
  return t1831;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1837 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1836;
  {
    u_value v_f = t1837;
    u_value t1834 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1834)) {
      static struct u_object l__46_u_1833 = U_LIT(".u");
      t1834 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1833);
    }
    u_value t1835;
    if (u_truth(t1834)) {
      t1835 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1835 = v_f;
    }
    t1836 = t1835;
  }
  return t1836;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1842 = u_str_split(v_path, u_mk_char(47));
  u_value t1841;
  {
    u_value v_parts = t1842;
    u_value t1840;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1838 = U_LIT(".");
      t1840 = &l__46__1838;
    } else {
      static struct u_object l__47__1839 = U_LIT("/");
      t1840 = u_str_join(&l__47__1839, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1841 = t1840;
  }
  return t1841;
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
    static struct u_object l_prelude_1843 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1843, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1844 = u_mk_tuple(2, v_cs, v_ds);
    int d1845 = 0;
    if ((!d1845) && (u_is_con(u_field(s1844, 0), &n_list_46_Cons) && u_is_con(u_field(s1844, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1844, 0), 0);
      u_value v_more = u_field(u_field(s1844, 0), 1);
      u_value v_d = u_field(u_field(s1844, 1), 0);
      u_value v_rest = u_field(u_field(s1844, 1), 1);
      d1845 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1845) {
      d1845 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1845) {
      u_no_case(s1844);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1846 = v_paths;
    int d1847 = 0;
    if ((!d1847) && u_is_con(s1846, &n_list_46_Nil)) {
      d1847 = 1;
      {
      }
    }
    if ((!d1847) && u_is_con(s1846, &n_list_46_Cons)) {
      u_value v_path = u_field(s1846, 0);
      u_value v_more = u_field(s1846, 1);
      d1847 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1847) {
      u_no_case(s1846);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1848 = v_paths;
    int d1849 = 0;
    if ((!d1849) && u_is_con(s1848, &n_list_46_Nil)) {
      d1849 = 1;
      {
      }
    }
    if ((!d1849) && u_is_con(s1848, &n_list_46_Cons)) {
      u_value v_path = u_field(s1848, 0);
      u_value v_more = u_field(s1848, 1);
      d1849 = 1;
      {
        u_value t1850 = u_mk_bool(0);
        v_failed = t1850;
        {
          struct u_handler *h1851 = u_push();
          if (setjmp(h1851->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1851);
          } else {
            u_pop(h1851);
            int d1852 = 0;
            if ((!d1852) && (u_has_type(h1851, "parse.ParseError") && u_is_con(u_caught(h1851), &n_parse_46_ParseError))) {
              d1852 = 1;
              {
                u_value t1853 = u_mk_bool(1);
                v_failed = t1853;
              }
            }
            if (!d1852) {
              u_reraise(h1851);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1849) {
      u_no_case(s1848);
    }
  }
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s1854 = v_x;
  u_value t1855 = NULL;
  if ((!t1855) && u_is_con(s1854, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1854, 0);
    t1855 = v_y;
  }
  if (!t1855) {
    u_no_case(s1854);
  }
  return t1855;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s1856 = v_x;
  u_value t1857 = NULL;
  if ((!t1857) && u_is_con(s1856, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1856, 1);
    t1857 = v_y;
  }
  if (!t1857) {
    u_no_case(s1856);
  }
  return t1857;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s1858 = v_x;
  u_value t1859 = NULL;
  if ((!t1859) && u_is_con(s1858, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1858, 2);
    t1859 = v_y;
  }
  if (!t1859) {
    u_no_case(s1858);
  }
  return t1859;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s1860 = v_x;
  u_value t1861 = NULL;
  if ((!t1861) && u_is_con(s1860, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1860, 3);
    t1861 = v_y;
  }
  if (!t1861) {
    u_no_case(s1860);
  }
  return t1861;
}

u_value f_compile_46_Globals_46_funs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1862 = v_x;
  u_value t1863 = NULL;
  if ((!t1863) && u_is_con(s1862, &n_compile_46_Globals)) {
    u_value v_a1 = u_field(s1862, 1);
    u_value v_a2 = u_field(s1862, 2);
    u_value v_a3 = u_field(s1862, 3);
    t1863 = u_mk_con(&n_compile_46_Globals, 4, v_y, v_a1, v_a2, v_a3);
  }
  if (!t1863) {
    u_no_case(s1862);
  }
  return t1863;
}

u_value f_compile_46_Globals_46_constrs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1864 = v_x;
  u_value t1865 = NULL;
  if ((!t1865) && u_is_con(s1864, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1864, 0);
    u_value v_a2 = u_field(s1864, 2);
    u_value v_a3 = u_field(s1864, 3);
    t1865 = u_mk_con(&n_compile_46_Globals, 4, v_a0, v_y, v_a2, v_a3);
  }
  if (!t1865) {
    u_no_case(s1864);
  }
  return t1865;
}

u_value f_compile_46_Globals_46_arities_58__61__2(u_value v_x, u_value v_y) {
  u_value s1866 = v_x;
  u_value t1867 = NULL;
  if ((!t1867) && u_is_con(s1866, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1866, 0);
    u_value v_a1 = u_field(s1866, 1);
    u_value v_a3 = u_field(s1866, 3);
    t1867 = u_mk_con(&n_compile_46_Globals, 4, v_a0, v_a1, v_y, v_a3);
  }
  if (!t1867) {
    u_no_case(s1866);
  }
  return t1867;
}

u_value f_compile_46_Globals_46_fields_58__61__2(u_value v_x, u_value v_y) {
  u_value s1868 = v_x;
  u_value t1869 = NULL;
  if ((!t1869) && u_is_con(s1868, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1868, 0);
    u_value v_a1 = u_field(s1868, 1);
    u_value v_a2 = u_field(s1868, 2);
    t1869 = u_mk_con(&n_compile_46_Globals, 4, v_a0, v_a1, v_a2, v_y);
  }
  if (!t1869) {
    u_no_case(s1868);
  }
  return t1869;
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
  static struct u_object l_u_value_1870 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1870);
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
  static struct u_object l_u_value_1871 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1871));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t1872 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1873 = &t1872;
  u_value l1874 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l1874, &n_list_46_Cons)) {
    u_value v_c = u_field(l1874, 0);
    if (u_truth(u_mk_bool(1))) {
      e1873 = u_append(e1873, f_compile_46_mangle_char_1(v_c));
    }
    l1874 = u_field(l1874, 1);
  }
  return u_string(f_list_46_concat_1(t1872));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t1875 = u_is_letter(v_c);
  if (!u_truth(t1875)) {
    t1875 = u_is_digit(v_c);
  }
  u_value t1876 = t1875;
  if (!u_truth(t1876)) {
    t1876 = u_eq(v_c, u_mk_char(95));
  }
  u_value t1879;
  if (u_truth(t1876)) {
    t1879 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___1877 = U_LIT("_");
    static struct u_object l___1878 = U_LIT("_");
    t1879 = u_chars(u_concat(&l___1877, u_concat(u_str_show(u_ord(v_c)), &l___1878)));
  }
  return t1879;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__1880 = U_LIT("f_");
  static struct u_object l___1881 = U_LIT("_");
  return u_concat(&l_f__1880, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___1881, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__1882 = U_LIT("p_");
  return u_concat(&l_p__1882, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__1883 = U_LIT("v_");
  return u_concat(&l_v__1883, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__1884 = U_LIT("r_");
  return u_concat(&l_r__1884, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__1885 = U_LIT("+");
  static struct u_object l_u_add_1886 = U_LIT("u_add");
  static struct u_object l__45__1887 = U_LIT("-");
  static struct u_object l_u_sub_1888 = U_LIT("u_sub");
  static struct u_object l__42__1889 = U_LIT("*");
  static struct u_object l_u_mul_1890 = U_LIT("u_mul");
  static struct u_object l__47__1891 = U_LIT("/");
  static struct u_object l_u_div_1892 = U_LIT("u_div");
  static struct u_object l__37__1893 = U_LIT("%");
  static struct u_object l_u_mod_1894 = U_LIT("u_mod");
  static struct u_object l__60__1895 = U_LIT("<");
  static struct u_object l_u_lt_1896 = U_LIT("u_lt");
  static struct u_object l__61__61__1897 = U_LIT("==");
  static struct u_object l_u_eq_1898 = U_LIT("u_eq");
  static struct u_object l_compare_1899 = U_LIT("compare");
  static struct u_object l_u_compare_1900 = U_LIT("u_compare");
  static struct u_object l_chars_1901 = U_LIT("chars");
  static struct u_object l_u_chars_1902 = U_LIT("u_chars");
  static struct u_object l_string_1903 = U_LIT("string");
  static struct u_object l_u_string_1904 = U_LIT("u_string");
  static struct u_object l_ord_1905 = U_LIT("ord");
  static struct u_object l_u_ord_1906 = U_LIT("u_ord");
  static struct u_object l_chr_1907 = U_LIT("chr");
  static struct u_object l_u_chr_1908 = U_LIT("u_chr");
  static struct u_object l__94__1909 = U_LIT("^");
  static struct u_object l_u_concat_1910 = U_LIT("u_concat");
  static struct u_object l_str_size_1911 = U_LIT("str_size");
  static struct u_object l_u_str_size_1912 = U_LIT("u_str_size");
  static struct u_object l_str_join_1913 = U_LIT("str_join");
  static struct u_object l_u_str_join_1914 = U_LIT("u_str_join");
  static struct u_object l_str_slice_1915 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_1916 = U_LIT("u_str_slice");
  static struct u_object l_str_split_1917 = U_LIT("str_split");
  static struct u_object l_u_str_split_1918 = U_LIT("u_str_split");
  static struct u_object l_str_get_1919 = U_LIT("str_get");
  static struct u_object l_u_str_get_1920 = U_LIT("u_str_get");
  static struct u_object l_str_contains_1921 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_1922 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_1923 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_1924 = U_LIT("u_str_compare");
  static struct u_object l_str_show_1925 = U_LIT("str_show");
  static struct u_object l_u_str_show_1926 = U_LIT("u_str_show");
  static struct u_object l_str_quote_1927 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_1928 = U_LIT("u_str_quote");
  static struct u_object l_str_number_1929 = U_LIT("str_number");
  static struct u_object l_u_str_number_1930 = U_LIT("u_str_number");
  static struct u_object l_is_digit_1931 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_1932 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_1933 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_1934 = U_LIT("u_is_letter");
  static struct u_object l_is_space_1935 = U_LIT("is_space");
  static struct u_object l_u_is_space_1936 = U_LIT("u_is_space");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__1885), u_mk_tuple(2, &l_u_add_1886, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__1887), u_mk_tuple(2, &l_u_sub_1888, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__1889), u_mk_tuple(2, &l_u_mul_1890, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__1891), u_mk_tuple(2, &l_u_div_1892, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__1893), u_mk_tuple(2, &l_u_mod_1894, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__1895), u_mk_tuple(2, &l_u_lt_1896, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__1897), u_mk_tuple(2, &l_u_eq_1898, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_1899), u_mk_tuple(2, &l_u_compare_1900, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_1901), u_mk_tuple(2, &l_u_chars_1902, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_1903), u_mk_tuple(2, &l_u_string_1904, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_1905), u_mk_tuple(2, &l_u_ord_1906, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_1907), u_mk_tuple(2, &l_u_chr_1908, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__1909), u_mk_tuple(2, &l_u_concat_1910, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_1911), u_mk_tuple(2, &l_u_str_size_1912, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_1913), u_mk_tuple(2, &l_u_str_join_1914, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_1915), u_mk_tuple(2, &l_u_str_slice_1916, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_1917), u_mk_tuple(2, &l_u_str_split_1918, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_1919), u_mk_tuple(2, &l_u_str_get_1920, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_1921), u_mk_tuple(2, &l_u_str_contains_1922, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_1923), u_mk_tuple(2, &l_u_str_compare_1924, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_1925), u_mk_tuple(2, &l_u_str_show_1926, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_1927), u_mk_tuple(2, &l_u_str_quote_1928, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_1929), u_mk_tuple(2, &l_u_str_number_1930, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_1931), u_mk_tuple(2, &l_u_is_digit_1932, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_1933), u_mk_tuple(2, &l_u_is_letter_1934, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_1935), u_mk_tuple(2, &l_u_is_space_1936, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)))))))))))))))))))))))))));
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__1937 = U_LIT("&");
  static struct u_object l_n__1938 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__1937, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__1938, f_compile_46_mangle_1(v_n))));
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
  cache = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_nil_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_cons_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_none_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_some_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_os_error_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_division_by_zero_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_no_index_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_no_character_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_missing_field_name_0(), u_mk_con(&n_list_46_Nil, 0))))))))));
  return cache;
}

u_value f_compile_46_descriptor_decl_2(u_value v_n, u_value v_i) {
  u_value s1939 = v_n;
  u_value t1940 = NULL;
  if ((!t1940) && (u_is_con(s1939, &n_syntax_46_Name) && u_is_con(u_field(s1939, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s1939, 0), 0);
    u_value v_b = u_field(s1939, 1);
    static struct u_object l_const_32_struct_32_u_n_1941 = U_LIT("const struct u_name");
    static struct u_object l_n__1942 = U_LIT("n_");
    t1940 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1941), u_concat(&l_n__1942, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t1940) && (u_is_con(s1939, &n_syntax_46_Name) && u_is_con(u_field(s1939, 0), &n_option_46_None))) {
    u_value v_b = u_field(s1939, 1);
    static struct u_object l_const_32_struct_32_u_n_1943 = U_LIT("const struct u_name");
    static struct u_object l_n__1944 = U_LIT("n_");
    static struct u_object l_NULL_1945 = U_LIT("NULL");
    t1940 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1943), u_concat(&l_n__1944, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1945), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t1940) {
    u_no_case(s1939);
  }
  return t1940;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s1946 = v_cs;
  u_value t1947 = NULL;
  if ((!t1947) && u_is_con(s1946, &n_list_46_Nil)) {
    t1947 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1947) && (u_is_con(s1946, &n_list_46_Cons) && u_is_con(u_field(s1946, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s1946, 0), 2);
    u_value v_rest = u_field(s1946, 1);
    u_value t1948 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1949 = &t1948;
    u_value l1950 = v_ks;
    while (u_is_con(l1950, &n_list_46_Cons)) {
      u_value v_k = u_field(l1950, 0);
      if (u_truth(u_mk_bool(1))) {
        e1949 = u_append(e1949, f_syntax_46_Constr_46_name_1(v_k));
      }
      l1950 = u_field(l1950, 1);
    }
    t1947 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t1948, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t1947) && u_is_con(s1946, &n_list_46_Cons)) {
    u_value v_rest = u_field(s1946, 1);
    t1947 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t1947) {
    u_no_case(s1946);
  }
  return t1947;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s1951 = v_ks;
  u_value t1952 = NULL;
  if ((!t1952) && u_is_con(s1951, &n_list_46_Nil)) {
    t1952 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1952) && u_is_con(s1951, &n_list_46_Cons)) {
    u_value v_k = u_field(s1951, 0);
    u_value v_rest = u_field(s1951, 1);
    u_value t1953;
    if (u_truth(f_list_46_contains_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t1953 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t1953 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t1952 = t1953;
  }
  if (!t1952) {
    u_no_case(s1951);
  }
  return t1952;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t1954 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1955 = &t1954;
  u_value l1956 = v_xs;
  while (u_is_con(l1956, &n_list_46_Cons)) {
    u_value v_x = u_field(l1956, 0);
    if (u_truth(u_mk_bool(1))) {
      e1955 = u_append(e1955, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l1956 = u_field(l1956, 1);
  }
  return t1954;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__1957 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__1957, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s1958 = f_list_46_lookup_2(v_x, v_env);
  u_value t1959 = NULL;
  if ((!t1959) && u_is_con(s1958, &n_option_46_Some)) {
    u_value v_c = u_field(s1958, 0);
    t1959 = v_c;
  }
  if (!t1959) {
    t1959 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t1959) {
    u_no_case(s1958);
  }
  return t1959;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s1960 = v_c;
  u_value t1961 = NULL;
  if ((!t1961) && (u_is_con(s1960, &n_c_46_CeUnary) && u_equals(u_field(s1960, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s1960, 1);
    t1961 = v_p;
  }
  if (!t1961) {
    static struct u_object l__38__1962 = U_LIT("&");
    t1961 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1962, v_c);
  }
  if (!t1961) {
    u_no_case(s1960);
  }
  return t1961;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s1963 = v_t;
  u_value t1964 = NULL;
  if ((!t1964) && (u_is_con(s1963, &n_syntax_46_TSort) && u_is_con(u_field(s1963, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s1963, 0);
    t1964 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t1964) && u_is_con(s1963, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s1963, 0);
    u_value v_args = u_field(s1963, 1);
    static struct u_object l__91__1965 = U_LIT("[");
    static struct u_object l__44__32__1966 = U_LIT(", ");
    u_value t1967 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1968 = &t1967;
    u_value l1969 = v_args;
    while (u_is_con(l1969, &n_list_46_Cons)) {
      u_value v_a = u_field(l1969, 0);
      if (u_truth(u_mk_bool(1))) {
        e1968 = u_append(e1968, f_compile_46_type_key_1(v_a));
      }
      l1969 = u_field(l1969, 1);
    }
    static struct u_object l__93__1970 = U_LIT("]");
    t1964 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__1965, u_concat(u_str_join(&l__44__32__1966, t1967), &l__93__1970)));
  }
  if ((!t1964) && u_is_con(s1963, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1963, 0);
    u_value v_r = u_field(s1963, 1);
    static struct u_object l__40__1971 = U_LIT("(");
    static struct u_object l__44__32__1972 = U_LIT(", ");
    u_value t1973 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1974 = &t1973;
    u_value l1975 = v_args;
    while (u_is_con(l1975, &n_list_46_Cons)) {
      u_value v_a = u_field(l1975, 0);
      if (u_truth(u_mk_bool(1))) {
        e1974 = u_append(e1974, f_compile_46_type_key_1(v_a));
      }
      l1975 = u_field(l1975, 1);
    }
    static struct u_object l__41__32__45__62__32__1976 = U_LIT(") -> ");
    t1964 = u_concat(&l__40__1971, u_concat(u_str_join(&l__44__32__1972, t1973), u_concat(&l__41__32__45__62__32__1976, f_compile_46_type_key_1(v_r))));
  }
  if ((!t1964) && u_is_con(s1963, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1963, 0);
    static struct u_object l__40__1977 = U_LIT("(");
    static struct u_object l__32__42__32__1978 = U_LIT(" * ");
    u_value t1979 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1980 = &t1979;
    u_value l1981 = v_ts;
    while (u_is_con(l1981, &n_list_46_Cons)) {
      u_value v_a = u_field(l1981, 0);
      if (u_truth(u_mk_bool(1))) {
        e1980 = u_append(e1980, f_compile_46_type_key_1(v_a));
      }
      l1981 = u_field(l1981, 1);
    }
    static struct u_object l__41__1982 = U_LIT(")");
    t1964 = u_concat(&l__40__1977, u_concat(u_str_join(&l__32__42__32__1978, t1979), &l__41__1982));
  }
  if (!t1964) {
    static struct u_object l__63__1983 = U_LIT("?");
    t1964 = &l__63__1983;
  }
  if (!t1964) {
    u_no_case(s1963);
  }
  return t1964;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t1984 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1985 = &t1984;
  u_value l1986 = v_cs;
  while (u_is_con(l1986, &n_list_46_Cons)) {
    u_value v_c = u_field(l1986, 0);
    if (u_truth(u_mk_bool(1))) {
      e1985 = u_append(e1985, f_compile_46_fun_arities_1(v_c));
    }
    l1986 = u_field(l1986, 1);
  }
  u_value t1987 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1988 = &t1987;
  u_value l1989 = v_cs;
  while (u_is_con(l1989, &n_list_46_Cons)) {
    u_value v_c = u_field(l1989, 0);
    if (u_truth(u_mk_bool(1))) {
      e1988 = u_append(e1988, f_compile_46_constr_arities_1(v_c));
    }
    l1989 = u_field(l1989, 1);
  }
  u_value t1991 = f_list_46_concat_1(t1984);
  u_value t1992 = f_list_46_concat_1(t1987);
  u_value t1990;
  {
    u_value v_fs = t1991;
    u_value v_ks = t1992;
    t1990 = u_mk_con(&n_compile_46_Globals, 4, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()));
  }
  return t1990;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s1993 = v_fs;
  u_value t1994 = NULL;
  if ((!t1994) && u_is_con(s1993, &n_list_46_Nil)) {
    t1994 = v_m;
  }
  if ((!t1994) && u_is_con(s1993, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1993, 0), 0);
    u_value v_k = u_field(u_field(s1993, 0), 1);
    u_value v_rest = u_field(s1993, 1);
    u_value s1995 = f_map_46_get_2(v_f, v_m);
    u_value t1996 = NULL;
    if ((!t1996) && u_is_con(s1995, &n_option_46_Some)) {
      u_value v_ks = u_field(s1995, 0);
      t1996 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t1996) && u_is_con(s1995, &n_option_46_None)) {
      t1996 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t1996) {
      u_no_case(s1995);
    }
    t1994 = t1996;
  }
  if (!t1994) {
    u_no_case(s1993);
  }
  return t1994;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s1997 = v_gl;
  u_value t1998 = NULL;
  if ((!t1998) && u_is_con(s1997, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s1997, 2);
    u_value s1999 = f_map_46_get_2(v_n, v_fm);
    u_value t2000 = NULL;
    if ((!t2000) && u_is_con(s1999, &n_option_46_Some)) {
      u_value v_ks = u_field(s1999, 0);
      t2000 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t2000) && u_is_con(s1999, &n_option_46_None)) {
      t2000 = u_mk_bool(0);
    }
    if (!t2000) {
      u_no_case(s1999);
    }
    t1998 = t2000;
  }
  if (!t1998) {
    u_no_case(s1997);
  }
  return t1998;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s2001 = v_gl;
  u_value t2002 = NULL;
  if ((!t2002) && u_is_con(s2001, &n_compile_46_Globals)) {
    u_value v_km = u_field(s2001, 3);
    t2002 = f_map_46_get_2(v_n, v_km);
  }
  if (!t2002) {
    u_no_case(s2001);
  }
  return t2002;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s2003 = v_c;
  u_value t2004 = NULL;
  if ((!t2004) && u_is_con(s2003, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s2003, 0);
    u_value v_xss = u_field(s2003, 2);
    t2004 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2004) {
    t2004 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2004) {
    u_no_case(s2003);
  }
  return t2004;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s2005 = v_c;
  u_value t2006 = NULL;
  if ((!t2006) && u_is_con(s2005, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s2005, 2);
    u_value t2007 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2008 = &t2007;
    u_value l2009 = v_ks;
    while (u_is_con(l2009, &n_list_46_Cons)) {
      u_value v_k = u_field(l2009, 0);
      if (u_truth(u_mk_bool(1))) {
        e2008 = u_append(e2008, f_compile_46_constr_arity_1(v_k));
      }
      l2009 = u_field(l2009, 1);
    }
    t2006 = t2007;
  }
  if (!t2006) {
    t2006 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2006) {
    u_no_case(s2005);
  }
  return t2006;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s2010 = v_k;
  u_value t2011 = NULL;
  if ((!t2011) && u_is_con(s2010, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s2010, 0);
    u_value v_ts = u_field(s2010, 1);
    t2011 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t2011) {
    u_no_case(s2010);
  }
  return t2011;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s2012 = v_xs;
  u_value t2013 = NULL;
  if ((!t2013) && u_is_con(s2012, &n_list_46_Nil)) {
    t2013 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2013) && u_is_con(s2012, &n_list_46_Cons)) {
    u_value v_x = u_field(s2012, 0);
    u_value v_rest = u_field(s2012, 1);
    t2013 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t2013) {
    u_no_case(s2012);
  }
  return t2013;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_2014 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_2014, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_2015 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_2015, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s2016 = v_cs;
  u_value t2017 = NULL;
  if ((!t2017) && u_is_con(s2016, &n_list_46_Nil)) {
    t2017 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t2017) && (u_is_con(s2016, &n_list_46_Cons) && u_is_con(u_field(s2016, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s2016, 0);
    t2017 = v_d;
  }
  if ((!t2017) && u_is_con(s2016, &n_list_46_Cons)) {
    u_value v_d = u_field(s2016, 0);
    u_value v_rest = u_field(s2016, 1);
    static struct u_object l__38__38__2018 = U_LIT("&&");
    t2017 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__2018, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t2017) {
    u_no_case(s2016);
  }
  return t2017;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s2019 = v_l;
  u_value t2020 = NULL;
  if ((!t2020) && u_is_con(s2019, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_2021 = U_LIT("u_mk_tuple");
    t2020 = f_c_46_c_call_2(&l_u_mk_tuple_2021, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2020) && u_is_con(s2019, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s2019, 0);
    static struct u_object l_u_mk_bool_2022 = U_LIT("u_mk_bool");
    u_value t2023;
    if (u_truth(v_b)) {
      t2023 = u_mk_int(1);
    } else {
      t2023 = u_mk_int(0);
    }
    t2020 = f_c_46_c_call_2(&l_u_mk_bool_2022, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t2023), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2020) && u_is_con(s2019, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s2019, 0);
    u_value t2024 = u_lt(f_prelude_46__45__1(u_mk_int_str("1000000000000000000")), v_n);
    if (u_truth(t2024)) {
      t2024 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t2027;
    if (u_truth(t2024)) {
      static struct u_object l_u_mk_int_2025 = U_LIT("u_mk_int");
      t2027 = f_c_46_c_call_2(&l_u_mk_int_2025, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_2026 = U_LIT("u_mk_int_str");
      t2027 = f_c_46_c_call_2(&l_u_mk_int_str_2026, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t2020 = t2027;
  }
  if ((!t2020) && u_is_con(s2019, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s2019, 0);
    static struct u_object l_u_mk_lit_2028 = U_LIT("u_mk_lit");
    t2020 = f_c_46_c_call_2(&l_u_mk_lit_2028, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2020) && u_is_con(s2019, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s2019, 0);
    static struct u_object l_u_mk_char_2029 = U_LIT("u_mk_char");
    t2020 = f_c_46_c_call_2(&l_u_mk_char_2029, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2020) {
    u_no_case(s2019);
  }
  return t2020;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s2030 = v_p;
  u_value t2031 = NULL;
  if ((!t2031) && u_is_con(s2030, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s2030, 0);
    static struct u_object l_u_equals_2032 = U_LIT("u_equals");
    t2031 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_2032, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2031) && u_is_con(s2030, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s2030, 0);
    u_value v_ps = u_field(s2030, 1);
    static struct u_object l_u_is_con_2033 = U_LIT("u_is_con");
    t2031 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_2033, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t2031) && u_is_con(s2030, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2030, 0);
    t2031 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2031) {
    t2031 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2031) {
    u_no_case(s2030);
  }
  return t2031;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s2034 = v_ps;
  u_value t2035 = NULL;
  if ((!t2035) && u_is_con(s2034, &n_list_46_Nil)) {
    t2035 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2035) && u_is_con(s2034, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2034, 0), 0);
    u_value v_i = u_field(u_field(s2034, 0), 1);
    u_value v_rest = u_field(s2034, 1);
    t2035 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t2035) {
    u_no_case(s2034);
  }
  return t2035;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s2036 = v_p;
  u_value t2037 = NULL;
  if ((!t2037) && (u_is_con(s2036, &n_syntax_46_PVar) && u_is_con(u_field(s2036, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2036, 0), 0);
    t2037 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2037) && u_is_con(s2036, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2036, 1);
    t2037 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t2037) && u_is_con(s2036, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2036, 0);
    t2037 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2037) {
    t2037 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2037) {
    u_no_case(s2036);
  }
  return t2037;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s2038 = v_ps;
  u_value t2039 = NULL;
  if ((!t2039) && u_is_con(s2038, &n_list_46_Nil)) {
    t2039 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2039) && u_is_con(s2038, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2038, 0), 0);
    u_value v_i = u_field(u_field(s2038, 0), 1);
    u_value v_rest = u_field(s2038, 1);
    t2039 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t2039) {
    u_no_case(s2038);
  }
  return t2039;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s2040 = v_p;
  u_value t2041 = NULL;
  if ((!t2041) && (u_is_con(s2040, &n_syntax_46_PVar) && u_is_con(u_field(s2040, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2040, 0), 0);
    t2041 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2041) && u_is_con(s2040, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2040, 1);
    u_value t2042 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2043 = &t2042;
    u_value l2044 = v_ps;
    while (u_is_con(l2044, &n_list_46_Cons)) {
      u_value v_q = u_field(l2044, 0);
      if (u_truth(u_mk_bool(1))) {
        e2043 = u_append(e2043, f_compile_46_pattern_names_1(v_q));
      }
      l2044 = u_field(l2044, 1);
    }
    t2041 = f_list_46_concat_1(t2042);
  }
  if ((!t2041) && u_is_con(s2040, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2040, 0);
    u_value t2045 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2046 = &t2045;
    u_value l2047 = v_ps;
    while (u_is_con(l2047, &n_list_46_Cons)) {
      u_value v_q = u_field(l2047, 0);
      if (u_truth(u_mk_bool(1))) {
        e2046 = u_append(e2046, f_compile_46_pattern_names_1(v_q));
      }
      l2047 = u_field(l2047, 1);
    }
    t2041 = f_list_46_concat_1(t2045);
  }
  if (!t2041) {
    t2041 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2041) {
    u_no_case(s2040);
  }
  return t2041;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s2048 = v_e;
  u_value t2049 = NULL;
  if ((!t2049) && u_is_con(s2048, &n_syntax_46_ELit)) {
    t2049 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2049) && (u_is_con(s2048, &n_syntax_46_EVar) && u_is_con(u_field(s2048, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2048, 0), 0);
    u_value t2050;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t2050 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t2050 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t2049 = t2050;
  }
  if ((!t2049) && u_is_con(s2048, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s2048, 0);
    u_value v_args = u_field(s2048, 1);
    u_value t2051 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2052 = &t2051;
    u_value l2053 = v_args;
    while (u_is_con(l2053, &n_list_46_Cons)) {
      u_value v_a = u_field(l2053, 0);
      if (u_truth(u_mk_bool(1))) {
        e2052 = u_append(e2052, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2053 = u_field(l2053, 1);
    }
    t2049 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t2051));
  }
  if ((!t2049) && u_is_con(s2048, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s2048, 0);
    u_value v_b = u_field(s2048, 1);
    t2049 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t2049) && u_is_con(s2048, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s2048, 0);
    u_value v_b = u_field(s2048, 1);
    u_value t2054 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2055 = &t2054;
    u_value l2056 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l2056, &n_list_46_Cons)) {
      u_value v_a = u_field(l2056, 0);
      if (u_truth(u_mk_bool(1))) {
        e2055 = u_append(e2055, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2056 = u_field(l2056, 1);
    }
    t2049 = f_list_46__43__43__2(f_list_46_concat_1(t2054), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t2049) && u_is_con(s2048, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s2048, 0);
    u_value v_l = u_field(s2048, 1);
    u_value v_r = u_field(s2048, 2);
    t2049 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t2049) && u_is_con(s2048, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s2048, 0);
    u_value v_cs = u_field(s2048, 1);
    u_value t2057 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2058 = &t2057;
    u_value l2059 = v_cs;
    while (u_is_con(l2059, &n_list_46_Cons)) {
      u_value v_b = u_field(l2059, 0);
      if (u_truth(u_mk_bool(1))) {
        e2058 = u_append(e2058, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l2059 = u_field(l2059, 1);
    }
    t2049 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2057));
  }
  if ((!t2049) && u_is_con(s2048, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s2048, 0);
    u_value v_xs = u_field(s2048, 1);
    u_value v_ls = u_field(s2048, 2);
    u_value v_c = u_field(s2048, 3);
    u_value t2060 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2061 = &t2060;
    u_value l2062 = v_ls;
    while (u_is_con(l2062, &n_list_46_Cons)) {
      u_value v_l = u_field(l2062, 0);
      if (u_truth(u_mk_bool(1))) {
        e2061 = u_append(e2061, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2062 = u_field(l2062, 1);
    }
    t2049 = f_list_46__43__43__2(f_list_46_concat_1(t2060), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t2049) && u_is_con(s2048, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s2048, 0);
    u_value t2063 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2064 = &t2063;
    u_value l2065 = v_es;
    while (u_is_con(l2065, &n_list_46_Cons)) {
      u_value v_a = u_field(l2065, 0);
      if (u_truth(u_mk_bool(1))) {
        e2064 = u_append(e2064, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2065 = u_field(l2065, 1);
    }
    t2049 = f_list_46_concat_1(t2063);
  }
  if ((!t2049) && u_is_con(s2048, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s2048, 0);
    t2049 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if (!t2049) {
    u_no_case(s2048);
  }
  return t2049;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s2066 = v_b;
  u_value t2067 = NULL;
  if ((!t2067) && (u_is_con(s2066, &n_syntax_46_Branch) && u_is_con(u_field(s2066, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2066, 0);
    u_value v_g = u_field(u_field(s2066, 1), 0);
    u_value v_e = u_field(s2066, 2);
    t2067 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2067) && (u_is_con(s2066, &n_syntax_46_Branch) && u_is_con(u_field(s2066, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2066, 0);
    u_value v_e = u_field(s2066, 2);
    t2067 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2067) {
    u_no_case(s2066);
  }
  return t2067;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s2068 = v_xss;
  u_value t2069 = NULL;
  if ((!t2069) && u_is_con(s2068, &n_list_46_Nil)) {
    t2069 = v_b;
  }
  if ((!t2069) && u_is_con(s2068, &n_list_46_Cons)) {
    u_value v_xs = u_field(s2068, 0);
    u_value v_rest = u_field(s2068, 1);
    t2069 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t2069) {
    u_no_case(s2068);
  }
  return t2069;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value s2070 = *r_g;
    int d2071 = 0;
    if ((!d2071) && u_is_con(s2070, &n_compile_46_Gen)) {
      u_value v_n = u_field(s2070, 0);
      u_value v_ds = u_field(s2070, 1);
      d2071 = 1;
      {
        u_value t2072 = u_concat(v_stem, u_str_show(v_n));
        u_value t2073 = u_mk_con(&n_compile_46_Gen, 2, u_add(v_n, u_mk_int(1)), v_ds);
        *r_x = t2072;
        *r_g = t2073;
      }
    }
    if (!d2071) {
      u_no_case(s2070);
    }
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value s2074 = *r_g;
    int d2075 = 0;
    if ((!d2075) && u_is_con(s2074, &n_compile_46_Gen)) {
      u_value v_n = u_field(s2074, 0);
      u_value v_ds = u_field(s2074, 1);
      d2075 = 1;
      {
        u_value t2076 = u_mk_con(&n_compile_46_Gen, 2, v_n, f_list_46__43__43__2(v_ds, u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
        *r_g = t2076;
      }
    }
    if (!d2075) {
      u_no_case(s2074);
    }
  }
}

void p_compile_46_global_value(u_value v_gl, u_value v_n, u_value v_t, u_value *r_r) {
  {
    if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
      {
        static struct u_object l_u_mk_closure_2077 = U_LIT("u_mk_closure");
        static struct u_object l_w__2078 = U_LIT("w_");
        u_value t2079 = f_c_46_c_call_2(&l_u_mk_closure_2077, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__2078, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        *r_r = t2079;
      }
    } else {
      {
        u_value s2080 = f_compile_46_fields_count_2(v_gl, v_n);
        int d2081 = 0;
        if ((!d2081) && (u_is_con(s2080, &n_option_46_Some) && u_equals(u_field(s2080, 0), u_mk_int(0)))) {
          d2081 = 1;
          {
            static struct u_object l_u_mk_con_2082 = U_LIT("u_mk_con");
            u_value t2083 = f_c_46_c_call_2(&l_u_mk_con_2082, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t2083;
          }
        }
        if ((!d2081) && u_is_con(s2080, &n_option_46_Some)) {
          d2081 = 1;
          {
            static struct u_object l_u_mk_closure_2084 = U_LIT("u_mk_closure");
            static struct u_object l_wc__2085 = U_LIT("wc_");
            u_value t2086 = f_c_46_c_call_2(&l_u_mk_closure_2084, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__2085, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t2086;
          }
        }
        if ((!d2081) && u_is_con(s2080, &n_option_46_None)) {
          d2081 = 1;
          {
            u_value s2087 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
            int d2088 = 0;
            if ((!d2088) && u_is_con(s2087, &n_option_46_Some)) {
              u_value v_f = u_field(u_field(s2087, 0), 0);
              d2088 = 1;
              {
                static struct u_object l_u_mk_closure_2089 = U_LIT("u_mk_closure");
                static struct u_object l_w_2090 = U_LIT("w");
                u_value t2091 = f_c_46_c_call_2(&l_u_mk_closure_2089, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_2090, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
                *r_r = t2091;
              }
            }
            if ((!d2088) && u_is_con(s2087, &n_option_46_None)) {
              d2088 = 1;
              {
                static struct u_object l_the_32_variable_32__2092 = U_LIT("the variable ");
                u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__2092, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
              }
            }
            if (!d2088) {
              u_no_case(s2087);
            }
          }
        }
        if (!d2081) {
          u_no_case(s2080);
        }
      }
    }
  }
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
    u_value s2093 = v_e;
    int d2094 = 0;
    if ((!d2094) && (u_is_con(s2093, &n_syntax_46_ELit) && u_is_con(u_field(s2093, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s2093, 0), 0);
      d2094 = 1;
      {
        static struct u_object l_l__2095 = U_LIT("l_");
        u_value t2096;
        if (u_truth(u_lt(u_str_size(v_s), u_mk_int(16)))) {
          t2096 = u_str_size(v_s);
        } else {
          t2096 = u_mk_int(16);
        }
        static struct u_object l___2097 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__2095, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), t2096))), &l___2097)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__2098 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_2099 = U_LIT("U_LIT");
        static struct u_object l__38__2100 = U_LIT("&");
        u_value t2101 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__2098), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_2099, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t2102 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2100, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t2101;
        *r_r = t2102;
      }
    }
    if ((!d2094) && u_is_con(s2093, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s2093, 0);
      d2094 = 1;
      {
        u_value t2103 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2104 = f_compile_46_literal_value_1(v_l);
        *r_ss = t2103;
        *r_r = t2104;
      }
    }
    if ((!d2094) && (u_is_con(s2093, &n_syntax_46_EVar) && u_is_con(u_field(s2093, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s2093, 0), 0);
      u_value v_ty = u_field(u_field(s2093, 0), 1);
      d2094 = 1;
      {
        u_value s2105 = f_list_46_lookup_2(v_n, v_env);
        int d2106 = 0;
        if ((!d2106) && u_is_con(s2105, &n_option_46_Some)) {
          u_value v_c = u_field(s2105, 0);
          d2106 = 1;
          {
            u_value t2107 = u_mk_con(&n_list_46_Nil, 0);
            u_value t2108 = v_c;
            *r_ss = t2107;
            *r_r = t2108;
          }
        }
        if ((!d2106) && u_is_con(s2105, &n_option_46_None)) {
          d2106 = 1;
          {
            p_compile_46_global_value(v_gl, v_n, v_ty, r_r);
            u_value t2109 = u_mk_con(&n_list_46_Nil, 0);
            *r_ss = t2109;
          }
        }
        if (!d2106) {
          u_no_case(s2105);
        }
      }
    }
    if ((!d2094) && (u_is_con(s2093, &n_syntax_46_EApp) && (u_is_con(u_field(s2093, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s2093, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s2093, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s2093, 0), 0), 1);
      u_value v_args = u_field(s2093, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d2094 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d2094) && u_is_con(s2093, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s2093, 0);
      u_value v_args = u_field(s2093, 1);
      d2094 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_2110 = U_LIT("u_apply");
        u_value t2111 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2112 = f_c_46_c_call_2(&l_u_apply_2110, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t2111;
        *r_r = t2112;
      }
    }
    if ((!d2094) && u_is_con(s2093, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s2093, 0);
      u_value v_b = u_field(s2093, 1);
      d2094 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2094) && u_is_con(s2093, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s2093, 0);
      u_value v_b = u_field(s2093, 1);
      d2094 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2113 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2113, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2114 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t2114;
        u_value t2115 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2115;
      }
    }
    if ((!d2094) && u_is_con(s2093, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s2093, 0);
      u_value v_l = u_field(s2093, 1);
      u_value v_rr = u_field(s2093, 2);
      d2094 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2116 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2116, &v_t, r_g);
        u_value t2117 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2117;
        u_value t2118 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2118;
      }
    }
    if ((!d2094) && u_is_con(s2093, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s2093, 0);
      u_value v_cases = u_field(s2093, 1);
      d2094 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2119 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2119, &v_d, r_g);
        static struct u_object l_t_2120 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2120, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_2121 = U_LIT("NULL");
        u_value t2122 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2121))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2122;
        static struct u_object l__33__2123 = U_LIT("!");
        static struct u_object l_u_no_case_2124 = U_LIT("u_no_case");
        u_value t2125 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2123, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2124, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2125;
        u_value t2126 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2126;
      }
    }
    if ((!d2094) && u_is_con(s2093, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s2093, 0);
      u_value v_xs = u_field(s2093, 1);
      u_value v_lists = u_field(s2093, 2);
      u_value v_c = u_field(s2093, 3);
      d2094 = 1;
      {
        static struct u_object l_t_2127 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2127, &v_t, r_g);
        static struct u_object l_e_2128 = U_LIT("e");
        p_compile_46_gen_name(&l_e_2128, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_2129 = U_LIT("u_mk_con");
        static struct u_object l__38__2130 = U_LIT("&");
        u_value t2131 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_2129, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__2130, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t2131;
        u_value t2132 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2132;
      }
    }
    if ((!d2094) && u_is_con(s2093, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s2093, 0);
      d2094 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_2133 = U_LIT("u_mk_tuple");
        u_value t2134 = f_c_46_c_call_2(&l_u_mk_tuple_2133, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t2134;
      }
    }
    if ((!d2094) && u_is_con(s2093, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s2093, 0);
      u_value v_ty = u_field(s2093, 1);
      d2094 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2135 = U_LIT("u_raise");
        static struct u_object l_NULL_2136 = U_LIT("NULL");
        u_value t2137 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2135, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        u_value t2138 = u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2136);
        *r_ss = t2137;
        *r_r = t2138;
      }
    }
    if (!d2094) {
      u_no_case(s2093);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s2139 = v_es;
    int d2140 = 0;
    if ((!d2140) && u_is_con(s2139, &n_list_46_Nil)) {
      d2140 = 1;
      {
        u_value t2141 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2142 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2141;
        *r_rs = t2142;
      }
    }
    if ((!d2140) && u_is_con(s2139, &n_list_46_Cons)) {
      u_value v_e = u_field(s2139, 0);
      u_value v_rest = u_field(s2139, 1);
      d2140 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t2143 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2144 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t2143;
        *r_rs = t2144;
      }
    }
    if (!d2140) {
      u_no_case(s2139);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s2145 = v_rs;
    int d2146 = 0;
    if ((!d2146) && u_is_con(s2145, &n_list_46_Nil)) {
      d2146 = 1;
      {
        u_value t2147 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2147;
      }
    }
    if ((!d2146) && u_is_con(s2145, &n_list_46_Cons)) {
      u_value v_r = u_field(s2145, 0);
      u_value v_rest = u_field(s2145, 1);
      d2146 = 1;
      {
        static struct u_object l_t_2148 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2148, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t2149 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t2149;
      }
    }
    if (!d2146) {
      u_no_case(s2145);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s2150 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2151 = NULL;
  if ((!t2151) && (u_is_con(u_field(s2150, 0), &n_list_46_Cons) && (u_is_con(u_field(s2150, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2150, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2150, 0), 0);
    u_value v_more = u_field(u_field(s2150, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2150, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2150, 1), 1);
    t2151 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t2151) {
    t2151 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2151) {
    u_no_case(s2150);
  }
  return t2151;
}

void p_compile_46_compile_named(u_value v_gl, u_value v_env, u_value v_n, u_value v_ty, u_value v_args, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value v_ra = NULL;
    u_value v_rb = NULL;
    u_value v_rs = NULL;
    u_value v_f = NULL;
    u_value v_t = NULL;
    u_value s2152 = v_args;
    int d2153 = 0;
    if ((!d2153) && (u_is_con(s2152, &n_list_46_Cons) && (u_is_con(u_field(s2152, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2152, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s2152, 0);
      u_value v_b = u_field(u_field(s2152, 1), 0);
      static struct u_object l_and_2160 = U_LIT("and");
      u_value t2162 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2160));
      if (!u_truth(t2162)) {
        static struct u_object l_or_2161 = U_LIT("or");
        t2162 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_2161));
      }
      if (u_truth(t2162)) {
        d2153 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_2154 = U_LIT("t");
          p_compile_46_gen_name(&l_t_2154, &v_t, r_g);
          static struct u_object l_and_2155 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2155)))) {
            {
              u_value t2156 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2156;
            }
          } else {
            {
              static struct u_object l__33__2157 = U_LIT("!");
              u_value t2158 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2157, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2158;
            }
          }
          u_value t2159 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t2159;
        }
      }
    }
    if (!d2153) {
      d2153 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t2163 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t2163)) {
          t2163 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t2163)) {
          {
            u_value t2164 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t2164;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_2165 = U_LIT("u_mk_con");
              u_value t2166 = f_c_46_c_call_2(&l_u_mk_con_2165, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t2166;
            }
          } else {
            {
              u_value s2167 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
              int d2168 = 0;
              if ((!d2168) && u_is_con(s2167, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s2167, 0), 0);
                u_value v_k = u_field(u_field(s2167, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d2168 = 1;
                  {
                    u_value t2169 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t2169;
                  }
                }
              }
              if (!d2168) {
                d2168 = 1;
                {
                  p_compile_46_global_value(v_gl, v_n, v_ty, &v_f);
                  static struct u_object l_u_apply_2170 = U_LIT("u_apply");
                  u_value t2171 = f_c_46_c_call_2(&l_u_apply_2170, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t2171;
                }
              }
              if (!d2168) {
                u_no_case(s2167);
              }
            }
          }
        }
      }
    }
    if (!d2153) {
      u_no_case(s2152);
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
    u_value t2172 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2173 = &t2172;
    u_value l2174 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l2174, &n_list_46_Cons)) {
      u_value v_x = u_field(l2174, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2173 = u_append(e2173, v_x);
      }
      l2174 = u_field(l2174, 1);
    }
    u_value t2175 = t2172;
    v_caps = t2175;
    static struct u_object l_lam_2176 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_2176, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_2177 = U_LIT("env");
    static struct u_object l_args_2178 = U_LIT("args");
    u_value t2179 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2177), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_2178));
    v_decls = t2179;
    static struct u_object l_env_2180 = U_LIT("env");
    static struct u_object l_args_2181 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2180), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2181), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_2182 = U_LIT("u_mk_closure");
    u_value t2183 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2184 = &t2183;
    u_value l2185 = v_caps;
    while (u_is_con(l2185, &n_list_46_Cons)) {
      u_value v_x = u_field(l2185, 0);
      if (u_truth(u_mk_bool(1))) {
        e2184 = u_append(e2184, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2185 = u_field(l2185, 1);
    }
    u_value t2186 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2187 = f_c_46_c_call_2(&l_u_mk_closure_2182, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2183));
    *r_ss = t2186;
    *r_r = t2187;
  }
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s2188 = v_xs;
  u_value t2189 = NULL;
  if ((!t2189) && u_is_con(s2188, &n_list_46_Nil)) {
    t2189 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2189) && u_is_con(s2188, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2188, 0), 0);
    u_value v_i = u_field(u_field(s2188, 0), 1);
    u_value v_rest = u_field(s2188, 1);
    t2189 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t2189) {
    u_no_case(s2188);
  }
  return t2189;
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
    u_value s2190 = v_cases;
    int d2191 = 0;
    if ((!d2191) && u_is_con(s2190, &n_list_46_Nil)) {
      d2191 = 1;
      {
        u_value t2192 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2192;
      }
    }
    if ((!d2191) && (u_is_con(s2190, &n_list_46_Cons) && u_is_con(u_field(s2190, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2190, 0), 0);
      u_value v_guard = u_field(u_field(s2190, 0), 1);
      u_value v_e = u_field(u_field(s2190, 0), 2);
      u_value v_more = u_field(s2190, 1);
      d2191 = 1;
      {
        u_value t2193 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2193;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2194 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t2194;
        u_value s2195 = v_guard;
        int d2196 = 0;
        if ((!d2196) && u_is_con(s2195, &n_option_46_Some)) {
          u_value v_c = u_field(s2195, 0);
          d2196 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2197 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2197;
          }
        }
        if ((!d2196) && u_is_con(s2195, &n_option_46_None)) {
          d2196 = 1;
          {
          }
        }
        if (!d2196) {
          u_no_case(s2195);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2198 = U_LIT("!");
        u_value t2199 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2198, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2199;
      }
    }
    if (!d2191) {
      u_no_case(s2190);
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
    u_value s2200 = u_mk_tuple(2, v_xs, v_lists);
    int d2201 = 0;
    if ((!d2201) && (u_is_con(u_field(s2200, 0), &n_list_46_Cons) && u_is_con(u_field(s2200, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2200, 0), 0);
      u_value v_more = u_field(u_field(s2200, 0), 1);
      u_value v_list = u_field(u_field(s2200, 1), 0);
      u_value v_others = u_field(u_field(s2200, 1), 1);
      d2201 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2202 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2202, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2203 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2203;
        static struct u_object l_u_is_con_2204 = U_LIT("u_is_con");
        u_value t2205 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2204, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2205;
      }
    }
    if (!d2201) {
      d2201 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2206 = U_LIT("u_append");
        u_value t2207 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2206, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2207;
      }
    }
    if (!d2201) {
      u_no_case(s2200);
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
    u_value s2208 = v_s;
    int d2209 = 0;
    if ((!d2209) && u_is_con(s2208, &n_syntax_46_SSkip)) {
      d2209 = 1;
      {
        u_value t2210 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2210;
      }
    }
    if ((!d2209) && u_is_con(s2208, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2208, 0);
      d2209 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2211 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2211;
      }
    }
    if ((!d2209) && u_is_con(s2208, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2208, 0);
      u_value v_body = u_field(s2208, 1);
      d2209 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t2212 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2213 = &t2212;
        u_value l2214 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2214, &n_list_46_Cons)) {
          u_value v_x = u_field(l2214, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2215 = U_LIT("NULL");
            e2213 = u_append(e2213, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2215))));
          }
          l2214 = u_field(l2214, 1);
        }
        u_value t2216 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2212, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2216;
      }
    }
    if ((!d2209) && u_is_con(s2208, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2208, 0);
      u_value v_l = u_field(s2208, 1);
      u_value v_r = u_field(s2208, 2);
      d2209 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2217 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2217;
      }
    }
    if ((!d2209) && u_is_con(s2208, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2208, 0);
      u_value v_b = u_field(s2208, 1);
      d2209 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2218 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2218, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2219 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2219;
      }
    }
    if ((!d2209) && u_is_con(s2208, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2208, 0);
      u_value v_ins = u_field(s2208, 1);
      u_value v_outs = u_field(s2208, 2);
      u_value v_refs = u_field(s2208, 3);
      d2209 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        u_value t2220 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2221 = &t2220;
        u_value l2222 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2222, &n_list_46_Cons)) {
          u_value v_x = u_field(l2222, 0);
          if (u_truth(u_mk_bool(1))) {
            e2221 = u_append(e2221, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
          }
          l2222 = u_field(l2222, 1);
        }
        u_value t2223 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2220))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2223;
      }
    }
    if ((!d2209) && u_is_con(s2208, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2208, 0);
      u_value v_cases = u_field(s2208, 1);
      d2209 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2224 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2224, &v_d, r_g);
        static struct u_object l_d_2225 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2225, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2226 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2226;
        static struct u_object l__33__2227 = U_LIT("!");
        static struct u_object l_u_no_case_2228 = U_LIT("u_no_case");
        u_value t2229 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2227, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2228, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2229;
      }
    }
    if ((!d2209) && (u_is_con(s2208, &n_syntax_46_SAssert) && (u_is_con(u_field(s2208, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2208, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2208, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2208, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2208, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2208, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2208, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2208, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2208, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2208, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2208, 0), 1), 1), 0);
      d2209 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2230 = U_LIT("u_assert_equal");
        u_value t2231 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2230, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2231;
      }
    }
    if ((!d2209) && u_is_con(s2208, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2208, 0);
      d2209 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2232 = U_LIT("u_assert_true");
        u_value t2233 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2232, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2233;
      }
    }
    if ((!d2209) && u_is_con(s2208, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2208, 0);
      u_value v_ty = u_field(s2208, 1);
      d2209 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2234 = U_LIT("u_raise");
        u_value t2235 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2234, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2235;
      }
    }
    if ((!d2209) && u_is_con(s2208, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2208, 0);
      u_value v_hs = u_field(s2208, 1);
      d2209 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2236 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2236, &v_d, r_g);
        static struct u_object l_d_2237 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2237, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2238 = U_LIT("u_pop");
        static struct u_object l__33__2239 = U_LIT("!");
        static struct u_object l_u_reraise_2240 = U_LIT("u_reraise");
        u_value t2241 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2238, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2239, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2240, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2241;
        static struct u_object l_struct_32_u_handler_2242 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2243 = U_LIT("u_push");
        static struct u_object l__61__61__2244 = U_LIT("==");
        static struct u_object l_setjmp_2245 = U_LIT("setjmp");
        static struct u_object l_env_2246 = U_LIT("env");
        static struct u_object l_u_pop_2247 = U_LIT("u_pop");
        u_value t2248 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2242)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2243, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2244, f_c_46_c_call_2(&l_setjmp_2245, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2246), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2247, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2248;
      }
    }
    if (!d2209) {
      u_no_case(s2208);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2249 = v_xs;
    int d2250 = 0;
    if ((!d2250) && u_is_con(s2249, &n_list_46_Nil)) {
      d2250 = 1;
      {
        u_value t2251 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2251;
      }
    }
    if ((!d2250) && u_is_con(s2249, &n_list_46_Cons)) {
      u_value v_x = u_field(s2249, 0);
      u_value v_rest = u_field(s2249, 1);
      d2250 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2252 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2252;
      }
    }
    if (!d2250) {
      u_no_case(s2249);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2253 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2254 = NULL;
  if ((!t2254) && (u_is_con(u_field(s2253, 0), &n_list_46_Cons) && (u_is_con(u_field(s2253, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2253, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2253, 0), 0);
    u_value v_more = u_field(u_field(s2253, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2253, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2253, 1), 1);
    t2254 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2254) {
    t2254 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2254) {
    u_no_case(s2253);
  }
  return t2254;
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
    static struct u_object l_u_caught_2255 = U_LIT("u_caught");
    u_value t2256 = f_c_46_c_call_2(&l_u_caught_2255, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2256;
    u_value s2257 = v_hs;
    int d2258 = 0;
    if ((!d2258) && u_is_con(s2257, &n_list_46_Nil)) {
      d2258 = 1;
      {
        u_value t2259 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2259;
      }
    }
    if ((!d2258) && (u_is_con(s2257, &n_list_46_Cons) && u_is_con(u_field(s2257, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2257, 0), 0);
      u_value v_p = u_field(u_field(s2257, 0), 1);
      u_value v_guard = u_field(u_field(s2257, 0), 2);
      u_value v_s = u_field(u_field(s2257, 0), 3);
      u_value v_more = u_field(s2257, 1);
      d2258 = 1;
      {
        u_value t2260 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2260;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2261 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2261;
        u_value s2262 = v_guard;
        int d2263 = 0;
        if ((!d2263) && u_is_con(s2262, &n_option_46_Some)) {
          u_value v_c = u_field(s2262, 0);
          d2263 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2264 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2264;
          }
        }
        if ((!d2263) && u_is_con(s2262, &n_option_46_None)) {
          d2263 = 1;
          {
          }
        }
        if (!d2263) {
          u_no_case(s2262);
        }
        u_value s2265 = v_p;
        int d2266 = 0;
        if ((!d2266) && u_is_con(s2265, &n_syntax_46_PWild)) {
          d2266 = 1;
          {
            u_value t2267 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2267;
          }
        }
        if (!d2266) {
          d2266 = 1;
          {
            static struct u_object l_u_has_type_2268 = U_LIT("u_has_type");
            u_value t2269 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2268, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2269;
          }
        }
        if (!d2266) {
          u_no_case(s2265);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2270 = U_LIT("!");
        u_value t2271 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2270, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2271;
      }
    }
    if (!d2258) {
      u_no_case(s2257);
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
    u_value s2272 = v_cases;
    int d2273 = 0;
    if ((!d2273) && u_is_con(s2272, &n_list_46_Nil)) {
      d2273 = 1;
      {
        u_value t2274 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2274;
      }
    }
    if ((!d2273) && (u_is_con(s2272, &n_list_46_Cons) && u_is_con(u_field(s2272, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2272, 0), 0);
      u_value v_guard = u_field(u_field(s2272, 0), 1);
      u_value v_s = u_field(u_field(s2272, 0), 2);
      u_value v_more = u_field(s2272, 1);
      d2273 = 1;
      {
        u_value t2275 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2275;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2276 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2276;
        u_value s2277 = v_guard;
        int d2278 = 0;
        if ((!d2278) && u_is_con(s2277, &n_option_46_Some)) {
          u_value v_c = u_field(s2277, 0);
          d2278 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2279 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2279;
          }
        }
        if ((!d2278) && u_is_con(s2277, &n_option_46_None)) {
          d2278 = 1;
          {
          }
        }
        if (!d2278) {
          u_no_case(s2277);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2280 = U_LIT("!");
        u_value t2281 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2280, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2281;
      }
    }
    if (!d2273) {
      u_no_case(s2272);
    }
  }
}

void p_compile_46_compile_cmd(u_value v_gl, u_value v_c, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_bs = NULL;
    u_value v_rb = NULL;
    u_value v_params = NULL;
    u_value v_copy = NULL;
    u_value v_back = NULL;
    u_value s2282 = v_c;
    int d2283 = 0;
    if ((!d2283) && u_is_con(s2282, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2282, 0);
      u_value v_xss = u_field(s2282, 2);
      u_value v_b = u_field(s2282, 4);
      d2283 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2284 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2285 = &t2284;
        u_value l2286 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2286, &n_list_46_Cons)) {
          u_value v_x = u_field(l2286, 0);
          if (u_truth(u_mk_bool(1))) {
            e2285 = u_append(e2285, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2286 = u_field(l2286, 1);
        }
        u_value t2287 = t2284;
        v_params = t2287;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2288 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2288;
          }
        } else {
          {
            u_value t2289 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2289;
          }
        }
        u_value t2290 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2291 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2290;
        *r_ms = t2291;
      }
    }
    if ((!d2283) && u_is_con(s2282, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2282, 0);
      u_value v_ins = u_field(s2282, 2);
      u_value v_outs = u_field(s2282, 3);
      u_value v_refs = u_field(s2282, 4);
      u_value v_body = u_field(s2282, 5);
      d2283 = 1;
      {
        u_value t2292 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2293 = &t2292;
        u_value l2294 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2294, &n_list_46_Cons)) {
          u_value v_x = u_field(l2294, 0);
          if (u_truth(u_mk_bool(1))) {
            e2293 = u_append(e2293, f_compile_46_pointer_1(v_x));
          }
          l2294 = u_field(l2294, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2292), v_body, &v_bs, r_g);
        u_value t2295 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2296 = &t2295;
        u_value l2297 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2297, &n_list_46_Cons)) {
          u_value v_x = u_field(l2297, 0);
          if (u_truth(u_mk_bool(1))) {
            e2296 = u_append(e2296, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2297 = u_field(l2297, 1);
        }
        u_value t2298 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2299 = &t2298;
        u_value l2300 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2300, &n_list_46_Cons)) {
          u_value v_x = u_field(l2300, 0);
          if (u_truth(u_mk_bool(1))) {
            e2299 = u_append(e2299, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2300 = u_field(l2300, 1);
        }
        u_value t2301 = f_list_46__43__43__2(t2295, t2298);
        v_params = t2301;
        u_value t2302 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2303 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2302;
        *r_ms = t2303;
      }
    }
    if ((!d2283) && u_is_con(s2282, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s2282, 0);
      d2283 = 1;
      {
        p_compile_46_compile_expr(v_gl, u_mk_con(&n_list_46_Nil, 0), v_e, &v_bs, &v_rb, r_g);
        static struct u_object l_u_print_line_2304 = U_LIT("u_print_line");
        u_value t2305 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2306 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_line_2304, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2305;
        *r_ms = t2306;
      }
    }
    if ((!d2283) && (u_is_con(s2282, &n_syntax_46_CRun) && u_is_con(u_field(s2282, 1), &n_syntax_46_SBlock))) {
      u_value v_xs = u_field(u_field(s2282, 1), 0);
      u_value v_body = u_field(u_field(s2282, 1), 1);
      d2283 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_body, &v_bs, r_g);
        u_value t2307 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2308 = &t2307;
        u_value l2309 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2309, &n_list_46_Cons)) {
          u_value v_x = u_field(l2309, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2310 = U_LIT("NULL");
            e2308 = u_append(e2308, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2310))));
          }
          l2309 = u_field(l2309, 1);
        }
        u_value t2311 = t2307;
        v_copy = t2311;
        u_value t2312 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2313 = &t2312;
        u_value l2314 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2314, &n_list_46_Cons)) {
          u_value v_x = u_field(l2314, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_u_print_binding_2315 = U_LIT("u_print_binding");
            e2313 = u_append(e2313, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_binding_2315, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_syntax_46_Name_46_base_1(v_x)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x)), u_mk_con(&n_list_46_Nil, 0))))));
          }
          l2314 = u_field(l2314, 1);
        }
        u_value t2316 = t2312;
        v_back = t2316;
        u_value t2317 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2318 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_copy, f_list_46__43__43__2(v_bs, v_back))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2317;
        *r_ms = t2318;
      }
    }
    if ((!d2283) && u_is_con(s2282, &n_syntax_46_CRun)) {
      u_value v_body = u_field(s2282, 1);
      d2283 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2319 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2320 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2319;
        *r_ms = t2320;
      }
    }
    if ((!d2283) && u_is_con(s2282, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s2282, 1);
      d2283 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2321 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2322 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2321;
        *r_ms = t2322;
      }
    }
    if (!d2283) {
      d2283 = 1;
      {
        u_value t2323 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2324 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2323;
        *r_ms = t2324;
      }
    }
    if (!d2283) {
      u_no_case(s2282);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2325 = v_cs;
    int d2326 = 0;
    if ((!d2326) && u_is_con(s2325, &n_list_46_Nil)) {
      d2326 = 1;
      {
        u_value t2327 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2328 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2327;
        *r_ms = t2328;
      }
    }
    if ((!d2326) && u_is_con(s2325, &n_list_46_Cons)) {
      u_value v_c = u_field(s2325, 0);
      u_value v_rest = u_field(s2325, 1);
      d2326 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_rest, &v_d2, &v_m2, r_g);
        u_value t2329 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2330 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2329;
        *r_ms = t2330;
      }
    }
    if (!d2326) {
      u_no_case(s2325);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_handler_2331 = U_LIT("handler");
  u_value t2358 = u_mk_con(&n_c_46_CeVar, 1, &l_handler_2331);
  u_value t2357;
  {
    u_value v_h = t2358;
    static struct u_object l_static_32_u_value_2332 = U_LIT("static u_value");
    static struct u_object l_cache_2333 = U_LIT("cache");
    static struct u_object l_NULL_2334 = U_LIT("NULL");
    static struct u_object l__61__61__2335 = U_LIT("==");
    static struct u_object l_cache_2336 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2337 = U_LIT("U_COMPUTING");
    static struct u_object l_u_fail_2338 = U_LIT("u_fail");
    static struct u_object l_recursive_32_initia_2339 = U_LIT("recursive initialization of ");
    static struct u_object l_cache_2340 = U_LIT("cache");
    static struct u_object l_cache_2341 = U_LIT("cache");
    static struct u_object l_cache_2342 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2343 = U_LIT("U_COMPUTING");
    static struct u_object l_struct_32_u_handler_2344 = U_LIT("struct u_handler");
    static struct u_object l_handler_2345 = U_LIT("handler");
    static struct u_object l_u_push_2346 = U_LIT("u_push");
    static struct u_object l__33__61__2347 = U_LIT("!=");
    static struct u_object l_setjmp_2348 = U_LIT("setjmp");
    static struct u_object l_env_2349 = U_LIT("env");
    static struct u_object l_u_pop_2350 = U_LIT("u_pop");
    static struct u_object l_cache_2351 = U_LIT("cache");
    static struct u_object l_NULL_2352 = U_LIT("NULL");
    static struct u_object l_u_reraise_2353 = U_LIT("u_reraise");
    static struct u_object l_u_pop_2354 = U_LIT("u_pop");
    static struct u_object l_cache_2355 = U_LIT("cache");
    static struct u_object l_cache_2356 = U_LIT("cache");
    t2357 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2332), &l_cache_2333, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2334))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2335, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2336), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2337)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2338, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2339, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2340), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2341))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2342), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2343)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2344)), &l_handler_2345, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2346, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__33__61__2347, f_c_46_c_call_2(&l_setjmp_2348, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, v_h, &l_env_2349), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2350, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2351), u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2352)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2353, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2354, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2355), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2356))), u_mk_con(&n_list_46_Nil, 0))))));
  }
  return t2357;
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2359 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2360 = &t2359;
  u_value l2361 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l2361, &n_list_46_Cons)) {
    u_value v_p = u_field(l2361, 0);
    if (u_truth(u_mk_bool(1))) {
      e2360 = u_append(e2360, f_compile_46_fun_wrapper_1(v_p));
    }
    l2361 = u_field(l2361, 1);
  }
  u_value t2362 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2363 = &t2362;
  u_value l2364 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l2364, &n_list_46_Cons)) {
    u_value v_p = u_field(l2364, 0);
    if (u_truth(u_mk_bool(1))) {
      e2363 = u_append(e2363, f_compile_46_constr_wrapper_1(v_p));
    }
    l2364 = u_field(l2364, 1);
  }
  u_value t2365 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2366 = &t2365;
  u_value l2367 = f_list_46_seconds_1(f_compile_46_builtin_functions_0());
  while (u_is_con(l2367, &n_list_46_Cons)) {
    u_value v_p = u_field(l2367, 0);
    if (u_truth(u_mk_bool(1))) {
      e2366 = u_append(e2366, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2367 = u_field(l2367, 1);
  }
  return f_list_46__43__43__2(t2359, f_list_46__43__43__2(f_list_46_concat_1(t2362), t2365));
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2368 = v_p;
  u_value t2369 = NULL;
  if (!t2369) {
    u_value v_n = u_field(s2368, 0);
    u_value v_k = u_field(s2368, 1);
    static struct u_object l_w__2370 = U_LIT("w_");
    t2369 = f_compile_46_wrapper_3(u_concat(&l_w__2370, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2369) {
    u_no_case(s2368);
  }
  return t2369;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2371 = v_p;
  u_value t2372 = NULL;
  if (!t2372) {
    u_value v_f = u_field(s2371, 0);
    u_value v_k = u_field(s2371, 1);
    static struct u_object l_w_2373 = U_LIT("w");
    t2372 = f_compile_46_wrapper_3(u_concat(&l_w_2373, v_f), v_f, v_k);
  }
  if (!t2372) {
    u_no_case(s2371);
  }
  return t2372;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2374 = U_LIT("env");
  static struct u_object l_args_2375 = U_LIT("args");
  u_value t2376 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2377 = &t2376;
  u_value l2378 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2378, &n_list_46_Cons)) {
    u_value v_i = u_field(l2378, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2379 = U_LIT("args");
      e2377 = u_append(e2377, f_compile_46_index_2(&l_args_2379, v_i));
    }
    l2378 = u_field(l2378, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2374), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2375), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2376))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2380 = v_p;
  u_value t2381 = NULL;
  if ((!t2381) && u_equals(u_field(s2380, 1), u_mk_int(0))) {
    t2381 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2381) {
    u_value v_n = u_field(s2380, 0);
    u_value v_k = u_field(s2380, 1);
    static struct u_object l_wc__2382 = U_LIT("wc_");
    static struct u_object l_env_2383 = U_LIT("env");
    static struct u_object l_args_2384 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2385 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2386 = U_LIT("args");
    t2381 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2382, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2383), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2384), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2385, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2386), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2381) {
    u_no_case(s2380);
  }
  return t2381;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2387;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2387 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2387 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2387;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2388 = v_d;
  u_value t2389 = NULL;
  if ((!t2389) && u_is_con(s2388, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2388, 0);
    u_value v_f = u_field(s2388, 1);
    u_value v_ps = u_field(s2388, 2);
    t2389 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2389) {
    t2389 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2389) {
    u_no_case(s2388);
  }
  return t2389;
}

void p_compile_46_compile_program(u_value v_q, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2390 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2390;
    u_value t2391 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t2391;
    p_compile_46_compile_cmds(v_gl, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value s2392 = v_g;
    int d2393 = 0;
    if ((!d2393) && u_is_con(s2392, &n_compile_46_Gen)) {
      u_value v_lifted = u_field(s2392, 1);
      d2393 = 1;
      {
        u_value t2394 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, v_lifted));
        v_fs = t2394;
      }
    }
    if (!d2393) {
      u_no_case(s2392);
    }
    static struct u_object l_ulang_46_h_2395 = U_LIT("ulang.h");
    u_value t2396 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2397 = &t2396;
    u_value l2398 = v_fs;
    while (u_is_con(l2398, &n_list_46_Cons)) {
      u_value v_d = u_field(l2398, 0);
      if (u_truth(u_mk_bool(1))) {
        e2397 = u_append(e2397, f_compile_46_prototype_1(v_d));
      }
      l2398 = u_field(l2398, 1);
    }
    static struct u_object l_main_2399 = U_LIT("main");
    static struct u_object l_argc_2400 = U_LIT("argc");
    static struct u_object l_char_2401 = U_LIT("char");
    static struct u_object l_argv_2402 = U_LIT("argv");
    static struct u_object l_u_args_2403 = U_LIT("u_args");
    static struct u_object l_argc_2404 = U_LIT("argc");
    static struct u_object l_argv_2405 = U_LIT("argv");
    u_value t2406 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2395), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2396), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2399, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2400), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2401))), &l_argv_2402), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2403, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2404), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2405), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2406;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_target) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2407 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2407, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2408 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2408, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q);
    p_compile_46_compile_program(v_q, &v_f);
    p_os_write(v_target, f_c_46_c_file_1(v_f));
  }
}

u_value lam366(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam367(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam369(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam379(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_f = args[0];
  return f_ctx_46_is_fun_2(f_check_46_State_46_ctx_1(v_s), v_f);
}

u_value lam386(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam611(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_t = args[0];
  return f_ctx_46_is_sort_2(f_check_46_State_46_ctx_1(v_s), v_t);
}

u_value lam720(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam723(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam785(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_q = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_q);
}

u_value lam1062(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  {
    u_value v_args = NULL;
    p_os_args(&v_args);
    u_value s2409 = v_args;
    int d2410 = 0;
    if ((!d2410) && u_is_con(s2409, &n_list_46_Nil)) {
      d2410 = 1;
      {
      }
    }
    if ((!d2410) && (u_is_con(s2409, &n_list_46_Cons) && (u_is_con(u_field(s2409, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2409, 1), 1), &n_list_46_Nil)))) {
      u_value v_source = u_field(s2409, 0);
      u_value v_target = u_field(u_field(s2409, 1), 0);
      d2410 = 1;
      {
        p_compile_46_compile_file(v_source, v_target);
      }
    }
    if (!d2410) {
      d2410 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_sou_2411 = U_LIT("usage: ulang source.u target.c");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_sou_2411), "os.OsError");
      }
    }
    if (!d2410) {
      u_no_case(s2409);
    }
    u_print_binding("args", v_args);
  }
  return 0;
}
