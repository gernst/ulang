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

u_value lam348(u_value *env, u_value *args);

u_value lam349(u_value *env, u_value *args);

u_value lam351(u_value *env, u_value *args);

u_value lam361(u_value *env, u_value *args);

u_value lam368(u_value *env, u_value *args);

u_value lam587(u_value *env, u_value *args);

u_value lam696(u_value *env, u_value *args);

u_value lam699(u_value *env, u_value *args);

u_value lam761(u_value *env, u_value *args);

u_value lam1038(u_value *env, u_value *args);

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
  return f_ctx_46_Ctx_46_named_58__61__2(f_ctx_46_Ctx_46_aliases_58__61__2(v_c, f_map_46_put_3(v_n, u_mk_tuple(2, v_ps, v_t), f_ctx_46_Ctx_46_aliases_1(v_c))), f_ctx_46_with_named_2(f_ctx_46_Ctx_46_named_1(v_c), v_n));
}

u_value f_ctx_46_with_fun_3(u_value v_c, u_value v_n, u_value v_sc) {
  return f_ctx_46_Ctx_46_named_58__61__2(f_ctx_46_Ctx_46_funs_58__61__2(v_c, f_map_46_put_3(v_n, f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), u_mk_con(&n_list_46_Cons, 2, v_sc, u_mk_con(&n_list_46_Nil, 0))), f_ctx_46_Ctx_46_funs_1(v_c))), f_ctx_46_with_named_2(f_ctx_46_Ctx_46_named_1(v_c), v_n));
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
  u_value s229 = v_x;
  u_value t230 = NULL;
  if ((!t230) && u_is_con(s229, &n_check_46_State)) {
    u_value v_y = u_field(s229, 0);
    t230 = v_y;
  }
  if (!t230) {
    u_no_case(s229);
  }
  return t230;
}

u_value f_check_46_State_46_sub_1(u_value v_x) {
  u_value s231 = v_x;
  u_value t232 = NULL;
  if ((!t232) && u_is_con(s231, &n_check_46_State)) {
    u_value v_y = u_field(s231, 1);
    t232 = v_y;
  }
  if (!t232) {
    u_no_case(s231);
  }
  return t232;
}

u_value f_check_46_State_46_count_1(u_value v_x) {
  u_value s233 = v_x;
  u_value t234 = NULL;
  if ((!t234) && u_is_con(s233, &n_check_46_State)) {
    u_value v_y = u_field(s233, 2);
    t234 = v_y;
  }
  if (!t234) {
    u_no_case(s233);
  }
  return t234;
}

u_value f_check_46_State_46_module_1(u_value v_x) {
  u_value s235 = v_x;
  u_value t236 = NULL;
  if ((!t236) && u_is_con(s235, &n_check_46_State)) {
    u_value v_y = u_field(s235, 3);
    t236 = v_y;
  }
  if (!t236) {
    u_no_case(s235);
  }
  return t236;
}

u_value f_check_46_State_46_visible_1(u_value v_x) {
  u_value s237 = v_x;
  u_value t238 = NULL;
  if ((!t238) && u_is_con(s237, &n_check_46_State)) {
    u_value v_y = u_field(s237, 4);
    t238 = v_y;
  }
  if (!t238) {
    u_no_case(s237);
  }
  return t238;
}

u_value f_check_46_State_46_table_1(u_value v_x) {
  u_value s239 = v_x;
  u_value t240 = NULL;
  if ((!t240) && u_is_con(s239, &n_check_46_State)) {
    u_value v_y = u_field(s239, 5);
    t240 = v_y;
  }
  if (!t240) {
    u_no_case(s239);
  }
  return t240;
}

u_value f_check_46_State_46_ctx_58__61__2(u_value v_x, u_value v_y) {
  u_value s241 = v_x;
  u_value t242 = NULL;
  if ((!t242) && u_is_con(s241, &n_check_46_State)) {
    u_value v_a1 = u_field(s241, 1);
    u_value v_a2 = u_field(s241, 2);
    u_value v_a3 = u_field(s241, 3);
    u_value v_a4 = u_field(s241, 4);
    u_value v_a5 = u_field(s241, 5);
    t242 = u_mk_con(&n_check_46_State, 6, v_y, v_a1, v_a2, v_a3, v_a4, v_a5);
  }
  if (!t242) {
    u_no_case(s241);
  }
  return t242;
}

u_value f_check_46_State_46_sub_58__61__2(u_value v_x, u_value v_y) {
  u_value s243 = v_x;
  u_value t244 = NULL;
  if ((!t244) && u_is_con(s243, &n_check_46_State)) {
    u_value v_a0 = u_field(s243, 0);
    u_value v_a2 = u_field(s243, 2);
    u_value v_a3 = u_field(s243, 3);
    u_value v_a4 = u_field(s243, 4);
    u_value v_a5 = u_field(s243, 5);
    t244 = u_mk_con(&n_check_46_State, 6, v_a0, v_y, v_a2, v_a3, v_a4, v_a5);
  }
  if (!t244) {
    u_no_case(s243);
  }
  return t244;
}

u_value f_check_46_State_46_count_58__61__2(u_value v_x, u_value v_y) {
  u_value s245 = v_x;
  u_value t246 = NULL;
  if ((!t246) && u_is_con(s245, &n_check_46_State)) {
    u_value v_a0 = u_field(s245, 0);
    u_value v_a1 = u_field(s245, 1);
    u_value v_a3 = u_field(s245, 3);
    u_value v_a4 = u_field(s245, 4);
    u_value v_a5 = u_field(s245, 5);
    t246 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_y, v_a3, v_a4, v_a5);
  }
  if (!t246) {
    u_no_case(s245);
  }
  return t246;
}

u_value f_check_46_State_46_module_58__61__2(u_value v_x, u_value v_y) {
  u_value s247 = v_x;
  u_value t248 = NULL;
  if ((!t248) && u_is_con(s247, &n_check_46_State)) {
    u_value v_a0 = u_field(s247, 0);
    u_value v_a1 = u_field(s247, 1);
    u_value v_a2 = u_field(s247, 2);
    u_value v_a4 = u_field(s247, 4);
    u_value v_a5 = u_field(s247, 5);
    t248 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_a2, v_y, v_a4, v_a5);
  }
  if (!t248) {
    u_no_case(s247);
  }
  return t248;
}

u_value f_check_46_State_46_visible_58__61__2(u_value v_x, u_value v_y) {
  u_value s249 = v_x;
  u_value t250 = NULL;
  if ((!t250) && u_is_con(s249, &n_check_46_State)) {
    u_value v_a0 = u_field(s249, 0);
    u_value v_a1 = u_field(s249, 1);
    u_value v_a2 = u_field(s249, 2);
    u_value v_a3 = u_field(s249, 3);
    u_value v_a5 = u_field(s249, 5);
    t250 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_a2, v_a3, v_y, v_a5);
  }
  if (!t250) {
    u_no_case(s249);
  }
  return t250;
}

u_value f_check_46_State_46_table_58__61__2(u_value v_x, u_value v_y) {
  u_value s251 = v_x;
  u_value t252 = NULL;
  if ((!t252) && u_is_con(s251, &n_check_46_State)) {
    u_value v_a0 = u_field(s251, 0);
    u_value v_a1 = u_field(s251, 1);
    u_value v_a2 = u_field(s251, 2);
    u_value v_a3 = u_field(s251, 3);
    u_value v_a4 = u_field(s251, 4);
    t252 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_a2, v_a3, v_a4, v_y);
  }
  if (!t252) {
    u_no_case(s251);
  }
  return t252;
}

u_value f_check_46_with_ctx_2(u_value v_s, u_value v_c) {
  return f_check_46_State_46_ctx_58__61__2(v_s, v_c);
}

u_value f_check_46_enter_2(u_value v_s, u_value v_m) {
  return f_check_46_State_46_visible_58__61__2(f_check_46_State_46_module_58__61__2(f_check_46_State_46_count_58__61__2(f_check_46_State_46_sub_58__61__2(v_s, f_map_46_empty_0()), u_mk_int(0)), v_m), f_check_46_sees_2(v_m, f_check_46_State_46_table_1(v_s)));
}

u_value f_check_46_sees_2(u_value v_m, u_value v_vs) {
  u_value s253 = f_list_46_lookup_2(v_m, v_vs);
  u_value t254 = NULL;
  if ((!t254) && u_is_con(s253, &n_option_46_Some)) {
    u_value v_ms = u_field(s253, 0);
    t254 = v_ms;
  }
  if ((!t254) && u_is_con(s253, &n_option_46_None)) {
    t254 = u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t254) {
    u_no_case(s253);
  }
  return t254;
}

u_value f_check_46_start_2(u_value v_main, u_value v_vs) {
  return u_mk_con(&n_check_46_State, 6, f_check_46_initial_0(), f_map_46_empty_0(), u_mk_int(0), v_main, f_check_46_sees_2(v_main, v_vs), v_vs);
}

u_value f_check_46_flatten_2(u_value v_m, u_value v_cs) {
  u_value s255 = v_cs;
  u_value t256 = NULL;
  if ((!t256) && u_is_con(s255, &n_list_46_Nil)) {
    t256 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t256) && (u_is_con(s255, &n_list_46_Cons) && u_is_con(u_field(s255, 0), &n_syntax_46_CModule))) {
    u_value v_n = u_field(u_field(s255, 0), 0);
    u_value v_ds = u_field(u_field(s255, 0), 1);
    u_value v_rest = u_field(s255, 1);
    t256 = f_list_46__43__43__2(f_check_46_flatten_2(v_n, v_ds), f_check_46_flatten_2(v_m, v_rest));
  }
  if ((!t256) && u_is_con(s255, &n_list_46_Cons)) {
    u_value v_c = u_field(s255, 0);
    u_value v_rest = u_field(s255, 1);
    t256 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_m, f_check_46_qualify_2(v_m, v_c)), f_check_46_flatten_2(v_m, v_rest));
  }
  if (!t256) {
    u_no_case(s255);
  }
  return t256;
}

u_value f_check_46_in_module_2(u_value v_m, u_value v_n) {
  return u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), f_syntax_46_Name_46_base_1(v_n));
}

u_value f_check_46_qualify_2(u_value v_m, u_value v_c) {
  u_value s257 = v_c;
  u_value t258 = NULL;
  if ((!t258) && u_is_con(s257, &n_syntax_46_CSort)) {
    u_value v_n = u_field(s257, 0);
    u_value v_ps = u_field(s257, 1);
    u_value v_t = u_field(s257, 2);
    t258 = u_mk_con(&n_syntax_46_CSort, 3, f_check_46_in_module_2(v_m, v_n), v_ps, v_t);
  }
  if ((!t258) && u_is_con(s257, &n_syntax_46_CData)) {
    u_value v_n = u_field(s257, 0);
    u_value v_ps = u_field(s257, 1);
    u_value v_ks = u_field(s257, 2);
    u_value t259 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e260 = &t259;
    u_value l261 = v_ks;
    while (u_is_con(l261, &n_list_46_Cons)) {
      u_value v_k = u_field(l261, 0);
      if (u_truth(u_mk_bool(1))) {
        e260 = u_append(e260, f_check_46_qualify_constr_2(v_m, v_k));
      }
      l261 = u_field(l261, 1);
    }
    t258 = u_mk_con(&n_syntax_46_CData, 3, f_check_46_in_module_2(v_m, v_n), v_ps, t259);
  }
  if ((!t258) && u_is_con(s257, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s257, 0);
    u_value v_ps = u_field(s257, 1);
    u_value v_xss = u_field(s257, 2);
    u_value v_r = u_field(s257, 3);
    u_value v_b = u_field(s257, 4);
    t258 = u_mk_con(&n_syntax_46_CFun, 5, f_check_46_in_module_2(v_m, v_n), v_ps, v_xss, v_r, v_b);
  }
  if ((!t258) && u_is_con(s257, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s257, 0);
    u_value v_ps = u_field(s257, 1);
    u_value v_ins = u_field(s257, 2);
    u_value v_outs = u_field(s257, 3);
    u_value v_refs = u_field(s257, 4);
    u_value v_b = u_field(s257, 5);
    t258 = u_mk_con(&n_syntax_46_CProc, 6, f_check_46_in_module_2(v_m, v_n), v_ps, v_ins, v_outs, v_refs, v_b);
  }
  if (!t258) {
    t258 = v_c;
  }
  if (!t258) {
    u_no_case(s257);
  }
  return t258;
}

u_value f_check_46_qualify_constr_2(u_value v_m, u_value v_k) {
  return f_syntax_46_Constr_46_name_58__61__2(v_k, f_check_46_in_module_2(v_m, f_syntax_46_Constr_46_name_1(v_k)));
}

u_value f_check_46_imports_in_2(u_value v_ds, u_value v_m) {
  u_value s262 = v_ds;
  u_value t263 = NULL;
  if ((!t263) && u_is_con(s262, &n_list_46_Nil)) {
    t263 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t263) && (u_is_con(s262, &n_list_46_Cons) && u_is_con(u_field(u_field(s262, 0), 1), &n_syntax_46_CImport))) {
    u_value v_k = u_field(u_field(s262, 0), 0);
    u_value v_n = u_field(u_field(u_field(s262, 0), 1), 0);
    u_value v_rest = u_field(s262, 1);
    if (u_truth(u_eq(v_k, v_m))) {
      t263 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_imports_in_2(v_rest, v_m));
    }
  }
  if ((!t263) && u_is_con(s262, &n_list_46_Cons)) {
    u_value v_rest = u_field(s262, 1);
    t263 = f_check_46_imports_in_2(v_rest, v_m);
  }
  if (!t263) {
    u_no_case(s262);
  }
  return t263;
}

u_value f_check_46_visibility_2(u_value v_ds, u_value v_main) {
  u_value t264 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e265 = &t264;
  u_value l266 = f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_main, u_mk_con(&n_list_46_Nil, 0)), f_list_46_firsts_1(v_ds)));
  while (u_is_con(l266, &n_list_46_Cons)) {
    u_value v_m = u_field(l266, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_prelude_267 = U_LIT("prelude");
      static struct u_object l_prelude_268 = U_LIT("prelude");
      e265 = u_append(e265, u_mk_tuple(2, v_m, f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_check_46_imports_in_2(v_ds, v_m), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, &l_prelude_267, u_mk_con(&n_list_46_Nil, 0)), f_check_46_imports_in_2(v_ds, &l_prelude_268)))))));
    }
    l266 = u_field(l266, 1);
  }
  return t264;
}

u_value f_check_46_visible_names_3(u_value v_s, u_value v_n, u_value v_declared) {
  u_value t269 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e270 = &t269;
  u_value l271 = f_ctx_46_names_of_2(f_check_46_State_46_ctx_1(v_s), f_syntax_46_Name_46_base_1(v_n));
  while (u_is_con(l271, &n_list_46_Cons)) {
    u_value v_d = u_field(l271, 0);
    if (u_truth(u_apply(v_declared, 1, v_d))) {
      e270 = u_append(e270, v_d);
    }
    l271 = u_field(l271, 1);
  }
  u_value t280 = t269;
  u_value t279;
  {
    u_value v_ds = t280;
    u_value s272 = v_n;
    u_value t273 = NULL;
    if ((!t273) && (u_is_con(s272, &n_syntax_46_Name) && u_is_con(u_field(s272, 0), &n_option_46_Some))) {
      u_value t274;
      if (u_truth(f_list_46_contains_2(v_n, v_ds))) {
        t274 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
      } else {
        t274 = u_mk_con(&n_list_46_Nil, 0);
      }
      t273 = t274;
    }
    if ((!t273) && (u_is_con(s272, &n_syntax_46_Name) && u_is_con(u_field(s272, 0), &n_option_46_None))) {
      u_value v_b = u_field(s272, 1);
      u_value t278;
      if (u_truth(f_list_46_contains_2(u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), v_ds))) {
        t278 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), u_mk_con(&n_list_46_Nil, 0));
      } else {
        u_value t275 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e276 = &t275;
        u_value l277 = v_ds;
        while (u_is_con(l277, &n_list_46_Cons)) {
          u_value v_d = u_field(l277, 0);
          if (u_truth(f_check_46_is_visible_2(v_s, v_d))) {
            e276 = u_append(e276, v_d);
          }
          l277 = u_field(l277, 1);
        }
        t278 = t275;
      }
      t273 = t278;
    }
    if (!t273) {
      u_no_case(s272);
    }
    t279 = t273;
  }
  return t279;
}

u_value f_check_46_is_visible_2(u_value v_s, u_value v_d) {
  u_value s281 = f_syntax_46_Name_46_module_1(v_d);
  u_value t282 = NULL;
  if ((!t282) && u_is_con(s281, &n_option_46_Some)) {
    u_value v_m = u_field(s281, 0);
    t282 = f_list_46_contains_2(v_m, f_check_46_State_46_visible_1(v_s));
  }
  if ((!t282) && u_is_con(s281, &n_option_46_None)) {
    t282 = u_mk_bool(0);
  }
  if (!t282) {
    u_no_case(s281);
  }
  return t282;
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
  static struct u_object l_Bool_283 = U_LIT("Bool");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Bool_283), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Int_284 = U_LIT("Int");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Int_284), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Str_285 = U_LIT("Str");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Str_285), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Char_286 = U_LIT("Char");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Char_286), u_mk_con(&n_list_46_Nil, 0));
  return cache;
}

u_value f_check_46_tlist_1(u_value v_t) {
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_list_name_0(), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_check_46_fun_2(u_value v_args, u_value v_res) {
  return u_mk_con(&n_syntax_46_TFun, 2, v_args, v_res);
}

u_value f_check_46_params_1(u_value v_ps) {
  u_value t287 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e288 = &t287;
  u_value l289 = v_ps;
  while (u_is_con(l289, &n_list_46_Cons)) {
    u_value v_p = u_field(l289, 0);
    if (u_truth(u_mk_bool(1))) {
      e288 = u_append(e288, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l289 = u_field(l289, 1);
  }
  return t287;
}

u_value f_check_46_basic_1(u_value v_n) {
  u_value s290 = v_n;
  u_value t291 = NULL;
  if ((!t291) && (u_is_con(s290, &n_syntax_46_Name) && u_is_con(u_field(s290, 0), &n_option_46_None))) {
    u_value v_b = u_field(s290, 1);
    static struct u_object l_Bool_292 = U_LIT("Bool");
    static struct u_object l_Int_293 = U_LIT("Int");
    static struct u_object l_Str_294 = U_LIT("Str");
    static struct u_object l_Char_295 = U_LIT("Char");
    t291 = f_list_46_contains_2(v_b, u_mk_con(&n_list_46_Cons, 2, &l_Bool_292, u_mk_con(&n_list_46_Cons, 2, &l_Int_293, u_mk_con(&n_list_46_Cons, 2, &l_Str_294, u_mk_con(&n_list_46_Cons, 2, &l_Char_295, u_mk_con(&n_list_46_Nil, 0))))));
  }
  if (!t291) {
    t291 = u_mk_bool(0);
  }
  if (!t291) {
    u_no_case(s290);
  }
  return t291;
}

u_value f_check_46_literal_1(u_value v_v) {
  u_value s296 = v_v;
  u_value t297 = NULL;
  if ((!t297) && u_is_con(s296, &n_syntax_46_VUnit)) {
    t297 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t297) && u_is_con(s296, &n_syntax_46_VBool)) {
    t297 = f_check_46_tbool_0();
  }
  if ((!t297) && u_is_con(s296, &n_syntax_46_VInt)) {
    t297 = f_check_46_tint_0();
  }
  if ((!t297) && u_is_con(s296, &n_syntax_46_VStr)) {
    t297 = f_check_46_tstr_0();
  }
  if ((!t297) && u_is_con(s296, &n_syntax_46_VChar)) {
    t297 = f_check_46_tchar_0();
  }
  if (!t297) {
    u_no_case(s296);
  }
  return t297;
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
  u_value s298 = v_n;
  u_value t299 = NULL;
  if ((!t299) && (u_is_con(s298, &n_syntax_46_Name) && u_is_con(u_field(s298, 0), &n_option_46_None))) {
    u_value v_b = u_field(s298, 1);
    t299 = f_check_46_builtin_scheme_1(v_b);
  }
  if (!t299) {
    t299 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t299) {
    u_no_case(s298);
  }
  return t299;
}

u_value f_check_46_builtin_scheme_1(u_value v_b) {
  static struct u_object l_A_300 = U_LIT("A");
  u_value t304 = u_mk_con(&n_syntax_46_TParam, 1, &l_A_300);
  u_value t303;
  {
    u_value v_a = t304;
    u_value s301 = v_b;
    u_value t302 = NULL;
    if ((!t302) && u_equals(s301, u_mk_lit("+"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("-"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("*"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("/"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("%"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("<"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("=="))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("compare"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("and"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("or"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("chars"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tlist_1(f_check_46_tchar_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("string"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tchar_0()), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("ord"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("chr"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("^"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("str_size"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("str_join"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("str_slice"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("str_split"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tlist_1(f_check_46_tstr_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("str_get"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("str_contains"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("str_compare"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("str_show"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("str_quote"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("str_number"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_option_name_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("is_digit"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("is_letter"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t302) && u_equals(s301, u_mk_lit("is_space"))) {
      t302 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if (!t302) {
      t302 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t302) {
      u_no_case(s301);
    }
    t303 = t302;
  }
  return t303;
}

u_value f_check_46_is_meta_1(u_value v_t) {
  u_value s305 = v_t;
  u_value t306 = NULL;
  if ((!t306) && u_is_con(s305, &n_syntax_46_TMeta)) {
    t306 = u_mk_bool(1);
  }
  if (!t306) {
    t306 = u_mk_bool(0);
  }
  if (!t306) {
    u_no_case(s305);
  }
  return t306;
}

u_value f_check_46_free_1(u_value v_t) {
  u_value s307 = v_t;
  u_value t308 = NULL;
  if ((!t308) && u_is_con(s307, &n_syntax_46_TParam)) {
    t308 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t308) && u_is_con(s307, &n_syntax_46_TMeta)) {
    t308 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t308) && u_is_con(s307, &n_syntax_46_THole)) {
    t308 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t308) && u_is_con(s307, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s307, 1);
    u_value t309 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e310 = &t309;
    u_value l311 = v_args;
    while (u_is_con(l311, &n_list_46_Cons)) {
      u_value v_a = u_field(l311, 0);
      if (u_truth(u_mk_bool(1))) {
        e310 = u_append(e310, f_check_46_free_1(v_a));
      }
      l311 = u_field(l311, 1);
    }
    t308 = f_list_46_distinct_1(f_list_46_concat_1(t309));
  }
  if ((!t308) && u_is_con(s307, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s307, 0);
    u_value v_r = u_field(s307, 1);
    u_value t312 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e313 = &t312;
    u_value l314 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l314, &n_list_46_Cons)) {
      u_value v_a = u_field(l314, 0);
      if (u_truth(u_mk_bool(1))) {
        e313 = u_append(e313, f_check_46_free_1(v_a));
      }
      l314 = u_field(l314, 1);
    }
    t308 = f_list_46_distinct_1(f_list_46_concat_1(t312));
  }
  if ((!t308) && u_is_con(s307, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s307, 0);
    u_value t315 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e316 = &t315;
    u_value l317 = v_ts;
    while (u_is_con(l317, &n_list_46_Cons)) {
      u_value v_a = u_field(l317, 0);
      if (u_truth(u_mk_bool(1))) {
        e316 = u_append(e316, f_check_46_free_1(v_a));
      }
      l317 = u_field(l317, 1);
    }
    t308 = f_list_46_distinct_1(f_list_46_concat_1(t315));
  }
  if (!t308) {
    u_no_case(s307);
  }
  return t308;
}

u_value f_check_46_holes_1(u_value v_t) {
  u_value s318 = v_t;
  u_value t319 = NULL;
  if ((!t319) && u_is_con(s318, &n_syntax_46_THole)) {
    t319 = u_mk_bool(1);
  }
  if ((!t319) && u_is_con(s318, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s318, 1);
    u_value t320 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e321 = &t320;
    u_value l322 = v_args;
    while (u_is_con(l322, &n_list_46_Cons)) {
      u_value v_a = u_field(l322, 0);
      if (u_truth(u_mk_bool(1))) {
        e321 = u_append(e321, f_check_46_holes_1(v_a));
      }
      l322 = u_field(l322, 1);
    }
    t319 = f_list_46_contains_2(u_mk_bool(1), t320);
  }
  if ((!t319) && u_is_con(s318, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s318, 0);
    u_value v_r = u_field(s318, 1);
    u_value t323 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e324 = &t323;
    u_value l325 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l325, &n_list_46_Cons)) {
      u_value v_a = u_field(l325, 0);
      if (u_truth(u_mk_bool(1))) {
        e324 = u_append(e324, f_check_46_holes_1(v_a));
      }
      l325 = u_field(l325, 1);
    }
    t319 = f_list_46_contains_2(u_mk_bool(1), t323);
  }
  if ((!t319) && u_is_con(s318, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s318, 0);
    u_value t326 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e327 = &t326;
    u_value l328 = v_ts;
    while (u_is_con(l328, &n_list_46_Cons)) {
      u_value v_a = u_field(l328, 0);
      if (u_truth(u_mk_bool(1))) {
        e327 = u_append(e327, f_check_46_holes_1(v_a));
      }
      l328 = u_field(l328, 1);
    }
    t319 = f_list_46_contains_2(u_mk_bool(1), t326);
  }
  if (!t319) {
    t319 = u_mk_bool(0);
  }
  if (!t319) {
    u_no_case(s318);
  }
  return t319;
}

u_value f_check_46_subst_2(u_value v_t, u_value v_m) {
  u_value s329 = v_t;
  u_value t330 = NULL;
  if ((!t330) && u_is_con(s329, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s329, 0);
    u_value v_args = u_field(s329, 1);
    u_value t331 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e332 = &t331;
    u_value l333 = v_args;
    while (u_is_con(l333, &n_list_46_Cons)) {
      u_value v_a = u_field(l333, 0);
      if (u_truth(u_mk_bool(1))) {
        e332 = u_append(e332, f_check_46_subst_2(v_a, v_m));
      }
      l333 = u_field(l333, 1);
    }
    t330 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t331);
  }
  if ((!t330) && u_is_con(s329, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s329, 0);
    u_value v_r = u_field(s329, 1);
    u_value t334 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e335 = &t334;
    u_value l336 = v_args;
    while (u_is_con(l336, &n_list_46_Cons)) {
      u_value v_a = u_field(l336, 0);
      if (u_truth(u_mk_bool(1))) {
        e335 = u_append(e335, f_check_46_subst_2(v_a, v_m));
      }
      l336 = u_field(l336, 1);
    }
    t330 = u_mk_con(&n_syntax_46_TFun, 2, t334, f_check_46_subst_2(v_r, v_m));
  }
  if ((!t330) && u_is_con(s329, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s329, 0);
    u_value t337 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e338 = &t337;
    u_value l339 = v_ts;
    while (u_is_con(l339, &n_list_46_Cons)) {
      u_value v_a = u_field(l339, 0);
      if (u_truth(u_mk_bool(1))) {
        e338 = u_append(e338, f_check_46_subst_2(v_a, v_m));
      }
      l339 = u_field(l339, 1);
    }
    t330 = u_mk_con(&n_syntax_46_TTuple, 1, t337);
  }
  if (!t330) {
    u_value s340 = f_list_46_lookup_2(v_t, v_m);
    u_value t341 = NULL;
    if ((!t341) && u_is_con(s340, &n_option_46_Some)) {
      u_value v_u = u_field(s340, 0);
      t341 = v_u;
    }
    if ((!t341) && u_is_con(s340, &n_option_46_None)) {
      t341 = v_t;
    }
    if (!t341) {
      u_no_case(s340);
    }
    t330 = t341;
  }
  if (!t330) {
    u_no_case(s329);
  }
  return t330;
}

u_value f_check_46_resolve_2(u_value v_su, u_value v_t) {
  u_value s342 = v_t;
  u_value t343 = NULL;
  if ((!t343) && u_is_con(s342, &n_syntax_46_TMeta)) {
    u_value v_i = u_field(s342, 0);
    u_value s344 = f_map_46_get_2(v_i, v_su);
    u_value t345 = NULL;
    if ((!t345) && u_is_con(s344, &n_option_46_Some)) {
      u_value v_u = u_field(s344, 0);
      t345 = f_check_46_resolve_2(v_su, v_u);
    }
    if ((!t345) && u_is_con(s344, &n_option_46_None)) {
      t345 = v_t;
    }
    if (!t345) {
      u_no_case(s344);
    }
    t343 = t345;
  }
  if (!t343) {
    t343 = v_t;
  }
  if (!t343) {
    u_no_case(s342);
  }
  return t343;
}

u_value f_check_46_occurs_3(u_value v_su, u_value v_i, u_value v_t) {
  u_value s346 = f_check_46_resolve_2(v_su, v_t);
  u_value t347 = NULL;
  if ((!t347) && u_is_con(s346, &n_syntax_46_TMeta)) {
    u_value v_j = u_field(s346, 0);
    t347 = u_eq(v_i, v_j);
  }
  if ((!t347) && u_is_con(s346, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s346, 1);
    t347 = f_list_46_exists_2(v_args, u_mk_closure(lam348, 2, v_su, v_i));
  }
  if ((!t347) && u_is_con(s346, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s346, 0);
    u_value v_r = u_field(s346, 1);
    u_value t350 = f_list_46_exists_2(v_args, u_mk_closure(lam349, 2, v_su, v_i));
    if (!u_truth(t350)) {
      t350 = f_check_46_occurs_3(v_su, v_i, v_r);
    }
    t347 = t350;
  }
  if ((!t347) && u_is_con(s346, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s346, 0);
    t347 = f_list_46_exists_2(v_ts, u_mk_closure(lam351, 2, v_su, v_i));
  }
  if (!t347) {
    t347 = u_mk_bool(0);
  }
  if (!t347) {
    u_no_case(s346);
  }
  return t347;
}

u_value f_check_46_constructor_scheme_1(u_value v_k) {
  u_value s352 = v_k;
  u_value t353 = NULL;
  if (!t353) {
    u_value v_ps = u_field(s352, 0);
    u_value v_ks = u_field(s352, 1);
    u_value v_r = u_field(s352, 2);
    u_value t354;
    if (u_truth(u_eq(v_ks, u_mk_con(&n_list_46_Nil, 0)))) {
      t354 = v_r;
    } else {
      t354 = f_check_46_fun_2(v_ks, v_r);
    }
    t353 = u_mk_tuple(2, v_ps, t354);
  }
  if (!t353) {
    u_no_case(s352);
  }
  return t353;
}

u_value f_check_46_constructor_of_2(u_value v_c, u_value v_k) {
  u_value s355 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t356 = NULL;
  if ((!t356) && u_is_con(s355, &n_option_46_Some)) {
    u_value v_kc = u_field(s355, 0);
    t356 = f_check_46_constructor_scheme_1(v_kc);
  }
  if ((!t356) && u_is_con(s355, &n_option_46_None)) {
    t356 = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t356) {
    u_no_case(s355);
  }
  return t356;
}

u_value f_check_46_candidates_3(u_value v_s, u_value v_env, u_value v_n) {
  u_value s357 = f_list_46_lookup_2(v_n, v_env);
  u_value t358 = NULL;
  if ((!t358) && u_is_con(s357, &n_option_46_Some)) {
    u_value v_sc = u_field(s357, 0);
    t358 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, v_sc), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t358) && u_is_con(s357, &n_option_46_None)) {
    u_value t359 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e360 = &t359;
    u_value l362 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam361, 1, v_s));
    while (u_is_con(l362, &n_list_46_Cons)) {
      u_value v_f = u_field(l362, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t363 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e364 = &t363;
        u_value l365 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(v_s), v_f);
        while (u_is_con(l365, &n_list_46_Cons)) {
          u_value v_sc = u_field(l365, 0);
          if (u_truth(u_mk_bool(1))) {
            e364 = u_append(e364, u_mk_tuple(2, v_f, v_sc));
          }
          l365 = u_field(l365, 1);
        }
        e360 = u_append(e360, t363);
      }
      l362 = u_field(l362, 1);
    }
    u_value t366 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e367 = &t366;
    u_value l369 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam368, 1, v_s));
    while (u_is_con(l369, &n_list_46_Cons)) {
      u_value v_k = u_field(l369, 0);
      if (u_truth(u_mk_bool(1))) {
        e367 = u_append(e367, u_mk_tuple(2, v_k, f_check_46_constructor_of_2(f_check_46_State_46_ctx_1(v_s), v_k)));
      }
      l369 = u_field(l369, 1);
    }
    u_value t370 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e371 = &t370;
    u_value l372 = f_check_46_builtin_1(v_n);
    while (u_is_con(l372, &n_list_46_Cons)) {
      u_value v_sc = u_field(l372, 0);
      if (u_truth(u_mk_bool(1))) {
        e371 = u_append(e371, u_mk_tuple(2, v_n, v_sc));
      }
      l372 = u_field(l372, 1);
    }
    t358 = f_list_46__43__43__2(f_list_46_concat_1(t359), f_list_46__43__43__2(t366, t370));
  }
  if (!t358) {
    u_no_case(s357);
  }
  return t358;
}

u_value f_check_46_with_arity_2(u_value v_cs, u_value v_k) {
  u_value s373 = v_cs;
  u_value t374 = NULL;
  if ((!t374) && u_is_con(s373, &n_list_46_Nil)) {
    t374 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t374) && u_is_con(s373, &n_list_46_Cons)) {
    u_value v_n = u_field(u_field(s373, 0), 0);
    u_value v_ps = u_field(u_field(u_field(s373, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s373, 0), 1), 1);
    u_value v_rest = u_field(s373, 1);
    u_value t375;
    if (u_truth(u_eq(f_syntax_46_arity_1(v_t), v_k))) {
      t375 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, v_ps, v_t)), f_check_46_with_arity_2(v_rest, v_k));
    } else {
      t375 = f_check_46_with_arity_2(v_rest, v_k);
    }
    t374 = t375;
  }
  if (!t374) {
    u_no_case(s373);
  }
  return t374;
}

u_value f_check_46_has_arity_2(u_value v_scs, u_value v_k) {
  u_value s376 = v_scs;
  u_value t377 = NULL;
  if ((!t377) && u_is_con(s376, &n_list_46_Nil)) {
    t377 = u_mk_bool(0);
  }
  if ((!t377) && u_is_con(s376, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s376, 0), 1);
    u_value v_rest = u_field(s376, 1);
    u_value t378 = u_eq(f_syntax_46_arity_1(v_t), v_k);
    if (!u_truth(t378)) {
      t378 = f_check_46_has_arity_2(v_rest, v_k);
    }
    t377 = t378;
  }
  if (!t377) {
    u_no_case(s376);
  }
  return t377;
}

u_value f_check_46_bind_1(u_value v_xs) {
  u_value t379 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e380 = &t379;
  u_value l381 = v_xs;
  while (u_is_con(l381, &n_list_46_Cons)) {
    u_value v_x = u_field(l381, 0);
    if (u_truth(u_mk_bool(1))) {
      e380 = u_append(e380, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_syntax_46_Var_46_typ_1(v_x))));
    }
    l381 = u_field(l381, 1);
  }
  return t379;
}

u_value f_check_46_fixed_2(u_value v_su, u_value v_env) {
  u_value s382 = v_env;
  u_value t383 = NULL;
  if ((!t383) && u_is_con(s382, &n_list_46_Nil)) {
    t383 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t383) && u_is_con(s382, &n_list_46_Cons)) {
    u_value v_ps = u_field(u_field(u_field(s382, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s382, 0), 1), 1);
    u_value v_rest = u_field(s382, 1);
    t383 = f_list_46__43__43__2(f_list_46_diff_2(f_check_46_free_1(f_check_46_zonk_type_2(v_su, v_t)), v_ps), f_check_46_fixed_2(v_su, v_rest));
  }
  if (!t383) {
    u_no_case(s382);
  }
  return t383;
}

u_value f_check_46_generalize_3(u_value v_su, u_value v_env, u_value v_t) {
  u_value t389 = f_check_46_fixed_2(v_su, v_env);
  u_value t390 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t388;
  {
    u_value v_fx = t389;
    u_value v_u = t390;
    u_value t384 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e385 = &t384;
    u_value l386 = f_check_46_free_1(v_u);
    while (u_is_con(l386, &n_list_46_Cons)) {
      u_value v_m = u_field(l386, 0);
      u_value t387 = f_check_46_is_meta_1(v_m);
      if (u_truth(t387)) {
        t387 = f_prelude_46_not_1(f_list_46_contains_2(v_m, v_fx));
      }
      if (u_truth(t387)) {
        e385 = u_append(e385, v_m);
      }
      l386 = u_field(l386, 1);
    }
    t388 = u_mk_tuple(2, t384, v_u);
  }
  return t388;
}

u_value f_check_46_generalized_3(u_value v_su, u_value v_env, u_value v_xs) {
  u_value t391 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e392 = &t391;
  u_value l393 = v_xs;
  while (u_is_con(l393, &n_list_46_Cons)) {
    u_value v_x = u_field(l393, 0);
    if (u_truth(u_mk_bool(1))) {
      e392 = u_append(e392, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), f_check_46_generalize_3(v_su, v_env, f_syntax_46_Var_46_typ_1(v_x))));
    }
    l393 = u_field(l393, 1);
  }
  return t391;
}

u_value f_check_46_case_of_1(u_value v_b) {
  u_value s394 = v_b;
  u_value t395 = NULL;
  if ((!t395) && u_is_con(s394, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s394, 0);
    u_value v_g = u_field(s394, 1);
    t395 = u_mk_tuple(2, v_p, u_eq(v_g, u_mk_con(&n_option_46_None, 0)));
  }
  if (!t395) {
    u_no_case(s394);
  }
  return t395;
}

u_value f_check_46_zonk_guard_2(u_value v_su, u_value v_g) {
  u_value s396 = v_g;
  u_value t397 = NULL;
  if ((!t397) && u_is_con(s396, &n_option_46_Some)) {
    u_value v_e = u_field(s396, 0);
    t397 = u_mk_con(&n_option_46_Some, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t397) && u_is_con(s396, &n_option_46_None)) {
    t397 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t397) {
    u_no_case(s396);
  }
  return t397;
}

u_value f_check_46_zonk_type_2(u_value v_su, u_value v_t) {
  u_value s398 = f_check_46_resolve_2(v_su, v_t);
  u_value t399 = NULL;
  if ((!t399) && u_is_con(s398, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s398, 0);
    u_value v_args = u_field(s398, 1);
    u_value t400 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e401 = &t400;
    u_value l402 = v_args;
    while (u_is_con(l402, &n_list_46_Cons)) {
      u_value v_a = u_field(l402, 0);
      if (u_truth(u_mk_bool(1))) {
        e401 = u_append(e401, f_check_46_zonk_type_2(v_su, v_a));
      }
      l402 = u_field(l402, 1);
    }
    t399 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t400);
  }
  if ((!t399) && u_is_con(s398, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s398, 0);
    u_value v_r = u_field(s398, 1);
    u_value t403 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e404 = &t403;
    u_value l405 = v_args;
    while (u_is_con(l405, &n_list_46_Cons)) {
      u_value v_a = u_field(l405, 0);
      if (u_truth(u_mk_bool(1))) {
        e404 = u_append(e404, f_check_46_zonk_type_2(v_su, v_a));
      }
      l405 = u_field(l405, 1);
    }
    t399 = u_mk_con(&n_syntax_46_TFun, 2, t403, f_check_46_zonk_type_2(v_su, v_r));
  }
  if ((!t399) && u_is_con(s398, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s398, 0);
    u_value t406 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e407 = &t406;
    u_value l408 = v_ts;
    while (u_is_con(l408, &n_list_46_Cons)) {
      u_value v_a = u_field(l408, 0);
      if (u_truth(u_mk_bool(1))) {
        e407 = u_append(e407, f_check_46_zonk_type_2(v_su, v_a));
      }
      l408 = u_field(l408, 1);
    }
    t399 = u_mk_con(&n_syntax_46_TTuple, 1, t406);
  }
  if (!t399) {
    u_value v_u = s398;
    t399 = v_u;
  }
  if (!t399) {
    u_no_case(s398);
  }
  return t399;
}

u_value f_check_46_zonk_var_2(u_value v_su, u_value v_x) {
  return f_syntax_46_Var_46_typ_58__61__2(v_x, f_check_46_zonk_type_2(v_su, f_syntax_46_Var_46_typ_1(v_x)));
}

u_value f_check_46_zonk_vars_2(u_value v_su, u_value v_xs) {
  u_value t409 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e410 = &t409;
  u_value l411 = v_xs;
  while (u_is_con(l411, &n_list_46_Cons)) {
    u_value v_x = u_field(l411, 0);
    if (u_truth(u_mk_bool(1))) {
      e410 = u_append(e410, f_check_46_zonk_var_2(v_su, v_x));
    }
    l411 = u_field(l411, 1);
  }
  return t409;
}

u_value f_check_46_zonk_expr_2(u_value v_su, u_value v_e) {
  u_value s412 = v_e;
  u_value t413 = NULL;
  if ((!t413) && u_is_con(s412, &n_syntax_46_ELit)) {
    t413 = v_e;
  }
  if ((!t413) && u_is_con(s412, &n_syntax_46_EVar)) {
    u_value v_x = u_field(s412, 0);
    t413 = u_mk_con(&n_syntax_46_EVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t413) && u_is_con(s412, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s412, 0);
    u_value v_args = u_field(s412, 1);
    u_value t414 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e415 = &t414;
    u_value l416 = v_args;
    while (u_is_con(l416, &n_list_46_Cons)) {
      u_value v_a = u_field(l416, 0);
      if (u_truth(u_mk_bool(1))) {
        e415 = u_append(e415, f_check_46_zonk_expr_2(v_su, v_a));
      }
      l416 = u_field(l416, 1);
    }
    t413 = u_mk_con(&n_syntax_46_EApp, 2, f_check_46_zonk_expr_2(v_su, v_f), t414);
  }
  if ((!t413) && u_is_con(s412, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s412, 0);
    u_value v_b = u_field(s412, 1);
    t413 = u_mk_con(&n_syntax_46_ELam, 2, f_check_46_zonk_vars_2(v_su, v_xs), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t413) && u_is_con(s412, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s412, 0);
    u_value v_b = u_field(s412, 1);
    u_value t417 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e418 = &t417;
    u_value l419 = v_ps;
    while (u_is_con(l419, &n_list_46_Cons)) {
      u_value v_p = u_field(l419, 0);
      if (u_truth(u_mk_bool(1))) {
        e418 = u_append(e418, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l419 = u_field(l419, 1);
    }
    t413 = u_mk_con(&n_syntax_46_ELet, 2, t417, f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t413) && u_is_con(s412, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s412, 0);
    u_value v_l = u_field(s412, 1);
    u_value v_r = u_field(s412, 2);
    t413 = u_mk_con(&n_syntax_46_EIte, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_expr_2(v_su, v_l), f_check_46_zonk_expr_2(v_su, v_r));
  }
  if ((!t413) && u_is_con(s412, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s412, 0);
    u_value v_bs = u_field(s412, 1);
    u_value t420 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e421 = &t420;
    u_value l422 = v_bs;
    while (u_is_con(l422, &n_list_46_Cons)) {
      u_value v_b = u_field(l422, 0);
      if (u_truth(u_mk_bool(1))) {
        e421 = u_append(e421, f_check_46_zonk_expr_branch_2(v_su, v_b));
      }
      l422 = u_field(l422, 1);
    }
    t413 = u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t420);
  }
  if ((!t413) && u_is_con(s412, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s412, 0);
    u_value v_xs = u_field(s412, 1);
    u_value v_ls = u_field(s412, 2);
    u_value v_c = u_field(s412, 3);
    u_value t423 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e424 = &t423;
    u_value l425 = v_ls;
    while (u_is_con(l425, &n_list_46_Cons)) {
      u_value v_l = u_field(l425, 0);
      if (u_truth(u_mk_bool(1))) {
        e424 = u_append(e424, f_check_46_zonk_expr_2(v_su, v_l));
      }
      l425 = u_field(l425, 1);
    }
    t413 = u_mk_con(&n_syntax_46_EComp, 4, f_check_46_zonk_expr_2(v_su, v_r), f_check_46_zonk_vars_2(v_su, v_xs), t423, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t413) && u_is_con(s412, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s412, 0);
    u_value t426 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e427 = &t426;
    u_value l428 = v_es;
    while (u_is_con(l428, &n_list_46_Cons)) {
      u_value v_e = u_field(l428, 0);
      if (u_truth(u_mk_bool(1))) {
        e427 = u_append(e427, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l428 = u_field(l428, 1);
    }
    t413 = u_mk_con(&n_syntax_46_ETuple, 1, t426);
  }
  if ((!t413) && u_is_con(s412, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s412, 0);
    u_value v_t = u_field(s412, 1);
    t413 = u_mk_con(&n_syntax_46_ERaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t413) {
    u_no_case(s412);
  }
  return t413;
}

u_value f_check_46_zonk_binding_2(u_value v_su, u_value v_p) {
  u_value s429 = v_p;
  u_value t430 = NULL;
  if (!t430) {
    u_value v_x = u_field(s429, 0);
    u_value v_e = u_field(s429, 1);
    t430 = u_mk_tuple(2, f_check_46_zonk_var_2(v_su, v_x), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t430) {
    u_no_case(s429);
  }
  return t430;
}

u_value f_check_46_zonk_expr_branch_2(u_value v_su, u_value v_b) {
  u_value s431 = v_b;
  u_value t432 = NULL;
  if ((!t432) && u_is_con(s431, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s431, 0);
    u_value v_g = u_field(s431, 1);
    u_value v_e = u_field(s431, 2);
    t432 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t432) {
    u_no_case(s431);
  }
  return t432;
}

u_value f_check_46_zonk_pat_2(u_value v_su, u_value v_p) {
  u_value s433 = v_p;
  u_value t434 = NULL;
  if ((!t434) && u_is_con(s433, &n_syntax_46_PVar)) {
    u_value v_x = u_field(s433, 0);
    t434 = u_mk_con(&n_syntax_46_PVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t434) && u_is_con(s433, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s433, 0);
    u_value v_args = u_field(s433, 1);
    u_value t435 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e436 = &t435;
    u_value l437 = v_args;
    while (u_is_con(l437, &n_list_46_Cons)) {
      u_value v_a = u_field(l437, 0);
      if (u_truth(u_mk_bool(1))) {
        e436 = u_append(e436, f_check_46_zonk_pat_2(v_su, v_a));
      }
      l437 = u_field(l437, 1);
    }
    t434 = u_mk_con(&n_syntax_46_PApp, 2, v_n, t435);
  }
  if ((!t434) && u_is_con(s433, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s433, 0);
    u_value t438 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e439 = &t438;
    u_value l440 = v_ps;
    while (u_is_con(l440, &n_list_46_Cons)) {
      u_value v_q = u_field(l440, 0);
      if (u_truth(u_mk_bool(1))) {
        e439 = u_append(e439, f_check_46_zonk_pat_2(v_su, v_q));
      }
      l440 = u_field(l440, 1);
    }
    t434 = u_mk_con(&n_syntax_46_PTuple, 1, t438);
  }
  if (!t434) {
    t434 = v_p;
  }
  if (!t434) {
    u_no_case(s433);
  }
  return t434;
}

u_value f_check_46_zonk_stmt_2(u_value v_su, u_value v_x) {
  u_value s441 = v_x;
  u_value t442 = NULL;
  if ((!t442) && u_is_con(s441, &n_syntax_46_SSkip)) {
    t442 = v_x;
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s441, 0);
    u_value t443 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e444 = &t443;
    u_value l445 = v_ps;
    while (u_is_con(l445, &n_list_46_Cons)) {
      u_value v_p = u_field(l445, 0);
      if (u_truth(u_mk_bool(1))) {
        e444 = u_append(e444, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l445 = u_field(l445, 1);
    }
    t442 = u_mk_con(&n_syntax_46_SAssign, 1, t443);
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s441, 0);
    u_value v_ys = u_field(s441, 1);
    u_value t446 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e447 = &t446;
    u_value l448 = v_ys;
    while (u_is_con(l448, &n_list_46_Cons)) {
      u_value v_y = u_field(l448, 0);
      if (u_truth(u_mk_bool(1))) {
        e447 = u_append(e447, f_check_46_zonk_stmt_2(v_su, v_y));
      }
      l448 = u_field(l448, 1);
    }
    t442 = u_mk_con(&n_syntax_46_SBlock, 2, f_check_46_zonk_vars_2(v_su, v_xs), t446);
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s441, 0);
    u_value v_l = u_field(s441, 1);
    u_value v_r = u_field(s441, 2);
    t442 = u_mk_con(&n_syntax_46_SIf, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_l), f_check_46_zonk_stmt_2(v_su, v_r));
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s441, 0);
    u_value v_b = u_field(s441, 1);
    t442 = u_mk_con(&n_syntax_46_SWhile, 2, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s441, 0);
    u_value v_ins = u_field(s441, 1);
    u_value v_outs = u_field(s441, 2);
    u_value v_refs = u_field(s441, 3);
    u_value t449 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e450 = &t449;
    u_value l451 = v_ins;
    while (u_is_con(l451, &n_list_46_Cons)) {
      u_value v_e = u_field(l451, 0);
      if (u_truth(u_mk_bool(1))) {
        e450 = u_append(e450, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l451 = u_field(l451, 1);
    }
    t442 = u_mk_con(&n_syntax_46_SCall, 4, v_p, t449, f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs));
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s441, 0);
    u_value v_bs = u_field(s441, 1);
    u_value t452 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e453 = &t452;
    u_value l454 = v_bs;
    while (u_is_con(l454, &n_list_46_Cons)) {
      u_value v_b = u_field(l454, 0);
      if (u_truth(u_mk_bool(1))) {
        e453 = u_append(e453, f_check_46_zonk_stmt_branch_2(v_su, v_b));
      }
      l454 = u_field(l454, 1);
    }
    t442 = u_mk_con(&n_syntax_46_SMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t452);
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s441, 0);
    t442 = u_mk_con(&n_syntax_46_SAssert, 1, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s441, 0);
    u_value v_t = u_field(s441, 1);
    t442 = u_mk_con(&n_syntax_46_SRaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_STry)) {
    u_value v_b = u_field(s441, 0);
    u_value v_hs = u_field(s441, 1);
    u_value t455 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e456 = &t455;
    u_value l457 = v_hs;
    while (u_is_con(l457, &n_list_46_Cons)) {
      u_value v_h = u_field(l457, 0);
      if (u_truth(u_mk_bool(1))) {
        e456 = u_append(e456, f_check_46_zonk_handler_2(v_su, v_h));
      }
      l457 = u_field(l457, 1);
    }
    t442 = u_mk_con(&n_syntax_46_STry, 2, f_check_46_zonk_stmt_2(v_su, v_b), t455);
  }
  if (!t442) {
    u_no_case(s441);
  }
  return t442;
}

u_value f_check_46_zonk_stmt_branch_2(u_value v_su, u_value v_b) {
  u_value s458 = v_b;
  u_value t459 = NULL;
  if ((!t459) && u_is_con(s458, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s458, 0);
    u_value v_g = u_field(s458, 1);
    u_value v_y = u_field(s458, 2);
    t459 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_y));
  }
  if (!t459) {
    u_no_case(s458);
  }
  return t459;
}

u_value f_check_46_zonk_handler_2(u_value v_su, u_value v_h) {
  u_value s460 = v_h;
  u_value t461 = NULL;
  if ((!t461) && u_is_con(s460, &n_syntax_46_Handler)) {
    u_value v_t = u_field(s460, 0);
    u_value v_p = u_field(s460, 1);
    u_value v_g = u_field(s460, 2);
    u_value v_b = u_field(s460, 3);
    t461 = u_mk_con(&n_syntax_46_Handler, 4, f_check_46_zonk_type_2(v_su, v_t), f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t461) {
    u_no_case(s460);
  }
  return t461;
}

u_value f_check_46_zonk_cmd_2(u_value v_su, u_value v_c) {
  u_value s462 = v_c;
  u_value t463 = NULL;
  if ((!t463) && u_is_con(s462, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s462, 0);
    u_value v_ps = u_field(s462, 1);
    u_value v_xss = u_field(s462, 2);
    u_value v_r = u_field(s462, 3);
    u_value v_b = u_field(s462, 4);
    u_value t464 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e465 = &t464;
    u_value l466 = v_xss;
    while (u_is_con(l466, &n_list_46_Cons)) {
      u_value v_xs = u_field(l466, 0);
      if (u_truth(u_mk_bool(1))) {
        e465 = u_append(e465, f_check_46_zonk_vars_2(v_su, v_xs));
      }
      l466 = u_field(l466, 1);
    }
    t463 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, t464, f_check_46_zonk_type_2(v_su, v_r), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t463) && u_is_con(s462, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s462, 0);
    u_value v_ps = u_field(s462, 1);
    u_value v_ins = u_field(s462, 2);
    u_value v_outs = u_field(s462, 3);
    u_value v_refs = u_field(s462, 4);
    u_value v_b = u_field(s462, 5);
    t463 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, f_check_46_zonk_vars_2(v_su, v_ins), f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t463) && u_is_con(s462, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s462, 0);
    t463 = u_mk_con(&n_syntax_46_CEval, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t463) && u_is_con(s462, &n_syntax_46_CRun)) {
    u_value v_n = u_field(s462, 0);
    u_value v_b = u_field(s462, 1);
    t463 = u_mk_con(&n_syntax_46_CRun, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t463) && u_is_con(s462, &n_syntax_46_CTest)) {
    u_value v_n = u_field(s462, 0);
    u_value v_b = u_field(s462, 1);
    t463 = u_mk_con(&n_syntax_46_CTest, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t463) {
    t463 = v_c;
  }
  if (!t463) {
    u_no_case(s462);
  }
  return t463;
}

u_value f_check_46_unknown_type_1(u_value v_t) {
  u_value t467;
  if (u_truth(u_eq(f_check_46_free_1(v_t), u_mk_con(&n_list_46_Nil, 0)))) {
    t467 = u_mk_con(&n_option_46_None, 0);
  } else {
    t467 = u_mk_con(&n_option_46_Some, 1, v_t);
  }
  return t467;
}

u_value f_check_46_unknown_in_1(u_value v_x) {
  u_value s468 = v_x;
  u_value t469 = NULL;
  if ((!t469) && u_is_con(s468, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s468, 0);
    t469 = f_check_46_unknown_in_exprs_1(f_list_46_seconds_1(v_ps));
  }
  if ((!t469) && u_is_con(s468, &n_syntax_46_SBlock)) {
    u_value v_ys = u_field(s468, 1);
    t469 = f_check_46_unknown_in_all_1(v_ys);
  }
  if ((!t469) && u_is_con(s468, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s468, 0);
    u_value v_l = u_field(s468, 1);
    u_value v_r = u_field(s468, 2);
    t469 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_option_46_or_else_2(f_check_46_unknown_in_1(v_l), f_check_46_unknown_in_1(v_r)));
  }
  if ((!t469) && u_is_con(s468, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s468, 0);
    u_value v_b = u_field(s468, 1);
    t469 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_check_46_unknown_in_1(v_b));
  }
  if ((!t469) && u_is_con(s468, &n_syntax_46_SCall)) {
    u_value v_ins = u_field(s468, 1);
    t469 = f_check_46_unknown_in_exprs_1(v_ins);
  }
  if ((!t469) && u_is_con(s468, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s468, 0);
    u_value v_bs = u_field(s468, 1);
    t469 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_a), f_check_46_unknown_in_branches_1(v_bs));
  }
  if ((!t469) && u_is_con(s468, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s468, 0);
    t469 = f_check_46_unknown_in_expr_1(v_c);
  }
  if ((!t469) && u_is_con(s468, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s468, 0);
    u_value v_t = u_field(s468, 1);
    t469 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t469) && u_is_con(s468, &n_syntax_46_STry)) {
    u_value v_b = u_field(s468, 0);
    u_value v_hs = u_field(s468, 1);
    t469 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_hs));
  }
  if (!t469) {
    t469 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t469) {
    u_no_case(s468);
  }
  return t469;
}

u_value f_check_46_unknown_in_expr_1(u_value v_e) {
  u_value s470 = v_e;
  u_value t471 = NULL;
  if ((!t471) && u_is_con(s470, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s470, 0);
    u_value v_t = u_field(s470, 1);
    t471 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t471) && u_is_con(s470, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s470, 0);
    u_value v_args = u_field(s470, 1);
    t471 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_f, v_args));
  }
  if ((!t471) && u_is_con(s470, &n_syntax_46_ELam)) {
    u_value v_b = u_field(s470, 1);
    t471 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t471) && u_is_con(s470, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s470, 0);
    u_value v_b = u_field(s470, 1);
    t471 = f_check_46_unknown_in_exprs_1(f_list_46__43__43__2(f_list_46_seconds_1(v_ps), u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t471) && u_is_con(s470, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s470, 0);
    u_value v_l = u_field(s470, 1);
    u_value v_r = u_field(s470, 2);
    t471 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)))));
  }
  if ((!t471) && u_is_con(s470, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s470, 0);
    u_value v_bs = u_field(s470, 1);
    u_value t472 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e473 = &t472;
    u_value l474 = v_bs;
    while (u_is_con(l474, &n_list_46_Cons)) {
      u_value v_b = u_field(l474, 0);
      if (u_truth(u_mk_bool(1))) {
        e473 = u_append(e473, f_check_46_branch_exprs_1(v_b));
      }
      l474 = u_field(l474, 1);
    }
    t471 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_a, f_list_46_concat_1(t472)));
  }
  if ((!t471) && u_is_con(s470, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s470, 0);
    u_value v_ls = u_field(s470, 2);
    u_value v_c = u_field(s470, 3);
    t471 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Cons, 2, v_c, v_ls)));
  }
  if ((!t471) && u_is_con(s470, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s470, 0);
    t471 = f_check_46_unknown_in_exprs_1(v_es);
  }
  if (!t471) {
    t471 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t471) {
    u_no_case(s470);
  }
  return t471;
}

u_value f_check_46_unknown_in_exprs_1(u_value v_es) {
  u_value s475 = v_es;
  u_value t476 = NULL;
  if ((!t476) && u_is_con(s475, &n_list_46_Nil)) {
    t476 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t476) && u_is_con(s475, &n_list_46_Cons)) {
    u_value v_e = u_field(s475, 0);
    u_value v_rest = u_field(s475, 1);
    t476 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_e), f_check_46_unknown_in_exprs_1(v_rest));
  }
  if (!t476) {
    u_no_case(s475);
  }
  return t476;
}

u_value f_check_46_branch_exprs_1(u_value v_b) {
  u_value s477 = v_b;
  u_value t478 = NULL;
  if ((!t478) && (u_is_con(s477, &n_syntax_46_Branch) && u_is_con(u_field(s477, 1), &n_option_46_Some))) {
    u_value v_g = u_field(u_field(s477, 1), 0);
    u_value v_e = u_field(s477, 2);
    t478 = u_mk_con(&n_list_46_Cons, 2, v_g, u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t478) && (u_is_con(s477, &n_syntax_46_Branch) && u_is_con(u_field(s477, 1), &n_option_46_None))) {
    u_value v_e = u_field(s477, 2);
    t478 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t478) {
    u_no_case(s477);
  }
  return t478;
}

u_value f_check_46_unknown_in_all_1(u_value v_xs) {
  u_value s479 = v_xs;
  u_value t480 = NULL;
  if ((!t480) && u_is_con(s479, &n_list_46_Nil)) {
    t480 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t480) && u_is_con(s479, &n_list_46_Cons)) {
    u_value v_y = u_field(s479, 0);
    u_value v_ys = u_field(s479, 1);
    t480 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_all_1(v_ys));
  }
  if (!t480) {
    u_no_case(s479);
  }
  return t480;
}

u_value f_check_46_unknown_in_branches_1(u_value v_bs) {
  u_value s481 = v_bs;
  u_value t482 = NULL;
  if ((!t482) && u_is_con(s481, &n_list_46_Nil)) {
    t482 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t482) && (u_is_con(s481, &n_list_46_Cons) && (u_is_con(u_field(s481, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s481, 0), 1), &n_option_46_Some)))) {
    u_value v_g = u_field(u_field(u_field(s481, 0), 1), 0);
    u_value v_y = u_field(u_field(s481, 0), 2);
    u_value v_rest = u_field(s481, 1);
    t482 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_g), f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest)));
  }
  if ((!t482) && (u_is_con(s481, &n_list_46_Cons) && (u_is_con(u_field(s481, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s481, 0), 1), &n_option_46_None)))) {
    u_value v_y = u_field(u_field(s481, 0), 2);
    u_value v_rest = u_field(s481, 1);
    t482 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest));
  }
  if (!t482) {
    u_no_case(s481);
  }
  return t482;
}

u_value f_check_46_unknown_in_handlers_1(u_value v_hs) {
  u_value s483 = v_hs;
  u_value t484 = NULL;
  if ((!t484) && u_is_con(s483, &n_list_46_Nil)) {
    t484 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t484) && (u_is_con(s483, &n_list_46_Cons) && (u_is_con(u_field(s483, 0), &n_syntax_46_Handler) && u_is_con(u_field(u_field(s483, 0), 1), &n_syntax_46_PWild)))) {
    u_value v_b = u_field(u_field(s483, 0), 3);
    u_value v_rest = u_field(s483, 1);
    t484 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest));
  }
  if ((!t484) && (u_is_con(s483, &n_list_46_Cons) && u_is_con(u_field(s483, 0), &n_syntax_46_Handler))) {
    u_value v_t = u_field(u_field(s483, 0), 0);
    u_value v_b = u_field(u_field(s483, 0), 3);
    u_value v_rest = u_field(s483, 1);
    t484 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest)));
  }
  if (!t484) {
    u_no_case(s483);
  }
  return t484;
}

u_value f_check_46_unknown_in_cmd_1(u_value v_c) {
  u_value s485 = v_c;
  u_value t486 = NULL;
  if ((!t486) && u_is_con(s485, &n_syntax_46_CFun)) {
    u_value v_b = u_field(s485, 4);
    t486 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t486) && u_is_con(s485, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s485, 0);
    t486 = f_check_46_unknown_in_expr_1(v_e);
  }
  if ((!t486) && u_is_con(s485, &n_syntax_46_CProc)) {
    u_value v_b = u_field(s485, 5);
    t486 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t486) && u_is_con(s485, &n_syntax_46_CRun)) {
    u_value v_b = u_field(s485, 1);
    t486 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t486) && u_is_con(s485, &n_syntax_46_CTest)) {
    u_value v_b = u_field(s485, 1);
    t486 = f_check_46_unknown_in_1(v_b);
  }
  if (!t486) {
    t486 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t486) {
    u_no_case(s485);
  }
  return t486;
}

u_value f_check_46_head_of_1(u_value v_p) {
  u_value s487 = v_p;
  u_value t488 = NULL;
  if ((!t488) && u_is_con(s487, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s487, 0);
    u_value v_args = u_field(s487, 1);
    t488 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_n), v_args));
  }
  if ((!t488) && u_is_con(s487, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s487, 0);
    t488 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ps));
  }
  if ((!t488) && u_is_con(s487, &n_syntax_46_PLit)) {
    u_value v_v = u_field(s487, 0);
    t488 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, v_v), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t488) {
    t488 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t488) {
    u_no_case(s487);
  }
  return t488;
}

u_value f_check_46_heads_1(u_value v_rows) {
  u_value s489 = v_rows;
  u_value t490 = NULL;
  if ((!t490) && u_is_con(s489, &n_list_46_Nil)) {
    t490 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t490) && (u_is_con(s489, &n_list_46_Cons) && u_is_con(u_field(s489, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s489, 0), 0);
    u_value v_rest = u_field(s489, 1);
    u_value s491 = f_check_46_head_of_1(v_p);
    u_value t492 = NULL;
    if ((!t492) && u_is_con(s491, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s491, 0), 0);
      t492 = u_mk_con(&n_list_46_Cons, 2, v_h, f_check_46_heads_1(v_rest));
    }
    if ((!t492) && u_is_con(s491, &n_option_46_None)) {
      t492 = f_check_46_heads_1(v_rest);
    }
    if (!t492) {
      u_no_case(s491);
    }
    t490 = t492;
  }
  if ((!t490) && (u_is_con(s489, &n_list_46_Cons) && u_is_con(u_field(s489, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s489, 1);
    t490 = f_check_46_heads_1(v_rest);
  }
  if (!t490) {
    u_no_case(s489);
  }
  return t490;
}

u_value f_check_46_field_types_3(u_value v_c, u_value v_k, u_value v_args) {
  u_value s493 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t494 = NULL;
  if ((!t494) && u_is_con(s493, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s493, 0), 0);
    u_value v_ks = u_field(u_field(s493, 0), 1);
    u_value t495 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e496 = &t495;
    u_value l497 = v_ks;
    while (u_is_con(l497, &n_list_46_Cons)) {
      u_value v_a = u_field(l497, 0);
      if (u_truth(u_mk_bool(1))) {
        e496 = u_append(e496, f_check_46_subst_2(v_a, f_list_46_zip_2(v_ps, v_args)));
      }
      l497 = u_field(l497, 1);
    }
    t494 = t495;
  }
  if ((!t494) && u_is_con(s493, &n_option_46_None)) {
    t494 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t494) {
    u_no_case(s493);
  }
  return t494;
}

u_value f_check_46_constructors_of_3(u_value v_c, u_value v_su, u_value v_t) {
  u_value s498 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t499 = NULL;
  if ((!t499) && (u_is_con(s498, &n_syntax_46_TSort) && (u_is_con(u_field(s498, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s498, 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(s498, 0), 1), u_mk_lit("Bool")) && u_is_con(u_field(s498, 1), &n_list_46_Nil)))))) {
    t499 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t499) && u_is_con(s498, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s498, 0);
    t499 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ts), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t499) && u_is_con(s498, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s498, 0);
    u_value v_args = u_field(s498, 1);
    u_value s500 = f_ctx_46_members_of_2(v_c, v_n);
    u_value t501 = NULL;
    if ((!t501) && u_is_con(s500, &n_option_46_Some)) {
      u_value v_ks = u_field(s500, 0);
      u_value t502 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e503 = &t502;
      u_value l504 = v_ks;
      while (u_is_con(l504, &n_list_46_Cons)) {
        u_value v_k = u_field(l504, 0);
        if (u_truth(u_mk_bool(1))) {
          e503 = u_append(e503, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_k), f_check_46_field_types_3(v_c, v_k, v_args)));
        }
        l504 = u_field(l504, 1);
      }
      t501 = u_mk_con(&n_option_46_Some, 1, t502);
    }
    if ((!t501) && u_is_con(s500, &n_option_46_None)) {
      t501 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t501) {
      u_no_case(s500);
    }
    t499 = t501;
  }
  if (!t499) {
    t499 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t499) {
    u_no_case(s498);
  }
  return t499;
}

u_value f_check_46_fields_of_4(u_value v_c, u_value v_su, u_value v_t, u_value v_h) {
  u_value s505 = f_check_46_constructors_of_3(v_c, v_su, v_t);
  u_value t506 = NULL;
  if ((!t506) && u_is_con(s505, &n_option_46_Some)) {
    u_value v_ks = u_field(s505, 0);
    u_value s507 = f_list_46_lookup_2(v_h, v_ks);
    u_value t508 = NULL;
    if ((!t508) && u_is_con(s507, &n_option_46_Some)) {
      u_value v_ts = u_field(s507, 0);
      t508 = v_ts;
    }
    if ((!t508) && u_is_con(s507, &n_option_46_None)) {
      t508 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t508) {
      u_no_case(s507);
    }
    t506 = t508;
  }
  if ((!t506) && u_is_con(s505, &n_option_46_None)) {
    t506 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t506) {
    u_no_case(s505);
  }
  return t506;
}

u_value f_check_46_covers_2(u_value v_hs, u_value v_ks) {
  u_value s509 = v_ks;
  u_value t510 = NULL;
  if ((!t510) && u_is_con(s509, &n_list_46_Nil)) {
    t510 = u_mk_bool(1);
  }
  if ((!t510) && u_is_con(s509, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s509, 0), 0);
    u_value v_rest = u_field(s509, 1);
    u_value t511 = f_list_46_contains_2(v_h, v_hs);
    if (u_truth(t511)) {
      t511 = f_check_46_covers_2(v_hs, v_rest);
    }
    t510 = t511;
  }
  if (!t510) {
    u_no_case(s509);
  }
  return t510;
}

u_value f_check_46_defaults_1(u_value v_rows) {
  u_value s512 = v_rows;
  u_value t513 = NULL;
  if ((!t513) && u_is_con(s512, &n_list_46_Nil)) {
    t513 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t513) && (u_is_con(s512, &n_list_46_Cons) && u_is_con(u_field(s512, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s512, 0), 0);
    u_value v_ps = u_field(u_field(s512, 0), 1);
    u_value v_rest = u_field(s512, 1);
    u_value s514 = f_check_46_head_of_1(v_p);
    u_value t515 = NULL;
    if ((!t515) && u_is_con(s514, &n_option_46_Some)) {
      t515 = f_check_46_defaults_1(v_rest);
    }
    if ((!t515) && u_is_con(s514, &n_option_46_None)) {
      t515 = u_mk_con(&n_list_46_Cons, 2, v_ps, f_check_46_defaults_1(v_rest));
    }
    if (!t515) {
      u_no_case(s514);
    }
    t513 = t515;
  }
  if ((!t513) && (u_is_con(s512, &n_list_46_Cons) && u_is_con(u_field(s512, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s512, 1);
    t513 = f_check_46_defaults_1(v_rest);
  }
  if (!t513) {
    u_no_case(s512);
  }
  return t513;
}

u_value f_check_46_specialize_3(u_value v_rows, u_value v_h, u_value v_n) {
  u_value s516 = v_rows;
  u_value t517 = NULL;
  if ((!t517) && u_is_con(s516, &n_list_46_Nil)) {
    t517 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t517) && (u_is_con(s516, &n_list_46_Cons) && u_is_con(u_field(s516, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s516, 0), 0);
    u_value v_ps = u_field(u_field(s516, 0), 1);
    u_value v_rest = u_field(s516, 1);
    u_value s518 = f_check_46_head_of_1(v_p);
    u_value t519 = NULL;
    if ((!t519) && u_is_con(s518, &n_option_46_Some)) {
      u_value v_g = u_field(u_field(s518, 0), 0);
      u_value v_args = u_field(u_field(s518, 0), 1);
      u_value t520;
      if (u_truth(u_eq(v_g, v_h))) {
        t520 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(v_args, v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
      } else {
        t520 = f_check_46_specialize_3(v_rest, v_h, v_n);
      }
      t519 = t520;
    }
    if ((!t519) && u_is_con(s518, &n_option_46_None)) {
      t519 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(f_list_46_replicate_2(v_n, u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
    }
    if (!t519) {
      u_no_case(s518);
    }
    t517 = t519;
  }
  if ((!t517) && (u_is_con(s516, &n_list_46_Cons) && u_is_con(u_field(s516, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s516, 1);
    t517 = f_check_46_specialize_3(v_rest, v_h, v_n);
  }
  if (!t517) {
    u_no_case(s516);
  }
  return t517;
}

u_value f_check_46_useful_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_q, u_value v_ts) {
  u_value s521 = u_mk_tuple(2, v_q, v_ts);
  u_value t522 = NULL;
  if ((!t522) && u_is_con(u_field(s521, 0), &n_list_46_Nil)) {
    t522 = u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t522) && (u_is_con(u_field(s521, 0), &n_list_46_Cons) && u_is_con(u_field(s521, 1), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s521, 0), 0);
    u_value v_ps = u_field(u_field(s521, 0), 1);
    u_value v_t = u_field(u_field(s521, 1), 0);
    u_value v_rest = u_field(u_field(s521, 1), 1);
    u_value s523 = f_check_46_head_of_1(v_p);
    u_value t524 = NULL;
    if ((!t524) && u_is_con(s523, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s523, 0), 0);
      u_value v_args = u_field(u_field(s523, 0), 1);
      t524 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_ps), f_list_46__43__43__2(f_check_46_fields_of_4(v_c, v_su, v_t, v_h), v_rest));
    }
    if ((!t524) && u_is_con(s523, &n_option_46_None)) {
      u_value s525 = f_check_46_constructors_of_3(v_c, v_su, v_t);
      u_value t526 = NULL;
      if ((!t526) && u_is_con(s525, &n_option_46_Some)) {
        u_value v_ks = u_field(s525, 0);
        u_value t527;
        if (u_truth(f_check_46_covers_2(f_check_46_heads_1(v_rows), v_ks))) {
          t527 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_ks, v_ps, v_rest);
        } else {
          t527 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
        }
        t526 = t527;
      }
      if ((!t526) && u_is_con(s525, &n_option_46_None)) {
        t526 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
      }
      if (!t526) {
        u_no_case(s525);
      }
      t524 = t526;
    }
    if (!t524) {
      u_no_case(s523);
    }
    t522 = t524;
  }
  if ((!t522) && (u_is_con(u_field(s521, 0), &n_list_46_Cons) && u_is_con(u_field(s521, 1), &n_list_46_Nil))) {
    t522 = u_mk_bool(0);
  }
  if (!t522) {
    u_no_case(s521);
  }
  return t522;
}

u_value f_check_46_useful_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_ps, u_value v_rest) {
  u_value s528 = v_ks;
  u_value t529 = NULL;
  if ((!t529) && u_is_con(s528, &n_list_46_Nil)) {
    t529 = u_mk_bool(0);
  }
  if ((!t529) && u_is_con(s528, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s528, 0), 0);
    u_value v_args = u_field(u_field(s528, 0), 1);
    u_value v_more = u_field(s528, 1);
    u_value t530 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_list_46__43__43__2(v_args, v_rest));
    if (!u_truth(t530)) {
      t530 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_more, v_ps, v_rest);
    }
    t529 = t530;
  }
  if (!t529) {
    u_no_case(s528);
  }
  return t529;
}

u_value f_check_46_witness_2(u_value v_h, u_value v_args) {
  u_value s531 = v_h;
  u_value t532 = NULL;
  if ((!t532) && u_is_con(s531, &n_check_46_HCon)) {
    u_value v_n = u_field(s531, 0);
    u_value t533;
    if (u_truth(u_eq(v_n, f_syntax_46_tuple_name_0()))) {
      t533 = u_mk_con(&n_syntax_46_PTuple, 1, v_args);
    } else {
      t533 = u_mk_con(&n_syntax_46_PApp, 2, v_n, v_args);
    }
    t532 = t533;
  }
  if ((!t532) && u_is_con(s531, &n_check_46_HLit)) {
    u_value v_v = u_field(s531, 0);
    t532 = u_mk_con(&n_syntax_46_PLit, 1, v_v);
  }
  if (!t532) {
    u_no_case(s531);
  }
  return t532;
}

u_value f_check_46_missing_4(u_value v_c, u_value v_su, u_value v_rows, u_value v_ts) {
  u_value s534 = v_ts;
  u_value t535 = NULL;
  if ((!t535) && u_is_con(s534, &n_list_46_Nil)) {
    u_value t536;
    if (u_truth(u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0)))) {
      t536 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
    } else {
      t536 = u_mk_con(&n_option_46_None, 0);
    }
    t535 = t536;
  }
  if ((!t535) && u_is_con(s534, &n_list_46_Cons)) {
    u_value v_t = u_field(s534, 0);
    u_value v_rest = u_field(s534, 1);
    t535 = f_check_46_missing_in_6(v_c, v_su, v_rows, f_check_46_heads_1(v_rows), f_check_46_constructors_of_3(v_c, v_su, v_t), v_rest);
  }
  if (!t535) {
    u_no_case(s534);
  }
  return t535;
}

u_value f_check_46_missing_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s537 = v_cs;
  u_value t538 = NULL;
  if ((!t538) && u_is_con(s537, &n_option_46_Some)) {
    u_value v_ks = u_field(s537, 0);
    u_value t539;
    if (u_truth(f_check_46_covers_2(v_hs, v_ks))) {
      t539 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_ks, v_rest);
    } else {
      t539 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
    }
    t538 = t539;
  }
  if ((!t538) && u_is_con(s537, &n_option_46_None)) {
    t538 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
  }
  if (!t538) {
    u_no_case(s537);
  }
  return t538;
}

u_value f_check_46_first_missing_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_rest) {
  u_value s540 = v_ks;
  u_value t541 = NULL;
  if ((!t541) && u_is_con(s540, &n_list_46_Nil)) {
    t541 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t541) && u_is_con(s540, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s540, 0), 0);
    u_value v_args = u_field(u_field(s540, 0), 1);
    u_value v_more = u_field(s540, 1);
    u_value s542 = f_check_46_missing_4(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_rest));
    u_value t543 = NULL;
    if ((!t543) && u_is_con(s542, &n_option_46_Some)) {
      u_value v_ws = u_field(s542, 0);
      t543 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_witness_2(v_h, f_list_46_take_2(f_list_46_length_1(v_args), v_ws)), f_list_46_drop_2(f_list_46_length_1(v_args), v_ws)));
    }
    if ((!t543) && u_is_con(s542, &n_option_46_None)) {
      t543 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_more, v_rest);
    }
    if (!t543) {
      u_no_case(s542);
    }
    t541 = t543;
  }
  if (!t541) {
    u_no_case(s540);
  }
  return t541;
}

u_value f_check_46_other_missing_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s544 = f_check_46_missing_4(v_c, v_su, f_check_46_defaults_1(v_rows), v_rest);
  u_value t545 = NULL;
  if ((!t545) && u_is_con(s544, &n_option_46_Some)) {
    u_value v_ws = u_field(s544, 0);
    t545 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_uncovered_2(v_hs, v_cs), v_ws));
  }
  if ((!t545) && u_is_con(s544, &n_option_46_None)) {
    t545 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t545) {
    u_no_case(s544);
  }
  return t545;
}

u_value f_check_46_uncovered_2(u_value v_hs, u_value v_cs) {
  u_value s546 = v_cs;
  u_value t547 = NULL;
  if ((!t547) && u_is_con(s546, &n_option_46_Some)) {
    u_value v_ks = u_field(s546, 0);
    u_value t548;
    if (u_truth(u_eq(v_hs, u_mk_con(&n_list_46_Nil, 0)))) {
      t548 = u_mk_con(&n_syntax_46_PWild, 0);
    } else {
      t548 = f_check_46_first_uncovered_2(v_hs, v_ks);
    }
    t547 = t548;
  }
  if ((!t547) && u_is_con(s546, &n_option_46_None)) {
    t547 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if (!t547) {
    u_no_case(s546);
  }
  return t547;
}

u_value f_check_46_first_uncovered_2(u_value v_hs, u_value v_ks) {
  u_value s549 = v_ks;
  u_value t550 = NULL;
  if ((!t550) && u_is_con(s549, &n_list_46_Nil)) {
    t550 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if ((!t550) && u_is_con(s549, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s549, 0), 0);
    u_value v_args = u_field(u_field(s549, 0), 1);
    u_value v_rest = u_field(s549, 1);
    u_value t551;
    if (u_truth(f_list_46_contains_2(v_h, v_hs))) {
      t551 = f_check_46_first_uncovered_2(v_hs, v_rest);
    } else {
      t551 = f_check_46_witness_2(v_h, f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)));
    }
    t550 = t551;
  }
  if (!t550) {
    u_no_case(s549);
  }
  return t550;
}

void p_check_46_fresh(u_value *r_t, u_value *r_s) {
  {
    u_value t552 = u_mk_con(&n_syntax_46_TMeta, 1, u_add(f_check_46_State_46_count_1(*r_s), u_mk_int(1)));
    u_value t553 = f_check_46_State_46_count_58__61__2(*r_s, u_add(f_check_46_State_46_count_1(*r_s), u_mk_int(1)));
    *r_t = t552;
    *r_s = t553;
  }
}

void p_check_46_bind_meta(u_value v_i, u_value v_t, u_value *r_s) {
  {
    if (u_truth(f_check_46_occurs_3(f_check_46_State_46_sub_1(*r_s), v_i, v_t))) {
      {
        u_raise(u_mk_con(&n_check_46_InfiniteType, 2, u_mk_con(&n_syntax_46_TMeta, 1, v_i), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t)), "check.TypeError");
      }
    }
    u_value t554 = f_check_46_State_46_sub_58__61__2(*r_s, f_map_46_put_3(v_i, v_t, f_check_46_State_46_sub_1(*r_s)));
    *r_s = t554;
  }
}

void p_check_46_unify(u_value v_t1, u_value v_t2, u_value *r_s) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value t555 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t1);
    v_a = t555;
    u_value t556 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t2);
    v_b = t556;
    u_value s557 = u_mk_tuple(2, v_a, v_b);
    int d558 = 0;
    if ((!d558) && (u_is_con(u_field(s557, 0), &n_syntax_46_TMeta) && u_is_con(u_field(s557, 1), &n_syntax_46_TMeta))) {
      u_value v_i = u_field(u_field(s557, 0), 0);
      u_value v_j = u_field(u_field(s557, 1), 0);
      d558 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_i, v_j))) {
          {
            p_check_46_bind_meta(v_i, v_b, r_s);
          }
        }
      }
    }
    if ((!d558) && u_is_con(u_field(s557, 0), &n_syntax_46_TMeta)) {
      u_value v_i = u_field(u_field(s557, 0), 0);
      d558 = 1;
      {
        p_check_46_bind_meta(v_i, v_b, r_s);
      }
    }
    if ((!d558) && u_is_con(u_field(s557, 1), &n_syntax_46_TMeta)) {
      u_value v_j = u_field(u_field(s557, 1), 0);
      d558 = 1;
      {
        p_check_46_bind_meta(v_j, v_a, r_s);
      }
    }
    if ((!d558) && (u_is_con(u_field(s557, 0), &n_syntax_46_TParam) && u_is_con(u_field(s557, 1), &n_syntax_46_TParam))) {
      u_value v_x = u_field(u_field(s557, 0), 0);
      u_value v_y = u_field(u_field(s557, 1), 0);
      d558 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_x, v_y))) {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, v_a, v_b), "check.TypeError");
          }
        }
      }
    }
    if ((!d558) && (u_is_con(u_field(s557, 0), &n_syntax_46_TSort) && u_is_con(u_field(s557, 1), &n_syntax_46_TSort))) {
      u_value v_f = u_field(u_field(s557, 0), 0);
      u_value v_xs = u_field(u_field(s557, 0), 1);
      u_value v_g = u_field(u_field(s557, 1), 0);
      u_value v_ys = u_field(u_field(s557, 1), 1);
      d558 = 1;
      {
        u_value t559 = u_eq(v_f, v_g);
        if (u_truth(t559)) {
          t559 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys));
        }
        if (u_truth(t559)) {
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
    if ((!d558) && (u_is_con(u_field(s557, 0), &n_syntax_46_TFun) && u_is_con(u_field(s557, 1), &n_syntax_46_TFun))) {
      u_value v_xs = u_field(u_field(s557, 0), 0);
      u_value v_r1 = u_field(u_field(s557, 0), 1);
      u_value v_ys = u_field(u_field(s557, 1), 0);
      u_value v_r2 = u_field(u_field(s557, 1), 1);
      d558 = 1;
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
    if ((!d558) && (u_is_con(u_field(s557, 0), &n_syntax_46_TTuple) && u_is_con(u_field(s557, 1), &n_syntax_46_TTuple))) {
      u_value v_xs = u_field(u_field(s557, 0), 0);
      u_value v_ys = u_field(u_field(s557, 1), 0);
      d558 = 1;
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
    if (!d558) {
      d558 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t2)), "check.TypeError");
      }
    }
    if (!d558) {
      u_no_case(s557);
    }
  }
}

void p_check_46_unifies(u_value v_xs, u_value v_ys, u_value *r_s) {
  {
    u_value s560 = u_mk_tuple(2, v_xs, v_ys);
    int d561 = 0;
    if ((!d561) && (u_is_con(u_field(s560, 0), &n_list_46_Cons) && u_is_con(u_field(s560, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s560, 0), 0);
      u_value v_xs1 = u_field(u_field(s560, 0), 1);
      u_value v_y = u_field(u_field(s560, 1), 0);
      u_value v_ys1 = u_field(u_field(s560, 1), 1);
      d561 = 1;
      {
        p_check_46_unify(v_x, v_y, r_s);
        p_check_46_unifies(v_xs1, v_ys1, r_s);
      }
    }
    if ((!d561) && (u_is_con(u_field(s560, 0), &n_list_46_Nil) && u_is_con(u_field(s560, 1), &n_list_46_Nil))) {
      d561 = 1;
      {
      }
    }
    if (!d561) {
      d561 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)), "check.TypeError");
      }
    }
    if (!d561) {
      u_no_case(s560);
    }
  }
}

void p_check_46_fresh_list(u_value v_xs, u_value *r_ts, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s562 = v_xs;
    int d563 = 0;
    if ((!d563) && u_is_con(s562, &n_list_46_Nil)) {
      d563 = 1;
      {
        u_value t564 = u_mk_con(&n_list_46_Nil, 0);
        *r_ts = t564;
      }
    }
    if ((!d563) && u_is_con(s562, &n_list_46_Cons)) {
      u_value v_more = u_field(s562, 1);
      d563 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_fresh_list(v_more, &v_rest, r_s);
        u_value t565 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rest);
        *r_ts = t565;
      }
    }
    if (!d563) {
      u_no_case(s562);
    }
  }
}

void p_check_46_instance(u_value v_ps, u_value *r_m, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s566 = v_ps;
    int d567 = 0;
    if ((!d567) && u_is_con(s566, &n_list_46_Nil)) {
      d567 = 1;
      {
        u_value t568 = u_mk_con(&n_list_46_Nil, 0);
        *r_m = t568;
      }
    }
    if ((!d567) && u_is_con(s566, &n_list_46_Cons)) {
      u_value v_p = u_field(s566, 0);
      u_value v_qs = u_field(s566, 1);
      d567 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_instance(v_qs, &v_rest, r_s);
        u_value t569 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_p, v_t), v_rest);
        *r_m = t569;
      }
    }
    if (!d567) {
      u_no_case(s566);
    }
  }
}

void p_check_46_instantiate(u_value v_sc, u_value *r_t, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value s570 = v_sc;
    int d571 = 0;
    if (!d571) {
      u_value v_ps = u_field(s570, 0);
      u_value v_u = u_field(s570, 1);
      d571 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        u_value t572 = f_check_46_subst_2(f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u), v_m);
        *r_t = t572;
      }
    }
    if (!d571) {
      u_no_case(s570);
    }
  }
}

void p_check_46_unique(u_value v_ns) {
  {
    u_value s573 = f_list_46_repeated_1(v_ns);
    int d574 = 0;
    if ((!d574) && u_is_con(s573, &n_option_46_Some)) {
      u_value v_n = u_field(s573, 0);
      d574 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, f_syntax_46_qualified_1(v_n)), "check.TypeError");
      }
    }
    if ((!d574) && u_is_con(s573, &n_option_46_None)) {
      d574 = 1;
      {
      }
    }
    if (!d574) {
      u_no_case(s573);
    }
  }
}

void p_check_46_unique_ids(u_value v_ns) {
  {
    u_value s575 = f_list_46_repeated_1(v_ns);
    int d576 = 0;
    if ((!d576) && u_is_con(s575, &n_option_46_Some)) {
      u_value v_n = u_field(s575, 0);
      d576 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, v_n), "check.TypeError");
      }
    }
    if ((!d576) && u_is_con(s575, &n_option_46_None)) {
      d576 = 1;
      {
      }
    }
    if (!d576) {
      u_no_case(s575);
    }
  }
}

void p_check_46_one_of(u_value v_n, u_value v_ns, u_value *r_m) {
  {
    u_value s577 = v_ns;
    int d578 = 0;
    if ((!d578) && (u_is_con(s577, &n_list_46_Cons) && u_is_con(u_field(s577, 1), &n_list_46_Nil))) {
      u_value v_k = u_field(s577, 0);
      d578 = 1;
      {
        u_value t579 = v_k;
        *r_m = t579;
      }
    }
    if ((!d578) && u_is_con(s577, &n_list_46_Nil)) {
      d578 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if (!d578) {
      d578 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ns), "check.TypeError");
      }
    }
    if (!d578) {
      u_no_case(s577);
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
    u_value s580 = v_t;
    int d581 = 0;
    if ((!d581) && u_is_con(s580, &n_syntax_46_THole)) {
      d581 = 1;
      {
        p_check_46_fresh(r_u, r_s);
      }
    }
    if ((!d581) && u_is_con(s580, &n_syntax_46_TParam)) {
      d581 = 1;
      {
        u_value t582 = v_t;
        *r_u = t582;
      }
    }
    if ((!d581) && u_is_con(s580, &n_syntax_46_TMeta)) {
      d581 = 1;
      {
        u_value t583 = v_t;
        *r_u = t583;
      }
    }
    if ((!d581) && u_is_con(s580, &n_syntax_46_TFun)) {
      u_value v_args = u_field(s580, 0);
      u_value v_r = u_field(s580, 1);
      d581 = 1;
      {
        p_check_46_normalizes(v_args, &v_us, r_s);
        p_check_46_normalize(v_r, &v_v, r_s);
        u_value t584 = u_mk_con(&n_syntax_46_TFun, 2, v_us, v_v);
        *r_u = t584;
      }
    }
    if ((!d581) && u_is_con(s580, &n_syntax_46_TTuple)) {
      u_value v_ts = u_field(s580, 0);
      d581 = 1;
      {
        p_check_46_normalizes(v_ts, &v_us, r_s);
        u_value t585 = u_mk_con(&n_syntax_46_TTuple, 1, v_us);
        *r_u = t585;
      }
    }
    if ((!d581) && u_is_con(s580, &n_syntax_46_TSort)) {
      u_value v_n = u_field(s580, 0);
      u_value v_args = u_field(s580, 1);
      d581 = 1;
      {
        if (u_truth(f_check_46_basic_1(v_n))) {
          {
            p_check_46_arguments(v_n, u_mk_int(0), v_args);
            u_value t586 = v_t;
            *r_u = t586;
          }
        } else {
          {
            u_value s588 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam587, 1, *r_s));
            int d589 = 0;
            if ((!d589) && (u_is_con(s588, &n_list_46_Cons) && u_is_con(u_field(s588, 1), &n_list_46_Nil))) {
              u_value v_m = u_field(s588, 0);
              d589 = 1;
              {
                p_check_46_normalize_sort(v_m, v_args, r_u, r_s);
              }
            }
            if ((!d589) && u_is_con(s588, &n_list_46_Nil)) {
              d589 = 1;
              {
                u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
              }
            }
            if (!d589) {
              u_value v_ms = s588;
              d589 = 1;
              {
                u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ms), "check.TypeError");
              }
            }
            if (!d589) {
              u_no_case(s588);
            }
          }
        }
      }
    }
    if (!d581) {
      u_no_case(s580);
    }
  }
}

void p_check_46_normalize_sort(u_value v_n, u_value v_args, u_value *r_u, u_value *r_s) {
  {
    u_value v_us = NULL;
    u_value s590 = f_ctx_46_data_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
    int d591 = 0;
    if ((!d591) && u_is_con(s590, &n_option_46_Some)) {
      u_value v_k = u_field(s590, 0);
      d591 = 1;
      {
        p_check_46_arguments(v_n, v_k, v_args);
        p_check_46_normalizes(v_args, &v_us, r_s);
        u_value t592 = u_mk_con(&n_syntax_46_TSort, 2, v_n, v_us);
        *r_u = t592;
      }
    }
    if ((!d591) && u_is_con(s590, &n_option_46_None)) {
      d591 = 1;
      {
        u_value s593 = f_ctx_46_alias_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
        int d594 = 0;
        if ((!d594) && u_is_con(s593, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s593, 0), 0);
          u_value v_rhs = u_field(u_field(s593, 0), 1);
          d594 = 1;
          {
            p_check_46_arguments(v_n, f_list_46_length_1(v_ps), v_args);
            p_check_46_normalizes(v_args, &v_us, r_s);
            u_value t595 = f_check_46_subst_2(v_rhs, f_list_46_zip_2(f_check_46_params_1(v_ps), v_us));
            *r_u = t595;
          }
        }
        if ((!d594) && u_is_con(s593, &n_option_46_None)) {
          d594 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
          }
        }
        if (!d594) {
          u_no_case(s593);
        }
      }
    }
    if (!d591) {
      u_no_case(s590);
    }
  }
}

void p_check_46_normalizes(u_value v_ts, u_value *r_us, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_rest = NULL;
    u_value s596 = v_ts;
    int d597 = 0;
    if ((!d597) && u_is_con(s596, &n_list_46_Nil)) {
      d597 = 1;
      {
        u_value t598 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t598;
      }
    }
    if ((!d597) && u_is_con(s596, &n_list_46_Cons)) {
      u_value v_t = u_field(s596, 0);
      u_value v_more = u_field(s596, 1);
      d597 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalizes(v_more, &v_rest, r_s);
        u_value t599 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t599;
      }
    }
    if (!d597) {
      u_no_case(s596);
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
    u_value s600 = v_ts;
    int d601 = 0;
    if ((!d601) && u_is_con(s600, &n_list_46_Nil)) {
      d601 = 1;
      {
        u_value t602 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t602;
      }
    }
    if ((!d601) && u_is_con(s600, &n_list_46_Cons)) {
      u_value v_t = u_field(s600, 0);
      u_value v_more = u_field(s600, 1);
      d601 = 1;
      {
        p_check_46_known(v_t, v_what, &v_u, r_s);
        p_check_46_knowns(v_more, v_what, &v_rest, r_s);
        u_value t603 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t603;
      }
    }
    if (!d601) {
      u_no_case(s600);
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
    u_value s604 = v_xs;
    int d605 = 0;
    if ((!d605) && u_is_con(s604, &n_list_46_Nil)) {
      d605 = 1;
      {
        u_value t606 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t606;
      }
    }
    if ((!d605) && (u_is_con(s604, &n_list_46_Cons) && u_is_con(u_field(s604, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s604, 0), 0);
      u_value v_t = u_field(u_field(s604, 0), 1);
      u_value v_more = u_field(s604, 1);
      d605 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalize_vars(v_more, &v_rest, r_s);
        u_value t607 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t607;
      }
    }
    if (!d605) {
      u_no_case(s604);
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
    u_value s608 = v_xs;
    int d609 = 0;
    if ((!d609) && u_is_con(s608, &n_list_46_Nil)) {
      d609 = 1;
      {
        u_value t610 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t610;
      }
    }
    if ((!d609) && (u_is_con(s608, &n_list_46_Cons) && u_is_con(u_field(s608, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s608, 0), 0);
      u_value v_t = u_field(u_field(s608, 0), 1);
      u_value v_more = u_field(s608, 1);
      d609 = 1;
      {
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_known_vars(v_more, &v_rest, r_s);
        u_value t611 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t611;
      }
    }
    if (!d609) {
      u_no_case(s608);
    }
  }
}

void p_check_46_resolve_name(u_value v_s, u_value v_env, u_value v_n, u_value v_k, u_value *r_m, u_value *r_sc) {
  {
    u_value s612 = f_check_46_candidates_3(v_s, v_env, v_n);
    int d613 = 0;
    if ((!d613) && u_is_con(s612, &n_list_46_Nil)) {
      d613 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if ((!d613) && (u_is_con(s612, &n_list_46_Cons) && u_is_con(u_field(s612, 1), &n_list_46_Nil))) {
      u_value v_m1 = u_field(u_field(s612, 0), 0);
      u_value v_one = u_field(u_field(s612, 0), 1);
      d613 = 1;
      {
        u_value t614 = v_m1;
        u_value t615 = v_one;
        *r_m = t614;
        *r_sc = t615;
      }
    }
    if (!d613) {
      u_value v_cs = s612;
      d613 = 1;
      {
        if (u_truth(u_lt(v_k, u_mk_int(0)))) {
          {
            u_raise(u_mk_con(&n_check_46_OverloadedUnapplied, 1, v_n), "check.TypeError");
          }
        }
        u_value s616 = f_check_46_with_arity_2(v_cs, v_k);
        int d617 = 0;
        if ((!d617) && (u_is_con(s616, &n_list_46_Cons) && u_is_con(u_field(s616, 1), &n_list_46_Nil))) {
          u_value v_m1 = u_field(u_field(s616, 0), 0);
          u_value v_one = u_field(u_field(s616, 0), 1);
          d617 = 1;
          {
            u_value t618 = v_m1;
            u_value t619 = v_one;
            *r_m = t618;
            *r_sc = t619;
          }
        }
        if ((!d617) && u_is_con(s616, &n_list_46_Nil)) {
          d617 = 1;
          {
            u_raise(u_mk_con(&n_check_46_WrongArity, 2, v_n, v_k), "check.TypeError");
          }
        }
        if (!d617) {
          u_value v_more = s616;
          d617 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, f_list_46_firsts_1(v_more)), "check.TypeError");
          }
        }
        if (!d617) {
          u_no_case(s616);
        }
      }
    }
    if (!d613) {
      u_no_case(s612);
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
    u_value t620 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_u);
    *r_x = t620;
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
    u_value s621 = v_e;
    int d622 = 0;
    if ((!d622) && u_is_con(s621, &n_syntax_46_ELit)) {
      u_value v_v = u_field(s621, 0);
      d622 = 1;
      {
        u_value t623 = v_e;
        u_value t624 = f_check_46_literal_1(v_v);
        *r_e1 = t623;
        *r_t = t624;
      }
    }
    if ((!d622) && (u_is_con(s621, &n_syntax_46_EVar) && u_is_con(u_field(s621, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s621, 0), 0);
      u_value v_a = u_field(u_field(s621, 0), 1);
      d622 = 1;
      {
        p_check_46_resolve_name(*r_s, v_env, v_n, f_prelude_46__45__1(u_mk_int(1)), &v_m, &v_sc);
        p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
        u_value t625 = u_mk_con(&n_syntax_46_EVar, 1, v_x);
        u_value t626 = f_syntax_46_Var_46_typ_1(v_x);
        *r_e1 = t625;
        *r_t = t626;
      }
    }
    if ((!d622) && u_is_con(s621, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s621, 0);
      u_value v_args = u_field(s621, 1);
      d622 = 1;
      {
        p_check_46_application(v_env, v_f, v_args, r_e1, r_t, r_s);
      }
    }
    if ((!d622) && u_is_con(s621, &n_syntax_46_ELam)) {
      u_value v_bound = u_field(s621, 0);
      u_value v_body = u_field(s621, 1);
      d622 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t627 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, v_b);
        u_value t628 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), v_u);
        *r_e1 = t627;
        *r_t = t628;
      }
    }
    if ((!d622) && u_is_con(s621, &n_syntax_46_ELet)) {
      u_value v_bound = u_field(s621, 0);
      u_value v_body = u_field(s621, 1);
      d622 = 1;
      {
        p_check_46_formals(f_list_46_firsts_1(v_bound), &v_xs, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_bound), f_syntax_46_types_1(v_xs), &v_es, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_generalized_3(f_check_46_State_46_sub_1(*r_s), v_env, v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t629 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        u_value t630 = v_u;
        *r_e1 = t629;
        *r_t = t630;
      }
    }
    if ((!d622) && u_is_con(s621, &n_syntax_46_EIte)) {
      u_value v_cond = u_field(s621, 0);
      u_value v_left = u_field(s621, 1);
      u_value v_right = u_field(s621, 2);
      d622 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(v_env, v_left, &v_l, &v_u, r_s);
        p_check_46_check(v_env, v_right, v_u, &v_b, r_s);
        u_value t631 = u_mk_con(&n_syntax_46_EIte, 3, v_c, v_l, v_b);
        u_value t632 = v_u;
        *r_e1 = t631;
        *r_t = t632;
      }
    }
    if ((!d622) && u_is_con(s621, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s621, 0);
      u_value v_bound = u_field(s621, 1);
      u_value v_lists = u_field(s621, 2);
      u_value v_cond = u_field(s621, 3);
      d622 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_same_length(f_list_46_length_1(v_xs), f_list_46_length_1(v_lists));
        p_check_46_generators(v_env, v_xs, v_lists, &v_es, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_result, &v_b, &v_u, r_s);
        u_value t633 = u_mk_con(&n_syntax_46_EComp, 4, v_b, v_xs, v_es, v_c);
        u_value t634 = f_check_46_tlist_1(v_u);
        *r_e1 = t633;
        *r_t = t634;
      }
    }
    if ((!d622) && u_is_con(s621, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s621, 0);
      d622 = 1;
      {
        p_check_46_infers(v_env, v_items, &v_es, &v_ts, r_s);
        u_value t635 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
        u_value t636 = u_mk_con(&n_syntax_46_TTuple, 1, v_ts);
        *r_e1 = t635;
        *r_t = t636;
      }
    }
    if ((!d622) && u_is_con(s621, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s621, 0);
      u_value v_a = u_field(s621, 1);
      d622 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_normalize(v_a, &v_w, r_s);
        p_check_46_unify(v_u, v_w, r_s);
        p_check_46_fresh(r_t, r_s);
        u_value t637 = u_mk_con(&n_syntax_46_ERaise, 2, v_c, v_u);
        *r_e1 = t637;
      }
    }
    if ((!d622) && u_is_con(s621, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s621, 0);
      u_value v_cases = u_field(s621, 1);
      d622 = 1;
      {
        p_check_46_fresh(r_t, r_s);
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_expr_branches(v_env, v_cases, v_u, *r_t, &v_bs, r_s);
        u_value t638 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e639 = &t638;
        u_value l640 = v_bs;
        while (u_is_con(l640, &n_list_46_Cons)) {
          u_value v_br = u_field(l640, 0);
          if (u_truth(u_mk_bool(1))) {
            e639 = u_append(e639, f_check_46_case_of_1(v_br));
          }
          l640 = u_field(l640, 1);
        }
        p_check_46_coverage(*r_s, v_u, t638);
        u_value t641 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, v_bs);
        *r_e1 = t641;
      }
    }
    if (!d622) {
      u_no_case(s621);
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
    u_value s642 = v_f;
    int d643 = 0;
    if ((!d643) && (u_is_con(s642, &n_syntax_46_EVar) && (u_is_con(u_field(s642, 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(s642, 0), 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(u_field(s642, 0), 0), 0), &n_option_46_None))))) {
      u_value v_b = u_field(u_field(u_field(s642, 0), 0), 1);
      u_value v_a = u_field(u_field(s642, 0), 1);
      u_value t644 = f_prelude_46__62__2(u_str_size(v_b), u_mk_int(1));
      if (u_truth(t644)) {
        t644 = u_eq(u_str_get(v_b, u_mk_int(0)), u_mk_char(46));
      }
      if (u_truth(t644)) {
        d643 = 1;
        {
          p_check_46_field_access(v_env, v_b, v_a, v_args, r_e1, r_t, r_s);
        }
      }
    }
    if ((!d643) && (u_is_con(s642, &n_syntax_46_EVar) && u_is_con(u_field(s642, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s642, 0), 0);
      u_value v_a = u_field(u_field(s642, 0), 1);
      d643 = 1;
      {
        if (u_truth(f_prelude_46__62__2(f_list_46_length_1(f_check_46_candidates_3(*r_s, v_env, v_n)), u_mk_int(1)))) {
          {
            p_check_46_resolve_name(*r_s, v_env, v_n, f_list_46_length_1(v_args), &v_m, &v_sc);
            p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
            p_check_46_apply(v_env, f_syntax_46_Var_46_typ_1(v_x), v_args, &v_args1, r_t, r_s);
            u_value t645 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), v_args1);
            *r_e1 = t645;
          }
        } else {
          {
            p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
            p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
            u_value t646 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
            *r_e1 = t646;
          }
        }
      }
    }
    if (!d643) {
      d643 = 1;
      {
        p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
        p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
        u_value t647 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
        *r_e1 = t647;
      }
    }
    if (!d643) {
      u_no_case(s642);
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
    u_value s648 = v_args;
    int d649 = 0;
    if ((!d649) && u_is_con(s648, &n_list_46_Cons)) {
      u_value v_arg = u_field(s648, 0);
      u_value v_rest = u_field(s648, 1);
      d649 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_arg1, &v_u, r_s);
        u_value s650 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u);
        int d651 = 0;
        if ((!d651) && (u_is_con(s650, &n_syntax_46_TSort) && (u_is_con(u_field(s650, 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(s650, 0), 0), &n_option_46_Some)))) {
          u_value v_m = u_field(u_field(u_field(s650, 0), 0), 0);
          u_value v_tn = u_field(u_field(s650, 0), 1);
          d651 = 1;
          {
            u_value t652 = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), u_concat(v_tn, v_f));
            v_acc = t652;
          }
        }
        if (!d651) {
          u_value v_v = s650;
          d651 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownFieldType, 2, v_f, v_v), "check.TypeError");
          }
        }
        if (!d651) {
          u_no_case(s650);
        }
        u_value s653 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(*r_s), v_acc);
        int d654 = 0;
        if ((!d654) && (u_is_con(s653, &n_list_46_Cons) && u_is_con(u_field(s653, 1), &n_list_46_Nil))) {
          u_value v_sc = u_field(s653, 0);
          d654 = 1;
          {
            p_check_46_use(v_acc, v_a, v_sc, &v_x, r_s);
          }
        }
        if (!d654) {
          d654 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownField, 1, v_acc), "check.TypeError");
          }
        }
        if (!d654) {
          u_no_case(s653);
        }
        p_check_46_infers(v_env, v_rest, &v_rest1, &v_us, r_s);
        p_check_46_fresh(r_t, r_s);
        p_check_46_unify(f_syntax_46_Var_46_typ_1(v_x), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_u, v_us), *r_t), r_s);
        u_value t655 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), u_mk_con(&n_list_46_Cons, 2, v_arg1, v_rest1));
        *r_e1 = t655;
      }
    }
    if (!d649) {
      d649 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, u_mk_int(1), f_list_46_length_1(v_args)), "check.TypeError");
      }
    }
    if (!d649) {
      u_no_case(s648);
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
    u_value s656 = u_mk_tuple(2, v_es, v_ts);
    int d657 = 0;
    if ((!d657) && (u_is_con(u_field(s656, 0), &n_list_46_Cons) && u_is_con(u_field(s656, 1), &n_list_46_Cons))) {
      u_value v_e = u_field(u_field(s656, 0), 0);
      u_value v_more = u_field(u_field(s656, 0), 1);
      u_value v_t = u_field(u_field(s656, 1), 0);
      u_value v_ts1 = u_field(u_field(s656, 1), 1);
      d657 = 1;
      {
        p_check_46_check(v_env, v_e, v_t, &v_e1, r_s);
        p_check_46_checks(v_env, v_more, v_ts1, &v_rest, r_s);
        u_value t658 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        *r_es1 = t658;
      }
    }
    if ((!d657) && (u_is_con(u_field(s656, 0), &n_list_46_Nil) && u_is_con(u_field(s656, 1), &n_list_46_Nil))) {
      d657 = 1;
      {
        u_value t659 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t659;
      }
    }
    if (!d657) {
      d657 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_es)), "check.TypeError");
      }
    }
    if (!d657) {
      u_no_case(s656);
    }
  }
}

void p_check_46_infers(u_value v_env, u_value v_es, u_value *r_es1, u_value *r_ts, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value v_us = NULL;
    u_value s660 = v_es;
    int d661 = 0;
    if ((!d661) && u_is_con(s660, &n_list_46_Nil)) {
      d661 = 1;
      {
        u_value t662 = u_mk_con(&n_list_46_Nil, 0);
        u_value t663 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t662;
        *r_ts = t663;
      }
    }
    if ((!d661) && u_is_con(s660, &n_list_46_Cons)) {
      u_value v_e = u_field(s660, 0);
      u_value v_more = u_field(s660, 1);
      d661 = 1;
      {
        p_check_46_infer(v_env, v_e, &v_e1, &v_t, r_s);
        p_check_46_infers(v_env, v_more, &v_rest, &v_us, r_s);
        u_value t664 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        u_value t665 = u_mk_con(&n_list_46_Cons, 2, v_t, v_us);
        *r_es1 = t664;
        *r_ts = t665;
      }
    }
    if (!d661) {
      u_no_case(s660);
    }
  }
}

void p_check_46_generators(u_value v_env, u_value v_xs, u_value v_lists, u_value *r_lists1, u_value *r_s) {
  {
    u_value v_l = NULL;
    u_value v_rest = NULL;
    u_value s666 = u_mk_tuple(2, v_xs, v_lists);
    int d667 = 0;
    if ((!d667) && (u_is_con(u_field(s666, 0), &n_list_46_Cons) && u_is_con(u_field(s666, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s666, 0), 0);
      u_value v_ys = u_field(u_field(s666, 0), 1);
      u_value v_e = u_field(u_field(s666, 1), 0);
      u_value v_es = u_field(u_field(s666, 1), 1);
      d667 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tlist_1(f_syntax_46_Var_46_typ_1(v_x)), &v_l, r_s);
        p_check_46_generators(f_list_46__43__43__2(f_check_46_bind_1(u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0))), v_env), v_ys, v_es, &v_rest, r_s);
        u_value t668 = u_mk_con(&n_list_46_Cons, 2, v_l, v_rest);
        *r_lists1 = t668;
      }
    }
    if (!d667) {
      d667 = 1;
      {
        u_value t669 = u_mk_con(&n_list_46_Nil, 0);
        *r_lists1 = t669;
      }
    }
    if (!d667) {
      u_no_case(s666);
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
    u_value s670 = v_cases;
    int d671 = 0;
    if ((!d671) && u_is_con(s670, &n_list_46_Nil)) {
      d671 = 1;
      {
        u_value t672 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t672;
      }
    }
    if ((!d671) && (u_is_con(s670, &n_list_46_Cons) && u_is_con(u_field(s670, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s670, 0), 0);
      u_value v_g = u_field(u_field(s670, 0), 1);
      u_value v_b = u_field(u_field(s670, 0), 2);
      u_value v_more = u_field(s670, 1);
      d671 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_b, v_r, &v_b1, r_s);
        p_check_46_expr_branches(v_env, v_more, v_t, v_r, &v_rest, r_s);
        u_value t673 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t673;
      }
    }
    if (!d671) {
      u_no_case(s670);
    }
  }
}

void p_check_46_guard(u_value v_env, u_value v_g, u_value *r_g1, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value s674 = v_g;
    int d675 = 0;
    if ((!d675) && u_is_con(s674, &n_option_46_Some)) {
      u_value v_e = u_field(s674, 0);
      d675 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tbool_0(), &v_e1, r_s);
        u_value t676 = u_mk_con(&n_option_46_Some, 1, v_e1);
        *r_g1 = t676;
      }
    }
    if ((!d675) && u_is_con(s674, &n_option_46_None)) {
      d675 = 1;
      {
        u_value t677 = u_mk_con(&n_option_46_None, 0);
        *r_g1 = t677;
      }
    }
    if (!d675) {
      u_no_case(s674);
    }
  }
}

void p_check_46_coverage(u_value v_s, u_value v_t, u_value v_cs) {
  {
    p_check_46_reachable(v_s, v_t, u_mk_con(&n_list_46_Nil, 0), v_cs);
    u_value t678 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e679 = &t678;
    u_value l680 = f_check_46_unguarded_1(v_cs);
    while (u_is_con(l680, &n_list_46_Cons)) {
      u_value v_p = u_field(l680, 0);
      if (u_truth(u_mk_bool(1))) {
        e679 = u_append(e679, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
      }
      l680 = u_field(l680, 1);
    }
    u_value s681 = f_check_46_missing_4(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t678, u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
    int d682 = 0;
    if ((!d682) && (u_is_con(s681, &n_option_46_Some) && u_is_con(u_field(s681, 0), &n_list_46_Cons))) {
      u_value v_w = u_field(u_field(s681, 0), 0);
      d682 = 1;
      {
        u_raise(u_mk_con(&n_check_46_NotExhaustive, 1, v_w), "check.TypeError");
      }
    }
    if (!d682) {
      d682 = 1;
      {
      }
    }
    if (!d682) {
      u_no_case(s681);
    }
  }
}

u_value f_check_46_unguarded_1(u_value v_cs) {
  u_value s683 = v_cs;
  u_value t684 = NULL;
  if ((!t684) && u_is_con(s683, &n_list_46_Nil)) {
    t684 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t684) && (u_is_con(s683, &n_list_46_Cons) && u_equals(u_field(u_field(s683, 0), 1), u_mk_bool(1)))) {
    u_value v_p = u_field(u_field(s683, 0), 0);
    u_value v_rest = u_field(s683, 1);
    t684 = u_mk_con(&n_list_46_Cons, 2, v_p, f_check_46_unguarded_1(v_rest));
  }
  if ((!t684) && (u_is_con(s683, &n_list_46_Cons) && u_equals(u_field(u_field(s683, 0), 1), u_mk_bool(0)))) {
    u_value v_rest = u_field(s683, 1);
    t684 = f_check_46_unguarded_1(v_rest);
  }
  if (!t684) {
    u_no_case(s683);
  }
  return t684;
}

void p_check_46_reachable(u_value v_s, u_value v_t, u_value v_before, u_value v_cs) {
  {
    u_value s685 = v_cs;
    int d686 = 0;
    if ((!d686) && u_is_con(s685, &n_list_46_Nil)) {
      d686 = 1;
      {
      }
    }
    if ((!d686) && u_is_con(s685, &n_list_46_Cons)) {
      u_value v_p = u_field(u_field(s685, 0), 0);
      u_value v_open = u_field(u_field(s685, 0), 1);
      u_value v_rest = u_field(s685, 1);
      d686 = 1;
      {
        u_value t687 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e688 = &t687;
        u_value l689 = v_before;
        while (u_is_con(l689, &n_list_46_Cons)) {
          u_value v_q = u_field(l689, 0);
          if (u_truth(u_mk_bool(1))) {
            e688 = u_append(e688, u_mk_con(&n_list_46_Cons, 2, v_q, u_mk_con(&n_list_46_Nil, 0)));
          }
          l689 = u_field(l689, 1);
        }
        if (u_truth(f_prelude_46_not_1(f_check_46_useful_5(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t687, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)))))) {
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
    if (!d686) {
      u_no_case(s685);
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
    u_value s690 = v_p;
    int d691 = 0;
    if ((!d691) && u_is_con(s690, &n_syntax_46_PWild)) {
      d691 = 1;
      {
        u_value t692 = v_p;
        u_value t693 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t692;
        *r_xs = t693;
      }
    }
    if ((!d691) && u_is_con(s690, &n_syntax_46_PLit)) {
      u_value v_v = u_field(s690, 0);
      d691 = 1;
      {
        p_check_46_unify(f_check_46_literal_1(v_v), v_t, r_s);
        u_value t694 = v_p;
        u_value t695 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t694;
        *r_xs = t695;
      }
    }
    if ((!d691) && (u_is_con(s690, &n_syntax_46_PVar) && u_is_con(u_field(s690, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s690, 0), 0);
      u_value v_a = u_field(u_field(s690, 0), 1);
      d691 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam696, 1, *r_s)), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            p_check_46_binders(u_mk_con(&n_syntax_46_PApp, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_t, r_p1, r_xs, r_s);
          }
        } else {
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_u, v_t, r_s);
            u_value t697 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u));
            u_value t698 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), u_mk_con(&n_list_46_Nil, 0));
            *r_p1 = t697;
            *r_xs = t698;
          }
        }
      }
    }
    if ((!d691) && u_is_con(s690, &n_syntax_46_PApp)) {
      u_value v_n = u_field(s690, 0);
      u_value v_args = u_field(s690, 1);
      d691 = 1;
      {
        u_value s700 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam699, 1, *r_s));
        int d701 = 0;
        if ((!d701) && (u_is_con(s700, &n_list_46_Cons) && u_is_con(u_field(s700, 1), &n_list_46_Nil))) {
          u_value v_k1 = u_field(s700, 0);
          d701 = 1;
          {
            u_value t702 = v_k1;
            v_k = t702;
          }
        }
        if ((!d701) && u_is_con(s700, &n_list_46_Nil)) {
          d701 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d701) {
          u_value v_ks = s700;
          d701 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ks), "check.TypeError");
          }
        }
        if (!d701) {
          u_no_case(s700);
        }
        u_value s703 = f_ctx_46_constr_of_2(f_check_46_State_46_ctx_1(*r_s), v_k);
        int d704 = 0;
        if ((!d704) && u_is_con(s703, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s703, 0), 0);
          u_value v_ks = u_field(u_field(s703, 0), 1);
          u_value v_r = u_field(u_field(s703, 0), 2);
          d704 = 1;
          {
            p_check_46_same_length(f_list_46_length_1(v_ks), f_list_46_length_1(v_args));
            p_check_46_instance(v_ps, &v_m, r_s);
            p_check_46_unify(f_check_46_subst_2(v_r, v_m), v_t, r_s);
            u_value t705 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e706 = &t705;
            u_value l707 = v_ks;
            while (u_is_con(l707, &n_list_46_Cons)) {
              u_value v_kt = u_field(l707, 0);
              if (u_truth(u_mk_bool(1))) {
                e706 = u_append(e706, f_check_46_subst_2(v_kt, v_m));
              }
              l707 = u_field(l707, 1);
            }
            p_check_46_binder_list(v_args, t705, &v_qs, r_xs, r_s);
            u_value t708 = u_mk_con(&n_syntax_46_PApp, 2, v_k, v_qs);
            *r_p1 = t708;
          }
        }
        if ((!d704) && u_is_con(s703, &n_option_46_None)) {
          d704 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d704) {
          u_no_case(s703);
        }
      }
    }
    if ((!d691) && u_is_con(s690, &n_syntax_46_PTuple)) {
      u_value v_items = u_field(s690, 0);
      d691 = 1;
      {
        p_check_46_fresh_list(v_items, &v_ts, r_s);
        p_check_46_unify(u_mk_con(&n_syntax_46_TTuple, 1, v_ts), v_t, r_s);
        p_check_46_binder_list(v_items, v_ts, &v_qs, r_xs, r_s);
        u_value t709 = u_mk_con(&n_syntax_46_PTuple, 1, v_qs);
        *r_p1 = t709;
      }
    }
    if (!d691) {
      u_no_case(s690);
    }
  }
}

void p_check_46_binder_list(u_value v_ps, u_value v_ts, u_value *r_qs, u_value *r_xs, u_value *r_s) {
  {
    u_value v_q = NULL;
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value v_zs = NULL;
    u_value s710 = u_mk_tuple(2, v_ps, v_ts);
    int d711 = 0;
    if ((!d711) && (u_is_con(u_field(s710, 0), &n_list_46_Cons) && u_is_con(u_field(s710, 1), &n_list_46_Cons))) {
      u_value v_p = u_field(u_field(s710, 0), 0);
      u_value v_more = u_field(u_field(s710, 0), 1);
      u_value v_t = u_field(u_field(s710, 1), 0);
      u_value v_ts1 = u_field(u_field(s710, 1), 1);
      d711 = 1;
      {
        p_check_46_binders(v_p, v_t, &v_q, &v_ys, r_s);
        p_check_46_binder_list(v_more, v_ts1, &v_rest, &v_zs, r_s);
        u_value t712 = u_mk_con(&n_list_46_Cons, 2, v_q, v_rest);
        u_value t713 = f_list_46__43__43__2(v_ys, v_zs);
        *r_qs = t712;
        *r_xs = t713;
      }
    }
    if ((!d711) && (u_is_con(u_field(s710, 0), &n_list_46_Nil) && u_is_con(u_field(s710, 1), &n_list_46_Nil))) {
      d711 = 1;
      {
        u_value t714 = u_mk_con(&n_list_46_Nil, 0);
        u_value t715 = u_mk_con(&n_list_46_Nil, 0);
        *r_qs = t714;
        *r_xs = t715;
      }
    }
    if (!d711) {
      d711 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_ps)), "check.TypeError");
      }
    }
    if (!d711) {
      u_no_case(s710);
    }
  }
}

void p_check_46_variable(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s716 = v_x;
    int d717 = 0;
    if ((!d717) && u_is_con(s716, &n_syntax_46_Var)) {
      u_value v_n = u_field(s716, 0);
      u_value v_a = u_field(s716, 1);
      d717 = 1;
      {
        if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, v_assignable)))) {
          {
            u_raise(u_mk_con(&n_check_46_NotAssignable, 1, v_n), "check.TypeError");
          }
        }
        u_value s718 = f_list_46_lookup_2(v_n, v_env);
        int d719 = 0;
        if ((!d719) && u_is_con(s718, &n_option_46_Some)) {
          u_value v_t = u_field(u_field(s718, 0), 1);
          d719 = 1;
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_t, v_u, r_s);
            u_value t720 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_t);
            *r_y = t720;
          }
        }
        if ((!d719) && u_is_con(s718, &n_option_46_None)) {
          d719 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
          }
        }
        if (!d719) {
          u_no_case(s718);
        }
      }
    }
    if (!d717) {
      u_no_case(s716);
    }
  }
}

void p_check_46_variables(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s721 = v_xs;
    int d722 = 0;
    if ((!d722) && u_is_con(s721, &n_list_46_Nil)) {
      d722 = 1;
      {
        u_value t723 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t723;
      }
    }
    if ((!d722) && u_is_con(s721, &n_list_46_Cons)) {
      u_value v_x = u_field(s721, 0);
      u_value v_more = u_field(s721, 1);
      d722 = 1;
      {
        p_check_46_variable(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_variables(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t724 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t724;
      }
    }
    if (!d722) {
      u_no_case(s721);
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
    u_value s725 = v_x;
    int d726 = 0;
    if ((!d726) && u_is_con(s725, &n_syntax_46_SSkip)) {
      d726 = 1;
      {
        u_value t727 = v_x;
        *r_y = t727;
      }
    }
    if ((!d726) && u_is_con(s725, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s725, 0);
      d726 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)));
        p_check_46_variables(v_env, v_assignable, f_list_46_firsts_1(v_pairs), &v_ys, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_pairs), f_syntax_46_types_1(v_ys), &v_es, r_s);
        u_value t728 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_zip_2(v_ys, v_es));
        *r_y = t728;
      }
    }
    if ((!d726) && u_is_con(s725, &n_syntax_46_SBlock)) {
      u_value v_locals = u_field(s725, 0);
      u_value v_body = u_field(s725, 1);
      d726 = 1;
      {
        p_check_46_untyped(v_locals, f_check_46_initialized_1(v_body));
        p_check_46_formals(v_locals, &v_ys, r_s);
        p_check_46_statements(f_list_46__43__43__2(f_check_46_bind_1(v_ys), v_env), f_list_46__43__43__2(f_syntax_46_names_1(v_ys), v_assignable), v_body, &v_xs, r_s);
        u_value t729 = u_mk_con(&n_syntax_46_SBlock, 2, v_ys, v_xs);
        *r_y = t729;
      }
    }
    if ((!d726) && u_is_con(s725, &n_syntax_46_SIf)) {
      u_value v_cond = u_field(s725, 0);
      u_value v_left = u_field(s725, 1);
      u_value v_right = u_field(s725, 2);
      d726 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_left, &v_l, r_s);
        p_check_46_statement(v_env, v_assignable, v_right, &v_r, r_s);
        u_value t730 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
        *r_y = t730;
      }
    }
    if ((!d726) && u_is_con(s725, &n_syntax_46_SWhile)) {
      u_value v_cond = u_field(s725, 0);
      u_value v_body = u_field(s725, 1);
      d726 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        u_value t731 = u_mk_con(&n_syntax_46_SWhile, 2, v_c, v_l);
        *r_y = t731;
      }
    }
    if ((!d726) && u_is_con(s725, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s725, 0);
      u_value v_ins = u_field(s725, 1);
      u_value v_outs = u_field(s725, 2);
      u_value v_refs = u_field(s725, 3);
      d726 = 1;
      {
        p_check_46_call_proc(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d726) && u_is_con(s725, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s725, 0);
      u_value v_cases = u_field(s725, 1);
      d726 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_cases, v_t, &v_bs, r_s);
        u_value t732 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e733 = &t732;
        u_value l734 = v_bs;
        while (u_is_con(l734, &n_list_46_Cons)) {
          u_value v_br = u_field(l734, 0);
          if (u_truth(u_mk_bool(1))) {
            e733 = u_append(e733, f_check_46_case_of_1(v_br));
          }
          l734 = u_field(l734, 1);
        }
        p_check_46_coverage(*r_s, v_t, t732);
        u_value t735 = u_mk_con(&n_syntax_46_SMatch, 2, v_c, v_bs);
        *r_y = t735;
      }
    }
    if ((!d726) && u_is_con(s725, &n_syntax_46_SAssert)) {
      u_value v_cond = u_field(s725, 0);
      d726 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        u_value t736 = u_mk_con(&n_syntax_46_SAssert, 1, v_c);
        *r_y = t736;
      }
    }
    if ((!d726) && u_is_con(s725, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s725, 0);
      u_value v_a = u_field(s725, 1);
      d726 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_unify(v_t, v_u, r_s);
        u_value t737 = u_mk_con(&n_syntax_46_SRaise, 2, v_c, v_t);
        *r_y = t737;
      }
    }
    if ((!d726) && u_is_con(s725, &n_syntax_46_STry)) {
      u_value v_body = u_field(s725, 0);
      u_value v_handlers = u_field(s725, 1);
      d726 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_handlers, &v_hs, r_s);
        u_value t738 = u_mk_con(&n_syntax_46_STry, 2, v_l, v_hs);
        *r_y = t738;
      }
    }
    if (!d726) {
      u_no_case(s725);
    }
  }
}

u_value f_check_46_initialized_1(u_value v_xs) {
  u_value s739 = v_xs;
  u_value t740 = NULL;
  if ((!t740) && (u_is_con(s739, &n_list_46_Cons) && (u_is_con(u_field(s739, 0), &n_syntax_46_SAssign) && (u_is_con(u_field(u_field(s739, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s739, 0), 0), 1), &n_list_46_Nil))))) {
    u_value v_x = u_field(u_field(u_field(u_field(s739, 0), 0), 0), 0);
    u_value v_rest = u_field(s739, 1);
    t740 = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), f_check_46_initialized_1(v_rest));
  }
  if (!t740) {
    t740 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t740) {
    u_no_case(s739);
  }
  return t740;
}

void p_check_46_untyped(u_value v_xs, u_value v_inits) {
  {
    u_value s741 = v_xs;
    int d742 = 0;
    if ((!d742) && u_is_con(s741, &n_list_46_Nil)) {
      d742 = 1;
      {
      }
    }
    if ((!d742) && (u_is_con(s741, &n_list_46_Cons) && u_is_con(u_field(s741, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s741, 0), 0);
      u_value v_t = u_field(u_field(s741, 0), 1);
      u_value v_rest = u_field(s741, 1);
      d742 = 1;
      {
        u_value t743 = f_check_46_holes_1(v_t);
        if (u_truth(t743)) {
          t743 = f_prelude_46_not_1(f_list_46_contains_2(v_n, v_inits));
        }
        if (u_truth(t743)) {
          {
            u_raise(u_mk_con(&n_check_46_MissingType, 1, v_n), "check.TypeError");
          }
        }
        p_check_46_untyped(v_rest, v_inits);
      }
    }
    if (!d742) {
      u_no_case(s741);
    }
  }
}

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s744 = v_xs;
    int d745 = 0;
    if ((!d745) && u_is_con(s744, &n_list_46_Nil)) {
      d745 = 1;
      {
        u_value t746 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t746;
      }
    }
    if ((!d745) && u_is_con(s744, &n_list_46_Cons)) {
      u_value v_x = u_field(s744, 0);
      u_value v_more = u_field(s744, 1);
      d745 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_statements(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t747 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t747;
      }
    }
    if (!d745) {
      u_no_case(s744);
    }
  }
}

u_value f_check_46_os_procs_1(u_value v_c) {
  u_value t752;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_748 = U_LIT("os_args");
    static struct u_object l_os_read_749 = U_LIT("os_read");
    static struct u_object l_os_write_750 = U_LIT("os_write");
    static struct u_object l_os_library_751 = U_LIT("os_library");
    t752 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_748), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_749), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_750), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_751), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)))));
  } else {
    t752 = u_mk_con(&n_list_46_Nil, 0);
  }
  return t752;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t753 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e754 = &t753;
  static struct u_object l_Program_755 = U_LIT("Program");
  u_value l756 = f_ctx_46_names_of_2(v_c, &l_Program_755);
  while (u_is_con(l756, &n_list_46_Cons)) {
    u_value v_d = u_field(l756, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e754 = u_append(e754, v_d);
    }
    l756 = u_field(l756, 1);
  }
  u_value s757 = t753;
  u_value t758 = NULL;
  if ((!t758) && u_is_con(s757, &n_list_46_Cons)) {
    u_value v_d = u_field(s757, 0);
    static struct u_object l_read_759 = U_LIT("read");
    static struct u_object l_library_760 = U_LIT("library");
    t758 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_759), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_760), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t758) {
    t758 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t758) {
    u_no_case(s757);
  }
  return t758;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s762 = f_check_46_visible_names_3(*r_s, v_p, u_mk_closure(lam761, 1, *r_s));
    int d763 = 0;
    if ((!d763) && (u_is_con(s762, &n_list_46_Cons) && u_is_con(u_field(s762, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s762, 0);
      d763 = 1;
      {
        u_value t764 = v_q1;
        v_q = t764;
      }
    }
    if ((!d763) && u_is_con(s762, &n_list_46_Nil)) {
      d763 = 1;
      {
        u_value t765 = v_p;
        v_q = t765;
      }
    }
    if (!d763) {
      u_value v_qs = s762;
      d763 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d763) {
      u_no_case(s762);
    }
    u_value s766 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_State_46_ctx_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_State_46_ctx_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_State_46_ctx_1(*r_s)))));
    int d767 = 0;
    if ((!d767) && u_is_con(s766, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s766, 0), 0);
      u_value v_is = u_field(u_field(s766, 0), 1);
      u_value v_os = u_field(u_field(s766, 0), 2);
      u_value v_rs = u_field(u_field(s766, 0), 3);
      d767 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t768 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e769 = &t768;
        u_value l770 = v_is;
        while (u_is_con(l770, &n_list_46_Cons)) {
          u_value v_t = u_field(l770, 0);
          if (u_truth(u_mk_bool(1))) {
            e769 = u_append(e769, f_check_46_subst_2(v_t, v_m));
          }
          l770 = u_field(l770, 1);
        }
        p_check_46_checks(v_env, v_ins, t768, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t771 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e772 = &t771;
        u_value l773 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l773, &n_list_46_Cons)) {
          u_value v_t = u_field(l773, 0);
          if (u_truth(u_mk_bool(1))) {
            e772 = u_append(e772, f_check_46_subst_2(v_t, v_m));
          }
          l773 = u_field(l773, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t771, r_s);
        u_value t774 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t774;
      }
    }
    if ((!d767) && u_is_con(s766, &n_option_46_None)) {
      d767 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d767) {
      u_no_case(s766);
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
    u_value s775 = v_cases;
    int d776 = 0;
    if ((!d776) && u_is_con(s775, &n_list_46_Nil)) {
      d776 = 1;
      {
        u_value t777 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t777;
      }
    }
    if ((!d776) && (u_is_con(s775, &n_list_46_Cons) && u_is_con(u_field(s775, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s775, 0), 0);
      u_value v_g = u_field(u_field(s775, 0), 1);
      u_value v_b = u_field(u_field(s775, 0), 2);
      u_value v_more = u_field(s775, 1);
      d776 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t778 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t778;
      }
    }
    if (!d776) {
      u_no_case(s775);
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
    u_value s779 = v_hs;
    int d780 = 0;
    if ((!d780) && u_is_con(s779, &n_list_46_Nil)) {
      d780 = 1;
      {
        u_value t781 = u_mk_con(&n_list_46_Nil, 0);
        *r_hs1 = t781;
      }
    }
    if ((!d780) && (u_is_con(s779, &n_list_46_Cons) && u_is_con(u_field(s779, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s779, 0), 0);
      u_value v_p = u_field(u_field(s779, 0), 1);
      u_value v_g = u_field(u_field(s779, 0), 2);
      u_value v_b = u_field(u_field(s779, 0), 3);
      u_value v_more = u_field(s779, 1);
      d780 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t782 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t782;
      }
    }
    if (!d780) {
      u_no_case(s779);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t783 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t783)) {
      t783 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t784 = t783;
    if (!u_truth(t784)) {
      t784 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t785 = t784;
    if (!u_truth(t785)) {
      t785 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n))), u_mk_con(&n_list_46_Nil, 0));
    }
    if (u_truth(t785)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t786 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t786)) {
      t786 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t787 = t786;
    if (!u_truth(t787)) {
      t787 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t787)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_788 = U_LIT("Bool");
    static struct u_object l_Int_789 = U_LIT("Int");
    static struct u_object l_Str_790 = U_LIT("Str");
    static struct u_object l_Char_791 = U_LIT("Char");
    static struct u_object l__45__62__792 = U_LIT("->");
    u_value t793 = f_list_46_contains_2(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_788, u_mk_con(&n_list_46_Cons, 2, &l_Int_789, u_mk_con(&n_list_46_Cons, 2, &l_Str_790, u_mk_con(&n_list_46_Cons, 2, &l_Char_791, u_mk_con(&n_list_46_Cons, 2, &l__45__62__792, u_mk_con(&n_list_46_Nil, 0)))))));
    if (!u_truth(t793)) {
      t793 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t794 = t793;
    if (!u_truth(t794)) {
      t794 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t794)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t795 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e796 = &t795;
    u_value l797 = v_ts;
    while (u_is_con(l797, &n_list_46_Cons)) {
      u_value v_t = u_field(l797, 0);
      if (u_truth(u_mk_bool(1))) {
        e796 = u_append(e796, f_check_46_free_1(v_t));
      }
      l797 = u_field(l797, 1);
    }
    u_value t798 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t795)), f_check_46_params_1(v_ps));
    v_undeclared = t798;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, u_mk_con(&n_list_46_Nil, 0)))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
  {
    u_value s799 = v_cs;
    int d800 = 0;
    if ((!d800) && u_is_con(s799, &n_list_46_Nil)) {
      d800 = 1;
      {
      }
    }
    if ((!d800) && (u_is_con(s799, &n_list_46_Cons) && u_is_con(u_field(u_field(s799, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s799, 0), 0);
      u_value v_n = u_field(u_field(u_field(s799, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s799, 0), 1), 1);
      u_value v_rest = u_field(s799, 1);
      d800 = 1;
      {
        u_value t801 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t801;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        u_value t802 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t802;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d800) && u_is_con(s799, &n_list_46_Cons)) {
      u_value v_rest = u_field(s799, 1);
      d800 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d800) {
      u_no_case(s799);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s803 = v_cs;
    int d804 = 0;
    if ((!d804) && u_is_con(s803, &n_list_46_Nil)) {
      d804 = 1;
      {
      }
    }
    if ((!d804) && (u_is_con(s803, &n_list_46_Cons) && u_is_con(u_field(u_field(s803, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s803, 0), 0);
      u_value v_n = u_field(u_field(u_field(s803, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s803, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s803, 0), 1), 2);
      u_value v_rest = u_field(s803, 1);
      d804 = 1;
      {
        u_value t805 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t805;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)));
        u_value t806 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_State_46_ctx_1(*r_s), v_n, v_ps, v_u));
        *r_s = t806;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d804) && u_is_con(s803, &n_list_46_Cons)) {
      u_value v_rest = u_field(s803, 1);
      d804 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d804) {
      u_no_case(s803);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
  {
    u_value s807 = v_cs;
    int d808 = 0;
    if ((!d808) && u_is_con(s807, &n_list_46_Nil)) {
      d808 = 1;
      {
      }
    }
    if ((!d808) && (u_is_con(s807, &n_list_46_Cons) && u_is_con(u_field(u_field(s807, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s807, 0), 0);
      u_value v_n = u_field(u_field(u_field(s807, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s807, 0), 1), 1);
      u_value v_ks = u_field(u_field(u_field(s807, 0), 1), 2);
      u_value v_rest = u_field(s807, 1);
      d808 = 1;
      {
        u_value t809 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t809;
        p_check_46_unique_ids(v_ps);
        u_value t810 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e811 = &t810;
        u_value l812 = v_ks;
        while (u_is_con(l812, &n_list_46_Cons)) {
          u_value v_k = u_field(l812, 0);
          if (u_truth(u_mk_bool(1))) {
            e811 = u_append(e811, f_syntax_46_Constr_46_name_1(v_k));
          }
          l812 = u_field(l812, 1);
        }
        u_value t813 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_State_46_ctx_1(*r_s), v_n, t810));
        *r_s = t813;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d808) && u_is_con(s807, &n_list_46_Cons)) {
      u_value v_rest = u_field(s807, 1);
      d808 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d808) {
      u_no_case(s807);
    }
  }
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s814 = v_ks;
    int d815 = 0;
    if ((!d815) && u_is_con(s814, &n_list_46_Nil)) {
      d815 = 1;
      {
      }
    }
    if ((!d815) && (u_is_con(s814, &n_list_46_Cons) && u_is_con(u_field(s814, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s814, 0), 0);
      u_value v_args = u_field(u_field(s814, 0), 1);
      u_value v_rest = u_field(s814, 1);
      d815 = 1;
      {
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t816 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_State_46_ctx_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t816;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d815) {
      u_no_case(s814);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s817 = v_xss;
  u_value t818 = NULL;
  if ((!t818) && u_is_con(s817, &n_list_46_Nil)) {
    t818 = v_r;
  }
  if ((!t818) && u_is_con(s817, &n_list_46_Cons)) {
    u_value v_xs = u_field(s817, 0);
    u_value v_rest = u_field(s817, 1);
    t818 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t818) {
    u_no_case(s817);
  }
  return t818;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s819 = v_xss;
  u_value t820 = NULL;
  if ((!t820) && u_is_con(s819, &n_list_46_Nil)) {
    t820 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t820) && u_is_con(s819, &n_list_46_Cons)) {
    u_value v_xs = u_field(s819, 0);
    t820 = v_xs;
  }
  if (!t820) {
    u_no_case(s819);
  }
  return t820;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s821 = v_xss;
    int d822 = 0;
    if ((!d822) && u_is_con(s821, &n_list_46_Nil)) {
      d822 = 1;
      {
        u_value t823 = u_mk_con(&n_list_46_Nil, 0);
        *r_yss = t823;
      }
    }
    if ((!d822) && u_is_con(s821, &n_list_46_Cons)) {
      u_value v_xs = u_field(s821, 0);
      u_value v_more = u_field(s821, 1);
      d822 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t824 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t824;
      }
    }
    if (!d822) {
      u_no_case(s821);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s825 = v_cs;
    int d826 = 0;
    if ((!d826) && u_is_con(s825, &n_list_46_Nil)) {
      d826 = 1;
      {
      }
    }
    if ((!d826) && (u_is_con(s825, &n_list_46_Cons) && u_is_con(u_field(u_field(s825, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s825, 0), 0);
      u_value v_n = u_field(u_field(u_field(s825, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s825, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s825, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s825, 0), 1), 3);
      u_value v_rest = u_field(s825, 1);
      d826 = 1;
      {
        u_value t827 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t827;
        p_check_46_overload(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), u_mk_con(&n_list_46_Nil, 0)));
        u_value t828 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t828;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d826) && u_is_con(s825, &n_list_46_Cons)) {
      u_value v_rest = u_field(s825, 1);
      d826 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d826) {
      u_no_case(s825);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s829 = v_cs;
    int d830 = 0;
    if ((!d830) && u_is_con(s829, &n_list_46_Nil)) {
      d830 = 1;
      {
      }
    }
    if ((!d830) && (u_is_con(s829, &n_list_46_Cons) && u_is_con(u_field(u_field(s829, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s829, 0), 0);
      u_value v_n = u_field(u_field(u_field(s829, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s829, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s829, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s829, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s829, 0), 1), 4);
      u_value v_rest = u_field(s829, 1);
      d830 = 1;
      {
        u_value t831 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t831;
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t832 = f_syntax_46_types_1(v_xs);
        v_ts = t832;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t833 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t833;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d830) && u_is_con(s829, &n_list_46_Cons)) {
      u_value v_rest = u_field(s829, 1);
      d830 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d830) {
      u_no_case(s829);
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
    u_value s834 = v_c;
    int d835 = 0;
    if ((!d835) && u_is_con(s834, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s834, 0);
      u_value v_ps = u_field(s834, 1);
      u_value v_bound = u_field(s834, 2);
      u_value v_res = u_field(s834, 3);
      u_value v_body = u_field(s834, 4);
      d835 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t836 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t836;
      }
    }
    if ((!d835) && u_is_con(s834, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s834, 0);
      u_value v_ps = u_field(s834, 1);
      u_value v_ins = u_field(s834, 2);
      u_value v_outs = u_field(s834, 3);
      u_value v_refs = u_field(s834, 4);
      u_value v_body = u_field(s834, 5);
      d835 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t837 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t837;
      }
    }
    if ((!d835) && u_is_con(s834, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s834, 0);
      d835 = 1;
      {
        p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_b, &v_t, r_s);
        u_value t838 = u_mk_con(&n_syntax_46_CEval, 1, v_b);
        *r_c1 = t838;
      }
    }
    if ((!d835) && u_is_con(s834, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s834, 0);
      u_value v_body = u_field(s834, 1);
      d835 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t839 = u_mk_con(&n_syntax_46_CRun, 2, v_n, v_y);
        *r_c1 = t839;
      }
    }
    if ((!d835) && u_is_con(s834, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s834, 0);
      u_value v_body = u_field(s834, 1);
      d835 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t840 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t840;
      }
    }
    if (!d835) {
      d835 = 1;
      {
        u_value t841 = v_c;
        *r_c1 = t841;
      }
    }
    if (!d835) {
      u_no_case(s834);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s842 = v_cs;
    int d843 = 0;
    if ((!d843) && u_is_con(s842, &n_list_46_Nil)) {
      d843 = 1;
      {
        u_value t844 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t844;
      }
    }
    if ((!d843) && u_is_con(s842, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s842, 0), 0);
      u_value v_c = u_field(u_field(s842, 0), 1);
      u_value v_more = u_field(s842, 1);
      d843 = 1;
      {
        u_value t845 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t845;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t846 = f_check_46_zonk_cmd_2(f_check_46_State_46_sub_1(*r_s), v_c1);
        v_c2 = t846;
        u_value s847 = f_check_46_unknown_in_cmd_1(v_c2);
        int d848 = 0;
        if ((!d848) && u_is_con(s847, &n_option_46_Some)) {
          u_value v_t = u_field(s847, 0);
          d848 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d848) && u_is_con(s847, &n_option_46_None)) {
          d848 = 1;
          {
          }
        }
        if (!d848) {
          u_no_case(s847);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t849 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t849;
      }
    }
    if (!d843) {
      u_no_case(s842);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s850 = v_cs;
  u_value t851 = NULL;
  if ((!t851) && u_is_con(s850, &n_list_46_Nil)) {
    t851 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t851) && (u_is_con(s850, &n_list_46_Cons) && u_is_con(u_field(u_field(s850, 0), 1), &n_syntax_46_CRun))) {
    u_value v_n = u_field(u_field(u_field(s850, 0), 1), 0);
    u_value v_more = u_field(s850, 1);
    t851 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t851) && (u_is_con(s850, &n_list_46_Cons) && u_is_con(u_field(u_field(s850, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s850, 0), 1), 0);
    u_value v_more = u_field(s850, 1);
    t851 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t851) && u_is_con(s850, &n_list_46_Cons)) {
    u_value v_more = u_field(s850, 1);
    t851 = f_check_46_block_names_1(v_more);
  }
  if (!t851) {
    u_no_case(s850);
  }
  return t851;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_s = NULL;
    u_value v_ds = NULL;
    u_value v_cs = NULL;
    u_value s852 = v_p;
    int d853 = 0;
    if ((!d853) && u_is_con(s852, &n_syntax_46_Program)) {
      u_value v_cmds = u_field(s852, 0);
      d853 = 1;
      {
        u_value t854 = f_check_46_flatten_2(v_main, v_cmds);
        v_ds = t854;
        u_value t855 = f_check_46_start_2(v_main, f_check_46_visibility_2(v_ds, v_main));
        v_s = t855;
        p_check_46_declare_datas(v_ds, &v_s);
        p_check_46_declare_sorts(v_ds, &v_s);
        p_check_46_declare_constrs(v_ds, &v_s);
        p_check_46_declare_funs(v_ds, &v_s);
        p_check_46_declare_procs(v_ds, &v_s);
        p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
        p_check_46_commands(v_ds, &v_cs, &v_s);
        u_value t856 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
        *r_q = t856;
      }
    }
    if (!d853) {
      u_no_case(s852);
    }
  }
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_857 = U_LIT("main");
    u_value t858 = f_check_46_start_2(&l_main_857, u_mk_con(&n_list_46_Nil, 0));
    v_s = t858;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t859 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(v_s), v_u);
    *r_t = t859;
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
  static struct u_object l_List_860 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_860), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_861 = U_LIT("list");
  static struct u_object l_List_862 = U_LIT("List");
  static struct u_object l_A_863 = U_LIT("A");
  static struct u_object l_Nil_864 = U_LIT("Nil");
  static struct u_object l_Cons_865 = U_LIT("Cons");
  static struct u_object l_A_866 = U_LIT("A");
  static struct u_object l_A_867 = U_LIT("A");
  static struct u_object l_list_868 = U_LIT("list");
  static struct u_object l_len_869 = U_LIT("len");
  static struct u_object l_A_870 = U_LIT("A");
  static struct u_object l_xs_871 = U_LIT("xs");
  static struct u_object l_A_872 = U_LIT("A");
  static struct u_object l_xs_873 = U_LIT("xs");
  static struct u_object l_Nil_874 = U_LIT("Nil");
  static struct u_object l_Cons_875 = U_LIT("Cons");
  static struct u_object l_ys_876 = U_LIT("ys");
  static struct u_object l__43__877 = U_LIT("+");
  static struct u_object l_len_878 = U_LIT("len");
  static struct u_object l_ys_879 = U_LIT("ys");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_861, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_862), u_mk_con(&n_list_46_Cons, 2, &l_A_863, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_864), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_865), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_866), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_867)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_868), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_869), u_mk_con(&n_list_46_Cons, 2, &l_A_870, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_871), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_872))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_873), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_874), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_875), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_876), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__877, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_878, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_879), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s880 = v_t;
  u_value t881 = NULL;
  if ((!t881) && u_is_con(s880, &n_c_46_CtVoid)) {
    static struct u_object l_void_882 = U_LIT("void");
    t881 = &l_void_882;
  }
  if ((!t881) && u_is_con(s880, &n_c_46_CtInt)) {
    static struct u_object l_int_883 = U_LIT("int");
    t881 = &l_int_883;
  }
  if ((!t881) && u_is_con(s880, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s880, 0);
    t881 = v_n;
  }
  if ((!t881) && (u_is_con(s880, &n_c_46_CtPtr) && u_is_con(u_field(s880, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s880, 0), 0);
    static struct u_object l__42__884 = U_LIT("*");
    t881 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__884);
  }
  if ((!t881) && u_is_con(s880, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s880, 0);
    static struct u_object l__32__42__885 = U_LIT(" *");
    t881 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__885);
  }
  if (!t881) {
    u_no_case(s880);
  }
  return t881;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s886 = v_t;
  u_value t887 = NULL;
  if ((!t887) && u_is_con(s886, &n_c_46_CtPtr)) {
    t887 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t887) {
    static struct u_object l__32__888 = U_LIT(" ");
    t887 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__888, v_x));
  }
  if (!t887) {
    u_no_case(s886);
  }
  return t887;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s889 = v_e;
  u_value t890 = NULL;
  if ((!t890) && u_is_con(s889, &n_c_46_CeVar)) {
    u_value v_x = u_field(s889, 0);
    t890 = v_x;
  }
  if ((!t890) && u_is_con(s889, &n_c_46_CeNum)) {
    u_value v_n = u_field(s889, 0);
    t890 = u_str_show(v_n);
  }
  if ((!t890) && u_is_con(s889, &n_c_46_CeStr)) {
    u_value v_s = u_field(s889, 0);
    t890 = u_str_quote(v_s);
  }
  if ((!t890) && u_is_con(s889, &n_c_46_CeCall)) {
    u_value v_f = u_field(s889, 0);
    u_value v_args = u_field(s889, 1);
    static struct u_object l__40__891 = U_LIT("(");
    static struct u_object l__44__32__892 = U_LIT(", ");
    u_value t893 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e894 = &t893;
    u_value l895 = v_args;
    while (u_is_con(l895, &n_list_46_Cons)) {
      u_value v_a = u_field(l895, 0);
      if (u_truth(u_mk_bool(1))) {
        e894 = u_append(e894, f_c_46_c_expr_1(v_a));
      }
      l895 = u_field(l895, 1);
    }
    static struct u_object l__41__896 = U_LIT(")");
    t890 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__891, u_concat(u_str_join(&l__44__32__892, t893), &l__41__896)));
  }
  if ((!t890) && u_is_con(s889, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s889, 0);
    u_value v_a = u_field(s889, 1);
    t890 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t890) && u_is_con(s889, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s889, 0);
    u_value v_a = u_field(s889, 1);
    u_value v_b = u_field(s889, 2);
    static struct u_object l__32__897 = U_LIT(" ");
    static struct u_object l__32__898 = U_LIT(" ");
    t890 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__897, u_concat(v_o, u_concat(&l__32__898, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t890) && u_is_con(s889, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s889, 0);
    u_value v_i = u_field(s889, 1);
    static struct u_object l__91__899 = U_LIT("[");
    static struct u_object l__93__900 = U_LIT("]");
    t890 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__899, u_concat(f_c_46_c_expr_1(v_i), &l__93__900)));
  }
  if ((!t890) && u_is_con(s889, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s889, 0);
    u_value v_f = u_field(s889, 1);
    static struct u_object l__45__62__901 = U_LIT("->");
    t890 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__901, v_f));
  }
  if ((!t890) && u_is_con(s889, &n_c_46_CeCast)) {
    u_value v_t = u_field(s889, 0);
    u_value v_a = u_field(s889, 1);
    static struct u_object l__40__902 = U_LIT("(");
    static struct u_object l__41__903 = U_LIT(")");
    t890 = u_concat(&l__40__902, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__903, f_c_46_c_operand_1(v_a))));
  }
  if ((!t890) && u_is_con(s889, &n_c_46_CeCond)) {
    u_value v_c = u_field(s889, 0);
    u_value v_a = u_field(s889, 1);
    u_value v_b = u_field(s889, 2);
    static struct u_object l__32__63__32__904 = U_LIT(" ? ");
    static struct u_object l__32__58__32__905 = U_LIT(" : ");
    t890 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__904, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__905, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t890) && u_is_con(s889, &n_c_46_CeInit)) {
    u_value v_es = u_field(s889, 0);
    static struct u_object l__123__906 = U_LIT("{");
    static struct u_object l__44__32__907 = U_LIT(", ");
    u_value t908 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e909 = &t908;
    u_value l910 = v_es;
    while (u_is_con(l910, &n_list_46_Cons)) {
      u_value v_a = u_field(l910, 0);
      if (u_truth(u_mk_bool(1))) {
        e909 = u_append(e909, f_c_46_c_expr_1(v_a));
      }
      l910 = u_field(l910, 1);
    }
    static struct u_object l__125__911 = U_LIT("}");
    t890 = u_concat(&l__123__906, u_concat(u_str_join(&l__44__32__907, t908), &l__125__911));
  }
  if (!t890) {
    u_no_case(s889);
  }
  return t890;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s912 = v_e;
  u_value t913 = NULL;
  if ((!t913) && u_is_con(s912, &n_c_46_CeVar)) {
    t913 = f_c_46_c_expr_1(v_e);
  }
  if ((!t913) && u_is_con(s912, &n_c_46_CeNum)) {
    u_value v_n = u_field(s912, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t913 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t913) && u_is_con(s912, &n_c_46_CeStr)) {
    t913 = f_c_46_c_expr_1(v_e);
  }
  if ((!t913) && u_is_con(s912, &n_c_46_CeCall)) {
    t913 = f_c_46_c_expr_1(v_e);
  }
  if ((!t913) && u_is_con(s912, &n_c_46_CeIndex)) {
    t913 = f_c_46_c_expr_1(v_e);
  }
  if ((!t913) && u_is_con(s912, &n_c_46_CeArrow)) {
    t913 = f_c_46_c_expr_1(v_e);
  }
  if (!t913) {
    static struct u_object l__40__914 = U_LIT("(");
    static struct u_object l__41__915 = U_LIT(")");
    t913 = u_concat(&l__40__914, u_concat(f_c_46_c_expr_1(v_e), &l__41__915));
  }
  if (!t913) {
    u_no_case(s912);
  }
  return t913;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s916 = v_s;
  u_value t917 = NULL;
  if ((!t917) && u_is_con(s916, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s916, 0);
    static struct u_object l__59__918 = U_LIT(";");
    t917 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__918)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t917) && (u_is_con(s916, &n_c_46_CsDecl) && u_is_con(u_field(s916, 2), &n_option_46_None))) {
    u_value v_t = u_field(s916, 0);
    u_value v_x = u_field(s916, 1);
    static struct u_object l__59__919 = U_LIT(";");
    t917 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__919)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t917) && (u_is_con(s916, &n_c_46_CsDecl) && u_is_con(u_field(s916, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s916, 0);
    u_value v_x = u_field(s916, 1);
    u_value v_e = u_field(u_field(s916, 2), 0);
    static struct u_object l__32__61__32__920 = U_LIT(" = ");
    static struct u_object l__59__921 = U_LIT(";");
    t917 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__920, u_concat(f_c_46_c_expr_1(v_e), &l__59__921)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t917) && u_is_con(s916, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s916, 0);
    u_value v_e = u_field(s916, 1);
    static struct u_object l__32__61__32__922 = U_LIT(" = ");
    static struct u_object l__59__923 = U_LIT(";");
    t917 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__922, u_concat(f_c_46_c_expr_1(v_e), &l__59__923)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t917) && (u_is_con(s916, &n_c_46_CsIf) && u_is_con(u_field(s916, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s916, 0);
    u_value v_l = u_field(s916, 1);
    static struct u_object l_if_32__40__924 = U_LIT("if (");
    static struct u_object l__41__32__123__925 = U_LIT(") {");
    static struct u_object l__32__32__926 = U_LIT("  ");
    static struct u_object l__125__927 = U_LIT("}");
    t917 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__924, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__925))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__926), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__927), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t917) && u_is_con(s916, &n_c_46_CsIf)) {
    u_value v_c = u_field(s916, 0);
    u_value v_l = u_field(s916, 1);
    u_value v_r = u_field(s916, 2);
    static struct u_object l_if_32__40__928 = U_LIT("if (");
    static struct u_object l__41__32__123__929 = U_LIT(") {");
    static struct u_object l__32__32__930 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__931 = U_LIT("} else {");
    static struct u_object l__32__32__932 = U_LIT("  ");
    static struct u_object l__125__933 = U_LIT("}");
    t917 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__928, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__929))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__930), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__931), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__932), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__933), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t917) && u_is_con(s916, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s916, 0);
    u_value v_b = u_field(s916, 1);
    static struct u_object l_while_32__40__934 = U_LIT("while (");
    static struct u_object l__41__32__123__935 = U_LIT(") {");
    static struct u_object l__32__32__936 = U_LIT("  ");
    static struct u_object l__125__937 = U_LIT("}");
    t917 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__934, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__935))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__936), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__937), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t917) && (u_is_con(s916, &n_c_46_CsReturn) && u_is_con(u_field(s916, 0), &n_option_46_None))) {
    static struct u_object l_return_59__938 = U_LIT("return;");
    t917 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__938), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t917) && (u_is_con(s916, &n_c_46_CsReturn) && u_is_con(u_field(s916, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s916, 0), 0);
    static struct u_object l_return_32__939 = U_LIT("return ");
    static struct u_object l__59__940 = U_LIT(";");
    t917 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__939, u_concat(f_c_46_c_expr_1(v_e), &l__59__940))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t917) && u_is_con(s916, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s916, 0);
    static struct u_object l__123__941 = U_LIT("{");
    static struct u_object l__32__32__942 = U_LIT("  ");
    static struct u_object l__125__943 = U_LIT("}");
    t917 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__941), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__942), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__943), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t917) {
    u_no_case(s916);
  }
  return t917;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t944 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e945 = &t944;
  u_value l946 = v_ss;
  while (u_is_con(l946, &n_list_46_Cons)) {
    u_value v_s = u_field(l946, 0);
    if (u_truth(u_mk_bool(1))) {
      e945 = u_append(e945, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l946 = u_field(l946, 1);
  }
  return f_list_46_concat_1(t944);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s947 = v_ps;
  u_value t948 = NULL;
  if ((!t948) && u_is_con(s947, &n_list_46_Nil)) {
    static struct u_object l_void_949 = U_LIT("void");
    t948 = &l_void_949;
  }
  if (!t948) {
    static struct u_object l__44__32__950 = U_LIT(", ");
    u_value t951 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e952 = &t951;
    u_value l953 = v_ps;
    while (u_is_con(l953, &n_list_46_Cons)) {
      u_value v_p = u_field(l953, 0);
      if (u_truth(u_mk_bool(1))) {
        e952 = u_append(e952, f_c_46_c_param_1(v_p));
      }
      l953 = u_field(l953, 1);
    }
    t948 = u_str_join(&l__44__32__950, t951);
  }
  if (!t948) {
    u_no_case(s947);
  }
  return t948;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s954 = v_p;
  u_value t955 = NULL;
  if (!t955) {
    u_value v_t = u_field(s954, 0);
    u_value v_x = u_field(s954, 1);
    t955 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t955) {
    u_no_case(s954);
  }
  return t955;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s956 = v_d;
  u_value t957 = NULL;
  if ((!t957) && u_is_con(s956, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s956, 0);
    static struct u_object l__35_include_32__958 = U_LIT("#include ");
    t957 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__958, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t957) && u_is_con(s956, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s956, 0);
    u_value v_f = u_field(s956, 1);
    u_value v_ps = u_field(s956, 2);
    static struct u_object l__40__959 = U_LIT("(");
    static struct u_object l__41__59__960 = U_LIT(");");
    t957 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__959, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__960))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t957) && u_is_con(s956, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s956, 0);
    u_value v_f = u_field(s956, 1);
    u_value v_ps = u_field(s956, 2);
    u_value v_b = u_field(s956, 3);
    static struct u_object l__40__961 = U_LIT("(");
    static struct u_object l__41__32__123__962 = U_LIT(") {");
    static struct u_object l__32__32__963 = U_LIT("  ");
    static struct u_object l__125__964 = U_LIT("}");
    t957 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__961, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__962))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__963, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__964, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t957) && (u_is_con(s956, &n_c_46_CdGlobal) && u_is_con(u_field(s956, 2), &n_option_46_None))) {
    u_value v_t = u_field(s956, 0);
    u_value v_x = u_field(s956, 1);
    static struct u_object l__59__965 = U_LIT(";");
    t957 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__965), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t957) && (u_is_con(s956, &n_c_46_CdGlobal) && u_is_con(u_field(s956, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s956, 0);
    u_value v_x = u_field(s956, 1);
    u_value v_e = u_field(u_field(s956, 2), 0);
    static struct u_object l__32__61__32__966 = U_LIT(" = ");
    static struct u_object l__59__967 = U_LIT(";");
    t957 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__966, u_concat(f_c_46_c_expr_1(v_e), &l__59__967))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t957) {
    u_no_case(s956);
  }
  return t957;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s968 = v_f;
  u_value t969 = NULL;
  if ((!t969) && u_is_con(s968, &n_c_46_CFile)) {
    u_value v_ds = u_field(s968, 0);
    static struct u_object l__10__10__970 = U_LIT("\n\n");
    u_value t971 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e972 = &t971;
    u_value l973 = v_ds;
    while (u_is_con(l973, &n_list_46_Cons)) {
      u_value v_d = u_field(l973, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__974 = U_LIT("\n");
        e972 = u_append(e972, u_str_join(&l__10__974, f_c_46_c_decl_1(v_d)));
      }
      l973 = u_field(l973, 1);
    }
    static struct u_object l__10__975 = U_LIT("\n");
    t969 = u_concat(u_str_join(&l__10__10__970, t971), &l__10__975);
  }
  if (!t969) {
    u_no_case(s968);
  }
  return t969;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__976 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__976, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__44__59__46__977 = U_LIT("()[],;.");
  return u_str_contains(&l__40__41__91__93__44__59__46__977, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t978 = u_is_letter(v_c);
  if (!u_truth(t978)) {
    t978 = u_is_digit(v_c);
  }
  u_value t979 = t978;
  if (!u_truth(t979)) {
    t979 = u_eq(v_c, u_mk_char(95));
  }
  return t979;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t980;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t980 = u_str_get(v_s, v_i);
  } else {
    t980 = u_chr(u_mk_int(0));
  }
  return t980;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s981 = v_c;
  u_value t982 = NULL;
  if ((!t982) && u_equals(s981, u_mk_char(34))) {
    t982 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t982) && u_equals(s981, u_mk_char(39))) {
    t982 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t982) && u_equals(s981, u_mk_char(92))) {
    t982 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t982) && u_equals(s981, u_mk_char(110))) {
    t982 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t982) && u_equals(s981, u_mk_char(116))) {
    t982 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t982) && u_equals(s981, u_mk_char(114))) {
    t982 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t982) {
    t982 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t982) {
    u_no_case(s981);
  }
  return t982;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t983;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t983 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t983 = v_i;
  }
  return t983;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t984 = u_mk_int(0);
    u_value t985 = u_mk_int(1);
    u_value t986 = u_mk_int(1);
    v_j = t984;
    *r_line = t985;
    *r_column = t986;
    u_value t991 = u_lt(v_j, v_i);
    while (u_truth(t991)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t987 = u_add(*r_line, u_mk_int(1));
            u_value t988 = u_mk_int(1);
            *r_line = t987;
            *r_column = t988;
          }
        } else {
          {
            u_value t989 = u_add(*r_column, u_mk_int(1));
            *r_column = t989;
          }
        }
        u_value t990 = u_add(v_j, u_mk_int(1));
        v_j = t990;
      }
      t991 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t992 = *r_i;
    u_value t993 = u_add(*r_i, u_mk_int(1));
    u_value t994 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t992;
    *r_i = t993;
    v_parts = t994;
    u_value t995 = *r_i;
    v_from = t995;
    u_value t1006 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t1006)) {
      {
        u_value t996 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t996;
        u_value t997 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t997)) {
          t997 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t997)) {
          {
            static struct u_object l_unterminated_32_lit_998 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_998, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s999 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d1000 = 0;
            if ((!d1000) && u_is_con(s999, &n_option_46_Some)) {
              u_value v_d = u_field(s999, 0);
              d1000 = 1;
              {
                u_value t1001 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t1002 = u_add(*r_i, u_mk_int(2));
                v_parts = t1001;
                *r_i = t1002;
                u_value t1003 = *r_i;
                v_from = t1003;
              }
            }
            if ((!d1000) && u_is_con(s999, &n_option_46_None)) {
              d1000 = 1;
              {
                static struct u_object l_invalid_32_escape_1004 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_1004, *r_i), "lex.LexError");
              }
            }
            if (!d1000) {
              u_no_case(s999);
            }
          }
        } else {
          {
            u_value t1005 = u_add(*r_i, u_mk_int(1));
            *r_i = t1005;
          }
        }
      }
      t1006 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__1007 = U_LIT("");
    u_value t1008 = u_str_join(&l__1007, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t1009 = u_add(*r_i, u_mk_int(1));
    *r_text = t1008;
    *r_i = t1009;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1010 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t1010;
    u_value t1011 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1012 = v_j;
    *r_t = t1011;
    *r_i = t1012;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1013 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t1013;
    u_value s1014 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d1015 = 0;
    if ((!d1015) && u_is_con(s1014, &n_option_46_Some)) {
      u_value v_n = u_field(s1014, 0);
      d1015 = 1;
      {
        u_value t1016 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t1017 = v_j;
        *r_t = t1016;
        *r_i = t1017;
      }
    }
    if ((!d1015) && u_is_con(s1014, &n_option_46_None)) {
      d1015 = 1;
      {
        static struct u_object l_invalid_32_number_1018 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_1018, *r_i), "lex.LexError");
      }
    }
    if (!d1015) {
      u_no_case(s1014);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1019 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t1019;
    u_value t1020 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1021 = v_j;
    *r_t = t1020;
    *r_i = t1021;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t1022 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t1022;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t1023 = *r_i;
    v_start = t1023;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_1024 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_1024, v_start), "lex.LexError");
      }
    }
    u_value t1025 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t1025;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t1026 = u_str_get(v_s, *r_i);
    v_c = t1026;
    u_value s1027 = v_c;
    int d1028 = 0;
    if (!d1028) {
      u_value t1029 = u_is_letter(v_c);
      if (!u_truth(t1029)) {
        t1029 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t1029)) {
        d1028 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d1028) {
      if (u_truth(u_is_digit(v_c))) {
        d1028 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d1028) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d1028 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d1028) && u_equals(s1027, u_mk_char(34))) {
      d1028 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d1028) && u_equals(s1027, u_mk_char(39))) {
      d1028 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d1028) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d1028 = 1;
        {
          u_value t1030 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t1031 = u_add(*r_i, u_mk_int(1));
          *r_t = t1030;
          *r_i = t1031;
        }
      }
    }
    if (!d1028) {
      d1028 = 1;
      {
        static struct u_object l_unexpected_32_chara_1032 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_1032, *r_i), "lex.LexError");
      }
    }
    if (!d1028) {
      u_no_case(s1027);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t1033 = u_mk_int(0);
    v_i = t1033;
    u_value t1034 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1034;
    u_value t1041 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t1041)) {
      {
        u_value t1035 = v_i;
        v_start = t1035;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t1036 = u_add(v_i, u_mk_int(1));
            v_i = t1036;
          }
        } else {
          u_value t1037 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t1037)) {
            t1037 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t1037)) {
            {
              u_value t1039 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam1038, 0));
              v_i = t1039;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t1040 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t1040;
            }
          }
        }
      }
      t1041 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t1042 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t1042;
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
  u_value s1043 = v_s;
  u_value t1044 = NULL;
  if ((!t1044) && u_is_con(s1043, &n_set_46_SLeaf)) {
    t1044 = u_mk_bool(0);
  }
  if ((!t1044) && u_is_con(s1043, &n_set_46_SNode)) {
    u_value v_l = u_field(s1043, 0);
    u_value v_y = u_field(s1043, 1);
    u_value v_r = u_field(s1043, 2);
    u_value t1048 = u_compare(v_x, v_y);
    u_value t1047;
    {
      u_value v_c = t1048;
      u_value t1046;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t1046 = f_set_46_member_2(v_x, v_l);
      } else {
        u_value t1045;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t1045 = f_set_46_member_2(v_x, v_r);
        } else {
          t1045 = u_mk_bool(1);
        }
        t1046 = t1045;
      }
      t1047 = t1046;
    }
    t1044 = t1047;
  }
  if (!t1044) {
    u_no_case(s1043);
  }
  return t1044;
}

u_value f_set_46_insert_2(u_value v_x, u_value v_s) {
  u_value s1049 = v_s;
  u_value t1050 = NULL;
  if ((!t1050) && u_is_con(s1049, &n_set_46_SLeaf)) {
    t1050 = u_mk_con(&n_set_46_SNode, 4, u_mk_con(&n_set_46_SLeaf, 0), v_x, u_mk_con(&n_set_46_SLeaf, 0), u_mk_int(1));
  }
  if ((!t1050) && u_is_con(s1049, &n_set_46_SNode)) {
    u_value v_l = u_field(s1049, 0);
    u_value v_y = u_field(s1049, 1);
    u_value v_r = u_field(s1049, 2);
    u_value t1054 = u_compare(v_x, v_y);
    u_value t1053;
    {
      u_value v_c = t1054;
      u_value t1052;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t1052 = f_set_46_rebalance_3(f_set_46_insert_2(v_x, v_l), v_y, v_r);
      } else {
        u_value t1051;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t1051 = f_set_46_rebalance_3(v_l, v_y, f_set_46_insert_2(v_x, v_r));
        } else {
          t1051 = v_s;
        }
        t1052 = t1051;
      }
      t1053 = t1052;
    }
    t1050 = t1053;
  }
  if (!t1050) {
    u_no_case(s1049);
  }
  return t1050;
}

u_value f_set_46_from_list_1(u_value v_xs) {
  return f_set_46_insert_all_2(v_xs, u_mk_con(&n_set_46_SLeaf, 0));
}

u_value f_set_46_insert_all_2(u_value v_xs, u_value v_s) {
  u_value s1055 = v_xs;
  u_value t1056 = NULL;
  if ((!t1056) && u_is_con(s1055, &n_list_46_Nil)) {
    t1056 = v_s;
  }
  if ((!t1056) && u_is_con(s1055, &n_list_46_Cons)) {
    u_value v_x = u_field(s1055, 0);
    u_value v_rest = u_field(s1055, 1);
    t1056 = f_set_46_insert_all_2(v_rest, f_set_46_insert_2(v_x, v_s));
  }
  if (!t1056) {
    u_no_case(s1055);
  }
  return t1056;
}

u_value f_set_46_elements_1(u_value v_s) {
  return f_set_46_elements_onto_2(v_s, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_set_46_elements_onto_2(u_value v_s, u_value v_rest) {
  u_value s1057 = v_s;
  u_value t1058 = NULL;
  if ((!t1058) && u_is_con(s1057, &n_set_46_SLeaf)) {
    t1058 = v_rest;
  }
  if ((!t1058) && u_is_con(s1057, &n_set_46_SNode)) {
    u_value v_l = u_field(s1057, 0);
    u_value v_x = u_field(s1057, 1);
    u_value v_r = u_field(s1057, 2);
    t1058 = f_set_46_elements_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, v_x, f_set_46_elements_onto_2(v_r, v_rest)));
  }
  if (!t1058) {
    u_no_case(s1057);
  }
  return t1058;
}

u_value f_set_46_count_1(u_value v_s) {
  u_value s1059 = v_s;
  u_value t1060 = NULL;
  if ((!t1060) && u_is_con(s1059, &n_set_46_SLeaf)) {
    t1060 = u_mk_int(0);
  }
  if ((!t1060) && u_is_con(s1059, &n_set_46_SNode)) {
    u_value v_l = u_field(s1059, 0);
    u_value v_r = u_field(s1059, 2);
    t1060 = u_add(u_add(f_set_46_count_1(v_l), u_mk_int(1)), f_set_46_count_1(v_r));
  }
  if (!t1060) {
    u_no_case(s1059);
  }
  return t1060;
}

u_value f_set_46_depth_1(u_value v_s) {
  u_value s1061 = v_s;
  u_value t1062 = NULL;
  if ((!t1062) && u_is_con(s1061, &n_set_46_SLeaf)) {
    t1062 = u_mk_int(0);
  }
  if ((!t1062) && u_is_con(s1061, &n_set_46_SNode)) {
    u_value v_d = u_field(s1061, 3);
    t1062 = v_d;
  }
  if (!t1062) {
    u_no_case(s1061);
  }
  return t1062;
}

u_value f_set_46_snode_depth_2(u_value v_l, u_value v_r) {
  u_value t1063;
  if (u_truth(u_lt(f_set_46_depth_1(v_l), f_set_46_depth_1(v_r)))) {
    t1063 = f_set_46_depth_1(v_r);
  } else {
    t1063 = f_set_46_depth_1(v_l);
  }
  return u_add(u_mk_int(1), t1063);
}

u_value f_set_46_snode_3(u_value v_l, u_value v_x, u_value v_r) {
  return u_mk_con(&n_set_46_SNode, 4, v_l, v_x, v_r, f_set_46_snode_depth_2(v_l, v_r));
}

u_value f_set_46_turn_left_1(u_value v_s) {
  u_value s1064 = v_s;
  u_value t1065 = NULL;
  if ((!t1065) && (u_is_con(s1064, &n_set_46_SNode) && u_is_con(u_field(s1064, 2), &n_set_46_SNode))) {
    u_value v_a = u_field(s1064, 0);
    u_value v_x = u_field(s1064, 1);
    u_value v_b = u_field(u_field(s1064, 2), 0);
    u_value v_y = u_field(u_field(s1064, 2), 1);
    u_value v_c = u_field(u_field(s1064, 2), 2);
    t1065 = f_set_46_snode_3(f_set_46_snode_3(v_a, v_x, v_b), v_y, v_c);
  }
  if (!t1065) {
    t1065 = v_s;
  }
  if (!t1065) {
    u_no_case(s1064);
  }
  return t1065;
}

u_value f_set_46_turn_right_1(u_value v_s) {
  u_value s1066 = v_s;
  u_value t1067 = NULL;
  if ((!t1067) && (u_is_con(s1066, &n_set_46_SNode) && u_is_con(u_field(s1066, 0), &n_set_46_SNode))) {
    u_value v_a = u_field(u_field(s1066, 0), 0);
    u_value v_x = u_field(u_field(s1066, 0), 1);
    u_value v_b = u_field(u_field(s1066, 0), 2);
    u_value v_y = u_field(s1066, 1);
    u_value v_c = u_field(s1066, 2);
    t1067 = f_set_46_snode_3(v_a, v_x, f_set_46_snode_3(v_b, v_y, v_c));
  }
  if (!t1067) {
    t1067 = v_s;
  }
  if (!t1067) {
    u_no_case(s1066);
  }
  return t1067;
}

u_value f_set_46_smaller_1(u_value v_s) {
  u_value s1068 = v_s;
  u_value t1069 = NULL;
  if ((!t1069) && u_is_con(s1068, &n_set_46_SNode)) {
    u_value v_l = u_field(s1068, 0);
    t1069 = v_l;
  }
  if (!t1069) {
    t1069 = v_s;
  }
  if (!t1069) {
    u_no_case(s1068);
  }
  return t1069;
}

u_value f_set_46_greater_1(u_value v_s) {
  u_value s1070 = v_s;
  u_value t1071 = NULL;
  if ((!t1071) && u_is_con(s1070, &n_set_46_SNode)) {
    u_value v_r = u_field(s1070, 2);
    t1071 = v_r;
  }
  if (!t1071) {
    t1071 = v_s;
  }
  if (!t1071) {
    u_no_case(s1070);
  }
  return t1071;
}

u_value f_set_46_rebalance_3(u_value v_l, u_value v_x, u_value v_r) {
  u_value t1075;
  if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1))))) {
    u_value t1072;
    if (u_truth(u_lt(f_set_46_depth_1(f_set_46_smaller_1(v_l)), f_set_46_depth_1(f_set_46_greater_1(v_l))))) {
      t1072 = f_set_46_turn_left_1(v_l);
    } else {
      t1072 = v_l;
    }
    t1075 = f_set_46_turn_right_1(f_set_46_snode_3(t1072, v_x, v_r));
  } else {
    u_value t1074;
    if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1))))) {
      u_value t1073;
      if (u_truth(u_lt(f_set_46_depth_1(f_set_46_greater_1(v_r)), f_set_46_depth_1(f_set_46_smaller_1(v_r))))) {
        t1073 = f_set_46_turn_right_1(v_r);
      } else {
        t1073 = v_r;
      }
      t1074 = f_set_46_turn_left_1(f_set_46_snode_3(v_l, v_x, t1073));
    } else {
      t1074 = f_set_46_snode_3(v_l, v_x, v_r);
    }
    t1075 = t1074;
  }
  return t1075;
}

u_value f_set_46_well_formed_1(u_value v_s) {
  u_value s1076 = v_s;
  u_value t1077 = NULL;
  if ((!t1077) && u_is_con(s1076, &n_set_46_SLeaf)) {
    t1077 = u_mk_bool(1);
  }
  if ((!t1077) && u_is_con(s1076, &n_set_46_SNode)) {
    u_value v_l = u_field(s1076, 0);
    u_value v_r = u_field(s1076, 2);
    u_value v_d = u_field(s1076, 3);
    u_value t1078 = f_set_46_well_formed_1(v_l);
    if (u_truth(t1078)) {
      t1078 = f_set_46_well_formed_1(v_r);
    }
    u_value t1079 = t1078;
    if (u_truth(t1079)) {
      t1079 = u_eq(v_d, f_set_46_snode_depth_2(v_l, v_r));
    }
    u_value t1080 = t1079;
    if (u_truth(t1080)) {
      t1080 = f_prelude_46__60__61__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1)));
    }
    u_value t1081 = t1080;
    if (u_truth(t1081)) {
      t1081 = f_prelude_46__60__61__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1)));
    }
    t1077 = t1081;
  }
  if (!t1077) {
    u_no_case(s1076);
  }
  return t1077;
}

u_value f_set_46_upto_1(u_value v_n) {
  u_value t1082;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t1082 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1082 = f_list_46__43__43__2(f_set_46_upto_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t1082;
}

u_value f_parse_46_Notations_46_cmds_1(u_value v_x) {
  u_value s1083 = v_x;
  u_value t1084 = NULL;
  if ((!t1084) && u_is_con(s1083, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1083, 0);
    t1084 = v_y;
  }
  if (!t1084) {
    u_no_case(s1083);
  }
  return t1084;
}

u_value f_parse_46_Notations_46_ops_1(u_value v_x) {
  u_value s1085 = v_x;
  u_value t1086 = NULL;
  if ((!t1086) && u_is_con(s1085, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1085, 1);
    t1086 = v_y;
  }
  if (!t1086) {
    u_no_case(s1085);
  }
  return t1086;
}

u_value f_parse_46_Notations_46_cmds_58__61__2(u_value v_x, u_value v_y) {
  u_value s1087 = v_x;
  u_value t1088 = NULL;
  if ((!t1088) && u_is_con(s1087, &n_parse_46_Notations)) {
    u_value v_a1 = u_field(s1087, 1);
    t1088 = u_mk_con(&n_parse_46_Notations, 2, v_y, v_a1);
  }
  if (!t1088) {
    u_no_case(s1087);
  }
  return t1088;
}

u_value f_parse_46_Notations_46_ops_58__61__2(u_value v_x, u_value v_y) {
  u_value s1089 = v_x;
  u_value t1090 = NULL;
  if ((!t1090) && u_is_con(s1089, &n_parse_46_Notations)) {
    u_value v_a0 = u_field(s1089, 0);
    t1090 = u_mk_con(&n_parse_46_Notations, 2, v_a0, v_y);
  }
  if (!t1090) {
    u_no_case(s1089);
  }
  return t1090;
}

u_value f_parse_46_Scope_46_params_1(u_value v_x) {
  u_value s1091 = v_x;
  u_value t1092 = NULL;
  if ((!t1092) && u_is_con(s1091, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1091, 0);
    t1092 = v_y;
  }
  if (!t1092) {
    u_no_case(s1091);
  }
  return t1092;
}

u_value f_parse_46_Scope_46_notations_1(u_value v_x) {
  u_value s1093 = v_x;
  u_value t1094 = NULL;
  if ((!t1094) && u_is_con(s1093, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1093, 1);
    t1094 = v_y;
  }
  if (!t1094) {
    u_no_case(s1093);
  }
  return t1094;
}

u_value f_parse_46_Scope_46_params_58__61__2(u_value v_x, u_value v_y) {
  u_value s1095 = v_x;
  u_value t1096 = NULL;
  if ((!t1096) && u_is_con(s1095, &n_parse_46_Scope)) {
    u_value v_a1 = u_field(s1095, 1);
    t1096 = u_mk_con(&n_parse_46_Scope, 2, v_y, v_a1);
  }
  if (!t1096) {
    u_no_case(s1095);
  }
  return t1096;
}

u_value f_parse_46_Scope_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1097 = v_x;
  u_value t1098 = NULL;
  if ((!t1098) && u_is_con(s1097, &n_parse_46_Scope)) {
    u_value v_a0 = u_field(s1097, 0);
    t1098 = u_mk_con(&n_parse_46_Scope, 2, v_a0, v_y);
  }
  if (!t1098) {
    u_no_case(s1097);
  }
  return t1098;
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
  static struct u_object l___1099 = U_LIT("_");
  static struct u_object l_assert_1100 = U_LIT("assert");
  static struct u_object l_assign_1101 = U_LIT("assign");
  static struct u_object l_call_1102 = U_LIT("call");
  static struct u_object l_case_1103 = U_LIT("case");
  static struct u_object l_catch_1104 = U_LIT("catch");
  static struct u_object l_data_1105 = U_LIT("data");
  static struct u_object l_else_1106 = U_LIT("else");
  static struct u_object l_end_1107 = U_LIT("end");
  static struct u_object l_eval_1108 = U_LIT("eval");
  static struct u_object l_false_1109 = U_LIT("false");
  static struct u_object l_for_1110 = U_LIT("for");
  static struct u_object l_function_1111 = U_LIT("function");
  static struct u_object l_if_1112 = U_LIT("if");
  static struct u_object l_import_1113 = U_LIT("import");
  static struct u_object l_in_1114 = U_LIT("in");
  static struct u_object l_infix_1115 = U_LIT("infix");
  static struct u_object l_lambda_1116 = U_LIT("lambda");
  static struct u_object l_let_1117 = U_LIT("let");
  static struct u_object l_local_1118 = U_LIT("local");
  static struct u_object l_match_1119 = U_LIT("match");
  static struct u_object l_out_1120 = U_LIT("out");
  static struct u_object l_postfix_1121 = U_LIT("postfix");
  static struct u_object l_prefix_1122 = U_LIT("prefix");
  static struct u_object l_procedure_1123 = U_LIT("procedure");
  static struct u_object l_raise_1124 = U_LIT("raise");
  static struct u_object l_ref_1125 = U_LIT("ref");
  static struct u_object l_run_1126 = U_LIT("run");
  static struct u_object l_skip_1127 = U_LIT("skip");
  static struct u_object l_test_1128 = U_LIT("test");
  static struct u_object l_then_1129 = U_LIT("then");
  static struct u_object l_true_1130 = U_LIT("true");
  static struct u_object l_type_1131 = U_LIT("type");
  static struct u_object l_try_1132 = U_LIT("try");
  static struct u_object l_while_1133 = U_LIT("while");
  static struct u_object l_with_1134 = U_LIT("with");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, &l___1099, u_mk_con(&n_list_46_Cons, 2, &l_assert_1100, u_mk_con(&n_list_46_Cons, 2, &l_assign_1101, u_mk_con(&n_list_46_Cons, 2, &l_call_1102, u_mk_con(&n_list_46_Cons, 2, &l_case_1103, u_mk_con(&n_list_46_Cons, 2, &l_catch_1104, u_mk_con(&n_list_46_Cons, 2, &l_data_1105, u_mk_con(&n_list_46_Cons, 2, &l_else_1106, u_mk_con(&n_list_46_Cons, 2, &l_end_1107, u_mk_con(&n_list_46_Cons, 2, &l_eval_1108, u_mk_con(&n_list_46_Cons, 2, &l_false_1109, u_mk_con(&n_list_46_Cons, 2, &l_for_1110, u_mk_con(&n_list_46_Cons, 2, &l_function_1111, u_mk_con(&n_list_46_Cons, 2, &l_if_1112, u_mk_con(&n_list_46_Cons, 2, &l_import_1113, u_mk_con(&n_list_46_Cons, 2, &l_in_1114, u_mk_con(&n_list_46_Cons, 2, &l_infix_1115, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1116, u_mk_con(&n_list_46_Cons, 2, &l_let_1117, u_mk_con(&n_list_46_Cons, 2, &l_local_1118, u_mk_con(&n_list_46_Cons, 2, &l_match_1119, u_mk_con(&n_list_46_Cons, 2, &l_out_1120, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1121, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1122, u_mk_con(&n_list_46_Cons, 2, &l_procedure_1123, u_mk_con(&n_list_46_Cons, 2, &l_raise_1124, u_mk_con(&n_list_46_Cons, 2, &l_ref_1125, u_mk_con(&n_list_46_Cons, 2, &l_run_1126, u_mk_con(&n_list_46_Cons, 2, &l_skip_1127, u_mk_con(&n_list_46_Cons, 2, &l_test_1128, u_mk_con(&n_list_46_Cons, 2, &l_then_1129, u_mk_con(&n_list_46_Cons, 2, &l_true_1130, u_mk_con(&n_list_46_Cons, 2, &l_type_1131, u_mk_con(&n_list_46_Cons, 2, &l_try_1132, u_mk_con(&n_list_46_Cons, 2, &l_while_1133, u_mk_con(&n_list_46_Cons, 2, &l_with_1134, u_mk_con(&n_list_46_Nil, 0)))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_list_46_contains_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1135 = U_LIT("=");
  static struct u_object l__61__62__1136 = U_LIT("=>");
  static struct u_object l__58__61__1137 = U_LIT(":=");
  static struct u_object l__58__1138 = U_LIT(":");
  static struct u_object l__124__1139 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1135, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1136, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1137, u_mk_con(&n_list_46_Cons, 2, &l__58__1138, u_mk_con(&n_list_46_Cons, 2, &l__124__1139, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1140 = v_ns;
  u_value t1141 = NULL;
  if ((!t1141) && u_is_con(s1140, &n_list_46_Nil)) {
    t1141 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1141) && (u_is_con(s1140, &n_list_46_Cons) && (u_is_con(u_field(s1140, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1140, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1140, 0), 1);
    u_value v_ops = u_field(u_field(s1140, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1141 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1141) && u_is_con(s1140, &n_list_46_Cons)) {
    u_value v_more = u_field(s1140, 1);
    t1141 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1141) {
    u_no_case(s1140);
  }
  return t1141;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1142 = v_ns;
  u_value t1143 = NULL;
  if ((!t1143) && u_is_con(s1142, &n_list_46_Nil)) {
    t1143 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1143) && (u_is_con(s1142, &n_list_46_Cons) && (u_is_con(u_field(s1142, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1142, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1142, 0), 1);
    u_value v_ops = u_field(u_field(s1142, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1143 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1143) && u_is_con(s1142, &n_list_46_Cons)) {
    u_value v_more = u_field(s1142, 1);
    t1143 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1143) {
    u_no_case(s1142);
  }
  return t1143;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1144 = v_ns;
  u_value t1145 = NULL;
  if ((!t1145) && u_is_con(s1144, &n_list_46_Nil)) {
    t1145 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1145) && (u_is_con(s1144, &n_list_46_Cons) && u_is_con(u_field(s1144, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1144, 0), 0);
    u_value v_p = u_field(u_field(s1144, 0), 1);
    u_value v_ops = u_field(u_field(s1144, 0), 2);
    u_value t1146 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1146)) {
      t1146 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1147 = t1146;
    if (u_truth(t1147)) {
      t1147 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1147)) {
      t1145 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1145) && u_is_con(s1144, &n_list_46_Cons)) {
    u_value v_more = u_field(s1144, 1);
    t1145 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1145) {
    u_no_case(s1144);
  }
  return t1145;
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
  u_value s1148 = v_cs;
  u_value t1149 = NULL;
  if ((!t1149) && u_is_con(s1148, &n_list_46_Nil)) {
    t1149 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1149) && (u_is_con(s1148, &n_list_46_Cons) && u_is_con(u_field(s1148, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1148, 0), 2);
    u_value v_more = u_field(s1148, 1);
    t1149 = f_list_46__43__43__2(v_ops, f_parse_46_operators_of_1(v_more));
  }
  if ((!t1149) && u_is_con(s1148, &n_list_46_Cons)) {
    u_value v_more = u_field(s1148, 1);
    t1149 = f_parse_46_operators_of_1(v_more);
  }
  if (!t1149) {
    u_no_case(s1148);
  }
  return t1149;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1150 = v_ts;
  u_value t1151 = NULL;
  if ((!t1151) && u_is_con(s1150, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1150, 0), 0);
    t1151 = v_t;
  }
  if (!t1151) {
    t1151 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1151) {
    u_no_case(s1150);
  }
  return t1151;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1152 = v_ts;
  u_value t1153 = NULL;
  if ((!t1153) && (u_is_con(s1152, &n_list_46_Cons) && u_is_con(u_field(s1152, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1152, 1), 0), 0);
    t1153 = v_t;
  }
  if (!t1153) {
    t1153 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1153) {
    u_no_case(s1152);
  }
  return t1153;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1154 = v_ts;
  u_value t1155 = NULL;
  if ((!t1155) && u_is_con(s1154, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1154, 0), 1);
    t1155 = v_i;
  }
  if (!t1155) {
    t1155 = u_mk_int(0);
  }
  if (!t1155) {
    u_no_case(s1154);
  }
  return t1155;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1156 = v_t;
  u_value t1157 = NULL;
  if ((!t1157) && u_is_con(s1156, &n_lex_46_TName)) {
    u_value v_n = u_field(s1156, 0);
    t1157 = v_n;
  }
  if ((!t1157) && u_is_con(s1156, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1156, 0);
    t1157 = v_s;
  }
  if ((!t1157) && u_is_con(s1156, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1156, 0);
    t1157 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1157) && u_is_con(s1156, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1156, 0);
    t1157 = u_str_show(v_n);
  }
  if ((!t1157) && u_is_con(s1156, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1156, 0);
    t1157 = u_str_quote(v_s);
  }
  if ((!t1157) && u_is_con(s1156, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1156, 0);
    static struct u_object l__39__1158 = U_LIT("'");
    static struct u_object l__39__1159 = U_LIT("'");
    t1157 = u_concat(&l__39__1158, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1159));
  }
  if ((!t1157) && u_is_con(s1156, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1160 = U_LIT("end of input");
    t1157 = &l_end_32_of_32_input_1160;
  }
  if (!t1157) {
    u_no_case(s1156);
  }
  return t1157;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1161 = f_parse_46_head_token_1(v_ts);
  u_value t1162 = NULL;
  if ((!t1162) && u_is_con(s1161, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1161, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1162 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1162) && u_is_con(s1161, &n_lex_46_TName)) {
    u_value v_n = u_field(s1161, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1162 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1162) {
    t1162 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1162) {
    u_no_case(s1161);
  }
  return t1162;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1163 = *r_ts;
    int d1164 = 0;
    if ((!d1164) && u_is_con(s1163, &n_list_46_Cons)) {
      u_value v_more = u_field(s1163, 1);
      d1164 = 1;
      {
        u_value t1165 = v_more;
        *r_ts = t1165;
      }
    }
    if (!d1164) {
      d1164 = 1;
      {
      }
    }
    if (!d1164) {
      u_no_case(s1163);
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
        static struct u_object l__32_expected_32_but_32__1166 = U_LIT(" expected but ");
        static struct u_object l__32_found_1167 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1166, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1167))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1168 = f_parse_46_head_token_1(*r_ts);
    int d1169 = 0;
    if ((!d1169) && u_is_con(s1168, &n_lex_46_TName)) {
      u_value v_m = u_field(s1168, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1169 = 1;
        {
          u_value t1170 = v_m;
          *r_n = t1170;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1169) {
      d1169 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1171 = U_LIT("name expected but ");
        static struct u_object l__32_found_1172 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1171, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1172)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1169) {
      u_no_case(s1168);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1173 = f_parse_46_head_token_1(*r_ts);
    int d1174 = 0;
    if ((!d1174) && u_is_con(s1173, &n_lex_46_TName)) {
      u_value v_m = u_field(s1173, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_m))) {
        d1174 = 1;
        {
          static struct u_object l_operator_32__1175 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1176 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1175, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1176)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1174) {
      d1174 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1174) {
      u_no_case(s1173);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1177 = f_parse_46_head_token_1(*r_ts);
    int d1178 = 0;
    if ((!d1178) && u_is_con(s1177, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1177, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1178 = 1;
        {
          u_value t1179 = v_s;
          *r_n = t1179;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1178) {
      d1178 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1178) {
      u_no_case(s1177);
    }
  }
}

u_value f_parse_46_Ops_46_notations_1(u_value v_x) {
  u_value s1180 = v_x;
  u_value t1181 = NULL;
  if ((!t1181) && u_is_con(s1180, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1180, 0);
    t1181 = v_y;
  }
  if (!t1181) {
    u_no_case(s1180);
  }
  return t1181;
}

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x) {
  u_value s1182 = v_x;
  u_value t1183 = NULL;
  if ((!t1183) && u_is_con(s1182, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1182, 1);
    t1183 = v_y;
  }
  if (!t1183) {
    u_no_case(s1182);
  }
  return t1183;
}

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x) {
  u_value s1184 = v_x;
  u_value t1185 = NULL;
  if ((!t1185) && u_is_con(s1184, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1184, 2);
    t1185 = v_y;
  }
  if (!t1185) {
    u_no_case(s1184);
  }
  return t1185;
}

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x) {
  u_value s1186 = v_x;
  u_value t1187 = NULL;
  if ((!t1187) && u_is_con(s1186, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1186, 3);
    t1187 = v_y;
  }
  if (!t1187) {
    u_no_case(s1186);
  }
  return t1187;
}

u_value f_parse_46_Ops_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1188 = v_x;
  u_value t1189 = NULL;
  if ((!t1189) && u_is_con(s1188, &n_parse_46_Ops)) {
    u_value v_a1 = u_field(s1188, 1);
    u_value v_a2 = u_field(s1188, 2);
    u_value v_a3 = u_field(s1188, 3);
    t1189 = u_mk_con(&n_parse_46_Ops, 4, v_y, v_a1, v_a2, v_a3);
  }
  if (!t1189) {
    u_no_case(s1188);
  }
  return t1189;
}

u_value f_parse_46_Ops_46_prefix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1190 = v_x;
  u_value t1191 = NULL;
  if ((!t1191) && u_is_con(s1190, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1190, 0);
    u_value v_a2 = u_field(s1190, 2);
    u_value v_a3 = u_field(s1190, 3);
    t1191 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_y, v_a2, v_a3);
  }
  if (!t1191) {
    u_no_case(s1190);
  }
  return t1191;
}

u_value f_parse_46_Ops_46_postfix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1192 = v_x;
  u_value t1193 = NULL;
  if ((!t1193) && u_is_con(s1192, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1192, 0);
    u_value v_a1 = u_field(s1192, 1);
    u_value v_a3 = u_field(s1192, 3);
    t1193 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_y, v_a3);
  }
  if (!t1193) {
    u_no_case(s1192);
  }
  return t1193;
}

u_value f_parse_46_Ops_46_infix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1194 = v_x;
  u_value t1195 = NULL;
  if ((!t1195) && u_is_con(s1194, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1194, 0);
    u_value v_a1 = u_field(s1194, 1);
    u_value v_a2 = u_field(s1194, 2);
    t1195 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_a2, v_y);
  }
  if (!t1195) {
    u_no_case(s1194);
  }
  return t1195;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1196 = v_rest;
    int d1197 = 0;
    if ((!d1197) && u_is_con(s1196, &n_list_46_Nil)) {
      d1197 = 1;
      {
      }
    }
    if ((!d1197) && (u_is_con(s1196, &n_list_46_Cons) && u_is_con(u_field(s1196, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1196, 0), 0);
      u_value v_i = u_field(u_field(s1196, 0), 1);
      d1197 = 1;
      {
        static struct u_object l_unexpected_32_opera_1198 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1198, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1197) {
      d1197 = 1;
      {
        static struct u_object l_missing_32_operator_1199 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1199, v_at), "parse.ParseError");
      }
    }
    if (!d1197) {
      u_no_case(s1196);
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
    u_value s1200 = v_items;
    int d1201 = 0;
    if ((!d1201) && (u_is_con(s1200, &n_list_46_Cons) && u_is_con(u_field(s1200, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1200, 0), 0);
      u_value v_more = u_field(s1200, 1);
      d1201 = 1;
      {
        u_value t1202 = v_x;
        u_value t1203 = v_more;
        *r_a = t1202;
        *r_rest = t1203;
      }
    }
    if ((!d1201) && (u_is_con(s1200, &n_list_46_Cons) && u_is_con(u_field(s1200, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1200, 0), 0);
      u_value v_i = u_field(u_field(s1200, 0), 1);
      u_value v_more = u_field(s1200, 1);
      d1201 = 1;
      {
        u_value s1204 = f_parse_46_prefix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op);
        int d1205 = 0;
        if ((!d1205) && u_is_con(s1204, &n_option_46_Some)) {
          u_value v_p = u_field(s1204, 0);
          d1205 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1206 = u_apply(f_parse_46_Ops_46_prefix_op_1(v_o), 2, v_op, v_b);
            *r_a = t1206;
          }
        }
        if ((!d1205) && u_is_con(s1204, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op))) {
            d1205 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1207 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1207), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1205) && u_is_con(s1204, &n_option_46_None)) {
          d1205 = 1;
          {
            static struct u_object l_undeclared_32_opera_1208 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1208, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1205) {
          u_no_case(s1204);
        }
      }
    }
    if ((!d1201) && u_is_con(s1200, &n_list_46_Nil)) {
      d1201 = 1;
      {
        static struct u_object l_missing_32_operand_1209 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1209, v_at), "parse.ParseError");
      }
    }
    if (!d1201) {
      u_no_case(s1200);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1210 = v_items;
    int d1211 = 0;
    if ((!d1211) && (u_is_con(s1210, &n_list_46_Cons) && u_is_con(u_field(s1210, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1210, 0), 0);
      u_value v_i = u_field(u_field(s1210, 0), 1);
      u_value v_tail = u_field(s1210, 1);
      d1211 = 1;
      {
        u_value s1212 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op), f_parse_46_infix_of_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op));
        int d1213 = 0;
        if ((!d1213) && u_is_con(u_field(s1212, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1212, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1213 = 1;
            {
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_postfix_op_1(v_o), 2, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1213) && u_is_con(u_field(s1212, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1212, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1212, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1213 = 1;
            {
              u_value t1214;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1214 = v_p;
              } else {
                t1214 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1214, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)));
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_infix_op_1(v_o), 3, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1213) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op)))) {
            d1213 = 1;
            {
              static struct u_object l_undeclared_32_opera_1215 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1215, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1213) {
          d1213 = 1;
          {
            u_value t1216 = v_lhs;
            u_value t1217 = v_items;
            *r_a = t1216;
            *r_rest = t1217;
          }
        }
        if (!d1213) {
          u_no_case(s1212);
        }
      }
    }
    if (!d1211) {
      d1211 = 1;
      {
        u_value t1218 = v_lhs;
        u_value t1219 = v_items;
        *r_a = t1218;
        *r_rest = t1219;
      }
    }
    if (!d1211) {
      u_no_case(s1210);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1220 = v_rest;
    int d1221 = 0;
    if ((!d1221) && (u_is_con(s1220, &n_list_46_Cons) && u_is_con(u_field(s1220, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1220, 0), 0);
      u_value v_i = u_field(u_field(s1220, 0), 1);
      u_value t1224 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1224)) {
        t1224 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1224)) {
        d1221 = 1;
        {
          static struct u_object l_operator_32__1222 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1223 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1222, u_concat(v_op, &l__32_is_32_not_32_associat_1223)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1221) {
      d1221 = 1;
      {
      }
    }
    if (!d1221) {
      u_no_case(s1220);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1225 = v_fp;
  u_value t1226 = NULL;
  if ((!t1226) && u_is_con(s1225, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1225, 0), 1);
    t1226 = u_eq(v_q, v_p);
  }
  if (!t1226) {
    t1226 = u_mk_bool(0);
  }
  if (!t1226) {
    u_no_case(s1225);
  }
  return t1226;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1227 = v_ts;
  u_value t1228 = NULL;
  if ((!t1228) && (u_is_con(s1227, &n_list_46_Cons) && (u_is_con(u_field(s1227, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1227, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1227, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1227, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1227, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1227, 0), 1);
    t1228 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1228) && (u_is_con(s1227, &n_list_46_Cons) && u_is_con(u_field(s1227, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1227, 0);
    t1228 = v_t;
  }
  if ((!t1228) && u_is_con(s1227, &n_list_46_Nil)) {
    t1228 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1228) {
    static struct u_object l__40_invalid_41__1229 = U_LIT("(invalid)");
    t1228 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1229), v_ts);
  }
  if (!t1228) {
    u_no_case(s1227);
  }
  return t1228;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1230 = v_ts;
  u_value t1231 = NULL;
  if ((!t1231) && (u_is_con(s1230, &n_list_46_Cons) && (u_is_con(u_field(s1230, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1230, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1230, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1230, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1230, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1230, 0), 1);
    t1231 = v_cs;
  }
  if (!t1231) {
    t1231 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1231) {
    u_no_case(s1230);
  }
  return t1231;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1232 = U_LIT("->");
  u_value t1239;
  if (u_truth(u_eq(v_op, &l__45__62__1232))) {
    u_value t1233 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1234 = &t1233;
    u_value l1235 = v_l;
    while (u_is_con(l1235, &n_list_46_Cons)) {
      u_value v_t = u_field(l1235, 0);
      if (u_truth(u_mk_bool(1))) {
        e1234 = u_append(e1234, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1235 = u_field(l1235, 1);
    }
    t1239 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1233, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1236 = U_LIT("*");
    u_value t1238;
    if (u_truth(u_eq(v_op, &l__42__1236))) {
      static struct u_object l__40_product_41__1237 = U_LIT("(product)");
      t1238 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1237), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1238 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1239 = t1238;
  }
  return t1239;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1240 = v_t;
  u_value t1241 = NULL;
  if ((!t1241) && (u_is_con(s1240, &n_syntax_46_TSort) && (u_is_con(u_field(s1240, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1240, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1240, 0), 1), u_mk_lit("(invalid)")))))) {
    t1241 = u_mk_bool(1);
  }
  if ((!t1241) && u_is_con(s1240, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1240, 1);
    t1241 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1241) && u_is_con(s1240, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1240, 0);
    u_value v_r = u_field(s1240, 1);
    u_value t1242 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1242)) {
      t1242 = f_parse_46_has_invalid_1(v_r);
    }
    t1241 = t1242;
  }
  if ((!t1241) && u_is_con(s1240, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1240, 0);
    t1241 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1241) {
    t1241 = u_mk_bool(0);
  }
  if (!t1241) {
    u_no_case(s1240);
  }
  return t1241;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1243 = v_ts;
  u_value t1244 = NULL;
  if ((!t1244) && u_is_con(s1243, &n_list_46_Nil)) {
    t1244 = u_mk_bool(0);
  }
  if ((!t1244) && u_is_con(s1243, &n_list_46_Cons)) {
    u_value v_t = u_field(s1243, 0);
    u_value v_more = u_field(s1243, 1);
    u_value t1245 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1245)) {
      t1245 = f_parse_46_any_invalid_1(v_more);
    }
    t1244 = t1245;
  }
  if (!t1244) {
    u_no_case(s1243);
  }
  return t1244;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1246 = v_t;
  u_value t1247 = NULL;
  if ((!t1247) && u_is_con(s1246, &n_lex_46_TName)) {
    u_value v_n = u_field(s1246, 0);
    u_value t1248 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1248)) {
      t1248 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1247 = t1248;
  }
  if ((!t1247) && (u_is_con(s1246, &n_lex_46_TPunct) && u_equals(u_field(s1246, 0), u_mk_char(40)))) {
    t1247 = u_mk_bool(1);
  }
  if (!t1247) {
    t1247 = u_mk_bool(0);
  }
  if (!t1247) {
    u_no_case(s1246);
  }
  return t1247;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1249 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1249;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1250 = f_parse_46_type_one_1(v_r);
    *r_t = t1250;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1251 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1251, v_start), "parse.ParseError");
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
    u_value t1252 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1252;
    u_value t1253 = u_mk_bool(1);
    v_next = t1253;
    u_value t1254 = u_mk_bool(1);
    v_go = t1254;
    u_value t1263 = v_go;
    while (u_truth(t1263)) {
      {
        u_value s1255 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1256 = 0;
        if ((!d1256) && u_is_con(s1255, &n_option_46_Some)) {
          u_value v_op = u_field(s1255, 0);
          d1256 = 1;
          {
            u_value t1257 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1258 = u_mk_bool(1);
            v_rev = t1257;
            v_next = t1258;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1256) && u_is_con(s1255, &n_option_46_None)) {
          u_value t1261 = v_next;
          if (u_truth(t1261)) {
            t1261 = f_parse_46_starts_type_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1261)) {
            d1256 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1259 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1260 = u_mk_bool(0);
              v_rev = t1259;
              v_next = t1260;
            }
          }
        }
        if (!d1256) {
          d1256 = 1;
          {
            u_value t1262 = u_mk_bool(0);
            v_go = t1262;
          }
        }
        if (!d1256) {
          u_no_case(s1255);
        }
      }
      t1263 = v_go;
    }
    u_value s1264 = v_rev;
    int d1265 = 0;
    if ((!d1265) && u_is_con(s1264, &n_list_46_Nil)) {
      d1265 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1266 = U_LIT("type expected but ");
        static struct u_object l__32_found_1267 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1266, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1267)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1265) {
      d1265 = 1;
      {
        u_value t1268 = f_list_46_reverse_1(v_rev);
        *r_items = t1268;
      }
    }
    if (!d1265) {
      u_no_case(s1264);
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
            u_value t1269 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
            *r_r = t1269;
          }
        } else {
          if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Scope_46_params_1(v_sc)))) {
            {
              u_value t1270 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1270;
            }
          } else {
            {
              u_value t1271 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1271;
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
    u_value t1272 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1272;
    u_value t1273 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1273)) {
      t1273 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1273)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1274 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1274;
        u_value t1276 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1276)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1275 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1275;
          }
          t1276 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1277 = f_list_46_reverse_1(v_rev);
    *r_r = t1277;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1278 = v_es;
  u_value t1279 = NULL;
  if ((!t1279) && u_is_con(s1278, &n_list_46_Nil)) {
    t1279 = v_rest;
  }
  if ((!t1279) && u_is_con(s1278, &n_list_46_Cons)) {
    u_value v_e = u_field(s1278, 0);
    u_value v_more = u_field(s1278, 1);
    static struct u_object l_Cons_1280 = U_LIT("Cons");
    t1279 = f_parse_46_expr_op2_3(&l_Cons_1280, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1279) {
    u_no_case(s1278);
  }
  return t1279;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1281 = v_t;
  u_value t1282 = NULL;
  if ((!t1282) && u_is_con(s1281, &n_lex_46_TName)) {
    u_value v_n = u_field(s1281, 0);
    static struct u_object l_let_1283 = U_LIT("let");
    static struct u_object l_if_1284 = U_LIT("if");
    static struct u_object l_lambda_1285 = U_LIT("lambda");
    static struct u_object l_match_1286 = U_LIT("match");
    static struct u_object l_true_1287 = U_LIT("true");
    static struct u_object l_false_1288 = U_LIT("false");
    u_value t1290 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1283, u_mk_con(&n_list_46_Cons, 2, &l_if_1284, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1285, u_mk_con(&n_list_46_Cons, 2, &l_match_1286, u_mk_con(&n_list_46_Cons, 2, &l_true_1287, u_mk_con(&n_list_46_Cons, 2, &l_false_1288, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1290)) {
      u_value t1289 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1289)) {
        t1289 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1290 = t1289;
    }
    t1282 = t1290;
  }
  if ((!t1282) && u_is_con(s1281, &n_lex_46_TNum)) {
    t1282 = u_mk_bool(1);
  }
  if ((!t1282) && u_is_con(s1281, &n_lex_46_TStr)) {
    t1282 = u_mk_bool(1);
  }
  if ((!t1282) && u_is_con(s1281, &n_lex_46_TChar)) {
    t1282 = u_mk_bool(1);
  }
  if ((!t1282) && u_is_con(s1281, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1281, 0);
    u_value t1291 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1291)) {
      t1291 = u_eq(v_c, u_mk_char(91));
    }
    t1282 = t1291;
  }
  if (!t1282) {
    t1282 = u_mk_bool(0);
  }
  if (!t1282) {
    u_no_case(s1281);
  }
  return t1282;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    static struct u_object l_raise_1292 = U_LIT("raise");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_raise_1292))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, r_e, r_ts);
        u_value t1293 = u_mk_con(&n_syntax_46_ERaise, 2, *r_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_e = t1293;
      }
    } else {
      {
        p_parse_46_operators(v_sc, r_e, r_ts);
        static struct u_object l_with_1294 = U_LIT("with");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_with_1294))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_update(v_sc, r_e, r_ts);
            u_value t1295 = f_parse_46_at_update_1(*r_ts);
            while (u_truth(t1295)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_update(v_sc, r_e, r_ts);
              }
              t1295 = f_parse_46_at_update_1(*r_ts);
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
    static struct u_object l__58__61__1296 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1296), r_ts);
    p_parse_46_operators(v_sc, &v_a, r_ts);
    static struct u_object l__46__1297 = U_LIT(".");
    static struct u_object l__58__61__1298 = U_LIT(":=");
    u_value t1299 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1297, u_concat(v_f, &l__58__61__1298))), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1299;
  }
}

u_value f_parse_46_at_update_1(u_value v_ts) {
  u_value s1300 = v_ts;
  u_value t1301 = NULL;
  if ((!t1301) && (u_is_con(s1300, &n_list_46_Cons) && (u_is_con(u_field(u_field(s1300, 0), 0), &n_lex_46_TPunct) && (u_equals(u_field(u_field(u_field(s1300, 0), 0), 0), u_mk_char(44)) && (u_is_con(u_field(s1300, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s1300, 1), 0), 0), &n_lex_46_TName) && (u_is_con(u_field(u_field(s1300, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(u_field(s1300, 1), 1), 0), 0), &n_lex_46_TSym) && u_equals(u_field(u_field(u_field(u_field(u_field(s1300, 1), 1), 0), 0), 0), u_mk_lit(":=")))))))))) {
    t1301 = u_mk_bool(1);
  }
  if (!t1301) {
    t1301 = u_mk_bool(0);
  }
  if (!t1301) {
    u_no_case(s1300);
  }
  return t1301;
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
    u_value t1302 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1302;
    u_value t1303 = u_mk_bool(1);
    v_next = t1303;
    u_value t1304 = u_mk_bool(1);
    v_go = t1304;
    u_value t1313 = v_go;
    while (u_truth(t1313)) {
      {
        u_value s1305 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1306 = 0;
        if ((!d1306) && u_is_con(s1305, &n_option_46_Some)) {
          u_value v_op = u_field(s1305, 0);
          d1306 = 1;
          {
            u_value t1307 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1308 = u_mk_bool(1);
            v_rev = t1307;
            v_next = t1308;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1306) && u_is_con(s1305, &n_option_46_None)) {
          u_value t1311 = v_next;
          if (u_truth(t1311)) {
            t1311 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1311)) {
            d1306 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1309 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1310 = u_mk_bool(0);
              v_rev = t1309;
              v_next = t1310;
            }
          }
        }
        if (!d1306) {
          d1306 = 1;
          {
            u_value t1312 = u_mk_bool(0);
            v_go = t1312;
          }
        }
        if (!d1306) {
          u_no_case(s1305);
        }
      }
      t1313 = v_go;
    }
    u_value s1314 = v_rev;
    int d1315 = 0;
    if ((!d1315) && u_is_con(s1314, &n_list_46_Nil)) {
      d1315 = 1;
      {
        static struct u_object l_expression_32_expec_1316 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1317 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1316, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1317)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1315) {
      d1315 = 1;
      {
        u_value t1318 = f_list_46_reverse_1(v_rev);
        *r_items = t1318;
      }
    }
    if (!d1315) {
      u_no_case(s1314);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1319 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1319;
    u_value t1321 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1321)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1320 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1320;
      }
      t1321 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1322 = f_list_46_reverse_1(v_rev);
    *r_es = t1322;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1323 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1323;
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
    u_value t1324 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1324)) {
      t1324 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1328 = t1324;
    while (u_truth(t1328)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1325 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1325;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1326 = U_LIT(".");
            u_value t1327 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1326, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1327;
          }
        }
      }
      u_value t1329 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1329)) {
        t1329 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1328 = t1329;
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
    u_value s1330 = f_parse_46_head_token_1(*r_ts);
    int d1331 = 0;
    if ((!d1331) && (u_is_con(s1330, &n_lex_46_TName) && u_equals(u_field(s1330, 0), u_mk_lit("let")))) {
      d1331 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1331) && (u_is_con(s1330, &n_lex_46_TName) && u_equals(u_field(s1330, 0), u_mk_lit("if")))) {
      d1331 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1332 = U_LIT("then");
        p_parse_46_keyword(&l_then_1332, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1333 = U_LIT("else");
        p_parse_46_keyword(&l_else_1333, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1334 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1334;
      }
    }
    if ((!d1331) && (u_is_con(s1330, &n_lex_46_TName) && u_equals(u_field(s1330, 0), u_mk_lit("lambda")))) {
      d1331 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1331) && (u_is_con(s1330, &n_lex_46_TName) && u_equals(u_field(s1330, 0), u_mk_lit("match")))) {
      d1331 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1335 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1335;
      }
    }
    if ((!d1331) && (u_is_con(s1330, &n_lex_46_TName) && u_equals(u_field(s1330, 0), u_mk_lit("true")))) {
      d1331 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1336 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1336;
      }
    }
    if ((!d1331) && (u_is_con(s1330, &n_lex_46_TName) && u_equals(u_field(s1330, 0), u_mk_lit("false")))) {
      d1331 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1337 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1337;
      }
    }
    if ((!d1331) && u_is_con(s1330, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1330, 0);
      d1331 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1338 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1338;
      }
    }
    if ((!d1331) && u_is_con(s1330, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1330, 0);
      d1331 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1339 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1339;
      }
    }
    if ((!d1331) && u_is_con(s1330, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1330, 0);
      d1331 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1340 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1340;
      }
    }
    if ((!d1331) && (u_is_con(s1330, &n_lex_46_TPunct) && u_equals(u_field(s1330, 0), u_mk_char(40)))) {
      d1331 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1341 = v_es;
        int d1342 = 0;
        if ((!d1342) && (u_is_con(s1341, &n_list_46_Cons) && u_is_con(u_field(s1341, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1341, 0);
          d1342 = 1;
          {
            u_value t1343 = v_x;
            *r_e = t1343;
          }
        }
        if (!d1342) {
          d1342 = 1;
          {
            u_value t1344 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1344;
          }
        }
        if (!d1342) {
          u_no_case(s1341);
        }
      }
    }
    if ((!d1331) && (u_is_con(s1330, &n_lex_46_TPunct) && u_equals(u_field(s1330, 0), u_mk_char(91)))) {
      d1331 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1331) && u_is_con(s1330, &n_lex_46_TName)) {
      u_value v_n = u_field(s1330, 0);
      u_value t1346 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1346)) {
        t1346 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t1346)) {
        d1331 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1345 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1345;
        }
      }
    }
    if (!d1331) {
      d1331 = 1;
      {
        static struct u_object l_expression_32_expec_1347 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1348 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1347, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1348)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1331) {
      u_no_case(s1330);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1349 = v_ps;
  u_value t1350 = NULL;
  if ((!t1350) && u_is_con(s1349, &n_list_46_Nil)) {
    t1350 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1350) && (u_is_con(s1349, &n_list_46_Cons) && u_is_con(u_field(s1349, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1349, 0), 0);
    u_value v_more = u_field(s1349, 1);
    u_value s1351 = f_parse_46_pattern_vars_1(v_more);
    u_value t1352 = NULL;
    if ((!t1352) && u_is_con(s1351, &n_option_46_Some)) {
      u_value v_xs = u_field(s1351, 0);
      t1352 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1352) && u_is_con(s1351, &n_option_46_None)) {
      t1352 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1352) {
      u_no_case(s1351);
    }
    t1350 = t1352;
  }
  if (!t1350) {
    t1350 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1350) {
    u_no_case(s1349);
  }
  return t1350;
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
    static struct u_object l__58__61__1353 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1353), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1354 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1355 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1354;
    v_es = t1355;
    u_value t1359 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1359)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1356 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1356), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1357 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1358 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1357;
        v_es = t1358;
      }
      t1359 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1360 = U_LIT("in");
    p_parse_46_keyword(&l_in_1360, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1361 = f_list_46_reverse_1(v_ps);
    u_value t1362 = f_list_46_reverse_1(v_es);
    v_ps = t1361;
    v_es = t1362;
    u_value s1363 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1364 = 0;
    if ((!d1364) && u_is_con(u_field(s1363, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1363, 0), 0);
      d1364 = 1;
      {
        u_value t1365 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1365;
      }
    }
    if ((!d1364) && (u_is_con(u_field(s1363, 0), &n_option_46_None) && (u_is_con(u_field(s1363, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1363, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1363, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1363, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1363, 1), 0);
      u_value v_c = u_field(u_field(s1363, 2), 0);
      d1364 = 1;
      {
        u_value t1366 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1366;
      }
    }
    if (!d1364) {
      d1364 = 1;
      {
        u_value t1367 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1367;
      }
    }
    if (!d1364) {
      u_no_case(s1363);
    }
  }
}

void p_parse_46_parse_lambda(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    p_parse_46_advance(r_ts);
    u_value t1368 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1368;
    static struct u_object l__61__62__1369 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1369)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1370 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1370;
        u_value t1372 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1372)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1371 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1371;
          }
          t1372 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1373 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1373), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1374 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1374;
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
        static struct u_object l_Nil_1375 = U_LIT("Nil");
        u_value t1376 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1375), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1376;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1377 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1377))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1378 = U_LIT("in");
            p_parse_46_keyword(&l_in_1378, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1379 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1380 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1379;
            v_lists = t1380;
            u_value t1384 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1384)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1381 = U_LIT("in");
                p_parse_46_keyword(&l_in_1381, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1382 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1383 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1382;
                v_lists = t1383;
              }
              t1384 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1385 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1385;
            static struct u_object l_if_1386 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1386))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1387 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1387;
          }
        } else {
          {
            u_value t1388 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1388;
            u_value t1390 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1390)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1389 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1389;
              }
              t1390 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1391 = U_LIT("Nil");
            u_value t1392 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1391), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1392;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1393 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1393;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1394 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1394))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1395 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1395;
      }
    } else {
      {
        u_value t1396 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1396;
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
    u_value t1397 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1397;
    static struct u_object l_case_1398 = U_LIT("case");
    u_value t1401 = f_parse_46_at_keyword_2(*r_ts, &l_case_1398);
    while (u_truth(t1401)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1399 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1399), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1400 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1400;
      }
      static struct u_object l_case_1402 = U_LIT("case");
      t1401 = f_parse_46_at_keyword_2(*r_ts, &l_case_1402);
    }
    static struct u_object l_else_1403 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1403))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1404 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1404;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1405 = U_LIT("end");
    p_parse_46_keyword(&l_end_1405, r_ts);
    u_value t1406 = f_list_46_reverse_1(v_rev);
    *r_cs = t1406;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1407 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1407))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1408 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1408, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
  u_value s1409 = v_ps;
  u_value t1410 = NULL;
  if ((!t1410) && u_is_con(s1409, &n_list_46_Nil)) {
    t1410 = v_rest;
  }
  if ((!t1410) && u_is_con(s1409, &n_list_46_Cons)) {
    u_value v_p = u_field(s1409, 0);
    u_value v_more = u_field(s1409, 1);
    static struct u_object l_Cons_1411 = U_LIT("Cons");
    t1410 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1411), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1410) {
    u_no_case(s1409);
  }
  return t1410;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1412 = v_t;
  u_value t1413 = NULL;
  if ((!t1413) && u_is_con(s1412, &n_lex_46_TName)) {
    u_value v_n = u_field(s1412, 0);
    static struct u_object l___1414 = U_LIT("_");
    static struct u_object l_true_1415 = U_LIT("true");
    static struct u_object l_false_1416 = U_LIT("false");
    u_value t1418 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1414, u_mk_con(&n_list_46_Cons, 2, &l_true_1415, u_mk_con(&n_list_46_Cons, 2, &l_false_1416, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1418)) {
      u_value t1417 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1417)) {
        t1417 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1418 = t1417;
    }
    t1413 = t1418;
  }
  if ((!t1413) && u_is_con(s1412, &n_lex_46_TNum)) {
    t1413 = u_mk_bool(1);
  }
  if ((!t1413) && u_is_con(s1412, &n_lex_46_TStr)) {
    t1413 = u_mk_bool(1);
  }
  if ((!t1413) && u_is_con(s1412, &n_lex_46_TChar)) {
    t1413 = u_mk_bool(1);
  }
  if ((!t1413) && u_is_con(s1412, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1412, 0);
    u_value t1419 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1419)) {
      t1419 = u_eq(v_c, u_mk_char(91));
    }
    t1413 = t1419;
  }
  if (!t1413) {
    t1413 = u_mk_bool(0);
  }
  if (!t1413) {
    u_no_case(s1412);
  }
  return t1413;
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
    u_value t1420 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1420;
    u_value t1421 = u_mk_bool(1);
    v_next = t1421;
    u_value t1422 = u_mk_bool(1);
    v_go = t1422;
    u_value t1431 = v_go;
    while (u_truth(t1431)) {
      {
        u_value s1423 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1424 = 0;
        if ((!d1424) && u_is_con(s1423, &n_option_46_Some)) {
          u_value v_op = u_field(s1423, 0);
          d1424 = 1;
          {
            u_value t1425 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1426 = u_mk_bool(1);
            v_rev = t1425;
            v_next = t1426;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1424) && u_is_con(s1423, &n_option_46_None)) {
          u_value t1429 = v_next;
          if (u_truth(t1429)) {
            t1429 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1429)) {
            d1424 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1427 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1428 = u_mk_bool(0);
              v_rev = t1427;
              v_next = t1428;
            }
          }
        }
        if (!d1424) {
          d1424 = 1;
          {
            u_value t1430 = u_mk_bool(0);
            v_go = t1430;
          }
        }
        if (!d1424) {
          u_no_case(s1423);
        }
      }
      t1431 = v_go;
    }
    u_value s1432 = v_rev;
    int d1433 = 0;
    if ((!d1433) && u_is_con(s1432, &n_list_46_Nil)) {
      d1433 = 1;
      {
        static struct u_object l_pattern_32_expected_1434 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1435 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1434, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1435)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1433) {
      d1433 = 1;
      {
        u_value t1436 = f_list_46_reverse_1(v_rev);
        *r_items = t1436;
      }
    }
    if (!d1433) {
      u_no_case(s1432);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1437 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1437;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1438 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1438;
        u_value t1440 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1440)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1439 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1439;
          }
          t1440 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1441 = f_list_46_reverse_1(v_rev);
    *r_ps = t1441;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1442 = f_parse_46_head_token_1(*r_ts);
    int d1443 = 0;
    if ((!d1443) && (u_is_con(s1442, &n_lex_46_TName) && u_equals(u_field(s1442, 0), u_mk_lit("_")))) {
      d1443 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1444 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1444;
      }
    }
    if ((!d1443) && (u_is_con(s1442, &n_lex_46_TName) && u_equals(u_field(s1442, 0), u_mk_lit("true")))) {
      d1443 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1445 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1445;
      }
    }
    if ((!d1443) && (u_is_con(s1442, &n_lex_46_TName) && u_equals(u_field(s1442, 0), u_mk_lit("false")))) {
      d1443 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1446 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1446;
      }
    }
    if ((!d1443) && u_is_con(s1442, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1442, 0);
      d1443 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1447 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1447;
      }
    }
    if ((!d1443) && u_is_con(s1442, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1442, 0);
      d1443 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1448 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1448;
      }
    }
    if ((!d1443) && u_is_con(s1442, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1442, 0);
      d1443 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1449 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1449;
      }
    }
    if ((!d1443) && (u_is_con(s1442, &n_lex_46_TPunct) && u_equals(u_field(s1442, 0), u_mk_char(40)))) {
      d1443 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1450 = v_ps;
        int d1451 = 0;
        if ((!d1451) && (u_is_con(s1450, &n_list_46_Cons) && u_is_con(u_field(s1450, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1450, 0);
          d1451 = 1;
          {
            u_value t1452 = v_q;
            *r_p = t1452;
          }
        }
        if (!d1451) {
          d1451 = 1;
          {
            u_value t1453 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1453;
          }
        }
        if (!d1451) {
          u_no_case(s1450);
        }
      }
    }
    if ((!d1443) && (u_is_con(s1442, &n_lex_46_TPunct) && u_equals(u_field(s1442, 0), u_mk_char(91)))) {
      d1443 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1454 = U_LIT("Nil");
            u_value t1455 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1454), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1455;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1456 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1456;
            u_value t1458 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1458)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1457 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1457;
              }
              t1458 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1459 = U_LIT("Nil");
            u_value t1460 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1459), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1460;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1461 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1461;
          }
        }
      }
    }
    if (!d1443) {
      d1443 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1462 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1462;
          }
        } else {
          {
            u_value t1463 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1463;
          }
        }
      }
    }
    if (!d1443) {
      u_no_case(s1442);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1464 = v_t;
  u_value t1465 = NULL;
  if ((!t1465) && u_is_con(s1464, &n_lex_46_TName)) {
    u_value v_n = u_field(s1464, 0);
    static struct u_object l_skip_1466 = U_LIT("skip");
    static struct u_object l_if_1467 = U_LIT("if");
    static struct u_object l_while_1468 = U_LIT("while");
    static struct u_object l_call_1469 = U_LIT("call");
    static struct u_object l_match_1470 = U_LIT("match");
    static struct u_object l_assert_1471 = U_LIT("assert");
    static struct u_object l_assign_1472 = U_LIT("assign");
    static struct u_object l_raise_1473 = U_LIT("raise");
    static struct u_object l_try_1474 = U_LIT("try");
    t1465 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1466, u_mk_con(&n_list_46_Cons, 2, &l_if_1467, u_mk_con(&n_list_46_Cons, 2, &l_while_1468, u_mk_con(&n_list_46_Cons, 2, &l_call_1469, u_mk_con(&n_list_46_Cons, 2, &l_match_1470, u_mk_con(&n_list_46_Cons, 2, &l_assert_1471, u_mk_con(&n_list_46_Cons, 2, &l_assign_1472, u_mk_con(&n_list_46_Cons, 2, &l_raise_1473, u_mk_con(&n_list_46_Cons, 2, &l_try_1474, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1465) {
    t1465 = u_mk_bool(0);
  }
  if (!t1465) {
    u_no_case(s1464);
  }
  return t1465;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1475 = v_cs;
  u_value t1476 = NULL;
  if ((!t1476) && u_is_con(s1475, &n_list_46_Nil)) {
    t1476 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1476) && (u_is_con(s1475, &n_list_46_Cons) && u_is_con(u_field(s1475, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1475, 0), 0);
    u_value v_g = u_field(u_field(s1475, 0), 1);
    u_value v_b = u_field(u_field(s1475, 0), 2);
    u_value v_more = u_field(s1475, 1);
    t1476 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1476) {
    u_no_case(s1475);
  }
  return t1476;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1477 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1477;
    u_value t1478 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1478;
    u_value t1479 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1479;
    static struct u_object l_local_1480 = U_LIT("local");
    u_value t1482 = f_parse_46_at_keyword_2(*r_ts, &l_local_1480);
    while (u_truth(t1482)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1481 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1481)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1481 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1483 = U_LIT("local");
      t1482 = f_parse_46_at_keyword_2(*r_ts, &l_local_1483);
    }
    u_value t1485 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1485)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1484 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1484;
      }
      t1485 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1486 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1486;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1487 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1487;
    static struct u_object l__58__1488 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1488)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__1489 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1489)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1490 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1490;
      }
    }
    u_value t1491 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1491;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1492 = f_parse_46_head_token_1(*r_ts);
    int d1493 = 0;
    if ((!d1493) && (u_is_con(s1492, &n_lex_46_TName) && u_equals(u_field(s1492, 0), u_mk_lit("skip")))) {
      d1493 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1494 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1494;
      }
    }
    if ((!d1493) && (u_is_con(s1492, &n_lex_46_TName) && u_equals(u_field(s1492, 0), u_mk_lit("if")))) {
      d1493 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1493) && (u_is_con(s1492, &n_lex_46_TName) && u_equals(u_field(s1492, 0), u_mk_lit("while")))) {
      d1493 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1495 = U_LIT("end");
        p_parse_46_keyword(&l_end_1495, r_ts);
        u_value t1496 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1496;
      }
    }
    if ((!d1493) && (u_is_con(s1492, &n_lex_46_TName) && u_equals(u_field(s1492, 0), u_mk_lit("call")))) {
      d1493 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1493) && (u_is_con(s1492, &n_lex_46_TName) && u_equals(u_field(s1492, 0), u_mk_lit("match")))) {
      d1493 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1497 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1497;
      }
    }
    if ((!d1493) && (u_is_con(s1492, &n_lex_46_TName) && u_equals(u_field(s1492, 0), u_mk_lit("assert")))) {
      d1493 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1498 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1498;
      }
    }
    if ((!d1493) && (u_is_con(s1492, &n_lex_46_TName) && u_equals(u_field(s1492, 0), u_mk_lit("assign")))) {
      d1493 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1493) && (u_is_con(s1492, &n_lex_46_TName) && u_equals(u_field(s1492, 0), u_mk_lit("raise")))) {
      d1493 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1499 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1499;
      }
    }
    if ((!d1493) && (u_is_con(s1492, &n_lex_46_TName) && u_equals(u_field(s1492, 0), u_mk_lit("try")))) {
      d1493 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1500 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1500, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1501 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1501;
      }
    }
    if (!d1493) {
      d1493 = 1;
      {
        static struct u_object l_statement_32_expect_1502 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1503 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1502, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1503)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1493) {
      u_no_case(s1492);
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
    static struct u_object l_else_1504 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1504)))) {
      {
        static struct u_object l_end_1505 = U_LIT("end");
        p_parse_46_keyword(&l_end_1505, r_ts);
        u_value t1506 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1506;
      }
    } else {
      static struct u_object l_if_1507 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1507)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1508 = U_LIT("end");
          p_parse_46_keyword(&l_end_1508, r_ts);
        }
      }
    }
    u_value t1509 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1509;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1510 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1510), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1511 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1511;
    u_value t1514 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1514)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1512 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1512), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1513 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1513;
      }
      t1514 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1515 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1515;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1516 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1516;
    static struct u_object l_case_1517 = U_LIT("case");
    u_value t1520 = f_parse_46_at_keyword_2(*r_ts, &l_case_1517);
    while (u_truth(t1520)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1518 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1518), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1519 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1519;
      }
      static struct u_object l_case_1521 = U_LIT("case");
      t1520 = f_parse_46_at_keyword_2(*r_ts, &l_case_1521);
    }
    static struct u_object l_else_1522 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1522))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1523 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1523;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1524 = U_LIT("end");
    p_parse_46_keyword(&l_end_1524, r_ts);
    u_value t1525 = f_list_46_reverse_1(v_rev);
    *r_cs = t1525;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1527;
  if (u_truth(v_first)) {
    t1527 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1526 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1526)) {
      t1526 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1527 = t1526;
  }
  return t1527;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1529 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1529)) {
    static struct u_object l_out_1528 = U_LIT("out");
    t1529 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1528));
  }
  u_value t1531 = t1529;
  if (u_truth(t1531)) {
    static struct u_object l_ref_1530 = U_LIT("ref");
    t1531 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1530));
  }
  return t1531;
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
    u_value t1532 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1533 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1534 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1535 = u_mk_bool(1);
    v_ins = t1532;
    v_outs = t1533;
    v_refs = t1534;
    v_first = t1535;
    static struct u_object l_in_1536 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1536))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1537 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
        u_value t1538 = u_mk_bool(0);
        v_ins = t1537;
        v_first = t1538;
        u_value t1540 = f_parse_46_item_follows_1(*r_ts);
        while (u_truth(t1540)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_expr(v_sc, &v_e, r_ts);
            u_value t1539 = u_mk_con(&n_list_46_Cons, 2, v_e, v_ins);
            v_ins = t1539;
          }
          t1540 = f_parse_46_item_follows_1(*r_ts);
        }
      }
    }
    static struct u_object l_out_1541 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1541, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_outs, r_ts);
        u_value t1542 = u_mk_bool(0);
        v_first = t1542;
      }
    }
    static struct u_object l_ref_1543 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1543, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    u_value t1544 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), f_list_46_reverse_1(v_ins), v_outs, v_refs);
    *r_s = t1544;
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
    u_value t1545 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1545;
    u_value t1547 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1547)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1546 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_rev);
        v_rev = t1546;
      }
      t1547 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1548 = f_list_46_reverse_1(v_rev);
    *r_xs = t1548;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1549 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1549;
    static struct u_object l__58__1550 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1550)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1551 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1551;
  }
}

void p_parse_46_parse_formals1(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_formal(v_sc, &v_x, r_ts);
    u_value t1552 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1552;
    u_value t1554 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1554)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formal(v_sc, &v_x, r_ts);
        u_value t1553 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1553;
      }
      t1554 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1555 = f_list_46_reverse_1(v_rev);
    *r_xs = t1555;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1556 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1556;
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
  u_value s1557 = v_cs;
  u_value t1558 = NULL;
  if ((!t1558) && u_is_con(s1557, &n_list_46_Nil)) {
    t1558 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1558) && (u_is_con(s1557, &n_list_46_Cons) && u_is_con(u_field(s1557, 0), &n_syntax_46_CEval))) {
    u_value v_more = u_field(s1557, 1);
    t1558 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1558) && (u_is_con(s1557, &n_list_46_Cons) && u_is_con(u_field(s1557, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1557, 1);
    t1558 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1558) && (u_is_con(s1557, &n_list_46_Cons) && u_is_con(u_field(s1557, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1557, 1);
    t1558 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1558) && u_is_con(s1557, &n_list_46_Cons)) {
    u_value v_c = u_field(s1557, 0);
    u_value v_more = u_field(s1557, 1);
    t1558 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1558) {
    u_no_case(s1557);
  }
  return t1558;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1559 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1560 = &t1559;
  u_value l1561 = v_cs;
  while (u_is_con(l1561, &n_list_46_Cons)) {
    u_value v_c = u_field(l1561, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1560 = u_append(e1560, v_c);
    }
    l1561 = u_field(l1561, 1);
  }
  return t1559;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1562 = v_c;
  u_value t1563 = NULL;
  if ((!t1563) && u_is_con(s1562, &n_syntax_46_CNotation)) {
    t1563 = u_mk_bool(1);
  }
  if (!t1563) {
    t1563 = u_mk_bool(0);
  }
  if (!t1563) {
    u_no_case(s1562);
  }
  return t1563;
}

u_value f_parse_46_Context_46_notations_1(u_value v_x) {
  u_value s1564 = v_x;
  u_value t1565 = NULL;
  if ((!t1565) && u_is_con(s1564, &n_parse_46_Context)) {
    u_value v_y = u_field(s1564, 0);
    t1565 = v_y;
  }
  if (!t1565) {
    u_no_case(s1564);
  }
  return t1565;
}

u_value f_parse_46_Context_46_modules_1(u_value v_x) {
  u_value s1566 = v_x;
  u_value t1567 = NULL;
  if ((!t1567) && u_is_con(s1566, &n_parse_46_Context)) {
    u_value v_y = u_field(s1566, 1);
    t1567 = v_y;
  }
  if (!t1567) {
    u_no_case(s1566);
  }
  return t1567;
}

u_value f_parse_46_Context_46_loading_1(u_value v_x) {
  u_value s1568 = v_x;
  u_value t1569 = NULL;
  if ((!t1569) && u_is_con(s1568, &n_parse_46_Context)) {
    u_value v_y = u_field(s1568, 2);
    t1569 = v_y;
  }
  if (!t1569) {
    u_no_case(s1568);
  }
  return t1569;
}

u_value f_parse_46_Context_46_base_1(u_value v_x) {
  u_value s1570 = v_x;
  u_value t1571 = NULL;
  if ((!t1571) && u_is_con(s1570, &n_parse_46_Context)) {
    u_value v_y = u_field(s1570, 3);
    t1571 = v_y;
  }
  if (!t1571) {
    u_no_case(s1570);
  }
  return t1571;
}

u_value f_parse_46_Context_46_dir_1(u_value v_x) {
  u_value s1572 = v_x;
  u_value t1573 = NULL;
  if ((!t1573) && u_is_con(s1572, &n_parse_46_Context)) {
    u_value v_y = u_field(s1572, 4);
    t1573 = v_y;
  }
  if (!t1573) {
    u_no_case(s1572);
  }
  return t1573;
}

u_value f_parse_46_Context_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1574 = v_x;
  u_value t1575 = NULL;
  if ((!t1575) && u_is_con(s1574, &n_parse_46_Context)) {
    u_value v_a1 = u_field(s1574, 1);
    u_value v_a2 = u_field(s1574, 2);
    u_value v_a3 = u_field(s1574, 3);
    u_value v_a4 = u_field(s1574, 4);
    t1575 = u_mk_con(&n_parse_46_Context, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1575) {
    u_no_case(s1574);
  }
  return t1575;
}

u_value f_parse_46_Context_46_modules_58__61__2(u_value v_x, u_value v_y) {
  u_value s1576 = v_x;
  u_value t1577 = NULL;
  if ((!t1577) && u_is_con(s1576, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1576, 0);
    u_value v_a2 = u_field(s1576, 2);
    u_value v_a3 = u_field(s1576, 3);
    u_value v_a4 = u_field(s1576, 4);
    t1577 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t1577) {
    u_no_case(s1576);
  }
  return t1577;
}

u_value f_parse_46_Context_46_loading_58__61__2(u_value v_x, u_value v_y) {
  u_value s1578 = v_x;
  u_value t1579 = NULL;
  if ((!t1579) && u_is_con(s1578, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1578, 0);
    u_value v_a1 = u_field(s1578, 1);
    u_value v_a3 = u_field(s1578, 3);
    u_value v_a4 = u_field(s1578, 4);
    t1579 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t1579) {
    u_no_case(s1578);
  }
  return t1579;
}

u_value f_parse_46_Context_46_base_58__61__2(u_value v_x, u_value v_y) {
  u_value s1580 = v_x;
  u_value t1581 = NULL;
  if ((!t1581) && u_is_con(s1580, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1580, 0);
    u_value v_a1 = u_field(s1580, 1);
    u_value v_a2 = u_field(s1580, 2);
    u_value v_a4 = u_field(s1580, 4);
    t1581 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t1581) {
    u_no_case(s1580);
  }
  return t1581;
}

u_value f_parse_46_Context_46_dir_58__61__2(u_value v_x, u_value v_y) {
  u_value s1582 = v_x;
  u_value t1583 = NULL;
  if ((!t1583) && u_is_con(s1582, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1582, 0);
    u_value v_a1 = u_field(s1582, 1);
    u_value v_a2 = u_field(s1582, 2);
    u_value v_a3 = u_field(s1582, 3);
    t1583 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t1583) {
    u_no_case(s1582);
  }
  return t1583;
}

u_value f_parse_46_with_commands_2(u_value v_cx, u_value v_cs) {
  return f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_with_notations_2(f_parse_46_Context_46_notations_1(v_cx), v_cs));
}

u_value f_parse_46_with_module_2(u_value v_cx, u_value v_c) {
  return f_parse_46_Context_46_modules_58__61__2(v_cx, f_list_46__43__43__2(f_parse_46_modules_of_1(v_c), f_parse_46_Context_46_modules_1(v_cx)));
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1584 = v_c;
  u_value t1585 = NULL;
  if ((!t1585) && u_is_con(s1584, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1584, 0);
    u_value v_cs = u_field(s1584, 1);
    u_value t1586 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1587 = &t1586;
    u_value l1588 = v_cs;
    while (u_is_con(l1588, &n_list_46_Cons)) {
      u_value v_d = u_field(l1588, 0);
      if (u_truth(u_mk_bool(1))) {
        e1587 = u_append(e1587, f_parse_46_modules_of_1(v_d));
      }
      l1588 = u_field(l1588, 1);
    }
    t1585 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1586));
  }
  if (!t1585) {
    t1585 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1585) {
    u_no_case(s1584);
  }
  return t1585;
}

u_value f_parse_46_own_notations_2(u_value v_cx, u_value v_n) {
  u_value s1589 = f_list_46_lookup_2(v_n, f_parse_46_Context_46_modules_1(v_cx));
  u_value t1590 = NULL;
  if ((!t1590) && u_is_con(s1589, &n_option_46_Some)) {
    u_value v_ns = u_field(s1589, 0);
    t1590 = v_ns;
  }
  if ((!t1590) && u_is_con(s1589, &n_option_46_None)) {
    t1590 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1590) {
    u_no_case(s1589);
  }
  return t1590;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1591 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1591;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1592 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1592;
        u_value t1594 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1594)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1593 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1593;
          }
          t1594 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1595 = f_list_46_reverse_1(v_rev);
    *r_ps = t1595;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1596 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1596;
    u_value s1597 = f_parse_46_head_token_1(*r_ts);
    int d1598 = 0;
    if ((!d1598) && (u_is_con(s1597, &n_lex_46_TName) && u_equals(u_field(s1597, 0), u_mk_lit("prefix")))) {
      d1598 = 1;
      {
        u_value t1599 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1599;
      }
    }
    if ((!d1598) && (u_is_con(s1597, &n_lex_46_TName) && u_equals(u_field(s1597, 0), u_mk_lit("postfix")))) {
      d1598 = 1;
      {
        u_value t1600 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1600;
      }
    }
    if (!d1598) {
      d1598 = 1;
      {
        u_value s1601 = f_parse_46_second_token_1(*r_ts);
        int d1602 = 0;
        if ((!d1602) && (u_is_con(s1601, &n_lex_46_TName) && u_equals(u_field(s1601, 0), u_mk_lit("left")))) {
          d1602 = 1;
          {
            u_value t1603 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1603;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1602) && (u_is_con(s1601, &n_lex_46_TName) && u_equals(u_field(s1601, 0), u_mk_lit("right")))) {
          d1602 = 1;
          {
            u_value t1604 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1604;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1602) {
          d1602 = 1;
          {
          }
        }
        if (!d1602) {
          u_no_case(s1601);
        }
      }
    }
    if (!d1598) {
      u_no_case(s1597);
    }
    p_parse_46_advance(r_ts);
    u_value s1605 = f_parse_46_head_token_1(*r_ts);
    int d1606 = 0;
    if ((!d1606) && u_is_con(s1605, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1605, 0);
      d1606 = 1;
      {
        u_value t1607 = v_n;
        v_p = t1607;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1606) {
      d1606 = 1;
      {
        static struct u_object l_precedence_32_expec_1608 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1609 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1608, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1609)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1606) {
      u_no_case(s1605);
    }
    u_value t1610 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1611 = u_mk_bool(1);
    v_ops = t1610;
    v_go = t1611;
    u_value t1617 = v_go;
    while (u_truth(t1617)) {
      {
        u_value s1612 = f_parse_46_head_token_1(*r_ts);
        int d1613 = 0;
        if ((!d1613) && u_is_con(s1612, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1612, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1613 = 1;
            {
              u_value t1614 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1614;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1613) && u_is_con(s1612, &n_lex_46_TName)) {
          u_value v_n = u_field(s1612, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1613 = 1;
            {
              u_value t1615 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1615;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1613) {
          d1613 = 1;
          {
            u_value t1616 = u_mk_bool(0);
            v_go = t1616;
          }
        }
        if (!d1613) {
          u_no_case(s1612);
        }
      }
      t1617 = v_go;
    }
    u_value s1618 = v_ops;
    int d1619 = 0;
    if ((!d1619) && u_is_con(s1618, &n_list_46_Nil)) {
      d1619 = 1;
      {
        static struct u_object l_operator_32_expecte_1620 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1621 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1620, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1621)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1619) {
      d1619 = 1;
      {
        u_value t1622 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1622;
      }
    }
    if (!d1619) {
      u_no_case(s1618);
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
    u_value t1623 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1624 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1623;
    *r_fs = t1624;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1625 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
            u_value t1626 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
            v_args = t1625;
            *r_fs = t1626;
            u_value t1629 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1629)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t1627 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t1628 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t1627;
                *r_fs = t1628;
              }
              t1629 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1630 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t1631 = f_list_46_reverse_1(*r_fs);
    *r_k = t1630;
    *r_fs = t1631;
    u_value s1632 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d1633 = 0;
    if ((!d1633) && u_is_con(s1632, &n_option_46_Some)) {
      u_value v_f = u_field(s1632, 0);
      d1633 = 1;
      {
        static struct u_object l_repeated_32_field_32__1634 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__1635 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__1634, u_concat(v_f, u_concat(&l__32_of_32__1635, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1633) && u_is_con(s1632, &n_option_46_None)) {
      d1633 = 1;
      {
      }
    }
    if (!d1633) {
      u_no_case(s1632);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1636 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1637 = 0;
    if ((!d1637) && (u_is_con(u_field(s1636, 0), &n_lex_46_TName) && (u_is_con(u_field(s1636, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1636, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1636, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1637 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1638 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1638;
        }
      }
    }
    if (!d1637) {
      d1637 = 1;
      {
        u_value t1639 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1639;
      }
    }
    if (!d1637) {
      u_no_case(s1636);
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
    static struct u_object l__58__61__1640 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1640), r_ts);
    static struct u_object l__124__1641 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1641)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1642 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1642;
    static struct u_object l__124__1643 = U_LIT("|");
    u_value t1645 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1643));
    while (u_truth(t1645)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1644 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1644;
      }
      static struct u_object l__124__1646 = U_LIT("|");
      t1645 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1646));
    }
    u_value s1647 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d1648 = 0;
    if ((!d1648) && u_is_con(s1647, &n_option_46_Some)) {
      u_value v_f = u_field(s1647, 0);
      d1648 = 1;
      {
        static struct u_object l_field_32__1649 = U_LIT("field ");
        static struct u_object l__32_of_32__1650 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_1651 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__1649, u_concat(v_f, u_concat(&l__32_of_32__1650, u_concat(v_n, &l__32_has_32_different_32_t_1651)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1648) && u_is_con(s1647, &n_option_46_None)) {
      d1648 = 1;
      {
      }
    }
    if (!d1648) {
      u_no_case(s1647);
    }
    u_value t1652 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_list_46__43__43__2(f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)), f_parse_46_updaters_3(v_n, v_ps, f_list_46_reverse_1(v_rev))));
    *r_cs = t1652;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s1653 = v_fs;
  u_value t1654 = NULL;
  if ((!t1654) && u_is_con(s1653, &n_list_46_Nil)) {
    t1654 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1654) && (u_is_con(s1653, &n_list_46_Cons) && u_is_con(u_field(s1653, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s1653, 0), 0);
    u_value v_rest = u_field(s1653, 1);
    t1654 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t1654) && (u_is_con(s1653, &n_list_46_Cons) && u_is_con(u_field(s1653, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1653, 1);
    t1654 = f_parse_46_field_names_1(v_rest);
  }
  if (!t1654) {
    u_no_case(s1653);
  }
  return t1654;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s1655 = v_fs;
  u_value t1656 = NULL;
  if ((!t1656) && u_is_con(s1655, &n_list_46_Nil)) {
    t1656 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1656) && u_is_con(s1655, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1655, 0), 0);
    u_value v_t = u_field(u_field(u_field(s1655, 0), 1), 3);
    u_value v_rest = u_field(s1655, 1);
    u_value t1657 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1658 = &t1657;
    u_value l1659 = f_parse_46_fields_named_2(v_f, v_rest);
    while (u_is_con(l1659, &n_list_46_Cons)) {
      u_value v_e = u_field(l1659, 0);
      if (u_truth(u_mk_bool(1))) {
        e1658 = u_append(e1658, f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t));
      }
      l1659 = u_field(l1659, 1);
    }
    u_value t1660;
    if (u_truth(f_list_46_contains_2(u_mk_bool(1), t1657))) {
      t1660 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t1660 = f_parse_46_differing_1(v_rest);
    }
    t1656 = t1660;
  }
  if (!t1656) {
    u_no_case(s1655);
  }
  return t1656;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s1661 = v_e;
  u_value t1662 = NULL;
  if (!t1662) {
    u_value v_t = u_field(u_field(s1661, 1), 3);
    t1662 = v_t;
  }
  if (!t1662) {
    u_no_case(s1661);
  }
  return t1662;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s1663 = v_fs;
  u_value t1664 = NULL;
  if ((!t1664) && u_is_con(s1663, &n_list_46_Nil)) {
    t1664 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1664) && u_is_con(s1663, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s1663, 0), 0);
    u_value v_d = u_field(u_field(s1663, 0), 1);
    u_value v_rest = u_field(s1663, 1);
    u_value t1665;
    if (u_truth(u_eq(v_g, v_f))) {
      t1665 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t1665 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t1664 = t1665;
  }
  if (!t1664) {
    u_no_case(s1663);
  }
  return t1664;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1666 = v_ks;
  u_value t1667 = NULL;
  if ((!t1667) && u_is_con(s1666, &n_list_46_Nil)) {
    t1667 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1667) && (u_is_con(s1666, &n_list_46_Cons) && u_is_con(u_field(u_field(s1666, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1666, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1666, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1666, 0), 1);
    u_value v_rest = u_field(s1666, 1);
    t1667 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1667) {
    u_no_case(s1666);
  }
  return t1667;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1668 = v_fs;
  u_value t1669 = NULL;
  if ((!t1669) && u_is_con(s1668, &n_list_46_Nil)) {
    t1669 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1669) && (u_is_con(s1668, &n_list_46_Cons) && u_is_con(u_field(u_field(s1668, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1668, 0), 0), 0);
    u_value v_t = u_field(u_field(s1668, 0), 1);
    u_value v_rest = u_field(s1668, 1);
    t1669 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1669) && (u_is_con(s1668, &n_list_46_Cons) && u_is_con(u_field(u_field(s1668, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1668, 1);
    t1669 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1669) {
    u_no_case(s1668);
  }
  return t1669;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1674 = f_parse_46_fields_of_1(v_ks);
  u_value t1673;
  {
    u_value v_fs = t1674;
    u_value t1670 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1671 = &t1670;
    u_value l1672 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1672, &n_list_46_Cons)) {
      u_value v_f = u_field(l1672, 0);
      if (u_truth(u_mk_bool(1))) {
        e1671 = u_append(e1671, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1672 = u_field(l1672, 1);
    }
    t1673 = t1670;
  }
  return t1673;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1675 = U_LIT("x");
  u_value t1676 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1677 = &t1676;
  u_value l1678 = v_ps;
  while (u_is_con(l1678, &n_list_46_Cons)) {
    u_value v_p = u_field(l1678, 0);
    if (u_truth(u_mk_bool(1))) {
      e1677 = u_append(e1677, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1678 = u_field(l1678, 1);
  }
  u_value t1686 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1675), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1687 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1676);
  u_value t1685;
  {
    u_value v_x = t1686;
    u_value v_owner = t1687;
    static struct u_object l__46__1679 = U_LIT(".");
    static struct u_object l_x_1680 = U_LIT("x");
    u_value t1681 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1682 = &t1681;
    u_value l1683 = v_es;
    while (u_is_con(l1683, &n_list_46_Cons)) {
      u_value v_e = u_field(l1683, 0);
      if (u_truth(u_mk_bool(1))) {
        e1682 = u_append(e1682, f_parse_46_field_case_1(v_e));
      }
      l1683 = u_field(l1683, 1);
    }
    u_value t1684;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1684 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1684 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1685 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1679, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1680), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1681, t1684)));
  }
  return t1685;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1688 = v_es;
  u_value t1689 = NULL;
  if ((!t1689) && u_is_con(s1688, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1688, 0), 1), 3);
    t1689 = v_t;
  }
  if (!t1689) {
    t1689 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1689) {
    u_no_case(s1688);
  }
  return t1689;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s1690 = v_e;
  u_value t1691 = NULL;
  if (!t1691) {
    u_value v_k = u_field(u_field(s1690, 1), 0);
    u_value v_i = u_field(u_field(s1690, 1), 1);
    u_value v_n = u_field(u_field(s1690, 1), 2);
    u_value t1692 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1693 = &t1692;
    u_value l1694 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1694, &n_list_46_Cons)) {
      u_value v_j = u_field(l1694, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1696;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1695 = U_LIT("y");
          t1696 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1695), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1696 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1693 = u_append(e1693, t1696);
      }
      l1694 = u_field(l1694, 1);
    }
    static struct u_object l_y_1697 = U_LIT("y");
    t1691 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1692), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1697), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1691) {
    u_no_case(s1690);
  }
  return t1691;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_MissingField_1698 = U_LIT("MissingField");
  static struct u_object l__46__1699 = U_LIT(".");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_ERaise, 2, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_MissingField_1698), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_t, u_concat(&l__46__1699, v_f)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_parse_46_updaters_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1704 = f_parse_46_fields_of_1(v_ks);
  u_value t1703;
  {
    u_value v_fs = t1704;
    u_value t1700 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1701 = &t1700;
    u_value l1702 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1702, &n_list_46_Cons)) {
      u_value v_f = u_field(l1702, 0);
      if (u_truth(u_mk_bool(1))) {
        e1701 = u_append(e1701, f_parse_46_updater_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1702 = u_field(l1702, 1);
    }
    t1703 = t1700;
  }
  return t1703;
}

u_value f_parse_46_updater_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1705 = U_LIT("x");
  u_value t1706 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1707 = &t1706;
  u_value l1708 = v_ps;
  while (u_is_con(l1708, &n_list_46_Cons)) {
    u_value v_p = u_field(l1708, 0);
    if (u_truth(u_mk_bool(1))) {
      e1707 = u_append(e1707, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1708 = u_field(l1708, 1);
  }
  u_value t1718 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1705), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1719 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1706);
  u_value t1717;
  {
    u_value v_x = t1718;
    u_value v_owner = t1719;
    static struct u_object l__46__1709 = U_LIT(".");
    static struct u_object l__58__61__1710 = U_LIT(":=");
    static struct u_object l_x_1711 = U_LIT("x");
    static struct u_object l_y_1712 = U_LIT("y");
    u_value t1713 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1714 = &t1713;
    u_value l1715 = v_es;
    while (u_is_con(l1715, &n_list_46_Cons)) {
      u_value v_e = u_field(l1715, 0);
      if (u_truth(u_mk_bool(1))) {
        e1714 = u_append(e1714, f_parse_46_update_case_1(v_e));
      }
      l1715 = u_field(l1715, 1);
    }
    u_value t1716;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1716 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1716 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1717 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1709, u_concat(v_f, &l__58__61__1710)))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1711), v_owner), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1712), f_parse_46_field_type_1(v_es)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), v_owner, u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1713, t1716)));
  }
  return t1717;
}

u_value f_parse_46_update_case_1(u_value v_e) {
  u_value s1720 = v_e;
  u_value t1721 = NULL;
  if (!t1721) {
    u_value v_k = u_field(u_field(s1720, 1), 0);
    u_value v_i = u_field(u_field(s1720, 1), 1);
    u_value v_n = u_field(u_field(s1720, 1), 2);
    u_value t1722 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1723 = &t1722;
    u_value l1724 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1724, &n_list_46_Cons)) {
      u_value v_j = u_field(l1724, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1726;
        if (u_truth(u_eq(v_j, v_i))) {
          t1726 = u_mk_con(&n_syntax_46_PWild, 0);
        } else {
          static struct u_object l_a_1725 = U_LIT("a");
          t1726 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l_a_1725, u_str_show(v_j))), u_mk_con(&n_syntax_46_THole, 0)));
        }
        e1723 = u_append(e1723, t1726);
      }
      l1724 = u_field(l1724, 1);
    }
    u_value t1727 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1728 = &t1727;
    u_value l1729 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1729, &n_list_46_Cons)) {
      u_value v_j = u_field(l1729, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1732;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1730 = U_LIT("y");
          t1732 = &l_y_1730;
        } else {
          static struct u_object l_a_1731 = U_LIT("a");
          t1732 = u_concat(&l_a_1731, u_str_show(v_j));
        }
        e1728 = u_append(e1728, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(t1732), u_mk_con(&n_syntax_46_THole, 0))));
      }
      l1729 = u_field(l1729, 1);
    }
    t1721 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1722), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_k, u_mk_con(&n_syntax_46_THole, 0))), t1727));
  }
  if (!t1721) {
    u_no_case(s1720);
  }
  return t1721;
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t1733;
  if (u_truth(u_eq(v_i, v_n))) {
    t1733 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1733 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t1733;
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
    u_value t1734 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1734;
    u_value t1736 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1736)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1735 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1735;
      }
      t1736 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1737 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1737), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1738 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1738), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1739 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1739;
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
    u_value t1740 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1741 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1742 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1743 = u_mk_bool(1);
    v_ins = t1740;
    v_outs = t1741;
    v_refs = t1742;
    v_first = t1743;
    static struct u_object l_in_1744 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1744))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_ins, r_ts);
        u_value t1745 = u_mk_bool(0);
        v_first = t1745;
      }
    }
    static struct u_object l_out_1746 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1746, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_outs, r_ts);
        u_value t1747 = u_mk_bool(0);
        v_first = t1747;
      }
    }
    static struct u_object l_ref_1748 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1748, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    static struct u_object l__58__61__1749 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1749), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1750 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1750;
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
    u_value t1751 = f_parse_46_Context_46_notations_1(v_cx);
    v_ns = t1751;
    u_value s1752 = f_parse_46_head_token_1(*r_ts);
    int d1753 = 0;
    if ((!d1753) && (u_is_con(s1752, &n_lex_46_TName) && u_equals(u_field(s1752, 0), u_mk_lit("import")))) {
      d1753 = 1;
      {
        u_value t1754 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1754;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Context_46_modules_1(v_cx))))) {
          {
            u_value t1755 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1755;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_cx, v_at, r_cs);
            u_value t1756 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1756;
          }
        }
      }
    }
    if ((!d1753) && u_is_con(s1752, &n_lex_46_TName)) {
      u_value v_w = u_field(s1752, 0);
      static struct u_object l_prefix_1758 = U_LIT("prefix");
      static struct u_object l_postfix_1759 = U_LIT("postfix");
      static struct u_object l_infix_1760 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1758, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1759, u_mk_con(&n_list_46_Cons, 2, &l_infix_1760, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1753 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1757 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1757;
        }
      }
    }
    if ((!d1753) && (u_is_con(s1752, &n_lex_46_TName) && u_equals(u_field(s1752, 0), u_mk_lit("type")))) {
      d1753 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1761 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1761), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1762 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1762;
      }
    }
    if ((!d1753) && (u_is_con(s1752, &n_lex_46_TName) && u_equals(u_field(s1752, 0), u_mk_lit("data")))) {
      d1753 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1753) && (u_is_con(s1752, &n_lex_46_TName) && u_equals(u_field(s1752, 0), u_mk_lit("function")))) {
      d1753 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1763 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1763;
      }
    }
    if ((!d1753) && (u_is_con(s1752, &n_lex_46_TName) && u_equals(u_field(s1752, 0), u_mk_lit("procedure")))) {
      d1753 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1764 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1764;
      }
    }
    if ((!d1753) && (u_is_con(s1752, &n_lex_46_TName) && u_equals(u_field(s1752, 0), u_mk_lit("eval")))) {
      d1753 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_e, r_ts);
        u_value t1765 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CEval, 1, v_e), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1765;
      }
    }
    if ((!d1753) && (u_is_con(s1752, &n_lex_46_TName) && u_equals(u_field(s1752, 0), u_mk_lit("run")))) {
      d1753 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1766 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1766), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1767 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1767;
      }
    }
    if ((!d1753) && (u_is_con(s1752, &n_lex_46_TName) && u_equals(u_field(s1752, 0), u_mk_lit("test")))) {
      d1753 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1768 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1768), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1769 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1769;
      }
    }
    if (!d1753) {
      d1753 = 1;
      {
        static struct u_object l_declaration_32_expe_1770 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1771 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1770, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1771)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1753) {
      u_no_case(s1752);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_cx) {
  {
    u_value s1772 = v_cs;
    int d1773 = 0;
    if ((!d1773) && u_is_con(s1772, &n_list_46_Nil)) {
      d1773 = 1;
      {
      }
    }
    if ((!d1773) && (u_is_con(s1772, &n_list_46_Cons) && u_is_con(u_field(s1772, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1772, 0), 0);
      u_value v_p = u_field(u_field(s1772, 0), 1);
      u_value v_ops = u_field(u_field(s1772, 0), 2);
      u_value v_more = u_field(s1772, 1);
      d1773 = 1;
      {
        u_value t1774 = f_parse_46_with_commands_2(*r_cx, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_cx = t1774;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1773) && (u_is_con(s1772, &n_list_46_Cons) && u_is_con(u_field(s1772, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1772, 0), 0);
      u_value v_ds = u_field(u_field(s1772, 0), 1);
      u_value v_more = u_field(s1772, 1);
      d1773 = 1;
      {
        u_value t1775 = f_parse_46_with_module_2(*r_cx, u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds));
        *r_cx = t1775;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1773) && (u_is_con(s1772, &n_list_46_Cons) && u_is_con(u_field(s1772, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1772, 0), 0);
      u_value v_more = u_field(s1772, 1);
      d1773 = 1;
      {
        u_value t1776 = f_parse_46_with_commands_2(*r_cx, f_parse_46_own_notations_2(*r_cx, v_n));
        *r_cx = t1776;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1773) && u_is_con(s1772, &n_list_46_Cons)) {
      u_value v_more = u_field(s1772, 1);
      d1773 = 1;
      {
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if (!d1773) {
      u_no_case(s1772);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_cx, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1777 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1777;
    u_value t1779 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1779)) {
      {
        p_parse_46_parse_command(*r_cx, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_cx);
        u_value t1778 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1778;
      }
      t1779 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1780 = f_list_46_reverse_1(v_rev);
    *r_cs = t1780;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1781 = v_dir;
    int d1782 = 0;
    if ((!d1782) && u_is_con(s1781, &n_option_46_Some)) {
      u_value v_d = u_field(s1781, 0);
      d1782 = 1;
      {
        {
          struct u_handler *h1785 = u_push();
          if (setjmp(h1785->env) == 0) {
            {
              static struct u_object l__47__1783 = U_LIT("/");
              static struct u_object l__46_u_1784 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1783, u_concat(v_n, &l__46_u_1784))), r_text);
            }
            u_pop(h1785);
          } else {
            u_pop(h1785);
            int d1786 = 0;
            if ((!d1786) && (u_has_type(h1785, "os.OsError") && u_is_con(u_caught(h1785), &n_os_46_OsError))) {
              d1786 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1786) {
              u_reraise(h1785);
            }
          }
        }
      }
    }
    if ((!d1782) && u_is_con(s1781, &n_option_46_None)) {
      d1782 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1782) {
      u_no_case(s1781);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1787 = u_push();
      if (setjmp(h1787->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1787);
      } else {
        u_pop(h1787);
        int d1788 = 0;
        if ((!d1788) && (u_has_type(h1787, "os.OsError") && u_is_con(u_caught(h1787), &n_os_46_OsError))) {
          d1788 = 1;
          {
            static struct u_object l_unknown_32_module_32__1789 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1789, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1788) {
          u_reraise(h1787);
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
        static struct u_object l_cyclic_32_import_32_of_1790 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1790, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Context_46_dir_1(v_cx), v_at, &v_text);
    u_value t1791 = f_parse_46_Context_46_loading_58__61__2(f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_base_notations_1(v_cx)), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Context_46_loading_1(v_cx)));
    v_mcx = t1791;
    {
      struct u_handler *h1792 = u_push();
      if (setjmp(h1792->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mcx, &v_mts);
        }
        u_pop(h1792);
      } else {
        u_pop(h1792);
        int d1793 = 0;
        if ((!d1793) && (u_has_type(h1792, "lex.LexError") && u_is_con(u_caught(h1792), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1792), 0);
          u_value v_i = u_field(u_caught(h1792), 1);
          d1793 = 1;
          {
            static struct u_object l__32_in_32__1794 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1794, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1793) && (u_has_type(h1792, "parse.ParseError") && u_is_con(u_caught(h1792), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1792), 0);
          u_value v_i = u_field(u_caught(h1792), 1);
          d1793 = 1;
          {
            static struct u_object l__32_in_32__1795 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1795, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1793) {
          u_reraise(h1792);
        }
      }
    }
  }
}

u_value f_parse_46_base_notations_1(u_value v_cx) {
  u_value s1796 = f_parse_46_Context_46_base_1(v_cx);
  u_value t1797 = NULL;
  if ((!t1797) && u_is_con(s1796, &n_option_46_Some)) {
    u_value v_b = u_field(s1796, 0);
    t1797 = v_b;
  }
  if ((!t1797) && u_is_con(s1796, &n_option_46_None)) {
    t1797 = f_parse_46_Context_46_notations_1(v_cx);
  }
  if (!t1797) {
    u_no_case(s1796);
  }
  return t1797;
}

void p_parse_46_parse_text(u_value v_text, u_value v_cx, u_value *r_p) {
  {
    u_value v_ts = NULL;
    u_value v_cs = NULL;
    u_value v_after = NULL;
    u_value t1798 = v_cx;
    v_after = t1798;
    {
      struct u_handler *h1799 = u_push();
      if (setjmp(h1799->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1799);
      } else {
        u_pop(h1799);
        int d1800 = 0;
        if ((!d1800) && (u_has_type(h1799, "lex.LexError") && u_is_con(u_caught(h1799), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1799), 0);
          u_value v_i = u_field(u_caught(h1799), 1);
          d1800 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1800) {
          u_reraise(h1799);
        }
      }
    }
    p_parse_46_parse_commands(&v_cs, &v_after, &v_ts);
    u_value t1801 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1801;
  }
}

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p) {
  {
    u_value v_cx = NULL;
    u_value t1802 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    v_cx = t1802;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), &v_cx);
    p_parse_46_parse_text(v_text, f_parse_46_Context_46_dir_58__61__2(f_parse_46_Context_46_base_58__61__2(f_parse_46_Context_46_loading_58__61__2(v_cx, u_mk_con(&n_list_46_Cons, 2, v_name, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_option_46_Some, 1, f_parse_46_Context_46_notations_1(v_cx))), v_dir), r_p);
  }
}

void p_parse_46_parse_library(u_value v_n, u_value *r_p) {
  {
    u_value v_text = NULL;
    u_value v_pre = NULL;
    p_parse_46_library_text(v_n, u_mk_int(0), &v_text);
    static struct u_object l_prelude_1803 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1803))) {
      {
        static struct u_object l_prelude_1804 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1804, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1805 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1805, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1806 = v_xs;
  u_value t1807 = NULL;
  if ((!t1807) && (u_is_con(s1806, &n_list_46_Cons) && u_is_con(u_field(s1806, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1806, 0);
    t1807 = v_x;
  }
  if ((!t1807) && u_is_con(s1806, &n_list_46_Cons)) {
    u_value v_more = u_field(s1806, 1);
    t1807 = f_parse_46_last_part_1(v_more);
  }
  if (!t1807) {
    static struct u_object l__1808 = U_LIT("");
    t1807 = &l__1808;
  }
  if (!t1807) {
    u_no_case(s1806);
  }
  return t1807;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1813 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1812;
  {
    u_value v_f = t1813;
    u_value t1810 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1810)) {
      static struct u_object l__46_u_1809 = U_LIT(".u");
      t1810 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1809);
    }
    u_value t1811;
    if (u_truth(t1810)) {
      t1811 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1811 = v_f;
    }
    t1812 = t1811;
  }
  return t1812;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1818 = u_str_split(v_path, u_mk_char(47));
  u_value t1817;
  {
    u_value v_parts = t1818;
    u_value t1816;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1814 = U_LIT(".");
      t1816 = &l__46__1814;
    } else {
      static struct u_object l__47__1815 = U_LIT("/");
      t1816 = u_str_join(&l__47__1815, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1817 = t1816;
  }
  return t1817;
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
    static struct u_object l_prelude_1819 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1819, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1820 = u_mk_tuple(2, v_cs, v_ds);
    int d1821 = 0;
    if ((!d1821) && (u_is_con(u_field(s1820, 0), &n_list_46_Cons) && u_is_con(u_field(s1820, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1820, 0), 0);
      u_value v_more = u_field(u_field(s1820, 0), 1);
      u_value v_d = u_field(u_field(s1820, 1), 0);
      u_value v_rest = u_field(u_field(s1820, 1), 1);
      d1821 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1821) {
      d1821 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1821) {
      u_no_case(s1820);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1822 = v_paths;
    int d1823 = 0;
    if ((!d1823) && u_is_con(s1822, &n_list_46_Nil)) {
      d1823 = 1;
      {
      }
    }
    if ((!d1823) && u_is_con(s1822, &n_list_46_Cons)) {
      u_value v_path = u_field(s1822, 0);
      u_value v_more = u_field(s1822, 1);
      d1823 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1823) {
      u_no_case(s1822);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1824 = v_paths;
    int d1825 = 0;
    if ((!d1825) && u_is_con(s1824, &n_list_46_Nil)) {
      d1825 = 1;
      {
      }
    }
    if ((!d1825) && u_is_con(s1824, &n_list_46_Cons)) {
      u_value v_path = u_field(s1824, 0);
      u_value v_more = u_field(s1824, 1);
      d1825 = 1;
      {
        u_value t1826 = u_mk_bool(0);
        v_failed = t1826;
        {
          struct u_handler *h1827 = u_push();
          if (setjmp(h1827->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1827);
          } else {
            u_pop(h1827);
            int d1828 = 0;
            if ((!d1828) && (u_has_type(h1827, "parse.ParseError") && u_is_con(u_caught(h1827), &n_parse_46_ParseError))) {
              d1828 = 1;
              {
                u_value t1829 = u_mk_bool(1);
                v_failed = t1829;
              }
            }
            if (!d1828) {
              u_reraise(h1827);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1825) {
      u_no_case(s1824);
    }
  }
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s1830 = v_x;
  u_value t1831 = NULL;
  if ((!t1831) && u_is_con(s1830, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1830, 0);
    t1831 = v_y;
  }
  if (!t1831) {
    u_no_case(s1830);
  }
  return t1831;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s1832 = v_x;
  u_value t1833 = NULL;
  if ((!t1833) && u_is_con(s1832, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1832, 1);
    t1833 = v_y;
  }
  if (!t1833) {
    u_no_case(s1832);
  }
  return t1833;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s1834 = v_x;
  u_value t1835 = NULL;
  if ((!t1835) && u_is_con(s1834, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1834, 2);
    t1835 = v_y;
  }
  if (!t1835) {
    u_no_case(s1834);
  }
  return t1835;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s1836 = v_x;
  u_value t1837 = NULL;
  if ((!t1837) && u_is_con(s1836, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1836, 3);
    t1837 = v_y;
  }
  if (!t1837) {
    u_no_case(s1836);
  }
  return t1837;
}

u_value f_compile_46_Globals_46_funs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1838 = v_x;
  u_value t1839 = NULL;
  if ((!t1839) && u_is_con(s1838, &n_compile_46_Globals)) {
    u_value v_a1 = u_field(s1838, 1);
    u_value v_a2 = u_field(s1838, 2);
    u_value v_a3 = u_field(s1838, 3);
    t1839 = u_mk_con(&n_compile_46_Globals, 4, v_y, v_a1, v_a2, v_a3);
  }
  if (!t1839) {
    u_no_case(s1838);
  }
  return t1839;
}

u_value f_compile_46_Globals_46_constrs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1840 = v_x;
  u_value t1841 = NULL;
  if ((!t1841) && u_is_con(s1840, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1840, 0);
    u_value v_a2 = u_field(s1840, 2);
    u_value v_a3 = u_field(s1840, 3);
    t1841 = u_mk_con(&n_compile_46_Globals, 4, v_a0, v_y, v_a2, v_a3);
  }
  if (!t1841) {
    u_no_case(s1840);
  }
  return t1841;
}

u_value f_compile_46_Globals_46_arities_58__61__2(u_value v_x, u_value v_y) {
  u_value s1842 = v_x;
  u_value t1843 = NULL;
  if ((!t1843) && u_is_con(s1842, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1842, 0);
    u_value v_a1 = u_field(s1842, 1);
    u_value v_a3 = u_field(s1842, 3);
    t1843 = u_mk_con(&n_compile_46_Globals, 4, v_a0, v_a1, v_y, v_a3);
  }
  if (!t1843) {
    u_no_case(s1842);
  }
  return t1843;
}

u_value f_compile_46_Globals_46_fields_58__61__2(u_value v_x, u_value v_y) {
  u_value s1844 = v_x;
  u_value t1845 = NULL;
  if ((!t1845) && u_is_con(s1844, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1844, 0);
    u_value v_a1 = u_field(s1844, 1);
    u_value v_a2 = u_field(s1844, 2);
    t1845 = u_mk_con(&n_compile_46_Globals, 4, v_a0, v_a1, v_a2, v_y);
  }
  if (!t1845) {
    u_no_case(s1844);
  }
  return t1845;
}

u_value f_compile_46_Gen_46_count_1(u_value v_x) {
  u_value s1846 = v_x;
  u_value t1847 = NULL;
  if ((!t1847) && u_is_con(s1846, &n_compile_46_Gen)) {
    u_value v_y = u_field(s1846, 0);
    t1847 = v_y;
  }
  if (!t1847) {
    u_no_case(s1846);
  }
  return t1847;
}

u_value f_compile_46_Gen_46_lifted_1(u_value v_x) {
  u_value s1848 = v_x;
  u_value t1849 = NULL;
  if ((!t1849) && u_is_con(s1848, &n_compile_46_Gen)) {
    u_value v_y = u_field(s1848, 1);
    t1849 = v_y;
  }
  if (!t1849) {
    u_no_case(s1848);
  }
  return t1849;
}

u_value f_compile_46_Gen_46_count_58__61__2(u_value v_x, u_value v_y) {
  u_value s1850 = v_x;
  u_value t1851 = NULL;
  if ((!t1851) && u_is_con(s1850, &n_compile_46_Gen)) {
    u_value v_a1 = u_field(s1850, 1);
    t1851 = u_mk_con(&n_compile_46_Gen, 2, v_y, v_a1);
  }
  if (!t1851) {
    u_no_case(s1850);
  }
  return t1851;
}

u_value f_compile_46_Gen_46_lifted_58__61__2(u_value v_x, u_value v_y) {
  u_value s1852 = v_x;
  u_value t1853 = NULL;
  if ((!t1853) && u_is_con(s1852, &n_compile_46_Gen)) {
    u_value v_a0 = u_field(s1852, 0);
    t1853 = u_mk_con(&n_compile_46_Gen, 2, v_a0, v_y);
  }
  if (!t1853) {
    u_no_case(s1852);
  }
  return t1853;
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
  static struct u_object l_u_value_1854 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1854);
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
  static struct u_object l_u_value_1855 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1855));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t1856 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1857 = &t1856;
  u_value l1858 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l1858, &n_list_46_Cons)) {
    u_value v_c = u_field(l1858, 0);
    if (u_truth(u_mk_bool(1))) {
      e1857 = u_append(e1857, f_compile_46_mangle_char_1(v_c));
    }
    l1858 = u_field(l1858, 1);
  }
  return u_string(f_list_46_concat_1(t1856));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t1859 = u_is_letter(v_c);
  if (!u_truth(t1859)) {
    t1859 = u_is_digit(v_c);
  }
  u_value t1860 = t1859;
  if (!u_truth(t1860)) {
    t1860 = u_eq(v_c, u_mk_char(95));
  }
  u_value t1863;
  if (u_truth(t1860)) {
    t1863 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___1861 = U_LIT("_");
    static struct u_object l___1862 = U_LIT("_");
    t1863 = u_chars(u_concat(&l___1861, u_concat(u_str_show(u_ord(v_c)), &l___1862)));
  }
  return t1863;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__1864 = U_LIT("f_");
  static struct u_object l___1865 = U_LIT("_");
  return u_concat(&l_f__1864, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___1865, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__1866 = U_LIT("p_");
  return u_concat(&l_p__1866, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__1867 = U_LIT("v_");
  return u_concat(&l_v__1867, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__1868 = U_LIT("r_");
  return u_concat(&l_r__1868, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__1869 = U_LIT("+");
  static struct u_object l_u_add_1870 = U_LIT("u_add");
  static struct u_object l__45__1871 = U_LIT("-");
  static struct u_object l_u_sub_1872 = U_LIT("u_sub");
  static struct u_object l__42__1873 = U_LIT("*");
  static struct u_object l_u_mul_1874 = U_LIT("u_mul");
  static struct u_object l__47__1875 = U_LIT("/");
  static struct u_object l_u_div_1876 = U_LIT("u_div");
  static struct u_object l__37__1877 = U_LIT("%");
  static struct u_object l_u_mod_1878 = U_LIT("u_mod");
  static struct u_object l__60__1879 = U_LIT("<");
  static struct u_object l_u_lt_1880 = U_LIT("u_lt");
  static struct u_object l__61__61__1881 = U_LIT("==");
  static struct u_object l_u_eq_1882 = U_LIT("u_eq");
  static struct u_object l_compare_1883 = U_LIT("compare");
  static struct u_object l_u_compare_1884 = U_LIT("u_compare");
  static struct u_object l_chars_1885 = U_LIT("chars");
  static struct u_object l_u_chars_1886 = U_LIT("u_chars");
  static struct u_object l_string_1887 = U_LIT("string");
  static struct u_object l_u_string_1888 = U_LIT("u_string");
  static struct u_object l_ord_1889 = U_LIT("ord");
  static struct u_object l_u_ord_1890 = U_LIT("u_ord");
  static struct u_object l_chr_1891 = U_LIT("chr");
  static struct u_object l_u_chr_1892 = U_LIT("u_chr");
  static struct u_object l__94__1893 = U_LIT("^");
  static struct u_object l_u_concat_1894 = U_LIT("u_concat");
  static struct u_object l_str_size_1895 = U_LIT("str_size");
  static struct u_object l_u_str_size_1896 = U_LIT("u_str_size");
  static struct u_object l_str_join_1897 = U_LIT("str_join");
  static struct u_object l_u_str_join_1898 = U_LIT("u_str_join");
  static struct u_object l_str_slice_1899 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_1900 = U_LIT("u_str_slice");
  static struct u_object l_str_split_1901 = U_LIT("str_split");
  static struct u_object l_u_str_split_1902 = U_LIT("u_str_split");
  static struct u_object l_str_get_1903 = U_LIT("str_get");
  static struct u_object l_u_str_get_1904 = U_LIT("u_str_get");
  static struct u_object l_str_contains_1905 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_1906 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_1907 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_1908 = U_LIT("u_str_compare");
  static struct u_object l_str_show_1909 = U_LIT("str_show");
  static struct u_object l_u_str_show_1910 = U_LIT("u_str_show");
  static struct u_object l_str_quote_1911 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_1912 = U_LIT("u_str_quote");
  static struct u_object l_str_number_1913 = U_LIT("str_number");
  static struct u_object l_u_str_number_1914 = U_LIT("u_str_number");
  static struct u_object l_is_digit_1915 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_1916 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_1917 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_1918 = U_LIT("u_is_letter");
  static struct u_object l_is_space_1919 = U_LIT("is_space");
  static struct u_object l_u_is_space_1920 = U_LIT("u_is_space");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__1869), u_mk_tuple(2, &l_u_add_1870, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__1871), u_mk_tuple(2, &l_u_sub_1872, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__1873), u_mk_tuple(2, &l_u_mul_1874, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__1875), u_mk_tuple(2, &l_u_div_1876, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__1877), u_mk_tuple(2, &l_u_mod_1878, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__1879), u_mk_tuple(2, &l_u_lt_1880, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__1881), u_mk_tuple(2, &l_u_eq_1882, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_1883), u_mk_tuple(2, &l_u_compare_1884, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_1885), u_mk_tuple(2, &l_u_chars_1886, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_1887), u_mk_tuple(2, &l_u_string_1888, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_1889), u_mk_tuple(2, &l_u_ord_1890, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_1891), u_mk_tuple(2, &l_u_chr_1892, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__1893), u_mk_tuple(2, &l_u_concat_1894, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_1895), u_mk_tuple(2, &l_u_str_size_1896, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_1897), u_mk_tuple(2, &l_u_str_join_1898, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_1899), u_mk_tuple(2, &l_u_str_slice_1900, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_1901), u_mk_tuple(2, &l_u_str_split_1902, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_1903), u_mk_tuple(2, &l_u_str_get_1904, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_1905), u_mk_tuple(2, &l_u_str_contains_1906, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_1907), u_mk_tuple(2, &l_u_str_compare_1908, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_1909), u_mk_tuple(2, &l_u_str_show_1910, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_1911), u_mk_tuple(2, &l_u_str_quote_1912, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_1913), u_mk_tuple(2, &l_u_str_number_1914, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_1915), u_mk_tuple(2, &l_u_is_digit_1916, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_1917), u_mk_tuple(2, &l_u_is_letter_1918, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_1919), u_mk_tuple(2, &l_u_is_space_1920, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)))))))))))))))))))))))))));
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__1921 = U_LIT("&");
  static struct u_object l_n__1922 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__1921, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__1922, f_compile_46_mangle_1(v_n))));
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
  u_value s1923 = v_n;
  u_value t1924 = NULL;
  if ((!t1924) && (u_is_con(s1923, &n_syntax_46_Name) && u_is_con(u_field(s1923, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s1923, 0), 0);
    u_value v_b = u_field(s1923, 1);
    static struct u_object l_const_32_struct_32_u_n_1925 = U_LIT("const struct u_name");
    static struct u_object l_n__1926 = U_LIT("n_");
    t1924 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1925), u_concat(&l_n__1926, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t1924) && (u_is_con(s1923, &n_syntax_46_Name) && u_is_con(u_field(s1923, 0), &n_option_46_None))) {
    u_value v_b = u_field(s1923, 1);
    static struct u_object l_const_32_struct_32_u_n_1927 = U_LIT("const struct u_name");
    static struct u_object l_n__1928 = U_LIT("n_");
    static struct u_object l_NULL_1929 = U_LIT("NULL");
    t1924 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1927), u_concat(&l_n__1928, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1929), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t1924) {
    u_no_case(s1923);
  }
  return t1924;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s1930 = v_cs;
  u_value t1931 = NULL;
  if ((!t1931) && u_is_con(s1930, &n_list_46_Nil)) {
    t1931 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1931) && (u_is_con(s1930, &n_list_46_Cons) && u_is_con(u_field(s1930, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s1930, 0), 2);
    u_value v_rest = u_field(s1930, 1);
    u_value t1932 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1933 = &t1932;
    u_value l1934 = v_ks;
    while (u_is_con(l1934, &n_list_46_Cons)) {
      u_value v_k = u_field(l1934, 0);
      if (u_truth(u_mk_bool(1))) {
        e1933 = u_append(e1933, f_syntax_46_Constr_46_name_1(v_k));
      }
      l1934 = u_field(l1934, 1);
    }
    t1931 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t1932, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t1931) && u_is_con(s1930, &n_list_46_Cons)) {
    u_value v_rest = u_field(s1930, 1);
    t1931 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t1931) {
    u_no_case(s1930);
  }
  return t1931;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s1935 = v_ks;
  u_value t1936 = NULL;
  if ((!t1936) && u_is_con(s1935, &n_list_46_Nil)) {
    t1936 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1936) && u_is_con(s1935, &n_list_46_Cons)) {
    u_value v_k = u_field(s1935, 0);
    u_value v_rest = u_field(s1935, 1);
    u_value t1937;
    if (u_truth(f_list_46_contains_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t1937 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t1937 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t1936 = t1937;
  }
  if (!t1936) {
    u_no_case(s1935);
  }
  return t1936;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t1938 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1939 = &t1938;
  u_value l1940 = v_xs;
  while (u_is_con(l1940, &n_list_46_Cons)) {
    u_value v_x = u_field(l1940, 0);
    if (u_truth(u_mk_bool(1))) {
      e1939 = u_append(e1939, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l1940 = u_field(l1940, 1);
  }
  return t1938;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__1941 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__1941, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s1942 = f_list_46_lookup_2(v_x, v_env);
  u_value t1943 = NULL;
  if ((!t1943) && u_is_con(s1942, &n_option_46_Some)) {
    u_value v_c = u_field(s1942, 0);
    t1943 = v_c;
  }
  if (!t1943) {
    t1943 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t1943) {
    u_no_case(s1942);
  }
  return t1943;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s1944 = v_c;
  u_value t1945 = NULL;
  if ((!t1945) && (u_is_con(s1944, &n_c_46_CeUnary) && u_equals(u_field(s1944, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s1944, 1);
    t1945 = v_p;
  }
  if (!t1945) {
    static struct u_object l__38__1946 = U_LIT("&");
    t1945 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1946, v_c);
  }
  if (!t1945) {
    u_no_case(s1944);
  }
  return t1945;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s1947 = v_t;
  u_value t1948 = NULL;
  if ((!t1948) && (u_is_con(s1947, &n_syntax_46_TSort) && u_is_con(u_field(s1947, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s1947, 0);
    t1948 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t1948) && u_is_con(s1947, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s1947, 0);
    u_value v_args = u_field(s1947, 1);
    static struct u_object l__91__1949 = U_LIT("[");
    static struct u_object l__44__32__1950 = U_LIT(", ");
    u_value t1951 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1952 = &t1951;
    u_value l1953 = v_args;
    while (u_is_con(l1953, &n_list_46_Cons)) {
      u_value v_a = u_field(l1953, 0);
      if (u_truth(u_mk_bool(1))) {
        e1952 = u_append(e1952, f_compile_46_type_key_1(v_a));
      }
      l1953 = u_field(l1953, 1);
    }
    static struct u_object l__93__1954 = U_LIT("]");
    t1948 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__1949, u_concat(u_str_join(&l__44__32__1950, t1951), &l__93__1954)));
  }
  if ((!t1948) && u_is_con(s1947, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1947, 0);
    u_value v_r = u_field(s1947, 1);
    static struct u_object l__40__1955 = U_LIT("(");
    static struct u_object l__44__32__1956 = U_LIT(", ");
    u_value t1957 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1958 = &t1957;
    u_value l1959 = v_args;
    while (u_is_con(l1959, &n_list_46_Cons)) {
      u_value v_a = u_field(l1959, 0);
      if (u_truth(u_mk_bool(1))) {
        e1958 = u_append(e1958, f_compile_46_type_key_1(v_a));
      }
      l1959 = u_field(l1959, 1);
    }
    static struct u_object l__41__32__45__62__32__1960 = U_LIT(") -> ");
    t1948 = u_concat(&l__40__1955, u_concat(u_str_join(&l__44__32__1956, t1957), u_concat(&l__41__32__45__62__32__1960, f_compile_46_type_key_1(v_r))));
  }
  if ((!t1948) && u_is_con(s1947, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1947, 0);
    static struct u_object l__40__1961 = U_LIT("(");
    static struct u_object l__32__42__32__1962 = U_LIT(" * ");
    u_value t1963 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1964 = &t1963;
    u_value l1965 = v_ts;
    while (u_is_con(l1965, &n_list_46_Cons)) {
      u_value v_a = u_field(l1965, 0);
      if (u_truth(u_mk_bool(1))) {
        e1964 = u_append(e1964, f_compile_46_type_key_1(v_a));
      }
      l1965 = u_field(l1965, 1);
    }
    static struct u_object l__41__1966 = U_LIT(")");
    t1948 = u_concat(&l__40__1961, u_concat(u_str_join(&l__32__42__32__1962, t1963), &l__41__1966));
  }
  if (!t1948) {
    static struct u_object l__63__1967 = U_LIT("?");
    t1948 = &l__63__1967;
  }
  if (!t1948) {
    u_no_case(s1947);
  }
  return t1948;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t1968 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1969 = &t1968;
  u_value l1970 = v_cs;
  while (u_is_con(l1970, &n_list_46_Cons)) {
    u_value v_c = u_field(l1970, 0);
    if (u_truth(u_mk_bool(1))) {
      e1969 = u_append(e1969, f_compile_46_fun_arities_1(v_c));
    }
    l1970 = u_field(l1970, 1);
  }
  u_value t1971 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1972 = &t1971;
  u_value l1973 = v_cs;
  while (u_is_con(l1973, &n_list_46_Cons)) {
    u_value v_c = u_field(l1973, 0);
    if (u_truth(u_mk_bool(1))) {
      e1972 = u_append(e1972, f_compile_46_constr_arities_1(v_c));
    }
    l1973 = u_field(l1973, 1);
  }
  u_value t1975 = f_list_46_concat_1(t1968);
  u_value t1976 = f_list_46_concat_1(t1971);
  u_value t1974;
  {
    u_value v_fs = t1975;
    u_value v_ks = t1976;
    t1974 = u_mk_con(&n_compile_46_Globals, 4, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()));
  }
  return t1974;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s1977 = v_fs;
  u_value t1978 = NULL;
  if ((!t1978) && u_is_con(s1977, &n_list_46_Nil)) {
    t1978 = v_m;
  }
  if ((!t1978) && u_is_con(s1977, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1977, 0), 0);
    u_value v_k = u_field(u_field(s1977, 0), 1);
    u_value v_rest = u_field(s1977, 1);
    u_value s1979 = f_map_46_get_2(v_f, v_m);
    u_value t1980 = NULL;
    if ((!t1980) && u_is_con(s1979, &n_option_46_Some)) {
      u_value v_ks = u_field(s1979, 0);
      t1980 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t1980) && u_is_con(s1979, &n_option_46_None)) {
      t1980 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t1980) {
      u_no_case(s1979);
    }
    t1978 = t1980;
  }
  if (!t1978) {
    u_no_case(s1977);
  }
  return t1978;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s1981 = v_gl;
  u_value t1982 = NULL;
  if ((!t1982) && u_is_con(s1981, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s1981, 2);
    u_value s1983 = f_map_46_get_2(v_n, v_fm);
    u_value t1984 = NULL;
    if ((!t1984) && u_is_con(s1983, &n_option_46_Some)) {
      u_value v_ks = u_field(s1983, 0);
      t1984 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t1984) && u_is_con(s1983, &n_option_46_None)) {
      t1984 = u_mk_bool(0);
    }
    if (!t1984) {
      u_no_case(s1983);
    }
    t1982 = t1984;
  }
  if (!t1982) {
    u_no_case(s1981);
  }
  return t1982;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s1985 = v_gl;
  u_value t1986 = NULL;
  if ((!t1986) && u_is_con(s1985, &n_compile_46_Globals)) {
    u_value v_km = u_field(s1985, 3);
    t1986 = f_map_46_get_2(v_n, v_km);
  }
  if (!t1986) {
    u_no_case(s1985);
  }
  return t1986;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s1987 = v_c;
  u_value t1988 = NULL;
  if ((!t1988) && u_is_con(s1987, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s1987, 0);
    u_value v_xss = u_field(s1987, 2);
    t1988 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1988) {
    t1988 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1988) {
    u_no_case(s1987);
  }
  return t1988;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s1989 = v_c;
  u_value t1990 = NULL;
  if ((!t1990) && u_is_con(s1989, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s1989, 2);
    u_value t1991 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1992 = &t1991;
    u_value l1993 = v_ks;
    while (u_is_con(l1993, &n_list_46_Cons)) {
      u_value v_k = u_field(l1993, 0);
      if (u_truth(u_mk_bool(1))) {
        e1992 = u_append(e1992, f_compile_46_constr_arity_1(v_k));
      }
      l1993 = u_field(l1993, 1);
    }
    t1990 = t1991;
  }
  if (!t1990) {
    t1990 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1990) {
    u_no_case(s1989);
  }
  return t1990;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s1994 = v_k;
  u_value t1995 = NULL;
  if ((!t1995) && u_is_con(s1994, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s1994, 0);
    u_value v_ts = u_field(s1994, 1);
    t1995 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t1995) {
    u_no_case(s1994);
  }
  return t1995;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s1996 = v_xs;
  u_value t1997 = NULL;
  if ((!t1997) && u_is_con(s1996, &n_list_46_Nil)) {
    t1997 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1997) && u_is_con(s1996, &n_list_46_Cons)) {
    u_value v_x = u_field(s1996, 0);
    u_value v_rest = u_field(s1996, 1);
    t1997 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t1997) {
    u_no_case(s1996);
  }
  return t1997;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_1998 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_1998, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_1999 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_1999, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s2000 = v_cs;
  u_value t2001 = NULL;
  if ((!t2001) && u_is_con(s2000, &n_list_46_Nil)) {
    t2001 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t2001) && (u_is_con(s2000, &n_list_46_Cons) && u_is_con(u_field(s2000, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s2000, 0);
    t2001 = v_d;
  }
  if ((!t2001) && u_is_con(s2000, &n_list_46_Cons)) {
    u_value v_d = u_field(s2000, 0);
    u_value v_rest = u_field(s2000, 1);
    static struct u_object l__38__38__2002 = U_LIT("&&");
    t2001 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__2002, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t2001) {
    u_no_case(s2000);
  }
  return t2001;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s2003 = v_l;
  u_value t2004 = NULL;
  if ((!t2004) && u_is_con(s2003, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_2005 = U_LIT("u_mk_tuple");
    t2004 = f_c_46_c_call_2(&l_u_mk_tuple_2005, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2004) && u_is_con(s2003, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s2003, 0);
    static struct u_object l_u_mk_bool_2006 = U_LIT("u_mk_bool");
    u_value t2007;
    if (u_truth(v_b)) {
      t2007 = u_mk_int(1);
    } else {
      t2007 = u_mk_int(0);
    }
    t2004 = f_c_46_c_call_2(&l_u_mk_bool_2006, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t2007), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2004) && u_is_con(s2003, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s2003, 0);
    u_value t2008 = u_lt(f_prelude_46__45__1(u_mk_int_str("1000000000000000000")), v_n);
    if (u_truth(t2008)) {
      t2008 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t2011;
    if (u_truth(t2008)) {
      static struct u_object l_u_mk_int_2009 = U_LIT("u_mk_int");
      t2011 = f_c_46_c_call_2(&l_u_mk_int_2009, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_2010 = U_LIT("u_mk_int_str");
      t2011 = f_c_46_c_call_2(&l_u_mk_int_str_2010, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t2004 = t2011;
  }
  if ((!t2004) && u_is_con(s2003, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s2003, 0);
    static struct u_object l_u_mk_lit_2012 = U_LIT("u_mk_lit");
    t2004 = f_c_46_c_call_2(&l_u_mk_lit_2012, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2004) && u_is_con(s2003, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s2003, 0);
    static struct u_object l_u_mk_char_2013 = U_LIT("u_mk_char");
    t2004 = f_c_46_c_call_2(&l_u_mk_char_2013, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2004) {
    u_no_case(s2003);
  }
  return t2004;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s2014 = v_p;
  u_value t2015 = NULL;
  if ((!t2015) && u_is_con(s2014, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s2014, 0);
    static struct u_object l_u_equals_2016 = U_LIT("u_equals");
    t2015 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_2016, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2015) && u_is_con(s2014, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s2014, 0);
    u_value v_ps = u_field(s2014, 1);
    static struct u_object l_u_is_con_2017 = U_LIT("u_is_con");
    t2015 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_2017, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t2015) && u_is_con(s2014, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2014, 0);
    t2015 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2015) {
    t2015 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2015) {
    u_no_case(s2014);
  }
  return t2015;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s2018 = v_ps;
  u_value t2019 = NULL;
  if ((!t2019) && u_is_con(s2018, &n_list_46_Nil)) {
    t2019 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2019) && u_is_con(s2018, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2018, 0), 0);
    u_value v_i = u_field(u_field(s2018, 0), 1);
    u_value v_rest = u_field(s2018, 1);
    t2019 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t2019) {
    u_no_case(s2018);
  }
  return t2019;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s2020 = v_p;
  u_value t2021 = NULL;
  if ((!t2021) && (u_is_con(s2020, &n_syntax_46_PVar) && u_is_con(u_field(s2020, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2020, 0), 0);
    t2021 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2021) && u_is_con(s2020, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2020, 1);
    t2021 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t2021) && u_is_con(s2020, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2020, 0);
    t2021 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2021) {
    t2021 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2021) {
    u_no_case(s2020);
  }
  return t2021;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s2022 = v_ps;
  u_value t2023 = NULL;
  if ((!t2023) && u_is_con(s2022, &n_list_46_Nil)) {
    t2023 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2023) && u_is_con(s2022, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2022, 0), 0);
    u_value v_i = u_field(u_field(s2022, 0), 1);
    u_value v_rest = u_field(s2022, 1);
    t2023 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t2023) {
    u_no_case(s2022);
  }
  return t2023;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s2024 = v_p;
  u_value t2025 = NULL;
  if ((!t2025) && (u_is_con(s2024, &n_syntax_46_PVar) && u_is_con(u_field(s2024, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2024, 0), 0);
    t2025 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2025) && u_is_con(s2024, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2024, 1);
    u_value t2026 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2027 = &t2026;
    u_value l2028 = v_ps;
    while (u_is_con(l2028, &n_list_46_Cons)) {
      u_value v_q = u_field(l2028, 0);
      if (u_truth(u_mk_bool(1))) {
        e2027 = u_append(e2027, f_compile_46_pattern_names_1(v_q));
      }
      l2028 = u_field(l2028, 1);
    }
    t2025 = f_list_46_concat_1(t2026);
  }
  if ((!t2025) && u_is_con(s2024, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2024, 0);
    u_value t2029 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2030 = &t2029;
    u_value l2031 = v_ps;
    while (u_is_con(l2031, &n_list_46_Cons)) {
      u_value v_q = u_field(l2031, 0);
      if (u_truth(u_mk_bool(1))) {
        e2030 = u_append(e2030, f_compile_46_pattern_names_1(v_q));
      }
      l2031 = u_field(l2031, 1);
    }
    t2025 = f_list_46_concat_1(t2029);
  }
  if (!t2025) {
    t2025 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2025) {
    u_no_case(s2024);
  }
  return t2025;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s2032 = v_e;
  u_value t2033 = NULL;
  if ((!t2033) && u_is_con(s2032, &n_syntax_46_ELit)) {
    t2033 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2033) && (u_is_con(s2032, &n_syntax_46_EVar) && u_is_con(u_field(s2032, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2032, 0), 0);
    u_value t2034;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t2034 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t2034 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t2033 = t2034;
  }
  if ((!t2033) && u_is_con(s2032, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s2032, 0);
    u_value v_args = u_field(s2032, 1);
    u_value t2035 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2036 = &t2035;
    u_value l2037 = v_args;
    while (u_is_con(l2037, &n_list_46_Cons)) {
      u_value v_a = u_field(l2037, 0);
      if (u_truth(u_mk_bool(1))) {
        e2036 = u_append(e2036, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2037 = u_field(l2037, 1);
    }
    t2033 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t2035));
  }
  if ((!t2033) && u_is_con(s2032, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s2032, 0);
    u_value v_b = u_field(s2032, 1);
    t2033 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t2033) && u_is_con(s2032, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s2032, 0);
    u_value v_b = u_field(s2032, 1);
    u_value t2038 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2039 = &t2038;
    u_value l2040 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l2040, &n_list_46_Cons)) {
      u_value v_a = u_field(l2040, 0);
      if (u_truth(u_mk_bool(1))) {
        e2039 = u_append(e2039, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2040 = u_field(l2040, 1);
    }
    t2033 = f_list_46__43__43__2(f_list_46_concat_1(t2038), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t2033) && u_is_con(s2032, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s2032, 0);
    u_value v_l = u_field(s2032, 1);
    u_value v_r = u_field(s2032, 2);
    t2033 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t2033) && u_is_con(s2032, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s2032, 0);
    u_value v_cs = u_field(s2032, 1);
    u_value t2041 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2042 = &t2041;
    u_value l2043 = v_cs;
    while (u_is_con(l2043, &n_list_46_Cons)) {
      u_value v_b = u_field(l2043, 0);
      if (u_truth(u_mk_bool(1))) {
        e2042 = u_append(e2042, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l2043 = u_field(l2043, 1);
    }
    t2033 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2041));
  }
  if ((!t2033) && u_is_con(s2032, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s2032, 0);
    u_value v_xs = u_field(s2032, 1);
    u_value v_ls = u_field(s2032, 2);
    u_value v_c = u_field(s2032, 3);
    u_value t2044 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2045 = &t2044;
    u_value l2046 = v_ls;
    while (u_is_con(l2046, &n_list_46_Cons)) {
      u_value v_l = u_field(l2046, 0);
      if (u_truth(u_mk_bool(1))) {
        e2045 = u_append(e2045, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2046 = u_field(l2046, 1);
    }
    t2033 = f_list_46__43__43__2(f_list_46_concat_1(t2044), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t2033) && u_is_con(s2032, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s2032, 0);
    u_value t2047 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2048 = &t2047;
    u_value l2049 = v_es;
    while (u_is_con(l2049, &n_list_46_Cons)) {
      u_value v_a = u_field(l2049, 0);
      if (u_truth(u_mk_bool(1))) {
        e2048 = u_append(e2048, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2049 = u_field(l2049, 1);
    }
    t2033 = f_list_46_concat_1(t2047);
  }
  if ((!t2033) && u_is_con(s2032, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s2032, 0);
    t2033 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if (!t2033) {
    u_no_case(s2032);
  }
  return t2033;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s2050 = v_b;
  u_value t2051 = NULL;
  if ((!t2051) && (u_is_con(s2050, &n_syntax_46_Branch) && u_is_con(u_field(s2050, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2050, 0);
    u_value v_g = u_field(u_field(s2050, 1), 0);
    u_value v_e = u_field(s2050, 2);
    t2051 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2051) && (u_is_con(s2050, &n_syntax_46_Branch) && u_is_con(u_field(s2050, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2050, 0);
    u_value v_e = u_field(s2050, 2);
    t2051 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2051) {
    u_no_case(s2050);
  }
  return t2051;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s2052 = v_xss;
  u_value t2053 = NULL;
  if ((!t2053) && u_is_con(s2052, &n_list_46_Nil)) {
    t2053 = v_b;
  }
  if ((!t2053) && u_is_con(s2052, &n_list_46_Cons)) {
    u_value v_xs = u_field(s2052, 0);
    u_value v_rest = u_field(s2052, 1);
    t2053 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t2053) {
    u_no_case(s2052);
  }
  return t2053;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value t2054 = u_concat(v_stem, u_str_show(f_compile_46_Gen_46_count_1(*r_g)));
    u_value t2055 = f_compile_46_Gen_46_count_58__61__2(*r_g, u_add(f_compile_46_Gen_46_count_1(*r_g), u_mk_int(1)));
    *r_x = t2054;
    *r_g = t2055;
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value t2056 = f_compile_46_Gen_46_lifted_58__61__2(*r_g, f_list_46__43__43__2(f_compile_46_Gen_46_lifted_1(*r_g), u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
    *r_g = t2056;
  }
}

void p_compile_46_global_value(u_value v_gl, u_value v_n, u_value v_t, u_value *r_r) {
  {
    if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
      {
        static struct u_object l_u_mk_closure_2057 = U_LIT("u_mk_closure");
        static struct u_object l_w__2058 = U_LIT("w_");
        u_value t2059 = f_c_46_c_call_2(&l_u_mk_closure_2057, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__2058, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        *r_r = t2059;
      }
    } else {
      {
        u_value s2060 = f_compile_46_fields_count_2(v_gl, v_n);
        int d2061 = 0;
        if ((!d2061) && (u_is_con(s2060, &n_option_46_Some) && u_equals(u_field(s2060, 0), u_mk_int(0)))) {
          d2061 = 1;
          {
            static struct u_object l_u_mk_con_2062 = U_LIT("u_mk_con");
            u_value t2063 = f_c_46_c_call_2(&l_u_mk_con_2062, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t2063;
          }
        }
        if ((!d2061) && u_is_con(s2060, &n_option_46_Some)) {
          d2061 = 1;
          {
            static struct u_object l_u_mk_closure_2064 = U_LIT("u_mk_closure");
            static struct u_object l_wc__2065 = U_LIT("wc_");
            u_value t2066 = f_c_46_c_call_2(&l_u_mk_closure_2064, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__2065, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t2066;
          }
        }
        if ((!d2061) && u_is_con(s2060, &n_option_46_None)) {
          d2061 = 1;
          {
            u_value s2067 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
            int d2068 = 0;
            if ((!d2068) && u_is_con(s2067, &n_option_46_Some)) {
              u_value v_f = u_field(u_field(s2067, 0), 0);
              d2068 = 1;
              {
                static struct u_object l_u_mk_closure_2069 = U_LIT("u_mk_closure");
                static struct u_object l_w_2070 = U_LIT("w");
                u_value t2071 = f_c_46_c_call_2(&l_u_mk_closure_2069, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_2070, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
                *r_r = t2071;
              }
            }
            if ((!d2068) && u_is_con(s2067, &n_option_46_None)) {
              d2068 = 1;
              {
                static struct u_object l_the_32_variable_32__2072 = U_LIT("the variable ");
                u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__2072, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
              }
            }
            if (!d2068) {
              u_no_case(s2067);
            }
          }
        }
        if (!d2061) {
          u_no_case(s2060);
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
    u_value s2073 = v_e;
    int d2074 = 0;
    if ((!d2074) && (u_is_con(s2073, &n_syntax_46_ELit) && u_is_con(u_field(s2073, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s2073, 0), 0);
      d2074 = 1;
      {
        static struct u_object l_l__2075 = U_LIT("l_");
        u_value t2076;
        if (u_truth(u_lt(u_str_size(v_s), u_mk_int(16)))) {
          t2076 = u_str_size(v_s);
        } else {
          t2076 = u_mk_int(16);
        }
        static struct u_object l___2077 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__2075, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), t2076))), &l___2077)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__2078 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_2079 = U_LIT("U_LIT");
        static struct u_object l__38__2080 = U_LIT("&");
        u_value t2081 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__2078), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_2079, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t2082 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2080, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t2081;
        *r_r = t2082;
      }
    }
    if ((!d2074) && u_is_con(s2073, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s2073, 0);
      d2074 = 1;
      {
        u_value t2083 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2084 = f_compile_46_literal_value_1(v_l);
        *r_ss = t2083;
        *r_r = t2084;
      }
    }
    if ((!d2074) && (u_is_con(s2073, &n_syntax_46_EVar) && u_is_con(u_field(s2073, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s2073, 0), 0);
      u_value v_ty = u_field(u_field(s2073, 0), 1);
      d2074 = 1;
      {
        u_value s2085 = f_list_46_lookup_2(v_n, v_env);
        int d2086 = 0;
        if ((!d2086) && u_is_con(s2085, &n_option_46_Some)) {
          u_value v_c = u_field(s2085, 0);
          d2086 = 1;
          {
            u_value t2087 = u_mk_con(&n_list_46_Nil, 0);
            u_value t2088 = v_c;
            *r_ss = t2087;
            *r_r = t2088;
          }
        }
        if ((!d2086) && u_is_con(s2085, &n_option_46_None)) {
          d2086 = 1;
          {
            p_compile_46_global_value(v_gl, v_n, v_ty, r_r);
            u_value t2089 = u_mk_con(&n_list_46_Nil, 0);
            *r_ss = t2089;
          }
        }
        if (!d2086) {
          u_no_case(s2085);
        }
      }
    }
    if ((!d2074) && (u_is_con(s2073, &n_syntax_46_EApp) && (u_is_con(u_field(s2073, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s2073, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s2073, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s2073, 0), 0), 1);
      u_value v_args = u_field(s2073, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d2074 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d2074) && u_is_con(s2073, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s2073, 0);
      u_value v_args = u_field(s2073, 1);
      d2074 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_2090 = U_LIT("u_apply");
        u_value t2091 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2092 = f_c_46_c_call_2(&l_u_apply_2090, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t2091;
        *r_r = t2092;
      }
    }
    if ((!d2074) && u_is_con(s2073, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s2073, 0);
      u_value v_b = u_field(s2073, 1);
      d2074 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2074) && u_is_con(s2073, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s2073, 0);
      u_value v_b = u_field(s2073, 1);
      d2074 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2093 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2093, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2094 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t2094;
        u_value t2095 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2095;
      }
    }
    if ((!d2074) && u_is_con(s2073, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s2073, 0);
      u_value v_l = u_field(s2073, 1);
      u_value v_rr = u_field(s2073, 2);
      d2074 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2096 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2096, &v_t, r_g);
        u_value t2097 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2097;
        u_value t2098 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2098;
      }
    }
    if ((!d2074) && u_is_con(s2073, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s2073, 0);
      u_value v_cases = u_field(s2073, 1);
      d2074 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2099 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2099, &v_d, r_g);
        static struct u_object l_t_2100 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2100, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_2101 = U_LIT("NULL");
        u_value t2102 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2101))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2102;
        static struct u_object l__33__2103 = U_LIT("!");
        static struct u_object l_u_no_case_2104 = U_LIT("u_no_case");
        u_value t2105 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2103, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2104, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2105;
        u_value t2106 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2106;
      }
    }
    if ((!d2074) && u_is_con(s2073, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s2073, 0);
      u_value v_xs = u_field(s2073, 1);
      u_value v_lists = u_field(s2073, 2);
      u_value v_c = u_field(s2073, 3);
      d2074 = 1;
      {
        static struct u_object l_t_2107 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2107, &v_t, r_g);
        static struct u_object l_e_2108 = U_LIT("e");
        p_compile_46_gen_name(&l_e_2108, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_2109 = U_LIT("u_mk_con");
        static struct u_object l__38__2110 = U_LIT("&");
        u_value t2111 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_2109, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__2110, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t2111;
        u_value t2112 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2112;
      }
    }
    if ((!d2074) && u_is_con(s2073, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s2073, 0);
      d2074 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_2113 = U_LIT("u_mk_tuple");
        u_value t2114 = f_c_46_c_call_2(&l_u_mk_tuple_2113, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t2114;
      }
    }
    if ((!d2074) && u_is_con(s2073, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s2073, 0);
      u_value v_ty = u_field(s2073, 1);
      d2074 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2115 = U_LIT("u_raise");
        static struct u_object l_NULL_2116 = U_LIT("NULL");
        u_value t2117 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2115, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        u_value t2118 = u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2116);
        *r_ss = t2117;
        *r_r = t2118;
      }
    }
    if (!d2074) {
      u_no_case(s2073);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s2119 = v_es;
    int d2120 = 0;
    if ((!d2120) && u_is_con(s2119, &n_list_46_Nil)) {
      d2120 = 1;
      {
        u_value t2121 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2122 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2121;
        *r_rs = t2122;
      }
    }
    if ((!d2120) && u_is_con(s2119, &n_list_46_Cons)) {
      u_value v_e = u_field(s2119, 0);
      u_value v_rest = u_field(s2119, 1);
      d2120 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t2123 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2124 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t2123;
        *r_rs = t2124;
      }
    }
    if (!d2120) {
      u_no_case(s2119);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s2125 = v_rs;
    int d2126 = 0;
    if ((!d2126) && u_is_con(s2125, &n_list_46_Nil)) {
      d2126 = 1;
      {
        u_value t2127 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2127;
      }
    }
    if ((!d2126) && u_is_con(s2125, &n_list_46_Cons)) {
      u_value v_r = u_field(s2125, 0);
      u_value v_rest = u_field(s2125, 1);
      d2126 = 1;
      {
        static struct u_object l_t_2128 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2128, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t2129 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t2129;
      }
    }
    if (!d2126) {
      u_no_case(s2125);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s2130 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2131 = NULL;
  if ((!t2131) && (u_is_con(u_field(s2130, 0), &n_list_46_Cons) && (u_is_con(u_field(s2130, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2130, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2130, 0), 0);
    u_value v_more = u_field(u_field(s2130, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2130, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2130, 1), 1);
    t2131 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t2131) {
    t2131 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2131) {
    u_no_case(s2130);
  }
  return t2131;
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
    u_value s2132 = v_args;
    int d2133 = 0;
    if ((!d2133) && (u_is_con(s2132, &n_list_46_Cons) && (u_is_con(u_field(s2132, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2132, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s2132, 0);
      u_value v_b = u_field(u_field(s2132, 1), 0);
      static struct u_object l_and_2140 = U_LIT("and");
      u_value t2142 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2140));
      if (!u_truth(t2142)) {
        static struct u_object l_or_2141 = U_LIT("or");
        t2142 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_2141));
      }
      if (u_truth(t2142)) {
        d2133 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_2134 = U_LIT("t");
          p_compile_46_gen_name(&l_t_2134, &v_t, r_g);
          static struct u_object l_and_2135 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2135)))) {
            {
              u_value t2136 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2136;
            }
          } else {
            {
              static struct u_object l__33__2137 = U_LIT("!");
              u_value t2138 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2137, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2138;
            }
          }
          u_value t2139 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t2139;
        }
      }
    }
    if (!d2133) {
      d2133 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t2143 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t2143)) {
          t2143 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t2143)) {
          {
            u_value t2144 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t2144;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_2145 = U_LIT("u_mk_con");
              u_value t2146 = f_c_46_c_call_2(&l_u_mk_con_2145, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t2146;
            }
          } else {
            {
              u_value s2147 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
              int d2148 = 0;
              if ((!d2148) && u_is_con(s2147, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s2147, 0), 0);
                u_value v_k = u_field(u_field(s2147, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d2148 = 1;
                  {
                    u_value t2149 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t2149;
                  }
                }
              }
              if (!d2148) {
                d2148 = 1;
                {
                  p_compile_46_global_value(v_gl, v_n, v_ty, &v_f);
                  static struct u_object l_u_apply_2150 = U_LIT("u_apply");
                  u_value t2151 = f_c_46_c_call_2(&l_u_apply_2150, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t2151;
                }
              }
              if (!d2148) {
                u_no_case(s2147);
              }
            }
          }
        }
      }
    }
    if (!d2133) {
      u_no_case(s2132);
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
    u_value t2152 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2153 = &t2152;
    u_value l2154 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l2154, &n_list_46_Cons)) {
      u_value v_x = u_field(l2154, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2153 = u_append(e2153, v_x);
      }
      l2154 = u_field(l2154, 1);
    }
    u_value t2155 = t2152;
    v_caps = t2155;
    static struct u_object l_lam_2156 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_2156, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_2157 = U_LIT("env");
    static struct u_object l_args_2158 = U_LIT("args");
    u_value t2159 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2157), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_2158));
    v_decls = t2159;
    static struct u_object l_env_2160 = U_LIT("env");
    static struct u_object l_args_2161 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2160), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2161), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_2162 = U_LIT("u_mk_closure");
    u_value t2163 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2164 = &t2163;
    u_value l2165 = v_caps;
    while (u_is_con(l2165, &n_list_46_Cons)) {
      u_value v_x = u_field(l2165, 0);
      if (u_truth(u_mk_bool(1))) {
        e2164 = u_append(e2164, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2165 = u_field(l2165, 1);
    }
    u_value t2166 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2167 = f_c_46_c_call_2(&l_u_mk_closure_2162, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2163));
    *r_ss = t2166;
    *r_r = t2167;
  }
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s2168 = v_xs;
  u_value t2169 = NULL;
  if ((!t2169) && u_is_con(s2168, &n_list_46_Nil)) {
    t2169 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2169) && u_is_con(s2168, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2168, 0), 0);
    u_value v_i = u_field(u_field(s2168, 0), 1);
    u_value v_rest = u_field(s2168, 1);
    t2169 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t2169) {
    u_no_case(s2168);
  }
  return t2169;
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
    u_value s2170 = v_cases;
    int d2171 = 0;
    if ((!d2171) && u_is_con(s2170, &n_list_46_Nil)) {
      d2171 = 1;
      {
        u_value t2172 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2172;
      }
    }
    if ((!d2171) && (u_is_con(s2170, &n_list_46_Cons) && u_is_con(u_field(s2170, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2170, 0), 0);
      u_value v_guard = u_field(u_field(s2170, 0), 1);
      u_value v_e = u_field(u_field(s2170, 0), 2);
      u_value v_more = u_field(s2170, 1);
      d2171 = 1;
      {
        u_value t2173 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2173;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2174 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t2174;
        u_value s2175 = v_guard;
        int d2176 = 0;
        if ((!d2176) && u_is_con(s2175, &n_option_46_Some)) {
          u_value v_c = u_field(s2175, 0);
          d2176 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2177 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2177;
          }
        }
        if ((!d2176) && u_is_con(s2175, &n_option_46_None)) {
          d2176 = 1;
          {
          }
        }
        if (!d2176) {
          u_no_case(s2175);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2178 = U_LIT("!");
        u_value t2179 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2178, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2179;
      }
    }
    if (!d2171) {
      u_no_case(s2170);
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
    u_value s2180 = u_mk_tuple(2, v_xs, v_lists);
    int d2181 = 0;
    if ((!d2181) && (u_is_con(u_field(s2180, 0), &n_list_46_Cons) && u_is_con(u_field(s2180, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2180, 0), 0);
      u_value v_more = u_field(u_field(s2180, 0), 1);
      u_value v_list = u_field(u_field(s2180, 1), 0);
      u_value v_others = u_field(u_field(s2180, 1), 1);
      d2181 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2182 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2182, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2183 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2183;
        static struct u_object l_u_is_con_2184 = U_LIT("u_is_con");
        u_value t2185 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2184, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2185;
      }
    }
    if (!d2181) {
      d2181 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2186 = U_LIT("u_append");
        u_value t2187 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2186, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2187;
      }
    }
    if (!d2181) {
      u_no_case(s2180);
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
    u_value s2188 = v_s;
    int d2189 = 0;
    if ((!d2189) && u_is_con(s2188, &n_syntax_46_SSkip)) {
      d2189 = 1;
      {
        u_value t2190 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2190;
      }
    }
    if ((!d2189) && u_is_con(s2188, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2188, 0);
      d2189 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2191 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2191;
      }
    }
    if ((!d2189) && u_is_con(s2188, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2188, 0);
      u_value v_body = u_field(s2188, 1);
      d2189 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t2192 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2193 = &t2192;
        u_value l2194 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2194, &n_list_46_Cons)) {
          u_value v_x = u_field(l2194, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2195 = U_LIT("NULL");
            e2193 = u_append(e2193, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2195))));
          }
          l2194 = u_field(l2194, 1);
        }
        u_value t2196 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2192, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2196;
      }
    }
    if ((!d2189) && u_is_con(s2188, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2188, 0);
      u_value v_l = u_field(s2188, 1);
      u_value v_r = u_field(s2188, 2);
      d2189 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2197 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2197;
      }
    }
    if ((!d2189) && u_is_con(s2188, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2188, 0);
      u_value v_b = u_field(s2188, 1);
      d2189 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2198 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2198, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2199 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2199;
      }
    }
    if ((!d2189) && u_is_con(s2188, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2188, 0);
      u_value v_ins = u_field(s2188, 1);
      u_value v_outs = u_field(s2188, 2);
      u_value v_refs = u_field(s2188, 3);
      d2189 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        u_value t2200 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2201 = &t2200;
        u_value l2202 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2202, &n_list_46_Cons)) {
          u_value v_x = u_field(l2202, 0);
          if (u_truth(u_mk_bool(1))) {
            e2201 = u_append(e2201, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
          }
          l2202 = u_field(l2202, 1);
        }
        u_value t2203 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2200))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2203;
      }
    }
    if ((!d2189) && u_is_con(s2188, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2188, 0);
      u_value v_cases = u_field(s2188, 1);
      d2189 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2204 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2204, &v_d, r_g);
        static struct u_object l_d_2205 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2205, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2206 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2206;
        static struct u_object l__33__2207 = U_LIT("!");
        static struct u_object l_u_no_case_2208 = U_LIT("u_no_case");
        u_value t2209 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2207, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2208, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2209;
      }
    }
    if ((!d2189) && (u_is_con(s2188, &n_syntax_46_SAssert) && (u_is_con(u_field(s2188, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2188, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2188, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2188, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2188, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2188, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2188, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2188, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2188, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2188, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2188, 0), 1), 1), 0);
      d2189 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2210 = U_LIT("u_assert_equal");
        u_value t2211 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2210, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2211;
      }
    }
    if ((!d2189) && u_is_con(s2188, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2188, 0);
      d2189 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2212 = U_LIT("u_assert_true");
        u_value t2213 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2212, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2213;
      }
    }
    if ((!d2189) && u_is_con(s2188, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2188, 0);
      u_value v_ty = u_field(s2188, 1);
      d2189 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2214 = U_LIT("u_raise");
        u_value t2215 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2214, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2215;
      }
    }
    if ((!d2189) && u_is_con(s2188, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2188, 0);
      u_value v_hs = u_field(s2188, 1);
      d2189 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2216 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2216, &v_d, r_g);
        static struct u_object l_d_2217 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2217, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2218 = U_LIT("u_pop");
        static struct u_object l__33__2219 = U_LIT("!");
        static struct u_object l_u_reraise_2220 = U_LIT("u_reraise");
        u_value t2221 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2218, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2219, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2220, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2221;
        static struct u_object l_struct_32_u_handler_2222 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2223 = U_LIT("u_push");
        static struct u_object l__61__61__2224 = U_LIT("==");
        static struct u_object l_setjmp_2225 = U_LIT("setjmp");
        static struct u_object l_env_2226 = U_LIT("env");
        static struct u_object l_u_pop_2227 = U_LIT("u_pop");
        u_value t2228 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2222)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2223, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2224, f_c_46_c_call_2(&l_setjmp_2225, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2226), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2227, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2228;
      }
    }
    if (!d2189) {
      u_no_case(s2188);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2229 = v_xs;
    int d2230 = 0;
    if ((!d2230) && u_is_con(s2229, &n_list_46_Nil)) {
      d2230 = 1;
      {
        u_value t2231 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2231;
      }
    }
    if ((!d2230) && u_is_con(s2229, &n_list_46_Cons)) {
      u_value v_x = u_field(s2229, 0);
      u_value v_rest = u_field(s2229, 1);
      d2230 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2232 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2232;
      }
    }
    if (!d2230) {
      u_no_case(s2229);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2233 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2234 = NULL;
  if ((!t2234) && (u_is_con(u_field(s2233, 0), &n_list_46_Cons) && (u_is_con(u_field(s2233, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2233, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2233, 0), 0);
    u_value v_more = u_field(u_field(s2233, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2233, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2233, 1), 1);
    t2234 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2234) {
    t2234 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2234) {
    u_no_case(s2233);
  }
  return t2234;
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
    static struct u_object l_u_caught_2235 = U_LIT("u_caught");
    u_value t2236 = f_c_46_c_call_2(&l_u_caught_2235, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2236;
    u_value s2237 = v_hs;
    int d2238 = 0;
    if ((!d2238) && u_is_con(s2237, &n_list_46_Nil)) {
      d2238 = 1;
      {
        u_value t2239 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2239;
      }
    }
    if ((!d2238) && (u_is_con(s2237, &n_list_46_Cons) && u_is_con(u_field(s2237, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2237, 0), 0);
      u_value v_p = u_field(u_field(s2237, 0), 1);
      u_value v_guard = u_field(u_field(s2237, 0), 2);
      u_value v_s = u_field(u_field(s2237, 0), 3);
      u_value v_more = u_field(s2237, 1);
      d2238 = 1;
      {
        u_value t2240 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2240;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2241 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2241;
        u_value s2242 = v_guard;
        int d2243 = 0;
        if ((!d2243) && u_is_con(s2242, &n_option_46_Some)) {
          u_value v_c = u_field(s2242, 0);
          d2243 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2244 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2244;
          }
        }
        if ((!d2243) && u_is_con(s2242, &n_option_46_None)) {
          d2243 = 1;
          {
          }
        }
        if (!d2243) {
          u_no_case(s2242);
        }
        u_value s2245 = v_p;
        int d2246 = 0;
        if ((!d2246) && u_is_con(s2245, &n_syntax_46_PWild)) {
          d2246 = 1;
          {
            u_value t2247 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2247;
          }
        }
        if (!d2246) {
          d2246 = 1;
          {
            static struct u_object l_u_has_type_2248 = U_LIT("u_has_type");
            u_value t2249 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2248, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2249;
          }
        }
        if (!d2246) {
          u_no_case(s2245);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2250 = U_LIT("!");
        u_value t2251 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2250, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2251;
      }
    }
    if (!d2238) {
      u_no_case(s2237);
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
    u_value s2252 = v_cases;
    int d2253 = 0;
    if ((!d2253) && u_is_con(s2252, &n_list_46_Nil)) {
      d2253 = 1;
      {
        u_value t2254 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2254;
      }
    }
    if ((!d2253) && (u_is_con(s2252, &n_list_46_Cons) && u_is_con(u_field(s2252, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2252, 0), 0);
      u_value v_guard = u_field(u_field(s2252, 0), 1);
      u_value v_s = u_field(u_field(s2252, 0), 2);
      u_value v_more = u_field(s2252, 1);
      d2253 = 1;
      {
        u_value t2255 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2255;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2256 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2256;
        u_value s2257 = v_guard;
        int d2258 = 0;
        if ((!d2258) && u_is_con(s2257, &n_option_46_Some)) {
          u_value v_c = u_field(s2257, 0);
          d2258 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2259 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2259;
          }
        }
        if ((!d2258) && u_is_con(s2257, &n_option_46_None)) {
          d2258 = 1;
          {
          }
        }
        if (!d2258) {
          u_no_case(s2257);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2260 = U_LIT("!");
        u_value t2261 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2260, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2261;
      }
    }
    if (!d2253) {
      u_no_case(s2252);
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
    u_value s2262 = v_c;
    int d2263 = 0;
    if ((!d2263) && u_is_con(s2262, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2262, 0);
      u_value v_xss = u_field(s2262, 2);
      u_value v_b = u_field(s2262, 4);
      d2263 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2264 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2265 = &t2264;
        u_value l2266 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2266, &n_list_46_Cons)) {
          u_value v_x = u_field(l2266, 0);
          if (u_truth(u_mk_bool(1))) {
            e2265 = u_append(e2265, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2266 = u_field(l2266, 1);
        }
        u_value t2267 = t2264;
        v_params = t2267;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2268 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2268;
          }
        } else {
          {
            u_value t2269 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2269;
          }
        }
        u_value t2270 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2271 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2270;
        *r_ms = t2271;
      }
    }
    if ((!d2263) && u_is_con(s2262, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2262, 0);
      u_value v_ins = u_field(s2262, 2);
      u_value v_outs = u_field(s2262, 3);
      u_value v_refs = u_field(s2262, 4);
      u_value v_body = u_field(s2262, 5);
      d2263 = 1;
      {
        u_value t2272 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2273 = &t2272;
        u_value l2274 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2274, &n_list_46_Cons)) {
          u_value v_x = u_field(l2274, 0);
          if (u_truth(u_mk_bool(1))) {
            e2273 = u_append(e2273, f_compile_46_pointer_1(v_x));
          }
          l2274 = u_field(l2274, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2272), v_body, &v_bs, r_g);
        u_value t2275 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2276 = &t2275;
        u_value l2277 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2277, &n_list_46_Cons)) {
          u_value v_x = u_field(l2277, 0);
          if (u_truth(u_mk_bool(1))) {
            e2276 = u_append(e2276, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2277 = u_field(l2277, 1);
        }
        u_value t2278 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2279 = &t2278;
        u_value l2280 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2280, &n_list_46_Cons)) {
          u_value v_x = u_field(l2280, 0);
          if (u_truth(u_mk_bool(1))) {
            e2279 = u_append(e2279, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2280 = u_field(l2280, 1);
        }
        u_value t2281 = f_list_46__43__43__2(t2275, t2278);
        v_params = t2281;
        u_value t2282 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2283 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2282;
        *r_ms = t2283;
      }
    }
    if ((!d2263) && u_is_con(s2262, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s2262, 0);
      d2263 = 1;
      {
        p_compile_46_compile_expr(v_gl, u_mk_con(&n_list_46_Nil, 0), v_e, &v_bs, &v_rb, r_g);
        static struct u_object l_u_print_line_2284 = U_LIT("u_print_line");
        u_value t2285 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2286 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_line_2284, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2285;
        *r_ms = t2286;
      }
    }
    if ((!d2263) && (u_is_con(s2262, &n_syntax_46_CRun) && u_is_con(u_field(s2262, 1), &n_syntax_46_SBlock))) {
      u_value v_xs = u_field(u_field(s2262, 1), 0);
      u_value v_body = u_field(u_field(s2262, 1), 1);
      d2263 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_body, &v_bs, r_g);
        u_value t2287 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2288 = &t2287;
        u_value l2289 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2289, &n_list_46_Cons)) {
          u_value v_x = u_field(l2289, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2290 = U_LIT("NULL");
            e2288 = u_append(e2288, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2290))));
          }
          l2289 = u_field(l2289, 1);
        }
        u_value t2291 = t2287;
        v_copy = t2291;
        u_value t2292 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2293 = &t2292;
        u_value l2294 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2294, &n_list_46_Cons)) {
          u_value v_x = u_field(l2294, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_u_print_binding_2295 = U_LIT("u_print_binding");
            e2293 = u_append(e2293, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_binding_2295, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_syntax_46_Name_46_base_1(v_x)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x)), u_mk_con(&n_list_46_Nil, 0))))));
          }
          l2294 = u_field(l2294, 1);
        }
        u_value t2296 = t2292;
        v_back = t2296;
        u_value t2297 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2298 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_copy, f_list_46__43__43__2(v_bs, v_back))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2297;
        *r_ms = t2298;
      }
    }
    if ((!d2263) && u_is_con(s2262, &n_syntax_46_CRun)) {
      u_value v_body = u_field(s2262, 1);
      d2263 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2299 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2300 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2299;
        *r_ms = t2300;
      }
    }
    if ((!d2263) && u_is_con(s2262, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s2262, 1);
      d2263 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2301 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2302 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2301;
        *r_ms = t2302;
      }
    }
    if (!d2263) {
      d2263 = 1;
      {
        u_value t2303 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2304 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2303;
        *r_ms = t2304;
      }
    }
    if (!d2263) {
      u_no_case(s2262);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2305 = v_cs;
    int d2306 = 0;
    if ((!d2306) && u_is_con(s2305, &n_list_46_Nil)) {
      d2306 = 1;
      {
        u_value t2307 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2308 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2307;
        *r_ms = t2308;
      }
    }
    if ((!d2306) && u_is_con(s2305, &n_list_46_Cons)) {
      u_value v_c = u_field(s2305, 0);
      u_value v_rest = u_field(s2305, 1);
      d2306 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_rest, &v_d2, &v_m2, r_g);
        u_value t2309 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2310 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2309;
        *r_ms = t2310;
      }
    }
    if (!d2306) {
      u_no_case(s2305);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_handler_2311 = U_LIT("handler");
  u_value t2338 = u_mk_con(&n_c_46_CeVar, 1, &l_handler_2311);
  u_value t2337;
  {
    u_value v_h = t2338;
    static struct u_object l_static_32_u_value_2312 = U_LIT("static u_value");
    static struct u_object l_cache_2313 = U_LIT("cache");
    static struct u_object l_NULL_2314 = U_LIT("NULL");
    static struct u_object l__61__61__2315 = U_LIT("==");
    static struct u_object l_cache_2316 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2317 = U_LIT("U_COMPUTING");
    static struct u_object l_u_fail_2318 = U_LIT("u_fail");
    static struct u_object l_recursive_32_initia_2319 = U_LIT("recursive initialization of ");
    static struct u_object l_cache_2320 = U_LIT("cache");
    static struct u_object l_cache_2321 = U_LIT("cache");
    static struct u_object l_cache_2322 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2323 = U_LIT("U_COMPUTING");
    static struct u_object l_struct_32_u_handler_2324 = U_LIT("struct u_handler");
    static struct u_object l_handler_2325 = U_LIT("handler");
    static struct u_object l_u_push_2326 = U_LIT("u_push");
    static struct u_object l__33__61__2327 = U_LIT("!=");
    static struct u_object l_setjmp_2328 = U_LIT("setjmp");
    static struct u_object l_env_2329 = U_LIT("env");
    static struct u_object l_u_pop_2330 = U_LIT("u_pop");
    static struct u_object l_cache_2331 = U_LIT("cache");
    static struct u_object l_NULL_2332 = U_LIT("NULL");
    static struct u_object l_u_reraise_2333 = U_LIT("u_reraise");
    static struct u_object l_u_pop_2334 = U_LIT("u_pop");
    static struct u_object l_cache_2335 = U_LIT("cache");
    static struct u_object l_cache_2336 = U_LIT("cache");
    t2337 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2312), &l_cache_2313, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2314))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2315, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2316), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2317)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2318, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2319, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2320), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2321))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2322), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2323)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2324)), &l_handler_2325, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2326, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__33__61__2327, f_c_46_c_call_2(&l_setjmp_2328, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, v_h, &l_env_2329), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2330, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2331), u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2332)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2333, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2334, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2335), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2336))), u_mk_con(&n_list_46_Nil, 0))))));
  }
  return t2337;
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2339 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2340 = &t2339;
  u_value l2341 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l2341, &n_list_46_Cons)) {
    u_value v_p = u_field(l2341, 0);
    if (u_truth(u_mk_bool(1))) {
      e2340 = u_append(e2340, f_compile_46_fun_wrapper_1(v_p));
    }
    l2341 = u_field(l2341, 1);
  }
  u_value t2342 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2343 = &t2342;
  u_value l2344 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l2344, &n_list_46_Cons)) {
    u_value v_p = u_field(l2344, 0);
    if (u_truth(u_mk_bool(1))) {
      e2343 = u_append(e2343, f_compile_46_constr_wrapper_1(v_p));
    }
    l2344 = u_field(l2344, 1);
  }
  u_value t2345 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2346 = &t2345;
  u_value l2347 = f_list_46_seconds_1(f_compile_46_builtin_functions_0());
  while (u_is_con(l2347, &n_list_46_Cons)) {
    u_value v_p = u_field(l2347, 0);
    if (u_truth(u_mk_bool(1))) {
      e2346 = u_append(e2346, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2347 = u_field(l2347, 1);
  }
  return f_list_46__43__43__2(t2339, f_list_46__43__43__2(f_list_46_concat_1(t2342), t2345));
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2348 = v_p;
  u_value t2349 = NULL;
  if (!t2349) {
    u_value v_n = u_field(s2348, 0);
    u_value v_k = u_field(s2348, 1);
    static struct u_object l_w__2350 = U_LIT("w_");
    t2349 = f_compile_46_wrapper_3(u_concat(&l_w__2350, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2349) {
    u_no_case(s2348);
  }
  return t2349;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2351 = v_p;
  u_value t2352 = NULL;
  if (!t2352) {
    u_value v_f = u_field(s2351, 0);
    u_value v_k = u_field(s2351, 1);
    static struct u_object l_w_2353 = U_LIT("w");
    t2352 = f_compile_46_wrapper_3(u_concat(&l_w_2353, v_f), v_f, v_k);
  }
  if (!t2352) {
    u_no_case(s2351);
  }
  return t2352;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2354 = U_LIT("env");
  static struct u_object l_args_2355 = U_LIT("args");
  u_value t2356 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2357 = &t2356;
  u_value l2358 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2358, &n_list_46_Cons)) {
    u_value v_i = u_field(l2358, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2359 = U_LIT("args");
      e2357 = u_append(e2357, f_compile_46_index_2(&l_args_2359, v_i));
    }
    l2358 = u_field(l2358, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2354), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2355), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2356))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2360 = v_p;
  u_value t2361 = NULL;
  if ((!t2361) && u_equals(u_field(s2360, 1), u_mk_int(0))) {
    t2361 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2361) {
    u_value v_n = u_field(s2360, 0);
    u_value v_k = u_field(s2360, 1);
    static struct u_object l_wc__2362 = U_LIT("wc_");
    static struct u_object l_env_2363 = U_LIT("env");
    static struct u_object l_args_2364 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2365 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2366 = U_LIT("args");
    t2361 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2362, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2363), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2364), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2365, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2366), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2361) {
    u_no_case(s2360);
  }
  return t2361;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2367;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2367 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2367 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2367;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2368 = v_d;
  u_value t2369 = NULL;
  if ((!t2369) && u_is_con(s2368, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2368, 0);
    u_value v_f = u_field(s2368, 1);
    u_value v_ps = u_field(s2368, 2);
    t2369 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2369) {
    t2369 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2369) {
    u_no_case(s2368);
  }
  return t2369;
}

void p_compile_46_compile_program(u_value v_q, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2370 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2370;
    u_value t2371 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t2371;
    p_compile_46_compile_cmds(v_gl, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value t2372 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, f_compile_46_Gen_46_lifted_1(v_g)));
    v_fs = t2372;
    static struct u_object l_ulang_46_h_2373 = U_LIT("ulang.h");
    u_value t2374 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2375 = &t2374;
    u_value l2376 = v_fs;
    while (u_is_con(l2376, &n_list_46_Cons)) {
      u_value v_d = u_field(l2376, 0);
      if (u_truth(u_mk_bool(1))) {
        e2375 = u_append(e2375, f_compile_46_prototype_1(v_d));
      }
      l2376 = u_field(l2376, 1);
    }
    static struct u_object l_main_2377 = U_LIT("main");
    static struct u_object l_argc_2378 = U_LIT("argc");
    static struct u_object l_char_2379 = U_LIT("char");
    static struct u_object l_argv_2380 = U_LIT("argv");
    static struct u_object l_u_args_2381 = U_LIT("u_args");
    static struct u_object l_argc_2382 = U_LIT("argc");
    static struct u_object l_argv_2383 = U_LIT("argv");
    u_value t2384 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2373), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2374), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2377, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2378), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2379))), &l_argv_2380), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2381, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2382), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2383), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2384;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_target) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2385 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2385, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2386 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2386, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q);
    p_compile_46_compile_program(v_q, &v_f);
    p_os_write(v_target, f_c_46_c_file_1(v_f));
  }
}

u_value lam348(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam349(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam351(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam361(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_f = args[0];
  return f_ctx_46_is_fun_2(f_check_46_State_46_ctx_1(v_s), v_f);
}

u_value lam368(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam587(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_t = args[0];
  return f_ctx_46_is_sort_2(f_check_46_State_46_ctx_1(v_s), v_t);
}

u_value lam696(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam699(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam761(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_q = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_q);
}

u_value lam1038(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  {
    u_value v_args = NULL;
    p_os_args(&v_args);
    u_value s2387 = v_args;
    int d2388 = 0;
    if ((!d2388) && u_is_con(s2387, &n_list_46_Nil)) {
      d2388 = 1;
      {
      }
    }
    if ((!d2388) && (u_is_con(s2387, &n_list_46_Cons) && (u_is_con(u_field(s2387, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2387, 1), 1), &n_list_46_Nil)))) {
      u_value v_source = u_field(s2387, 0);
      u_value v_target = u_field(u_field(s2387, 1), 0);
      d2388 = 1;
      {
        p_compile_46_compile_file(v_source, v_target);
      }
    }
    if (!d2388) {
      d2388 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_sou_2389 = U_LIT("usage: ulang source.u target.c");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_sou_2389), "os.OsError");
      }
    }
    if (!d2388) {
      u_no_case(s2387);
    }
    u_print_binding("args", v_args);
  }
  return 0;
}
