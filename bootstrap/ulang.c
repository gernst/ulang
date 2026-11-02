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

u_value w_f_parse_46_expr_prefix_2(u_value *env, u_value *args);

u_value w_f_parse_46_expr_op2_3(u_value *env, u_value *args);

u_value w_f_parse_46_cons_exprs_2(u_value *env, u_value *args);

u_value w_f_parse_46_starts_expr_2(u_value *env, u_value *args);

u_value w_f_parse_46_at_update_1(u_value *env, u_value *args);

u_value w_f_parse_46_pattern_vars_1(u_value *env, u_value *args);

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

void p_parse_46_last_case(u_value *r_ts);

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

u_value lam344(u_value *env, u_value *args);

u_value lam345(u_value *env, u_value *args);

u_value lam347(u_value *env, u_value *args);

u_value lam357(u_value *env, u_value *args);

u_value lam364(u_value *env, u_value *args);

u_value lam583(u_value *env, u_value *args);

u_value lam692(u_value *env, u_value *args);

u_value lam695(u_value *env, u_value *args);

u_value lam757(u_value *env, u_value *args);

u_value lam1034(u_value *env, u_value *args);

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
    t152 = u_mk_int(-1);
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
    u_value s155 = u_compare(v_k, v_j);
    u_value t156 = NULL;
    if ((!t156) && u_equals(s155, u_mk_int(-1))) {
      t156 = f_map_46_get_2(v_k, v_l);
    }
    if ((!t156) && u_equals(s155, u_mk_int(1))) {
      t156 = f_map_46_get_2(v_k, v_r);
    }
    if (!t156) {
      t156 = u_mk_con(&n_option_46_Some, 1, v_v);
    }
    if (!t156) {
      u_no_case(s155);
    }
    t154 = t156;
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
  u_value s157 = v_m;
  u_value t158 = NULL;
  if ((!t158) && u_is_con(s157, &n_map_46_MLeaf)) {
    t158 = u_mk_con(&n_map_46_MNode, 5, u_mk_con(&n_map_46_MLeaf, 0), v_k, v_v, u_mk_con(&n_map_46_MLeaf, 0), u_mk_int(1));
  }
  if ((!t158) && u_is_con(s157, &n_map_46_MNode)) {
    u_value v_l = u_field(s157, 0);
    u_value v_j = u_field(s157, 1);
    u_value v_w = u_field(s157, 2);
    u_value v_r = u_field(s157, 3);
    u_value v_h = u_field(s157, 4);
    u_value s159 = u_compare(v_k, v_j);
    u_value t160 = NULL;
    if ((!t160) && u_equals(s159, u_mk_int(-1))) {
      t160 = f_map_46_balance_4(f_map_46_put_3(v_k, v_v, v_l), v_j, v_w, v_r);
    }
    if ((!t160) && u_equals(s159, u_mk_int(1))) {
      t160 = f_map_46_balance_4(v_l, v_j, v_w, f_map_46_put_3(v_k, v_v, v_r));
    }
    if (!t160) {
      t160 = u_mk_con(&n_map_46_MNode, 5, v_l, v_k, v_v, v_r, v_h);
    }
    if (!t160) {
      u_no_case(s159);
    }
    t158 = t160;
  }
  if (!t158) {
    u_no_case(s157);
  }
  return t158;
}

u_value f_map_46_entries_1(u_value v_m) {
  return f_map_46_entries_onto_2(v_m, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_map_46_entries_onto_2(u_value v_m, u_value v_rest) {
  u_value s161 = v_m;
  u_value t162 = NULL;
  if ((!t162) && u_is_con(s161, &n_map_46_MLeaf)) {
    t162 = v_rest;
  }
  if ((!t162) && u_is_con(s161, &n_map_46_MNode)) {
    u_value v_l = u_field(s161, 0);
    u_value v_k = u_field(s161, 1);
    u_value v_v = u_field(s161, 2);
    u_value v_r = u_field(s161, 3);
    t162 = f_map_46_entries_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_v), f_map_46_entries_onto_2(v_r, v_rest)));
  }
  if (!t162) {
    u_no_case(s161);
  }
  return t162;
}

u_value f_map_46_size_1(u_value v_m) {
  u_value s163 = v_m;
  u_value t164 = NULL;
  if ((!t164) && u_is_con(s163, &n_map_46_MLeaf)) {
    t164 = u_mk_int(0);
  }
  if ((!t164) && u_is_con(s163, &n_map_46_MNode)) {
    u_value v_l = u_field(s163, 0);
    u_value v_r = u_field(s163, 3);
    t164 = u_add(u_add(f_map_46_size_1(v_l), u_mk_int(1)), f_map_46_size_1(v_r));
  }
  if (!t164) {
    u_no_case(s163);
  }
  return t164;
}

u_value f_map_46_height_1(u_value v_m) {
  u_value s165 = v_m;
  u_value t166 = NULL;
  if ((!t166) && u_is_con(s165, &n_map_46_MLeaf)) {
    t166 = u_mk_int(0);
  }
  if ((!t166) && u_is_con(s165, &n_map_46_MNode)) {
    u_value v_h = u_field(s165, 4);
    t166 = v_h;
  }
  if (!t166) {
    u_no_case(s165);
  }
  return t166;
}

u_value f_map_46_mnode_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r) {
  return u_mk_con(&n_map_46_MNode, 5, v_l, v_k, v_v, v_r, f_map_46_mnode_height_2(v_l, v_r));
}

u_value f_map_46_rotate_left_1(u_value v_m) {
  u_value s167 = v_m;
  u_value t168 = NULL;
  if ((!t168) && (u_is_con(s167, &n_map_46_MNode) && u_is_con(u_field(s167, 3), &n_map_46_MNode))) {
    u_value v_a = u_field(s167, 0);
    u_value v_j = u_field(s167, 1);
    u_value v_v = u_field(s167, 2);
    u_value v_b = u_field(u_field(s167, 3), 0);
    u_value v_k = u_field(u_field(s167, 3), 1);
    u_value v_w = u_field(u_field(s167, 3), 2);
    u_value v_c = u_field(u_field(s167, 3), 3);
    t168 = f_map_46_mnode_4(f_map_46_mnode_4(v_a, v_j, v_v, v_b), v_k, v_w, v_c);
  }
  if (!t168) {
    t168 = v_m;
  }
  if (!t168) {
    u_no_case(s167);
  }
  return t168;
}

u_value f_map_46_rotate_right_1(u_value v_m) {
  u_value s169 = v_m;
  u_value t170 = NULL;
  if ((!t170) && (u_is_con(s169, &n_map_46_MNode) && u_is_con(u_field(s169, 0), &n_map_46_MNode))) {
    u_value v_a = u_field(u_field(s169, 0), 0);
    u_value v_j = u_field(u_field(s169, 0), 1);
    u_value v_v = u_field(u_field(s169, 0), 2);
    u_value v_b = u_field(u_field(s169, 0), 3);
    u_value v_k = u_field(s169, 1);
    u_value v_w = u_field(s169, 2);
    u_value v_c = u_field(s169, 3);
    t170 = f_map_46_mnode_4(v_a, v_j, v_v, f_map_46_mnode_4(v_b, v_k, v_w, v_c));
  }
  if (!t170) {
    t170 = v_m;
  }
  if (!t170) {
    u_no_case(s169);
  }
  return t170;
}

u_value f_map_46_left_of_1(u_value v_m) {
  u_value s171 = v_m;
  u_value t172 = NULL;
  if ((!t172) && u_is_con(s171, &n_map_46_MNode)) {
    u_value v_l = u_field(s171, 0);
    t172 = v_l;
  }
  if (!t172) {
    t172 = v_m;
  }
  if (!t172) {
    u_no_case(s171);
  }
  return t172;
}

u_value f_map_46_right_of_1(u_value v_m) {
  u_value s173 = v_m;
  u_value t174 = NULL;
  if ((!t174) && u_is_con(s173, &n_map_46_MNode)) {
    u_value v_r = u_field(s173, 3);
    t174 = v_r;
  }
  if (!t174) {
    t174 = v_m;
  }
  if (!t174) {
    u_no_case(s173);
  }
  return t174;
}

u_value f_map_46_balance_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r) {
  u_value t178;
  if (u_truth(f_prelude_46__62__2(f_map_46_height_1(v_l), u_add(f_map_46_height_1(v_r), u_mk_int(1))))) {
    u_value t175;
    if (u_truth(u_lt(f_map_46_height_1(f_map_46_left_of_1(v_l)), f_map_46_height_1(f_map_46_right_of_1(v_l))))) {
      t175 = f_map_46_rotate_left_1(v_l);
    } else {
      t175 = v_l;
    }
    t178 = f_map_46_rotate_right_1(f_map_46_mnode_4(t175, v_k, v_v, v_r));
  } else {
    u_value t177;
    if (u_truth(f_prelude_46__62__2(f_map_46_height_1(v_r), u_add(f_map_46_height_1(v_l), u_mk_int(1))))) {
      u_value t176;
      if (u_truth(u_lt(f_map_46_height_1(f_map_46_right_of_1(v_r)), f_map_46_height_1(f_map_46_left_of_1(v_r))))) {
        t176 = f_map_46_rotate_right_1(v_r);
      } else {
        t176 = v_r;
      }
      t177 = f_map_46_rotate_left_1(f_map_46_mnode_4(v_l, v_k, v_v, t176));
    } else {
      t177 = f_map_46_mnode_4(v_l, v_k, v_v, v_r);
    }
    t178 = t177;
  }
  return t178;
}

u_value f_map_46_balanced_1(u_value v_m) {
  u_value s179 = v_m;
  u_value t180 = NULL;
  if ((!t180) && u_is_con(s179, &n_map_46_MLeaf)) {
    t180 = u_mk_bool(1);
  }
  if ((!t180) && u_is_con(s179, &n_map_46_MNode)) {
    u_value v_l = u_field(s179, 0);
    u_value v_r = u_field(s179, 3);
    u_value v_h = u_field(s179, 4);
    u_value t181 = f_map_46_balanced_1(v_l);
    if (u_truth(t181)) {
      t181 = f_map_46_balanced_1(v_r);
    }
    u_value t182 = t181;
    if (u_truth(t182)) {
      t182 = u_eq(v_h, f_map_46_mnode_height_2(v_l, v_r));
    }
    u_value t183 = t182;
    if (u_truth(t183)) {
      t183 = f_prelude_46__60__61__2(f_map_46_height_1(v_l), u_add(f_map_46_height_1(v_r), u_mk_int(1)));
    }
    u_value t184 = t183;
    if (u_truth(t184)) {
      t184 = f_prelude_46__60__61__2(f_map_46_height_1(v_r), u_add(f_map_46_height_1(v_l), u_mk_int(1)));
    }
    t180 = t184;
  }
  if (!t180) {
    u_no_case(s179);
  }
  return t180;
}

u_value f_map_46_mnode_height_2(u_value v_l, u_value v_r) {
  u_value t185;
  if (u_truth(u_lt(f_map_46_height_1(v_l), f_map_46_height_1(v_r)))) {
    t185 = f_map_46_height_1(v_r);
  } else {
    t185 = f_map_46_height_1(v_l);
  }
  return u_add(u_mk_int(1), t185);
}

u_value f_map_46_put_all_2(u_value v_ps, u_value v_m) {
  u_value s186 = v_ps;
  u_value t187 = NULL;
  if ((!t187) && u_is_con(s186, &n_list_46_Nil)) {
    t187 = v_m;
  }
  if ((!t187) && u_is_con(s186, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s186, 0), 0);
    u_value v_v = u_field(u_field(s186, 0), 1);
    u_value v_rest = u_field(s186, 1);
    t187 = f_map_46_put_all_2(v_rest, f_map_46_put_3(v_k, v_v, v_m));
  }
  if (!t187) {
    u_no_case(s186);
  }
  return t187;
}

u_value f_map_46_numbers_1(u_value v_n) {
  u_value t188;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t188 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t188 = f_list_46__43__43__2(f_map_46_numbers_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t188;
}

u_value f_ctx_46_Ctx_46_aliases_1(u_value v_x) {
  u_value s189 = v_x;
  u_value t190 = NULL;
  if ((!t190) && u_is_con(s189, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s189, 0);
    t190 = v_y;
  }
  if (!t190) {
    u_no_case(s189);
  }
  return t190;
}

u_value f_ctx_46_Ctx_46_funs_1(u_value v_x) {
  u_value s191 = v_x;
  u_value t192 = NULL;
  if ((!t192) && u_is_con(s191, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s191, 1);
    t192 = v_y;
  }
  if (!t192) {
    u_no_case(s191);
  }
  return t192;
}

u_value f_ctx_46_Ctx_46_procs_1(u_value v_x) {
  u_value s193 = v_x;
  u_value t194 = NULL;
  if ((!t194) && u_is_con(s193, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s193, 2);
    t194 = v_y;
  }
  if (!t194) {
    u_no_case(s193);
  }
  return t194;
}

u_value f_ctx_46_Ctx_46_datas_1(u_value v_x) {
  u_value s195 = v_x;
  u_value t196 = NULL;
  if ((!t196) && u_is_con(s195, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s195, 3);
    t196 = v_y;
  }
  if (!t196) {
    u_no_case(s195);
  }
  return t196;
}

u_value f_ctx_46_Ctx_46_members_1(u_value v_x) {
  u_value s197 = v_x;
  u_value t198 = NULL;
  if ((!t198) && u_is_con(s197, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s197, 4);
    t198 = v_y;
  }
  if (!t198) {
    u_no_case(s197);
  }
  return t198;
}

u_value f_ctx_46_Ctx_46_constrs_1(u_value v_x) {
  u_value s199 = v_x;
  u_value t200 = NULL;
  if ((!t200) && u_is_con(s199, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s199, 5);
    t200 = v_y;
  }
  if (!t200) {
    u_no_case(s199);
  }
  return t200;
}

u_value f_ctx_46_Ctx_46_named_1(u_value v_x) {
  u_value s201 = v_x;
  u_value t202 = NULL;
  if ((!t202) && u_is_con(s201, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s201, 6);
    t202 = v_y;
  }
  if (!t202) {
    u_no_case(s201);
  }
  return t202;
}

u_value f_ctx_46_Ctx_46_aliases_58__61__2(u_value v_x, u_value v_y) {
  u_value s203 = v_x;
  u_value t204 = NULL;
  if ((!t204) && u_is_con(s203, &n_ctx_46_Ctx)) {
    u_value v_a1 = u_field(s203, 1);
    u_value v_a2 = u_field(s203, 2);
    u_value v_a3 = u_field(s203, 3);
    u_value v_a4 = u_field(s203, 4);
    u_value v_a5 = u_field(s203, 5);
    u_value v_a6 = u_field(s203, 6);
    t204 = u_mk_con(&n_ctx_46_Ctx, 7, v_y, v_a1, v_a2, v_a3, v_a4, v_a5, v_a6);
  }
  if (!t204) {
    u_no_case(s203);
  }
  return t204;
}

u_value f_ctx_46_Ctx_46_funs_58__61__2(u_value v_x, u_value v_y) {
  u_value s205 = v_x;
  u_value t206 = NULL;
  if ((!t206) && u_is_con(s205, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s205, 0);
    u_value v_a2 = u_field(s205, 2);
    u_value v_a3 = u_field(s205, 3);
    u_value v_a4 = u_field(s205, 4);
    u_value v_a5 = u_field(s205, 5);
    u_value v_a6 = u_field(s205, 6);
    t206 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_y, v_a2, v_a3, v_a4, v_a5, v_a6);
  }
  if (!t206) {
    u_no_case(s205);
  }
  return t206;
}

u_value f_ctx_46_Ctx_46_procs_58__61__2(u_value v_x, u_value v_y) {
  u_value s207 = v_x;
  u_value t208 = NULL;
  if ((!t208) && u_is_con(s207, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s207, 0);
    u_value v_a1 = u_field(s207, 1);
    u_value v_a3 = u_field(s207, 3);
    u_value v_a4 = u_field(s207, 4);
    u_value v_a5 = u_field(s207, 5);
    u_value v_a6 = u_field(s207, 6);
    t208 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_a1, v_y, v_a3, v_a4, v_a5, v_a6);
  }
  if (!t208) {
    u_no_case(s207);
  }
  return t208;
}

u_value f_ctx_46_Ctx_46_datas_58__61__2(u_value v_x, u_value v_y) {
  u_value s209 = v_x;
  u_value t210 = NULL;
  if ((!t210) && u_is_con(s209, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s209, 0);
    u_value v_a1 = u_field(s209, 1);
    u_value v_a2 = u_field(s209, 2);
    u_value v_a4 = u_field(s209, 4);
    u_value v_a5 = u_field(s209, 5);
    u_value v_a6 = u_field(s209, 6);
    t210 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_a1, v_a2, v_y, v_a4, v_a5, v_a6);
  }
  if (!t210) {
    u_no_case(s209);
  }
  return t210;
}

u_value f_ctx_46_Ctx_46_members_58__61__2(u_value v_x, u_value v_y) {
  u_value s211 = v_x;
  u_value t212 = NULL;
  if ((!t212) && u_is_con(s211, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s211, 0);
    u_value v_a1 = u_field(s211, 1);
    u_value v_a2 = u_field(s211, 2);
    u_value v_a3 = u_field(s211, 3);
    u_value v_a5 = u_field(s211, 5);
    u_value v_a6 = u_field(s211, 6);
    t212 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_a1, v_a2, v_a3, v_y, v_a5, v_a6);
  }
  if (!t212) {
    u_no_case(s211);
  }
  return t212;
}

u_value f_ctx_46_Ctx_46_constrs_58__61__2(u_value v_x, u_value v_y) {
  u_value s213 = v_x;
  u_value t214 = NULL;
  if ((!t214) && u_is_con(s213, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s213, 0);
    u_value v_a1 = u_field(s213, 1);
    u_value v_a2 = u_field(s213, 2);
    u_value v_a3 = u_field(s213, 3);
    u_value v_a4 = u_field(s213, 4);
    u_value v_a6 = u_field(s213, 6);
    t214 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_a1, v_a2, v_a3, v_a4, v_y, v_a6);
  }
  if (!t214) {
    u_no_case(s213);
  }
  return t214;
}

u_value f_ctx_46_Ctx_46_named_58__61__2(u_value v_x, u_value v_y) {
  u_value s215 = v_x;
  u_value t216 = NULL;
  if ((!t216) && u_is_con(s215, &n_ctx_46_Ctx)) {
    u_value v_a0 = u_field(s215, 0);
    u_value v_a1 = u_field(s215, 1);
    u_value v_a2 = u_field(s215, 2);
    u_value v_a3 = u_field(s215, 3);
    u_value v_a4 = u_field(s215, 4);
    u_value v_a5 = u_field(s215, 5);
    t216 = u_mk_con(&n_ctx_46_Ctx, 7, v_a0, v_a1, v_a2, v_a3, v_a4, v_a5, v_y);
  }
  if (!t216) {
    u_no_case(s215);
  }
  return t216;
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
  u_value s217 = f_map_46_get_2(v_n, f_ctx_46_Ctx_46_funs_1(v_c));
  u_value t218 = NULL;
  if ((!t218) && u_is_con(s217, &n_option_46_Some)) {
    u_value v_ss = u_field(s217, 0);
    t218 = v_ss;
  }
  if ((!t218) && u_is_con(s217, &n_option_46_None)) {
    t218 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t218) {
    u_no_case(s217);
  }
  return t218;
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
  u_value t219 = f_ctx_46_is_data_2(v_c, v_n);
  if (!u_truth(t219)) {
    t219 = f_ctx_46_is_alias_2(v_c, v_n);
  }
  return t219;
}

u_value f_ctx_46_names_of_2(u_value v_c, u_value v_b) {
  u_value s220 = f_map_46_get_2(v_b, f_ctx_46_Ctx_46_named_1(v_c));
  u_value t221 = NULL;
  if ((!t221) && u_is_con(s220, &n_option_46_Some)) {
    u_value v_ns = u_field(s220, 0);
    t221 = v_ns;
  }
  if ((!t221) && u_is_con(s220, &n_option_46_None)) {
    t221 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t221) {
    u_no_case(s220);
  }
  return t221;
}

u_value f_ctx_46_with_named_2(u_value v_bs, u_value v_n) {
  u_value s222 = f_map_46_get_2(f_syntax_46_Name_46_base_1(v_n), v_bs);
  u_value t223 = NULL;
  if ((!t223) && u_is_con(s222, &n_option_46_Some)) {
    u_value v_ns = u_field(s222, 0);
    u_value t224;
    if (u_truth(f_list_46_contains_2(v_n, v_ns))) {
      t224 = v_bs;
    } else {
      t224 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), f_list_46__43__43__2(v_ns, u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0))), v_bs);
    }
    t223 = t224;
  }
  if ((!t223) && u_is_con(s222, &n_option_46_None)) {
    t223 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_bs);
  }
  if (!t223) {
    u_no_case(s222);
  }
  return t223;
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
  u_value s225 = v_x;
  u_value t226 = NULL;
  if ((!t226) && u_is_con(s225, &n_check_46_State)) {
    u_value v_y = u_field(s225, 0);
    t226 = v_y;
  }
  if (!t226) {
    u_no_case(s225);
  }
  return t226;
}

u_value f_check_46_State_46_sub_1(u_value v_x) {
  u_value s227 = v_x;
  u_value t228 = NULL;
  if ((!t228) && u_is_con(s227, &n_check_46_State)) {
    u_value v_y = u_field(s227, 1);
    t228 = v_y;
  }
  if (!t228) {
    u_no_case(s227);
  }
  return t228;
}

u_value f_check_46_State_46_count_1(u_value v_x) {
  u_value s229 = v_x;
  u_value t230 = NULL;
  if ((!t230) && u_is_con(s229, &n_check_46_State)) {
    u_value v_y = u_field(s229, 2);
    t230 = v_y;
  }
  if (!t230) {
    u_no_case(s229);
  }
  return t230;
}

u_value f_check_46_State_46_module_1(u_value v_x) {
  u_value s231 = v_x;
  u_value t232 = NULL;
  if ((!t232) && u_is_con(s231, &n_check_46_State)) {
    u_value v_y = u_field(s231, 3);
    t232 = v_y;
  }
  if (!t232) {
    u_no_case(s231);
  }
  return t232;
}

u_value f_check_46_State_46_visible_1(u_value v_x) {
  u_value s233 = v_x;
  u_value t234 = NULL;
  if ((!t234) && u_is_con(s233, &n_check_46_State)) {
    u_value v_y = u_field(s233, 4);
    t234 = v_y;
  }
  if (!t234) {
    u_no_case(s233);
  }
  return t234;
}

u_value f_check_46_State_46_table_1(u_value v_x) {
  u_value s235 = v_x;
  u_value t236 = NULL;
  if ((!t236) && u_is_con(s235, &n_check_46_State)) {
    u_value v_y = u_field(s235, 5);
    t236 = v_y;
  }
  if (!t236) {
    u_no_case(s235);
  }
  return t236;
}

u_value f_check_46_State_46_ctx_58__61__2(u_value v_x, u_value v_y) {
  u_value s237 = v_x;
  u_value t238 = NULL;
  if ((!t238) && u_is_con(s237, &n_check_46_State)) {
    u_value v_a1 = u_field(s237, 1);
    u_value v_a2 = u_field(s237, 2);
    u_value v_a3 = u_field(s237, 3);
    u_value v_a4 = u_field(s237, 4);
    u_value v_a5 = u_field(s237, 5);
    t238 = u_mk_con(&n_check_46_State, 6, v_y, v_a1, v_a2, v_a3, v_a4, v_a5);
  }
  if (!t238) {
    u_no_case(s237);
  }
  return t238;
}

u_value f_check_46_State_46_sub_58__61__2(u_value v_x, u_value v_y) {
  u_value s239 = v_x;
  u_value t240 = NULL;
  if ((!t240) && u_is_con(s239, &n_check_46_State)) {
    u_value v_a0 = u_field(s239, 0);
    u_value v_a2 = u_field(s239, 2);
    u_value v_a3 = u_field(s239, 3);
    u_value v_a4 = u_field(s239, 4);
    u_value v_a5 = u_field(s239, 5);
    t240 = u_mk_con(&n_check_46_State, 6, v_a0, v_y, v_a2, v_a3, v_a4, v_a5);
  }
  if (!t240) {
    u_no_case(s239);
  }
  return t240;
}

u_value f_check_46_State_46_count_58__61__2(u_value v_x, u_value v_y) {
  u_value s241 = v_x;
  u_value t242 = NULL;
  if ((!t242) && u_is_con(s241, &n_check_46_State)) {
    u_value v_a0 = u_field(s241, 0);
    u_value v_a1 = u_field(s241, 1);
    u_value v_a3 = u_field(s241, 3);
    u_value v_a4 = u_field(s241, 4);
    u_value v_a5 = u_field(s241, 5);
    t242 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_y, v_a3, v_a4, v_a5);
  }
  if (!t242) {
    u_no_case(s241);
  }
  return t242;
}

u_value f_check_46_State_46_module_58__61__2(u_value v_x, u_value v_y) {
  u_value s243 = v_x;
  u_value t244 = NULL;
  if ((!t244) && u_is_con(s243, &n_check_46_State)) {
    u_value v_a0 = u_field(s243, 0);
    u_value v_a1 = u_field(s243, 1);
    u_value v_a2 = u_field(s243, 2);
    u_value v_a4 = u_field(s243, 4);
    u_value v_a5 = u_field(s243, 5);
    t244 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_a2, v_y, v_a4, v_a5);
  }
  if (!t244) {
    u_no_case(s243);
  }
  return t244;
}

u_value f_check_46_State_46_visible_58__61__2(u_value v_x, u_value v_y) {
  u_value s245 = v_x;
  u_value t246 = NULL;
  if ((!t246) && u_is_con(s245, &n_check_46_State)) {
    u_value v_a0 = u_field(s245, 0);
    u_value v_a1 = u_field(s245, 1);
    u_value v_a2 = u_field(s245, 2);
    u_value v_a3 = u_field(s245, 3);
    u_value v_a5 = u_field(s245, 5);
    t246 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_a2, v_a3, v_y, v_a5);
  }
  if (!t246) {
    u_no_case(s245);
  }
  return t246;
}

u_value f_check_46_State_46_table_58__61__2(u_value v_x, u_value v_y) {
  u_value s247 = v_x;
  u_value t248 = NULL;
  if ((!t248) && u_is_con(s247, &n_check_46_State)) {
    u_value v_a0 = u_field(s247, 0);
    u_value v_a1 = u_field(s247, 1);
    u_value v_a2 = u_field(s247, 2);
    u_value v_a3 = u_field(s247, 3);
    u_value v_a4 = u_field(s247, 4);
    t248 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_a2, v_a3, v_a4, v_y);
  }
  if (!t248) {
    u_no_case(s247);
  }
  return t248;
}

u_value f_check_46_with_ctx_2(u_value v_s, u_value v_c) {
  return f_check_46_State_46_ctx_58__61__2(v_s, v_c);
}

u_value f_check_46_enter_2(u_value v_s, u_value v_m) {
  return f_check_46_State_46_visible_58__61__2(f_check_46_State_46_module_58__61__2(f_check_46_State_46_count_58__61__2(f_check_46_State_46_sub_58__61__2(v_s, f_map_46_empty_0()), u_mk_int(0)), v_m), f_check_46_sees_2(v_m, f_check_46_State_46_table_1(v_s)));
}

u_value f_check_46_sees_2(u_value v_m, u_value v_vs) {
  u_value s249 = f_list_46_lookup_2(v_m, v_vs);
  u_value t250 = NULL;
  if ((!t250) && u_is_con(s249, &n_option_46_Some)) {
    u_value v_ms = u_field(s249, 0);
    t250 = v_ms;
  }
  if ((!t250) && u_is_con(s249, &n_option_46_None)) {
    t250 = u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t250) {
    u_no_case(s249);
  }
  return t250;
}

u_value f_check_46_start_2(u_value v_main, u_value v_vs) {
  return u_mk_con(&n_check_46_State, 6, f_check_46_initial_0(), f_map_46_empty_0(), u_mk_int(0), v_main, f_check_46_sees_2(v_main, v_vs), v_vs);
}

u_value f_check_46_flatten_2(u_value v_m, u_value v_cs) {
  u_value s251 = v_cs;
  u_value t252 = NULL;
  if ((!t252) && u_is_con(s251, &n_list_46_Nil)) {
    t252 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t252) && (u_is_con(s251, &n_list_46_Cons) && u_is_con(u_field(s251, 0), &n_syntax_46_CModule))) {
    u_value v_n = u_field(u_field(s251, 0), 0);
    u_value v_ds = u_field(u_field(s251, 0), 1);
    u_value v_rest = u_field(s251, 1);
    t252 = f_list_46__43__43__2(f_check_46_flatten_2(v_n, v_ds), f_check_46_flatten_2(v_m, v_rest));
  }
  if ((!t252) && u_is_con(s251, &n_list_46_Cons)) {
    u_value v_c = u_field(s251, 0);
    u_value v_rest = u_field(s251, 1);
    t252 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_m, f_check_46_qualify_2(v_m, v_c)), f_check_46_flatten_2(v_m, v_rest));
  }
  if (!t252) {
    u_no_case(s251);
  }
  return t252;
}

u_value f_check_46_in_module_2(u_value v_m, u_value v_n) {
  return u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), f_syntax_46_Name_46_base_1(v_n));
}

u_value f_check_46_qualify_2(u_value v_m, u_value v_c) {
  u_value s253 = v_c;
  u_value t254 = NULL;
  if ((!t254) && u_is_con(s253, &n_syntax_46_CSort)) {
    u_value v_n = u_field(s253, 0);
    u_value v_ps = u_field(s253, 1);
    u_value v_t = u_field(s253, 2);
    t254 = u_mk_con(&n_syntax_46_CSort, 3, f_check_46_in_module_2(v_m, v_n), v_ps, v_t);
  }
  if ((!t254) && u_is_con(s253, &n_syntax_46_CData)) {
    u_value v_n = u_field(s253, 0);
    u_value v_ps = u_field(s253, 1);
    u_value v_ks = u_field(s253, 2);
    u_value t255 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e256 = &t255;
    u_value l257 = v_ks;
    while (u_is_con(l257, &n_list_46_Cons)) {
      u_value v_k = u_field(l257, 0);
      if (u_truth(u_mk_bool(1))) {
        e256 = u_append(e256, f_check_46_qualify_constr_2(v_m, v_k));
      }
      l257 = u_field(l257, 1);
    }
    t254 = u_mk_con(&n_syntax_46_CData, 3, f_check_46_in_module_2(v_m, v_n), v_ps, t255);
  }
  if ((!t254) && u_is_con(s253, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s253, 0);
    u_value v_ps = u_field(s253, 1);
    u_value v_xss = u_field(s253, 2);
    u_value v_r = u_field(s253, 3);
    u_value v_b = u_field(s253, 4);
    t254 = u_mk_con(&n_syntax_46_CFun, 5, f_check_46_in_module_2(v_m, v_n), v_ps, v_xss, v_r, v_b);
  }
  if ((!t254) && u_is_con(s253, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s253, 0);
    u_value v_ps = u_field(s253, 1);
    u_value v_ins = u_field(s253, 2);
    u_value v_outs = u_field(s253, 3);
    u_value v_refs = u_field(s253, 4);
    u_value v_b = u_field(s253, 5);
    t254 = u_mk_con(&n_syntax_46_CProc, 6, f_check_46_in_module_2(v_m, v_n), v_ps, v_ins, v_outs, v_refs, v_b);
  }
  if (!t254) {
    t254 = v_c;
  }
  if (!t254) {
    u_no_case(s253);
  }
  return t254;
}

u_value f_check_46_qualify_constr_2(u_value v_m, u_value v_k) {
  return f_syntax_46_Constr_46_name_58__61__2(v_k, f_check_46_in_module_2(v_m, f_syntax_46_Constr_46_name_1(v_k)));
}

u_value f_check_46_imports_in_2(u_value v_ds, u_value v_m) {
  u_value s258 = v_ds;
  u_value t259 = NULL;
  if ((!t259) && u_is_con(s258, &n_list_46_Nil)) {
    t259 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t259) && (u_is_con(s258, &n_list_46_Cons) && u_is_con(u_field(u_field(s258, 0), 1), &n_syntax_46_CImport))) {
    u_value v_k = u_field(u_field(s258, 0), 0);
    u_value v_n = u_field(u_field(u_field(s258, 0), 1), 0);
    u_value v_rest = u_field(s258, 1);
    if (u_truth(u_eq(v_k, v_m))) {
      t259 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_imports_in_2(v_rest, v_m));
    }
  }
  if ((!t259) && u_is_con(s258, &n_list_46_Cons)) {
    u_value v_rest = u_field(s258, 1);
    t259 = f_check_46_imports_in_2(v_rest, v_m);
  }
  if (!t259) {
    u_no_case(s258);
  }
  return t259;
}

u_value f_check_46_visibility_2(u_value v_ds, u_value v_main) {
  u_value t260 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e261 = &t260;
  u_value l262 = f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_main, u_mk_con(&n_list_46_Nil, 0)), f_list_46_firsts_1(v_ds)));
  while (u_is_con(l262, &n_list_46_Cons)) {
    u_value v_m = u_field(l262, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_prelude_263 = U_LIT("prelude");
      static struct u_object l_prelude_264 = U_LIT("prelude");
      e261 = u_append(e261, u_mk_tuple(2, v_m, f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_check_46_imports_in_2(v_ds, v_m), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, &l_prelude_263, u_mk_con(&n_list_46_Nil, 0)), f_check_46_imports_in_2(v_ds, &l_prelude_264)))))));
    }
    l262 = u_field(l262, 1);
  }
  return t260;
}

u_value f_check_46_visible_names_3(u_value v_s, u_value v_n, u_value v_declared) {
  u_value t265 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e266 = &t265;
  u_value l267 = f_ctx_46_names_of_2(f_check_46_State_46_ctx_1(v_s), f_syntax_46_Name_46_base_1(v_n));
  while (u_is_con(l267, &n_list_46_Cons)) {
    u_value v_d = u_field(l267, 0);
    if (u_truth(u_apply(v_declared, 1, v_d))) {
      e266 = u_append(e266, v_d);
    }
    l267 = u_field(l267, 1);
  }
  u_value t276 = t265;
  u_value t275;
  {
    u_value v_ds = t276;
    u_value s268 = v_n;
    u_value t269 = NULL;
    if ((!t269) && (u_is_con(s268, &n_syntax_46_Name) && u_is_con(u_field(s268, 0), &n_option_46_Some))) {
      u_value t270;
      if (u_truth(f_list_46_contains_2(v_n, v_ds))) {
        t270 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
      } else {
        t270 = u_mk_con(&n_list_46_Nil, 0);
      }
      t269 = t270;
    }
    if ((!t269) && (u_is_con(s268, &n_syntax_46_Name) && u_is_con(u_field(s268, 0), &n_option_46_None))) {
      u_value v_b = u_field(s268, 1);
      u_value t274;
      if (u_truth(f_list_46_contains_2(u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), v_ds))) {
        t274 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), u_mk_con(&n_list_46_Nil, 0));
      } else {
        u_value t271 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e272 = &t271;
        u_value l273 = v_ds;
        while (u_is_con(l273, &n_list_46_Cons)) {
          u_value v_d = u_field(l273, 0);
          if (u_truth(f_check_46_is_visible_2(v_s, v_d))) {
            e272 = u_append(e272, v_d);
          }
          l273 = u_field(l273, 1);
        }
        t274 = t271;
      }
      t269 = t274;
    }
    if (!t269) {
      u_no_case(s268);
    }
    t275 = t269;
  }
  return t275;
}

u_value f_check_46_is_visible_2(u_value v_s, u_value v_d) {
  u_value s277 = f_syntax_46_Name_46_module_1(v_d);
  u_value t278 = NULL;
  if ((!t278) && u_is_con(s277, &n_option_46_Some)) {
    u_value v_m = u_field(s277, 0);
    t278 = f_list_46_contains_2(v_m, f_check_46_State_46_visible_1(v_s));
  }
  if ((!t278) && u_is_con(s277, &n_option_46_None)) {
    t278 = u_mk_bool(0);
  }
  if (!t278) {
    u_no_case(s277);
  }
  return t278;
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
  static struct u_object l_Bool_279 = U_LIT("Bool");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Bool_279), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Int_280 = U_LIT("Int");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Int_280), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Str_281 = U_LIT("Str");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Str_281), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Char_282 = U_LIT("Char");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Char_282), u_mk_con(&n_list_46_Nil, 0));
  return cache;
}

u_value f_check_46_tlist_1(u_value v_t) {
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_list_name_0(), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_check_46_fun_2(u_value v_args, u_value v_res) {
  return u_mk_con(&n_syntax_46_TFun, 2, v_args, v_res);
}

u_value f_check_46_params_1(u_value v_ps) {
  u_value t283 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e284 = &t283;
  u_value l285 = v_ps;
  while (u_is_con(l285, &n_list_46_Cons)) {
    u_value v_p = u_field(l285, 0);
    if (u_truth(u_mk_bool(1))) {
      e284 = u_append(e284, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l285 = u_field(l285, 1);
  }
  return t283;
}

u_value f_check_46_basic_1(u_value v_n) {
  u_value s286 = v_n;
  u_value t287 = NULL;
  if ((!t287) && (u_is_con(s286, &n_syntax_46_Name) && u_is_con(u_field(s286, 0), &n_option_46_None))) {
    u_value v_b = u_field(s286, 1);
    static struct u_object l_Bool_288 = U_LIT("Bool");
    static struct u_object l_Int_289 = U_LIT("Int");
    static struct u_object l_Str_290 = U_LIT("Str");
    static struct u_object l_Char_291 = U_LIT("Char");
    t287 = f_list_46_contains_2(v_b, u_mk_con(&n_list_46_Cons, 2, &l_Bool_288, u_mk_con(&n_list_46_Cons, 2, &l_Int_289, u_mk_con(&n_list_46_Cons, 2, &l_Str_290, u_mk_con(&n_list_46_Cons, 2, &l_Char_291, u_mk_con(&n_list_46_Nil, 0))))));
  }
  if (!t287) {
    t287 = u_mk_bool(0);
  }
  if (!t287) {
    u_no_case(s286);
  }
  return t287;
}

u_value f_check_46_literal_1(u_value v_v) {
  u_value s292 = v_v;
  u_value t293 = NULL;
  if ((!t293) && u_is_con(s292, &n_syntax_46_VUnit)) {
    t293 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t293) && u_is_con(s292, &n_syntax_46_VBool)) {
    t293 = f_check_46_tbool_0();
  }
  if ((!t293) && u_is_con(s292, &n_syntax_46_VInt)) {
    t293 = f_check_46_tint_0();
  }
  if ((!t293) && u_is_con(s292, &n_syntax_46_VStr)) {
    t293 = f_check_46_tstr_0();
  }
  if ((!t293) && u_is_con(s292, &n_syntax_46_VChar)) {
    t293 = f_check_46_tchar_0();
  }
  if (!t293) {
    u_no_case(s292);
  }
  return t293;
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
  u_value s294 = v_n;
  u_value t295 = NULL;
  if ((!t295) && (u_is_con(s294, &n_syntax_46_Name) && u_is_con(u_field(s294, 0), &n_option_46_None))) {
    u_value v_b = u_field(s294, 1);
    t295 = f_check_46_builtin_scheme_1(v_b);
  }
  if (!t295) {
    t295 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t295) {
    u_no_case(s294);
  }
  return t295;
}

u_value f_check_46_builtin_scheme_1(u_value v_b) {
  static struct u_object l_A_296 = U_LIT("A");
  u_value t300 = u_mk_con(&n_syntax_46_TParam, 1, &l_A_296);
  u_value t299;
  {
    u_value v_a = t300;
    u_value s297 = v_b;
    u_value t298 = NULL;
    if ((!t298) && u_equals(s297, u_mk_lit("+"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("-"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("*"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("/"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("%"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("<"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("=="))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("compare"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("and"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("or"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("chars"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tlist_1(f_check_46_tchar_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("string"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tchar_0()), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("ord"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("chr"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("^"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("str_size"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("str_join"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("str_slice"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("str_split"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tlist_1(f_check_46_tstr_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("str_get"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("str_contains"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("str_compare"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("str_show"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("str_quote"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("str_number"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_option_name_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("is_digit"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("is_letter"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t298) && u_equals(s297, u_mk_lit("is_space"))) {
      t298 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if (!t298) {
      t298 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t298) {
      u_no_case(s297);
    }
    t299 = t298;
  }
  return t299;
}

u_value f_check_46_is_meta_1(u_value v_t) {
  u_value s301 = v_t;
  u_value t302 = NULL;
  if ((!t302) && u_is_con(s301, &n_syntax_46_TMeta)) {
    t302 = u_mk_bool(1);
  }
  if (!t302) {
    t302 = u_mk_bool(0);
  }
  if (!t302) {
    u_no_case(s301);
  }
  return t302;
}

u_value f_check_46_free_1(u_value v_t) {
  u_value s303 = v_t;
  u_value t304 = NULL;
  if ((!t304) && u_is_con(s303, &n_syntax_46_TParam)) {
    t304 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t304) && u_is_con(s303, &n_syntax_46_TMeta)) {
    t304 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t304) && u_is_con(s303, &n_syntax_46_THole)) {
    t304 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t304) && u_is_con(s303, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s303, 1);
    u_value t305 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e306 = &t305;
    u_value l307 = v_args;
    while (u_is_con(l307, &n_list_46_Cons)) {
      u_value v_a = u_field(l307, 0);
      if (u_truth(u_mk_bool(1))) {
        e306 = u_append(e306, f_check_46_free_1(v_a));
      }
      l307 = u_field(l307, 1);
    }
    t304 = f_list_46_distinct_1(f_list_46_concat_1(t305));
  }
  if ((!t304) && u_is_con(s303, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s303, 0);
    u_value v_r = u_field(s303, 1);
    u_value t308 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e309 = &t308;
    u_value l310 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l310, &n_list_46_Cons)) {
      u_value v_a = u_field(l310, 0);
      if (u_truth(u_mk_bool(1))) {
        e309 = u_append(e309, f_check_46_free_1(v_a));
      }
      l310 = u_field(l310, 1);
    }
    t304 = f_list_46_distinct_1(f_list_46_concat_1(t308));
  }
  if ((!t304) && u_is_con(s303, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s303, 0);
    u_value t311 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e312 = &t311;
    u_value l313 = v_ts;
    while (u_is_con(l313, &n_list_46_Cons)) {
      u_value v_a = u_field(l313, 0);
      if (u_truth(u_mk_bool(1))) {
        e312 = u_append(e312, f_check_46_free_1(v_a));
      }
      l313 = u_field(l313, 1);
    }
    t304 = f_list_46_distinct_1(f_list_46_concat_1(t311));
  }
  if (!t304) {
    u_no_case(s303);
  }
  return t304;
}

u_value f_check_46_holes_1(u_value v_t) {
  u_value s314 = v_t;
  u_value t315 = NULL;
  if ((!t315) && u_is_con(s314, &n_syntax_46_THole)) {
    t315 = u_mk_bool(1);
  }
  if ((!t315) && u_is_con(s314, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s314, 1);
    u_value t316 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e317 = &t316;
    u_value l318 = v_args;
    while (u_is_con(l318, &n_list_46_Cons)) {
      u_value v_a = u_field(l318, 0);
      if (u_truth(u_mk_bool(1))) {
        e317 = u_append(e317, f_check_46_holes_1(v_a));
      }
      l318 = u_field(l318, 1);
    }
    t315 = f_list_46_contains_2(u_mk_bool(1), t316);
  }
  if ((!t315) && u_is_con(s314, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s314, 0);
    u_value v_r = u_field(s314, 1);
    u_value t319 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e320 = &t319;
    u_value l321 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l321, &n_list_46_Cons)) {
      u_value v_a = u_field(l321, 0);
      if (u_truth(u_mk_bool(1))) {
        e320 = u_append(e320, f_check_46_holes_1(v_a));
      }
      l321 = u_field(l321, 1);
    }
    t315 = f_list_46_contains_2(u_mk_bool(1), t319);
  }
  if ((!t315) && u_is_con(s314, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s314, 0);
    u_value t322 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e323 = &t322;
    u_value l324 = v_ts;
    while (u_is_con(l324, &n_list_46_Cons)) {
      u_value v_a = u_field(l324, 0);
      if (u_truth(u_mk_bool(1))) {
        e323 = u_append(e323, f_check_46_holes_1(v_a));
      }
      l324 = u_field(l324, 1);
    }
    t315 = f_list_46_contains_2(u_mk_bool(1), t322);
  }
  if (!t315) {
    t315 = u_mk_bool(0);
  }
  if (!t315) {
    u_no_case(s314);
  }
  return t315;
}

u_value f_check_46_subst_2(u_value v_t, u_value v_m) {
  u_value s325 = v_t;
  u_value t326 = NULL;
  if ((!t326) && u_is_con(s325, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s325, 0);
    u_value v_args = u_field(s325, 1);
    u_value t327 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e328 = &t327;
    u_value l329 = v_args;
    while (u_is_con(l329, &n_list_46_Cons)) {
      u_value v_a = u_field(l329, 0);
      if (u_truth(u_mk_bool(1))) {
        e328 = u_append(e328, f_check_46_subst_2(v_a, v_m));
      }
      l329 = u_field(l329, 1);
    }
    t326 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t327);
  }
  if ((!t326) && u_is_con(s325, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s325, 0);
    u_value v_r = u_field(s325, 1);
    u_value t330 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e331 = &t330;
    u_value l332 = v_args;
    while (u_is_con(l332, &n_list_46_Cons)) {
      u_value v_a = u_field(l332, 0);
      if (u_truth(u_mk_bool(1))) {
        e331 = u_append(e331, f_check_46_subst_2(v_a, v_m));
      }
      l332 = u_field(l332, 1);
    }
    t326 = u_mk_con(&n_syntax_46_TFun, 2, t330, f_check_46_subst_2(v_r, v_m));
  }
  if ((!t326) && u_is_con(s325, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s325, 0);
    u_value t333 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e334 = &t333;
    u_value l335 = v_ts;
    while (u_is_con(l335, &n_list_46_Cons)) {
      u_value v_a = u_field(l335, 0);
      if (u_truth(u_mk_bool(1))) {
        e334 = u_append(e334, f_check_46_subst_2(v_a, v_m));
      }
      l335 = u_field(l335, 1);
    }
    t326 = u_mk_con(&n_syntax_46_TTuple, 1, t333);
  }
  if (!t326) {
    u_value s336 = f_list_46_lookup_2(v_t, v_m);
    u_value t337 = NULL;
    if ((!t337) && u_is_con(s336, &n_option_46_Some)) {
      u_value v_u = u_field(s336, 0);
      t337 = v_u;
    }
    if ((!t337) && u_is_con(s336, &n_option_46_None)) {
      t337 = v_t;
    }
    if (!t337) {
      u_no_case(s336);
    }
    t326 = t337;
  }
  if (!t326) {
    u_no_case(s325);
  }
  return t326;
}

u_value f_check_46_resolve_2(u_value v_su, u_value v_t) {
  u_value s338 = v_t;
  u_value t339 = NULL;
  if ((!t339) && u_is_con(s338, &n_syntax_46_TMeta)) {
    u_value v_i = u_field(s338, 0);
    u_value s340 = f_map_46_get_2(v_i, v_su);
    u_value t341 = NULL;
    if ((!t341) && u_is_con(s340, &n_option_46_Some)) {
      u_value v_u = u_field(s340, 0);
      t341 = f_check_46_resolve_2(v_su, v_u);
    }
    if ((!t341) && u_is_con(s340, &n_option_46_None)) {
      t341 = v_t;
    }
    if (!t341) {
      u_no_case(s340);
    }
    t339 = t341;
  }
  if (!t339) {
    t339 = v_t;
  }
  if (!t339) {
    u_no_case(s338);
  }
  return t339;
}

u_value f_check_46_occurs_3(u_value v_su, u_value v_i, u_value v_t) {
  u_value s342 = f_check_46_resolve_2(v_su, v_t);
  u_value t343 = NULL;
  if ((!t343) && u_is_con(s342, &n_syntax_46_TMeta)) {
    u_value v_j = u_field(s342, 0);
    t343 = u_eq(v_i, v_j);
  }
  if ((!t343) && u_is_con(s342, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s342, 1);
    t343 = f_list_46_exists_2(v_args, u_mk_closure(lam344, 2, v_su, v_i));
  }
  if ((!t343) && u_is_con(s342, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s342, 0);
    u_value v_r = u_field(s342, 1);
    u_value t346 = f_list_46_exists_2(v_args, u_mk_closure(lam345, 2, v_su, v_i));
    if (!u_truth(t346)) {
      t346 = f_check_46_occurs_3(v_su, v_i, v_r);
    }
    t343 = t346;
  }
  if ((!t343) && u_is_con(s342, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s342, 0);
    t343 = f_list_46_exists_2(v_ts, u_mk_closure(lam347, 2, v_su, v_i));
  }
  if (!t343) {
    t343 = u_mk_bool(0);
  }
  if (!t343) {
    u_no_case(s342);
  }
  return t343;
}

u_value f_check_46_constructor_scheme_1(u_value v_k) {
  u_value s348 = v_k;
  u_value t349 = NULL;
  if (!t349) {
    u_value v_ps = u_field(s348, 0);
    u_value v_ks = u_field(s348, 1);
    u_value v_r = u_field(s348, 2);
    u_value t350;
    if (u_truth(u_eq(v_ks, u_mk_con(&n_list_46_Nil, 0)))) {
      t350 = v_r;
    } else {
      t350 = f_check_46_fun_2(v_ks, v_r);
    }
    t349 = u_mk_tuple(2, v_ps, t350);
  }
  if (!t349) {
    u_no_case(s348);
  }
  return t349;
}

u_value f_check_46_constructor_of_2(u_value v_c, u_value v_k) {
  u_value s351 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t352 = NULL;
  if ((!t352) && u_is_con(s351, &n_option_46_Some)) {
    u_value v_kc = u_field(s351, 0);
    t352 = f_check_46_constructor_scheme_1(v_kc);
  }
  if ((!t352) && u_is_con(s351, &n_option_46_None)) {
    t352 = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t352) {
    u_no_case(s351);
  }
  return t352;
}

u_value f_check_46_candidates_3(u_value v_s, u_value v_env, u_value v_n) {
  u_value s353 = f_list_46_lookup_2(v_n, v_env);
  u_value t354 = NULL;
  if ((!t354) && u_is_con(s353, &n_option_46_Some)) {
    u_value v_sc = u_field(s353, 0);
    t354 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, v_sc), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t354) && u_is_con(s353, &n_option_46_None)) {
    u_value t355 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e356 = &t355;
    u_value l358 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam357, 1, v_s));
    while (u_is_con(l358, &n_list_46_Cons)) {
      u_value v_f = u_field(l358, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t359 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e360 = &t359;
        u_value l361 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(v_s), v_f);
        while (u_is_con(l361, &n_list_46_Cons)) {
          u_value v_sc = u_field(l361, 0);
          if (u_truth(u_mk_bool(1))) {
            e360 = u_append(e360, u_mk_tuple(2, v_f, v_sc));
          }
          l361 = u_field(l361, 1);
        }
        e356 = u_append(e356, t359);
      }
      l358 = u_field(l358, 1);
    }
    u_value t362 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e363 = &t362;
    u_value l365 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam364, 1, v_s));
    while (u_is_con(l365, &n_list_46_Cons)) {
      u_value v_k = u_field(l365, 0);
      if (u_truth(u_mk_bool(1))) {
        e363 = u_append(e363, u_mk_tuple(2, v_k, f_check_46_constructor_of_2(f_check_46_State_46_ctx_1(v_s), v_k)));
      }
      l365 = u_field(l365, 1);
    }
    u_value t366 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e367 = &t366;
    u_value l368 = f_check_46_builtin_1(v_n);
    while (u_is_con(l368, &n_list_46_Cons)) {
      u_value v_sc = u_field(l368, 0);
      if (u_truth(u_mk_bool(1))) {
        e367 = u_append(e367, u_mk_tuple(2, v_n, v_sc));
      }
      l368 = u_field(l368, 1);
    }
    t354 = f_list_46__43__43__2(f_list_46_concat_1(t355), f_list_46__43__43__2(t362, t366));
  }
  if (!t354) {
    u_no_case(s353);
  }
  return t354;
}

u_value f_check_46_with_arity_2(u_value v_cs, u_value v_k) {
  u_value s369 = v_cs;
  u_value t370 = NULL;
  if ((!t370) && u_is_con(s369, &n_list_46_Nil)) {
    t370 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t370) && u_is_con(s369, &n_list_46_Cons)) {
    u_value v_n = u_field(u_field(s369, 0), 0);
    u_value v_ps = u_field(u_field(u_field(s369, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s369, 0), 1), 1);
    u_value v_rest = u_field(s369, 1);
    u_value t371;
    if (u_truth(u_eq(f_syntax_46_arity_1(v_t), v_k))) {
      t371 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, v_ps, v_t)), f_check_46_with_arity_2(v_rest, v_k));
    } else {
      t371 = f_check_46_with_arity_2(v_rest, v_k);
    }
    t370 = t371;
  }
  if (!t370) {
    u_no_case(s369);
  }
  return t370;
}

u_value f_check_46_has_arity_2(u_value v_scs, u_value v_k) {
  u_value s372 = v_scs;
  u_value t373 = NULL;
  if ((!t373) && u_is_con(s372, &n_list_46_Nil)) {
    t373 = u_mk_bool(0);
  }
  if ((!t373) && u_is_con(s372, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s372, 0), 1);
    u_value v_rest = u_field(s372, 1);
    u_value t374 = u_eq(f_syntax_46_arity_1(v_t), v_k);
    if (!u_truth(t374)) {
      t374 = f_check_46_has_arity_2(v_rest, v_k);
    }
    t373 = t374;
  }
  if (!t373) {
    u_no_case(s372);
  }
  return t373;
}

u_value f_check_46_bind_1(u_value v_xs) {
  u_value t375 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e376 = &t375;
  u_value l377 = v_xs;
  while (u_is_con(l377, &n_list_46_Cons)) {
    u_value v_x = u_field(l377, 0);
    if (u_truth(u_mk_bool(1))) {
      e376 = u_append(e376, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_syntax_46_Var_46_typ_1(v_x))));
    }
    l377 = u_field(l377, 1);
  }
  return t375;
}

u_value f_check_46_fixed_2(u_value v_su, u_value v_env) {
  u_value s378 = v_env;
  u_value t379 = NULL;
  if ((!t379) && u_is_con(s378, &n_list_46_Nil)) {
    t379 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t379) && u_is_con(s378, &n_list_46_Cons)) {
    u_value v_ps = u_field(u_field(u_field(s378, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s378, 0), 1), 1);
    u_value v_rest = u_field(s378, 1);
    t379 = f_list_46__43__43__2(f_list_46_diff_2(f_check_46_free_1(f_check_46_zonk_type_2(v_su, v_t)), v_ps), f_check_46_fixed_2(v_su, v_rest));
  }
  if (!t379) {
    u_no_case(s378);
  }
  return t379;
}

u_value f_check_46_generalize_3(u_value v_su, u_value v_env, u_value v_t) {
  u_value t385 = f_check_46_fixed_2(v_su, v_env);
  u_value t386 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t384;
  {
    u_value v_fx = t385;
    u_value v_u = t386;
    u_value t380 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e381 = &t380;
    u_value l382 = f_check_46_free_1(v_u);
    while (u_is_con(l382, &n_list_46_Cons)) {
      u_value v_m = u_field(l382, 0);
      u_value t383 = f_check_46_is_meta_1(v_m);
      if (u_truth(t383)) {
        t383 = f_prelude_46_not_1(f_list_46_contains_2(v_m, v_fx));
      }
      if (u_truth(t383)) {
        e381 = u_append(e381, v_m);
      }
      l382 = u_field(l382, 1);
    }
    t384 = u_mk_tuple(2, t380, v_u);
  }
  return t384;
}

u_value f_check_46_generalized_3(u_value v_su, u_value v_env, u_value v_xs) {
  u_value t387 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e388 = &t387;
  u_value l389 = v_xs;
  while (u_is_con(l389, &n_list_46_Cons)) {
    u_value v_x = u_field(l389, 0);
    if (u_truth(u_mk_bool(1))) {
      e388 = u_append(e388, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), f_check_46_generalize_3(v_su, v_env, f_syntax_46_Var_46_typ_1(v_x))));
    }
    l389 = u_field(l389, 1);
  }
  return t387;
}

u_value f_check_46_case_of_1(u_value v_b) {
  u_value s390 = v_b;
  u_value t391 = NULL;
  if ((!t391) && u_is_con(s390, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s390, 0);
    u_value v_g = u_field(s390, 1);
    t391 = u_mk_tuple(2, v_p, u_eq(v_g, u_mk_con(&n_option_46_None, 0)));
  }
  if (!t391) {
    u_no_case(s390);
  }
  return t391;
}

u_value f_check_46_zonk_guard_2(u_value v_su, u_value v_g) {
  u_value s392 = v_g;
  u_value t393 = NULL;
  if ((!t393) && u_is_con(s392, &n_option_46_Some)) {
    u_value v_e = u_field(s392, 0);
    t393 = u_mk_con(&n_option_46_Some, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t393) && u_is_con(s392, &n_option_46_None)) {
    t393 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t393) {
    u_no_case(s392);
  }
  return t393;
}

u_value f_check_46_zonk_type_2(u_value v_su, u_value v_t) {
  u_value s394 = f_check_46_resolve_2(v_su, v_t);
  u_value t395 = NULL;
  if ((!t395) && u_is_con(s394, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s394, 0);
    u_value v_args = u_field(s394, 1);
    u_value t396 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e397 = &t396;
    u_value l398 = v_args;
    while (u_is_con(l398, &n_list_46_Cons)) {
      u_value v_a = u_field(l398, 0);
      if (u_truth(u_mk_bool(1))) {
        e397 = u_append(e397, f_check_46_zonk_type_2(v_su, v_a));
      }
      l398 = u_field(l398, 1);
    }
    t395 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t396);
  }
  if ((!t395) && u_is_con(s394, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s394, 0);
    u_value v_r = u_field(s394, 1);
    u_value t399 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e400 = &t399;
    u_value l401 = v_args;
    while (u_is_con(l401, &n_list_46_Cons)) {
      u_value v_a = u_field(l401, 0);
      if (u_truth(u_mk_bool(1))) {
        e400 = u_append(e400, f_check_46_zonk_type_2(v_su, v_a));
      }
      l401 = u_field(l401, 1);
    }
    t395 = u_mk_con(&n_syntax_46_TFun, 2, t399, f_check_46_zonk_type_2(v_su, v_r));
  }
  if ((!t395) && u_is_con(s394, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s394, 0);
    u_value t402 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e403 = &t402;
    u_value l404 = v_ts;
    while (u_is_con(l404, &n_list_46_Cons)) {
      u_value v_a = u_field(l404, 0);
      if (u_truth(u_mk_bool(1))) {
        e403 = u_append(e403, f_check_46_zonk_type_2(v_su, v_a));
      }
      l404 = u_field(l404, 1);
    }
    t395 = u_mk_con(&n_syntax_46_TTuple, 1, t402);
  }
  if (!t395) {
    u_value v_u = s394;
    t395 = v_u;
  }
  if (!t395) {
    u_no_case(s394);
  }
  return t395;
}

u_value f_check_46_zonk_var_2(u_value v_su, u_value v_x) {
  return f_syntax_46_Var_46_typ_58__61__2(v_x, f_check_46_zonk_type_2(v_su, f_syntax_46_Var_46_typ_1(v_x)));
}

u_value f_check_46_zonk_vars_2(u_value v_su, u_value v_xs) {
  u_value t405 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e406 = &t405;
  u_value l407 = v_xs;
  while (u_is_con(l407, &n_list_46_Cons)) {
    u_value v_x = u_field(l407, 0);
    if (u_truth(u_mk_bool(1))) {
      e406 = u_append(e406, f_check_46_zonk_var_2(v_su, v_x));
    }
    l407 = u_field(l407, 1);
  }
  return t405;
}

u_value f_check_46_zonk_expr_2(u_value v_su, u_value v_e) {
  u_value s408 = v_e;
  u_value t409 = NULL;
  if ((!t409) && u_is_con(s408, &n_syntax_46_ELit)) {
    t409 = v_e;
  }
  if ((!t409) && u_is_con(s408, &n_syntax_46_EVar)) {
    u_value v_x = u_field(s408, 0);
    t409 = u_mk_con(&n_syntax_46_EVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t409) && u_is_con(s408, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s408, 0);
    u_value v_args = u_field(s408, 1);
    u_value t410 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e411 = &t410;
    u_value l412 = v_args;
    while (u_is_con(l412, &n_list_46_Cons)) {
      u_value v_a = u_field(l412, 0);
      if (u_truth(u_mk_bool(1))) {
        e411 = u_append(e411, f_check_46_zonk_expr_2(v_su, v_a));
      }
      l412 = u_field(l412, 1);
    }
    t409 = u_mk_con(&n_syntax_46_EApp, 2, f_check_46_zonk_expr_2(v_su, v_f), t410);
  }
  if ((!t409) && u_is_con(s408, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s408, 0);
    u_value v_b = u_field(s408, 1);
    t409 = u_mk_con(&n_syntax_46_ELam, 2, f_check_46_zonk_vars_2(v_su, v_xs), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t409) && u_is_con(s408, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s408, 0);
    u_value v_b = u_field(s408, 1);
    u_value t413 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e414 = &t413;
    u_value l415 = v_ps;
    while (u_is_con(l415, &n_list_46_Cons)) {
      u_value v_p = u_field(l415, 0);
      if (u_truth(u_mk_bool(1))) {
        e414 = u_append(e414, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l415 = u_field(l415, 1);
    }
    t409 = u_mk_con(&n_syntax_46_ELet, 2, t413, f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t409) && u_is_con(s408, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s408, 0);
    u_value v_l = u_field(s408, 1);
    u_value v_r = u_field(s408, 2);
    t409 = u_mk_con(&n_syntax_46_EIte, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_expr_2(v_su, v_l), f_check_46_zonk_expr_2(v_su, v_r));
  }
  if ((!t409) && u_is_con(s408, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s408, 0);
    u_value v_bs = u_field(s408, 1);
    u_value t416 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e417 = &t416;
    u_value l418 = v_bs;
    while (u_is_con(l418, &n_list_46_Cons)) {
      u_value v_b = u_field(l418, 0);
      if (u_truth(u_mk_bool(1))) {
        e417 = u_append(e417, f_check_46_zonk_expr_branch_2(v_su, v_b));
      }
      l418 = u_field(l418, 1);
    }
    t409 = u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t416);
  }
  if ((!t409) && u_is_con(s408, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s408, 0);
    u_value v_xs = u_field(s408, 1);
    u_value v_ls = u_field(s408, 2);
    u_value v_c = u_field(s408, 3);
    u_value t419 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e420 = &t419;
    u_value l421 = v_ls;
    while (u_is_con(l421, &n_list_46_Cons)) {
      u_value v_l = u_field(l421, 0);
      if (u_truth(u_mk_bool(1))) {
        e420 = u_append(e420, f_check_46_zonk_expr_2(v_su, v_l));
      }
      l421 = u_field(l421, 1);
    }
    t409 = u_mk_con(&n_syntax_46_EComp, 4, f_check_46_zonk_expr_2(v_su, v_r), f_check_46_zonk_vars_2(v_su, v_xs), t419, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t409) && u_is_con(s408, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s408, 0);
    u_value t422 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e423 = &t422;
    u_value l424 = v_es;
    while (u_is_con(l424, &n_list_46_Cons)) {
      u_value v_e = u_field(l424, 0);
      if (u_truth(u_mk_bool(1))) {
        e423 = u_append(e423, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l424 = u_field(l424, 1);
    }
    t409 = u_mk_con(&n_syntax_46_ETuple, 1, t422);
  }
  if ((!t409) && u_is_con(s408, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s408, 0);
    u_value v_t = u_field(s408, 1);
    t409 = u_mk_con(&n_syntax_46_ERaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t409) {
    u_no_case(s408);
  }
  return t409;
}

u_value f_check_46_zonk_binding_2(u_value v_su, u_value v_p) {
  u_value s425 = v_p;
  u_value t426 = NULL;
  if (!t426) {
    u_value v_x = u_field(s425, 0);
    u_value v_e = u_field(s425, 1);
    t426 = u_mk_tuple(2, f_check_46_zonk_var_2(v_su, v_x), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t426) {
    u_no_case(s425);
  }
  return t426;
}

u_value f_check_46_zonk_expr_branch_2(u_value v_su, u_value v_b) {
  u_value s427 = v_b;
  u_value t428 = NULL;
  if ((!t428) && u_is_con(s427, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s427, 0);
    u_value v_g = u_field(s427, 1);
    u_value v_e = u_field(s427, 2);
    t428 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t428) {
    u_no_case(s427);
  }
  return t428;
}

u_value f_check_46_zonk_pat_2(u_value v_su, u_value v_p) {
  u_value s429 = v_p;
  u_value t430 = NULL;
  if ((!t430) && u_is_con(s429, &n_syntax_46_PVar)) {
    u_value v_x = u_field(s429, 0);
    t430 = u_mk_con(&n_syntax_46_PVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t430) && u_is_con(s429, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s429, 0);
    u_value v_args = u_field(s429, 1);
    u_value t431 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e432 = &t431;
    u_value l433 = v_args;
    while (u_is_con(l433, &n_list_46_Cons)) {
      u_value v_a = u_field(l433, 0);
      if (u_truth(u_mk_bool(1))) {
        e432 = u_append(e432, f_check_46_zonk_pat_2(v_su, v_a));
      }
      l433 = u_field(l433, 1);
    }
    t430 = u_mk_con(&n_syntax_46_PApp, 2, v_n, t431);
  }
  if ((!t430) && u_is_con(s429, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s429, 0);
    u_value t434 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e435 = &t434;
    u_value l436 = v_ps;
    while (u_is_con(l436, &n_list_46_Cons)) {
      u_value v_q = u_field(l436, 0);
      if (u_truth(u_mk_bool(1))) {
        e435 = u_append(e435, f_check_46_zonk_pat_2(v_su, v_q));
      }
      l436 = u_field(l436, 1);
    }
    t430 = u_mk_con(&n_syntax_46_PTuple, 1, t434);
  }
  if (!t430) {
    t430 = v_p;
  }
  if (!t430) {
    u_no_case(s429);
  }
  return t430;
}

u_value f_check_46_zonk_stmt_2(u_value v_su, u_value v_x) {
  u_value s437 = v_x;
  u_value t438 = NULL;
  if ((!t438) && u_is_con(s437, &n_syntax_46_SSkip)) {
    t438 = v_x;
  }
  if ((!t438) && u_is_con(s437, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s437, 0);
    u_value t439 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e440 = &t439;
    u_value l441 = v_ps;
    while (u_is_con(l441, &n_list_46_Cons)) {
      u_value v_p = u_field(l441, 0);
      if (u_truth(u_mk_bool(1))) {
        e440 = u_append(e440, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l441 = u_field(l441, 1);
    }
    t438 = u_mk_con(&n_syntax_46_SAssign, 1, t439);
  }
  if ((!t438) && u_is_con(s437, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s437, 0);
    u_value v_ys = u_field(s437, 1);
    u_value t442 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e443 = &t442;
    u_value l444 = v_ys;
    while (u_is_con(l444, &n_list_46_Cons)) {
      u_value v_y = u_field(l444, 0);
      if (u_truth(u_mk_bool(1))) {
        e443 = u_append(e443, f_check_46_zonk_stmt_2(v_su, v_y));
      }
      l444 = u_field(l444, 1);
    }
    t438 = u_mk_con(&n_syntax_46_SBlock, 2, f_check_46_zonk_vars_2(v_su, v_xs), t442);
  }
  if ((!t438) && u_is_con(s437, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s437, 0);
    u_value v_l = u_field(s437, 1);
    u_value v_r = u_field(s437, 2);
    t438 = u_mk_con(&n_syntax_46_SIf, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_l), f_check_46_zonk_stmt_2(v_su, v_r));
  }
  if ((!t438) && u_is_con(s437, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s437, 0);
    u_value v_b = u_field(s437, 1);
    t438 = u_mk_con(&n_syntax_46_SWhile, 2, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t438) && u_is_con(s437, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s437, 0);
    u_value v_ins = u_field(s437, 1);
    u_value v_outs = u_field(s437, 2);
    u_value v_refs = u_field(s437, 3);
    u_value t445 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e446 = &t445;
    u_value l447 = v_ins;
    while (u_is_con(l447, &n_list_46_Cons)) {
      u_value v_e = u_field(l447, 0);
      if (u_truth(u_mk_bool(1))) {
        e446 = u_append(e446, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l447 = u_field(l447, 1);
    }
    t438 = u_mk_con(&n_syntax_46_SCall, 4, v_p, t445, f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs));
  }
  if ((!t438) && u_is_con(s437, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s437, 0);
    u_value v_bs = u_field(s437, 1);
    u_value t448 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e449 = &t448;
    u_value l450 = v_bs;
    while (u_is_con(l450, &n_list_46_Cons)) {
      u_value v_b = u_field(l450, 0);
      if (u_truth(u_mk_bool(1))) {
        e449 = u_append(e449, f_check_46_zonk_stmt_branch_2(v_su, v_b));
      }
      l450 = u_field(l450, 1);
    }
    t438 = u_mk_con(&n_syntax_46_SMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t448);
  }
  if ((!t438) && u_is_con(s437, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s437, 0);
    t438 = u_mk_con(&n_syntax_46_SAssert, 1, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t438) && u_is_con(s437, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s437, 0);
    u_value v_t = u_field(s437, 1);
    t438 = u_mk_con(&n_syntax_46_SRaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if ((!t438) && u_is_con(s437, &n_syntax_46_STry)) {
    u_value v_b = u_field(s437, 0);
    u_value v_hs = u_field(s437, 1);
    u_value t451 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e452 = &t451;
    u_value l453 = v_hs;
    while (u_is_con(l453, &n_list_46_Cons)) {
      u_value v_h = u_field(l453, 0);
      if (u_truth(u_mk_bool(1))) {
        e452 = u_append(e452, f_check_46_zonk_handler_2(v_su, v_h));
      }
      l453 = u_field(l453, 1);
    }
    t438 = u_mk_con(&n_syntax_46_STry, 2, f_check_46_zonk_stmt_2(v_su, v_b), t451);
  }
  if (!t438) {
    u_no_case(s437);
  }
  return t438;
}

u_value f_check_46_zonk_stmt_branch_2(u_value v_su, u_value v_b) {
  u_value s454 = v_b;
  u_value t455 = NULL;
  if ((!t455) && u_is_con(s454, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s454, 0);
    u_value v_g = u_field(s454, 1);
    u_value v_y = u_field(s454, 2);
    t455 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_y));
  }
  if (!t455) {
    u_no_case(s454);
  }
  return t455;
}

u_value f_check_46_zonk_handler_2(u_value v_su, u_value v_h) {
  u_value s456 = v_h;
  u_value t457 = NULL;
  if ((!t457) && u_is_con(s456, &n_syntax_46_Handler)) {
    u_value v_t = u_field(s456, 0);
    u_value v_p = u_field(s456, 1);
    u_value v_g = u_field(s456, 2);
    u_value v_b = u_field(s456, 3);
    t457 = u_mk_con(&n_syntax_46_Handler, 4, f_check_46_zonk_type_2(v_su, v_t), f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t457) {
    u_no_case(s456);
  }
  return t457;
}

u_value f_check_46_zonk_cmd_2(u_value v_su, u_value v_c) {
  u_value s458 = v_c;
  u_value t459 = NULL;
  if ((!t459) && u_is_con(s458, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s458, 0);
    u_value v_ps = u_field(s458, 1);
    u_value v_xss = u_field(s458, 2);
    u_value v_r = u_field(s458, 3);
    u_value v_b = u_field(s458, 4);
    u_value t460 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e461 = &t460;
    u_value l462 = v_xss;
    while (u_is_con(l462, &n_list_46_Cons)) {
      u_value v_xs = u_field(l462, 0);
      if (u_truth(u_mk_bool(1))) {
        e461 = u_append(e461, f_check_46_zonk_vars_2(v_su, v_xs));
      }
      l462 = u_field(l462, 1);
    }
    t459 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, t460, f_check_46_zonk_type_2(v_su, v_r), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t459) && u_is_con(s458, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s458, 0);
    u_value v_ps = u_field(s458, 1);
    u_value v_ins = u_field(s458, 2);
    u_value v_outs = u_field(s458, 3);
    u_value v_refs = u_field(s458, 4);
    u_value v_b = u_field(s458, 5);
    t459 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, f_check_46_zonk_vars_2(v_su, v_ins), f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t459) && u_is_con(s458, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s458, 0);
    t459 = u_mk_con(&n_syntax_46_CEval, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t459) && u_is_con(s458, &n_syntax_46_CRun)) {
    u_value v_n = u_field(s458, 0);
    u_value v_b = u_field(s458, 1);
    t459 = u_mk_con(&n_syntax_46_CRun, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t459) && u_is_con(s458, &n_syntax_46_CTest)) {
    u_value v_n = u_field(s458, 0);
    u_value v_b = u_field(s458, 1);
    t459 = u_mk_con(&n_syntax_46_CTest, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t459) {
    t459 = v_c;
  }
  if (!t459) {
    u_no_case(s458);
  }
  return t459;
}

u_value f_check_46_unknown_type_1(u_value v_t) {
  u_value t463;
  if (u_truth(u_eq(f_check_46_free_1(v_t), u_mk_con(&n_list_46_Nil, 0)))) {
    t463 = u_mk_con(&n_option_46_None, 0);
  } else {
    t463 = u_mk_con(&n_option_46_Some, 1, v_t);
  }
  return t463;
}

u_value f_check_46_unknown_in_1(u_value v_x) {
  u_value s464 = v_x;
  u_value t465 = NULL;
  if ((!t465) && u_is_con(s464, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s464, 0);
    t465 = f_check_46_unknown_in_exprs_1(f_list_46_seconds_1(v_ps));
  }
  if ((!t465) && u_is_con(s464, &n_syntax_46_SBlock)) {
    u_value v_ys = u_field(s464, 1);
    t465 = f_check_46_unknown_in_all_1(v_ys);
  }
  if ((!t465) && u_is_con(s464, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s464, 0);
    u_value v_l = u_field(s464, 1);
    u_value v_r = u_field(s464, 2);
    t465 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_option_46_or_else_2(f_check_46_unknown_in_1(v_l), f_check_46_unknown_in_1(v_r)));
  }
  if ((!t465) && u_is_con(s464, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s464, 0);
    u_value v_b = u_field(s464, 1);
    t465 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_check_46_unknown_in_1(v_b));
  }
  if ((!t465) && u_is_con(s464, &n_syntax_46_SCall)) {
    u_value v_ins = u_field(s464, 1);
    t465 = f_check_46_unknown_in_exprs_1(v_ins);
  }
  if ((!t465) && u_is_con(s464, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s464, 0);
    u_value v_bs = u_field(s464, 1);
    t465 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_a), f_check_46_unknown_in_branches_1(v_bs));
  }
  if ((!t465) && u_is_con(s464, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s464, 0);
    t465 = f_check_46_unknown_in_expr_1(v_c);
  }
  if ((!t465) && u_is_con(s464, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s464, 0);
    u_value v_t = u_field(s464, 1);
    t465 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t465) && u_is_con(s464, &n_syntax_46_STry)) {
    u_value v_b = u_field(s464, 0);
    u_value v_hs = u_field(s464, 1);
    t465 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_hs));
  }
  if (!t465) {
    t465 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t465) {
    u_no_case(s464);
  }
  return t465;
}

u_value f_check_46_unknown_in_expr_1(u_value v_e) {
  u_value s466 = v_e;
  u_value t467 = NULL;
  if ((!t467) && u_is_con(s466, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s466, 0);
    u_value v_t = u_field(s466, 1);
    t467 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t467) && u_is_con(s466, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s466, 0);
    u_value v_args = u_field(s466, 1);
    t467 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_f, v_args));
  }
  if ((!t467) && u_is_con(s466, &n_syntax_46_ELam)) {
    u_value v_b = u_field(s466, 1);
    t467 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t467) && u_is_con(s466, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s466, 0);
    u_value v_b = u_field(s466, 1);
    t467 = f_check_46_unknown_in_exprs_1(f_list_46__43__43__2(f_list_46_seconds_1(v_ps), u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t467) && u_is_con(s466, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s466, 0);
    u_value v_l = u_field(s466, 1);
    u_value v_r = u_field(s466, 2);
    t467 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)))));
  }
  if ((!t467) && u_is_con(s466, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s466, 0);
    u_value v_bs = u_field(s466, 1);
    u_value t468 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e469 = &t468;
    u_value l470 = v_bs;
    while (u_is_con(l470, &n_list_46_Cons)) {
      u_value v_b = u_field(l470, 0);
      if (u_truth(u_mk_bool(1))) {
        e469 = u_append(e469, f_check_46_branch_exprs_1(v_b));
      }
      l470 = u_field(l470, 1);
    }
    t467 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_a, f_list_46_concat_1(t468)));
  }
  if ((!t467) && u_is_con(s466, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s466, 0);
    u_value v_ls = u_field(s466, 2);
    u_value v_c = u_field(s466, 3);
    t467 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Cons, 2, v_c, v_ls)));
  }
  if ((!t467) && u_is_con(s466, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s466, 0);
    t467 = f_check_46_unknown_in_exprs_1(v_es);
  }
  if (!t467) {
    t467 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t467) {
    u_no_case(s466);
  }
  return t467;
}

u_value f_check_46_unknown_in_exprs_1(u_value v_es) {
  u_value s471 = v_es;
  u_value t472 = NULL;
  if ((!t472) && u_is_con(s471, &n_list_46_Nil)) {
    t472 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t472) && u_is_con(s471, &n_list_46_Cons)) {
    u_value v_e = u_field(s471, 0);
    u_value v_rest = u_field(s471, 1);
    t472 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_e), f_check_46_unknown_in_exprs_1(v_rest));
  }
  if (!t472) {
    u_no_case(s471);
  }
  return t472;
}

u_value f_check_46_branch_exprs_1(u_value v_b) {
  u_value s473 = v_b;
  u_value t474 = NULL;
  if ((!t474) && (u_is_con(s473, &n_syntax_46_Branch) && u_is_con(u_field(s473, 1), &n_option_46_Some))) {
    u_value v_g = u_field(u_field(s473, 1), 0);
    u_value v_e = u_field(s473, 2);
    t474 = u_mk_con(&n_list_46_Cons, 2, v_g, u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t474) && (u_is_con(s473, &n_syntax_46_Branch) && u_is_con(u_field(s473, 1), &n_option_46_None))) {
    u_value v_e = u_field(s473, 2);
    t474 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t474) {
    u_no_case(s473);
  }
  return t474;
}

u_value f_check_46_unknown_in_all_1(u_value v_xs) {
  u_value s475 = v_xs;
  u_value t476 = NULL;
  if ((!t476) && u_is_con(s475, &n_list_46_Nil)) {
    t476 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t476) && u_is_con(s475, &n_list_46_Cons)) {
    u_value v_y = u_field(s475, 0);
    u_value v_ys = u_field(s475, 1);
    t476 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_all_1(v_ys));
  }
  if (!t476) {
    u_no_case(s475);
  }
  return t476;
}

u_value f_check_46_unknown_in_branches_1(u_value v_bs) {
  u_value s477 = v_bs;
  u_value t478 = NULL;
  if ((!t478) && u_is_con(s477, &n_list_46_Nil)) {
    t478 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t478) && (u_is_con(s477, &n_list_46_Cons) && (u_is_con(u_field(s477, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s477, 0), 1), &n_option_46_Some)))) {
    u_value v_g = u_field(u_field(u_field(s477, 0), 1), 0);
    u_value v_y = u_field(u_field(s477, 0), 2);
    u_value v_rest = u_field(s477, 1);
    t478 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_g), f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest)));
  }
  if ((!t478) && (u_is_con(s477, &n_list_46_Cons) && (u_is_con(u_field(s477, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s477, 0), 1), &n_option_46_None)))) {
    u_value v_y = u_field(u_field(s477, 0), 2);
    u_value v_rest = u_field(s477, 1);
    t478 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest));
  }
  if (!t478) {
    u_no_case(s477);
  }
  return t478;
}

u_value f_check_46_unknown_in_handlers_1(u_value v_hs) {
  u_value s479 = v_hs;
  u_value t480 = NULL;
  if ((!t480) && u_is_con(s479, &n_list_46_Nil)) {
    t480 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t480) && (u_is_con(s479, &n_list_46_Cons) && (u_is_con(u_field(s479, 0), &n_syntax_46_Handler) && u_is_con(u_field(u_field(s479, 0), 1), &n_syntax_46_PWild)))) {
    u_value v_b = u_field(u_field(s479, 0), 3);
    u_value v_rest = u_field(s479, 1);
    t480 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest));
  }
  if ((!t480) && (u_is_con(s479, &n_list_46_Cons) && u_is_con(u_field(s479, 0), &n_syntax_46_Handler))) {
    u_value v_t = u_field(u_field(s479, 0), 0);
    u_value v_b = u_field(u_field(s479, 0), 3);
    u_value v_rest = u_field(s479, 1);
    t480 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest)));
  }
  if (!t480) {
    u_no_case(s479);
  }
  return t480;
}

u_value f_check_46_unknown_in_cmd_1(u_value v_c) {
  u_value s481 = v_c;
  u_value t482 = NULL;
  if ((!t482) && u_is_con(s481, &n_syntax_46_CFun)) {
    u_value v_b = u_field(s481, 4);
    t482 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t482) && u_is_con(s481, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s481, 0);
    t482 = f_check_46_unknown_in_expr_1(v_e);
  }
  if ((!t482) && u_is_con(s481, &n_syntax_46_CProc)) {
    u_value v_b = u_field(s481, 5);
    t482 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t482) && u_is_con(s481, &n_syntax_46_CRun)) {
    u_value v_b = u_field(s481, 1);
    t482 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t482) && u_is_con(s481, &n_syntax_46_CTest)) {
    u_value v_b = u_field(s481, 1);
    t482 = f_check_46_unknown_in_1(v_b);
  }
  if (!t482) {
    t482 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t482) {
    u_no_case(s481);
  }
  return t482;
}

u_value f_check_46_head_of_1(u_value v_p) {
  u_value s483 = v_p;
  u_value t484 = NULL;
  if ((!t484) && u_is_con(s483, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s483, 0);
    u_value v_args = u_field(s483, 1);
    t484 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_n), v_args));
  }
  if ((!t484) && u_is_con(s483, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s483, 0);
    t484 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ps));
  }
  if ((!t484) && u_is_con(s483, &n_syntax_46_PLit)) {
    u_value v_v = u_field(s483, 0);
    t484 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, v_v), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t484) {
    t484 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t484) {
    u_no_case(s483);
  }
  return t484;
}

u_value f_check_46_heads_1(u_value v_rows) {
  u_value s485 = v_rows;
  u_value t486 = NULL;
  if ((!t486) && u_is_con(s485, &n_list_46_Nil)) {
    t486 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t486) && (u_is_con(s485, &n_list_46_Cons) && u_is_con(u_field(s485, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s485, 0), 0);
    u_value v_rest = u_field(s485, 1);
    u_value s487 = f_check_46_head_of_1(v_p);
    u_value t488 = NULL;
    if ((!t488) && u_is_con(s487, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s487, 0), 0);
      t488 = u_mk_con(&n_list_46_Cons, 2, v_h, f_check_46_heads_1(v_rest));
    }
    if ((!t488) && u_is_con(s487, &n_option_46_None)) {
      t488 = f_check_46_heads_1(v_rest);
    }
    if (!t488) {
      u_no_case(s487);
    }
    t486 = t488;
  }
  if ((!t486) && (u_is_con(s485, &n_list_46_Cons) && u_is_con(u_field(s485, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s485, 1);
    t486 = f_check_46_heads_1(v_rest);
  }
  if (!t486) {
    u_no_case(s485);
  }
  return t486;
}

u_value f_check_46_field_types_3(u_value v_c, u_value v_k, u_value v_args) {
  u_value s489 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t490 = NULL;
  if ((!t490) && u_is_con(s489, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s489, 0), 0);
    u_value v_ks = u_field(u_field(s489, 0), 1);
    u_value t491 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e492 = &t491;
    u_value l493 = v_ks;
    while (u_is_con(l493, &n_list_46_Cons)) {
      u_value v_a = u_field(l493, 0);
      if (u_truth(u_mk_bool(1))) {
        e492 = u_append(e492, f_check_46_subst_2(v_a, f_list_46_zip_2(v_ps, v_args)));
      }
      l493 = u_field(l493, 1);
    }
    t490 = t491;
  }
  if ((!t490) && u_is_con(s489, &n_option_46_None)) {
    t490 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t490) {
    u_no_case(s489);
  }
  return t490;
}

u_value f_check_46_constructors_of_3(u_value v_c, u_value v_su, u_value v_t) {
  u_value s494 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t495 = NULL;
  if ((!t495) && (u_is_con(s494, &n_syntax_46_TSort) && (u_is_con(u_field(s494, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s494, 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(s494, 0), 1), u_mk_lit("Bool")) && u_is_con(u_field(s494, 1), &n_list_46_Nil)))))) {
    t495 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t495) && u_is_con(s494, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s494, 0);
    t495 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ts), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t495) && u_is_con(s494, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s494, 0);
    u_value v_args = u_field(s494, 1);
    u_value s496 = f_ctx_46_members_of_2(v_c, v_n);
    u_value t497 = NULL;
    if ((!t497) && u_is_con(s496, &n_option_46_Some)) {
      u_value v_ks = u_field(s496, 0);
      u_value t498 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e499 = &t498;
      u_value l500 = v_ks;
      while (u_is_con(l500, &n_list_46_Cons)) {
        u_value v_k = u_field(l500, 0);
        if (u_truth(u_mk_bool(1))) {
          e499 = u_append(e499, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_k), f_check_46_field_types_3(v_c, v_k, v_args)));
        }
        l500 = u_field(l500, 1);
      }
      t497 = u_mk_con(&n_option_46_Some, 1, t498);
    }
    if ((!t497) && u_is_con(s496, &n_option_46_None)) {
      t497 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t497) {
      u_no_case(s496);
    }
    t495 = t497;
  }
  if (!t495) {
    t495 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t495) {
    u_no_case(s494);
  }
  return t495;
}

u_value f_check_46_fields_of_4(u_value v_c, u_value v_su, u_value v_t, u_value v_h) {
  u_value s501 = f_check_46_constructors_of_3(v_c, v_su, v_t);
  u_value t502 = NULL;
  if ((!t502) && u_is_con(s501, &n_option_46_Some)) {
    u_value v_ks = u_field(s501, 0);
    u_value s503 = f_list_46_lookup_2(v_h, v_ks);
    u_value t504 = NULL;
    if ((!t504) && u_is_con(s503, &n_option_46_Some)) {
      u_value v_ts = u_field(s503, 0);
      t504 = v_ts;
    }
    if ((!t504) && u_is_con(s503, &n_option_46_None)) {
      t504 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t504) {
      u_no_case(s503);
    }
    t502 = t504;
  }
  if ((!t502) && u_is_con(s501, &n_option_46_None)) {
    t502 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t502) {
    u_no_case(s501);
  }
  return t502;
}

u_value f_check_46_covers_2(u_value v_hs, u_value v_ks) {
  u_value s505 = v_ks;
  u_value t506 = NULL;
  if ((!t506) && u_is_con(s505, &n_list_46_Nil)) {
    t506 = u_mk_bool(1);
  }
  if ((!t506) && u_is_con(s505, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s505, 0), 0);
    u_value v_rest = u_field(s505, 1);
    u_value t507 = f_list_46_contains_2(v_h, v_hs);
    if (u_truth(t507)) {
      t507 = f_check_46_covers_2(v_hs, v_rest);
    }
    t506 = t507;
  }
  if (!t506) {
    u_no_case(s505);
  }
  return t506;
}

u_value f_check_46_defaults_1(u_value v_rows) {
  u_value s508 = v_rows;
  u_value t509 = NULL;
  if ((!t509) && u_is_con(s508, &n_list_46_Nil)) {
    t509 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t509) && (u_is_con(s508, &n_list_46_Cons) && u_is_con(u_field(s508, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s508, 0), 0);
    u_value v_ps = u_field(u_field(s508, 0), 1);
    u_value v_rest = u_field(s508, 1);
    u_value s510 = f_check_46_head_of_1(v_p);
    u_value t511 = NULL;
    if ((!t511) && u_is_con(s510, &n_option_46_Some)) {
      t511 = f_check_46_defaults_1(v_rest);
    }
    if ((!t511) && u_is_con(s510, &n_option_46_None)) {
      t511 = u_mk_con(&n_list_46_Cons, 2, v_ps, f_check_46_defaults_1(v_rest));
    }
    if (!t511) {
      u_no_case(s510);
    }
    t509 = t511;
  }
  if ((!t509) && (u_is_con(s508, &n_list_46_Cons) && u_is_con(u_field(s508, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s508, 1);
    t509 = f_check_46_defaults_1(v_rest);
  }
  if (!t509) {
    u_no_case(s508);
  }
  return t509;
}

u_value f_check_46_specialize_3(u_value v_rows, u_value v_h, u_value v_n) {
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
      u_value v_g = u_field(u_field(s514, 0), 0);
      u_value v_args = u_field(u_field(s514, 0), 1);
      u_value t516;
      if (u_truth(u_eq(v_g, v_h))) {
        t516 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(v_args, v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
      } else {
        t516 = f_check_46_specialize_3(v_rest, v_h, v_n);
      }
      t515 = t516;
    }
    if ((!t515) && u_is_con(s514, &n_option_46_None)) {
      t515 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(f_list_46_replicate_2(v_n, u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
    }
    if (!t515) {
      u_no_case(s514);
    }
    t513 = t515;
  }
  if ((!t513) && (u_is_con(s512, &n_list_46_Cons) && u_is_con(u_field(s512, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s512, 1);
    t513 = f_check_46_specialize_3(v_rest, v_h, v_n);
  }
  if (!t513) {
    u_no_case(s512);
  }
  return t513;
}

u_value f_check_46_useful_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_q, u_value v_ts) {
  u_value s517 = u_mk_tuple(2, v_q, v_ts);
  u_value t518 = NULL;
  if ((!t518) && u_is_con(u_field(s517, 0), &n_list_46_Nil)) {
    t518 = u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t518) && (u_is_con(u_field(s517, 0), &n_list_46_Cons) && u_is_con(u_field(s517, 1), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s517, 0), 0);
    u_value v_ps = u_field(u_field(s517, 0), 1);
    u_value v_t = u_field(u_field(s517, 1), 0);
    u_value v_rest = u_field(u_field(s517, 1), 1);
    u_value s519 = f_check_46_head_of_1(v_p);
    u_value t520 = NULL;
    if ((!t520) && u_is_con(s519, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s519, 0), 0);
      u_value v_args = u_field(u_field(s519, 0), 1);
      t520 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_ps), f_list_46__43__43__2(f_check_46_fields_of_4(v_c, v_su, v_t, v_h), v_rest));
    }
    if ((!t520) && u_is_con(s519, &n_option_46_None)) {
      u_value s521 = f_check_46_constructors_of_3(v_c, v_su, v_t);
      u_value t522 = NULL;
      if ((!t522) && u_is_con(s521, &n_option_46_Some)) {
        u_value v_ks = u_field(s521, 0);
        u_value t523;
        if (u_truth(f_check_46_covers_2(f_check_46_heads_1(v_rows), v_ks))) {
          t523 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_ks, v_ps, v_rest);
        } else {
          t523 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
        }
        t522 = t523;
      }
      if ((!t522) && u_is_con(s521, &n_option_46_None)) {
        t522 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
      }
      if (!t522) {
        u_no_case(s521);
      }
      t520 = t522;
    }
    if (!t520) {
      u_no_case(s519);
    }
    t518 = t520;
  }
  if ((!t518) && (u_is_con(u_field(s517, 0), &n_list_46_Cons) && u_is_con(u_field(s517, 1), &n_list_46_Nil))) {
    t518 = u_mk_bool(0);
  }
  if (!t518) {
    u_no_case(s517);
  }
  return t518;
}

u_value f_check_46_useful_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_ps, u_value v_rest) {
  u_value s524 = v_ks;
  u_value t525 = NULL;
  if ((!t525) && u_is_con(s524, &n_list_46_Nil)) {
    t525 = u_mk_bool(0);
  }
  if ((!t525) && u_is_con(s524, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s524, 0), 0);
    u_value v_args = u_field(u_field(s524, 0), 1);
    u_value v_more = u_field(s524, 1);
    u_value t526 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_list_46__43__43__2(v_args, v_rest));
    if (!u_truth(t526)) {
      t526 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_more, v_ps, v_rest);
    }
    t525 = t526;
  }
  if (!t525) {
    u_no_case(s524);
  }
  return t525;
}

u_value f_check_46_witness_2(u_value v_h, u_value v_args) {
  u_value s527 = v_h;
  u_value t528 = NULL;
  if ((!t528) && u_is_con(s527, &n_check_46_HCon)) {
    u_value v_n = u_field(s527, 0);
    u_value t529;
    if (u_truth(u_eq(v_n, f_syntax_46_tuple_name_0()))) {
      t529 = u_mk_con(&n_syntax_46_PTuple, 1, v_args);
    } else {
      t529 = u_mk_con(&n_syntax_46_PApp, 2, v_n, v_args);
    }
    t528 = t529;
  }
  if ((!t528) && u_is_con(s527, &n_check_46_HLit)) {
    u_value v_v = u_field(s527, 0);
    t528 = u_mk_con(&n_syntax_46_PLit, 1, v_v);
  }
  if (!t528) {
    u_no_case(s527);
  }
  return t528;
}

u_value f_check_46_missing_4(u_value v_c, u_value v_su, u_value v_rows, u_value v_ts) {
  u_value s530 = v_ts;
  u_value t531 = NULL;
  if ((!t531) && u_is_con(s530, &n_list_46_Nil)) {
    u_value t532;
    if (u_truth(u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0)))) {
      t532 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
    } else {
      t532 = u_mk_con(&n_option_46_None, 0);
    }
    t531 = t532;
  }
  if ((!t531) && u_is_con(s530, &n_list_46_Cons)) {
    u_value v_t = u_field(s530, 0);
    u_value v_rest = u_field(s530, 1);
    t531 = f_check_46_missing_in_6(v_c, v_su, v_rows, f_check_46_heads_1(v_rows), f_check_46_constructors_of_3(v_c, v_su, v_t), v_rest);
  }
  if (!t531) {
    u_no_case(s530);
  }
  return t531;
}

u_value f_check_46_missing_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s533 = v_cs;
  u_value t534 = NULL;
  if ((!t534) && u_is_con(s533, &n_option_46_Some)) {
    u_value v_ks = u_field(s533, 0);
    u_value t535;
    if (u_truth(f_check_46_covers_2(v_hs, v_ks))) {
      t535 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_ks, v_rest);
    } else {
      t535 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
    }
    t534 = t535;
  }
  if ((!t534) && u_is_con(s533, &n_option_46_None)) {
    t534 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
  }
  if (!t534) {
    u_no_case(s533);
  }
  return t534;
}

u_value f_check_46_first_missing_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_rest) {
  u_value s536 = v_ks;
  u_value t537 = NULL;
  if ((!t537) && u_is_con(s536, &n_list_46_Nil)) {
    t537 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t537) && u_is_con(s536, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s536, 0), 0);
    u_value v_args = u_field(u_field(s536, 0), 1);
    u_value v_more = u_field(s536, 1);
    u_value s538 = f_check_46_missing_4(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_rest));
    u_value t539 = NULL;
    if ((!t539) && u_is_con(s538, &n_option_46_Some)) {
      u_value v_ws = u_field(s538, 0);
      t539 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_witness_2(v_h, f_list_46_take_2(f_list_46_length_1(v_args), v_ws)), f_list_46_drop_2(f_list_46_length_1(v_args), v_ws)));
    }
    if ((!t539) && u_is_con(s538, &n_option_46_None)) {
      t539 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_more, v_rest);
    }
    if (!t539) {
      u_no_case(s538);
    }
    t537 = t539;
  }
  if (!t537) {
    u_no_case(s536);
  }
  return t537;
}

u_value f_check_46_other_missing_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s540 = f_check_46_missing_4(v_c, v_su, f_check_46_defaults_1(v_rows), v_rest);
  u_value t541 = NULL;
  if ((!t541) && u_is_con(s540, &n_option_46_Some)) {
    u_value v_ws = u_field(s540, 0);
    t541 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_uncovered_2(v_hs, v_cs), v_ws));
  }
  if ((!t541) && u_is_con(s540, &n_option_46_None)) {
    t541 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t541) {
    u_no_case(s540);
  }
  return t541;
}

u_value f_check_46_uncovered_2(u_value v_hs, u_value v_cs) {
  u_value s542 = v_cs;
  u_value t543 = NULL;
  if ((!t543) && u_is_con(s542, &n_option_46_Some)) {
    u_value v_ks = u_field(s542, 0);
    u_value t544;
    if (u_truth(u_eq(v_hs, u_mk_con(&n_list_46_Nil, 0)))) {
      t544 = u_mk_con(&n_syntax_46_PWild, 0);
    } else {
      t544 = f_check_46_first_uncovered_2(v_hs, v_ks);
    }
    t543 = t544;
  }
  if ((!t543) && u_is_con(s542, &n_option_46_None)) {
    t543 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if (!t543) {
    u_no_case(s542);
  }
  return t543;
}

u_value f_check_46_first_uncovered_2(u_value v_hs, u_value v_ks) {
  u_value s545 = v_ks;
  u_value t546 = NULL;
  if ((!t546) && u_is_con(s545, &n_list_46_Nil)) {
    t546 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if ((!t546) && u_is_con(s545, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s545, 0), 0);
    u_value v_args = u_field(u_field(s545, 0), 1);
    u_value v_rest = u_field(s545, 1);
    u_value t547;
    if (u_truth(f_list_46_contains_2(v_h, v_hs))) {
      t547 = f_check_46_first_uncovered_2(v_hs, v_rest);
    } else {
      t547 = f_check_46_witness_2(v_h, f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)));
    }
    t546 = t547;
  }
  if (!t546) {
    u_no_case(s545);
  }
  return t546;
}

void p_check_46_fresh(u_value *r_t, u_value *r_s) {
  {
    u_value t548 = u_mk_con(&n_syntax_46_TMeta, 1, u_add(f_check_46_State_46_count_1(*r_s), u_mk_int(1)));
    u_value t549 = f_check_46_State_46_count_58__61__2(*r_s, u_add(f_check_46_State_46_count_1(*r_s), u_mk_int(1)));
    *r_t = t548;
    *r_s = t549;
  }
}

void p_check_46_bind_meta(u_value v_i, u_value v_t, u_value *r_s) {
  {
    if (u_truth(f_check_46_occurs_3(f_check_46_State_46_sub_1(*r_s), v_i, v_t))) {
      {
        u_raise(u_mk_con(&n_check_46_InfiniteType, 2, u_mk_con(&n_syntax_46_TMeta, 1, v_i), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t)), "check.TypeError");
      }
    }
    u_value t550 = f_check_46_State_46_sub_58__61__2(*r_s, f_map_46_put_3(v_i, v_t, f_check_46_State_46_sub_1(*r_s)));
    *r_s = t550;
  }
}

void p_check_46_unify(u_value v_t1, u_value v_t2, u_value *r_s) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value t551 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t1);
    v_a = t551;
    u_value t552 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t2);
    v_b = t552;
    u_value s553 = u_mk_tuple(2, v_a, v_b);
    int d554 = 0;
    if ((!d554) && (u_is_con(u_field(s553, 0), &n_syntax_46_TMeta) && u_is_con(u_field(s553, 1), &n_syntax_46_TMeta))) {
      u_value v_i = u_field(u_field(s553, 0), 0);
      u_value v_j = u_field(u_field(s553, 1), 0);
      d554 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_i, v_j))) {
          {
            p_check_46_bind_meta(v_i, v_b, r_s);
          }
        }
      }
    }
    if ((!d554) && u_is_con(u_field(s553, 0), &n_syntax_46_TMeta)) {
      u_value v_i = u_field(u_field(s553, 0), 0);
      d554 = 1;
      {
        p_check_46_bind_meta(v_i, v_b, r_s);
      }
    }
    if ((!d554) && u_is_con(u_field(s553, 1), &n_syntax_46_TMeta)) {
      u_value v_j = u_field(u_field(s553, 1), 0);
      d554 = 1;
      {
        p_check_46_bind_meta(v_j, v_a, r_s);
      }
    }
    if ((!d554) && (u_is_con(u_field(s553, 0), &n_syntax_46_TParam) && u_is_con(u_field(s553, 1), &n_syntax_46_TParam))) {
      u_value v_x = u_field(u_field(s553, 0), 0);
      u_value v_y = u_field(u_field(s553, 1), 0);
      d554 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_x, v_y))) {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, v_a, v_b), "check.TypeError");
          }
        }
      }
    }
    if ((!d554) && (u_is_con(u_field(s553, 0), &n_syntax_46_TSort) && u_is_con(u_field(s553, 1), &n_syntax_46_TSort))) {
      u_value v_f = u_field(u_field(s553, 0), 0);
      u_value v_xs = u_field(u_field(s553, 0), 1);
      u_value v_g = u_field(u_field(s553, 1), 0);
      u_value v_ys = u_field(u_field(s553, 1), 1);
      d554 = 1;
      {
        u_value t555 = u_eq(v_f, v_g);
        if (u_truth(t555)) {
          t555 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys));
        }
        if (u_truth(t555)) {
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
    if ((!d554) && (u_is_con(u_field(s553, 0), &n_syntax_46_TFun) && u_is_con(u_field(s553, 1), &n_syntax_46_TFun))) {
      u_value v_xs = u_field(u_field(s553, 0), 0);
      u_value v_r1 = u_field(u_field(s553, 0), 1);
      u_value v_ys = u_field(u_field(s553, 1), 0);
      u_value v_r2 = u_field(u_field(s553, 1), 1);
      d554 = 1;
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
    if ((!d554) && (u_is_con(u_field(s553, 0), &n_syntax_46_TTuple) && u_is_con(u_field(s553, 1), &n_syntax_46_TTuple))) {
      u_value v_xs = u_field(u_field(s553, 0), 0);
      u_value v_ys = u_field(u_field(s553, 1), 0);
      d554 = 1;
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
    if (!d554) {
      d554 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t2)), "check.TypeError");
      }
    }
    if (!d554) {
      u_no_case(s553);
    }
  }
}

void p_check_46_unifies(u_value v_xs, u_value v_ys, u_value *r_s) {
  {
    u_value s556 = u_mk_tuple(2, v_xs, v_ys);
    int d557 = 0;
    if ((!d557) && (u_is_con(u_field(s556, 0), &n_list_46_Cons) && u_is_con(u_field(s556, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s556, 0), 0);
      u_value v_xs1 = u_field(u_field(s556, 0), 1);
      u_value v_y = u_field(u_field(s556, 1), 0);
      u_value v_ys1 = u_field(u_field(s556, 1), 1);
      d557 = 1;
      {
        p_check_46_unify(v_x, v_y, r_s);
        p_check_46_unifies(v_xs1, v_ys1, r_s);
      }
    }
    if ((!d557) && (u_is_con(u_field(s556, 0), &n_list_46_Nil) && u_is_con(u_field(s556, 1), &n_list_46_Nil))) {
      d557 = 1;
      {
      }
    }
    if (!d557) {
      d557 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)), "check.TypeError");
      }
    }
    if (!d557) {
      u_no_case(s556);
    }
  }
}

void p_check_46_fresh_list(u_value v_xs, u_value *r_ts, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s558 = v_xs;
    int d559 = 0;
    if ((!d559) && u_is_con(s558, &n_list_46_Nil)) {
      d559 = 1;
      {
        u_value t560 = u_mk_con(&n_list_46_Nil, 0);
        *r_ts = t560;
      }
    }
    if ((!d559) && u_is_con(s558, &n_list_46_Cons)) {
      u_value v_more = u_field(s558, 1);
      d559 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_fresh_list(v_more, &v_rest, r_s);
        u_value t561 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rest);
        *r_ts = t561;
      }
    }
    if (!d559) {
      u_no_case(s558);
    }
  }
}

void p_check_46_instance(u_value v_ps, u_value *r_m, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s562 = v_ps;
    int d563 = 0;
    if ((!d563) && u_is_con(s562, &n_list_46_Nil)) {
      d563 = 1;
      {
        u_value t564 = u_mk_con(&n_list_46_Nil, 0);
        *r_m = t564;
      }
    }
    if ((!d563) && u_is_con(s562, &n_list_46_Cons)) {
      u_value v_p = u_field(s562, 0);
      u_value v_qs = u_field(s562, 1);
      d563 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_instance(v_qs, &v_rest, r_s);
        u_value t565 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_p, v_t), v_rest);
        *r_m = t565;
      }
    }
    if (!d563) {
      u_no_case(s562);
    }
  }
}

void p_check_46_instantiate(u_value v_sc, u_value *r_t, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value s566 = v_sc;
    int d567 = 0;
    if (!d567) {
      u_value v_ps = u_field(s566, 0);
      u_value v_u = u_field(s566, 1);
      d567 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        u_value t568 = f_check_46_subst_2(f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u), v_m);
        *r_t = t568;
      }
    }
    if (!d567) {
      u_no_case(s566);
    }
  }
}

void p_check_46_unique(u_value v_ns) {
  {
    u_value s569 = f_list_46_repeated_1(v_ns);
    int d570 = 0;
    if ((!d570) && u_is_con(s569, &n_option_46_Some)) {
      u_value v_n = u_field(s569, 0);
      d570 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, f_syntax_46_qualified_1(v_n)), "check.TypeError");
      }
    }
    if ((!d570) && u_is_con(s569, &n_option_46_None)) {
      d570 = 1;
      {
      }
    }
    if (!d570) {
      u_no_case(s569);
    }
  }
}

void p_check_46_unique_ids(u_value v_ns) {
  {
    u_value s571 = f_list_46_repeated_1(v_ns);
    int d572 = 0;
    if ((!d572) && u_is_con(s571, &n_option_46_Some)) {
      u_value v_n = u_field(s571, 0);
      d572 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, v_n), "check.TypeError");
      }
    }
    if ((!d572) && u_is_con(s571, &n_option_46_None)) {
      d572 = 1;
      {
      }
    }
    if (!d572) {
      u_no_case(s571);
    }
  }
}

void p_check_46_one_of(u_value v_n, u_value v_ns, u_value *r_m) {
  {
    u_value s573 = v_ns;
    int d574 = 0;
    if ((!d574) && (u_is_con(s573, &n_list_46_Cons) && u_is_con(u_field(s573, 1), &n_list_46_Nil))) {
      u_value v_k = u_field(s573, 0);
      d574 = 1;
      {
        u_value t575 = v_k;
        *r_m = t575;
      }
    }
    if ((!d574) && u_is_con(s573, &n_list_46_Nil)) {
      d574 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if (!d574) {
      d574 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ns), "check.TypeError");
      }
    }
    if (!d574) {
      u_no_case(s573);
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
    u_value s576 = v_t;
    int d577 = 0;
    if ((!d577) && u_is_con(s576, &n_syntax_46_THole)) {
      d577 = 1;
      {
        p_check_46_fresh(r_u, r_s);
      }
    }
    if ((!d577) && u_is_con(s576, &n_syntax_46_TParam)) {
      d577 = 1;
      {
        u_value t578 = v_t;
        *r_u = t578;
      }
    }
    if ((!d577) && u_is_con(s576, &n_syntax_46_TMeta)) {
      d577 = 1;
      {
        u_value t579 = v_t;
        *r_u = t579;
      }
    }
    if ((!d577) && u_is_con(s576, &n_syntax_46_TFun)) {
      u_value v_args = u_field(s576, 0);
      u_value v_r = u_field(s576, 1);
      d577 = 1;
      {
        p_check_46_normalizes(v_args, &v_us, r_s);
        p_check_46_normalize(v_r, &v_v, r_s);
        u_value t580 = u_mk_con(&n_syntax_46_TFun, 2, v_us, v_v);
        *r_u = t580;
      }
    }
    if ((!d577) && u_is_con(s576, &n_syntax_46_TTuple)) {
      u_value v_ts = u_field(s576, 0);
      d577 = 1;
      {
        p_check_46_normalizes(v_ts, &v_us, r_s);
        u_value t581 = u_mk_con(&n_syntax_46_TTuple, 1, v_us);
        *r_u = t581;
      }
    }
    if ((!d577) && u_is_con(s576, &n_syntax_46_TSort)) {
      u_value v_n = u_field(s576, 0);
      u_value v_args = u_field(s576, 1);
      d577 = 1;
      {
        if (u_truth(f_check_46_basic_1(v_n))) {
          {
            p_check_46_arguments(v_n, u_mk_int(0), v_args);
            u_value t582 = v_t;
            *r_u = t582;
          }
        } else {
          {
            u_value s584 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam583, 1, *r_s));
            int d585 = 0;
            if ((!d585) && (u_is_con(s584, &n_list_46_Cons) && u_is_con(u_field(s584, 1), &n_list_46_Nil))) {
              u_value v_m = u_field(s584, 0);
              d585 = 1;
              {
                p_check_46_normalize_sort(v_m, v_args, r_u, r_s);
              }
            }
            if ((!d585) && u_is_con(s584, &n_list_46_Nil)) {
              d585 = 1;
              {
                u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
              }
            }
            if (!d585) {
              u_value v_ms = s584;
              d585 = 1;
              {
                u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ms), "check.TypeError");
              }
            }
            if (!d585) {
              u_no_case(s584);
            }
          }
        }
      }
    }
    if (!d577) {
      u_no_case(s576);
    }
  }
}

void p_check_46_normalize_sort(u_value v_n, u_value v_args, u_value *r_u, u_value *r_s) {
  {
    u_value v_us = NULL;
    u_value s586 = f_ctx_46_data_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
    int d587 = 0;
    if ((!d587) && u_is_con(s586, &n_option_46_Some)) {
      u_value v_k = u_field(s586, 0);
      d587 = 1;
      {
        p_check_46_arguments(v_n, v_k, v_args);
        p_check_46_normalizes(v_args, &v_us, r_s);
        u_value t588 = u_mk_con(&n_syntax_46_TSort, 2, v_n, v_us);
        *r_u = t588;
      }
    }
    if ((!d587) && u_is_con(s586, &n_option_46_None)) {
      d587 = 1;
      {
        u_value s589 = f_ctx_46_alias_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
        int d590 = 0;
        if ((!d590) && u_is_con(s589, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s589, 0), 0);
          u_value v_rhs = u_field(u_field(s589, 0), 1);
          d590 = 1;
          {
            p_check_46_arguments(v_n, f_list_46_length_1(v_ps), v_args);
            p_check_46_normalizes(v_args, &v_us, r_s);
            u_value t591 = f_check_46_subst_2(v_rhs, f_list_46_zip_2(f_check_46_params_1(v_ps), v_us));
            *r_u = t591;
          }
        }
        if ((!d590) && u_is_con(s589, &n_option_46_None)) {
          d590 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
          }
        }
        if (!d590) {
          u_no_case(s589);
        }
      }
    }
    if (!d587) {
      u_no_case(s586);
    }
  }
}

void p_check_46_normalizes(u_value v_ts, u_value *r_us, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_rest = NULL;
    u_value s592 = v_ts;
    int d593 = 0;
    if ((!d593) && u_is_con(s592, &n_list_46_Nil)) {
      d593 = 1;
      {
        u_value t594 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t594;
      }
    }
    if ((!d593) && u_is_con(s592, &n_list_46_Cons)) {
      u_value v_t = u_field(s592, 0);
      u_value v_more = u_field(s592, 1);
      d593 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalizes(v_more, &v_rest, r_s);
        u_value t595 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t595;
      }
    }
    if (!d593) {
      u_no_case(s592);
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
        p_check_46_known(v_t, v_what, &v_u, r_s);
        p_check_46_knowns(v_more, v_what, &v_rest, r_s);
        u_value t599 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t599;
      }
    }
    if (!d597) {
      u_no_case(s596);
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
    u_value s600 = v_xs;
    int d601 = 0;
    if ((!d601) && u_is_con(s600, &n_list_46_Nil)) {
      d601 = 1;
      {
        u_value t602 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t602;
      }
    }
    if ((!d601) && (u_is_con(s600, &n_list_46_Cons) && u_is_con(u_field(s600, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s600, 0), 0);
      u_value v_t = u_field(u_field(s600, 0), 1);
      u_value v_more = u_field(s600, 1);
      d601 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalize_vars(v_more, &v_rest, r_s);
        u_value t603 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t603;
      }
    }
    if (!d601) {
      u_no_case(s600);
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
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_known_vars(v_more, &v_rest, r_s);
        u_value t607 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t607;
      }
    }
    if (!d605) {
      u_no_case(s604);
    }
  }
}

void p_check_46_resolve_name(u_value v_s, u_value v_env, u_value v_n, u_value v_k, u_value *r_m, u_value *r_sc) {
  {
    u_value s608 = f_check_46_candidates_3(v_s, v_env, v_n);
    int d609 = 0;
    if ((!d609) && u_is_con(s608, &n_list_46_Nil)) {
      d609 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if ((!d609) && (u_is_con(s608, &n_list_46_Cons) && u_is_con(u_field(s608, 1), &n_list_46_Nil))) {
      u_value v_m1 = u_field(u_field(s608, 0), 0);
      u_value v_one = u_field(u_field(s608, 0), 1);
      d609 = 1;
      {
        u_value t610 = v_m1;
        u_value t611 = v_one;
        *r_m = t610;
        *r_sc = t611;
      }
    }
    if (!d609) {
      u_value v_cs = s608;
      d609 = 1;
      {
        if (u_truth(u_lt(v_k, u_mk_int(0)))) {
          {
            u_raise(u_mk_con(&n_check_46_OverloadedUnapplied, 1, v_n), "check.TypeError");
          }
        }
        u_value s612 = f_check_46_with_arity_2(v_cs, v_k);
        int d613 = 0;
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
        if ((!d613) && u_is_con(s612, &n_list_46_Nil)) {
          d613 = 1;
          {
            u_raise(u_mk_con(&n_check_46_WrongArity, 2, v_n, v_k), "check.TypeError");
          }
        }
        if (!d613) {
          u_value v_more = s612;
          d613 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, f_list_46_firsts_1(v_more)), "check.TypeError");
          }
        }
        if (!d613) {
          u_no_case(s612);
        }
      }
    }
    if (!d609) {
      u_no_case(s608);
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
    u_value t616 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_u);
    *r_x = t616;
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
    u_value s617 = v_e;
    int d618 = 0;
    if ((!d618) && u_is_con(s617, &n_syntax_46_ELit)) {
      u_value v_v = u_field(s617, 0);
      d618 = 1;
      {
        u_value t619 = v_e;
        u_value t620 = f_check_46_literal_1(v_v);
        *r_e1 = t619;
        *r_t = t620;
      }
    }
    if ((!d618) && (u_is_con(s617, &n_syntax_46_EVar) && u_is_con(u_field(s617, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s617, 0), 0);
      u_value v_a = u_field(u_field(s617, 0), 1);
      d618 = 1;
      {
        p_check_46_resolve_name(*r_s, v_env, v_n, u_mk_int(-1), &v_m, &v_sc);
        p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
        u_value t621 = u_mk_con(&n_syntax_46_EVar, 1, v_x);
        u_value t622 = f_syntax_46_Var_46_typ_1(v_x);
        *r_e1 = t621;
        *r_t = t622;
      }
    }
    if ((!d618) && u_is_con(s617, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s617, 0);
      u_value v_args = u_field(s617, 1);
      d618 = 1;
      {
        p_check_46_application(v_env, v_f, v_args, r_e1, r_t, r_s);
      }
    }
    if ((!d618) && u_is_con(s617, &n_syntax_46_ELam)) {
      u_value v_bound = u_field(s617, 0);
      u_value v_body = u_field(s617, 1);
      d618 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t623 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, v_b);
        u_value t624 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), v_u);
        *r_e1 = t623;
        *r_t = t624;
      }
    }
    if ((!d618) && u_is_con(s617, &n_syntax_46_ELet)) {
      u_value v_bound = u_field(s617, 0);
      u_value v_body = u_field(s617, 1);
      d618 = 1;
      {
        p_check_46_formals(f_list_46_firsts_1(v_bound), &v_xs, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_bound), f_syntax_46_types_1(v_xs), &v_es, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_generalized_3(f_check_46_State_46_sub_1(*r_s), v_env, v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t625 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        u_value t626 = v_u;
        *r_e1 = t625;
        *r_t = t626;
      }
    }
    if ((!d618) && u_is_con(s617, &n_syntax_46_EIte)) {
      u_value v_cond = u_field(s617, 0);
      u_value v_left = u_field(s617, 1);
      u_value v_right = u_field(s617, 2);
      d618 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(v_env, v_left, &v_l, &v_u, r_s);
        p_check_46_check(v_env, v_right, v_u, &v_b, r_s);
        u_value t627 = u_mk_con(&n_syntax_46_EIte, 3, v_c, v_l, v_b);
        u_value t628 = v_u;
        *r_e1 = t627;
        *r_t = t628;
      }
    }
    if ((!d618) && u_is_con(s617, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s617, 0);
      u_value v_bound = u_field(s617, 1);
      u_value v_lists = u_field(s617, 2);
      u_value v_cond = u_field(s617, 3);
      d618 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_same_length(f_list_46_length_1(v_xs), f_list_46_length_1(v_lists));
        p_check_46_generators(v_env, v_xs, v_lists, &v_es, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_result, &v_b, &v_u, r_s);
        u_value t629 = u_mk_con(&n_syntax_46_EComp, 4, v_b, v_xs, v_es, v_c);
        u_value t630 = f_check_46_tlist_1(v_u);
        *r_e1 = t629;
        *r_t = t630;
      }
    }
    if ((!d618) && u_is_con(s617, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s617, 0);
      d618 = 1;
      {
        p_check_46_infers(v_env, v_items, &v_es, &v_ts, r_s);
        u_value t631 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
        u_value t632 = u_mk_con(&n_syntax_46_TTuple, 1, v_ts);
        *r_e1 = t631;
        *r_t = t632;
      }
    }
    if ((!d618) && u_is_con(s617, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s617, 0);
      u_value v_a = u_field(s617, 1);
      d618 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_normalize(v_a, &v_w, r_s);
        p_check_46_unify(v_u, v_w, r_s);
        p_check_46_fresh(r_t, r_s);
        u_value t633 = u_mk_con(&n_syntax_46_ERaise, 2, v_c, v_u);
        *r_e1 = t633;
      }
    }
    if ((!d618) && u_is_con(s617, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s617, 0);
      u_value v_cases = u_field(s617, 1);
      d618 = 1;
      {
        p_check_46_fresh(r_t, r_s);
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_expr_branches(v_env, v_cases, v_u, *r_t, &v_bs, r_s);
        u_value t634 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e635 = &t634;
        u_value l636 = v_bs;
        while (u_is_con(l636, &n_list_46_Cons)) {
          u_value v_br = u_field(l636, 0);
          if (u_truth(u_mk_bool(1))) {
            e635 = u_append(e635, f_check_46_case_of_1(v_br));
          }
          l636 = u_field(l636, 1);
        }
        p_check_46_coverage(*r_s, v_u, t634);
        u_value t637 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, v_bs);
        *r_e1 = t637;
      }
    }
    if (!d618) {
      u_no_case(s617);
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
    u_value s638 = v_f;
    int d639 = 0;
    if ((!d639) && (u_is_con(s638, &n_syntax_46_EVar) && (u_is_con(u_field(s638, 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(s638, 0), 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(u_field(s638, 0), 0), 0), &n_option_46_None))))) {
      u_value v_b = u_field(u_field(u_field(s638, 0), 0), 1);
      u_value v_a = u_field(u_field(s638, 0), 1);
      u_value t640 = f_prelude_46__62__2(u_str_size(v_b), u_mk_int(1));
      if (u_truth(t640)) {
        t640 = u_eq(u_str_get(v_b, u_mk_int(0)), u_mk_char(46));
      }
      if (u_truth(t640)) {
        d639 = 1;
        {
          p_check_46_field_access(v_env, v_b, v_a, v_args, r_e1, r_t, r_s);
        }
      }
    }
    if ((!d639) && (u_is_con(s638, &n_syntax_46_EVar) && u_is_con(u_field(s638, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s638, 0), 0);
      u_value v_a = u_field(u_field(s638, 0), 1);
      d639 = 1;
      {
        if (u_truth(f_prelude_46__62__2(f_list_46_length_1(f_check_46_candidates_3(*r_s, v_env, v_n)), u_mk_int(1)))) {
          {
            p_check_46_resolve_name(*r_s, v_env, v_n, f_list_46_length_1(v_args), &v_m, &v_sc);
            p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
            p_check_46_apply(v_env, f_syntax_46_Var_46_typ_1(v_x), v_args, &v_args1, r_t, r_s);
            u_value t641 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), v_args1);
            *r_e1 = t641;
          }
        } else {
          {
            p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
            p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
            u_value t642 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
            *r_e1 = t642;
          }
        }
      }
    }
    if (!d639) {
      d639 = 1;
      {
        p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
        p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
        u_value t643 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
        *r_e1 = t643;
      }
    }
    if (!d639) {
      u_no_case(s638);
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
    u_value s644 = v_args;
    int d645 = 0;
    if ((!d645) && u_is_con(s644, &n_list_46_Cons)) {
      u_value v_arg = u_field(s644, 0);
      u_value v_rest = u_field(s644, 1);
      d645 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_arg1, &v_u, r_s);
        u_value s646 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u);
        int d647 = 0;
        if ((!d647) && (u_is_con(s646, &n_syntax_46_TSort) && (u_is_con(u_field(s646, 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(s646, 0), 0), &n_option_46_Some)))) {
          u_value v_m = u_field(u_field(u_field(s646, 0), 0), 0);
          u_value v_tn = u_field(u_field(s646, 0), 1);
          d647 = 1;
          {
            u_value t648 = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), u_concat(v_tn, v_f));
            v_acc = t648;
          }
        }
        if (!d647) {
          u_value v_v = s646;
          d647 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownFieldType, 2, v_f, v_v), "check.TypeError");
          }
        }
        if (!d647) {
          u_no_case(s646);
        }
        u_value s649 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(*r_s), v_acc);
        int d650 = 0;
        if ((!d650) && (u_is_con(s649, &n_list_46_Cons) && u_is_con(u_field(s649, 1), &n_list_46_Nil))) {
          u_value v_sc = u_field(s649, 0);
          d650 = 1;
          {
            p_check_46_use(v_acc, v_a, v_sc, &v_x, r_s);
          }
        }
        if (!d650) {
          d650 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownField, 1, v_acc), "check.TypeError");
          }
        }
        if (!d650) {
          u_no_case(s649);
        }
        p_check_46_infers(v_env, v_rest, &v_rest1, &v_us, r_s);
        p_check_46_fresh(r_t, r_s);
        p_check_46_unify(f_syntax_46_Var_46_typ_1(v_x), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_u, v_us), *r_t), r_s);
        u_value t651 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), u_mk_con(&n_list_46_Cons, 2, v_arg1, v_rest1));
        *r_e1 = t651;
      }
    }
    if (!d645) {
      d645 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, u_mk_int(1), f_list_46_length_1(v_args)), "check.TypeError");
      }
    }
    if (!d645) {
      u_no_case(s644);
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
    u_value s652 = u_mk_tuple(2, v_es, v_ts);
    int d653 = 0;
    if ((!d653) && (u_is_con(u_field(s652, 0), &n_list_46_Cons) && u_is_con(u_field(s652, 1), &n_list_46_Cons))) {
      u_value v_e = u_field(u_field(s652, 0), 0);
      u_value v_more = u_field(u_field(s652, 0), 1);
      u_value v_t = u_field(u_field(s652, 1), 0);
      u_value v_ts1 = u_field(u_field(s652, 1), 1);
      d653 = 1;
      {
        p_check_46_check(v_env, v_e, v_t, &v_e1, r_s);
        p_check_46_checks(v_env, v_more, v_ts1, &v_rest, r_s);
        u_value t654 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        *r_es1 = t654;
      }
    }
    if ((!d653) && (u_is_con(u_field(s652, 0), &n_list_46_Nil) && u_is_con(u_field(s652, 1), &n_list_46_Nil))) {
      d653 = 1;
      {
        u_value t655 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t655;
      }
    }
    if (!d653) {
      d653 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_es)), "check.TypeError");
      }
    }
    if (!d653) {
      u_no_case(s652);
    }
  }
}

void p_check_46_infers(u_value v_env, u_value v_es, u_value *r_es1, u_value *r_ts, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value v_us = NULL;
    u_value s656 = v_es;
    int d657 = 0;
    if ((!d657) && u_is_con(s656, &n_list_46_Nil)) {
      d657 = 1;
      {
        u_value t658 = u_mk_con(&n_list_46_Nil, 0);
        u_value t659 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t658;
        *r_ts = t659;
      }
    }
    if ((!d657) && u_is_con(s656, &n_list_46_Cons)) {
      u_value v_e = u_field(s656, 0);
      u_value v_more = u_field(s656, 1);
      d657 = 1;
      {
        p_check_46_infer(v_env, v_e, &v_e1, &v_t, r_s);
        p_check_46_infers(v_env, v_more, &v_rest, &v_us, r_s);
        u_value t660 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        u_value t661 = u_mk_con(&n_list_46_Cons, 2, v_t, v_us);
        *r_es1 = t660;
        *r_ts = t661;
      }
    }
    if (!d657) {
      u_no_case(s656);
    }
  }
}

void p_check_46_generators(u_value v_env, u_value v_xs, u_value v_lists, u_value *r_lists1, u_value *r_s) {
  {
    u_value v_l = NULL;
    u_value v_rest = NULL;
    u_value s662 = u_mk_tuple(2, v_xs, v_lists);
    int d663 = 0;
    if ((!d663) && (u_is_con(u_field(s662, 0), &n_list_46_Cons) && u_is_con(u_field(s662, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s662, 0), 0);
      u_value v_ys = u_field(u_field(s662, 0), 1);
      u_value v_e = u_field(u_field(s662, 1), 0);
      u_value v_es = u_field(u_field(s662, 1), 1);
      d663 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tlist_1(f_syntax_46_Var_46_typ_1(v_x)), &v_l, r_s);
        p_check_46_generators(f_list_46__43__43__2(f_check_46_bind_1(u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0))), v_env), v_ys, v_es, &v_rest, r_s);
        u_value t664 = u_mk_con(&n_list_46_Cons, 2, v_l, v_rest);
        *r_lists1 = t664;
      }
    }
    if (!d663) {
      d663 = 1;
      {
        u_value t665 = u_mk_con(&n_list_46_Nil, 0);
        *r_lists1 = t665;
      }
    }
    if (!d663) {
      u_no_case(s662);
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
    u_value s666 = v_cases;
    int d667 = 0;
    if ((!d667) && u_is_con(s666, &n_list_46_Nil)) {
      d667 = 1;
      {
        u_value t668 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t668;
      }
    }
    if ((!d667) && (u_is_con(s666, &n_list_46_Cons) && u_is_con(u_field(s666, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s666, 0), 0);
      u_value v_g = u_field(u_field(s666, 0), 1);
      u_value v_b = u_field(u_field(s666, 0), 2);
      u_value v_more = u_field(s666, 1);
      d667 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_b, v_r, &v_b1, r_s);
        p_check_46_expr_branches(v_env, v_more, v_t, v_r, &v_rest, r_s);
        u_value t669 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t669;
      }
    }
    if (!d667) {
      u_no_case(s666);
    }
  }
}

void p_check_46_guard(u_value v_env, u_value v_g, u_value *r_g1, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value s670 = v_g;
    int d671 = 0;
    if ((!d671) && u_is_con(s670, &n_option_46_Some)) {
      u_value v_e = u_field(s670, 0);
      d671 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tbool_0(), &v_e1, r_s);
        u_value t672 = u_mk_con(&n_option_46_Some, 1, v_e1);
        *r_g1 = t672;
      }
    }
    if ((!d671) && u_is_con(s670, &n_option_46_None)) {
      d671 = 1;
      {
        u_value t673 = u_mk_con(&n_option_46_None, 0);
        *r_g1 = t673;
      }
    }
    if (!d671) {
      u_no_case(s670);
    }
  }
}

void p_check_46_coverage(u_value v_s, u_value v_t, u_value v_cs) {
  {
    p_check_46_reachable(v_s, v_t, u_mk_con(&n_list_46_Nil, 0), v_cs);
    u_value t674 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e675 = &t674;
    u_value l676 = f_check_46_unguarded_1(v_cs);
    while (u_is_con(l676, &n_list_46_Cons)) {
      u_value v_p = u_field(l676, 0);
      if (u_truth(u_mk_bool(1))) {
        e675 = u_append(e675, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
      }
      l676 = u_field(l676, 1);
    }
    u_value s677 = f_check_46_missing_4(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t674, u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
    int d678 = 0;
    if ((!d678) && (u_is_con(s677, &n_option_46_Some) && u_is_con(u_field(s677, 0), &n_list_46_Cons))) {
      u_value v_w = u_field(u_field(s677, 0), 0);
      d678 = 1;
      {
        u_raise(u_mk_con(&n_check_46_NotExhaustive, 1, v_w), "check.TypeError");
      }
    }
    if (!d678) {
      d678 = 1;
      {
      }
    }
    if (!d678) {
      u_no_case(s677);
    }
  }
}

u_value f_check_46_unguarded_1(u_value v_cs) {
  u_value s679 = v_cs;
  u_value t680 = NULL;
  if ((!t680) && u_is_con(s679, &n_list_46_Nil)) {
    t680 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t680) && (u_is_con(s679, &n_list_46_Cons) && u_equals(u_field(u_field(s679, 0), 1), u_mk_bool(1)))) {
    u_value v_p = u_field(u_field(s679, 0), 0);
    u_value v_rest = u_field(s679, 1);
    t680 = u_mk_con(&n_list_46_Cons, 2, v_p, f_check_46_unguarded_1(v_rest));
  }
  if ((!t680) && (u_is_con(s679, &n_list_46_Cons) && u_equals(u_field(u_field(s679, 0), 1), u_mk_bool(0)))) {
    u_value v_rest = u_field(s679, 1);
    t680 = f_check_46_unguarded_1(v_rest);
  }
  if (!t680) {
    u_no_case(s679);
  }
  return t680;
}

void p_check_46_reachable(u_value v_s, u_value v_t, u_value v_before, u_value v_cs) {
  {
    u_value s681 = v_cs;
    int d682 = 0;
    if ((!d682) && u_is_con(s681, &n_list_46_Nil)) {
      d682 = 1;
      {
      }
    }
    if ((!d682) && u_is_con(s681, &n_list_46_Cons)) {
      u_value v_p = u_field(u_field(s681, 0), 0);
      u_value v_open = u_field(u_field(s681, 0), 1);
      u_value v_rest = u_field(s681, 1);
      d682 = 1;
      {
        u_value t683 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e684 = &t683;
        u_value l685 = v_before;
        while (u_is_con(l685, &n_list_46_Cons)) {
          u_value v_q = u_field(l685, 0);
          if (u_truth(u_mk_bool(1))) {
            e684 = u_append(e684, u_mk_con(&n_list_46_Cons, 2, v_q, u_mk_con(&n_list_46_Nil, 0)));
          }
          l685 = u_field(l685, 1);
        }
        if (u_truth(f_prelude_46_not_1(f_check_46_useful_5(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t683, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)))))) {
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
    if (!d682) {
      u_no_case(s681);
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
    u_value s686 = v_p;
    int d687 = 0;
    if ((!d687) && u_is_con(s686, &n_syntax_46_PWild)) {
      d687 = 1;
      {
        u_value t688 = v_p;
        u_value t689 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t688;
        *r_xs = t689;
      }
    }
    if ((!d687) && u_is_con(s686, &n_syntax_46_PLit)) {
      u_value v_v = u_field(s686, 0);
      d687 = 1;
      {
        p_check_46_unify(f_check_46_literal_1(v_v), v_t, r_s);
        u_value t690 = v_p;
        u_value t691 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t690;
        *r_xs = t691;
      }
    }
    if ((!d687) && (u_is_con(s686, &n_syntax_46_PVar) && u_is_con(u_field(s686, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s686, 0), 0);
      u_value v_a = u_field(u_field(s686, 0), 1);
      d687 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam692, 1, *r_s)), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            p_check_46_binders(u_mk_con(&n_syntax_46_PApp, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_t, r_p1, r_xs, r_s);
          }
        } else {
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_u, v_t, r_s);
            u_value t693 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u));
            u_value t694 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), u_mk_con(&n_list_46_Nil, 0));
            *r_p1 = t693;
            *r_xs = t694;
          }
        }
      }
    }
    if ((!d687) && u_is_con(s686, &n_syntax_46_PApp)) {
      u_value v_n = u_field(s686, 0);
      u_value v_args = u_field(s686, 1);
      d687 = 1;
      {
        u_value s696 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam695, 1, *r_s));
        int d697 = 0;
        if ((!d697) && (u_is_con(s696, &n_list_46_Cons) && u_is_con(u_field(s696, 1), &n_list_46_Nil))) {
          u_value v_k1 = u_field(s696, 0);
          d697 = 1;
          {
            u_value t698 = v_k1;
            v_k = t698;
          }
        }
        if ((!d697) && u_is_con(s696, &n_list_46_Nil)) {
          d697 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d697) {
          u_value v_ks = s696;
          d697 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ks), "check.TypeError");
          }
        }
        if (!d697) {
          u_no_case(s696);
        }
        u_value s699 = f_ctx_46_constr_of_2(f_check_46_State_46_ctx_1(*r_s), v_k);
        int d700 = 0;
        if ((!d700) && u_is_con(s699, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s699, 0), 0);
          u_value v_ks = u_field(u_field(s699, 0), 1);
          u_value v_r = u_field(u_field(s699, 0), 2);
          d700 = 1;
          {
            p_check_46_same_length(f_list_46_length_1(v_ks), f_list_46_length_1(v_args));
            p_check_46_instance(v_ps, &v_m, r_s);
            p_check_46_unify(f_check_46_subst_2(v_r, v_m), v_t, r_s);
            u_value t701 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e702 = &t701;
            u_value l703 = v_ks;
            while (u_is_con(l703, &n_list_46_Cons)) {
              u_value v_kt = u_field(l703, 0);
              if (u_truth(u_mk_bool(1))) {
                e702 = u_append(e702, f_check_46_subst_2(v_kt, v_m));
              }
              l703 = u_field(l703, 1);
            }
            p_check_46_binder_list(v_args, t701, &v_qs, r_xs, r_s);
            u_value t704 = u_mk_con(&n_syntax_46_PApp, 2, v_k, v_qs);
            *r_p1 = t704;
          }
        }
        if ((!d700) && u_is_con(s699, &n_option_46_None)) {
          d700 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d700) {
          u_no_case(s699);
        }
      }
    }
    if ((!d687) && u_is_con(s686, &n_syntax_46_PTuple)) {
      u_value v_items = u_field(s686, 0);
      d687 = 1;
      {
        p_check_46_fresh_list(v_items, &v_ts, r_s);
        p_check_46_unify(u_mk_con(&n_syntax_46_TTuple, 1, v_ts), v_t, r_s);
        p_check_46_binder_list(v_items, v_ts, &v_qs, r_xs, r_s);
        u_value t705 = u_mk_con(&n_syntax_46_PTuple, 1, v_qs);
        *r_p1 = t705;
      }
    }
    if (!d687) {
      u_no_case(s686);
    }
  }
}

void p_check_46_binder_list(u_value v_ps, u_value v_ts, u_value *r_qs, u_value *r_xs, u_value *r_s) {
  {
    u_value v_q = NULL;
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value v_zs = NULL;
    u_value s706 = u_mk_tuple(2, v_ps, v_ts);
    int d707 = 0;
    if ((!d707) && (u_is_con(u_field(s706, 0), &n_list_46_Cons) && u_is_con(u_field(s706, 1), &n_list_46_Cons))) {
      u_value v_p = u_field(u_field(s706, 0), 0);
      u_value v_more = u_field(u_field(s706, 0), 1);
      u_value v_t = u_field(u_field(s706, 1), 0);
      u_value v_ts1 = u_field(u_field(s706, 1), 1);
      d707 = 1;
      {
        p_check_46_binders(v_p, v_t, &v_q, &v_ys, r_s);
        p_check_46_binder_list(v_more, v_ts1, &v_rest, &v_zs, r_s);
        u_value t708 = u_mk_con(&n_list_46_Cons, 2, v_q, v_rest);
        u_value t709 = f_list_46__43__43__2(v_ys, v_zs);
        *r_qs = t708;
        *r_xs = t709;
      }
    }
    if ((!d707) && (u_is_con(u_field(s706, 0), &n_list_46_Nil) && u_is_con(u_field(s706, 1), &n_list_46_Nil))) {
      d707 = 1;
      {
        u_value t710 = u_mk_con(&n_list_46_Nil, 0);
        u_value t711 = u_mk_con(&n_list_46_Nil, 0);
        *r_qs = t710;
        *r_xs = t711;
      }
    }
    if (!d707) {
      d707 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_ps)), "check.TypeError");
      }
    }
    if (!d707) {
      u_no_case(s706);
    }
  }
}

void p_check_46_variable(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s712 = v_x;
    int d713 = 0;
    if ((!d713) && u_is_con(s712, &n_syntax_46_Var)) {
      u_value v_n = u_field(s712, 0);
      u_value v_a = u_field(s712, 1);
      d713 = 1;
      {
        if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, v_assignable)))) {
          {
            u_raise(u_mk_con(&n_check_46_NotAssignable, 1, v_n), "check.TypeError");
          }
        }
        u_value s714 = f_list_46_lookup_2(v_n, v_env);
        int d715 = 0;
        if ((!d715) && u_is_con(s714, &n_option_46_Some)) {
          u_value v_t = u_field(u_field(s714, 0), 1);
          d715 = 1;
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_t, v_u, r_s);
            u_value t716 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_t);
            *r_y = t716;
          }
        }
        if ((!d715) && u_is_con(s714, &n_option_46_None)) {
          d715 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
          }
        }
        if (!d715) {
          u_no_case(s714);
        }
      }
    }
    if (!d713) {
      u_no_case(s712);
    }
  }
}

void p_check_46_variables(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s717 = v_xs;
    int d718 = 0;
    if ((!d718) && u_is_con(s717, &n_list_46_Nil)) {
      d718 = 1;
      {
        u_value t719 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t719;
      }
    }
    if ((!d718) && u_is_con(s717, &n_list_46_Cons)) {
      u_value v_x = u_field(s717, 0);
      u_value v_more = u_field(s717, 1);
      d718 = 1;
      {
        p_check_46_variable(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_variables(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t720 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t720;
      }
    }
    if (!d718) {
      u_no_case(s717);
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
    u_value s721 = v_x;
    int d722 = 0;
    if ((!d722) && u_is_con(s721, &n_syntax_46_SSkip)) {
      d722 = 1;
      {
        u_value t723 = v_x;
        *r_y = t723;
      }
    }
    if ((!d722) && u_is_con(s721, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s721, 0);
      d722 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)));
        p_check_46_variables(v_env, v_assignable, f_list_46_firsts_1(v_pairs), &v_ys, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_pairs), f_syntax_46_types_1(v_ys), &v_es, r_s);
        u_value t724 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_zip_2(v_ys, v_es));
        *r_y = t724;
      }
    }
    if ((!d722) && u_is_con(s721, &n_syntax_46_SBlock)) {
      u_value v_locals = u_field(s721, 0);
      u_value v_body = u_field(s721, 1);
      d722 = 1;
      {
        p_check_46_untyped(v_locals, f_check_46_initialized_1(v_body));
        p_check_46_formals(v_locals, &v_ys, r_s);
        p_check_46_statements(f_list_46__43__43__2(f_check_46_bind_1(v_ys), v_env), f_list_46__43__43__2(f_syntax_46_names_1(v_ys), v_assignable), v_body, &v_xs, r_s);
        u_value t725 = u_mk_con(&n_syntax_46_SBlock, 2, v_ys, v_xs);
        *r_y = t725;
      }
    }
    if ((!d722) && u_is_con(s721, &n_syntax_46_SIf)) {
      u_value v_cond = u_field(s721, 0);
      u_value v_left = u_field(s721, 1);
      u_value v_right = u_field(s721, 2);
      d722 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_left, &v_l, r_s);
        p_check_46_statement(v_env, v_assignable, v_right, &v_r, r_s);
        u_value t726 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
        *r_y = t726;
      }
    }
    if ((!d722) && u_is_con(s721, &n_syntax_46_SWhile)) {
      u_value v_cond = u_field(s721, 0);
      u_value v_body = u_field(s721, 1);
      d722 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        u_value t727 = u_mk_con(&n_syntax_46_SWhile, 2, v_c, v_l);
        *r_y = t727;
      }
    }
    if ((!d722) && u_is_con(s721, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s721, 0);
      u_value v_ins = u_field(s721, 1);
      u_value v_outs = u_field(s721, 2);
      u_value v_refs = u_field(s721, 3);
      d722 = 1;
      {
        p_check_46_call_proc(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d722) && u_is_con(s721, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s721, 0);
      u_value v_cases = u_field(s721, 1);
      d722 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_cases, v_t, &v_bs, r_s);
        u_value t728 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e729 = &t728;
        u_value l730 = v_bs;
        while (u_is_con(l730, &n_list_46_Cons)) {
          u_value v_br = u_field(l730, 0);
          if (u_truth(u_mk_bool(1))) {
            e729 = u_append(e729, f_check_46_case_of_1(v_br));
          }
          l730 = u_field(l730, 1);
        }
        p_check_46_coverage(*r_s, v_t, t728);
        u_value t731 = u_mk_con(&n_syntax_46_SMatch, 2, v_c, v_bs);
        *r_y = t731;
      }
    }
    if ((!d722) && u_is_con(s721, &n_syntax_46_SAssert)) {
      u_value v_cond = u_field(s721, 0);
      d722 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        u_value t732 = u_mk_con(&n_syntax_46_SAssert, 1, v_c);
        *r_y = t732;
      }
    }
    if ((!d722) && u_is_con(s721, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s721, 0);
      u_value v_a = u_field(s721, 1);
      d722 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_unify(v_t, v_u, r_s);
        u_value t733 = u_mk_con(&n_syntax_46_SRaise, 2, v_c, v_t);
        *r_y = t733;
      }
    }
    if ((!d722) && u_is_con(s721, &n_syntax_46_STry)) {
      u_value v_body = u_field(s721, 0);
      u_value v_handlers = u_field(s721, 1);
      d722 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_handlers, &v_hs, r_s);
        u_value t734 = u_mk_con(&n_syntax_46_STry, 2, v_l, v_hs);
        *r_y = t734;
      }
    }
    if (!d722) {
      u_no_case(s721);
    }
  }
}

u_value f_check_46_initialized_1(u_value v_xs) {
  u_value s735 = v_xs;
  u_value t736 = NULL;
  if ((!t736) && (u_is_con(s735, &n_list_46_Cons) && (u_is_con(u_field(s735, 0), &n_syntax_46_SAssign) && (u_is_con(u_field(u_field(s735, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s735, 0), 0), 1), &n_list_46_Nil))))) {
    u_value v_x = u_field(u_field(u_field(u_field(s735, 0), 0), 0), 0);
    u_value v_rest = u_field(s735, 1);
    t736 = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), f_check_46_initialized_1(v_rest));
  }
  if (!t736) {
    t736 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t736) {
    u_no_case(s735);
  }
  return t736;
}

void p_check_46_untyped(u_value v_xs, u_value v_inits) {
  {
    u_value s737 = v_xs;
    int d738 = 0;
    if ((!d738) && u_is_con(s737, &n_list_46_Nil)) {
      d738 = 1;
      {
      }
    }
    if ((!d738) && (u_is_con(s737, &n_list_46_Cons) && u_is_con(u_field(s737, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s737, 0), 0);
      u_value v_t = u_field(u_field(s737, 0), 1);
      u_value v_rest = u_field(s737, 1);
      d738 = 1;
      {
        u_value t739 = f_check_46_holes_1(v_t);
        if (u_truth(t739)) {
          t739 = f_prelude_46_not_1(f_list_46_contains_2(v_n, v_inits));
        }
        if (u_truth(t739)) {
          {
            u_raise(u_mk_con(&n_check_46_MissingType, 1, v_n), "check.TypeError");
          }
        }
        p_check_46_untyped(v_rest, v_inits);
      }
    }
    if (!d738) {
      u_no_case(s737);
    }
  }
}

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s740 = v_xs;
    int d741 = 0;
    if ((!d741) && u_is_con(s740, &n_list_46_Nil)) {
      d741 = 1;
      {
        u_value t742 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t742;
      }
    }
    if ((!d741) && u_is_con(s740, &n_list_46_Cons)) {
      u_value v_x = u_field(s740, 0);
      u_value v_more = u_field(s740, 1);
      d741 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_statements(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t743 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t743;
      }
    }
    if (!d741) {
      u_no_case(s740);
    }
  }
}

u_value f_check_46_os_procs_1(u_value v_c) {
  u_value t748;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_744 = U_LIT("os_args");
    static struct u_object l_os_read_745 = U_LIT("os_read");
    static struct u_object l_os_write_746 = U_LIT("os_write");
    static struct u_object l_os_library_747 = U_LIT("os_library");
    t748 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_744), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_745), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_746), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_747), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)))));
  } else {
    t748 = u_mk_con(&n_list_46_Nil, 0);
  }
  return t748;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t749 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e750 = &t749;
  static struct u_object l_Program_751 = U_LIT("Program");
  u_value l752 = f_ctx_46_names_of_2(v_c, &l_Program_751);
  while (u_is_con(l752, &n_list_46_Cons)) {
    u_value v_d = u_field(l752, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e750 = u_append(e750, v_d);
    }
    l752 = u_field(l752, 1);
  }
  u_value s753 = t749;
  u_value t754 = NULL;
  if ((!t754) && u_is_con(s753, &n_list_46_Cons)) {
    u_value v_d = u_field(s753, 0);
    static struct u_object l_read_755 = U_LIT("read");
    static struct u_object l_library_756 = U_LIT("library");
    t754 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_755), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_756), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t754) {
    t754 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t754) {
    u_no_case(s753);
  }
  return t754;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s758 = f_check_46_visible_names_3(*r_s, v_p, u_mk_closure(lam757, 1, *r_s));
    int d759 = 0;
    if ((!d759) && (u_is_con(s758, &n_list_46_Cons) && u_is_con(u_field(s758, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s758, 0);
      d759 = 1;
      {
        u_value t760 = v_q1;
        v_q = t760;
      }
    }
    if ((!d759) && u_is_con(s758, &n_list_46_Nil)) {
      d759 = 1;
      {
        u_value t761 = v_p;
        v_q = t761;
      }
    }
    if (!d759) {
      u_value v_qs = s758;
      d759 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d759) {
      u_no_case(s758);
    }
    u_value s762 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_State_46_ctx_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_State_46_ctx_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_State_46_ctx_1(*r_s)))));
    int d763 = 0;
    if ((!d763) && u_is_con(s762, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s762, 0), 0);
      u_value v_is = u_field(u_field(s762, 0), 1);
      u_value v_os = u_field(u_field(s762, 0), 2);
      u_value v_rs = u_field(u_field(s762, 0), 3);
      d763 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t764 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e765 = &t764;
        u_value l766 = v_is;
        while (u_is_con(l766, &n_list_46_Cons)) {
          u_value v_t = u_field(l766, 0);
          if (u_truth(u_mk_bool(1))) {
            e765 = u_append(e765, f_check_46_subst_2(v_t, v_m));
          }
          l766 = u_field(l766, 1);
        }
        p_check_46_checks(v_env, v_ins, t764, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t767 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e768 = &t767;
        u_value l769 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l769, &n_list_46_Cons)) {
          u_value v_t = u_field(l769, 0);
          if (u_truth(u_mk_bool(1))) {
            e768 = u_append(e768, f_check_46_subst_2(v_t, v_m));
          }
          l769 = u_field(l769, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t767, r_s);
        u_value t770 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t770;
      }
    }
    if ((!d763) && u_is_con(s762, &n_option_46_None)) {
      d763 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d763) {
      u_no_case(s762);
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
    u_value s771 = v_cases;
    int d772 = 0;
    if ((!d772) && u_is_con(s771, &n_list_46_Nil)) {
      d772 = 1;
      {
        u_value t773 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t773;
      }
    }
    if ((!d772) && (u_is_con(s771, &n_list_46_Cons) && u_is_con(u_field(s771, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s771, 0), 0);
      u_value v_g = u_field(u_field(s771, 0), 1);
      u_value v_b = u_field(u_field(s771, 0), 2);
      u_value v_more = u_field(s771, 1);
      d772 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t774 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t774;
      }
    }
    if (!d772) {
      u_no_case(s771);
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
    u_value s775 = v_hs;
    int d776 = 0;
    if ((!d776) && u_is_con(s775, &n_list_46_Nil)) {
      d776 = 1;
      {
        u_value t777 = u_mk_con(&n_list_46_Nil, 0);
        *r_hs1 = t777;
      }
    }
    if ((!d776) && (u_is_con(s775, &n_list_46_Cons) && u_is_con(u_field(s775, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s775, 0), 0);
      u_value v_p = u_field(u_field(s775, 0), 1);
      u_value v_g = u_field(u_field(s775, 0), 2);
      u_value v_b = u_field(u_field(s775, 0), 3);
      u_value v_more = u_field(s775, 1);
      d776 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t778 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t778;
      }
    }
    if (!d776) {
      u_no_case(s775);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t779 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t779)) {
      t779 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t780 = t779;
    if (!u_truth(t780)) {
      t780 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t781 = t780;
    if (!u_truth(t781)) {
      t781 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n))), u_mk_con(&n_list_46_Nil, 0));
    }
    if (u_truth(t781)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t782 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t782)) {
      t782 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t783 = t782;
    if (!u_truth(t783)) {
      t783 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t783)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_784 = U_LIT("Bool");
    static struct u_object l_Int_785 = U_LIT("Int");
    static struct u_object l_Str_786 = U_LIT("Str");
    static struct u_object l_Char_787 = U_LIT("Char");
    static struct u_object l__45__62__788 = U_LIT("->");
    u_value t789 = f_list_46_contains_2(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_784, u_mk_con(&n_list_46_Cons, 2, &l_Int_785, u_mk_con(&n_list_46_Cons, 2, &l_Str_786, u_mk_con(&n_list_46_Cons, 2, &l_Char_787, u_mk_con(&n_list_46_Cons, 2, &l__45__62__788, u_mk_con(&n_list_46_Nil, 0)))))));
    if (!u_truth(t789)) {
      t789 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t790 = t789;
    if (!u_truth(t790)) {
      t790 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t790)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t791 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e792 = &t791;
    u_value l793 = v_ts;
    while (u_is_con(l793, &n_list_46_Cons)) {
      u_value v_t = u_field(l793, 0);
      if (u_truth(u_mk_bool(1))) {
        e792 = u_append(e792, f_check_46_free_1(v_t));
      }
      l793 = u_field(l793, 1);
    }
    u_value t794 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t791)), f_check_46_params_1(v_ps));
    v_undeclared = t794;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, u_mk_con(&n_list_46_Nil, 0)))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
  {
    u_value s795 = v_cs;
    int d796 = 0;
    if ((!d796) && u_is_con(s795, &n_list_46_Nil)) {
      d796 = 1;
      {
      }
    }
    if ((!d796) && (u_is_con(s795, &n_list_46_Cons) && u_is_con(u_field(u_field(s795, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s795, 0), 0);
      u_value v_n = u_field(u_field(u_field(s795, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s795, 0), 1), 1);
      u_value v_rest = u_field(s795, 1);
      d796 = 1;
      {
        u_value t797 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t797;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        u_value t798 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t798;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d796) && u_is_con(s795, &n_list_46_Cons)) {
      u_value v_rest = u_field(s795, 1);
      d796 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d796) {
      u_no_case(s795);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s799 = v_cs;
    int d800 = 0;
    if ((!d800) && u_is_con(s799, &n_list_46_Nil)) {
      d800 = 1;
      {
      }
    }
    if ((!d800) && (u_is_con(s799, &n_list_46_Cons) && u_is_con(u_field(u_field(s799, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s799, 0), 0);
      u_value v_n = u_field(u_field(u_field(s799, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s799, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s799, 0), 1), 2);
      u_value v_rest = u_field(s799, 1);
      d800 = 1;
      {
        u_value t801 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t801;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)));
        u_value t802 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_State_46_ctx_1(*r_s), v_n, v_ps, v_u));
        *r_s = t802;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d800) && u_is_con(s799, &n_list_46_Cons)) {
      u_value v_rest = u_field(s799, 1);
      d800 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d800) {
      u_no_case(s799);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
  {
    u_value s803 = v_cs;
    int d804 = 0;
    if ((!d804) && u_is_con(s803, &n_list_46_Nil)) {
      d804 = 1;
      {
      }
    }
    if ((!d804) && (u_is_con(s803, &n_list_46_Cons) && u_is_con(u_field(u_field(s803, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s803, 0), 0);
      u_value v_n = u_field(u_field(u_field(s803, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s803, 0), 1), 1);
      u_value v_ks = u_field(u_field(u_field(s803, 0), 1), 2);
      u_value v_rest = u_field(s803, 1);
      d804 = 1;
      {
        u_value t805 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t805;
        p_check_46_unique_ids(v_ps);
        u_value t806 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e807 = &t806;
        u_value l808 = v_ks;
        while (u_is_con(l808, &n_list_46_Cons)) {
          u_value v_k = u_field(l808, 0);
          if (u_truth(u_mk_bool(1))) {
            e807 = u_append(e807, f_syntax_46_Constr_46_name_1(v_k));
          }
          l808 = u_field(l808, 1);
        }
        u_value t809 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_State_46_ctx_1(*r_s), v_n, t806));
        *r_s = t809;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d804) && u_is_con(s803, &n_list_46_Cons)) {
      u_value v_rest = u_field(s803, 1);
      d804 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d804) {
      u_no_case(s803);
    }
  }
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s810 = v_ks;
    int d811 = 0;
    if ((!d811) && u_is_con(s810, &n_list_46_Nil)) {
      d811 = 1;
      {
      }
    }
    if ((!d811) && (u_is_con(s810, &n_list_46_Cons) && u_is_con(u_field(s810, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s810, 0), 0);
      u_value v_args = u_field(u_field(s810, 0), 1);
      u_value v_rest = u_field(s810, 1);
      d811 = 1;
      {
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t812 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_State_46_ctx_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t812;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d811) {
      u_no_case(s810);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s813 = v_xss;
  u_value t814 = NULL;
  if ((!t814) && u_is_con(s813, &n_list_46_Nil)) {
    t814 = v_r;
  }
  if ((!t814) && u_is_con(s813, &n_list_46_Cons)) {
    u_value v_xs = u_field(s813, 0);
    u_value v_rest = u_field(s813, 1);
    t814 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t814) {
    u_no_case(s813);
  }
  return t814;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s815 = v_xss;
  u_value t816 = NULL;
  if ((!t816) && u_is_con(s815, &n_list_46_Nil)) {
    t816 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t816) && u_is_con(s815, &n_list_46_Cons)) {
    u_value v_xs = u_field(s815, 0);
    t816 = v_xs;
  }
  if (!t816) {
    u_no_case(s815);
  }
  return t816;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s817 = v_xss;
    int d818 = 0;
    if ((!d818) && u_is_con(s817, &n_list_46_Nil)) {
      d818 = 1;
      {
        u_value t819 = u_mk_con(&n_list_46_Nil, 0);
        *r_yss = t819;
      }
    }
    if ((!d818) && u_is_con(s817, &n_list_46_Cons)) {
      u_value v_xs = u_field(s817, 0);
      u_value v_more = u_field(s817, 1);
      d818 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t820 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t820;
      }
    }
    if (!d818) {
      u_no_case(s817);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s821 = v_cs;
    int d822 = 0;
    if ((!d822) && u_is_con(s821, &n_list_46_Nil)) {
      d822 = 1;
      {
      }
    }
    if ((!d822) && (u_is_con(s821, &n_list_46_Cons) && u_is_con(u_field(u_field(s821, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s821, 0), 0);
      u_value v_n = u_field(u_field(u_field(s821, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s821, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s821, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s821, 0), 1), 3);
      u_value v_rest = u_field(s821, 1);
      d822 = 1;
      {
        u_value t823 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t823;
        p_check_46_overload(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), u_mk_con(&n_list_46_Nil, 0)));
        u_value t824 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t824;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d822) && u_is_con(s821, &n_list_46_Cons)) {
      u_value v_rest = u_field(s821, 1);
      d822 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d822) {
      u_no_case(s821);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s825 = v_cs;
    int d826 = 0;
    if ((!d826) && u_is_con(s825, &n_list_46_Nil)) {
      d826 = 1;
      {
      }
    }
    if ((!d826) && (u_is_con(s825, &n_list_46_Cons) && u_is_con(u_field(u_field(s825, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s825, 0), 0);
      u_value v_n = u_field(u_field(u_field(s825, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s825, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s825, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s825, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s825, 0), 1), 4);
      u_value v_rest = u_field(s825, 1);
      d826 = 1;
      {
        u_value t827 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t827;
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t828 = f_syntax_46_types_1(v_xs);
        v_ts = t828;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t829 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t829;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d826) && u_is_con(s825, &n_list_46_Cons)) {
      u_value v_rest = u_field(s825, 1);
      d826 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d826) {
      u_no_case(s825);
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
    u_value s830 = v_c;
    int d831 = 0;
    if ((!d831) && u_is_con(s830, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s830, 0);
      u_value v_ps = u_field(s830, 1);
      u_value v_bound = u_field(s830, 2);
      u_value v_res = u_field(s830, 3);
      u_value v_body = u_field(s830, 4);
      d831 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t832 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t832;
      }
    }
    if ((!d831) && u_is_con(s830, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s830, 0);
      u_value v_ps = u_field(s830, 1);
      u_value v_ins = u_field(s830, 2);
      u_value v_outs = u_field(s830, 3);
      u_value v_refs = u_field(s830, 4);
      u_value v_body = u_field(s830, 5);
      d831 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t833 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t833;
      }
    }
    if ((!d831) && u_is_con(s830, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s830, 0);
      d831 = 1;
      {
        p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_b, &v_t, r_s);
        u_value t834 = u_mk_con(&n_syntax_46_CEval, 1, v_b);
        *r_c1 = t834;
      }
    }
    if ((!d831) && u_is_con(s830, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s830, 0);
      u_value v_body = u_field(s830, 1);
      d831 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t835 = u_mk_con(&n_syntax_46_CRun, 2, v_n, v_y);
        *r_c1 = t835;
      }
    }
    if ((!d831) && u_is_con(s830, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s830, 0);
      u_value v_body = u_field(s830, 1);
      d831 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t836 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t836;
      }
    }
    if (!d831) {
      d831 = 1;
      {
        u_value t837 = v_c;
        *r_c1 = t837;
      }
    }
    if (!d831) {
      u_no_case(s830);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s838 = v_cs;
    int d839 = 0;
    if ((!d839) && u_is_con(s838, &n_list_46_Nil)) {
      d839 = 1;
      {
        u_value t840 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t840;
      }
    }
    if ((!d839) && u_is_con(s838, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s838, 0), 0);
      u_value v_c = u_field(u_field(s838, 0), 1);
      u_value v_more = u_field(s838, 1);
      d839 = 1;
      {
        u_value t841 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t841;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t842 = f_check_46_zonk_cmd_2(f_check_46_State_46_sub_1(*r_s), v_c1);
        v_c2 = t842;
        u_value s843 = f_check_46_unknown_in_cmd_1(v_c2);
        int d844 = 0;
        if ((!d844) && u_is_con(s843, &n_option_46_Some)) {
          u_value v_t = u_field(s843, 0);
          d844 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d844) && u_is_con(s843, &n_option_46_None)) {
          d844 = 1;
          {
          }
        }
        if (!d844) {
          u_no_case(s843);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t845 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t845;
      }
    }
    if (!d839) {
      u_no_case(s838);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s846 = v_cs;
  u_value t847 = NULL;
  if ((!t847) && u_is_con(s846, &n_list_46_Nil)) {
    t847 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t847) && (u_is_con(s846, &n_list_46_Cons) && u_is_con(u_field(u_field(s846, 0), 1), &n_syntax_46_CRun))) {
    u_value v_n = u_field(u_field(u_field(s846, 0), 1), 0);
    u_value v_more = u_field(s846, 1);
    t847 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t847) && (u_is_con(s846, &n_list_46_Cons) && u_is_con(u_field(u_field(s846, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s846, 0), 1), 0);
    u_value v_more = u_field(s846, 1);
    t847 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t847) && u_is_con(s846, &n_list_46_Cons)) {
    u_value v_more = u_field(s846, 1);
    t847 = f_check_46_block_names_1(v_more);
  }
  if (!t847) {
    u_no_case(s846);
  }
  return t847;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_s = NULL;
    u_value v_ds = NULL;
    u_value v_cs = NULL;
    u_value s848 = v_p;
    int d849 = 0;
    if ((!d849) && u_is_con(s848, &n_syntax_46_Program)) {
      u_value v_cmds = u_field(s848, 0);
      d849 = 1;
      {
        u_value t850 = f_check_46_flatten_2(v_main, v_cmds);
        v_ds = t850;
        u_value t851 = f_check_46_start_2(v_main, f_check_46_visibility_2(v_ds, v_main));
        v_s = t851;
        p_check_46_declare_datas(v_ds, &v_s);
        p_check_46_declare_sorts(v_ds, &v_s);
        p_check_46_declare_constrs(v_ds, &v_s);
        p_check_46_declare_funs(v_ds, &v_s);
        p_check_46_declare_procs(v_ds, &v_s);
        p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
        p_check_46_commands(v_ds, &v_cs, &v_s);
        u_value t852 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
        *r_q = t852;
      }
    }
    if (!d849) {
      u_no_case(s848);
    }
  }
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_853 = U_LIT("main");
    u_value t854 = f_check_46_start_2(&l_main_853, u_mk_con(&n_list_46_Nil, 0));
    v_s = t854;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t855 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(v_s), v_u);
    *r_t = t855;
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
  static struct u_object l_List_856 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_856), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_857 = U_LIT("list");
  static struct u_object l_List_858 = U_LIT("List");
  static struct u_object l_A_859 = U_LIT("A");
  static struct u_object l_Nil_860 = U_LIT("Nil");
  static struct u_object l_Cons_861 = U_LIT("Cons");
  static struct u_object l_A_862 = U_LIT("A");
  static struct u_object l_A_863 = U_LIT("A");
  static struct u_object l_list_864 = U_LIT("list");
  static struct u_object l_len_865 = U_LIT("len");
  static struct u_object l_A_866 = U_LIT("A");
  static struct u_object l_xs_867 = U_LIT("xs");
  static struct u_object l_A_868 = U_LIT("A");
  static struct u_object l_xs_869 = U_LIT("xs");
  static struct u_object l_Nil_870 = U_LIT("Nil");
  static struct u_object l_Cons_871 = U_LIT("Cons");
  static struct u_object l_ys_872 = U_LIT("ys");
  static struct u_object l__43__873 = U_LIT("+");
  static struct u_object l_len_874 = U_LIT("len");
  static struct u_object l_ys_875 = U_LIT("ys");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_857, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_858), u_mk_con(&n_list_46_Cons, 2, &l_A_859, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_860), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_861), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_862), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_863)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_864), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_865), u_mk_con(&n_list_46_Cons, 2, &l_A_866, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_867), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_868))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_869), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_870), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_871), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_872), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__873, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_874, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_875), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s876 = v_t;
  u_value t877 = NULL;
  if ((!t877) && u_is_con(s876, &n_c_46_CtVoid)) {
    static struct u_object l_void_878 = U_LIT("void");
    t877 = &l_void_878;
  }
  if ((!t877) && u_is_con(s876, &n_c_46_CtInt)) {
    static struct u_object l_int_879 = U_LIT("int");
    t877 = &l_int_879;
  }
  if ((!t877) && u_is_con(s876, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s876, 0);
    t877 = v_n;
  }
  if ((!t877) && (u_is_con(s876, &n_c_46_CtPtr) && u_is_con(u_field(s876, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s876, 0), 0);
    static struct u_object l__42__880 = U_LIT("*");
    t877 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__880);
  }
  if ((!t877) && u_is_con(s876, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s876, 0);
    static struct u_object l__32__42__881 = U_LIT(" *");
    t877 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__881);
  }
  if (!t877) {
    u_no_case(s876);
  }
  return t877;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s882 = v_t;
  u_value t883 = NULL;
  if ((!t883) && u_is_con(s882, &n_c_46_CtPtr)) {
    t883 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t883) {
    static struct u_object l__32__884 = U_LIT(" ");
    t883 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__884, v_x));
  }
  if (!t883) {
    u_no_case(s882);
  }
  return t883;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s885 = v_e;
  u_value t886 = NULL;
  if ((!t886) && u_is_con(s885, &n_c_46_CeVar)) {
    u_value v_x = u_field(s885, 0);
    t886 = v_x;
  }
  if ((!t886) && u_is_con(s885, &n_c_46_CeNum)) {
    u_value v_n = u_field(s885, 0);
    t886 = u_str_show(v_n);
  }
  if ((!t886) && u_is_con(s885, &n_c_46_CeStr)) {
    u_value v_s = u_field(s885, 0);
    t886 = u_str_quote(v_s);
  }
  if ((!t886) && u_is_con(s885, &n_c_46_CeCall)) {
    u_value v_f = u_field(s885, 0);
    u_value v_args = u_field(s885, 1);
    static struct u_object l__40__887 = U_LIT("(");
    static struct u_object l__44__32__888 = U_LIT(", ");
    u_value t889 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e890 = &t889;
    u_value l891 = v_args;
    while (u_is_con(l891, &n_list_46_Cons)) {
      u_value v_a = u_field(l891, 0);
      if (u_truth(u_mk_bool(1))) {
        e890 = u_append(e890, f_c_46_c_expr_1(v_a));
      }
      l891 = u_field(l891, 1);
    }
    static struct u_object l__41__892 = U_LIT(")");
    t886 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__887, u_concat(u_str_join(&l__44__32__888, t889), &l__41__892)));
  }
  if ((!t886) && u_is_con(s885, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s885, 0);
    u_value v_a = u_field(s885, 1);
    t886 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t886) && u_is_con(s885, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s885, 0);
    u_value v_a = u_field(s885, 1);
    u_value v_b = u_field(s885, 2);
    static struct u_object l__32__893 = U_LIT(" ");
    static struct u_object l__32__894 = U_LIT(" ");
    t886 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__893, u_concat(v_o, u_concat(&l__32__894, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t886) && u_is_con(s885, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s885, 0);
    u_value v_i = u_field(s885, 1);
    static struct u_object l__91__895 = U_LIT("[");
    static struct u_object l__93__896 = U_LIT("]");
    t886 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__895, u_concat(f_c_46_c_expr_1(v_i), &l__93__896)));
  }
  if ((!t886) && u_is_con(s885, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s885, 0);
    u_value v_f = u_field(s885, 1);
    static struct u_object l__45__62__897 = U_LIT("->");
    t886 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__897, v_f));
  }
  if ((!t886) && u_is_con(s885, &n_c_46_CeCast)) {
    u_value v_t = u_field(s885, 0);
    u_value v_a = u_field(s885, 1);
    static struct u_object l__40__898 = U_LIT("(");
    static struct u_object l__41__899 = U_LIT(")");
    t886 = u_concat(&l__40__898, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__899, f_c_46_c_operand_1(v_a))));
  }
  if ((!t886) && u_is_con(s885, &n_c_46_CeCond)) {
    u_value v_c = u_field(s885, 0);
    u_value v_a = u_field(s885, 1);
    u_value v_b = u_field(s885, 2);
    static struct u_object l__32__63__32__900 = U_LIT(" ? ");
    static struct u_object l__32__58__32__901 = U_LIT(" : ");
    t886 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__900, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__901, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t886) && u_is_con(s885, &n_c_46_CeInit)) {
    u_value v_es = u_field(s885, 0);
    static struct u_object l__123__902 = U_LIT("{");
    static struct u_object l__44__32__903 = U_LIT(", ");
    u_value t904 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e905 = &t904;
    u_value l906 = v_es;
    while (u_is_con(l906, &n_list_46_Cons)) {
      u_value v_a = u_field(l906, 0);
      if (u_truth(u_mk_bool(1))) {
        e905 = u_append(e905, f_c_46_c_expr_1(v_a));
      }
      l906 = u_field(l906, 1);
    }
    static struct u_object l__125__907 = U_LIT("}");
    t886 = u_concat(&l__123__902, u_concat(u_str_join(&l__44__32__903, t904), &l__125__907));
  }
  if (!t886) {
    u_no_case(s885);
  }
  return t886;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s908 = v_e;
  u_value t909 = NULL;
  if ((!t909) && u_is_con(s908, &n_c_46_CeVar)) {
    t909 = f_c_46_c_expr_1(v_e);
  }
  if ((!t909) && u_is_con(s908, &n_c_46_CeNum)) {
    u_value v_n = u_field(s908, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t909 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t909) && u_is_con(s908, &n_c_46_CeStr)) {
    t909 = f_c_46_c_expr_1(v_e);
  }
  if ((!t909) && u_is_con(s908, &n_c_46_CeCall)) {
    t909 = f_c_46_c_expr_1(v_e);
  }
  if ((!t909) && u_is_con(s908, &n_c_46_CeIndex)) {
    t909 = f_c_46_c_expr_1(v_e);
  }
  if ((!t909) && u_is_con(s908, &n_c_46_CeArrow)) {
    t909 = f_c_46_c_expr_1(v_e);
  }
  if (!t909) {
    static struct u_object l__40__910 = U_LIT("(");
    static struct u_object l__41__911 = U_LIT(")");
    t909 = u_concat(&l__40__910, u_concat(f_c_46_c_expr_1(v_e), &l__41__911));
  }
  if (!t909) {
    u_no_case(s908);
  }
  return t909;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s912 = v_s;
  u_value t913 = NULL;
  if ((!t913) && u_is_con(s912, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s912, 0);
    static struct u_object l__59__914 = U_LIT(";");
    t913 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__914)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t913) && (u_is_con(s912, &n_c_46_CsDecl) && u_is_con(u_field(s912, 2), &n_option_46_None))) {
    u_value v_t = u_field(s912, 0);
    u_value v_x = u_field(s912, 1);
    static struct u_object l__59__915 = U_LIT(";");
    t913 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__915)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t913) && (u_is_con(s912, &n_c_46_CsDecl) && u_is_con(u_field(s912, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s912, 0);
    u_value v_x = u_field(s912, 1);
    u_value v_e = u_field(u_field(s912, 2), 0);
    static struct u_object l__32__61__32__916 = U_LIT(" = ");
    static struct u_object l__59__917 = U_LIT(";");
    t913 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__916, u_concat(f_c_46_c_expr_1(v_e), &l__59__917)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t913) && u_is_con(s912, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s912, 0);
    u_value v_e = u_field(s912, 1);
    static struct u_object l__32__61__32__918 = U_LIT(" = ");
    static struct u_object l__59__919 = U_LIT(";");
    t913 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__918, u_concat(f_c_46_c_expr_1(v_e), &l__59__919)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t913) && (u_is_con(s912, &n_c_46_CsIf) && u_is_con(u_field(s912, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s912, 0);
    u_value v_l = u_field(s912, 1);
    static struct u_object l_if_32__40__920 = U_LIT("if (");
    static struct u_object l__41__32__123__921 = U_LIT(") {");
    static struct u_object l__32__32__922 = U_LIT("  ");
    static struct u_object l__125__923 = U_LIT("}");
    t913 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__920, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__921))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__922), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__923), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t913) && u_is_con(s912, &n_c_46_CsIf)) {
    u_value v_c = u_field(s912, 0);
    u_value v_l = u_field(s912, 1);
    u_value v_r = u_field(s912, 2);
    static struct u_object l_if_32__40__924 = U_LIT("if (");
    static struct u_object l__41__32__123__925 = U_LIT(") {");
    static struct u_object l__32__32__926 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__927 = U_LIT("} else {");
    static struct u_object l__32__32__928 = U_LIT("  ");
    static struct u_object l__125__929 = U_LIT("}");
    t913 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__924, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__925))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__926), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__927), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__928), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__929), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t913) && u_is_con(s912, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s912, 0);
    u_value v_b = u_field(s912, 1);
    static struct u_object l_while_32__40__930 = U_LIT("while (");
    static struct u_object l__41__32__123__931 = U_LIT(") {");
    static struct u_object l__32__32__932 = U_LIT("  ");
    static struct u_object l__125__933 = U_LIT("}");
    t913 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__930, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__931))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__932), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__933), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t913) && (u_is_con(s912, &n_c_46_CsReturn) && u_is_con(u_field(s912, 0), &n_option_46_None))) {
    static struct u_object l_return_59__934 = U_LIT("return;");
    t913 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__934), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t913) && (u_is_con(s912, &n_c_46_CsReturn) && u_is_con(u_field(s912, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s912, 0), 0);
    static struct u_object l_return_32__935 = U_LIT("return ");
    static struct u_object l__59__936 = U_LIT(";");
    t913 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__935, u_concat(f_c_46_c_expr_1(v_e), &l__59__936))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t913) && u_is_con(s912, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s912, 0);
    static struct u_object l__123__937 = U_LIT("{");
    static struct u_object l__32__32__938 = U_LIT("  ");
    static struct u_object l__125__939 = U_LIT("}");
    t913 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__937), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__938), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__939), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t913) {
    u_no_case(s912);
  }
  return t913;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t940 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e941 = &t940;
  u_value l942 = v_ss;
  while (u_is_con(l942, &n_list_46_Cons)) {
    u_value v_s = u_field(l942, 0);
    if (u_truth(u_mk_bool(1))) {
      e941 = u_append(e941, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l942 = u_field(l942, 1);
  }
  return f_list_46_concat_1(t940);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s943 = v_ps;
  u_value t944 = NULL;
  if ((!t944) && u_is_con(s943, &n_list_46_Nil)) {
    static struct u_object l_void_945 = U_LIT("void");
    t944 = &l_void_945;
  }
  if (!t944) {
    static struct u_object l__44__32__946 = U_LIT(", ");
    u_value t947 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e948 = &t947;
    u_value l949 = v_ps;
    while (u_is_con(l949, &n_list_46_Cons)) {
      u_value v_p = u_field(l949, 0);
      if (u_truth(u_mk_bool(1))) {
        e948 = u_append(e948, f_c_46_c_param_1(v_p));
      }
      l949 = u_field(l949, 1);
    }
    t944 = u_str_join(&l__44__32__946, t947);
  }
  if (!t944) {
    u_no_case(s943);
  }
  return t944;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s950 = v_p;
  u_value t951 = NULL;
  if (!t951) {
    u_value v_t = u_field(s950, 0);
    u_value v_x = u_field(s950, 1);
    t951 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t951) {
    u_no_case(s950);
  }
  return t951;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s952 = v_d;
  u_value t953 = NULL;
  if ((!t953) && u_is_con(s952, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s952, 0);
    static struct u_object l__35_include_32__954 = U_LIT("#include ");
    t953 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__954, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t953) && u_is_con(s952, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s952, 0);
    u_value v_f = u_field(s952, 1);
    u_value v_ps = u_field(s952, 2);
    static struct u_object l__40__955 = U_LIT("(");
    static struct u_object l__41__59__956 = U_LIT(");");
    t953 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__955, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__956))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t953) && u_is_con(s952, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s952, 0);
    u_value v_f = u_field(s952, 1);
    u_value v_ps = u_field(s952, 2);
    u_value v_b = u_field(s952, 3);
    static struct u_object l__40__957 = U_LIT("(");
    static struct u_object l__41__32__123__958 = U_LIT(") {");
    static struct u_object l__32__32__959 = U_LIT("  ");
    static struct u_object l__125__960 = U_LIT("}");
    t953 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__957, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__958))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__959, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__960, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t953) && (u_is_con(s952, &n_c_46_CdGlobal) && u_is_con(u_field(s952, 2), &n_option_46_None))) {
    u_value v_t = u_field(s952, 0);
    u_value v_x = u_field(s952, 1);
    static struct u_object l__59__961 = U_LIT(";");
    t953 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__961), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t953) && (u_is_con(s952, &n_c_46_CdGlobal) && u_is_con(u_field(s952, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s952, 0);
    u_value v_x = u_field(s952, 1);
    u_value v_e = u_field(u_field(s952, 2), 0);
    static struct u_object l__32__61__32__962 = U_LIT(" = ");
    static struct u_object l__59__963 = U_LIT(";");
    t953 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__962, u_concat(f_c_46_c_expr_1(v_e), &l__59__963))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t953) {
    u_no_case(s952);
  }
  return t953;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s964 = v_f;
  u_value t965 = NULL;
  if ((!t965) && u_is_con(s964, &n_c_46_CFile)) {
    u_value v_ds = u_field(s964, 0);
    static struct u_object l__10__10__966 = U_LIT("\n\n");
    u_value t967 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e968 = &t967;
    u_value l969 = v_ds;
    while (u_is_con(l969, &n_list_46_Cons)) {
      u_value v_d = u_field(l969, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__970 = U_LIT("\n");
        e968 = u_append(e968, u_str_join(&l__10__970, f_c_46_c_decl_1(v_d)));
      }
      l969 = u_field(l969, 1);
    }
    static struct u_object l__10__971 = U_LIT("\n");
    t965 = u_concat(u_str_join(&l__10__10__966, t967), &l__10__971);
  }
  if (!t965) {
    u_no_case(s964);
  }
  return t965;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__972 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__972, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__123__125__44__59__46__973 = U_LIT("()[]{},;.");
  return u_str_contains(&l__40__41__91__93__123__125__44__59__46__973, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t974 = u_is_letter(v_c);
  if (!u_truth(t974)) {
    t974 = u_is_digit(v_c);
  }
  u_value t975 = t974;
  if (!u_truth(t975)) {
    t975 = u_eq(v_c, u_mk_char(95));
  }
  return t975;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t976;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t976 = u_str_get(v_s, v_i);
  } else {
    t976 = u_chr(u_mk_int(0));
  }
  return t976;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s977 = v_c;
  u_value t978 = NULL;
  if ((!t978) && u_equals(s977, u_mk_char(34))) {
    t978 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t978) && u_equals(s977, u_mk_char(39))) {
    t978 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t978) && u_equals(s977, u_mk_char(92))) {
    t978 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t978) && u_equals(s977, u_mk_char(110))) {
    t978 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t978) && u_equals(s977, u_mk_char(116))) {
    t978 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t978) && u_equals(s977, u_mk_char(114))) {
    t978 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t978) {
    t978 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t978) {
    u_no_case(s977);
  }
  return t978;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t979;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t979 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t979 = v_i;
  }
  return t979;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t980 = u_mk_int(0);
    u_value t981 = u_mk_int(1);
    u_value t982 = u_mk_int(1);
    v_j = t980;
    *r_line = t981;
    *r_column = t982;
    u_value t987 = u_lt(v_j, v_i);
    while (u_truth(t987)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t983 = u_add(*r_line, u_mk_int(1));
            u_value t984 = u_mk_int(1);
            *r_line = t983;
            *r_column = t984;
          }
        } else {
          {
            u_value t985 = u_add(*r_column, u_mk_int(1));
            *r_column = t985;
          }
        }
        u_value t986 = u_add(v_j, u_mk_int(1));
        v_j = t986;
      }
      t987 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t988 = *r_i;
    u_value t989 = u_add(*r_i, u_mk_int(1));
    u_value t990 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t988;
    *r_i = t989;
    v_parts = t990;
    u_value t991 = *r_i;
    v_from = t991;
    u_value t1002 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t1002)) {
      {
        u_value t992 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t992;
        u_value t993 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t993)) {
          t993 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t993)) {
          {
            static struct u_object l_unterminated_32_lit_994 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_994, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s995 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d996 = 0;
            if ((!d996) && u_is_con(s995, &n_option_46_Some)) {
              u_value v_d = u_field(s995, 0);
              d996 = 1;
              {
                u_value t997 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t998 = u_add(*r_i, u_mk_int(2));
                v_parts = t997;
                *r_i = t998;
                u_value t999 = *r_i;
                v_from = t999;
              }
            }
            if ((!d996) && u_is_con(s995, &n_option_46_None)) {
              d996 = 1;
              {
                static struct u_object l_invalid_32_escape_1000 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_1000, *r_i), "lex.LexError");
              }
            }
            if (!d996) {
              u_no_case(s995);
            }
          }
        } else {
          {
            u_value t1001 = u_add(*r_i, u_mk_int(1));
            *r_i = t1001;
          }
        }
      }
      t1002 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__1003 = U_LIT("");
    u_value t1004 = u_str_join(&l__1003, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t1005 = u_add(*r_i, u_mk_int(1));
    *r_text = t1004;
    *r_i = t1005;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1006 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t1006;
    u_value t1007 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1008 = v_j;
    *r_t = t1007;
    *r_i = t1008;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1009 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t1009;
    u_value s1010 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d1011 = 0;
    if ((!d1011) && u_is_con(s1010, &n_option_46_Some)) {
      u_value v_n = u_field(s1010, 0);
      d1011 = 1;
      {
        u_value t1012 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t1013 = v_j;
        *r_t = t1012;
        *r_i = t1013;
      }
    }
    if ((!d1011) && u_is_con(s1010, &n_option_46_None)) {
      d1011 = 1;
      {
        static struct u_object l_invalid_32_number_1014 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_1014, *r_i), "lex.LexError");
      }
    }
    if (!d1011) {
      u_no_case(s1010);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1015 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t1015;
    u_value t1016 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1017 = v_j;
    *r_t = t1016;
    *r_i = t1017;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t1018 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t1018;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t1019 = *r_i;
    v_start = t1019;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_1020 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_1020, v_start), "lex.LexError");
      }
    }
    u_value t1021 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t1021;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t1022 = u_str_get(v_s, *r_i);
    v_c = t1022;
    u_value s1023 = v_c;
    int d1024 = 0;
    if (!d1024) {
      u_value t1025 = u_is_letter(v_c);
      if (!u_truth(t1025)) {
        t1025 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t1025)) {
        d1024 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d1024) {
      if (u_truth(u_is_digit(v_c))) {
        d1024 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d1024) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d1024 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d1024) && u_equals(s1023, u_mk_char(34))) {
      d1024 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d1024) && u_equals(s1023, u_mk_char(39))) {
      d1024 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d1024) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d1024 = 1;
        {
          u_value t1026 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t1027 = u_add(*r_i, u_mk_int(1));
          *r_t = t1026;
          *r_i = t1027;
        }
      }
    }
    if (!d1024) {
      d1024 = 1;
      {
        static struct u_object l_unexpected_32_chara_1028 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_1028, *r_i), "lex.LexError");
      }
    }
    if (!d1024) {
      u_no_case(s1023);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t1029 = u_mk_int(0);
    v_i = t1029;
    u_value t1030 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1030;
    u_value t1037 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t1037)) {
      {
        u_value t1031 = v_i;
        v_start = t1031;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t1032 = u_add(v_i, u_mk_int(1));
            v_i = t1032;
          }
        } else {
          u_value t1033 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t1033)) {
            t1033 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t1033)) {
            {
              u_value t1035 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam1034, 0));
              v_i = t1035;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t1036 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t1036;
            }
          }
        }
      }
      t1037 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t1038 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t1038;
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
  u_value s1039 = v_s;
  u_value t1040 = NULL;
  if ((!t1040) && u_is_con(s1039, &n_set_46_SLeaf)) {
    t1040 = u_mk_bool(0);
  }
  if ((!t1040) && u_is_con(s1039, &n_set_46_SNode)) {
    u_value v_l = u_field(s1039, 0);
    u_value v_y = u_field(s1039, 1);
    u_value v_r = u_field(s1039, 2);
    u_value s1041 = u_compare(v_x, v_y);
    u_value t1042 = NULL;
    if ((!t1042) && u_equals(s1041, u_mk_int(-1))) {
      t1042 = f_set_46_member_2(v_x, v_l);
    }
    if ((!t1042) && u_equals(s1041, u_mk_int(1))) {
      t1042 = f_set_46_member_2(v_x, v_r);
    }
    if (!t1042) {
      t1042 = u_mk_bool(1);
    }
    if (!t1042) {
      u_no_case(s1041);
    }
    t1040 = t1042;
  }
  if (!t1040) {
    u_no_case(s1039);
  }
  return t1040;
}

u_value f_set_46_insert_2(u_value v_x, u_value v_s) {
  u_value s1043 = v_s;
  u_value t1044 = NULL;
  if ((!t1044) && u_is_con(s1043, &n_set_46_SLeaf)) {
    t1044 = u_mk_con(&n_set_46_SNode, 4, u_mk_con(&n_set_46_SLeaf, 0), v_x, u_mk_con(&n_set_46_SLeaf, 0), u_mk_int(1));
  }
  if ((!t1044) && u_is_con(s1043, &n_set_46_SNode)) {
    u_value v_l = u_field(s1043, 0);
    u_value v_y = u_field(s1043, 1);
    u_value v_r = u_field(s1043, 2);
    u_value s1045 = u_compare(v_x, v_y);
    u_value t1046 = NULL;
    if ((!t1046) && u_equals(s1045, u_mk_int(-1))) {
      t1046 = f_set_46_rebalance_3(f_set_46_insert_2(v_x, v_l), v_y, v_r);
    }
    if ((!t1046) && u_equals(s1045, u_mk_int(1))) {
      t1046 = f_set_46_rebalance_3(v_l, v_y, f_set_46_insert_2(v_x, v_r));
    }
    if (!t1046) {
      t1046 = v_s;
    }
    if (!t1046) {
      u_no_case(s1045);
    }
    t1044 = t1046;
  }
  if (!t1044) {
    u_no_case(s1043);
  }
  return t1044;
}

u_value f_set_46_from_list_1(u_value v_xs) {
  return f_set_46_insert_all_2(v_xs, u_mk_con(&n_set_46_SLeaf, 0));
}

u_value f_set_46_insert_all_2(u_value v_xs, u_value v_s) {
  u_value s1047 = v_xs;
  u_value t1048 = NULL;
  if ((!t1048) && u_is_con(s1047, &n_list_46_Nil)) {
    t1048 = v_s;
  }
  if ((!t1048) && u_is_con(s1047, &n_list_46_Cons)) {
    u_value v_x = u_field(s1047, 0);
    u_value v_rest = u_field(s1047, 1);
    t1048 = f_set_46_insert_all_2(v_rest, f_set_46_insert_2(v_x, v_s));
  }
  if (!t1048) {
    u_no_case(s1047);
  }
  return t1048;
}

u_value f_set_46_elements_1(u_value v_s) {
  return f_set_46_elements_onto_2(v_s, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_set_46_elements_onto_2(u_value v_s, u_value v_rest) {
  u_value s1049 = v_s;
  u_value t1050 = NULL;
  if ((!t1050) && u_is_con(s1049, &n_set_46_SLeaf)) {
    t1050 = v_rest;
  }
  if ((!t1050) && u_is_con(s1049, &n_set_46_SNode)) {
    u_value v_l = u_field(s1049, 0);
    u_value v_x = u_field(s1049, 1);
    u_value v_r = u_field(s1049, 2);
    t1050 = f_set_46_elements_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, v_x, f_set_46_elements_onto_2(v_r, v_rest)));
  }
  if (!t1050) {
    u_no_case(s1049);
  }
  return t1050;
}

u_value f_set_46_count_1(u_value v_s) {
  u_value s1051 = v_s;
  u_value t1052 = NULL;
  if ((!t1052) && u_is_con(s1051, &n_set_46_SLeaf)) {
    t1052 = u_mk_int(0);
  }
  if ((!t1052) && u_is_con(s1051, &n_set_46_SNode)) {
    u_value v_l = u_field(s1051, 0);
    u_value v_r = u_field(s1051, 2);
    t1052 = u_add(u_add(f_set_46_count_1(v_l), u_mk_int(1)), f_set_46_count_1(v_r));
  }
  if (!t1052) {
    u_no_case(s1051);
  }
  return t1052;
}

u_value f_set_46_depth_1(u_value v_s) {
  u_value s1053 = v_s;
  u_value t1054 = NULL;
  if ((!t1054) && u_is_con(s1053, &n_set_46_SLeaf)) {
    t1054 = u_mk_int(0);
  }
  if ((!t1054) && u_is_con(s1053, &n_set_46_SNode)) {
    u_value v_d = u_field(s1053, 3);
    t1054 = v_d;
  }
  if (!t1054) {
    u_no_case(s1053);
  }
  return t1054;
}

u_value f_set_46_snode_depth_2(u_value v_l, u_value v_r) {
  u_value t1055;
  if (u_truth(u_lt(f_set_46_depth_1(v_l), f_set_46_depth_1(v_r)))) {
    t1055 = f_set_46_depth_1(v_r);
  } else {
    t1055 = f_set_46_depth_1(v_l);
  }
  return u_add(u_mk_int(1), t1055);
}

u_value f_set_46_snode_3(u_value v_l, u_value v_x, u_value v_r) {
  return u_mk_con(&n_set_46_SNode, 4, v_l, v_x, v_r, f_set_46_snode_depth_2(v_l, v_r));
}

u_value f_set_46_turn_left_1(u_value v_s) {
  u_value s1056 = v_s;
  u_value t1057 = NULL;
  if ((!t1057) && (u_is_con(s1056, &n_set_46_SNode) && u_is_con(u_field(s1056, 2), &n_set_46_SNode))) {
    u_value v_a = u_field(s1056, 0);
    u_value v_x = u_field(s1056, 1);
    u_value v_b = u_field(u_field(s1056, 2), 0);
    u_value v_y = u_field(u_field(s1056, 2), 1);
    u_value v_c = u_field(u_field(s1056, 2), 2);
    t1057 = f_set_46_snode_3(f_set_46_snode_3(v_a, v_x, v_b), v_y, v_c);
  }
  if (!t1057) {
    t1057 = v_s;
  }
  if (!t1057) {
    u_no_case(s1056);
  }
  return t1057;
}

u_value f_set_46_turn_right_1(u_value v_s) {
  u_value s1058 = v_s;
  u_value t1059 = NULL;
  if ((!t1059) && (u_is_con(s1058, &n_set_46_SNode) && u_is_con(u_field(s1058, 0), &n_set_46_SNode))) {
    u_value v_a = u_field(u_field(s1058, 0), 0);
    u_value v_x = u_field(u_field(s1058, 0), 1);
    u_value v_b = u_field(u_field(s1058, 0), 2);
    u_value v_y = u_field(s1058, 1);
    u_value v_c = u_field(s1058, 2);
    t1059 = f_set_46_snode_3(v_a, v_x, f_set_46_snode_3(v_b, v_y, v_c));
  }
  if (!t1059) {
    t1059 = v_s;
  }
  if (!t1059) {
    u_no_case(s1058);
  }
  return t1059;
}

u_value f_set_46_smaller_1(u_value v_s) {
  u_value s1060 = v_s;
  u_value t1061 = NULL;
  if ((!t1061) && u_is_con(s1060, &n_set_46_SNode)) {
    u_value v_l = u_field(s1060, 0);
    t1061 = v_l;
  }
  if (!t1061) {
    t1061 = v_s;
  }
  if (!t1061) {
    u_no_case(s1060);
  }
  return t1061;
}

u_value f_set_46_greater_1(u_value v_s) {
  u_value s1062 = v_s;
  u_value t1063 = NULL;
  if ((!t1063) && u_is_con(s1062, &n_set_46_SNode)) {
    u_value v_r = u_field(s1062, 2);
    t1063 = v_r;
  }
  if (!t1063) {
    t1063 = v_s;
  }
  if (!t1063) {
    u_no_case(s1062);
  }
  return t1063;
}

u_value f_set_46_rebalance_3(u_value v_l, u_value v_x, u_value v_r) {
  u_value t1067;
  if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1))))) {
    u_value t1064;
    if (u_truth(u_lt(f_set_46_depth_1(f_set_46_smaller_1(v_l)), f_set_46_depth_1(f_set_46_greater_1(v_l))))) {
      t1064 = f_set_46_turn_left_1(v_l);
    } else {
      t1064 = v_l;
    }
    t1067 = f_set_46_turn_right_1(f_set_46_snode_3(t1064, v_x, v_r));
  } else {
    u_value t1066;
    if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1))))) {
      u_value t1065;
      if (u_truth(u_lt(f_set_46_depth_1(f_set_46_greater_1(v_r)), f_set_46_depth_1(f_set_46_smaller_1(v_r))))) {
        t1065 = f_set_46_turn_right_1(v_r);
      } else {
        t1065 = v_r;
      }
      t1066 = f_set_46_turn_left_1(f_set_46_snode_3(v_l, v_x, t1065));
    } else {
      t1066 = f_set_46_snode_3(v_l, v_x, v_r);
    }
    t1067 = t1066;
  }
  return t1067;
}

u_value f_set_46_well_formed_1(u_value v_s) {
  u_value s1068 = v_s;
  u_value t1069 = NULL;
  if ((!t1069) && u_is_con(s1068, &n_set_46_SLeaf)) {
    t1069 = u_mk_bool(1);
  }
  if ((!t1069) && u_is_con(s1068, &n_set_46_SNode)) {
    u_value v_l = u_field(s1068, 0);
    u_value v_r = u_field(s1068, 2);
    u_value v_d = u_field(s1068, 3);
    u_value t1070 = f_set_46_well_formed_1(v_l);
    if (u_truth(t1070)) {
      t1070 = f_set_46_well_formed_1(v_r);
    }
    u_value t1071 = t1070;
    if (u_truth(t1071)) {
      t1071 = u_eq(v_d, f_set_46_snode_depth_2(v_l, v_r));
    }
    u_value t1072 = t1071;
    if (u_truth(t1072)) {
      t1072 = f_prelude_46__60__61__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1)));
    }
    u_value t1073 = t1072;
    if (u_truth(t1073)) {
      t1073 = f_prelude_46__60__61__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1)));
    }
    t1069 = t1073;
  }
  if (!t1069) {
    u_no_case(s1068);
  }
  return t1069;
}

u_value f_set_46_upto_1(u_value v_n) {
  u_value t1074;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t1074 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1074 = f_list_46__43__43__2(f_set_46_upto_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t1074;
}

u_value f_parse_46_Notations_46_cmds_1(u_value v_x) {
  u_value s1075 = v_x;
  u_value t1076 = NULL;
  if ((!t1076) && u_is_con(s1075, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1075, 0);
    t1076 = v_y;
  }
  if (!t1076) {
    u_no_case(s1075);
  }
  return t1076;
}

u_value f_parse_46_Notations_46_ops_1(u_value v_x) {
  u_value s1077 = v_x;
  u_value t1078 = NULL;
  if ((!t1078) && u_is_con(s1077, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1077, 1);
    t1078 = v_y;
  }
  if (!t1078) {
    u_no_case(s1077);
  }
  return t1078;
}

u_value f_parse_46_Notations_46_cmds_58__61__2(u_value v_x, u_value v_y) {
  u_value s1079 = v_x;
  u_value t1080 = NULL;
  if ((!t1080) && u_is_con(s1079, &n_parse_46_Notations)) {
    u_value v_a1 = u_field(s1079, 1);
    t1080 = u_mk_con(&n_parse_46_Notations, 2, v_y, v_a1);
  }
  if (!t1080) {
    u_no_case(s1079);
  }
  return t1080;
}

u_value f_parse_46_Notations_46_ops_58__61__2(u_value v_x, u_value v_y) {
  u_value s1081 = v_x;
  u_value t1082 = NULL;
  if ((!t1082) && u_is_con(s1081, &n_parse_46_Notations)) {
    u_value v_a0 = u_field(s1081, 0);
    t1082 = u_mk_con(&n_parse_46_Notations, 2, v_a0, v_y);
  }
  if (!t1082) {
    u_no_case(s1081);
  }
  return t1082;
}

u_value f_parse_46_Scope_46_params_1(u_value v_x) {
  u_value s1083 = v_x;
  u_value t1084 = NULL;
  if ((!t1084) && u_is_con(s1083, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1083, 0);
    t1084 = v_y;
  }
  if (!t1084) {
    u_no_case(s1083);
  }
  return t1084;
}

u_value f_parse_46_Scope_46_notations_1(u_value v_x) {
  u_value s1085 = v_x;
  u_value t1086 = NULL;
  if ((!t1086) && u_is_con(s1085, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1085, 1);
    t1086 = v_y;
  }
  if (!t1086) {
    u_no_case(s1085);
  }
  return t1086;
}

u_value f_parse_46_Scope_46_params_58__61__2(u_value v_x, u_value v_y) {
  u_value s1087 = v_x;
  u_value t1088 = NULL;
  if ((!t1088) && u_is_con(s1087, &n_parse_46_Scope)) {
    u_value v_a1 = u_field(s1087, 1);
    t1088 = u_mk_con(&n_parse_46_Scope, 2, v_y, v_a1);
  }
  if (!t1088) {
    u_no_case(s1087);
  }
  return t1088;
}

u_value f_parse_46_Scope_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1089 = v_x;
  u_value t1090 = NULL;
  if ((!t1090) && u_is_con(s1089, &n_parse_46_Scope)) {
    u_value v_a0 = u_field(s1089, 0);
    t1090 = u_mk_con(&n_parse_46_Scope, 2, v_a0, v_y);
  }
  if (!t1090) {
    u_no_case(s1089);
  }
  return t1090;
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
  static struct u_object l___1091 = U_LIT("_");
  static struct u_object l_assert_1092 = U_LIT("assert");
  static struct u_object l_assign_1093 = U_LIT("assign");
  static struct u_object l_call_1094 = U_LIT("call");
  static struct u_object l_case_1095 = U_LIT("case");
  static struct u_object l_catch_1096 = U_LIT("catch");
  static struct u_object l_data_1097 = U_LIT("data");
  static struct u_object l_else_1098 = U_LIT("else");
  static struct u_object l_end_1099 = U_LIT("end");
  static struct u_object l_eval_1100 = U_LIT("eval");
  static struct u_object l_false_1101 = U_LIT("false");
  static struct u_object l_for_1102 = U_LIT("for");
  static struct u_object l_function_1103 = U_LIT("function");
  static struct u_object l_if_1104 = U_LIT("if");
  static struct u_object l_import_1105 = U_LIT("import");
  static struct u_object l_in_1106 = U_LIT("in");
  static struct u_object l_infix_1107 = U_LIT("infix");
  static struct u_object l_lambda_1108 = U_LIT("lambda");
  static struct u_object l_let_1109 = U_LIT("let");
  static struct u_object l_local_1110 = U_LIT("local");
  static struct u_object l_match_1111 = U_LIT("match");
  static struct u_object l_out_1112 = U_LIT("out");
  static struct u_object l_postfix_1113 = U_LIT("postfix");
  static struct u_object l_prefix_1114 = U_LIT("prefix");
  static struct u_object l_procedure_1115 = U_LIT("procedure");
  static struct u_object l_raise_1116 = U_LIT("raise");
  static struct u_object l_ref_1117 = U_LIT("ref");
  static struct u_object l_run_1118 = U_LIT("run");
  static struct u_object l_skip_1119 = U_LIT("skip");
  static struct u_object l_test_1120 = U_LIT("test");
  static struct u_object l_then_1121 = U_LIT("then");
  static struct u_object l_true_1122 = U_LIT("true");
  static struct u_object l_type_1123 = U_LIT("type");
  static struct u_object l_try_1124 = U_LIT("try");
  static struct u_object l_while_1125 = U_LIT("while");
  static struct u_object l_with_1126 = U_LIT("with");
  u_pop(handler);
  cache = f_set_46_from_list_1(u_mk_con(&n_list_46_Cons, 2, &l___1091, u_mk_con(&n_list_46_Cons, 2, &l_assert_1092, u_mk_con(&n_list_46_Cons, 2, &l_assign_1093, u_mk_con(&n_list_46_Cons, 2, &l_call_1094, u_mk_con(&n_list_46_Cons, 2, &l_case_1095, u_mk_con(&n_list_46_Cons, 2, &l_catch_1096, u_mk_con(&n_list_46_Cons, 2, &l_data_1097, u_mk_con(&n_list_46_Cons, 2, &l_else_1098, u_mk_con(&n_list_46_Cons, 2, &l_end_1099, u_mk_con(&n_list_46_Cons, 2, &l_eval_1100, u_mk_con(&n_list_46_Cons, 2, &l_false_1101, u_mk_con(&n_list_46_Cons, 2, &l_for_1102, u_mk_con(&n_list_46_Cons, 2, &l_function_1103, u_mk_con(&n_list_46_Cons, 2, &l_if_1104, u_mk_con(&n_list_46_Cons, 2, &l_import_1105, u_mk_con(&n_list_46_Cons, 2, &l_in_1106, u_mk_con(&n_list_46_Cons, 2, &l_infix_1107, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1108, u_mk_con(&n_list_46_Cons, 2, &l_let_1109, u_mk_con(&n_list_46_Cons, 2, &l_local_1110, u_mk_con(&n_list_46_Cons, 2, &l_match_1111, u_mk_con(&n_list_46_Cons, 2, &l_out_1112, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1113, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1114, u_mk_con(&n_list_46_Cons, 2, &l_procedure_1115, u_mk_con(&n_list_46_Cons, 2, &l_raise_1116, u_mk_con(&n_list_46_Cons, 2, &l_ref_1117, u_mk_con(&n_list_46_Cons, 2, &l_run_1118, u_mk_con(&n_list_46_Cons, 2, &l_skip_1119, u_mk_con(&n_list_46_Cons, 2, &l_test_1120, u_mk_con(&n_list_46_Cons, 2, &l_then_1121, u_mk_con(&n_list_46_Cons, 2, &l_true_1122, u_mk_con(&n_list_46_Cons, 2, &l_type_1123, u_mk_con(&n_list_46_Cons, 2, &l_try_1124, u_mk_con(&n_list_46_Cons, 2, &l_while_1125, u_mk_con(&n_list_46_Cons, 2, &l_with_1126, u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_set_46_member_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1127 = U_LIT("=");
  static struct u_object l__61__62__1128 = U_LIT("=>");
  static struct u_object l__58__61__1129 = U_LIT(":=");
  static struct u_object l__58__1130 = U_LIT(":");
  static struct u_object l__124__1131 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1127, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1128, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1129, u_mk_con(&n_list_46_Cons, 2, &l__58__1130, u_mk_con(&n_list_46_Cons, 2, &l__124__1131, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1132 = v_ns;
  u_value t1133 = NULL;
  if ((!t1133) && u_is_con(s1132, &n_list_46_Nil)) {
    t1133 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1133) && (u_is_con(s1132, &n_list_46_Cons) && (u_is_con(u_field(s1132, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1132, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1132, 0), 1);
    u_value v_ops = u_field(u_field(s1132, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1133 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1133) && u_is_con(s1132, &n_list_46_Cons)) {
    u_value v_more = u_field(s1132, 1);
    t1133 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1133) {
    u_no_case(s1132);
  }
  return t1133;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1134 = v_ns;
  u_value t1135 = NULL;
  if ((!t1135) && u_is_con(s1134, &n_list_46_Nil)) {
    t1135 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1135) && (u_is_con(s1134, &n_list_46_Cons) && (u_is_con(u_field(s1134, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1134, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1134, 0), 1);
    u_value v_ops = u_field(u_field(s1134, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1135 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1135) && u_is_con(s1134, &n_list_46_Cons)) {
    u_value v_more = u_field(s1134, 1);
    t1135 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1135) {
    u_no_case(s1134);
  }
  return t1135;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1136 = v_ns;
  u_value t1137 = NULL;
  if ((!t1137) && u_is_con(s1136, &n_list_46_Nil)) {
    t1137 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1137) && (u_is_con(s1136, &n_list_46_Cons) && u_is_con(u_field(s1136, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1136, 0), 0);
    u_value v_p = u_field(u_field(s1136, 0), 1);
    u_value v_ops = u_field(u_field(s1136, 0), 2);
    u_value t1138 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1138)) {
      t1138 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1139 = t1138;
    if (u_truth(t1139)) {
      t1139 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1139)) {
      t1137 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1137) && u_is_con(s1136, &n_list_46_Cons)) {
    u_value v_more = u_field(s1136, 1);
    t1137 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1137) {
    u_no_case(s1136);
  }
  return t1137;
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
  u_value s1140 = v_cs;
  u_value t1141 = NULL;
  if ((!t1141) && u_is_con(s1140, &n_list_46_Nil)) {
    t1141 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1141) && (u_is_con(s1140, &n_list_46_Cons) && u_is_con(u_field(s1140, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1140, 0), 2);
    u_value v_more = u_field(s1140, 1);
    t1141 = f_list_46__43__43__2(v_ops, f_parse_46_operators_of_1(v_more));
  }
  if ((!t1141) && u_is_con(s1140, &n_list_46_Cons)) {
    u_value v_more = u_field(s1140, 1);
    t1141 = f_parse_46_operators_of_1(v_more);
  }
  if (!t1141) {
    u_no_case(s1140);
  }
  return t1141;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1142 = v_ts;
  u_value t1143 = NULL;
  if ((!t1143) && u_is_con(s1142, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1142, 0), 0);
    t1143 = v_t;
  }
  if (!t1143) {
    t1143 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1143) {
    u_no_case(s1142);
  }
  return t1143;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1144 = v_ts;
  u_value t1145 = NULL;
  if ((!t1145) && (u_is_con(s1144, &n_list_46_Cons) && u_is_con(u_field(s1144, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1144, 1), 0), 0);
    t1145 = v_t;
  }
  if (!t1145) {
    t1145 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1145) {
    u_no_case(s1144);
  }
  return t1145;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1146 = v_ts;
  u_value t1147 = NULL;
  if ((!t1147) && u_is_con(s1146, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1146, 0), 1);
    t1147 = v_i;
  }
  if (!t1147) {
    t1147 = u_mk_int(0);
  }
  if (!t1147) {
    u_no_case(s1146);
  }
  return t1147;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1148 = v_t;
  u_value t1149 = NULL;
  if ((!t1149) && u_is_con(s1148, &n_lex_46_TName)) {
    u_value v_n = u_field(s1148, 0);
    t1149 = v_n;
  }
  if ((!t1149) && u_is_con(s1148, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1148, 0);
    t1149 = v_s;
  }
  if ((!t1149) && u_is_con(s1148, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1148, 0);
    t1149 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1149) && u_is_con(s1148, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1148, 0);
    t1149 = u_str_show(v_n);
  }
  if ((!t1149) && u_is_con(s1148, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1148, 0);
    t1149 = u_str_quote(v_s);
  }
  if ((!t1149) && u_is_con(s1148, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1148, 0);
    static struct u_object l__39__1150 = U_LIT("'");
    static struct u_object l__39__1151 = U_LIT("'");
    t1149 = u_concat(&l__39__1150, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1151));
  }
  if ((!t1149) && u_is_con(s1148, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1152 = U_LIT("end of input");
    t1149 = &l_end_32_of_32_input_1152;
  }
  if (!t1149) {
    u_no_case(s1148);
  }
  return t1149;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1153 = f_parse_46_head_token_1(v_ts);
  u_value t1154 = NULL;
  if ((!t1154) && u_is_con(s1153, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1153, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1154 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1154) && u_is_con(s1153, &n_lex_46_TName)) {
    u_value v_n = u_field(s1153, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1154 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1154) {
    t1154 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1154) {
    u_no_case(s1153);
  }
  return t1154;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1155 = *r_ts;
    int d1156 = 0;
    if ((!d1156) && u_is_con(s1155, &n_list_46_Cons)) {
      u_value v_more = u_field(s1155, 1);
      d1156 = 1;
      {
        u_value t1157 = v_more;
        *r_ts = t1157;
      }
    }
    if (!d1156) {
      d1156 = 1;
      {
      }
    }
    if (!d1156) {
      u_no_case(s1155);
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
        static struct u_object l__32_expected_32_but_32__1158 = U_LIT(" expected but ");
        static struct u_object l__32_found_1159 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1158, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1159))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1160 = f_parse_46_head_token_1(*r_ts);
    int d1161 = 0;
    if ((!d1161) && u_is_con(s1160, &n_lex_46_TName)) {
      u_value v_m = u_field(s1160, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1161 = 1;
        {
          u_value t1162 = v_m;
          *r_n = t1162;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1161) {
      d1161 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1163 = U_LIT("name expected but ");
        static struct u_object l__32_found_1164 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1163, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1164)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1161) {
      u_no_case(s1160);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1165 = f_parse_46_head_token_1(*r_ts);
    int d1166 = 0;
    if ((!d1166) && u_is_con(s1165, &n_lex_46_TName)) {
      u_value v_m = u_field(s1165, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_m))) {
        d1166 = 1;
        {
          static struct u_object l_operator_32__1167 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1168 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1167, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1168)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1166) {
      d1166 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1166) {
      u_no_case(s1165);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1169 = f_parse_46_head_token_1(*r_ts);
    int d1170 = 0;
    if ((!d1170) && u_is_con(s1169, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1169, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1170 = 1;
        {
          u_value t1171 = v_s;
          *r_n = t1171;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1170) {
      d1170 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1170) {
      u_no_case(s1169);
    }
  }
}

u_value f_parse_46_Ops_46_notations_1(u_value v_x) {
  u_value s1172 = v_x;
  u_value t1173 = NULL;
  if ((!t1173) && u_is_con(s1172, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1172, 0);
    t1173 = v_y;
  }
  if (!t1173) {
    u_no_case(s1172);
  }
  return t1173;
}

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x) {
  u_value s1174 = v_x;
  u_value t1175 = NULL;
  if ((!t1175) && u_is_con(s1174, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1174, 1);
    t1175 = v_y;
  }
  if (!t1175) {
    u_no_case(s1174);
  }
  return t1175;
}

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x) {
  u_value s1176 = v_x;
  u_value t1177 = NULL;
  if ((!t1177) && u_is_con(s1176, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1176, 2);
    t1177 = v_y;
  }
  if (!t1177) {
    u_no_case(s1176);
  }
  return t1177;
}

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x) {
  u_value s1178 = v_x;
  u_value t1179 = NULL;
  if ((!t1179) && u_is_con(s1178, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1178, 3);
    t1179 = v_y;
  }
  if (!t1179) {
    u_no_case(s1178);
  }
  return t1179;
}

u_value f_parse_46_Ops_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1180 = v_x;
  u_value t1181 = NULL;
  if ((!t1181) && u_is_con(s1180, &n_parse_46_Ops)) {
    u_value v_a1 = u_field(s1180, 1);
    u_value v_a2 = u_field(s1180, 2);
    u_value v_a3 = u_field(s1180, 3);
    t1181 = u_mk_con(&n_parse_46_Ops, 4, v_y, v_a1, v_a2, v_a3);
  }
  if (!t1181) {
    u_no_case(s1180);
  }
  return t1181;
}

u_value f_parse_46_Ops_46_prefix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1182 = v_x;
  u_value t1183 = NULL;
  if ((!t1183) && u_is_con(s1182, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1182, 0);
    u_value v_a2 = u_field(s1182, 2);
    u_value v_a3 = u_field(s1182, 3);
    t1183 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_y, v_a2, v_a3);
  }
  if (!t1183) {
    u_no_case(s1182);
  }
  return t1183;
}

u_value f_parse_46_Ops_46_postfix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1184 = v_x;
  u_value t1185 = NULL;
  if ((!t1185) && u_is_con(s1184, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1184, 0);
    u_value v_a1 = u_field(s1184, 1);
    u_value v_a3 = u_field(s1184, 3);
    t1185 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_y, v_a3);
  }
  if (!t1185) {
    u_no_case(s1184);
  }
  return t1185;
}

u_value f_parse_46_Ops_46_infix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1186 = v_x;
  u_value t1187 = NULL;
  if ((!t1187) && u_is_con(s1186, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1186, 0);
    u_value v_a1 = u_field(s1186, 1);
    u_value v_a2 = u_field(s1186, 2);
    t1187 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_a2, v_y);
  }
  if (!t1187) {
    u_no_case(s1186);
  }
  return t1187;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1188 = v_rest;
    int d1189 = 0;
    if ((!d1189) && u_is_con(s1188, &n_list_46_Nil)) {
      d1189 = 1;
      {
      }
    }
    if ((!d1189) && (u_is_con(s1188, &n_list_46_Cons) && u_is_con(u_field(s1188, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1188, 0), 0);
      u_value v_i = u_field(u_field(s1188, 0), 1);
      d1189 = 1;
      {
        static struct u_object l_unexpected_32_opera_1190 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1190, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1189) {
      d1189 = 1;
      {
        static struct u_object l_missing_32_operator_1191 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1191, v_at), "parse.ParseError");
      }
    }
    if (!d1189) {
      u_no_case(s1188);
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
    u_value s1192 = v_items;
    int d1193 = 0;
    if ((!d1193) && (u_is_con(s1192, &n_list_46_Cons) && u_is_con(u_field(s1192, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1192, 0), 0);
      u_value v_more = u_field(s1192, 1);
      d1193 = 1;
      {
        u_value t1194 = v_x;
        u_value t1195 = v_more;
        *r_a = t1194;
        *r_rest = t1195;
      }
    }
    if ((!d1193) && (u_is_con(s1192, &n_list_46_Cons) && u_is_con(u_field(s1192, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1192, 0), 0);
      u_value v_i = u_field(u_field(s1192, 0), 1);
      u_value v_more = u_field(s1192, 1);
      d1193 = 1;
      {
        u_value s1196 = f_parse_46_prefix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op);
        int d1197 = 0;
        if ((!d1197) && u_is_con(s1196, &n_option_46_Some)) {
          u_value v_p = u_field(s1196, 0);
          d1197 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1198 = u_apply(f_parse_46_Ops_46_prefix_op_1(v_o), 2, v_op, v_b);
            *r_a = t1198;
          }
        }
        if ((!d1197) && u_is_con(s1196, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op))) {
            d1197 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1199 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1199), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1197) && u_is_con(s1196, &n_option_46_None)) {
          d1197 = 1;
          {
            static struct u_object l_undeclared_32_opera_1200 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1200, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1197) {
          u_no_case(s1196);
        }
      }
    }
    if ((!d1193) && u_is_con(s1192, &n_list_46_Nil)) {
      d1193 = 1;
      {
        static struct u_object l_missing_32_operand_1201 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1201, v_at), "parse.ParseError");
      }
    }
    if (!d1193) {
      u_no_case(s1192);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1202 = v_items;
    int d1203 = 0;
    if ((!d1203) && (u_is_con(s1202, &n_list_46_Cons) && u_is_con(u_field(s1202, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1202, 0), 0);
      u_value v_i = u_field(u_field(s1202, 0), 1);
      u_value v_tail = u_field(s1202, 1);
      d1203 = 1;
      {
        u_value s1204 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op), f_parse_46_infix_of_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op));
        int d1205 = 0;
        if ((!d1205) && u_is_con(u_field(s1204, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1204, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1205 = 1;
            {
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_postfix_op_1(v_o), 2, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1205) && u_is_con(u_field(s1204, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1204, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1204, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1205 = 1;
            {
              u_value t1206;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1206 = v_p;
              } else {
                t1206 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1206, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)));
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_infix_op_1(v_o), 3, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1205) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op)))) {
            d1205 = 1;
            {
              static struct u_object l_undeclared_32_opera_1207 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1207, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1205) {
          d1205 = 1;
          {
            u_value t1208 = v_lhs;
            u_value t1209 = v_items;
            *r_a = t1208;
            *r_rest = t1209;
          }
        }
        if (!d1205) {
          u_no_case(s1204);
        }
      }
    }
    if (!d1203) {
      d1203 = 1;
      {
        u_value t1210 = v_lhs;
        u_value t1211 = v_items;
        *r_a = t1210;
        *r_rest = t1211;
      }
    }
    if (!d1203) {
      u_no_case(s1202);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1212 = v_rest;
    int d1213 = 0;
    if ((!d1213) && (u_is_con(s1212, &n_list_46_Cons) && u_is_con(u_field(s1212, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1212, 0), 0);
      u_value v_i = u_field(u_field(s1212, 0), 1);
      u_value t1216 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1216)) {
        t1216 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1216)) {
        d1213 = 1;
        {
          static struct u_object l_operator_32__1214 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1215 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1214, u_concat(v_op, &l__32_is_32_not_32_associat_1215)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1213) {
      d1213 = 1;
      {
      }
    }
    if (!d1213) {
      u_no_case(s1212);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1217 = v_fp;
  u_value t1218 = NULL;
  if ((!t1218) && u_is_con(s1217, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1217, 0), 1);
    t1218 = u_eq(v_q, v_p);
  }
  if (!t1218) {
    t1218 = u_mk_bool(0);
  }
  if (!t1218) {
    u_no_case(s1217);
  }
  return t1218;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1219 = v_ts;
  u_value t1220 = NULL;
  if ((!t1220) && (u_is_con(s1219, &n_list_46_Cons) && (u_is_con(u_field(s1219, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1219, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1219, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1219, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1219, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1219, 0), 1);
    t1220 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1220) && (u_is_con(s1219, &n_list_46_Cons) && u_is_con(u_field(s1219, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1219, 0);
    t1220 = v_t;
  }
  if ((!t1220) && u_is_con(s1219, &n_list_46_Nil)) {
    t1220 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1220) {
    static struct u_object l__40_invalid_41__1221 = U_LIT("(invalid)");
    t1220 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1221), v_ts);
  }
  if (!t1220) {
    u_no_case(s1219);
  }
  return t1220;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1222 = v_ts;
  u_value t1223 = NULL;
  if ((!t1223) && (u_is_con(s1222, &n_list_46_Cons) && (u_is_con(u_field(s1222, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1222, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1222, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1222, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1222, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1222, 0), 1);
    t1223 = v_cs;
  }
  if (!t1223) {
    t1223 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1223) {
    u_no_case(s1222);
  }
  return t1223;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1224 = U_LIT("->");
  u_value t1231;
  if (u_truth(u_eq(v_op, &l__45__62__1224))) {
    u_value t1225 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1226 = &t1225;
    u_value l1227 = v_l;
    while (u_is_con(l1227, &n_list_46_Cons)) {
      u_value v_t = u_field(l1227, 0);
      if (u_truth(u_mk_bool(1))) {
        e1226 = u_append(e1226, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1227 = u_field(l1227, 1);
    }
    t1231 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1225, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1228 = U_LIT("*");
    u_value t1230;
    if (u_truth(u_eq(v_op, &l__42__1228))) {
      static struct u_object l__40_product_41__1229 = U_LIT("(product)");
      t1230 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1229), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1230 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1231 = t1230;
  }
  return t1231;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1232 = v_t;
  u_value t1233 = NULL;
  if ((!t1233) && (u_is_con(s1232, &n_syntax_46_TSort) && (u_is_con(u_field(s1232, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1232, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1232, 0), 1), u_mk_lit("(invalid)")))))) {
    t1233 = u_mk_bool(1);
  }
  if ((!t1233) && u_is_con(s1232, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1232, 1);
    t1233 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1233) && u_is_con(s1232, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1232, 0);
    u_value v_r = u_field(s1232, 1);
    u_value t1234 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1234)) {
      t1234 = f_parse_46_has_invalid_1(v_r);
    }
    t1233 = t1234;
  }
  if ((!t1233) && u_is_con(s1232, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1232, 0);
    t1233 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1233) {
    t1233 = u_mk_bool(0);
  }
  if (!t1233) {
    u_no_case(s1232);
  }
  return t1233;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1235 = v_ts;
  u_value t1236 = NULL;
  if ((!t1236) && u_is_con(s1235, &n_list_46_Nil)) {
    t1236 = u_mk_bool(0);
  }
  if ((!t1236) && u_is_con(s1235, &n_list_46_Cons)) {
    u_value v_t = u_field(s1235, 0);
    u_value v_more = u_field(s1235, 1);
    u_value t1237 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1237)) {
      t1237 = f_parse_46_any_invalid_1(v_more);
    }
    t1236 = t1237;
  }
  if (!t1236) {
    u_no_case(s1235);
  }
  return t1236;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1238 = v_t;
  u_value t1239 = NULL;
  if ((!t1239) && u_is_con(s1238, &n_lex_46_TName)) {
    u_value v_n = u_field(s1238, 0);
    u_value t1240 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1240)) {
      t1240 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1239 = t1240;
  }
  if ((!t1239) && (u_is_con(s1238, &n_lex_46_TPunct) && u_equals(u_field(s1238, 0), u_mk_char(40)))) {
    t1239 = u_mk_bool(1);
  }
  if (!t1239) {
    t1239 = u_mk_bool(0);
  }
  if (!t1239) {
    u_no_case(s1238);
  }
  return t1239;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1241 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1241;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1242 = f_parse_46_type_one_1(v_r);
    *r_t = t1242;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1243 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1243, v_start), "parse.ParseError");
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
    u_value t1244 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1244;
    u_value t1245 = u_mk_bool(1);
    v_next = t1245;
    u_value t1246 = u_mk_bool(1);
    v_go = t1246;
    u_value t1255 = v_go;
    while (u_truth(t1255)) {
      {
        u_value s1247 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1248 = 0;
        if ((!d1248) && u_is_con(s1247, &n_option_46_Some)) {
          u_value v_op = u_field(s1247, 0);
          d1248 = 1;
          {
            u_value t1249 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1250 = u_mk_bool(1);
            v_rev = t1249;
            v_next = t1250;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1248) && u_is_con(s1247, &n_option_46_None)) {
          u_value t1253 = v_next;
          if (u_truth(t1253)) {
            t1253 = f_parse_46_starts_type_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1253)) {
            d1248 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1251 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1252 = u_mk_bool(0);
              v_rev = t1251;
              v_next = t1252;
            }
          }
        }
        if (!d1248) {
          d1248 = 1;
          {
            u_value t1254 = u_mk_bool(0);
            v_go = t1254;
          }
        }
        if (!d1248) {
          u_no_case(s1247);
        }
      }
      t1255 = v_go;
    }
    u_value s1256 = v_rev;
    int d1257 = 0;
    if ((!d1257) && u_is_con(s1256, &n_list_46_Nil)) {
      d1257 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1258 = U_LIT("type expected but ");
        static struct u_object l__32_found_1259 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1258, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1259)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1257) {
      d1257 = 1;
      {
        u_value t1260 = f_list_46_reverse_1(v_rev);
        *r_items = t1260;
      }
    }
    if (!d1257) {
      u_no_case(s1256);
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
            u_value t1261 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
            *r_r = t1261;
          }
        } else {
          if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Scope_46_params_1(v_sc)))) {
            {
              u_value t1262 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1262;
            }
          } else {
            {
              u_value t1263 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1263;
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
    u_value t1264 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1264;
    u_value t1265 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1265)) {
      t1265 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1265)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1266 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1266;
        u_value t1268 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1268)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1267 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1267;
          }
          t1268 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1269 = f_list_46_reverse_1(v_rev);
    *r_r = t1269;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_prefix_2(u_value v_op, u_value v_a) {
  u_value s1270 = v_a;
  u_value t1271 = NULL;
  if ((!t1271) && (u_is_con(s1270, &n_syntax_46_ELit) && u_is_con(u_field(s1270, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1270, 0), 0);
    static struct u_object l__45__1272 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1272))) {
      t1271 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1271) {
    t1271 = f_parse_46_expr_op1_2(v_op, v_a);
  }
  if (!t1271) {
    u_no_case(s1270);
  }
  return t1271;
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1273 = v_es;
  u_value t1274 = NULL;
  if ((!t1274) && u_is_con(s1273, &n_list_46_Nil)) {
    t1274 = v_rest;
  }
  if ((!t1274) && u_is_con(s1273, &n_list_46_Cons)) {
    u_value v_e = u_field(s1273, 0);
    u_value v_more = u_field(s1273, 1);
    static struct u_object l_Cons_1275 = U_LIT("Cons");
    t1274 = f_parse_46_expr_op2_3(&l_Cons_1275, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1274) {
    u_no_case(s1273);
  }
  return t1274;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1276 = v_t;
  u_value t1277 = NULL;
  if ((!t1277) && u_is_con(s1276, &n_lex_46_TName)) {
    u_value v_n = u_field(s1276, 0);
    static struct u_object l_let_1278 = U_LIT("let");
    static struct u_object l_if_1279 = U_LIT("if");
    static struct u_object l_lambda_1280 = U_LIT("lambda");
    static struct u_object l_match_1281 = U_LIT("match");
    static struct u_object l_true_1282 = U_LIT("true");
    static struct u_object l_false_1283 = U_LIT("false");
    u_value t1285 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1278, u_mk_con(&n_list_46_Cons, 2, &l_if_1279, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1280, u_mk_con(&n_list_46_Cons, 2, &l_match_1281, u_mk_con(&n_list_46_Cons, 2, &l_true_1282, u_mk_con(&n_list_46_Cons, 2, &l_false_1283, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1285)) {
      u_value t1284 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1284)) {
        t1284 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1285 = t1284;
    }
    t1277 = t1285;
  }
  if ((!t1277) && u_is_con(s1276, &n_lex_46_TNum)) {
    t1277 = u_mk_bool(1);
  }
  if ((!t1277) && u_is_con(s1276, &n_lex_46_TStr)) {
    t1277 = u_mk_bool(1);
  }
  if ((!t1277) && u_is_con(s1276, &n_lex_46_TChar)) {
    t1277 = u_mk_bool(1);
  }
  if ((!t1277) && u_is_con(s1276, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1276, 0);
    u_value t1286 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1286)) {
      t1286 = u_eq(v_c, u_mk_char(91));
    }
    u_value t1287 = t1286;
    if (!u_truth(t1287)) {
      t1287 = u_eq(v_c, u_mk_char(123));
    }
    t1277 = t1287;
  }
  if (!t1277) {
    t1277 = u_mk_bool(0);
  }
  if (!t1277) {
    u_no_case(s1276);
  }
  return t1277;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    static struct u_object l_raise_1288 = U_LIT("raise");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_raise_1288))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, r_e, r_ts);
        u_value t1289 = u_mk_con(&n_syntax_46_ERaise, 2, *r_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_e = t1289;
      }
    } else {
      {
        p_parse_46_operators(v_sc, r_e, r_ts);
        static struct u_object l_with_1290 = U_LIT("with");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_with_1290))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_update(v_sc, r_e, r_ts);
            u_value t1291 = f_parse_46_at_update_1(*r_ts);
            while (u_truth(t1291)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_update(v_sc, r_e, r_ts);
              }
              t1291 = f_parse_46_at_update_1(*r_ts);
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
    static struct u_object l__58__61__1292 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1292), r_ts);
    p_parse_46_operators(v_sc, &v_a, r_ts);
    static struct u_object l__46__1293 = U_LIT(".");
    static struct u_object l__58__61__1294 = U_LIT(":=");
    u_value t1295 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1293, u_concat(v_f, &l__58__61__1294))), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1295;
  }
}

u_value f_parse_46_at_update_1(u_value v_ts) {
  u_value s1296 = v_ts;
  u_value t1297 = NULL;
  if ((!t1297) && (u_is_con(s1296, &n_list_46_Cons) && (u_is_con(u_field(u_field(s1296, 0), 0), &n_lex_46_TPunct) && (u_equals(u_field(u_field(u_field(s1296, 0), 0), 0), u_mk_char(44)) && (u_is_con(u_field(s1296, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s1296, 1), 0), 0), &n_lex_46_TName) && (u_is_con(u_field(u_field(s1296, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(u_field(s1296, 1), 1), 0), 0), &n_lex_46_TSym) && u_equals(u_field(u_field(u_field(u_field(u_field(s1296, 1), 1), 0), 0), 0), u_mk_lit(":=")))))))))) {
    t1297 = u_mk_bool(1);
  }
  if (!t1297) {
    t1297 = u_mk_bool(0);
  }
  if (!t1297) {
    u_no_case(s1296);
  }
  return t1297;
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
    u_value t1298 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1298;
    u_value t1299 = u_mk_bool(1);
    v_next = t1299;
    u_value t1300 = u_mk_bool(1);
    v_go = t1300;
    u_value t1309 = v_go;
    while (u_truth(t1309)) {
      {
        u_value s1301 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1302 = 0;
        if ((!d1302) && u_is_con(s1301, &n_option_46_Some)) {
          u_value v_op = u_field(s1301, 0);
          d1302 = 1;
          {
            u_value t1303 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1304 = u_mk_bool(1);
            v_rev = t1303;
            v_next = t1304;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1302) && u_is_con(s1301, &n_option_46_None)) {
          u_value t1307 = v_next;
          if (u_truth(t1307)) {
            t1307 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1307)) {
            d1302 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1305 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1306 = u_mk_bool(0);
              v_rev = t1305;
              v_next = t1306;
            }
          }
        }
        if (!d1302) {
          d1302 = 1;
          {
            u_value t1308 = u_mk_bool(0);
            v_go = t1308;
          }
        }
        if (!d1302) {
          u_no_case(s1301);
        }
      }
      t1309 = v_go;
    }
    u_value s1310 = v_rev;
    int d1311 = 0;
    if ((!d1311) && u_is_con(s1310, &n_list_46_Nil)) {
      d1311 = 1;
      {
        static struct u_object l_expression_32_expec_1312 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1313 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1312, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1313)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1311) {
      d1311 = 1;
      {
        u_value t1314 = f_list_46_reverse_1(v_rev);
        *r_items = t1314;
      }
    }
    if (!d1311) {
      u_no_case(s1310);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1315 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1315;
    u_value t1317 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1317)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1316 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1316;
      }
      t1317 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1318 = f_list_46_reverse_1(v_rev);
    *r_es = t1318;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1319 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1319;
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
    u_value t1320 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1320)) {
      t1320 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1324 = t1320;
    while (u_truth(t1324)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1321 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1321;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1322 = U_LIT(".");
            u_value t1323 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1322, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1323;
          }
        }
      }
      u_value t1325 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1325)) {
        t1325 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1324 = t1325;
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
    u_value s1326 = f_parse_46_head_token_1(*r_ts);
    int d1327 = 0;
    if ((!d1327) && (u_is_con(s1326, &n_lex_46_TName) && u_equals(u_field(s1326, 0), u_mk_lit("let")))) {
      d1327 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1327) && (u_is_con(s1326, &n_lex_46_TName) && u_equals(u_field(s1326, 0), u_mk_lit("if")))) {
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1328 = U_LIT("then");
        p_parse_46_keyword(&l_then_1328, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1329 = U_LIT("else");
        p_parse_46_keyword(&l_else_1329, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1330 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1330;
      }
    }
    if ((!d1327) && (u_is_con(s1326, &n_lex_46_TName) && u_equals(u_field(s1326, 0), u_mk_lit("lambda")))) {
      d1327 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1327) && (u_is_con(s1326, &n_lex_46_TName) && u_equals(u_field(s1326, 0), u_mk_lit("match")))) {
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1331 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1331;
      }
    }
    if ((!d1327) && (u_is_con(s1326, &n_lex_46_TName) && u_equals(u_field(s1326, 0), u_mk_lit("true")))) {
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1332 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1332;
      }
    }
    if ((!d1327) && (u_is_con(s1326, &n_lex_46_TName) && u_equals(u_field(s1326, 0), u_mk_lit("false")))) {
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1333 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1333;
      }
    }
    if ((!d1327) && u_is_con(s1326, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1326, 0);
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1334 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1334;
      }
    }
    if ((!d1327) && u_is_con(s1326, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1326, 0);
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1335 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1335;
      }
    }
    if ((!d1327) && u_is_con(s1326, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1326, 0);
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1336 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1336;
      }
    }
    if ((!d1327) && (u_is_con(s1326, &n_lex_46_TPunct) && u_equals(u_field(s1326, 0), u_mk_char(40)))) {
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1337 = v_es;
        int d1338 = 0;
        if ((!d1338) && (u_is_con(s1337, &n_list_46_Cons) && u_is_con(u_field(s1337, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1337, 0);
          d1338 = 1;
          {
            u_value t1339 = v_x;
            *r_e = t1339;
          }
        }
        if (!d1338) {
          d1338 = 1;
          {
            u_value t1340 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1340;
          }
        }
        if (!d1338) {
          u_no_case(s1337);
        }
      }
    }
    if ((!d1327) && (u_is_con(s1326, &n_lex_46_TPunct) && u_equals(u_field(s1326, 0), u_mk_char(91)))) {
      d1327 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1327) && (u_is_con(s1326, &n_lex_46_TPunct) && u_equals(u_field(s1326, 0), u_mk_char(123)))) {
      d1327 = 1;
      {
        p_parse_46_parse_braces(v_sc, r_e, r_ts);
      }
    }
    if ((!d1327) && u_is_con(s1326, &n_lex_46_TName)) {
      u_value v_n = u_field(s1326, 0);
      u_value t1342 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1342)) {
        t1342 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t1342)) {
        d1327 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1341 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1341;
        }
      }
    }
    if (!d1327) {
      d1327 = 1;
      {
        static struct u_object l_expression_32_expec_1343 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1344 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1343, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1344)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1327) {
      u_no_case(s1326);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1345 = v_ps;
  u_value t1346 = NULL;
  if ((!t1346) && u_is_con(s1345, &n_list_46_Nil)) {
    t1346 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1346) && (u_is_con(s1345, &n_list_46_Cons) && u_is_con(u_field(s1345, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1345, 0), 0);
    u_value v_more = u_field(s1345, 1);
    u_value s1347 = f_parse_46_pattern_vars_1(v_more);
    u_value t1348 = NULL;
    if ((!t1348) && u_is_con(s1347, &n_option_46_Some)) {
      u_value v_xs = u_field(s1347, 0);
      t1348 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1348) && u_is_con(s1347, &n_option_46_None)) {
      t1348 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1348) {
      u_no_case(s1347);
    }
    t1346 = t1348;
  }
  if (!t1346) {
    t1346 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1346) {
    u_no_case(s1345);
  }
  return t1346;
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
    static struct u_object l__58__61__1349 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1349), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1350 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1351 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1350;
    v_es = t1351;
    u_value t1355 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1355)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1352 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1352), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1353 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1354 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1353;
        v_es = t1354;
      }
      t1355 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1356 = U_LIT("in");
    p_parse_46_keyword(&l_in_1356, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1357 = f_list_46_reverse_1(v_ps);
    u_value t1358 = f_list_46_reverse_1(v_es);
    v_ps = t1357;
    v_es = t1358;
    u_value s1359 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1360 = 0;
    if ((!d1360) && u_is_con(u_field(s1359, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1359, 0), 0);
      d1360 = 1;
      {
        u_value t1361 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1361;
      }
    }
    if ((!d1360) && (u_is_con(u_field(s1359, 0), &n_option_46_None) && (u_is_con(u_field(s1359, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1359, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1359, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1359, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1359, 1), 0);
      u_value v_c = u_field(u_field(s1359, 2), 0);
      d1360 = 1;
      {
        u_value t1362 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1362;
      }
    }
    if (!d1360) {
      d1360 = 1;
      {
        u_value t1363 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1363;
      }
    }
    if (!d1360) {
      u_no_case(s1359);
    }
  }
}

void p_parse_46_parse_lambda(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    p_parse_46_advance(r_ts);
    u_value t1364 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1364;
    static struct u_object l__61__62__1365 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1365)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1366 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1366;
        u_value t1368 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1368)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1367 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1367;
          }
          t1368 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1369 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1369), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1370 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1370;
  }
}

void p_parse_46_parse_braces(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_at = NULL;
    u_value v_k = NULL;
    u_value v_v = NULL;
    u_value v_entries = NULL;
    u_value v_rev = NULL;
    u_value t1371 = f_parse_46_head_offset_1(*r_ts);
    v_at = t1371;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_expr(v_sc, &v_k, r_ts);
    static struct u_object l__61__62__1372 = U_LIT("=>");
    u_value t1373 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1372));
    v_entries = t1373;
    if (u_truth(v_entries)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_v, r_ts);
      }
    }
    static struct u_object l_for_1374 = U_LIT("for");
    u_value t1375 = f_parse_46_at_keyword_2(*r_ts, &l_for_1374);
    if (u_truth(t1375)) {
      t1375 = v_entries;
    }
    if (u_truth(t1375)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_comprehension(v_sc, v_k, v_v, r_e, r_ts);
      }
    } else {
      static struct u_object l_for_1376 = U_LIT("for");
      if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1376))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_set_comprehension(v_sc, v_k, r_e, r_ts);
        }
      } else {
        {
          u_value t1377;
          if (u_truth(v_entries)) {
            t1377 = u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))));
          } else {
            t1377 = v_k;
          }
          u_value t1378 = u_mk_con(&n_list_46_Cons, 2, t1377, u_mk_con(&n_list_46_Nil, 0));
          v_rev = t1378;
          u_value t1383 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          while (u_truth(t1383)) {
            {
              p_parse_46_advance(r_ts);
              p_parse_46_parse_expr(v_sc, &v_k, r_ts);
              static struct u_object l__61__62__1379 = U_LIT("=>");
              if (u_truth(f_prelude_46__33__61__2(v_entries, u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1379))))) {
                {
                  static struct u_object l_a_32_literal_32__123__46__46__46__125__32__1380 = U_LIT("a literal {...} has either elements a or entries k => v");
                  u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_a_32_literal_32__123__46__46__46__125__32__1380, v_at), "parse.ParseError");
                }
              }
              if (u_truth(v_entries)) {
                {
                  p_parse_46_advance(r_ts);
                  p_parse_46_parse_expr(v_sc, &v_v, r_ts);
                  u_value t1381 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)))), v_rev);
                  v_rev = t1381;
                }
              } else {
                {
                  u_value t1382 = u_mk_con(&n_list_46_Cons, 2, v_k, v_rev);
                  v_rev = t1382;
                }
              }
            }
            t1383 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          }
          u_value t1390;
          if (u_truth(v_entries)) {
            static struct u_object l_map_1384 = U_LIT("map");
            static struct u_object l_put_all_1385 = U_LIT("put_all");
            static struct u_object l_map_1386 = U_LIT("map");
            static struct u_object l_empty_1387 = U_LIT("empty");
            t1390 = f_parse_46_in_library_3(&l_map_1384, &l_put_all_1385, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1386, &l_empty_1387, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
          } else {
            static struct u_object l_set_1388 = U_LIT("set");
            static struct u_object l_from_list_1389 = U_LIT("from_list");
            t1390 = f_parse_46_in_library_3(&l_set_1388, &l_from_list_1389, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Nil, 0)));
          }
          u_value t1391 = t1390;
          *r_e = t1391;
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
  static struct u_object l_Nil_1392 = U_LIT("Nil");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1392), u_mk_con(&n_syntax_46_THole, 0)));
  return cache;
}

u_value f_parse_46_in_library_3(u_value v_m, u_value v_f, u_value v_args) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), v_f), u_mk_con(&n_syntax_46_THole, 0))), v_args);
}

void p_parse_46_set_comprehension(u_value v_sc, u_value v_r, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_l = NULL;
    u_value v_xs = NULL;
    u_value v_ls = NULL;
    u_value v_c = NULL;
    u_value t1393 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1393;
    u_value t1394 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1394;
    u_value t1395 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1395;
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l_in_1396 = U_LIT("in");
    p_parse_46_keyword(&l_in_1396, r_ts);
    p_parse_46_parse_expr(v_sc, &v_l, r_ts);
    static struct u_object l_set_1397 = U_LIT("set");
    static struct u_object l_elements_1398 = U_LIT("elements");
    u_value t1399 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    u_value t1400 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1397, &l_elements_1398, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
    v_xs = t1399;
    v_ls = t1400;
    u_value t1406 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1406)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l_in_1401 = U_LIT("in");
        p_parse_46_keyword(&l_in_1401, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_set_1402 = U_LIT("set");
        static struct u_object l_elements_1403 = U_LIT("elements");
        u_value t1404 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
        u_value t1405 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1402, &l_elements_1403, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), v_ls);
        v_xs = t1404;
        v_ls = t1405;
      }
      t1406 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1407 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1407))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
      }
    }
    static struct u_object l_set_1408 = U_LIT("set");
    static struct u_object l_from_list_1409 = U_LIT("from_list");
    u_value t1410 = f_parse_46_in_library_3(&l_set_1408, &l_from_list_1409, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, v_r, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Nil, 0)));
    *r_e = t1410;
  }
}

void p_parse_46_map_comprehension(u_value v_sc, u_value v_k, u_value v_v, u_value *r_e, u_value *r_ts) {
  {
    u_value v_gs = NULL;
    u_value v_ls = NULL;
    u_value v_c = NULL;
    u_value t1411 = u_mk_con(&n_list_46_Nil, 0);
    v_gs = t1411;
    u_value t1412 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1412;
    u_value t1413 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1413;
    p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
    u_value t1414 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1414)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
      }
      t1414 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1415 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1415))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
        u_value t1416 = f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), v_c);
        v_c = t1416;
      }
    }
    static struct u_object l_map_1417 = U_LIT("map");
    static struct u_object l_put_all_1418 = U_LIT("put_all");
    static struct u_object l_map_1419 = U_LIT("map");
    static struct u_object l_empty_1420 = U_LIT("empty");
    u_value t1421 = f_parse_46_in_library_3(&l_map_1417, &l_put_all_1418, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))))), f_list_46_reverse_1(f_parse_46_hidden_vars_1(v_gs)), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1419, &l_empty_1420, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1421;
  }
}

void p_parse_46_map_generator(u_value v_sc, u_value *r_gs, u_value *r_ls, u_value *r_ts) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value v_m = NULL;
    p_parse_46_parse_plain(v_sc, &v_a, r_ts);
    static struct u_object l__61__62__1422 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1422), r_ts);
    p_parse_46_parse_plain(v_sc, &v_b, r_ts);
    static struct u_object l_in_1423 = U_LIT("in");
    p_parse_46_keyword(&l_in_1423, r_ts);
    p_parse_46_parse_expr(v_sc, &v_m, r_ts);
    static struct u_object l_map_1424 = U_LIT("map");
    static struct u_object l_entries_1425 = U_LIT("entries");
    u_value t1426 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1424, &l_entries_1425, u_mk_con(&n_list_46_Cons, 2, f_parse_46_unpack_2(f_list_46_reverse_1(*r_gs), v_m), u_mk_con(&n_list_46_Nil, 0))), *r_ls);
    *r_ls = t1426;
    static struct u_object l__40_entry_32__1427 = U_LIT("(entry ");
    static struct u_object l__41__1428 = U_LIT(")");
    u_value t1429 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(3, v_a, v_b, u_concat(&l__40_entry_32__1427, u_concat(u_str_show(u_add(f_list_46_length_1(*r_gs), u_mk_int(1))), &l__41__1428))), *r_gs);
    *r_gs = t1429;
  }
}

u_value f_parse_46_hidden_vars_1(u_value v_gs) {
  u_value s1430 = v_gs;
  u_value t1431 = NULL;
  if ((!t1431) && u_is_con(s1430, &n_list_46_Nil)) {
    t1431 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1431) && u_is_con(s1430, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s1430, 0), 2);
    u_value v_rest = u_field(s1430, 1);
    t1431 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0)), f_parse_46_hidden_vars_1(v_rest));
  }
  if (!t1431) {
    u_no_case(s1430);
  }
  return t1431;
}

u_value f_parse_46_unpack_2(u_value v_gs, u_value v_e) {
  u_value s1432 = v_gs;
  u_value t1433 = NULL;
  if ((!t1433) && u_is_con(s1432, &n_list_46_Nil)) {
    t1433 = v_e;
  }
  if ((!t1433) && u_is_con(s1432, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s1432, 0), 0);
    u_value v_v = u_field(u_field(s1432, 0), 1);
    u_value v_h = u_field(u_field(s1432, 0), 2);
    u_value v_rest = u_field(s1432, 1);
    t1433 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_k), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_v), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_parse_46_unpack_2(v_rest, v_e)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1433) {
    u_no_case(s1432);
  }
  return t1433;
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
        static struct u_object l_Nil_1434 = U_LIT("Nil");
        u_value t1435 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1434), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1435;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1436 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1436))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1437 = U_LIT("in");
            p_parse_46_keyword(&l_in_1437, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1438 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1439 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1438;
            v_lists = t1439;
            u_value t1443 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1443)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1440 = U_LIT("in");
                p_parse_46_keyword(&l_in_1440, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1441 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1442 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1441;
                v_lists = t1442;
              }
              t1443 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1444 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1444;
            static struct u_object l_if_1445 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1445))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1446 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1446;
          }
        } else {
          {
            u_value t1447 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1447;
            u_value t1449 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1449)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1448 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1448;
              }
              t1449 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1450 = U_LIT("Nil");
            u_value t1451 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1450), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1451;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1452 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1452;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1453 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1453))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1454 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1454;
      }
    } else {
      {
        u_value t1455 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1455;
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
    u_value t1456 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1456;
    static struct u_object l_case_1457 = U_LIT("case");
    u_value t1460 = f_parse_46_at_keyword_2(*r_ts, &l_case_1457);
    while (u_truth(t1460)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1458 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1458), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1459 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1459;
      }
      static struct u_object l_case_1461 = U_LIT("case");
      t1460 = f_parse_46_at_keyword_2(*r_ts, &l_case_1461);
    }
    static struct u_object l_else_1462 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1462))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1463 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1463;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1464 = U_LIT("end");
    p_parse_46_keyword(&l_end_1464, r_ts);
    u_value t1465 = f_list_46_reverse_1(v_rev);
    *r_cs = t1465;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1466 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1466))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1467 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1467, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
  }
}

u_value f_parse_46_pat_op1_2(u_value v_op, u_value v_p) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_pat_prefix_2(u_value v_op, u_value v_p) {
  u_value s1468 = v_p;
  u_value t1469 = NULL;
  if ((!t1469) && (u_is_con(s1468, &n_syntax_46_PLit) && u_is_con(u_field(s1468, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1468, 0), 0);
    static struct u_object l__45__1470 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1470))) {
      t1469 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1469) {
    t1469 = f_parse_46_pat_op1_2(v_op, v_p);
  }
  if (!t1469) {
    u_no_case(s1468);
  }
  return t1469;
}

u_value f_parse_46_pat_op2_3(u_value v_op, u_value v_l, u_value v_r) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_pats_2(u_value v_ps, u_value v_rest) {
  u_value s1471 = v_ps;
  u_value t1472 = NULL;
  if ((!t1472) && u_is_con(s1471, &n_list_46_Nil)) {
    t1472 = v_rest;
  }
  if ((!t1472) && u_is_con(s1471, &n_list_46_Cons)) {
    u_value v_p = u_field(s1471, 0);
    u_value v_more = u_field(s1471, 1);
    static struct u_object l_Cons_1473 = U_LIT("Cons");
    t1472 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1473), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1472) {
    u_no_case(s1471);
  }
  return t1472;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1474 = v_t;
  u_value t1475 = NULL;
  if ((!t1475) && u_is_con(s1474, &n_lex_46_TName)) {
    u_value v_n = u_field(s1474, 0);
    static struct u_object l___1476 = U_LIT("_");
    static struct u_object l_true_1477 = U_LIT("true");
    static struct u_object l_false_1478 = U_LIT("false");
    u_value t1480 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1476, u_mk_con(&n_list_46_Cons, 2, &l_true_1477, u_mk_con(&n_list_46_Cons, 2, &l_false_1478, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1480)) {
      u_value t1479 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1479)) {
        t1479 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1480 = t1479;
    }
    t1475 = t1480;
  }
  if ((!t1475) && u_is_con(s1474, &n_lex_46_TNum)) {
    t1475 = u_mk_bool(1);
  }
  if ((!t1475) && u_is_con(s1474, &n_lex_46_TStr)) {
    t1475 = u_mk_bool(1);
  }
  if ((!t1475) && u_is_con(s1474, &n_lex_46_TChar)) {
    t1475 = u_mk_bool(1);
  }
  if ((!t1475) && u_is_con(s1474, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1474, 0);
    u_value t1481 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1481)) {
      t1481 = u_eq(v_c, u_mk_char(91));
    }
    t1475 = t1481;
  }
  if (!t1475) {
    t1475 = u_mk_bool(0);
  }
  if (!t1475) {
    u_no_case(s1474);
  }
  return t1475;
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
    u_value t1482 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1482;
    u_value t1483 = u_mk_bool(1);
    v_next = t1483;
    u_value t1484 = u_mk_bool(1);
    v_go = t1484;
    u_value t1493 = v_go;
    while (u_truth(t1493)) {
      {
        u_value s1485 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1486 = 0;
        if ((!d1486) && u_is_con(s1485, &n_option_46_Some)) {
          u_value v_op = u_field(s1485, 0);
          d1486 = 1;
          {
            u_value t1487 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1488 = u_mk_bool(1);
            v_rev = t1487;
            v_next = t1488;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1486) && u_is_con(s1485, &n_option_46_None)) {
          u_value t1491 = v_next;
          if (u_truth(t1491)) {
            t1491 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1491)) {
            d1486 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1489 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1490 = u_mk_bool(0);
              v_rev = t1489;
              v_next = t1490;
            }
          }
        }
        if (!d1486) {
          d1486 = 1;
          {
            u_value t1492 = u_mk_bool(0);
            v_go = t1492;
          }
        }
        if (!d1486) {
          u_no_case(s1485);
        }
      }
      t1493 = v_go;
    }
    u_value s1494 = v_rev;
    int d1495 = 0;
    if ((!d1495) && u_is_con(s1494, &n_list_46_Nil)) {
      d1495 = 1;
      {
        static struct u_object l_pattern_32_expected_1496 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1497 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1496, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1497)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1495) {
      d1495 = 1;
      {
        u_value t1498 = f_list_46_reverse_1(v_rev);
        *r_items = t1498;
      }
    }
    if (!d1495) {
      u_no_case(s1494);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1499 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1499;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1500 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1500;
        u_value t1502 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1502)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1501 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1501;
          }
          t1502 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1503 = f_list_46_reverse_1(v_rev);
    *r_ps = t1503;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1504 = f_parse_46_head_token_1(*r_ts);
    int d1505 = 0;
    if ((!d1505) && (u_is_con(s1504, &n_lex_46_TName) && u_equals(u_field(s1504, 0), u_mk_lit("_")))) {
      d1505 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1506 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1506;
      }
    }
    if ((!d1505) && (u_is_con(s1504, &n_lex_46_TName) && u_equals(u_field(s1504, 0), u_mk_lit("true")))) {
      d1505 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1507 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1507;
      }
    }
    if ((!d1505) && (u_is_con(s1504, &n_lex_46_TName) && u_equals(u_field(s1504, 0), u_mk_lit("false")))) {
      d1505 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1508 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1508;
      }
    }
    if ((!d1505) && u_is_con(s1504, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1504, 0);
      d1505 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1509 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1509;
      }
    }
    if ((!d1505) && u_is_con(s1504, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1504, 0);
      d1505 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1510 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1510;
      }
    }
    if ((!d1505) && u_is_con(s1504, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1504, 0);
      d1505 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1511 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1511;
      }
    }
    if ((!d1505) && (u_is_con(s1504, &n_lex_46_TPunct) && u_equals(u_field(s1504, 0), u_mk_char(40)))) {
      d1505 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1512 = v_ps;
        int d1513 = 0;
        if ((!d1513) && (u_is_con(s1512, &n_list_46_Cons) && u_is_con(u_field(s1512, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1512, 0);
          d1513 = 1;
          {
            u_value t1514 = v_q;
            *r_p = t1514;
          }
        }
        if (!d1513) {
          d1513 = 1;
          {
            u_value t1515 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1515;
          }
        }
        if (!d1513) {
          u_no_case(s1512);
        }
      }
    }
    if ((!d1505) && (u_is_con(s1504, &n_lex_46_TPunct) && u_equals(u_field(s1504, 0), u_mk_char(91)))) {
      d1505 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1516 = U_LIT("Nil");
            u_value t1517 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1516), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1517;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1518 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1518;
            u_value t1520 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1520)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1519 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1519;
              }
              t1520 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1521 = U_LIT("Nil");
            u_value t1522 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1521), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1522;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1523 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1523;
          }
        }
      }
    }
    if (!d1505) {
      d1505 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1524 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1524;
          }
        } else {
          {
            u_value t1525 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1525;
          }
        }
      }
    }
    if (!d1505) {
      u_no_case(s1504);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1526 = v_t;
  u_value t1527 = NULL;
  if ((!t1527) && u_is_con(s1526, &n_lex_46_TName)) {
    u_value v_n = u_field(s1526, 0);
    static struct u_object l_skip_1528 = U_LIT("skip");
    static struct u_object l_if_1529 = U_LIT("if");
    static struct u_object l_while_1530 = U_LIT("while");
    static struct u_object l_call_1531 = U_LIT("call");
    static struct u_object l_match_1532 = U_LIT("match");
    static struct u_object l_assert_1533 = U_LIT("assert");
    static struct u_object l_assign_1534 = U_LIT("assign");
    static struct u_object l_raise_1535 = U_LIT("raise");
    static struct u_object l_try_1536 = U_LIT("try");
    t1527 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1528, u_mk_con(&n_list_46_Cons, 2, &l_if_1529, u_mk_con(&n_list_46_Cons, 2, &l_while_1530, u_mk_con(&n_list_46_Cons, 2, &l_call_1531, u_mk_con(&n_list_46_Cons, 2, &l_match_1532, u_mk_con(&n_list_46_Cons, 2, &l_assert_1533, u_mk_con(&n_list_46_Cons, 2, &l_assign_1534, u_mk_con(&n_list_46_Cons, 2, &l_raise_1535, u_mk_con(&n_list_46_Cons, 2, &l_try_1536, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1527) {
    t1527 = u_mk_bool(0);
  }
  if (!t1527) {
    u_no_case(s1526);
  }
  return t1527;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1537 = v_cs;
  u_value t1538 = NULL;
  if ((!t1538) && u_is_con(s1537, &n_list_46_Nil)) {
    t1538 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1538) && (u_is_con(s1537, &n_list_46_Cons) && u_is_con(u_field(s1537, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1537, 0), 0);
    u_value v_g = u_field(u_field(s1537, 0), 1);
    u_value v_b = u_field(u_field(s1537, 0), 2);
    u_value v_more = u_field(s1537, 1);
    t1538 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1538) {
    u_no_case(s1537);
  }
  return t1538;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1539 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1539;
    u_value t1540 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1540;
    u_value t1541 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1541;
    static struct u_object l_local_1542 = U_LIT("local");
    u_value t1544 = f_parse_46_at_keyword_2(*r_ts, &l_local_1542);
    while (u_truth(t1544)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1543 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1543)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1543 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1545 = U_LIT("local");
      t1544 = f_parse_46_at_keyword_2(*r_ts, &l_local_1545);
    }
    u_value t1547 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1547)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1546 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1546;
      }
      t1547 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1548 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1548;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
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
    static struct u_object l__58__61__1551 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1551)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1552 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1552;
      }
    }
    u_value t1553 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1553;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1554 = f_parse_46_head_token_1(*r_ts);
    int d1555 = 0;
    if ((!d1555) && (u_is_con(s1554, &n_lex_46_TName) && u_equals(u_field(s1554, 0), u_mk_lit("skip")))) {
      d1555 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1556 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1556;
      }
    }
    if ((!d1555) && (u_is_con(s1554, &n_lex_46_TName) && u_equals(u_field(s1554, 0), u_mk_lit("if")))) {
      d1555 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1555) && (u_is_con(s1554, &n_lex_46_TName) && u_equals(u_field(s1554, 0), u_mk_lit("while")))) {
      d1555 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1557 = U_LIT("end");
        p_parse_46_keyword(&l_end_1557, r_ts);
        u_value t1558 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1558;
      }
    }
    if ((!d1555) && (u_is_con(s1554, &n_lex_46_TName) && u_equals(u_field(s1554, 0), u_mk_lit("call")))) {
      d1555 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1555) && (u_is_con(s1554, &n_lex_46_TName) && u_equals(u_field(s1554, 0), u_mk_lit("match")))) {
      d1555 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1559 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1559;
      }
    }
    if ((!d1555) && (u_is_con(s1554, &n_lex_46_TName) && u_equals(u_field(s1554, 0), u_mk_lit("assert")))) {
      d1555 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1560 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1560;
      }
    }
    if ((!d1555) && (u_is_con(s1554, &n_lex_46_TName) && u_equals(u_field(s1554, 0), u_mk_lit("assign")))) {
      d1555 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1555) && (u_is_con(s1554, &n_lex_46_TName) && u_equals(u_field(s1554, 0), u_mk_lit("raise")))) {
      d1555 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1561 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1561;
      }
    }
    if ((!d1555) && (u_is_con(s1554, &n_lex_46_TName) && u_equals(u_field(s1554, 0), u_mk_lit("try")))) {
      d1555 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1562 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1562, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1563 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1563;
      }
    }
    if (!d1555) {
      d1555 = 1;
      {
        static struct u_object l_statement_32_expect_1564 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1565 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1564, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1565)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1555) {
      u_no_case(s1554);
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
    static struct u_object l_else_1566 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1566)))) {
      {
        static struct u_object l_end_1567 = U_LIT("end");
        p_parse_46_keyword(&l_end_1567, r_ts);
        u_value t1568 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1568;
      }
    } else {
      static struct u_object l_if_1569 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1569)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1570 = U_LIT("end");
          p_parse_46_keyword(&l_end_1570, r_ts);
        }
      }
    }
    u_value t1571 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1571;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1572 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1572), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1573 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1573;
    u_value t1576 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1576)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1574 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1574), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1575 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1575;
      }
      t1576 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1577 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1577;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1578 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1578;
    static struct u_object l_case_1579 = U_LIT("case");
    u_value t1582 = f_parse_46_at_keyword_2(*r_ts, &l_case_1579);
    while (u_truth(t1582)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1580 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1580), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1581 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1581;
      }
      static struct u_object l_case_1583 = U_LIT("case");
      t1582 = f_parse_46_at_keyword_2(*r_ts, &l_case_1583);
    }
    static struct u_object l_else_1584 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1584))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1585 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1585;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1586 = U_LIT("end");
    p_parse_46_keyword(&l_end_1586, r_ts);
    u_value t1587 = f_list_46_reverse_1(v_rev);
    *r_cs = t1587;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1589;
  if (u_truth(v_first)) {
    t1589 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1588 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1588)) {
      t1588 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1589 = t1588;
  }
  return t1589;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1591 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1591)) {
    static struct u_object l_out_1590 = U_LIT("out");
    t1591 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1590));
  }
  u_value t1593 = t1591;
  if (u_truth(t1593)) {
    static struct u_object l_ref_1592 = U_LIT("ref");
    t1593 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1592));
  }
  return t1593;
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
    u_value t1594 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1595 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1596 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1597 = u_mk_bool(1);
    v_ins = t1594;
    v_outs = t1595;
    v_refs = t1596;
    v_first = t1597;
    static struct u_object l_in_1598 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1598))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1599 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
        u_value t1600 = u_mk_bool(0);
        v_ins = t1599;
        v_first = t1600;
        u_value t1602 = f_parse_46_item_follows_1(*r_ts);
        while (u_truth(t1602)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_expr(v_sc, &v_e, r_ts);
            u_value t1601 = u_mk_con(&n_list_46_Cons, 2, v_e, v_ins);
            v_ins = t1601;
          }
          t1602 = f_parse_46_item_follows_1(*r_ts);
        }
      }
    }
    static struct u_object l_out_1603 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1603, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_outs, r_ts);
        u_value t1604 = u_mk_bool(0);
        v_first = t1604;
      }
    }
    static struct u_object l_ref_1605 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1605, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    u_value t1606 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), f_list_46_reverse_1(v_ins), v_outs, v_refs);
    *r_s = t1606;
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
    u_value t1607 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1607;
    u_value t1609 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1609)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1608 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_rev);
        v_rev = t1608;
      }
      t1609 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1610 = f_list_46_reverse_1(v_rev);
    *r_xs = t1610;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1611 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1611;
    static struct u_object l__58__1612 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1612)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1613 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1613;
  }
}

void p_parse_46_parse_formals1(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_formal(v_sc, &v_x, r_ts);
    u_value t1614 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1614;
    u_value t1616 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1616)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formal(v_sc, &v_x, r_ts);
        u_value t1615 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1615;
      }
      t1616 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1617 = f_list_46_reverse_1(v_rev);
    *r_xs = t1617;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1618 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1618;
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
  u_value s1619 = v_cs;
  u_value t1620 = NULL;
  if ((!t1620) && u_is_con(s1619, &n_list_46_Nil)) {
    t1620 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1620) && (u_is_con(s1619, &n_list_46_Cons) && u_is_con(u_field(s1619, 0), &n_syntax_46_CEval))) {
    u_value v_more = u_field(s1619, 1);
    t1620 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1620) && (u_is_con(s1619, &n_list_46_Cons) && u_is_con(u_field(s1619, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1619, 1);
    t1620 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1620) && (u_is_con(s1619, &n_list_46_Cons) && u_is_con(u_field(s1619, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1619, 1);
    t1620 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1620) && u_is_con(s1619, &n_list_46_Cons)) {
    u_value v_c = u_field(s1619, 0);
    u_value v_more = u_field(s1619, 1);
    t1620 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1620) {
    u_no_case(s1619);
  }
  return t1620;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1621 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1622 = &t1621;
  u_value l1623 = v_cs;
  while (u_is_con(l1623, &n_list_46_Cons)) {
    u_value v_c = u_field(l1623, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1622 = u_append(e1622, v_c);
    }
    l1623 = u_field(l1623, 1);
  }
  return t1621;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1624 = v_c;
  u_value t1625 = NULL;
  if ((!t1625) && u_is_con(s1624, &n_syntax_46_CNotation)) {
    t1625 = u_mk_bool(1);
  }
  if (!t1625) {
    t1625 = u_mk_bool(0);
  }
  if (!t1625) {
    u_no_case(s1624);
  }
  return t1625;
}

u_value f_parse_46_Context_46_notations_1(u_value v_x) {
  u_value s1626 = v_x;
  u_value t1627 = NULL;
  if ((!t1627) && u_is_con(s1626, &n_parse_46_Context)) {
    u_value v_y = u_field(s1626, 0);
    t1627 = v_y;
  }
  if (!t1627) {
    u_no_case(s1626);
  }
  return t1627;
}

u_value f_parse_46_Context_46_modules_1(u_value v_x) {
  u_value s1628 = v_x;
  u_value t1629 = NULL;
  if ((!t1629) && u_is_con(s1628, &n_parse_46_Context)) {
    u_value v_y = u_field(s1628, 1);
    t1629 = v_y;
  }
  if (!t1629) {
    u_no_case(s1628);
  }
  return t1629;
}

u_value f_parse_46_Context_46_loading_1(u_value v_x) {
  u_value s1630 = v_x;
  u_value t1631 = NULL;
  if ((!t1631) && u_is_con(s1630, &n_parse_46_Context)) {
    u_value v_y = u_field(s1630, 2);
    t1631 = v_y;
  }
  if (!t1631) {
    u_no_case(s1630);
  }
  return t1631;
}

u_value f_parse_46_Context_46_base_1(u_value v_x) {
  u_value s1632 = v_x;
  u_value t1633 = NULL;
  if ((!t1633) && u_is_con(s1632, &n_parse_46_Context)) {
    u_value v_y = u_field(s1632, 3);
    t1633 = v_y;
  }
  if (!t1633) {
    u_no_case(s1632);
  }
  return t1633;
}

u_value f_parse_46_Context_46_dir_1(u_value v_x) {
  u_value s1634 = v_x;
  u_value t1635 = NULL;
  if ((!t1635) && u_is_con(s1634, &n_parse_46_Context)) {
    u_value v_y = u_field(s1634, 4);
    t1635 = v_y;
  }
  if (!t1635) {
    u_no_case(s1634);
  }
  return t1635;
}

u_value f_parse_46_Context_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1636 = v_x;
  u_value t1637 = NULL;
  if ((!t1637) && u_is_con(s1636, &n_parse_46_Context)) {
    u_value v_a1 = u_field(s1636, 1);
    u_value v_a2 = u_field(s1636, 2);
    u_value v_a3 = u_field(s1636, 3);
    u_value v_a4 = u_field(s1636, 4);
    t1637 = u_mk_con(&n_parse_46_Context, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1637) {
    u_no_case(s1636);
  }
  return t1637;
}

u_value f_parse_46_Context_46_modules_58__61__2(u_value v_x, u_value v_y) {
  u_value s1638 = v_x;
  u_value t1639 = NULL;
  if ((!t1639) && u_is_con(s1638, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1638, 0);
    u_value v_a2 = u_field(s1638, 2);
    u_value v_a3 = u_field(s1638, 3);
    u_value v_a4 = u_field(s1638, 4);
    t1639 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t1639) {
    u_no_case(s1638);
  }
  return t1639;
}

u_value f_parse_46_Context_46_loading_58__61__2(u_value v_x, u_value v_y) {
  u_value s1640 = v_x;
  u_value t1641 = NULL;
  if ((!t1641) && u_is_con(s1640, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1640, 0);
    u_value v_a1 = u_field(s1640, 1);
    u_value v_a3 = u_field(s1640, 3);
    u_value v_a4 = u_field(s1640, 4);
    t1641 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t1641) {
    u_no_case(s1640);
  }
  return t1641;
}

u_value f_parse_46_Context_46_base_58__61__2(u_value v_x, u_value v_y) {
  u_value s1642 = v_x;
  u_value t1643 = NULL;
  if ((!t1643) && u_is_con(s1642, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1642, 0);
    u_value v_a1 = u_field(s1642, 1);
    u_value v_a2 = u_field(s1642, 2);
    u_value v_a4 = u_field(s1642, 4);
    t1643 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t1643) {
    u_no_case(s1642);
  }
  return t1643;
}

u_value f_parse_46_Context_46_dir_58__61__2(u_value v_x, u_value v_y) {
  u_value s1644 = v_x;
  u_value t1645 = NULL;
  if ((!t1645) && u_is_con(s1644, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1644, 0);
    u_value v_a1 = u_field(s1644, 1);
    u_value v_a2 = u_field(s1644, 2);
    u_value v_a3 = u_field(s1644, 3);
    t1645 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t1645) {
    u_no_case(s1644);
  }
  return t1645;
}

u_value f_parse_46_with_commands_2(u_value v_cx, u_value v_cs) {
  return f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_with_notations_2(f_parse_46_Context_46_notations_1(v_cx), v_cs));
}

u_value f_parse_46_with_module_2(u_value v_cx, u_value v_c) {
  return f_parse_46_Context_46_modules_58__61__2(v_cx, f_list_46__43__43__2(f_parse_46_modules_of_1(v_c), f_parse_46_Context_46_modules_1(v_cx)));
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1646 = v_c;
  u_value t1647 = NULL;
  if ((!t1647) && u_is_con(s1646, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1646, 0);
    u_value v_cs = u_field(s1646, 1);
    u_value t1648 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1649 = &t1648;
    u_value l1650 = v_cs;
    while (u_is_con(l1650, &n_list_46_Cons)) {
      u_value v_d = u_field(l1650, 0);
      if (u_truth(u_mk_bool(1))) {
        e1649 = u_append(e1649, f_parse_46_modules_of_1(v_d));
      }
      l1650 = u_field(l1650, 1);
    }
    t1647 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1648));
  }
  if (!t1647) {
    t1647 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1647) {
    u_no_case(s1646);
  }
  return t1647;
}

u_value f_parse_46_own_notations_2(u_value v_cx, u_value v_n) {
  u_value s1651 = f_list_46_lookup_2(v_n, f_parse_46_Context_46_modules_1(v_cx));
  u_value t1652 = NULL;
  if ((!t1652) && u_is_con(s1651, &n_option_46_Some)) {
    u_value v_ns = u_field(s1651, 0);
    t1652 = v_ns;
  }
  if ((!t1652) && u_is_con(s1651, &n_option_46_None)) {
    t1652 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1652) {
    u_no_case(s1651);
  }
  return t1652;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1653 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1653;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1654 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1654;
        u_value t1656 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1656)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1655 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1655;
          }
          t1656 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1657 = f_list_46_reverse_1(v_rev);
    *r_ps = t1657;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1658 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1658;
    u_value s1659 = f_parse_46_head_token_1(*r_ts);
    int d1660 = 0;
    if ((!d1660) && (u_is_con(s1659, &n_lex_46_TName) && u_equals(u_field(s1659, 0), u_mk_lit("prefix")))) {
      d1660 = 1;
      {
        u_value t1661 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1661;
      }
    }
    if ((!d1660) && (u_is_con(s1659, &n_lex_46_TName) && u_equals(u_field(s1659, 0), u_mk_lit("postfix")))) {
      d1660 = 1;
      {
        u_value t1662 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1662;
      }
    }
    if (!d1660) {
      d1660 = 1;
      {
        u_value s1663 = f_parse_46_second_token_1(*r_ts);
        int d1664 = 0;
        if ((!d1664) && (u_is_con(s1663, &n_lex_46_TName) && u_equals(u_field(s1663, 0), u_mk_lit("left")))) {
          d1664 = 1;
          {
            u_value t1665 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1665;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1664) && (u_is_con(s1663, &n_lex_46_TName) && u_equals(u_field(s1663, 0), u_mk_lit("right")))) {
          d1664 = 1;
          {
            u_value t1666 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1666;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1664) {
          d1664 = 1;
          {
          }
        }
        if (!d1664) {
          u_no_case(s1663);
        }
      }
    }
    if (!d1660) {
      u_no_case(s1659);
    }
    p_parse_46_advance(r_ts);
    u_value s1667 = f_parse_46_head_token_1(*r_ts);
    int d1668 = 0;
    if ((!d1668) && u_is_con(s1667, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1667, 0);
      d1668 = 1;
      {
        u_value t1669 = v_n;
        v_p = t1669;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1668) {
      d1668 = 1;
      {
        static struct u_object l_precedence_32_expec_1670 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1671 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1670, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1671)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1668) {
      u_no_case(s1667);
    }
    u_value t1672 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1673 = u_mk_bool(1);
    v_ops = t1672;
    v_go = t1673;
    u_value t1679 = v_go;
    while (u_truth(t1679)) {
      {
        u_value s1674 = f_parse_46_head_token_1(*r_ts);
        int d1675 = 0;
        if ((!d1675) && u_is_con(s1674, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1674, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1675 = 1;
            {
              u_value t1676 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1676;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1675) && u_is_con(s1674, &n_lex_46_TName)) {
          u_value v_n = u_field(s1674, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1675 = 1;
            {
              u_value t1677 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1677;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1675) {
          d1675 = 1;
          {
            u_value t1678 = u_mk_bool(0);
            v_go = t1678;
          }
        }
        if (!d1675) {
          u_no_case(s1674);
        }
      }
      t1679 = v_go;
    }
    u_value s1680 = v_ops;
    int d1681 = 0;
    if ((!d1681) && u_is_con(s1680, &n_list_46_Nil)) {
      d1681 = 1;
      {
        static struct u_object l_operator_32_expecte_1682 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1683 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1682, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1683)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1681) {
      d1681 = 1;
      {
        u_value t1684 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1684;
      }
    }
    if (!d1681) {
      u_no_case(s1680);
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
    u_value t1685 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1686 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1685;
    *r_fs = t1686;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1687 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
            u_value t1688 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
            v_args = t1687;
            *r_fs = t1688;
            u_value t1691 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1691)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t1689 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t1690 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t1689;
                *r_fs = t1690;
              }
              t1691 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1692 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t1693 = f_list_46_reverse_1(*r_fs);
    *r_k = t1692;
    *r_fs = t1693;
    u_value s1694 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d1695 = 0;
    if ((!d1695) && u_is_con(s1694, &n_option_46_Some)) {
      u_value v_f = u_field(s1694, 0);
      d1695 = 1;
      {
        static struct u_object l_repeated_32_field_32__1696 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__1697 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__1696, u_concat(v_f, u_concat(&l__32_of_32__1697, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1695) && u_is_con(s1694, &n_option_46_None)) {
      d1695 = 1;
      {
      }
    }
    if (!d1695) {
      u_no_case(s1694);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1698 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1699 = 0;
    if ((!d1699) && (u_is_con(u_field(s1698, 0), &n_lex_46_TName) && (u_is_con(u_field(s1698, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1698, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1698, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1699 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1700 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1700;
        }
      }
    }
    if (!d1699) {
      d1699 = 1;
      {
        u_value t1701 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1701;
      }
    }
    if (!d1699) {
      u_no_case(s1698);
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
    static struct u_object l__58__61__1702 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1702), r_ts);
    static struct u_object l__124__1703 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1703)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1704 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1704;
    static struct u_object l__124__1705 = U_LIT("|");
    u_value t1707 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1705));
    while (u_truth(t1707)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1706 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1706;
      }
      static struct u_object l__124__1708 = U_LIT("|");
      t1707 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1708));
    }
    u_value s1709 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d1710 = 0;
    if ((!d1710) && u_is_con(s1709, &n_option_46_Some)) {
      u_value v_f = u_field(s1709, 0);
      d1710 = 1;
      {
        static struct u_object l_field_32__1711 = U_LIT("field ");
        static struct u_object l__32_of_32__1712 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_1713 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__1711, u_concat(v_f, u_concat(&l__32_of_32__1712, u_concat(v_n, &l__32_has_32_different_32_t_1713)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1710) && u_is_con(s1709, &n_option_46_None)) {
      d1710 = 1;
      {
      }
    }
    if (!d1710) {
      u_no_case(s1709);
    }
    u_value t1714 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_list_46__43__43__2(f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)), f_parse_46_updaters_3(v_n, v_ps, f_list_46_reverse_1(v_rev))));
    *r_cs = t1714;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s1715 = v_fs;
  u_value t1716 = NULL;
  if ((!t1716) && u_is_con(s1715, &n_list_46_Nil)) {
    t1716 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1716) && (u_is_con(s1715, &n_list_46_Cons) && u_is_con(u_field(s1715, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s1715, 0), 0);
    u_value v_rest = u_field(s1715, 1);
    t1716 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t1716) && (u_is_con(s1715, &n_list_46_Cons) && u_is_con(u_field(s1715, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1715, 1);
    t1716 = f_parse_46_field_names_1(v_rest);
  }
  if (!t1716) {
    u_no_case(s1715);
  }
  return t1716;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s1717 = v_fs;
  u_value t1718 = NULL;
  if ((!t1718) && u_is_con(s1717, &n_list_46_Nil)) {
    t1718 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1718) && u_is_con(s1717, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1717, 0), 0);
    u_value v_t = u_field(u_field(u_field(s1717, 0), 1), 3);
    u_value v_rest = u_field(s1717, 1);
    u_value t1719 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1720 = &t1719;
    u_value l1721 = f_parse_46_fields_named_2(v_f, v_rest);
    while (u_is_con(l1721, &n_list_46_Cons)) {
      u_value v_e = u_field(l1721, 0);
      if (u_truth(u_mk_bool(1))) {
        e1720 = u_append(e1720, f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t));
      }
      l1721 = u_field(l1721, 1);
    }
    u_value t1722;
    if (u_truth(f_list_46_contains_2(u_mk_bool(1), t1719))) {
      t1722 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t1722 = f_parse_46_differing_1(v_rest);
    }
    t1718 = t1722;
  }
  if (!t1718) {
    u_no_case(s1717);
  }
  return t1718;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s1723 = v_e;
  u_value t1724 = NULL;
  if (!t1724) {
    u_value v_t = u_field(u_field(s1723, 1), 3);
    t1724 = v_t;
  }
  if (!t1724) {
    u_no_case(s1723);
  }
  return t1724;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s1725 = v_fs;
  u_value t1726 = NULL;
  if ((!t1726) && u_is_con(s1725, &n_list_46_Nil)) {
    t1726 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1726) && u_is_con(s1725, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s1725, 0), 0);
    u_value v_d = u_field(u_field(s1725, 0), 1);
    u_value v_rest = u_field(s1725, 1);
    u_value t1727;
    if (u_truth(u_eq(v_g, v_f))) {
      t1727 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t1727 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t1726 = t1727;
  }
  if (!t1726) {
    u_no_case(s1725);
  }
  return t1726;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1728 = v_ks;
  u_value t1729 = NULL;
  if ((!t1729) && u_is_con(s1728, &n_list_46_Nil)) {
    t1729 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1729) && (u_is_con(s1728, &n_list_46_Cons) && u_is_con(u_field(u_field(s1728, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1728, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1728, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1728, 0), 1);
    u_value v_rest = u_field(s1728, 1);
    t1729 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1729) {
    u_no_case(s1728);
  }
  return t1729;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1730 = v_fs;
  u_value t1731 = NULL;
  if ((!t1731) && u_is_con(s1730, &n_list_46_Nil)) {
    t1731 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1731) && (u_is_con(s1730, &n_list_46_Cons) && u_is_con(u_field(u_field(s1730, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1730, 0), 0), 0);
    u_value v_t = u_field(u_field(s1730, 0), 1);
    u_value v_rest = u_field(s1730, 1);
    t1731 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1731) && (u_is_con(s1730, &n_list_46_Cons) && u_is_con(u_field(u_field(s1730, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1730, 1);
    t1731 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1731) {
    u_no_case(s1730);
  }
  return t1731;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1736 = f_parse_46_fields_of_1(v_ks);
  u_value t1735;
  {
    u_value v_fs = t1736;
    u_value t1732 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1733 = &t1732;
    u_value l1734 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1734, &n_list_46_Cons)) {
      u_value v_f = u_field(l1734, 0);
      if (u_truth(u_mk_bool(1))) {
        e1733 = u_append(e1733, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1734 = u_field(l1734, 1);
    }
    t1735 = t1732;
  }
  return t1735;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1737 = U_LIT("x");
  u_value t1738 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1739 = &t1738;
  u_value l1740 = v_ps;
  while (u_is_con(l1740, &n_list_46_Cons)) {
    u_value v_p = u_field(l1740, 0);
    if (u_truth(u_mk_bool(1))) {
      e1739 = u_append(e1739, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1740 = u_field(l1740, 1);
  }
  u_value t1748 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1737), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1749 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1738);
  u_value t1747;
  {
    u_value v_x = t1748;
    u_value v_owner = t1749;
    static struct u_object l__46__1741 = U_LIT(".");
    static struct u_object l_x_1742 = U_LIT("x");
    u_value t1743 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1744 = &t1743;
    u_value l1745 = v_es;
    while (u_is_con(l1745, &n_list_46_Cons)) {
      u_value v_e = u_field(l1745, 0);
      if (u_truth(u_mk_bool(1))) {
        e1744 = u_append(e1744, f_parse_46_field_case_1(v_e));
      }
      l1745 = u_field(l1745, 1);
    }
    u_value t1746;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1746 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1746 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1747 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1741, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1742), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1743, t1746)));
  }
  return t1747;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1750 = v_es;
  u_value t1751 = NULL;
  if ((!t1751) && u_is_con(s1750, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1750, 0), 1), 3);
    t1751 = v_t;
  }
  if (!t1751) {
    t1751 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1751) {
    u_no_case(s1750);
  }
  return t1751;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s1752 = v_e;
  u_value t1753 = NULL;
  if (!t1753) {
    u_value v_k = u_field(u_field(s1752, 1), 0);
    u_value v_i = u_field(u_field(s1752, 1), 1);
    u_value v_n = u_field(u_field(s1752, 1), 2);
    u_value t1754 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1755 = &t1754;
    u_value l1756 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1756, &n_list_46_Cons)) {
      u_value v_j = u_field(l1756, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1758;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1757 = U_LIT("y");
          t1758 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1757), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1758 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1755 = u_append(e1755, t1758);
      }
      l1756 = u_field(l1756, 1);
    }
    static struct u_object l_y_1759 = U_LIT("y");
    t1753 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1754), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1759), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1753) {
    u_no_case(s1752);
  }
  return t1753;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_MissingField_1760 = U_LIT("MissingField");
  static struct u_object l__46__1761 = U_LIT(".");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_ERaise, 2, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_MissingField_1760), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_t, u_concat(&l__46__1761, v_f)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_parse_46_updaters_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1766 = f_parse_46_fields_of_1(v_ks);
  u_value t1765;
  {
    u_value v_fs = t1766;
    u_value t1762 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1763 = &t1762;
    u_value l1764 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1764, &n_list_46_Cons)) {
      u_value v_f = u_field(l1764, 0);
      if (u_truth(u_mk_bool(1))) {
        e1763 = u_append(e1763, f_parse_46_updater_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1764 = u_field(l1764, 1);
    }
    t1765 = t1762;
  }
  return t1765;
}

u_value f_parse_46_updater_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1767 = U_LIT("x");
  u_value t1768 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1769 = &t1768;
  u_value l1770 = v_ps;
  while (u_is_con(l1770, &n_list_46_Cons)) {
    u_value v_p = u_field(l1770, 0);
    if (u_truth(u_mk_bool(1))) {
      e1769 = u_append(e1769, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1770 = u_field(l1770, 1);
  }
  u_value t1780 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1767), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1781 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1768);
  u_value t1779;
  {
    u_value v_x = t1780;
    u_value v_owner = t1781;
    static struct u_object l__46__1771 = U_LIT(".");
    static struct u_object l__58__61__1772 = U_LIT(":=");
    static struct u_object l_x_1773 = U_LIT("x");
    static struct u_object l_y_1774 = U_LIT("y");
    u_value t1775 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1776 = &t1775;
    u_value l1777 = v_es;
    while (u_is_con(l1777, &n_list_46_Cons)) {
      u_value v_e = u_field(l1777, 0);
      if (u_truth(u_mk_bool(1))) {
        e1776 = u_append(e1776, f_parse_46_update_case_1(v_e));
      }
      l1777 = u_field(l1777, 1);
    }
    u_value t1778;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1778 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1778 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1779 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1771, u_concat(v_f, &l__58__61__1772)))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1773), v_owner), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1774), f_parse_46_field_type_1(v_es)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), v_owner, u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1775, t1778)));
  }
  return t1779;
}

u_value f_parse_46_update_case_1(u_value v_e) {
  u_value s1782 = v_e;
  u_value t1783 = NULL;
  if (!t1783) {
    u_value v_k = u_field(u_field(s1782, 1), 0);
    u_value v_i = u_field(u_field(s1782, 1), 1);
    u_value v_n = u_field(u_field(s1782, 1), 2);
    u_value t1784 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1785 = &t1784;
    u_value l1786 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1786, &n_list_46_Cons)) {
      u_value v_j = u_field(l1786, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1788;
        if (u_truth(u_eq(v_j, v_i))) {
          t1788 = u_mk_con(&n_syntax_46_PWild, 0);
        } else {
          static struct u_object l_a_1787 = U_LIT("a");
          t1788 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l_a_1787, u_str_show(v_j))), u_mk_con(&n_syntax_46_THole, 0)));
        }
        e1785 = u_append(e1785, t1788);
      }
      l1786 = u_field(l1786, 1);
    }
    u_value t1789 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1790 = &t1789;
    u_value l1791 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1791, &n_list_46_Cons)) {
      u_value v_j = u_field(l1791, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1794;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1792 = U_LIT("y");
          t1794 = &l_y_1792;
        } else {
          static struct u_object l_a_1793 = U_LIT("a");
          t1794 = u_concat(&l_a_1793, u_str_show(v_j));
        }
        e1790 = u_append(e1790, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(t1794), u_mk_con(&n_syntax_46_THole, 0))));
      }
      l1791 = u_field(l1791, 1);
    }
    t1783 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1784), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_k, u_mk_con(&n_syntax_46_THole, 0))), t1789));
  }
  if (!t1783) {
    u_no_case(s1782);
  }
  return t1783;
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t1795;
  if (u_truth(u_eq(v_i, v_n))) {
    t1795 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1795 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t1795;
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
    u_value t1796 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1796;
    u_value t1798 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1798)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1797 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1797;
      }
      t1798 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1799 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1799), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1800 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1800), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1801 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1801;
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
    u_value t1802 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1803 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1804 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1805 = u_mk_bool(1);
    v_ins = t1802;
    v_outs = t1803;
    v_refs = t1804;
    v_first = t1805;
    static struct u_object l_in_1806 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1806))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_ins, r_ts);
        u_value t1807 = u_mk_bool(0);
        v_first = t1807;
      }
    }
    static struct u_object l_out_1808 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1808, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_outs, r_ts);
        u_value t1809 = u_mk_bool(0);
        v_first = t1809;
      }
    }
    static struct u_object l_ref_1810 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1810, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    static struct u_object l__58__61__1811 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1811), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1812 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1812;
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
    u_value t1813 = f_parse_46_Context_46_notations_1(v_cx);
    v_ns = t1813;
    u_value s1814 = f_parse_46_head_token_1(*r_ts);
    int d1815 = 0;
    if ((!d1815) && (u_is_con(s1814, &n_lex_46_TName) && u_equals(u_field(s1814, 0), u_mk_lit("import")))) {
      d1815 = 1;
      {
        u_value t1816 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1816;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Context_46_modules_1(v_cx))))) {
          {
            u_value t1817 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1817;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_cx, v_at, r_cs);
            u_value t1818 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1818;
          }
        }
      }
    }
    if ((!d1815) && u_is_con(s1814, &n_lex_46_TName)) {
      u_value v_w = u_field(s1814, 0);
      static struct u_object l_prefix_1820 = U_LIT("prefix");
      static struct u_object l_postfix_1821 = U_LIT("postfix");
      static struct u_object l_infix_1822 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1820, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1821, u_mk_con(&n_list_46_Cons, 2, &l_infix_1822, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1815 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1819 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1819;
        }
      }
    }
    if ((!d1815) && (u_is_con(s1814, &n_lex_46_TName) && u_equals(u_field(s1814, 0), u_mk_lit("type")))) {
      d1815 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1823 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1823), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1824 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1824;
      }
    }
    if ((!d1815) && (u_is_con(s1814, &n_lex_46_TName) && u_equals(u_field(s1814, 0), u_mk_lit("data")))) {
      d1815 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1815) && (u_is_con(s1814, &n_lex_46_TName) && u_equals(u_field(s1814, 0), u_mk_lit("function")))) {
      d1815 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1825 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1825;
      }
    }
    if ((!d1815) && (u_is_con(s1814, &n_lex_46_TName) && u_equals(u_field(s1814, 0), u_mk_lit("procedure")))) {
      d1815 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1826 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1826;
      }
    }
    if ((!d1815) && (u_is_con(s1814, &n_lex_46_TName) && u_equals(u_field(s1814, 0), u_mk_lit("eval")))) {
      d1815 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_e, r_ts);
        u_value t1827 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CEval, 1, v_e), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1827;
      }
    }
    if ((!d1815) && (u_is_con(s1814, &n_lex_46_TName) && u_equals(u_field(s1814, 0), u_mk_lit("run")))) {
      d1815 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1828 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1828), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1829 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1829;
      }
    }
    if ((!d1815) && (u_is_con(s1814, &n_lex_46_TName) && u_equals(u_field(s1814, 0), u_mk_lit("test")))) {
      d1815 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1830 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1830), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1831 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1831;
      }
    }
    if (!d1815) {
      d1815 = 1;
      {
        static struct u_object l_declaration_32_expe_1832 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1833 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1832, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1833)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1815) {
      u_no_case(s1814);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_cx) {
  {
    u_value s1834 = v_cs;
    int d1835 = 0;
    if ((!d1835) && u_is_con(s1834, &n_list_46_Nil)) {
      d1835 = 1;
      {
      }
    }
    if ((!d1835) && (u_is_con(s1834, &n_list_46_Cons) && u_is_con(u_field(s1834, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1834, 0), 0);
      u_value v_p = u_field(u_field(s1834, 0), 1);
      u_value v_ops = u_field(u_field(s1834, 0), 2);
      u_value v_more = u_field(s1834, 1);
      d1835 = 1;
      {
        u_value t1836 = f_parse_46_with_commands_2(*r_cx, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_cx = t1836;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1835) && (u_is_con(s1834, &n_list_46_Cons) && u_is_con(u_field(s1834, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1834, 0), 0);
      u_value v_ds = u_field(u_field(s1834, 0), 1);
      u_value v_more = u_field(s1834, 1);
      d1835 = 1;
      {
        u_value t1837 = f_parse_46_with_module_2(*r_cx, u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds));
        *r_cx = t1837;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1835) && (u_is_con(s1834, &n_list_46_Cons) && u_is_con(u_field(s1834, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1834, 0), 0);
      u_value v_more = u_field(s1834, 1);
      d1835 = 1;
      {
        u_value t1838 = f_parse_46_with_commands_2(*r_cx, f_parse_46_own_notations_2(*r_cx, v_n));
        *r_cx = t1838;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1835) && u_is_con(s1834, &n_list_46_Cons)) {
      u_value v_more = u_field(s1834, 1);
      d1835 = 1;
      {
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if (!d1835) {
      u_no_case(s1834);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_cx, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1839 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1839;
    u_value t1841 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1841)) {
      {
        p_parse_46_parse_command(*r_cx, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_cx);
        u_value t1840 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1840;
      }
      t1841 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1842 = f_list_46_reverse_1(v_rev);
    *r_cs = t1842;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1843 = v_dir;
    int d1844 = 0;
    if ((!d1844) && u_is_con(s1843, &n_option_46_Some)) {
      u_value v_d = u_field(s1843, 0);
      d1844 = 1;
      {
        {
          struct u_handler *h1847 = u_push();
          if (setjmp(h1847->env) == 0) {
            {
              static struct u_object l__47__1845 = U_LIT("/");
              static struct u_object l__46_u_1846 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1845, u_concat(v_n, &l__46_u_1846))), r_text);
            }
            u_pop(h1847);
          } else {
            u_pop(h1847);
            int d1848 = 0;
            if ((!d1848) && (u_has_type(h1847, "os.OsError") && u_is_con(u_caught(h1847), &n_os_46_OsError))) {
              d1848 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1848) {
              u_reraise(h1847);
            }
          }
        }
      }
    }
    if ((!d1844) && u_is_con(s1843, &n_option_46_None)) {
      d1844 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1844) {
      u_no_case(s1843);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1849 = u_push();
      if (setjmp(h1849->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1849);
      } else {
        u_pop(h1849);
        int d1850 = 0;
        if ((!d1850) && (u_has_type(h1849, "os.OsError") && u_is_con(u_caught(h1849), &n_os_46_OsError))) {
          d1850 = 1;
          {
            static struct u_object l_unknown_32_module_32__1851 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1851, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1850) {
          u_reraise(h1849);
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
        static struct u_object l_cyclic_32_import_32_of_1852 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1852, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Context_46_dir_1(v_cx), v_at, &v_text);
    u_value t1853 = f_parse_46_Context_46_loading_58__61__2(f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_base_notations_1(v_cx)), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Context_46_loading_1(v_cx)));
    v_mcx = t1853;
    {
      struct u_handler *h1854 = u_push();
      if (setjmp(h1854->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mcx, &v_mts);
        }
        u_pop(h1854);
      } else {
        u_pop(h1854);
        int d1855 = 0;
        if ((!d1855) && (u_has_type(h1854, "lex.LexError") && u_is_con(u_caught(h1854), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1854), 0);
          u_value v_i = u_field(u_caught(h1854), 1);
          d1855 = 1;
          {
            static struct u_object l__32_in_32__1856 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1856, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1855) && (u_has_type(h1854, "parse.ParseError") && u_is_con(u_caught(h1854), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1854), 0);
          u_value v_i = u_field(u_caught(h1854), 1);
          d1855 = 1;
          {
            static struct u_object l__32_in_32__1857 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1857, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1855) {
          u_reraise(h1854);
        }
      }
    }
  }
}

u_value f_parse_46_base_notations_1(u_value v_cx) {
  u_value s1858 = f_parse_46_Context_46_base_1(v_cx);
  u_value t1859 = NULL;
  if ((!t1859) && u_is_con(s1858, &n_option_46_Some)) {
    u_value v_b = u_field(s1858, 0);
    t1859 = v_b;
  }
  if ((!t1859) && u_is_con(s1858, &n_option_46_None)) {
    t1859 = f_parse_46_Context_46_notations_1(v_cx);
  }
  if (!t1859) {
    u_no_case(s1858);
  }
  return t1859;
}

void p_parse_46_parse_text(u_value v_text, u_value v_cx, u_value *r_p) {
  {
    u_value v_ts = NULL;
    u_value v_cs = NULL;
    u_value v_after = NULL;
    u_value t1860 = v_cx;
    v_after = t1860;
    {
      struct u_handler *h1861 = u_push();
      if (setjmp(h1861->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1861);
      } else {
        u_pop(h1861);
        int d1862 = 0;
        if ((!d1862) && (u_has_type(h1861, "lex.LexError") && u_is_con(u_caught(h1861), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1861), 0);
          u_value v_i = u_field(u_caught(h1861), 1);
          d1862 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1862) {
          u_reraise(h1861);
        }
      }
    }
    p_parse_46_parse_commands(&v_cs, &v_after, &v_ts);
    u_value t1863 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1863;
  }
}

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p) {
  {
    u_value v_cx = NULL;
    u_value t1864 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    v_cx = t1864;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), &v_cx);
    p_parse_46_parse_text(v_text, f_parse_46_Context_46_dir_58__61__2(f_parse_46_Context_46_base_58__61__2(f_parse_46_Context_46_loading_58__61__2(v_cx, u_mk_con(&n_list_46_Cons, 2, v_name, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_option_46_Some, 1, f_parse_46_Context_46_notations_1(v_cx))), v_dir), r_p);
  }
}

void p_parse_46_parse_library(u_value v_n, u_value *r_p) {
  {
    u_value v_text = NULL;
    u_value v_pre = NULL;
    p_parse_46_library_text(v_n, u_mk_int(0), &v_text);
    static struct u_object l_prelude_1865 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1865))) {
      {
        static struct u_object l_prelude_1866 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1866, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1867 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1867, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1868 = v_xs;
  u_value t1869 = NULL;
  if ((!t1869) && (u_is_con(s1868, &n_list_46_Cons) && u_is_con(u_field(s1868, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1868, 0);
    t1869 = v_x;
  }
  if ((!t1869) && u_is_con(s1868, &n_list_46_Cons)) {
    u_value v_more = u_field(s1868, 1);
    t1869 = f_parse_46_last_part_1(v_more);
  }
  if (!t1869) {
    static struct u_object l__1870 = U_LIT("");
    t1869 = &l__1870;
  }
  if (!t1869) {
    u_no_case(s1868);
  }
  return t1869;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1875 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1874;
  {
    u_value v_f = t1875;
    u_value t1872 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1872)) {
      static struct u_object l__46_u_1871 = U_LIT(".u");
      t1872 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1871);
    }
    u_value t1873;
    if (u_truth(t1872)) {
      t1873 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1873 = v_f;
    }
    t1874 = t1873;
  }
  return t1874;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1880 = u_str_split(v_path, u_mk_char(47));
  u_value t1879;
  {
    u_value v_parts = t1880;
    u_value t1878;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1876 = U_LIT(".");
      t1878 = &l__46__1876;
    } else {
      static struct u_object l__47__1877 = U_LIT("/");
      t1878 = u_str_join(&l__47__1877, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1879 = t1878;
  }
  return t1879;
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
    static struct u_object l_prelude_1881 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1881, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1882 = u_mk_tuple(2, v_cs, v_ds);
    int d1883 = 0;
    if ((!d1883) && (u_is_con(u_field(s1882, 0), &n_list_46_Cons) && u_is_con(u_field(s1882, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1882, 0), 0);
      u_value v_more = u_field(u_field(s1882, 0), 1);
      u_value v_d = u_field(u_field(s1882, 1), 0);
      u_value v_rest = u_field(u_field(s1882, 1), 1);
      d1883 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1883) {
      d1883 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1883) {
      u_no_case(s1882);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1884 = v_paths;
    int d1885 = 0;
    if ((!d1885) && u_is_con(s1884, &n_list_46_Nil)) {
      d1885 = 1;
      {
      }
    }
    if ((!d1885) && u_is_con(s1884, &n_list_46_Cons)) {
      u_value v_path = u_field(s1884, 0);
      u_value v_more = u_field(s1884, 1);
      d1885 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1885) {
      u_no_case(s1884);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1886 = v_paths;
    int d1887 = 0;
    if ((!d1887) && u_is_con(s1886, &n_list_46_Nil)) {
      d1887 = 1;
      {
      }
    }
    if ((!d1887) && u_is_con(s1886, &n_list_46_Cons)) {
      u_value v_path = u_field(s1886, 0);
      u_value v_more = u_field(s1886, 1);
      d1887 = 1;
      {
        u_value t1888 = u_mk_bool(0);
        v_failed = t1888;
        {
          struct u_handler *h1889 = u_push();
          if (setjmp(h1889->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1889);
          } else {
            u_pop(h1889);
            int d1890 = 0;
            if ((!d1890) && (u_has_type(h1889, "parse.ParseError") && u_is_con(u_caught(h1889), &n_parse_46_ParseError))) {
              d1890 = 1;
              {
                u_value t1891 = u_mk_bool(1);
                v_failed = t1891;
              }
            }
            if (!d1890) {
              u_reraise(h1889);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1887) {
      u_no_case(s1886);
    }
  }
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s1892 = v_x;
  u_value t1893 = NULL;
  if ((!t1893) && u_is_con(s1892, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1892, 0);
    t1893 = v_y;
  }
  if (!t1893) {
    u_no_case(s1892);
  }
  return t1893;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s1894 = v_x;
  u_value t1895 = NULL;
  if ((!t1895) && u_is_con(s1894, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1894, 1);
    t1895 = v_y;
  }
  if (!t1895) {
    u_no_case(s1894);
  }
  return t1895;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s1896 = v_x;
  u_value t1897 = NULL;
  if ((!t1897) && u_is_con(s1896, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1896, 2);
    t1897 = v_y;
  }
  if (!t1897) {
    u_no_case(s1896);
  }
  return t1897;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s1898 = v_x;
  u_value t1899 = NULL;
  if ((!t1899) && u_is_con(s1898, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1898, 3);
    t1899 = v_y;
  }
  if (!t1899) {
    u_no_case(s1898);
  }
  return t1899;
}

u_value f_compile_46_Globals_46_funs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1900 = v_x;
  u_value t1901 = NULL;
  if ((!t1901) && u_is_con(s1900, &n_compile_46_Globals)) {
    u_value v_a1 = u_field(s1900, 1);
    u_value v_a2 = u_field(s1900, 2);
    u_value v_a3 = u_field(s1900, 3);
    t1901 = u_mk_con(&n_compile_46_Globals, 4, v_y, v_a1, v_a2, v_a3);
  }
  if (!t1901) {
    u_no_case(s1900);
  }
  return t1901;
}

u_value f_compile_46_Globals_46_constrs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1902 = v_x;
  u_value t1903 = NULL;
  if ((!t1903) && u_is_con(s1902, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1902, 0);
    u_value v_a2 = u_field(s1902, 2);
    u_value v_a3 = u_field(s1902, 3);
    t1903 = u_mk_con(&n_compile_46_Globals, 4, v_a0, v_y, v_a2, v_a3);
  }
  if (!t1903) {
    u_no_case(s1902);
  }
  return t1903;
}

u_value f_compile_46_Globals_46_arities_58__61__2(u_value v_x, u_value v_y) {
  u_value s1904 = v_x;
  u_value t1905 = NULL;
  if ((!t1905) && u_is_con(s1904, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1904, 0);
    u_value v_a1 = u_field(s1904, 1);
    u_value v_a3 = u_field(s1904, 3);
    t1905 = u_mk_con(&n_compile_46_Globals, 4, v_a0, v_a1, v_y, v_a3);
  }
  if (!t1905) {
    u_no_case(s1904);
  }
  return t1905;
}

u_value f_compile_46_Globals_46_fields_58__61__2(u_value v_x, u_value v_y) {
  u_value s1906 = v_x;
  u_value t1907 = NULL;
  if ((!t1907) && u_is_con(s1906, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1906, 0);
    u_value v_a1 = u_field(s1906, 1);
    u_value v_a2 = u_field(s1906, 2);
    t1907 = u_mk_con(&n_compile_46_Globals, 4, v_a0, v_a1, v_a2, v_y);
  }
  if (!t1907) {
    u_no_case(s1906);
  }
  return t1907;
}

u_value f_compile_46_Gen_46_count_1(u_value v_x) {
  u_value s1908 = v_x;
  u_value t1909 = NULL;
  if ((!t1909) && u_is_con(s1908, &n_compile_46_Gen)) {
    u_value v_y = u_field(s1908, 0);
    t1909 = v_y;
  }
  if (!t1909) {
    u_no_case(s1908);
  }
  return t1909;
}

u_value f_compile_46_Gen_46_lifted_1(u_value v_x) {
  u_value s1910 = v_x;
  u_value t1911 = NULL;
  if ((!t1911) && u_is_con(s1910, &n_compile_46_Gen)) {
    u_value v_y = u_field(s1910, 1);
    t1911 = v_y;
  }
  if (!t1911) {
    u_no_case(s1910);
  }
  return t1911;
}

u_value f_compile_46_Gen_46_count_58__61__2(u_value v_x, u_value v_y) {
  u_value s1912 = v_x;
  u_value t1913 = NULL;
  if ((!t1913) && u_is_con(s1912, &n_compile_46_Gen)) {
    u_value v_a1 = u_field(s1912, 1);
    t1913 = u_mk_con(&n_compile_46_Gen, 2, v_y, v_a1);
  }
  if (!t1913) {
    u_no_case(s1912);
  }
  return t1913;
}

u_value f_compile_46_Gen_46_lifted_58__61__2(u_value v_x, u_value v_y) {
  u_value s1914 = v_x;
  u_value t1915 = NULL;
  if ((!t1915) && u_is_con(s1914, &n_compile_46_Gen)) {
    u_value v_a0 = u_field(s1914, 0);
    t1915 = u_mk_con(&n_compile_46_Gen, 2, v_a0, v_y);
  }
  if (!t1915) {
    u_no_case(s1914);
  }
  return t1915;
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
  static struct u_object l_u_value_1916 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1916);
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
  static struct u_object l_u_value_1917 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1917));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t1918 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1919 = &t1918;
  u_value l1920 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l1920, &n_list_46_Cons)) {
    u_value v_c = u_field(l1920, 0);
    if (u_truth(u_mk_bool(1))) {
      e1919 = u_append(e1919, f_compile_46_mangle_char_1(v_c));
    }
    l1920 = u_field(l1920, 1);
  }
  return u_string(f_list_46_concat_1(t1918));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t1921 = u_is_letter(v_c);
  if (!u_truth(t1921)) {
    t1921 = u_is_digit(v_c);
  }
  u_value t1922 = t1921;
  if (!u_truth(t1922)) {
    t1922 = u_eq(v_c, u_mk_char(95));
  }
  u_value t1925;
  if (u_truth(t1922)) {
    t1925 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___1923 = U_LIT("_");
    static struct u_object l___1924 = U_LIT("_");
    t1925 = u_chars(u_concat(&l___1923, u_concat(u_str_show(u_ord(v_c)), &l___1924)));
  }
  return t1925;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__1926 = U_LIT("f_");
  static struct u_object l___1927 = U_LIT("_");
  return u_concat(&l_f__1926, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___1927, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__1928 = U_LIT("p_");
  return u_concat(&l_p__1928, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__1929 = U_LIT("v_");
  return u_concat(&l_v__1929, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__1930 = U_LIT("r_");
  return u_concat(&l_r__1930, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__1931 = U_LIT("+");
  static struct u_object l_u_add_1932 = U_LIT("u_add");
  static struct u_object l__45__1933 = U_LIT("-");
  static struct u_object l_u_sub_1934 = U_LIT("u_sub");
  static struct u_object l__42__1935 = U_LIT("*");
  static struct u_object l_u_mul_1936 = U_LIT("u_mul");
  static struct u_object l__47__1937 = U_LIT("/");
  static struct u_object l_u_div_1938 = U_LIT("u_div");
  static struct u_object l__37__1939 = U_LIT("%");
  static struct u_object l_u_mod_1940 = U_LIT("u_mod");
  static struct u_object l__60__1941 = U_LIT("<");
  static struct u_object l_u_lt_1942 = U_LIT("u_lt");
  static struct u_object l__61__61__1943 = U_LIT("==");
  static struct u_object l_u_eq_1944 = U_LIT("u_eq");
  static struct u_object l_compare_1945 = U_LIT("compare");
  static struct u_object l_u_compare_1946 = U_LIT("u_compare");
  static struct u_object l_chars_1947 = U_LIT("chars");
  static struct u_object l_u_chars_1948 = U_LIT("u_chars");
  static struct u_object l_string_1949 = U_LIT("string");
  static struct u_object l_u_string_1950 = U_LIT("u_string");
  static struct u_object l_ord_1951 = U_LIT("ord");
  static struct u_object l_u_ord_1952 = U_LIT("u_ord");
  static struct u_object l_chr_1953 = U_LIT("chr");
  static struct u_object l_u_chr_1954 = U_LIT("u_chr");
  static struct u_object l__94__1955 = U_LIT("^");
  static struct u_object l_u_concat_1956 = U_LIT("u_concat");
  static struct u_object l_str_size_1957 = U_LIT("str_size");
  static struct u_object l_u_str_size_1958 = U_LIT("u_str_size");
  static struct u_object l_str_join_1959 = U_LIT("str_join");
  static struct u_object l_u_str_join_1960 = U_LIT("u_str_join");
  static struct u_object l_str_slice_1961 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_1962 = U_LIT("u_str_slice");
  static struct u_object l_str_split_1963 = U_LIT("str_split");
  static struct u_object l_u_str_split_1964 = U_LIT("u_str_split");
  static struct u_object l_str_get_1965 = U_LIT("str_get");
  static struct u_object l_u_str_get_1966 = U_LIT("u_str_get");
  static struct u_object l_str_contains_1967 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_1968 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_1969 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_1970 = U_LIT("u_str_compare");
  static struct u_object l_str_show_1971 = U_LIT("str_show");
  static struct u_object l_u_str_show_1972 = U_LIT("u_str_show");
  static struct u_object l_str_quote_1973 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_1974 = U_LIT("u_str_quote");
  static struct u_object l_str_number_1975 = U_LIT("str_number");
  static struct u_object l_u_str_number_1976 = U_LIT("u_str_number");
  static struct u_object l_is_digit_1977 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_1978 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_1979 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_1980 = U_LIT("u_is_letter");
  static struct u_object l_is_space_1981 = U_LIT("is_space");
  static struct u_object l_u_is_space_1982 = U_LIT("u_is_space");
  u_pop(handler);
  cache = f_map_46_put_all_2(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__1931), u_mk_tuple(2, &l_u_add_1932, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__1933), u_mk_tuple(2, &l_u_sub_1934, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__1935), u_mk_tuple(2, &l_u_mul_1936, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__1937), u_mk_tuple(2, &l_u_div_1938, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__1939), u_mk_tuple(2, &l_u_mod_1940, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__1941), u_mk_tuple(2, &l_u_lt_1942, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__1943), u_mk_tuple(2, &l_u_eq_1944, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_1945), u_mk_tuple(2, &l_u_compare_1946, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_1947), u_mk_tuple(2, &l_u_chars_1948, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_1949), u_mk_tuple(2, &l_u_string_1950, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_1951), u_mk_tuple(2, &l_u_ord_1952, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_1953), u_mk_tuple(2, &l_u_chr_1954, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__1955), u_mk_tuple(2, &l_u_concat_1956, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_1957), u_mk_tuple(2, &l_u_str_size_1958, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_1959), u_mk_tuple(2, &l_u_str_join_1960, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_1961), u_mk_tuple(2, &l_u_str_slice_1962, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_1963), u_mk_tuple(2, &l_u_str_split_1964, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_1965), u_mk_tuple(2, &l_u_str_get_1966, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_1967), u_mk_tuple(2, &l_u_str_contains_1968, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_1969), u_mk_tuple(2, &l_u_str_compare_1970, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_1971), u_mk_tuple(2, &l_u_str_show_1972, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_1973), u_mk_tuple(2, &l_u_str_quote_1974, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_1975), u_mk_tuple(2, &l_u_str_number_1976, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_1977), u_mk_tuple(2, &l_u_is_digit_1978, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_1979), u_mk_tuple(2, &l_u_is_letter_1980, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_1981), u_mk_tuple(2, &l_u_is_space_1982, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))), f_map_46_empty_0());
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__1983 = U_LIT("&");
  static struct u_object l_n__1984 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__1983, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__1984, f_compile_46_mangle_1(v_n))));
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
  cache = f_set_46_from_list_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_nil_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_cons_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_none_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_some_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_os_error_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_division_by_zero_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_no_index_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_no_character_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_missing_field_name_0(), u_mk_con(&n_list_46_Nil, 0)))))))))));
  return cache;
}

u_value f_compile_46_descriptor_decl_2(u_value v_n, u_value v_i) {
  u_value s1985 = v_n;
  u_value t1986 = NULL;
  if ((!t1986) && (u_is_con(s1985, &n_syntax_46_Name) && u_is_con(u_field(s1985, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s1985, 0), 0);
    u_value v_b = u_field(s1985, 1);
    static struct u_object l_const_32_struct_32_u_n_1987 = U_LIT("const struct u_name");
    static struct u_object l_n__1988 = U_LIT("n_");
    t1986 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1987), u_concat(&l_n__1988, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t1986) && (u_is_con(s1985, &n_syntax_46_Name) && u_is_con(u_field(s1985, 0), &n_option_46_None))) {
    u_value v_b = u_field(s1985, 1);
    static struct u_object l_const_32_struct_32_u_n_1989 = U_LIT("const struct u_name");
    static struct u_object l_n__1990 = U_LIT("n_");
    static struct u_object l_NULL_1991 = U_LIT("NULL");
    t1986 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1989), u_concat(&l_n__1990, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1991), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t1986) {
    u_no_case(s1985);
  }
  return t1986;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s1992 = v_cs;
  u_value t1993 = NULL;
  if ((!t1993) && u_is_con(s1992, &n_list_46_Nil)) {
    t1993 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1993) && (u_is_con(s1992, &n_list_46_Cons) && u_is_con(u_field(s1992, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s1992, 0), 2);
    u_value v_rest = u_field(s1992, 1);
    u_value t1994 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1995 = &t1994;
    u_value l1996 = v_ks;
    while (u_is_con(l1996, &n_list_46_Cons)) {
      u_value v_k = u_field(l1996, 0);
      if (u_truth(u_mk_bool(1))) {
        e1995 = u_append(e1995, f_syntax_46_Constr_46_name_1(v_k));
      }
      l1996 = u_field(l1996, 1);
    }
    t1993 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t1994, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t1993) && u_is_con(s1992, &n_list_46_Cons)) {
    u_value v_rest = u_field(s1992, 1);
    t1993 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t1993) {
    u_no_case(s1992);
  }
  return t1993;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s1997 = v_ks;
  u_value t1998 = NULL;
  if ((!t1998) && u_is_con(s1997, &n_list_46_Nil)) {
    t1998 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1998) && u_is_con(s1997, &n_list_46_Cons)) {
    u_value v_k = u_field(s1997, 0);
    u_value v_rest = u_field(s1997, 1);
    u_value t1999;
    if (u_truth(f_set_46_member_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t1999 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t1999 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t1998 = t1999;
  }
  if (!t1998) {
    u_no_case(s1997);
  }
  return t1998;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t2000 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2001 = &t2000;
  u_value l2002 = v_xs;
  while (u_is_con(l2002, &n_list_46_Cons)) {
    u_value v_x = u_field(l2002, 0);
    if (u_truth(u_mk_bool(1))) {
      e2001 = u_append(e2001, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l2002 = u_field(l2002, 1);
  }
  return t2000;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__2003 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__2003, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s2004 = f_list_46_lookup_2(v_x, v_env);
  u_value t2005 = NULL;
  if ((!t2005) && u_is_con(s2004, &n_option_46_Some)) {
    u_value v_c = u_field(s2004, 0);
    t2005 = v_c;
  }
  if (!t2005) {
    t2005 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t2005) {
    u_no_case(s2004);
  }
  return t2005;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s2006 = v_c;
  u_value t2007 = NULL;
  if ((!t2007) && (u_is_con(s2006, &n_c_46_CeUnary) && u_equals(u_field(s2006, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s2006, 1);
    t2007 = v_p;
  }
  if (!t2007) {
    static struct u_object l__38__2008 = U_LIT("&");
    t2007 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2008, v_c);
  }
  if (!t2007) {
    u_no_case(s2006);
  }
  return t2007;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s2009 = v_t;
  u_value t2010 = NULL;
  if ((!t2010) && (u_is_con(s2009, &n_syntax_46_TSort) && u_is_con(u_field(s2009, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s2009, 0);
    t2010 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t2010) && u_is_con(s2009, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s2009, 0);
    u_value v_args = u_field(s2009, 1);
    static struct u_object l__91__2011 = U_LIT("[");
    static struct u_object l__44__32__2012 = U_LIT(", ");
    u_value t2013 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2014 = &t2013;
    u_value l2015 = v_args;
    while (u_is_con(l2015, &n_list_46_Cons)) {
      u_value v_a = u_field(l2015, 0);
      if (u_truth(u_mk_bool(1))) {
        e2014 = u_append(e2014, f_compile_46_type_key_1(v_a));
      }
      l2015 = u_field(l2015, 1);
    }
    static struct u_object l__93__2016 = U_LIT("]");
    t2010 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__2011, u_concat(u_str_join(&l__44__32__2012, t2013), &l__93__2016)));
  }
  if ((!t2010) && u_is_con(s2009, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s2009, 0);
    u_value v_r = u_field(s2009, 1);
    static struct u_object l__40__2017 = U_LIT("(");
    static struct u_object l__44__32__2018 = U_LIT(", ");
    u_value t2019 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2020 = &t2019;
    u_value l2021 = v_args;
    while (u_is_con(l2021, &n_list_46_Cons)) {
      u_value v_a = u_field(l2021, 0);
      if (u_truth(u_mk_bool(1))) {
        e2020 = u_append(e2020, f_compile_46_type_key_1(v_a));
      }
      l2021 = u_field(l2021, 1);
    }
    static struct u_object l__41__32__45__62__32__2022 = U_LIT(") -> ");
    t2010 = u_concat(&l__40__2017, u_concat(u_str_join(&l__44__32__2018, t2019), u_concat(&l__41__32__45__62__32__2022, f_compile_46_type_key_1(v_r))));
  }
  if ((!t2010) && u_is_con(s2009, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s2009, 0);
    static struct u_object l__40__2023 = U_LIT("(");
    static struct u_object l__32__42__32__2024 = U_LIT(" * ");
    u_value t2025 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2026 = &t2025;
    u_value l2027 = v_ts;
    while (u_is_con(l2027, &n_list_46_Cons)) {
      u_value v_a = u_field(l2027, 0);
      if (u_truth(u_mk_bool(1))) {
        e2026 = u_append(e2026, f_compile_46_type_key_1(v_a));
      }
      l2027 = u_field(l2027, 1);
    }
    static struct u_object l__41__2028 = U_LIT(")");
    t2010 = u_concat(&l__40__2023, u_concat(u_str_join(&l__32__42__32__2024, t2025), &l__41__2028));
  }
  if (!t2010) {
    static struct u_object l__63__2029 = U_LIT("?");
    t2010 = &l__63__2029;
  }
  if (!t2010) {
    u_no_case(s2009);
  }
  return t2010;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t2030 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2031 = &t2030;
  u_value l2032 = v_cs;
  while (u_is_con(l2032, &n_list_46_Cons)) {
    u_value v_c = u_field(l2032, 0);
    if (u_truth(u_mk_bool(1))) {
      e2031 = u_append(e2031, f_compile_46_fun_arities_1(v_c));
    }
    l2032 = u_field(l2032, 1);
  }
  u_value t2033 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2034 = &t2033;
  u_value l2035 = v_cs;
  while (u_is_con(l2035, &n_list_46_Cons)) {
    u_value v_c = u_field(l2035, 0);
    if (u_truth(u_mk_bool(1))) {
      e2034 = u_append(e2034, f_compile_46_constr_arities_1(v_c));
    }
    l2035 = u_field(l2035, 1);
  }
  u_value t2037 = f_list_46_concat_1(t2030);
  u_value t2038 = f_list_46_concat_1(t2033);
  u_value t2036;
  {
    u_value v_fs = t2037;
    u_value v_ks = t2038;
    t2036 = u_mk_con(&n_compile_46_Globals, 4, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()));
  }
  return t2036;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s2039 = v_fs;
  u_value t2040 = NULL;
  if ((!t2040) && u_is_con(s2039, &n_list_46_Nil)) {
    t2040 = v_m;
  }
  if ((!t2040) && u_is_con(s2039, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s2039, 0), 0);
    u_value v_k = u_field(u_field(s2039, 0), 1);
    u_value v_rest = u_field(s2039, 1);
    u_value s2041 = f_map_46_get_2(v_f, v_m);
    u_value t2042 = NULL;
    if ((!t2042) && u_is_con(s2041, &n_option_46_Some)) {
      u_value v_ks = u_field(s2041, 0);
      t2042 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t2042) && u_is_con(s2041, &n_option_46_None)) {
      t2042 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t2042) {
      u_no_case(s2041);
    }
    t2040 = t2042;
  }
  if (!t2040) {
    u_no_case(s2039);
  }
  return t2040;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s2043 = v_gl;
  u_value t2044 = NULL;
  if ((!t2044) && u_is_con(s2043, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s2043, 2);
    u_value s2045 = f_map_46_get_2(v_n, v_fm);
    u_value t2046 = NULL;
    if ((!t2046) && u_is_con(s2045, &n_option_46_Some)) {
      u_value v_ks = u_field(s2045, 0);
      t2046 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t2046) && u_is_con(s2045, &n_option_46_None)) {
      t2046 = u_mk_bool(0);
    }
    if (!t2046) {
      u_no_case(s2045);
    }
    t2044 = t2046;
  }
  if (!t2044) {
    u_no_case(s2043);
  }
  return t2044;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s2047 = v_gl;
  u_value t2048 = NULL;
  if ((!t2048) && u_is_con(s2047, &n_compile_46_Globals)) {
    u_value v_km = u_field(s2047, 3);
    t2048 = f_map_46_get_2(v_n, v_km);
  }
  if (!t2048) {
    u_no_case(s2047);
  }
  return t2048;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s2049 = v_c;
  u_value t2050 = NULL;
  if ((!t2050) && u_is_con(s2049, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s2049, 0);
    u_value v_xss = u_field(s2049, 2);
    t2050 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2050) {
    t2050 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2050) {
    u_no_case(s2049);
  }
  return t2050;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s2051 = v_c;
  u_value t2052 = NULL;
  if ((!t2052) && u_is_con(s2051, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s2051, 2);
    u_value t2053 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2054 = &t2053;
    u_value l2055 = v_ks;
    while (u_is_con(l2055, &n_list_46_Cons)) {
      u_value v_k = u_field(l2055, 0);
      if (u_truth(u_mk_bool(1))) {
        e2054 = u_append(e2054, f_compile_46_constr_arity_1(v_k));
      }
      l2055 = u_field(l2055, 1);
    }
    t2052 = t2053;
  }
  if (!t2052) {
    t2052 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2052) {
    u_no_case(s2051);
  }
  return t2052;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s2056 = v_k;
  u_value t2057 = NULL;
  if ((!t2057) && u_is_con(s2056, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s2056, 0);
    u_value v_ts = u_field(s2056, 1);
    t2057 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t2057) {
    u_no_case(s2056);
  }
  return t2057;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s2058 = v_xs;
  u_value t2059 = NULL;
  if ((!t2059) && u_is_con(s2058, &n_list_46_Nil)) {
    t2059 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2059) && u_is_con(s2058, &n_list_46_Cons)) {
    u_value v_x = u_field(s2058, 0);
    u_value v_rest = u_field(s2058, 1);
    t2059 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t2059) {
    u_no_case(s2058);
  }
  return t2059;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_2060 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_2060, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_2061 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_2061, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s2062 = v_cs;
  u_value t2063 = NULL;
  if ((!t2063) && u_is_con(s2062, &n_list_46_Nil)) {
    t2063 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t2063) && (u_is_con(s2062, &n_list_46_Cons) && u_is_con(u_field(s2062, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s2062, 0);
    t2063 = v_d;
  }
  if ((!t2063) && u_is_con(s2062, &n_list_46_Cons)) {
    u_value v_d = u_field(s2062, 0);
    u_value v_rest = u_field(s2062, 1);
    static struct u_object l__38__38__2064 = U_LIT("&&");
    t2063 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__2064, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t2063) {
    u_no_case(s2062);
  }
  return t2063;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s2065 = v_l;
  u_value t2066 = NULL;
  if ((!t2066) && u_is_con(s2065, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_2067 = U_LIT("u_mk_tuple");
    t2066 = f_c_46_c_call_2(&l_u_mk_tuple_2067, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2066) && u_is_con(s2065, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s2065, 0);
    static struct u_object l_u_mk_bool_2068 = U_LIT("u_mk_bool");
    u_value t2069;
    if (u_truth(v_b)) {
      t2069 = u_mk_int(1);
    } else {
      t2069 = u_mk_int(0);
    }
    t2066 = f_c_46_c_call_2(&l_u_mk_bool_2068, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t2069), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2066) && u_is_con(s2065, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s2065, 0);
    u_value t2070 = u_lt(u_mk_int_str("-1000000000000000000"), v_n);
    if (u_truth(t2070)) {
      t2070 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t2073;
    if (u_truth(t2070)) {
      static struct u_object l_u_mk_int_2071 = U_LIT("u_mk_int");
      t2073 = f_c_46_c_call_2(&l_u_mk_int_2071, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_2072 = U_LIT("u_mk_int_str");
      t2073 = f_c_46_c_call_2(&l_u_mk_int_str_2072, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t2066 = t2073;
  }
  if ((!t2066) && u_is_con(s2065, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s2065, 0);
    static struct u_object l_u_mk_lit_2074 = U_LIT("u_mk_lit");
    t2066 = f_c_46_c_call_2(&l_u_mk_lit_2074, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2066) && u_is_con(s2065, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s2065, 0);
    static struct u_object l_u_mk_char_2075 = U_LIT("u_mk_char");
    t2066 = f_c_46_c_call_2(&l_u_mk_char_2075, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2066) {
    u_no_case(s2065);
  }
  return t2066;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s2076 = v_p;
  u_value t2077 = NULL;
  if ((!t2077) && u_is_con(s2076, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s2076, 0);
    static struct u_object l_u_equals_2078 = U_LIT("u_equals");
    t2077 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_2078, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2077) && u_is_con(s2076, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s2076, 0);
    u_value v_ps = u_field(s2076, 1);
    static struct u_object l_u_is_con_2079 = U_LIT("u_is_con");
    t2077 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_2079, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t2077) && u_is_con(s2076, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2076, 0);
    t2077 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2077) {
    t2077 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2077) {
    u_no_case(s2076);
  }
  return t2077;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s2080 = v_ps;
  u_value t2081 = NULL;
  if ((!t2081) && u_is_con(s2080, &n_list_46_Nil)) {
    t2081 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2081) && u_is_con(s2080, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2080, 0), 0);
    u_value v_i = u_field(u_field(s2080, 0), 1);
    u_value v_rest = u_field(s2080, 1);
    t2081 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t2081) {
    u_no_case(s2080);
  }
  return t2081;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s2082 = v_p;
  u_value t2083 = NULL;
  if ((!t2083) && (u_is_con(s2082, &n_syntax_46_PVar) && u_is_con(u_field(s2082, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2082, 0), 0);
    t2083 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2083) && u_is_con(s2082, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2082, 1);
    t2083 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t2083) && u_is_con(s2082, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2082, 0);
    t2083 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2083) {
    t2083 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2083) {
    u_no_case(s2082);
  }
  return t2083;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s2084 = v_ps;
  u_value t2085 = NULL;
  if ((!t2085) && u_is_con(s2084, &n_list_46_Nil)) {
    t2085 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2085) && u_is_con(s2084, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2084, 0), 0);
    u_value v_i = u_field(u_field(s2084, 0), 1);
    u_value v_rest = u_field(s2084, 1);
    t2085 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t2085) {
    u_no_case(s2084);
  }
  return t2085;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s2086 = v_p;
  u_value t2087 = NULL;
  if ((!t2087) && (u_is_con(s2086, &n_syntax_46_PVar) && u_is_con(u_field(s2086, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2086, 0), 0);
    t2087 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2087) && u_is_con(s2086, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2086, 1);
    u_value t2088 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2089 = &t2088;
    u_value l2090 = v_ps;
    while (u_is_con(l2090, &n_list_46_Cons)) {
      u_value v_q = u_field(l2090, 0);
      if (u_truth(u_mk_bool(1))) {
        e2089 = u_append(e2089, f_compile_46_pattern_names_1(v_q));
      }
      l2090 = u_field(l2090, 1);
    }
    t2087 = f_list_46_concat_1(t2088);
  }
  if ((!t2087) && u_is_con(s2086, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2086, 0);
    u_value t2091 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2092 = &t2091;
    u_value l2093 = v_ps;
    while (u_is_con(l2093, &n_list_46_Cons)) {
      u_value v_q = u_field(l2093, 0);
      if (u_truth(u_mk_bool(1))) {
        e2092 = u_append(e2092, f_compile_46_pattern_names_1(v_q));
      }
      l2093 = u_field(l2093, 1);
    }
    t2087 = f_list_46_concat_1(t2091);
  }
  if (!t2087) {
    t2087 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2087) {
    u_no_case(s2086);
  }
  return t2087;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s2094 = v_e;
  u_value t2095 = NULL;
  if ((!t2095) && u_is_con(s2094, &n_syntax_46_ELit)) {
    t2095 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2095) && (u_is_con(s2094, &n_syntax_46_EVar) && u_is_con(u_field(s2094, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2094, 0), 0);
    u_value t2096;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t2096 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t2096 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t2095 = t2096;
  }
  if ((!t2095) && u_is_con(s2094, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s2094, 0);
    u_value v_args = u_field(s2094, 1);
    u_value t2097 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2098 = &t2097;
    u_value l2099 = v_args;
    while (u_is_con(l2099, &n_list_46_Cons)) {
      u_value v_a = u_field(l2099, 0);
      if (u_truth(u_mk_bool(1))) {
        e2098 = u_append(e2098, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2099 = u_field(l2099, 1);
    }
    t2095 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t2097));
  }
  if ((!t2095) && u_is_con(s2094, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s2094, 0);
    u_value v_b = u_field(s2094, 1);
    t2095 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t2095) && u_is_con(s2094, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s2094, 0);
    u_value v_b = u_field(s2094, 1);
    u_value t2100 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2101 = &t2100;
    u_value l2102 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l2102, &n_list_46_Cons)) {
      u_value v_a = u_field(l2102, 0);
      if (u_truth(u_mk_bool(1))) {
        e2101 = u_append(e2101, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2102 = u_field(l2102, 1);
    }
    t2095 = f_list_46__43__43__2(f_list_46_concat_1(t2100), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t2095) && u_is_con(s2094, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s2094, 0);
    u_value v_l = u_field(s2094, 1);
    u_value v_r = u_field(s2094, 2);
    t2095 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t2095) && u_is_con(s2094, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s2094, 0);
    u_value v_cs = u_field(s2094, 1);
    u_value t2103 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2104 = &t2103;
    u_value l2105 = v_cs;
    while (u_is_con(l2105, &n_list_46_Cons)) {
      u_value v_b = u_field(l2105, 0);
      if (u_truth(u_mk_bool(1))) {
        e2104 = u_append(e2104, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l2105 = u_field(l2105, 1);
    }
    t2095 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2103));
  }
  if ((!t2095) && u_is_con(s2094, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s2094, 0);
    u_value v_xs = u_field(s2094, 1);
    u_value v_ls = u_field(s2094, 2);
    u_value v_c = u_field(s2094, 3);
    u_value t2106 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2107 = &t2106;
    u_value l2108 = v_ls;
    while (u_is_con(l2108, &n_list_46_Cons)) {
      u_value v_l = u_field(l2108, 0);
      if (u_truth(u_mk_bool(1))) {
        e2107 = u_append(e2107, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2108 = u_field(l2108, 1);
    }
    t2095 = f_list_46__43__43__2(f_list_46_concat_1(t2106), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t2095) && u_is_con(s2094, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s2094, 0);
    u_value t2109 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2110 = &t2109;
    u_value l2111 = v_es;
    while (u_is_con(l2111, &n_list_46_Cons)) {
      u_value v_a = u_field(l2111, 0);
      if (u_truth(u_mk_bool(1))) {
        e2110 = u_append(e2110, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2111 = u_field(l2111, 1);
    }
    t2095 = f_list_46_concat_1(t2109);
  }
  if ((!t2095) && u_is_con(s2094, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s2094, 0);
    t2095 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if (!t2095) {
    u_no_case(s2094);
  }
  return t2095;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s2112 = v_b;
  u_value t2113 = NULL;
  if ((!t2113) && (u_is_con(s2112, &n_syntax_46_Branch) && u_is_con(u_field(s2112, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2112, 0);
    u_value v_g = u_field(u_field(s2112, 1), 0);
    u_value v_e = u_field(s2112, 2);
    t2113 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2113) && (u_is_con(s2112, &n_syntax_46_Branch) && u_is_con(u_field(s2112, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2112, 0);
    u_value v_e = u_field(s2112, 2);
    t2113 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2113) {
    u_no_case(s2112);
  }
  return t2113;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s2114 = v_xss;
  u_value t2115 = NULL;
  if ((!t2115) && u_is_con(s2114, &n_list_46_Nil)) {
    t2115 = v_b;
  }
  if ((!t2115) && u_is_con(s2114, &n_list_46_Cons)) {
    u_value v_xs = u_field(s2114, 0);
    u_value v_rest = u_field(s2114, 1);
    t2115 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t2115) {
    u_no_case(s2114);
  }
  return t2115;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value t2116 = u_concat(v_stem, u_str_show(f_compile_46_Gen_46_count_1(*r_g)));
    u_value t2117 = f_compile_46_Gen_46_count_58__61__2(*r_g, u_add(f_compile_46_Gen_46_count_1(*r_g), u_mk_int(1)));
    *r_x = t2116;
    *r_g = t2117;
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value t2118 = f_compile_46_Gen_46_lifted_58__61__2(*r_g, f_list_46__43__43__2(f_compile_46_Gen_46_lifted_1(*r_g), u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
    *r_g = t2118;
  }
}

void p_compile_46_global_value(u_value v_gl, u_value v_n, u_value v_t, u_value *r_r) {
  {
    if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
      {
        static struct u_object l_u_mk_closure_2119 = U_LIT("u_mk_closure");
        static struct u_object l_w__2120 = U_LIT("w_");
        u_value t2121 = f_c_46_c_call_2(&l_u_mk_closure_2119, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__2120, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        *r_r = t2121;
      }
    } else {
      {
        u_value s2122 = f_compile_46_fields_count_2(v_gl, v_n);
        int d2123 = 0;
        if ((!d2123) && (u_is_con(s2122, &n_option_46_Some) && u_equals(u_field(s2122, 0), u_mk_int(0)))) {
          d2123 = 1;
          {
            static struct u_object l_u_mk_con_2124 = U_LIT("u_mk_con");
            u_value t2125 = f_c_46_c_call_2(&l_u_mk_con_2124, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t2125;
          }
        }
        if ((!d2123) && u_is_con(s2122, &n_option_46_Some)) {
          d2123 = 1;
          {
            static struct u_object l_u_mk_closure_2126 = U_LIT("u_mk_closure");
            static struct u_object l_wc__2127 = U_LIT("wc_");
            u_value t2128 = f_c_46_c_call_2(&l_u_mk_closure_2126, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__2127, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t2128;
          }
        }
        if ((!d2123) && u_is_con(s2122, &n_option_46_None)) {
          d2123 = 1;
          {
            u_value s2129 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
            int d2130 = 0;
            if ((!d2130) && u_is_con(s2129, &n_option_46_Some)) {
              u_value v_f = u_field(u_field(s2129, 0), 0);
              d2130 = 1;
              {
                static struct u_object l_u_mk_closure_2131 = U_LIT("u_mk_closure");
                static struct u_object l_w_2132 = U_LIT("w");
                u_value t2133 = f_c_46_c_call_2(&l_u_mk_closure_2131, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_2132, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
                *r_r = t2133;
              }
            }
            if ((!d2130) && u_is_con(s2129, &n_option_46_None)) {
              d2130 = 1;
              {
                static struct u_object l_the_32_variable_32__2134 = U_LIT("the variable ");
                u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__2134, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
              }
            }
            if (!d2130) {
              u_no_case(s2129);
            }
          }
        }
        if (!d2123) {
          u_no_case(s2122);
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
    u_value s2135 = v_e;
    int d2136 = 0;
    if ((!d2136) && (u_is_con(s2135, &n_syntax_46_ELit) && u_is_con(u_field(s2135, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s2135, 0), 0);
      d2136 = 1;
      {
        static struct u_object l_l__2137 = U_LIT("l_");
        u_value t2138;
        if (u_truth(u_lt(u_str_size(v_s), u_mk_int(16)))) {
          t2138 = u_str_size(v_s);
        } else {
          t2138 = u_mk_int(16);
        }
        static struct u_object l___2139 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__2137, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), t2138))), &l___2139)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__2140 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_2141 = U_LIT("U_LIT");
        static struct u_object l__38__2142 = U_LIT("&");
        u_value t2143 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__2140), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_2141, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t2144 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2142, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t2143;
        *r_r = t2144;
      }
    }
    if ((!d2136) && u_is_con(s2135, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s2135, 0);
      d2136 = 1;
      {
        u_value t2145 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2146 = f_compile_46_literal_value_1(v_l);
        *r_ss = t2145;
        *r_r = t2146;
      }
    }
    if ((!d2136) && (u_is_con(s2135, &n_syntax_46_EVar) && u_is_con(u_field(s2135, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s2135, 0), 0);
      u_value v_ty = u_field(u_field(s2135, 0), 1);
      d2136 = 1;
      {
        u_value s2147 = f_list_46_lookup_2(v_n, v_env);
        int d2148 = 0;
        if ((!d2148) && u_is_con(s2147, &n_option_46_Some)) {
          u_value v_c = u_field(s2147, 0);
          d2148 = 1;
          {
            u_value t2149 = u_mk_con(&n_list_46_Nil, 0);
            u_value t2150 = v_c;
            *r_ss = t2149;
            *r_r = t2150;
          }
        }
        if ((!d2148) && u_is_con(s2147, &n_option_46_None)) {
          d2148 = 1;
          {
            p_compile_46_global_value(v_gl, v_n, v_ty, r_r);
            u_value t2151 = u_mk_con(&n_list_46_Nil, 0);
            *r_ss = t2151;
          }
        }
        if (!d2148) {
          u_no_case(s2147);
        }
      }
    }
    if ((!d2136) && (u_is_con(s2135, &n_syntax_46_EApp) && (u_is_con(u_field(s2135, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s2135, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s2135, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s2135, 0), 0), 1);
      u_value v_args = u_field(s2135, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d2136 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d2136) && u_is_con(s2135, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s2135, 0);
      u_value v_args = u_field(s2135, 1);
      d2136 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_2152 = U_LIT("u_apply");
        u_value t2153 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2154 = f_c_46_c_call_2(&l_u_apply_2152, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t2153;
        *r_r = t2154;
      }
    }
    if ((!d2136) && u_is_con(s2135, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s2135, 0);
      u_value v_b = u_field(s2135, 1);
      d2136 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2136) && u_is_con(s2135, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s2135, 0);
      u_value v_b = u_field(s2135, 1);
      d2136 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2155 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2155, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2156 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t2156;
        u_value t2157 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2157;
      }
    }
    if ((!d2136) && u_is_con(s2135, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s2135, 0);
      u_value v_l = u_field(s2135, 1);
      u_value v_rr = u_field(s2135, 2);
      d2136 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2158 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2158, &v_t, r_g);
        u_value t2159 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2159;
        u_value t2160 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2160;
      }
    }
    if ((!d2136) && u_is_con(s2135, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s2135, 0);
      u_value v_cases = u_field(s2135, 1);
      d2136 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2161 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2161, &v_d, r_g);
        static struct u_object l_t_2162 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2162, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_2163 = U_LIT("NULL");
        u_value t2164 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2163))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2164;
        static struct u_object l__33__2165 = U_LIT("!");
        static struct u_object l_u_no_case_2166 = U_LIT("u_no_case");
        u_value t2167 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2165, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2166, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2167;
        u_value t2168 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2168;
      }
    }
    if ((!d2136) && u_is_con(s2135, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s2135, 0);
      u_value v_xs = u_field(s2135, 1);
      u_value v_lists = u_field(s2135, 2);
      u_value v_c = u_field(s2135, 3);
      d2136 = 1;
      {
        static struct u_object l_t_2169 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2169, &v_t, r_g);
        static struct u_object l_e_2170 = U_LIT("e");
        p_compile_46_gen_name(&l_e_2170, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_2171 = U_LIT("u_mk_con");
        static struct u_object l__38__2172 = U_LIT("&");
        u_value t2173 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_2171, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__2172, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t2173;
        u_value t2174 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2174;
      }
    }
    if ((!d2136) && u_is_con(s2135, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s2135, 0);
      d2136 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_2175 = U_LIT("u_mk_tuple");
        u_value t2176 = f_c_46_c_call_2(&l_u_mk_tuple_2175, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t2176;
      }
    }
    if ((!d2136) && u_is_con(s2135, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s2135, 0);
      u_value v_ty = u_field(s2135, 1);
      d2136 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2177 = U_LIT("u_raise");
        static struct u_object l_NULL_2178 = U_LIT("NULL");
        u_value t2179 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2177, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        u_value t2180 = u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2178);
        *r_ss = t2179;
        *r_r = t2180;
      }
    }
    if (!d2136) {
      u_no_case(s2135);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s2181 = v_es;
    int d2182 = 0;
    if ((!d2182) && u_is_con(s2181, &n_list_46_Nil)) {
      d2182 = 1;
      {
        u_value t2183 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2184 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2183;
        *r_rs = t2184;
      }
    }
    if ((!d2182) && u_is_con(s2181, &n_list_46_Cons)) {
      u_value v_e = u_field(s2181, 0);
      u_value v_rest = u_field(s2181, 1);
      d2182 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t2185 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2186 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t2185;
        *r_rs = t2186;
      }
    }
    if (!d2182) {
      u_no_case(s2181);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s2187 = v_rs;
    int d2188 = 0;
    if ((!d2188) && u_is_con(s2187, &n_list_46_Nil)) {
      d2188 = 1;
      {
        u_value t2189 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2189;
      }
    }
    if ((!d2188) && u_is_con(s2187, &n_list_46_Cons)) {
      u_value v_r = u_field(s2187, 0);
      u_value v_rest = u_field(s2187, 1);
      d2188 = 1;
      {
        static struct u_object l_t_2190 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2190, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t2191 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t2191;
      }
    }
    if (!d2188) {
      u_no_case(s2187);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s2192 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2193 = NULL;
  if ((!t2193) && (u_is_con(u_field(s2192, 0), &n_list_46_Cons) && (u_is_con(u_field(s2192, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2192, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2192, 0), 0);
    u_value v_more = u_field(u_field(s2192, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2192, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2192, 1), 1);
    t2193 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t2193) {
    t2193 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2193) {
    u_no_case(s2192);
  }
  return t2193;
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
    u_value s2194 = v_args;
    int d2195 = 0;
    if ((!d2195) && (u_is_con(s2194, &n_list_46_Cons) && (u_is_con(u_field(s2194, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2194, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s2194, 0);
      u_value v_b = u_field(u_field(s2194, 1), 0);
      static struct u_object l_and_2202 = U_LIT("and");
      u_value t2204 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2202));
      if (!u_truth(t2204)) {
        static struct u_object l_or_2203 = U_LIT("or");
        t2204 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_2203));
      }
      if (u_truth(t2204)) {
        d2195 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_2196 = U_LIT("t");
          p_compile_46_gen_name(&l_t_2196, &v_t, r_g);
          static struct u_object l_and_2197 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2197)))) {
            {
              u_value t2198 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2198;
            }
          } else {
            {
              static struct u_object l__33__2199 = U_LIT("!");
              u_value t2200 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2199, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2200;
            }
          }
          u_value t2201 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t2201;
        }
      }
    }
    if (!d2195) {
      d2195 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t2205 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t2205)) {
          t2205 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t2205)) {
          {
            u_value t2206 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t2206;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_2207 = U_LIT("u_mk_con");
              u_value t2208 = f_c_46_c_call_2(&l_u_mk_con_2207, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t2208;
            }
          } else {
            {
              u_value s2209 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
              int d2210 = 0;
              if ((!d2210) && u_is_con(s2209, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s2209, 0), 0);
                u_value v_k = u_field(u_field(s2209, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d2210 = 1;
                  {
                    u_value t2211 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t2211;
                  }
                }
              }
              if (!d2210) {
                d2210 = 1;
                {
                  p_compile_46_global_value(v_gl, v_n, v_ty, &v_f);
                  static struct u_object l_u_apply_2212 = U_LIT("u_apply");
                  u_value t2213 = f_c_46_c_call_2(&l_u_apply_2212, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t2213;
                }
              }
              if (!d2210) {
                u_no_case(s2209);
              }
            }
          }
        }
      }
    }
    if (!d2195) {
      u_no_case(s2194);
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
    u_value t2214 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2215 = &t2214;
    u_value l2216 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l2216, &n_list_46_Cons)) {
      u_value v_x = u_field(l2216, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2215 = u_append(e2215, v_x);
      }
      l2216 = u_field(l2216, 1);
    }
    u_value t2217 = t2214;
    v_caps = t2217;
    static struct u_object l_lam_2218 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_2218, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_2219 = U_LIT("env");
    static struct u_object l_args_2220 = U_LIT("args");
    u_value t2221 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2219), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_2220));
    v_decls = t2221;
    static struct u_object l_env_2222 = U_LIT("env");
    static struct u_object l_args_2223 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2222), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2223), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_2224 = U_LIT("u_mk_closure");
    u_value t2225 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2226 = &t2225;
    u_value l2227 = v_caps;
    while (u_is_con(l2227, &n_list_46_Cons)) {
      u_value v_x = u_field(l2227, 0);
      if (u_truth(u_mk_bool(1))) {
        e2226 = u_append(e2226, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2227 = u_field(l2227, 1);
    }
    u_value t2228 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2229 = f_c_46_c_call_2(&l_u_mk_closure_2224, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2225));
    *r_ss = t2228;
    *r_r = t2229;
  }
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s2230 = v_xs;
  u_value t2231 = NULL;
  if ((!t2231) && u_is_con(s2230, &n_list_46_Nil)) {
    t2231 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2231) && u_is_con(s2230, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2230, 0), 0);
    u_value v_i = u_field(u_field(s2230, 0), 1);
    u_value v_rest = u_field(s2230, 1);
    t2231 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t2231) {
    u_no_case(s2230);
  }
  return t2231;
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
    u_value s2232 = v_cases;
    int d2233 = 0;
    if ((!d2233) && u_is_con(s2232, &n_list_46_Nil)) {
      d2233 = 1;
      {
        u_value t2234 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2234;
      }
    }
    if ((!d2233) && (u_is_con(s2232, &n_list_46_Cons) && u_is_con(u_field(s2232, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2232, 0), 0);
      u_value v_guard = u_field(u_field(s2232, 0), 1);
      u_value v_e = u_field(u_field(s2232, 0), 2);
      u_value v_more = u_field(s2232, 1);
      d2233 = 1;
      {
        u_value t2235 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2235;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2236 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t2236;
        u_value s2237 = v_guard;
        int d2238 = 0;
        if ((!d2238) && u_is_con(s2237, &n_option_46_Some)) {
          u_value v_c = u_field(s2237, 0);
          d2238 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2239 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2239;
          }
        }
        if ((!d2238) && u_is_con(s2237, &n_option_46_None)) {
          d2238 = 1;
          {
          }
        }
        if (!d2238) {
          u_no_case(s2237);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2240 = U_LIT("!");
        u_value t2241 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2240, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2241;
      }
    }
    if (!d2233) {
      u_no_case(s2232);
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
    u_value s2242 = u_mk_tuple(2, v_xs, v_lists);
    int d2243 = 0;
    if ((!d2243) && (u_is_con(u_field(s2242, 0), &n_list_46_Cons) && u_is_con(u_field(s2242, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2242, 0), 0);
      u_value v_more = u_field(u_field(s2242, 0), 1);
      u_value v_list = u_field(u_field(s2242, 1), 0);
      u_value v_others = u_field(u_field(s2242, 1), 1);
      d2243 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2244 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2244, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2245 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2245;
        static struct u_object l_u_is_con_2246 = U_LIT("u_is_con");
        u_value t2247 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2246, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2247;
      }
    }
    if (!d2243) {
      d2243 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2248 = U_LIT("u_append");
        u_value t2249 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2248, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2249;
      }
    }
    if (!d2243) {
      u_no_case(s2242);
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
    u_value s2250 = v_s;
    int d2251 = 0;
    if ((!d2251) && u_is_con(s2250, &n_syntax_46_SSkip)) {
      d2251 = 1;
      {
        u_value t2252 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2252;
      }
    }
    if ((!d2251) && u_is_con(s2250, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2250, 0);
      d2251 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2253 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2253;
      }
    }
    if ((!d2251) && u_is_con(s2250, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2250, 0);
      u_value v_body = u_field(s2250, 1);
      d2251 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t2254 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2255 = &t2254;
        u_value l2256 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2256, &n_list_46_Cons)) {
          u_value v_x = u_field(l2256, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2257 = U_LIT("NULL");
            e2255 = u_append(e2255, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2257))));
          }
          l2256 = u_field(l2256, 1);
        }
        u_value t2258 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2254, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2258;
      }
    }
    if ((!d2251) && u_is_con(s2250, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2250, 0);
      u_value v_l = u_field(s2250, 1);
      u_value v_r = u_field(s2250, 2);
      d2251 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2259 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2259;
      }
    }
    if ((!d2251) && u_is_con(s2250, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2250, 0);
      u_value v_b = u_field(s2250, 1);
      d2251 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2260 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2260, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2261 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2261;
      }
    }
    if ((!d2251) && u_is_con(s2250, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2250, 0);
      u_value v_ins = u_field(s2250, 1);
      u_value v_outs = u_field(s2250, 2);
      u_value v_refs = u_field(s2250, 3);
      d2251 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        u_value t2262 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2263 = &t2262;
        u_value l2264 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2264, &n_list_46_Cons)) {
          u_value v_x = u_field(l2264, 0);
          if (u_truth(u_mk_bool(1))) {
            e2263 = u_append(e2263, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
          }
          l2264 = u_field(l2264, 1);
        }
        u_value t2265 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2262))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2265;
      }
    }
    if ((!d2251) && u_is_con(s2250, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2250, 0);
      u_value v_cases = u_field(s2250, 1);
      d2251 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2266 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2266, &v_d, r_g);
        static struct u_object l_d_2267 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2267, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2268 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2268;
        static struct u_object l__33__2269 = U_LIT("!");
        static struct u_object l_u_no_case_2270 = U_LIT("u_no_case");
        u_value t2271 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2269, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2270, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2271;
      }
    }
    if ((!d2251) && (u_is_con(s2250, &n_syntax_46_SAssert) && (u_is_con(u_field(s2250, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2250, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2250, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2250, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2250, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2250, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2250, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2250, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2250, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2250, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2250, 0), 1), 1), 0);
      d2251 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2272 = U_LIT("u_assert_equal");
        u_value t2273 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2272, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2273;
      }
    }
    if ((!d2251) && u_is_con(s2250, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2250, 0);
      d2251 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2274 = U_LIT("u_assert_true");
        u_value t2275 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2274, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2275;
      }
    }
    if ((!d2251) && u_is_con(s2250, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2250, 0);
      u_value v_ty = u_field(s2250, 1);
      d2251 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2276 = U_LIT("u_raise");
        u_value t2277 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2276, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2277;
      }
    }
    if ((!d2251) && u_is_con(s2250, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2250, 0);
      u_value v_hs = u_field(s2250, 1);
      d2251 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2278 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2278, &v_d, r_g);
        static struct u_object l_d_2279 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2279, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2280 = U_LIT("u_pop");
        static struct u_object l__33__2281 = U_LIT("!");
        static struct u_object l_u_reraise_2282 = U_LIT("u_reraise");
        u_value t2283 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2280, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2281, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2282, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2283;
        static struct u_object l_struct_32_u_handler_2284 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2285 = U_LIT("u_push");
        static struct u_object l__61__61__2286 = U_LIT("==");
        static struct u_object l_setjmp_2287 = U_LIT("setjmp");
        static struct u_object l_env_2288 = U_LIT("env");
        static struct u_object l_u_pop_2289 = U_LIT("u_pop");
        u_value t2290 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2284)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2285, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2286, f_c_46_c_call_2(&l_setjmp_2287, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2288), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2289, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2290;
      }
    }
    if (!d2251) {
      u_no_case(s2250);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2291 = v_xs;
    int d2292 = 0;
    if ((!d2292) && u_is_con(s2291, &n_list_46_Nil)) {
      d2292 = 1;
      {
        u_value t2293 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2293;
      }
    }
    if ((!d2292) && u_is_con(s2291, &n_list_46_Cons)) {
      u_value v_x = u_field(s2291, 0);
      u_value v_rest = u_field(s2291, 1);
      d2292 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2294 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2294;
      }
    }
    if (!d2292) {
      u_no_case(s2291);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2295 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2296 = NULL;
  if ((!t2296) && (u_is_con(u_field(s2295, 0), &n_list_46_Cons) && (u_is_con(u_field(s2295, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2295, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2295, 0), 0);
    u_value v_more = u_field(u_field(s2295, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2295, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2295, 1), 1);
    t2296 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2296) {
    t2296 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2296) {
    u_no_case(s2295);
  }
  return t2296;
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
    static struct u_object l_u_caught_2297 = U_LIT("u_caught");
    u_value t2298 = f_c_46_c_call_2(&l_u_caught_2297, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2298;
    u_value s2299 = v_hs;
    int d2300 = 0;
    if ((!d2300) && u_is_con(s2299, &n_list_46_Nil)) {
      d2300 = 1;
      {
        u_value t2301 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2301;
      }
    }
    if ((!d2300) && (u_is_con(s2299, &n_list_46_Cons) && u_is_con(u_field(s2299, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2299, 0), 0);
      u_value v_p = u_field(u_field(s2299, 0), 1);
      u_value v_guard = u_field(u_field(s2299, 0), 2);
      u_value v_s = u_field(u_field(s2299, 0), 3);
      u_value v_more = u_field(s2299, 1);
      d2300 = 1;
      {
        u_value t2302 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2302;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2303 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2303;
        u_value s2304 = v_guard;
        int d2305 = 0;
        if ((!d2305) && u_is_con(s2304, &n_option_46_Some)) {
          u_value v_c = u_field(s2304, 0);
          d2305 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2306 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2306;
          }
        }
        if ((!d2305) && u_is_con(s2304, &n_option_46_None)) {
          d2305 = 1;
          {
          }
        }
        if (!d2305) {
          u_no_case(s2304);
        }
        u_value s2307 = v_p;
        int d2308 = 0;
        if ((!d2308) && u_is_con(s2307, &n_syntax_46_PWild)) {
          d2308 = 1;
          {
            u_value t2309 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2309;
          }
        }
        if (!d2308) {
          d2308 = 1;
          {
            static struct u_object l_u_has_type_2310 = U_LIT("u_has_type");
            u_value t2311 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2310, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2311;
          }
        }
        if (!d2308) {
          u_no_case(s2307);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2312 = U_LIT("!");
        u_value t2313 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2312, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2313;
      }
    }
    if (!d2300) {
      u_no_case(s2299);
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
    u_value s2314 = v_cases;
    int d2315 = 0;
    if ((!d2315) && u_is_con(s2314, &n_list_46_Nil)) {
      d2315 = 1;
      {
        u_value t2316 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2316;
      }
    }
    if ((!d2315) && (u_is_con(s2314, &n_list_46_Cons) && u_is_con(u_field(s2314, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2314, 0), 0);
      u_value v_guard = u_field(u_field(s2314, 0), 1);
      u_value v_s = u_field(u_field(s2314, 0), 2);
      u_value v_more = u_field(s2314, 1);
      d2315 = 1;
      {
        u_value t2317 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2317;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2318 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2318;
        u_value s2319 = v_guard;
        int d2320 = 0;
        if ((!d2320) && u_is_con(s2319, &n_option_46_Some)) {
          u_value v_c = u_field(s2319, 0);
          d2320 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2321 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2321;
          }
        }
        if ((!d2320) && u_is_con(s2319, &n_option_46_None)) {
          d2320 = 1;
          {
          }
        }
        if (!d2320) {
          u_no_case(s2319);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2322 = U_LIT("!");
        u_value t2323 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2322, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2323;
      }
    }
    if (!d2315) {
      u_no_case(s2314);
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
    u_value s2324 = v_c;
    int d2325 = 0;
    if ((!d2325) && u_is_con(s2324, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2324, 0);
      u_value v_xss = u_field(s2324, 2);
      u_value v_b = u_field(s2324, 4);
      d2325 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2326 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2327 = &t2326;
        u_value l2328 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2328, &n_list_46_Cons)) {
          u_value v_x = u_field(l2328, 0);
          if (u_truth(u_mk_bool(1))) {
            e2327 = u_append(e2327, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2328 = u_field(l2328, 1);
        }
        u_value t2329 = t2326;
        v_params = t2329;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2330 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2330;
          }
        } else {
          {
            u_value t2331 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2331;
          }
        }
        u_value t2332 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2333 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2332;
        *r_ms = t2333;
      }
    }
    if ((!d2325) && u_is_con(s2324, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2324, 0);
      u_value v_ins = u_field(s2324, 2);
      u_value v_outs = u_field(s2324, 3);
      u_value v_refs = u_field(s2324, 4);
      u_value v_body = u_field(s2324, 5);
      d2325 = 1;
      {
        u_value t2334 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2335 = &t2334;
        u_value l2336 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2336, &n_list_46_Cons)) {
          u_value v_x = u_field(l2336, 0);
          if (u_truth(u_mk_bool(1))) {
            e2335 = u_append(e2335, f_compile_46_pointer_1(v_x));
          }
          l2336 = u_field(l2336, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2334), v_body, &v_bs, r_g);
        u_value t2337 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2338 = &t2337;
        u_value l2339 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2339, &n_list_46_Cons)) {
          u_value v_x = u_field(l2339, 0);
          if (u_truth(u_mk_bool(1))) {
            e2338 = u_append(e2338, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2339 = u_field(l2339, 1);
        }
        u_value t2340 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2341 = &t2340;
        u_value l2342 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2342, &n_list_46_Cons)) {
          u_value v_x = u_field(l2342, 0);
          if (u_truth(u_mk_bool(1))) {
            e2341 = u_append(e2341, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2342 = u_field(l2342, 1);
        }
        u_value t2343 = f_list_46__43__43__2(t2337, t2340);
        v_params = t2343;
        u_value t2344 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2345 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2344;
        *r_ms = t2345;
      }
    }
    if ((!d2325) && u_is_con(s2324, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s2324, 0);
      d2325 = 1;
      {
        p_compile_46_compile_expr(v_gl, u_mk_con(&n_list_46_Nil, 0), v_e, &v_bs, &v_rb, r_g);
        static struct u_object l_u_print_line_2346 = U_LIT("u_print_line");
        u_value t2347 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2348 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_line_2346, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2347;
        *r_ms = t2348;
      }
    }
    if ((!d2325) && (u_is_con(s2324, &n_syntax_46_CRun) && u_is_con(u_field(s2324, 1), &n_syntax_46_SBlock))) {
      u_value v_xs = u_field(u_field(s2324, 1), 0);
      u_value v_body = u_field(u_field(s2324, 1), 1);
      d2325 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_body, &v_bs, r_g);
        u_value t2349 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2350 = &t2349;
        u_value l2351 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2351, &n_list_46_Cons)) {
          u_value v_x = u_field(l2351, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2352 = U_LIT("NULL");
            e2350 = u_append(e2350, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2352))));
          }
          l2351 = u_field(l2351, 1);
        }
        u_value t2353 = t2349;
        v_copy = t2353;
        u_value t2354 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2355 = &t2354;
        u_value l2356 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2356, &n_list_46_Cons)) {
          u_value v_x = u_field(l2356, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_u_print_binding_2357 = U_LIT("u_print_binding");
            e2355 = u_append(e2355, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_binding_2357, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_syntax_46_Name_46_base_1(v_x)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x)), u_mk_con(&n_list_46_Nil, 0))))));
          }
          l2356 = u_field(l2356, 1);
        }
        u_value t2358 = t2354;
        v_back = t2358;
        u_value t2359 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2360 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_copy, f_list_46__43__43__2(v_bs, v_back))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2359;
        *r_ms = t2360;
      }
    }
    if ((!d2325) && u_is_con(s2324, &n_syntax_46_CRun)) {
      u_value v_body = u_field(s2324, 1);
      d2325 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2361 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2362 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2361;
        *r_ms = t2362;
      }
    }
    if ((!d2325) && u_is_con(s2324, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s2324, 1);
      d2325 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2363 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2364 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2363;
        *r_ms = t2364;
      }
    }
    if (!d2325) {
      d2325 = 1;
      {
        u_value t2365 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2366 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2365;
        *r_ms = t2366;
      }
    }
    if (!d2325) {
      u_no_case(s2324);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2367 = v_cs;
    int d2368 = 0;
    if ((!d2368) && u_is_con(s2367, &n_list_46_Nil)) {
      d2368 = 1;
      {
        u_value t2369 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2370 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2369;
        *r_ms = t2370;
      }
    }
    if ((!d2368) && u_is_con(s2367, &n_list_46_Cons)) {
      u_value v_c = u_field(s2367, 0);
      u_value v_rest = u_field(s2367, 1);
      d2368 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_rest, &v_d2, &v_m2, r_g);
        u_value t2371 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2372 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2371;
        *r_ms = t2372;
      }
    }
    if (!d2368) {
      u_no_case(s2367);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_handler_2373 = U_LIT("handler");
  u_value t2400 = u_mk_con(&n_c_46_CeVar, 1, &l_handler_2373);
  u_value t2399;
  {
    u_value v_h = t2400;
    static struct u_object l_static_32_u_value_2374 = U_LIT("static u_value");
    static struct u_object l_cache_2375 = U_LIT("cache");
    static struct u_object l_NULL_2376 = U_LIT("NULL");
    static struct u_object l__61__61__2377 = U_LIT("==");
    static struct u_object l_cache_2378 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2379 = U_LIT("U_COMPUTING");
    static struct u_object l_u_fail_2380 = U_LIT("u_fail");
    static struct u_object l_recursive_32_initia_2381 = U_LIT("recursive initialization of ");
    static struct u_object l_cache_2382 = U_LIT("cache");
    static struct u_object l_cache_2383 = U_LIT("cache");
    static struct u_object l_cache_2384 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2385 = U_LIT("U_COMPUTING");
    static struct u_object l_struct_32_u_handler_2386 = U_LIT("struct u_handler");
    static struct u_object l_handler_2387 = U_LIT("handler");
    static struct u_object l_u_push_2388 = U_LIT("u_push");
    static struct u_object l__33__61__2389 = U_LIT("!=");
    static struct u_object l_setjmp_2390 = U_LIT("setjmp");
    static struct u_object l_env_2391 = U_LIT("env");
    static struct u_object l_u_pop_2392 = U_LIT("u_pop");
    static struct u_object l_cache_2393 = U_LIT("cache");
    static struct u_object l_NULL_2394 = U_LIT("NULL");
    static struct u_object l_u_reraise_2395 = U_LIT("u_reraise");
    static struct u_object l_u_pop_2396 = U_LIT("u_pop");
    static struct u_object l_cache_2397 = U_LIT("cache");
    static struct u_object l_cache_2398 = U_LIT("cache");
    t2399 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2374), &l_cache_2375, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2376))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2377, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2378), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2379)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2380, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2381, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2382), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2383))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2384), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2385)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2386)), &l_handler_2387, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2388, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__33__61__2389, f_c_46_c_call_2(&l_setjmp_2390, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, v_h, &l_env_2391), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2392, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2393), u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2394)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2395, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2396, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2397), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2398))), u_mk_con(&n_list_46_Nil, 0))))));
  }
  return t2399;
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2401 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2402 = &t2401;
  u_value l2403 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l2403, &n_list_46_Cons)) {
    u_value v_p = u_field(l2403, 0);
    if (u_truth(u_mk_bool(1))) {
      e2402 = u_append(e2402, f_compile_46_fun_wrapper_1(v_p));
    }
    l2403 = u_field(l2403, 1);
  }
  u_value t2404 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2405 = &t2404;
  u_value l2406 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l2406, &n_list_46_Cons)) {
    u_value v_p = u_field(l2406, 0);
    if (u_truth(u_mk_bool(1))) {
      e2405 = u_append(e2405, f_compile_46_constr_wrapper_1(v_p));
    }
    l2406 = u_field(l2406, 1);
  }
  u_value t2407 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2408 = &t2407;
  u_value l2409 = f_list_46_seconds_1(f_map_46_entries_1(f_compile_46_builtin_functions_0()));
  while (u_is_con(l2409, &n_list_46_Cons)) {
    u_value v_p = u_field(l2409, 0);
    if (u_truth(u_mk_bool(1))) {
      e2408 = u_append(e2408, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2409 = u_field(l2409, 1);
  }
  return f_list_46__43__43__2(t2401, f_list_46__43__43__2(f_list_46_concat_1(t2404), t2407));
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2410 = v_p;
  u_value t2411 = NULL;
  if (!t2411) {
    u_value v_n = u_field(s2410, 0);
    u_value v_k = u_field(s2410, 1);
    static struct u_object l_w__2412 = U_LIT("w_");
    t2411 = f_compile_46_wrapper_3(u_concat(&l_w__2412, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2411) {
    u_no_case(s2410);
  }
  return t2411;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2413 = v_p;
  u_value t2414 = NULL;
  if (!t2414) {
    u_value v_f = u_field(s2413, 0);
    u_value v_k = u_field(s2413, 1);
    static struct u_object l_w_2415 = U_LIT("w");
    t2414 = f_compile_46_wrapper_3(u_concat(&l_w_2415, v_f), v_f, v_k);
  }
  if (!t2414) {
    u_no_case(s2413);
  }
  return t2414;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2416 = U_LIT("env");
  static struct u_object l_args_2417 = U_LIT("args");
  u_value t2418 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2419 = &t2418;
  u_value l2420 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2420, &n_list_46_Cons)) {
    u_value v_i = u_field(l2420, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2421 = U_LIT("args");
      e2419 = u_append(e2419, f_compile_46_index_2(&l_args_2421, v_i));
    }
    l2420 = u_field(l2420, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2416), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2417), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2418))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2422 = v_p;
  u_value t2423 = NULL;
  if ((!t2423) && u_equals(u_field(s2422, 1), u_mk_int(0))) {
    t2423 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2423) {
    u_value v_n = u_field(s2422, 0);
    u_value v_k = u_field(s2422, 1);
    static struct u_object l_wc__2424 = U_LIT("wc_");
    static struct u_object l_env_2425 = U_LIT("env");
    static struct u_object l_args_2426 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2427 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2428 = U_LIT("args");
    t2423 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2424, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2425), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2426), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2427, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2428), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2423) {
    u_no_case(s2422);
  }
  return t2423;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2429;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2429 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2429 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2429;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2430 = v_d;
  u_value t2431 = NULL;
  if ((!t2431) && u_is_con(s2430, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2430, 0);
    u_value v_f = u_field(s2430, 1);
    u_value v_ps = u_field(s2430, 2);
    t2431 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2431) {
    t2431 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2431) {
    u_no_case(s2430);
  }
  return t2431;
}

void p_compile_46_compile_program(u_value v_q, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2432 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2432;
    u_value t2433 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t2433;
    p_compile_46_compile_cmds(v_gl, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value t2434 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, f_compile_46_Gen_46_lifted_1(v_g)));
    v_fs = t2434;
    static struct u_object l_ulang_46_h_2435 = U_LIT("ulang.h");
    u_value t2436 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2437 = &t2436;
    u_value l2438 = v_fs;
    while (u_is_con(l2438, &n_list_46_Cons)) {
      u_value v_d = u_field(l2438, 0);
      if (u_truth(u_mk_bool(1))) {
        e2437 = u_append(e2437, f_compile_46_prototype_1(v_d));
      }
      l2438 = u_field(l2438, 1);
    }
    static struct u_object l_main_2439 = U_LIT("main");
    static struct u_object l_argc_2440 = U_LIT("argc");
    static struct u_object l_char_2441 = U_LIT("char");
    static struct u_object l_argv_2442 = U_LIT("argv");
    static struct u_object l_u_args_2443 = U_LIT("u_args");
    static struct u_object l_argc_2444 = U_LIT("argc");
    static struct u_object l_argv_2445 = U_LIT("argv");
    u_value t2446 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2435), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2436), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2439, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2440), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2441))), &l_argv_2442), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2443, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2444), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2445), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2446;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_target) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2447 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2447, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2448 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2448, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q);
    p_compile_46_compile_program(v_q, &v_f);
    p_os_write(v_target, f_c_46_c_file_1(v_f));
  }
}

u_value lam344(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam345(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam347(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam357(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_f = args[0];
  return f_ctx_46_is_fun_2(f_check_46_State_46_ctx_1(v_s), v_f);
}

u_value lam364(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam583(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_t = args[0];
  return f_ctx_46_is_sort_2(f_check_46_State_46_ctx_1(v_s), v_t);
}

u_value lam692(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam695(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam757(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_q = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_q);
}

u_value lam1034(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  {
    u_value v_args = NULL;
    p_os_args(&v_args);
    u_value s2449 = v_args;
    int d2450 = 0;
    if ((!d2450) && u_is_con(s2449, &n_list_46_Nil)) {
      d2450 = 1;
      {
      }
    }
    if ((!d2450) && (u_is_con(s2449, &n_list_46_Cons) && (u_is_con(u_field(s2449, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2449, 1), 1), &n_list_46_Nil)))) {
      u_value v_source = u_field(s2449, 0);
      u_value v_target = u_field(u_field(s2449, 1), 0);
      d2450 = 1;
      {
        p_compile_46_compile_file(v_source, v_target);
      }
    }
    if (!d2450) {
      d2450 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_sou_2451 = U_LIT("usage: ulang source.u target.c");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_sou_2451), "os.OsError");
      }
    }
    if (!d2450) {
      u_no_case(s2449);
    }
    u_print_binding("args", v_args);
  }
  return 0;
}
