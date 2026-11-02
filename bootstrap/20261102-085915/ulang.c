#include "ulang.h"

const struct u_name n_nat_46_Zero = {"nat", "Zero", 0};

const struct u_name n_nat_46_Succ = {"nat", "Succ", 1};

const struct u_name n_syntax_46_Name = {"syntax", "Name", 0};

const struct u_name n_syntax_46_THole = {"syntax", "THole", 0};

const struct u_name n_syntax_46_TMeta = {"syntax", "TMeta", 1};

const struct u_name n_syntax_46_TParam = {"syntax", "TParam", 2};

const struct u_name n_syntax_46_TSort = {"syntax", "TSort", 3};

const struct u_name n_syntax_46_TFun = {"syntax", "TFun", 4};

const struct u_name n_syntax_46_TProc = {"syntax", "TProc", 5};

const struct u_name n_syntax_46_TTuple = {"syntax", "TTuple", 6};

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

const struct u_name n_syntax_46_EProc = {"syntax", "EProc", 4};

const struct u_name n_syntax_46_ELet = {"syntax", "ELet", 5};

const struct u_name n_syntax_46_EIte = {"syntax", "EIte", 6};

const struct u_name n_syntax_46_EMatch = {"syntax", "EMatch", 7};

const struct u_name n_syntax_46_EComp = {"syntax", "EComp", 8};

const struct u_name n_syntax_46_ETuple = {"syntax", "ETuple", 9};

const struct u_name n_syntax_46_ERaise = {"syntax", "ERaise", 10};

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

void wp_check_46_check_program(u_value *env, u_value *ins, u_value **outs);

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

void wp_check_46_one_of(u_value *env, u_value *ins, u_value **outs);

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

void wp_compile_46_expr_cases(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_gen_name(u_value *env, u_value *ins, u_value **outs);

void wp_compile_46_global_value(u_value *env, u_value *ins, u_value **outs);

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

void wp_parse_46_formal_sections(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_keyword(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_lambda_function(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_last_case(u_value *env, u_value *ins, u_value **outs);

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

void wp_parse_46_parse_formals1(u_value *env, u_value *ins, u_value **outs);

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

void wp_parse_46_parse_variables(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_pattern_items(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_pattern_list(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_pattern_simple(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_resolve_items(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_same_commands(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_same_parses(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_set_comprehension(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_skip_comma(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_type_basic(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_type_items(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_type_list(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_type_params(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_type_sections(u_value *env, u_value *ins, u_value **outs);

void wp_parse_46_types1(u_value *env, u_value *ins, u_value **outs);

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

void p_parse_46_type_sections(u_value v_sc, u_value *r_ins, u_value *r_outs, u_value *r_refs, u_value *r_ts);

void p_parse_46_types1(u_value v_sc, u_value *r_us, u_value *r_ts);

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

void p_parse_46_formal_sections(u_value v_sc, u_value *r_ins, u_value *r_outs, u_value *r_refs, u_value *r_ts);

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

void p_compile_46_global_value(u_value v_gl, u_value v_n, u_value v_t, u_value *r_r);

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

void p_compile_46_compile_cmd(u_value v_gl, u_value v_c, u_value *r_ds, u_value *r_ms, u_value *r_g);

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g);

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r);

u_value f_compile_46_wrappers_1(u_value v_gl);

u_value f_compile_46_proc_wrapper_1(u_value v_p);

u_value f_compile_46_fun_wrapper_1(u_value v_p);

u_value f_compile_46_builtin_wrapper_1(u_value v_p);

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k);

u_value f_compile_46_constr_wrapper_1(u_value v_p);

u_value f_compile_46_range_to_1(u_value v_k);

u_value f_compile_46_prototype_1(u_value v_d);

void p_compile_46_compile_program(u_value v_q, u_value *r_f);

void p_compile_46_compile_file(u_value v_source, u_value v_target);

u_value lam356(u_value *env, u_value *args);

u_value lam357(u_value *env, u_value *args);

u_value lam359(u_value *env, u_value *args);

u_value lam360(u_value *env, u_value *args);

u_value lam370(u_value *env, u_value *args);

u_value lam377(u_value *env, u_value *args);

u_value lam381(u_value *env, u_value *args);

u_value lam614(u_value *env, u_value *args);

u_value lam725(u_value *env, u_value *args);

u_value lam728(u_value *env, u_value *args);

u_value lam793(u_value *env, u_value *args);

u_value lam1070(u_value *env, u_value *args);

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

void wp_check_46_check_program(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_check_program(ins[0], ins[1], outs[0]);
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

void wp_check_46_one_of(u_value *env, u_value *ins, u_value **outs) {
  p_check_46_one_of(ins[0], ins[1], outs[0]);
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
  p_compile_46_compile_cmd(ins[0], ins[1], outs[0], outs[1], outs[2]);
}

void wp_compile_46_compile_cmds(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_cmds(ins[0], ins[1], outs[0], outs[1], outs[2]);
}

void wp_compile_46_compile_expr(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_expr(ins[0], ins[1], ins[2], outs[0], outs[1], outs[2]);
}

void wp_compile_46_compile_exprs(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_exprs(ins[0], ins[1], ins[2], outs[0], outs[1], outs[2]);
}

void wp_compile_46_compile_file(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_file(ins[0], ins[1]);
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
  p_compile_46_compile_program(ins[0], outs[0]);
}

void wp_compile_46_compile_stmt(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_stmt(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_compile_46_compile_stmts(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_compile_stmts(ins[0], ins[1], ins[2], outs[0], outs[1]);
}

void wp_compile_46_expr_cases(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_expr_cases(ins[0], ins[1], ins[2], ins[3], ins[4], outs[0], outs[1]);
}

void wp_compile_46_gen_name(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_gen_name(ins[0], outs[0], outs[1]);
}

void wp_compile_46_global_value(u_value *env, u_value *ins, u_value **outs) {
  p_compile_46_global_value(ins[0], ins[1], ins[2], outs[0]);
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

void wp_parse_46_formal_sections(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_formal_sections(ins[0], outs[0], outs[1], outs[2], outs[3]);
}

void wp_parse_46_keyword(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_keyword(ins[0], outs[0]);
}

void wp_parse_46_lambda_function(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_lambda_function(ins[0], outs[0], outs[1]);
}

void wp_parse_46_last_case(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_last_case(outs[0]);
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

void wp_parse_46_parse_formals1(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_formals1(ins[0], outs[0], outs[1]);
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

void wp_parse_46_parse_variables(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_parse_variables(ins[0], outs[0], outs[1]);
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

void wp_parse_46_type_sections(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_type_sections(ins[0], outs[0], outs[1], outs[2], outs[3]);
}

void wp_parse_46_types1(u_value *env, u_value *ins, u_value **outs) {
  p_parse_46_types1(ins[0], outs[0], outs[1]);
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
  if ((!t304) && u_is_con(s303, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s303, 0);
    u_value v_os = u_field(s303, 1);
    u_value v_rs = u_field(s303, 2);
    u_value t311 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e312 = &t311;
    u_value l313 = f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs));
    while (u_is_con(l313, &n_list_46_Cons)) {
      u_value v_a = u_field(l313, 0);
      if (u_truth(u_mk_bool(1))) {
        e312 = u_append(e312, f_check_46_free_1(v_a));
      }
      l313 = u_field(l313, 1);
    }
    t304 = f_list_46_distinct_1(f_list_46_concat_1(t311));
  }
  if ((!t304) && u_is_con(s303, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s303, 0);
    u_value t314 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e315 = &t314;
    u_value l316 = v_ts;
    while (u_is_con(l316, &n_list_46_Cons)) {
      u_value v_a = u_field(l316, 0);
      if (u_truth(u_mk_bool(1))) {
        e315 = u_append(e315, f_check_46_free_1(v_a));
      }
      l316 = u_field(l316, 1);
    }
    t304 = f_list_46_distinct_1(f_list_46_concat_1(t314));
  }
  if (!t304) {
    u_no_case(s303);
  }
  return t304;
}

u_value f_check_46_holes_1(u_value v_t) {
  u_value s317 = v_t;
  u_value t318 = NULL;
  if ((!t318) && u_is_con(s317, &n_syntax_46_THole)) {
    t318 = u_mk_bool(1);
  }
  if ((!t318) && u_is_con(s317, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s317, 1);
    u_value t319 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e320 = &t319;
    u_value l321 = v_args;
    while (u_is_con(l321, &n_list_46_Cons)) {
      u_value v_a = u_field(l321, 0);
      if (u_truth(u_mk_bool(1))) {
        e320 = u_append(e320, f_check_46_holes_1(v_a));
      }
      l321 = u_field(l321, 1);
    }
    t318 = f_list_46_contains_2(u_mk_bool(1), t319);
  }
  if ((!t318) && u_is_con(s317, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s317, 0);
    u_value v_r = u_field(s317, 1);
    u_value t322 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e323 = &t322;
    u_value l324 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l324, &n_list_46_Cons)) {
      u_value v_a = u_field(l324, 0);
      if (u_truth(u_mk_bool(1))) {
        e323 = u_append(e323, f_check_46_holes_1(v_a));
      }
      l324 = u_field(l324, 1);
    }
    t318 = f_list_46_contains_2(u_mk_bool(1), t322);
  }
  if ((!t318) && u_is_con(s317, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s317, 0);
    u_value v_os = u_field(s317, 1);
    u_value v_rs = u_field(s317, 2);
    t318 = f_list_46_exists_2(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs)), u_mk_closure(w_f_check_46_holes_1, 0));
  }
  if ((!t318) && u_is_con(s317, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s317, 0);
    u_value t325 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e326 = &t325;
    u_value l327 = v_ts;
    while (u_is_con(l327, &n_list_46_Cons)) {
      u_value v_a = u_field(l327, 0);
      if (u_truth(u_mk_bool(1))) {
        e326 = u_append(e326, f_check_46_holes_1(v_a));
      }
      l327 = u_field(l327, 1);
    }
    t318 = f_list_46_contains_2(u_mk_bool(1), t325);
  }
  if (!t318) {
    t318 = u_mk_bool(0);
  }
  if (!t318) {
    u_no_case(s317);
  }
  return t318;
}

u_value f_check_46_subst_2(u_value v_t, u_value v_m) {
  u_value s328 = v_t;
  u_value t329 = NULL;
  if ((!t329) && u_is_con(s328, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s328, 0);
    u_value v_args = u_field(s328, 1);
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
    t329 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t330);
  }
  if ((!t329) && u_is_con(s328, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s328, 0);
    u_value v_r = u_field(s328, 1);
    u_value t333 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e334 = &t333;
    u_value l335 = v_args;
    while (u_is_con(l335, &n_list_46_Cons)) {
      u_value v_a = u_field(l335, 0);
      if (u_truth(u_mk_bool(1))) {
        e334 = u_append(e334, f_check_46_subst_2(v_a, v_m));
      }
      l335 = u_field(l335, 1);
    }
    t329 = u_mk_con(&n_syntax_46_TFun, 2, t333, f_check_46_subst_2(v_r, v_m));
  }
  if ((!t329) && u_is_con(s328, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s328, 0);
    u_value v_os = u_field(s328, 1);
    u_value v_rs = u_field(s328, 2);
    u_value t336 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e337 = &t336;
    u_value l338 = v_is;
    while (u_is_con(l338, &n_list_46_Cons)) {
      u_value v_a = u_field(l338, 0);
      if (u_truth(u_mk_bool(1))) {
        e337 = u_append(e337, f_check_46_subst_2(v_a, v_m));
      }
      l338 = u_field(l338, 1);
    }
    u_value t339 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e340 = &t339;
    u_value l341 = v_os;
    while (u_is_con(l341, &n_list_46_Cons)) {
      u_value v_a = u_field(l341, 0);
      if (u_truth(u_mk_bool(1))) {
        e340 = u_append(e340, f_check_46_subst_2(v_a, v_m));
      }
      l341 = u_field(l341, 1);
    }
    u_value t342 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e343 = &t342;
    u_value l344 = v_rs;
    while (u_is_con(l344, &n_list_46_Cons)) {
      u_value v_a = u_field(l344, 0);
      if (u_truth(u_mk_bool(1))) {
        e343 = u_append(e343, f_check_46_subst_2(v_a, v_m));
      }
      l344 = u_field(l344, 1);
    }
    t329 = u_mk_con(&n_syntax_46_TProc, 3, t336, t339, t342);
  }
  if ((!t329) && u_is_con(s328, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s328, 0);
    u_value t345 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e346 = &t345;
    u_value l347 = v_ts;
    while (u_is_con(l347, &n_list_46_Cons)) {
      u_value v_a = u_field(l347, 0);
      if (u_truth(u_mk_bool(1))) {
        e346 = u_append(e346, f_check_46_subst_2(v_a, v_m));
      }
      l347 = u_field(l347, 1);
    }
    t329 = u_mk_con(&n_syntax_46_TTuple, 1, t345);
  }
  if (!t329) {
    u_value s348 = f_list_46_lookup_2(v_t, v_m);
    u_value t349 = NULL;
    if ((!t349) && u_is_con(s348, &n_option_46_Some)) {
      u_value v_u = u_field(s348, 0);
      t349 = v_u;
    }
    if ((!t349) && u_is_con(s348, &n_option_46_None)) {
      t349 = v_t;
    }
    if (!t349) {
      u_no_case(s348);
    }
    t329 = t349;
  }
  if (!t329) {
    u_no_case(s328);
  }
  return t329;
}

u_value f_check_46_resolve_2(u_value v_su, u_value v_t) {
  u_value s350 = v_t;
  u_value t351 = NULL;
  if ((!t351) && u_is_con(s350, &n_syntax_46_TMeta)) {
    u_value v_i = u_field(s350, 0);
    u_value s352 = f_map_46_get_2(v_i, v_su);
    u_value t353 = NULL;
    if ((!t353) && u_is_con(s352, &n_option_46_Some)) {
      u_value v_u = u_field(s352, 0);
      t353 = f_check_46_resolve_2(v_su, v_u);
    }
    if ((!t353) && u_is_con(s352, &n_option_46_None)) {
      t353 = v_t;
    }
    if (!t353) {
      u_no_case(s352);
    }
    t351 = t353;
  }
  if (!t351) {
    t351 = v_t;
  }
  if (!t351) {
    u_no_case(s350);
  }
  return t351;
}

u_value f_check_46_occurs_3(u_value v_su, u_value v_i, u_value v_t) {
  u_value s354 = f_check_46_resolve_2(v_su, v_t);
  u_value t355 = NULL;
  if ((!t355) && u_is_con(s354, &n_syntax_46_TMeta)) {
    u_value v_j = u_field(s354, 0);
    t355 = u_eq(v_i, v_j);
  }
  if ((!t355) && u_is_con(s354, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s354, 1);
    t355 = f_list_46_exists_2(v_args, u_mk_closure(lam356, 2, v_su, v_i));
  }
  if ((!t355) && u_is_con(s354, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s354, 0);
    u_value v_r = u_field(s354, 1);
    u_value t358 = f_list_46_exists_2(v_args, u_mk_closure(lam357, 2, v_su, v_i));
    if (!u_truth(t358)) {
      t358 = f_check_46_occurs_3(v_su, v_i, v_r);
    }
    t355 = t358;
  }
  if ((!t355) && u_is_con(s354, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s354, 0);
    u_value v_os = u_field(s354, 1);
    u_value v_rs = u_field(s354, 2);
    t355 = f_list_46_exists_2(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs)), u_mk_closure(lam359, 2, v_su, v_i));
  }
  if ((!t355) && u_is_con(s354, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s354, 0);
    t355 = f_list_46_exists_2(v_ts, u_mk_closure(lam360, 2, v_su, v_i));
  }
  if (!t355) {
    t355 = u_mk_bool(0);
  }
  if (!t355) {
    u_no_case(s354);
  }
  return t355;
}

u_value f_check_46_constructor_scheme_1(u_value v_k) {
  u_value s361 = v_k;
  u_value t362 = NULL;
  if (!t362) {
    u_value v_ps = u_field(s361, 0);
    u_value v_ks = u_field(s361, 1);
    u_value v_r = u_field(s361, 2);
    u_value t363;
    if (u_truth(u_eq(v_ks, u_mk_con(&n_list_46_Nil, 0)))) {
      t363 = v_r;
    } else {
      t363 = f_check_46_fun_2(v_ks, v_r);
    }
    t362 = u_mk_tuple(2, v_ps, t363);
  }
  if (!t362) {
    u_no_case(s361);
  }
  return t362;
}

u_value f_check_46_constructor_of_2(u_value v_c, u_value v_k) {
  u_value s364 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t365 = NULL;
  if ((!t365) && u_is_con(s364, &n_option_46_Some)) {
    u_value v_kc = u_field(s364, 0);
    t365 = f_check_46_constructor_scheme_1(v_kc);
  }
  if ((!t365) && u_is_con(s364, &n_option_46_None)) {
    t365 = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t365) {
    u_no_case(s364);
  }
  return t365;
}

u_value f_check_46_candidates_3(u_value v_s, u_value v_env, u_value v_n) {
  u_value s366 = f_list_46_lookup_2(v_n, v_env);
  u_value t367 = NULL;
  if ((!t367) && u_is_con(s366, &n_option_46_Some)) {
    u_value v_sc = u_field(s366, 0);
    t367 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, v_sc), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t367) && u_is_con(s366, &n_option_46_None)) {
    u_value t368 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e369 = &t368;
    u_value l371 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam370, 1, v_s));
    while (u_is_con(l371, &n_list_46_Cons)) {
      u_value v_f = u_field(l371, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t372 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e373 = &t372;
        u_value l374 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(v_s), v_f);
        while (u_is_con(l374, &n_list_46_Cons)) {
          u_value v_sc = u_field(l374, 0);
          if (u_truth(u_mk_bool(1))) {
            e373 = u_append(e373, u_mk_tuple(2, v_f, v_sc));
          }
          l374 = u_field(l374, 1);
        }
        e369 = u_append(e369, t372);
      }
      l371 = u_field(l371, 1);
    }
    u_value t375 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e376 = &t375;
    u_value l378 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam377, 1, v_s));
    while (u_is_con(l378, &n_list_46_Cons)) {
      u_value v_k = u_field(l378, 0);
      if (u_truth(u_mk_bool(1))) {
        e376 = u_append(e376, u_mk_tuple(2, v_k, f_check_46_constructor_of_2(f_check_46_State_46_ctx_1(v_s), v_k)));
      }
      l378 = u_field(l378, 1);
    }
    u_value t379 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e380 = &t379;
    u_value l382 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam381, 1, v_s));
    while (u_is_con(l382, &n_list_46_Cons)) {
      u_value v_p = u_field(l382, 0);
      if (u_truth(u_mk_bool(1))) {
        e380 = u_append(e380, u_mk_tuple(2, v_p, f_check_46_proc_scheme_2(f_check_46_State_46_ctx_1(v_s), v_p)));
      }
      l382 = u_field(l382, 1);
    }
    u_value t383 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e384 = &t383;
    u_value l385 = f_check_46_builtin_1(v_n);
    while (u_is_con(l385, &n_list_46_Cons)) {
      u_value v_sc = u_field(l385, 0);
      if (u_truth(u_mk_bool(1))) {
        e384 = u_append(e384, u_mk_tuple(2, v_n, v_sc));
      }
      l385 = u_field(l385, 1);
    }
    t367 = f_list_46__43__43__2(f_list_46_concat_1(t368), f_list_46__43__43__2(t375, f_list_46__43__43__2(t379, t383)));
  }
  if (!t367) {
    u_no_case(s366);
  }
  return t367;
}

u_value f_check_46_proc_scheme_2(u_value v_c, u_value v_p) {
  u_value s386 = f_ctx_46_proc_of_2(v_c, v_p);
  u_value t387 = NULL;
  if ((!t387) && u_is_con(s386, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s386, 0), 0);
    u_value v_is = u_field(u_field(s386, 0), 1);
    u_value v_os = u_field(u_field(s386, 0), 2);
    u_value v_rs = u_field(u_field(s386, 0), 3);
    t387 = u_mk_tuple(2, v_ps, u_mk_con(&n_syntax_46_TProc, 3, v_is, v_os, v_rs));
  }
  if ((!t387) && u_is_con(s386, &n_option_46_None)) {
    t387 = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_syntax_46_THole, 0));
  }
  if (!t387) {
    u_no_case(s386);
  }
  return t387;
}

u_value f_check_46_with_arity_2(u_value v_cs, u_value v_k) {
  u_value s388 = v_cs;
  u_value t389 = NULL;
  if ((!t389) && u_is_con(s388, &n_list_46_Nil)) {
    t389 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t389) && u_is_con(s388, &n_list_46_Cons)) {
    u_value v_n = u_field(u_field(s388, 0), 0);
    u_value v_ps = u_field(u_field(u_field(s388, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s388, 0), 1), 1);
    u_value v_rest = u_field(s388, 1);
    u_value t390;
    if (u_truth(u_eq(f_syntax_46_arity_1(v_t), v_k))) {
      t390 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, v_ps, v_t)), f_check_46_with_arity_2(v_rest, v_k));
    } else {
      t390 = f_check_46_with_arity_2(v_rest, v_k);
    }
    t389 = t390;
  }
  if (!t389) {
    u_no_case(s388);
  }
  return t389;
}

u_value f_check_46_has_arity_2(u_value v_scs, u_value v_k) {
  u_value s391 = v_scs;
  u_value t392 = NULL;
  if ((!t392) && u_is_con(s391, &n_list_46_Nil)) {
    t392 = u_mk_bool(0);
  }
  if ((!t392) && u_is_con(s391, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s391, 0), 1);
    u_value v_rest = u_field(s391, 1);
    u_value t393 = u_eq(f_syntax_46_arity_1(v_t), v_k);
    if (!u_truth(t393)) {
      t393 = f_check_46_has_arity_2(v_rest, v_k);
    }
    t392 = t393;
  }
  if (!t392) {
    u_no_case(s391);
  }
  return t392;
}

u_value f_check_46_bind_1(u_value v_xs) {
  u_value t394 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e395 = &t394;
  u_value l396 = v_xs;
  while (u_is_con(l396, &n_list_46_Cons)) {
    u_value v_x = u_field(l396, 0);
    if (u_truth(u_mk_bool(1))) {
      e395 = u_append(e395, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_syntax_46_Var_46_typ_1(v_x))));
    }
    l396 = u_field(l396, 1);
  }
  return t394;
}

u_value f_check_46_fixed_2(u_value v_su, u_value v_env) {
  u_value s397 = v_env;
  u_value t398 = NULL;
  if ((!t398) && u_is_con(s397, &n_list_46_Nil)) {
    t398 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t398) && u_is_con(s397, &n_list_46_Cons)) {
    u_value v_ps = u_field(u_field(u_field(s397, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s397, 0), 1), 1);
    u_value v_rest = u_field(s397, 1);
    t398 = f_list_46__43__43__2(f_list_46_diff_2(f_check_46_free_1(f_check_46_zonk_type_2(v_su, v_t)), v_ps), f_check_46_fixed_2(v_su, v_rest));
  }
  if (!t398) {
    u_no_case(s397);
  }
  return t398;
}

u_value f_check_46_generalize_3(u_value v_su, u_value v_env, u_value v_t) {
  u_value t404 = f_check_46_fixed_2(v_su, v_env);
  u_value t405 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t403;
  {
    u_value v_fx = t404;
    u_value v_u = t405;
    u_value t399 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e400 = &t399;
    u_value l401 = f_check_46_free_1(v_u);
    while (u_is_con(l401, &n_list_46_Cons)) {
      u_value v_m = u_field(l401, 0);
      u_value t402 = f_check_46_is_meta_1(v_m);
      if (u_truth(t402)) {
        t402 = f_prelude_46_not_1(f_list_46_contains_2(v_m, v_fx));
      }
      if (u_truth(t402)) {
        e400 = u_append(e400, v_m);
      }
      l401 = u_field(l401, 1);
    }
    t403 = u_mk_tuple(2, t399, v_u);
  }
  return t403;
}

u_value f_check_46_generalized_3(u_value v_su, u_value v_env, u_value v_xs) {
  u_value t406 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e407 = &t406;
  u_value l408 = v_xs;
  while (u_is_con(l408, &n_list_46_Cons)) {
    u_value v_x = u_field(l408, 0);
    if (u_truth(u_mk_bool(1))) {
      e407 = u_append(e407, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), f_check_46_generalize_3(v_su, v_env, f_syntax_46_Var_46_typ_1(v_x))));
    }
    l408 = u_field(l408, 1);
  }
  return t406;
}

u_value f_check_46_case_of_1(u_value v_b) {
  u_value s409 = v_b;
  u_value t410 = NULL;
  if ((!t410) && u_is_con(s409, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s409, 0);
    u_value v_g = u_field(s409, 1);
    t410 = u_mk_tuple(2, v_p, u_eq(v_g, u_mk_con(&n_option_46_None, 0)));
  }
  if (!t410) {
    u_no_case(s409);
  }
  return t410;
}

u_value f_check_46_zonk_guard_2(u_value v_su, u_value v_g) {
  u_value s411 = v_g;
  u_value t412 = NULL;
  if ((!t412) && u_is_con(s411, &n_option_46_Some)) {
    u_value v_e = u_field(s411, 0);
    t412 = u_mk_con(&n_option_46_Some, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t412) && u_is_con(s411, &n_option_46_None)) {
    t412 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t412) {
    u_no_case(s411);
  }
  return t412;
}

u_value f_check_46_zonk_type_2(u_value v_su, u_value v_t) {
  u_value s413 = f_check_46_resolve_2(v_su, v_t);
  u_value t414 = NULL;
  if ((!t414) && u_is_con(s413, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s413, 0);
    u_value v_args = u_field(s413, 1);
    u_value t415 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e416 = &t415;
    u_value l417 = v_args;
    while (u_is_con(l417, &n_list_46_Cons)) {
      u_value v_a = u_field(l417, 0);
      if (u_truth(u_mk_bool(1))) {
        e416 = u_append(e416, f_check_46_zonk_type_2(v_su, v_a));
      }
      l417 = u_field(l417, 1);
    }
    t414 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t415);
  }
  if ((!t414) && u_is_con(s413, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s413, 0);
    u_value v_r = u_field(s413, 1);
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
    t414 = u_mk_con(&n_syntax_46_TFun, 2, t418, f_check_46_zonk_type_2(v_su, v_r));
  }
  if ((!t414) && u_is_con(s413, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s413, 0);
    u_value v_os = u_field(s413, 1);
    u_value v_rs = u_field(s413, 2);
    u_value t421 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e422 = &t421;
    u_value l423 = v_is;
    while (u_is_con(l423, &n_list_46_Cons)) {
      u_value v_a = u_field(l423, 0);
      if (u_truth(u_mk_bool(1))) {
        e422 = u_append(e422, f_check_46_zonk_type_2(v_su, v_a));
      }
      l423 = u_field(l423, 1);
    }
    u_value t424 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e425 = &t424;
    u_value l426 = v_os;
    while (u_is_con(l426, &n_list_46_Cons)) {
      u_value v_a = u_field(l426, 0);
      if (u_truth(u_mk_bool(1))) {
        e425 = u_append(e425, f_check_46_zonk_type_2(v_su, v_a));
      }
      l426 = u_field(l426, 1);
    }
    u_value t427 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e428 = &t427;
    u_value l429 = v_rs;
    while (u_is_con(l429, &n_list_46_Cons)) {
      u_value v_a = u_field(l429, 0);
      if (u_truth(u_mk_bool(1))) {
        e428 = u_append(e428, f_check_46_zonk_type_2(v_su, v_a));
      }
      l429 = u_field(l429, 1);
    }
    t414 = u_mk_con(&n_syntax_46_TProc, 3, t421, t424, t427);
  }
  if ((!t414) && u_is_con(s413, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s413, 0);
    u_value t430 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e431 = &t430;
    u_value l432 = v_ts;
    while (u_is_con(l432, &n_list_46_Cons)) {
      u_value v_a = u_field(l432, 0);
      if (u_truth(u_mk_bool(1))) {
        e431 = u_append(e431, f_check_46_zonk_type_2(v_su, v_a));
      }
      l432 = u_field(l432, 1);
    }
    t414 = u_mk_con(&n_syntax_46_TTuple, 1, t430);
  }
  if (!t414) {
    u_value v_u = s413;
    t414 = v_u;
  }
  if (!t414) {
    u_no_case(s413);
  }
  return t414;
}

u_value f_check_46_zonk_var_2(u_value v_su, u_value v_x) {
  return f_syntax_46_Var_46_typ_58__61__2(v_x, f_check_46_zonk_type_2(v_su, f_syntax_46_Var_46_typ_1(v_x)));
}

u_value f_check_46_zonk_vars_2(u_value v_su, u_value v_xs) {
  u_value t433 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e434 = &t433;
  u_value l435 = v_xs;
  while (u_is_con(l435, &n_list_46_Cons)) {
    u_value v_x = u_field(l435, 0);
    if (u_truth(u_mk_bool(1))) {
      e434 = u_append(e434, f_check_46_zonk_var_2(v_su, v_x));
    }
    l435 = u_field(l435, 1);
  }
  return t433;
}

u_value f_check_46_zonk_expr_2(u_value v_su, u_value v_e) {
  u_value s436 = v_e;
  u_value t437 = NULL;
  if ((!t437) && u_is_con(s436, &n_syntax_46_ELit)) {
    t437 = v_e;
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_EVar)) {
    u_value v_x = u_field(s436, 0);
    t437 = u_mk_con(&n_syntax_46_EVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s436, 0);
    u_value v_args = u_field(s436, 1);
    u_value t438 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e439 = &t438;
    u_value l440 = v_args;
    while (u_is_con(l440, &n_list_46_Cons)) {
      u_value v_a = u_field(l440, 0);
      if (u_truth(u_mk_bool(1))) {
        e439 = u_append(e439, f_check_46_zonk_expr_2(v_su, v_a));
      }
      l440 = u_field(l440, 1);
    }
    t437 = u_mk_con(&n_syntax_46_EApp, 2, f_check_46_zonk_expr_2(v_su, v_f), t438);
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s436, 0);
    u_value v_b = u_field(s436, 1);
    t437 = u_mk_con(&n_syntax_46_ELam, 2, f_check_46_zonk_vars_2(v_su, v_xs), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_EProc)) {
    u_value v_is = u_field(s436, 0);
    u_value v_os = u_field(s436, 1);
    u_value v_rs = u_field(s436, 2);
    u_value v_b = u_field(s436, 3);
    t437 = u_mk_con(&n_syntax_46_EProc, 4, f_check_46_zonk_vars_2(v_su, v_is), f_check_46_zonk_vars_2(v_su, v_os), f_check_46_zonk_vars_2(v_su, v_rs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s436, 0);
    u_value v_b = u_field(s436, 1);
    u_value t441 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e442 = &t441;
    u_value l443 = v_ps;
    while (u_is_con(l443, &n_list_46_Cons)) {
      u_value v_p = u_field(l443, 0);
      if (u_truth(u_mk_bool(1))) {
        e442 = u_append(e442, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l443 = u_field(l443, 1);
    }
    t437 = u_mk_con(&n_syntax_46_ELet, 2, t441, f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s436, 0);
    u_value v_l = u_field(s436, 1);
    u_value v_r = u_field(s436, 2);
    t437 = u_mk_con(&n_syntax_46_EIte, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_expr_2(v_su, v_l), f_check_46_zonk_expr_2(v_su, v_r));
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s436, 0);
    u_value v_bs = u_field(s436, 1);
    u_value t444 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e445 = &t444;
    u_value l446 = v_bs;
    while (u_is_con(l446, &n_list_46_Cons)) {
      u_value v_b = u_field(l446, 0);
      if (u_truth(u_mk_bool(1))) {
        e445 = u_append(e445, f_check_46_zonk_expr_branch_2(v_su, v_b));
      }
      l446 = u_field(l446, 1);
    }
    t437 = u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t444);
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s436, 0);
    u_value v_xs = u_field(s436, 1);
    u_value v_ls = u_field(s436, 2);
    u_value v_c = u_field(s436, 3);
    u_value t447 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e448 = &t447;
    u_value l449 = v_ls;
    while (u_is_con(l449, &n_list_46_Cons)) {
      u_value v_l = u_field(l449, 0);
      if (u_truth(u_mk_bool(1))) {
        e448 = u_append(e448, f_check_46_zonk_expr_2(v_su, v_l));
      }
      l449 = u_field(l449, 1);
    }
    t437 = u_mk_con(&n_syntax_46_EComp, 4, f_check_46_zonk_expr_2(v_su, v_r), f_check_46_zonk_vars_2(v_su, v_xs), t447, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s436, 0);
    u_value t450 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e451 = &t450;
    u_value l452 = v_es;
    while (u_is_con(l452, &n_list_46_Cons)) {
      u_value v_e = u_field(l452, 0);
      if (u_truth(u_mk_bool(1))) {
        e451 = u_append(e451, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l452 = u_field(l452, 1);
    }
    t437 = u_mk_con(&n_syntax_46_ETuple, 1, t450);
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s436, 0);
    u_value v_t = u_field(s436, 1);
    t437 = u_mk_con(&n_syntax_46_ERaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t437) {
    u_no_case(s436);
  }
  return t437;
}

u_value f_check_46_zonk_binding_2(u_value v_su, u_value v_p) {
  u_value s453 = v_p;
  u_value t454 = NULL;
  if (!t454) {
    u_value v_x = u_field(s453, 0);
    u_value v_e = u_field(s453, 1);
    t454 = u_mk_tuple(2, f_check_46_zonk_var_2(v_su, v_x), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t454) {
    u_no_case(s453);
  }
  return t454;
}

u_value f_check_46_zonk_expr_branch_2(u_value v_su, u_value v_b) {
  u_value s455 = v_b;
  u_value t456 = NULL;
  if ((!t456) && u_is_con(s455, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s455, 0);
    u_value v_g = u_field(s455, 1);
    u_value v_e = u_field(s455, 2);
    t456 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t456) {
    u_no_case(s455);
  }
  return t456;
}

u_value f_check_46_zonk_pat_2(u_value v_su, u_value v_p) {
  u_value s457 = v_p;
  u_value t458 = NULL;
  if ((!t458) && u_is_con(s457, &n_syntax_46_PVar)) {
    u_value v_x = u_field(s457, 0);
    t458 = u_mk_con(&n_syntax_46_PVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t458) && u_is_con(s457, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s457, 0);
    u_value v_args = u_field(s457, 1);
    u_value t459 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e460 = &t459;
    u_value l461 = v_args;
    while (u_is_con(l461, &n_list_46_Cons)) {
      u_value v_a = u_field(l461, 0);
      if (u_truth(u_mk_bool(1))) {
        e460 = u_append(e460, f_check_46_zonk_pat_2(v_su, v_a));
      }
      l461 = u_field(l461, 1);
    }
    t458 = u_mk_con(&n_syntax_46_PApp, 2, v_n, t459);
  }
  if ((!t458) && u_is_con(s457, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s457, 0);
    u_value t462 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e463 = &t462;
    u_value l464 = v_ps;
    while (u_is_con(l464, &n_list_46_Cons)) {
      u_value v_q = u_field(l464, 0);
      if (u_truth(u_mk_bool(1))) {
        e463 = u_append(e463, f_check_46_zonk_pat_2(v_su, v_q));
      }
      l464 = u_field(l464, 1);
    }
    t458 = u_mk_con(&n_syntax_46_PTuple, 1, t462);
  }
  if (!t458) {
    t458 = v_p;
  }
  if (!t458) {
    u_no_case(s457);
  }
  return t458;
}

u_value f_check_46_zonk_stmt_2(u_value v_su, u_value v_x) {
  u_value s465 = v_x;
  u_value t466 = NULL;
  if ((!t466) && u_is_con(s465, &n_syntax_46_SSkip)) {
    t466 = v_x;
  }
  if ((!t466) && u_is_con(s465, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s465, 0);
    u_value t467 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e468 = &t467;
    u_value l469 = v_ps;
    while (u_is_con(l469, &n_list_46_Cons)) {
      u_value v_p = u_field(l469, 0);
      if (u_truth(u_mk_bool(1))) {
        e468 = u_append(e468, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l469 = u_field(l469, 1);
    }
    t466 = u_mk_con(&n_syntax_46_SAssign, 1, t467);
  }
  if ((!t466) && u_is_con(s465, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s465, 0);
    u_value v_ys = u_field(s465, 1);
    u_value t470 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e471 = &t470;
    u_value l472 = v_ys;
    while (u_is_con(l472, &n_list_46_Cons)) {
      u_value v_y = u_field(l472, 0);
      if (u_truth(u_mk_bool(1))) {
        e471 = u_append(e471, f_check_46_zonk_stmt_2(v_su, v_y));
      }
      l472 = u_field(l472, 1);
    }
    t466 = u_mk_con(&n_syntax_46_SBlock, 2, f_check_46_zonk_vars_2(v_su, v_xs), t470);
  }
  if ((!t466) && u_is_con(s465, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s465, 0);
    u_value v_l = u_field(s465, 1);
    u_value v_r = u_field(s465, 2);
    t466 = u_mk_con(&n_syntax_46_SIf, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_l), f_check_46_zonk_stmt_2(v_su, v_r));
  }
  if ((!t466) && u_is_con(s465, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s465, 0);
    u_value v_b = u_field(s465, 1);
    t466 = u_mk_con(&n_syntax_46_SWhile, 2, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t466) && u_is_con(s465, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s465, 0);
    u_value v_ins = u_field(s465, 1);
    u_value v_outs = u_field(s465, 2);
    u_value v_refs = u_field(s465, 3);
    u_value t473 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e474 = &t473;
    u_value l475 = v_ins;
    while (u_is_con(l475, &n_list_46_Cons)) {
      u_value v_e = u_field(l475, 0);
      if (u_truth(u_mk_bool(1))) {
        e474 = u_append(e474, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l475 = u_field(l475, 1);
    }
    t466 = u_mk_con(&n_syntax_46_SCall, 4, v_p, t473, f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs));
  }
  if ((!t466) && u_is_con(s465, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s465, 0);
    u_value v_bs = u_field(s465, 1);
    u_value t476 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e477 = &t476;
    u_value l478 = v_bs;
    while (u_is_con(l478, &n_list_46_Cons)) {
      u_value v_b = u_field(l478, 0);
      if (u_truth(u_mk_bool(1))) {
        e477 = u_append(e477, f_check_46_zonk_stmt_branch_2(v_su, v_b));
      }
      l478 = u_field(l478, 1);
    }
    t466 = u_mk_con(&n_syntax_46_SMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t476);
  }
  if ((!t466) && u_is_con(s465, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s465, 0);
    t466 = u_mk_con(&n_syntax_46_SAssert, 1, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t466) && u_is_con(s465, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s465, 0);
    u_value v_t = u_field(s465, 1);
    t466 = u_mk_con(&n_syntax_46_SRaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if ((!t466) && u_is_con(s465, &n_syntax_46_STry)) {
    u_value v_b = u_field(s465, 0);
    u_value v_hs = u_field(s465, 1);
    u_value t479 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e480 = &t479;
    u_value l481 = v_hs;
    while (u_is_con(l481, &n_list_46_Cons)) {
      u_value v_h = u_field(l481, 0);
      if (u_truth(u_mk_bool(1))) {
        e480 = u_append(e480, f_check_46_zonk_handler_2(v_su, v_h));
      }
      l481 = u_field(l481, 1);
    }
    t466 = u_mk_con(&n_syntax_46_STry, 2, f_check_46_zonk_stmt_2(v_su, v_b), t479);
  }
  if (!t466) {
    u_no_case(s465);
  }
  return t466;
}

u_value f_check_46_zonk_stmt_branch_2(u_value v_su, u_value v_b) {
  u_value s482 = v_b;
  u_value t483 = NULL;
  if ((!t483) && u_is_con(s482, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s482, 0);
    u_value v_g = u_field(s482, 1);
    u_value v_y = u_field(s482, 2);
    t483 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_y));
  }
  if (!t483) {
    u_no_case(s482);
  }
  return t483;
}

u_value f_check_46_zonk_handler_2(u_value v_su, u_value v_h) {
  u_value s484 = v_h;
  u_value t485 = NULL;
  if ((!t485) && u_is_con(s484, &n_syntax_46_Handler)) {
    u_value v_t = u_field(s484, 0);
    u_value v_p = u_field(s484, 1);
    u_value v_g = u_field(s484, 2);
    u_value v_b = u_field(s484, 3);
    t485 = u_mk_con(&n_syntax_46_Handler, 4, f_check_46_zonk_type_2(v_su, v_t), f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t485) {
    u_no_case(s484);
  }
  return t485;
}

u_value f_check_46_zonk_cmd_2(u_value v_su, u_value v_c) {
  u_value s486 = v_c;
  u_value t487 = NULL;
  if ((!t487) && u_is_con(s486, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s486, 0);
    u_value v_ps = u_field(s486, 1);
    u_value v_xss = u_field(s486, 2);
    u_value v_r = u_field(s486, 3);
    u_value v_b = u_field(s486, 4);
    u_value t488 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e489 = &t488;
    u_value l490 = v_xss;
    while (u_is_con(l490, &n_list_46_Cons)) {
      u_value v_xs = u_field(l490, 0);
      if (u_truth(u_mk_bool(1))) {
        e489 = u_append(e489, f_check_46_zonk_vars_2(v_su, v_xs));
      }
      l490 = u_field(l490, 1);
    }
    t487 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, t488, f_check_46_zonk_type_2(v_su, v_r), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t487) && u_is_con(s486, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s486, 0);
    u_value v_ps = u_field(s486, 1);
    u_value v_ins = u_field(s486, 2);
    u_value v_outs = u_field(s486, 3);
    u_value v_refs = u_field(s486, 4);
    u_value v_b = u_field(s486, 5);
    t487 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, f_check_46_zonk_vars_2(v_su, v_ins), f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t487) && u_is_con(s486, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s486, 0);
    t487 = u_mk_con(&n_syntax_46_CEval, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t487) && u_is_con(s486, &n_syntax_46_CRun)) {
    u_value v_n = u_field(s486, 0);
    u_value v_b = u_field(s486, 1);
    t487 = u_mk_con(&n_syntax_46_CRun, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t487) && u_is_con(s486, &n_syntax_46_CTest)) {
    u_value v_n = u_field(s486, 0);
    u_value v_b = u_field(s486, 1);
    t487 = u_mk_con(&n_syntax_46_CTest, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t487) {
    t487 = v_c;
  }
  if (!t487) {
    u_no_case(s486);
  }
  return t487;
}

u_value f_check_46_unknown_type_1(u_value v_t) {
  u_value t491;
  if (u_truth(u_eq(f_check_46_free_1(v_t), u_mk_con(&n_list_46_Nil, 0)))) {
    t491 = u_mk_con(&n_option_46_None, 0);
  } else {
    t491 = u_mk_con(&n_option_46_Some, 1, v_t);
  }
  return t491;
}

u_value f_check_46_unknown_in_1(u_value v_x) {
  u_value s492 = v_x;
  u_value t493 = NULL;
  if ((!t493) && u_is_con(s492, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s492, 0);
    t493 = f_check_46_unknown_in_exprs_1(f_list_46_seconds_1(v_ps));
  }
  if ((!t493) && u_is_con(s492, &n_syntax_46_SBlock)) {
    u_value v_ys = u_field(s492, 1);
    t493 = f_check_46_unknown_in_all_1(v_ys);
  }
  if ((!t493) && u_is_con(s492, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s492, 0);
    u_value v_l = u_field(s492, 1);
    u_value v_r = u_field(s492, 2);
    t493 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_option_46_or_else_2(f_check_46_unknown_in_1(v_l), f_check_46_unknown_in_1(v_r)));
  }
  if ((!t493) && u_is_con(s492, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s492, 0);
    u_value v_b = u_field(s492, 1);
    t493 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_check_46_unknown_in_1(v_b));
  }
  if ((!t493) && u_is_con(s492, &n_syntax_46_SCall)) {
    u_value v_ins = u_field(s492, 1);
    t493 = f_check_46_unknown_in_exprs_1(v_ins);
  }
  if ((!t493) && u_is_con(s492, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s492, 0);
    u_value v_bs = u_field(s492, 1);
    t493 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_a), f_check_46_unknown_in_branches_1(v_bs));
  }
  if ((!t493) && u_is_con(s492, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s492, 0);
    t493 = f_check_46_unknown_in_expr_1(v_c);
  }
  if ((!t493) && u_is_con(s492, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s492, 0);
    u_value v_t = u_field(s492, 1);
    t493 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t493) && u_is_con(s492, &n_syntax_46_STry)) {
    u_value v_b = u_field(s492, 0);
    u_value v_hs = u_field(s492, 1);
    t493 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_hs));
  }
  if (!t493) {
    t493 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t493) {
    u_no_case(s492);
  }
  return t493;
}

u_value f_check_46_unknown_in_expr_1(u_value v_e) {
  u_value s494 = v_e;
  u_value t495 = NULL;
  if ((!t495) && u_is_con(s494, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s494, 0);
    u_value v_t = u_field(s494, 1);
    t495 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t495) && u_is_con(s494, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s494, 0);
    u_value v_args = u_field(s494, 1);
    t495 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_f, v_args));
  }
  if ((!t495) && u_is_con(s494, &n_syntax_46_ELam)) {
    u_value v_b = u_field(s494, 1);
    t495 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t495) && u_is_con(s494, &n_syntax_46_EProc)) {
    u_value v_b = u_field(s494, 3);
    t495 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t495) && u_is_con(s494, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s494, 0);
    u_value v_b = u_field(s494, 1);
    t495 = f_check_46_unknown_in_exprs_1(f_list_46__43__43__2(f_list_46_seconds_1(v_ps), u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t495) && u_is_con(s494, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s494, 0);
    u_value v_l = u_field(s494, 1);
    u_value v_r = u_field(s494, 2);
    t495 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)))));
  }
  if ((!t495) && u_is_con(s494, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s494, 0);
    u_value v_bs = u_field(s494, 1);
    u_value t496 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e497 = &t496;
    u_value l498 = v_bs;
    while (u_is_con(l498, &n_list_46_Cons)) {
      u_value v_b = u_field(l498, 0);
      if (u_truth(u_mk_bool(1))) {
        e497 = u_append(e497, f_check_46_branch_exprs_1(v_b));
      }
      l498 = u_field(l498, 1);
    }
    t495 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_a, f_list_46_concat_1(t496)));
  }
  if ((!t495) && u_is_con(s494, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s494, 0);
    u_value v_ls = u_field(s494, 2);
    u_value v_c = u_field(s494, 3);
    t495 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Cons, 2, v_c, v_ls)));
  }
  if ((!t495) && u_is_con(s494, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s494, 0);
    t495 = f_check_46_unknown_in_exprs_1(v_es);
  }
  if (!t495) {
    t495 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t495) {
    u_no_case(s494);
  }
  return t495;
}

u_value f_check_46_unknown_in_exprs_1(u_value v_es) {
  u_value s499 = v_es;
  u_value t500 = NULL;
  if ((!t500) && u_is_con(s499, &n_list_46_Nil)) {
    t500 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t500) && u_is_con(s499, &n_list_46_Cons)) {
    u_value v_e = u_field(s499, 0);
    u_value v_rest = u_field(s499, 1);
    t500 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_e), f_check_46_unknown_in_exprs_1(v_rest));
  }
  if (!t500) {
    u_no_case(s499);
  }
  return t500;
}

u_value f_check_46_branch_exprs_1(u_value v_b) {
  u_value s501 = v_b;
  u_value t502 = NULL;
  if ((!t502) && (u_is_con(s501, &n_syntax_46_Branch) && u_is_con(u_field(s501, 1), &n_option_46_Some))) {
    u_value v_g = u_field(u_field(s501, 1), 0);
    u_value v_e = u_field(s501, 2);
    t502 = u_mk_con(&n_list_46_Cons, 2, v_g, u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t502) && (u_is_con(s501, &n_syntax_46_Branch) && u_is_con(u_field(s501, 1), &n_option_46_None))) {
    u_value v_e = u_field(s501, 2);
    t502 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t502) {
    u_no_case(s501);
  }
  return t502;
}

u_value f_check_46_unknown_in_all_1(u_value v_xs) {
  u_value s503 = v_xs;
  u_value t504 = NULL;
  if ((!t504) && u_is_con(s503, &n_list_46_Nil)) {
    t504 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t504) && u_is_con(s503, &n_list_46_Cons)) {
    u_value v_y = u_field(s503, 0);
    u_value v_ys = u_field(s503, 1);
    t504 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_all_1(v_ys));
  }
  if (!t504) {
    u_no_case(s503);
  }
  return t504;
}

u_value f_check_46_unknown_in_branches_1(u_value v_bs) {
  u_value s505 = v_bs;
  u_value t506 = NULL;
  if ((!t506) && u_is_con(s505, &n_list_46_Nil)) {
    t506 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t506) && (u_is_con(s505, &n_list_46_Cons) && (u_is_con(u_field(s505, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s505, 0), 1), &n_option_46_Some)))) {
    u_value v_g = u_field(u_field(u_field(s505, 0), 1), 0);
    u_value v_y = u_field(u_field(s505, 0), 2);
    u_value v_rest = u_field(s505, 1);
    t506 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_g), f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest)));
  }
  if ((!t506) && (u_is_con(s505, &n_list_46_Cons) && (u_is_con(u_field(s505, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s505, 0), 1), &n_option_46_None)))) {
    u_value v_y = u_field(u_field(s505, 0), 2);
    u_value v_rest = u_field(s505, 1);
    t506 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest));
  }
  if (!t506) {
    u_no_case(s505);
  }
  return t506;
}

u_value f_check_46_unknown_in_handlers_1(u_value v_hs) {
  u_value s507 = v_hs;
  u_value t508 = NULL;
  if ((!t508) && u_is_con(s507, &n_list_46_Nil)) {
    t508 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t508) && (u_is_con(s507, &n_list_46_Cons) && (u_is_con(u_field(s507, 0), &n_syntax_46_Handler) && u_is_con(u_field(u_field(s507, 0), 1), &n_syntax_46_PWild)))) {
    u_value v_b = u_field(u_field(s507, 0), 3);
    u_value v_rest = u_field(s507, 1);
    t508 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest));
  }
  if ((!t508) && (u_is_con(s507, &n_list_46_Cons) && u_is_con(u_field(s507, 0), &n_syntax_46_Handler))) {
    u_value v_t = u_field(u_field(s507, 0), 0);
    u_value v_b = u_field(u_field(s507, 0), 3);
    u_value v_rest = u_field(s507, 1);
    t508 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest)));
  }
  if (!t508) {
    u_no_case(s507);
  }
  return t508;
}

u_value f_check_46_unknown_in_cmd_1(u_value v_c) {
  u_value s509 = v_c;
  u_value t510 = NULL;
  if ((!t510) && u_is_con(s509, &n_syntax_46_CFun)) {
    u_value v_b = u_field(s509, 4);
    t510 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t510) && u_is_con(s509, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s509, 0);
    t510 = f_check_46_unknown_in_expr_1(v_e);
  }
  if ((!t510) && u_is_con(s509, &n_syntax_46_CProc)) {
    u_value v_b = u_field(s509, 5);
    t510 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t510) && u_is_con(s509, &n_syntax_46_CRun)) {
    u_value v_b = u_field(s509, 1);
    t510 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t510) && u_is_con(s509, &n_syntax_46_CTest)) {
    u_value v_b = u_field(s509, 1);
    t510 = f_check_46_unknown_in_1(v_b);
  }
  if (!t510) {
    t510 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t510) {
    u_no_case(s509);
  }
  return t510;
}

u_value f_check_46_head_of_1(u_value v_p) {
  u_value s511 = v_p;
  u_value t512 = NULL;
  if ((!t512) && u_is_con(s511, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s511, 0);
    u_value v_args = u_field(s511, 1);
    t512 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_n), v_args));
  }
  if ((!t512) && u_is_con(s511, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s511, 0);
    t512 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ps));
  }
  if ((!t512) && u_is_con(s511, &n_syntax_46_PLit)) {
    u_value v_v = u_field(s511, 0);
    t512 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, v_v), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t512) {
    t512 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t512) {
    u_no_case(s511);
  }
  return t512;
}

u_value f_check_46_heads_1(u_value v_rows) {
  u_value s513 = v_rows;
  u_value t514 = NULL;
  if ((!t514) && u_is_con(s513, &n_list_46_Nil)) {
    t514 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t514) && (u_is_con(s513, &n_list_46_Cons) && u_is_con(u_field(s513, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s513, 0), 0);
    u_value v_rest = u_field(s513, 1);
    u_value s515 = f_check_46_head_of_1(v_p);
    u_value t516 = NULL;
    if ((!t516) && u_is_con(s515, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s515, 0), 0);
      t516 = u_mk_con(&n_list_46_Cons, 2, v_h, f_check_46_heads_1(v_rest));
    }
    if ((!t516) && u_is_con(s515, &n_option_46_None)) {
      t516 = f_check_46_heads_1(v_rest);
    }
    if (!t516) {
      u_no_case(s515);
    }
    t514 = t516;
  }
  if ((!t514) && (u_is_con(s513, &n_list_46_Cons) && u_is_con(u_field(s513, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s513, 1);
    t514 = f_check_46_heads_1(v_rest);
  }
  if (!t514) {
    u_no_case(s513);
  }
  return t514;
}

u_value f_check_46_field_types_3(u_value v_c, u_value v_k, u_value v_args) {
  u_value s517 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t518 = NULL;
  if ((!t518) && u_is_con(s517, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s517, 0), 0);
    u_value v_ks = u_field(u_field(s517, 0), 1);
    u_value t519 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e520 = &t519;
    u_value l521 = v_ks;
    while (u_is_con(l521, &n_list_46_Cons)) {
      u_value v_a = u_field(l521, 0);
      if (u_truth(u_mk_bool(1))) {
        e520 = u_append(e520, f_check_46_subst_2(v_a, f_list_46_zip_2(v_ps, v_args)));
      }
      l521 = u_field(l521, 1);
    }
    t518 = t519;
  }
  if ((!t518) && u_is_con(s517, &n_option_46_None)) {
    t518 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t518) {
    u_no_case(s517);
  }
  return t518;
}

u_value f_check_46_constructors_of_3(u_value v_c, u_value v_su, u_value v_t) {
  u_value s522 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t523 = NULL;
  if ((!t523) && (u_is_con(s522, &n_syntax_46_TSort) && (u_is_con(u_field(s522, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s522, 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(s522, 0), 1), u_mk_lit("Bool")) && u_is_con(u_field(s522, 1), &n_list_46_Nil)))))) {
    t523 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t523) && u_is_con(s522, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s522, 0);
    t523 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ts), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t523) && u_is_con(s522, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s522, 0);
    u_value v_args = u_field(s522, 1);
    u_value s524 = f_ctx_46_members_of_2(v_c, v_n);
    u_value t525 = NULL;
    if ((!t525) && u_is_con(s524, &n_option_46_Some)) {
      u_value v_ks = u_field(s524, 0);
      u_value t526 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e527 = &t526;
      u_value l528 = v_ks;
      while (u_is_con(l528, &n_list_46_Cons)) {
        u_value v_k = u_field(l528, 0);
        if (u_truth(u_mk_bool(1))) {
          e527 = u_append(e527, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_k), f_check_46_field_types_3(v_c, v_k, v_args)));
        }
        l528 = u_field(l528, 1);
      }
      t525 = u_mk_con(&n_option_46_Some, 1, t526);
    }
    if ((!t525) && u_is_con(s524, &n_option_46_None)) {
      t525 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t525) {
      u_no_case(s524);
    }
    t523 = t525;
  }
  if (!t523) {
    t523 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t523) {
    u_no_case(s522);
  }
  return t523;
}

u_value f_check_46_fields_of_4(u_value v_c, u_value v_su, u_value v_t, u_value v_h) {
  u_value s529 = f_check_46_constructors_of_3(v_c, v_su, v_t);
  u_value t530 = NULL;
  if ((!t530) && u_is_con(s529, &n_option_46_Some)) {
    u_value v_ks = u_field(s529, 0);
    u_value s531 = f_list_46_lookup_2(v_h, v_ks);
    u_value t532 = NULL;
    if ((!t532) && u_is_con(s531, &n_option_46_Some)) {
      u_value v_ts = u_field(s531, 0);
      t532 = v_ts;
    }
    if ((!t532) && u_is_con(s531, &n_option_46_None)) {
      t532 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t532) {
      u_no_case(s531);
    }
    t530 = t532;
  }
  if ((!t530) && u_is_con(s529, &n_option_46_None)) {
    t530 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t530) {
    u_no_case(s529);
  }
  return t530;
}

u_value f_check_46_covers_2(u_value v_hs, u_value v_ks) {
  u_value s533 = v_ks;
  u_value t534 = NULL;
  if ((!t534) && u_is_con(s533, &n_list_46_Nil)) {
    t534 = u_mk_bool(1);
  }
  if ((!t534) && u_is_con(s533, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s533, 0), 0);
    u_value v_rest = u_field(s533, 1);
    u_value t535 = f_list_46_contains_2(v_h, v_hs);
    if (u_truth(t535)) {
      t535 = f_check_46_covers_2(v_hs, v_rest);
    }
    t534 = t535;
  }
  if (!t534) {
    u_no_case(s533);
  }
  return t534;
}

u_value f_check_46_defaults_1(u_value v_rows) {
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
      t539 = f_check_46_defaults_1(v_rest);
    }
    if ((!t539) && u_is_con(s538, &n_option_46_None)) {
      t539 = u_mk_con(&n_list_46_Cons, 2, v_ps, f_check_46_defaults_1(v_rest));
    }
    if (!t539) {
      u_no_case(s538);
    }
    t537 = t539;
  }
  if ((!t537) && (u_is_con(s536, &n_list_46_Cons) && u_is_con(u_field(s536, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s536, 1);
    t537 = f_check_46_defaults_1(v_rest);
  }
  if (!t537) {
    u_no_case(s536);
  }
  return t537;
}

u_value f_check_46_specialize_3(u_value v_rows, u_value v_h, u_value v_n) {
  u_value s540 = v_rows;
  u_value t541 = NULL;
  if ((!t541) && u_is_con(s540, &n_list_46_Nil)) {
    t541 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t541) && (u_is_con(s540, &n_list_46_Cons) && u_is_con(u_field(s540, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s540, 0), 0);
    u_value v_ps = u_field(u_field(s540, 0), 1);
    u_value v_rest = u_field(s540, 1);
    u_value s542 = f_check_46_head_of_1(v_p);
    u_value t543 = NULL;
    if ((!t543) && u_is_con(s542, &n_option_46_Some)) {
      u_value v_g = u_field(u_field(s542, 0), 0);
      u_value v_args = u_field(u_field(s542, 0), 1);
      u_value t544;
      if (u_truth(u_eq(v_g, v_h))) {
        t544 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(v_args, v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
      } else {
        t544 = f_check_46_specialize_3(v_rest, v_h, v_n);
      }
      t543 = t544;
    }
    if ((!t543) && u_is_con(s542, &n_option_46_None)) {
      t543 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(f_list_46_replicate_2(v_n, u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
    }
    if (!t543) {
      u_no_case(s542);
    }
    t541 = t543;
  }
  if ((!t541) && (u_is_con(s540, &n_list_46_Cons) && u_is_con(u_field(s540, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s540, 1);
    t541 = f_check_46_specialize_3(v_rest, v_h, v_n);
  }
  if (!t541) {
    u_no_case(s540);
  }
  return t541;
}

u_value f_check_46_useful_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_q, u_value v_ts) {
  u_value s545 = u_mk_tuple(2, v_q, v_ts);
  u_value t546 = NULL;
  if ((!t546) && u_is_con(u_field(s545, 0), &n_list_46_Nil)) {
    t546 = u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t546) && (u_is_con(u_field(s545, 0), &n_list_46_Cons) && u_is_con(u_field(s545, 1), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s545, 0), 0);
    u_value v_ps = u_field(u_field(s545, 0), 1);
    u_value v_t = u_field(u_field(s545, 1), 0);
    u_value v_rest = u_field(u_field(s545, 1), 1);
    u_value s547 = f_check_46_head_of_1(v_p);
    u_value t548 = NULL;
    if ((!t548) && u_is_con(s547, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s547, 0), 0);
      u_value v_args = u_field(u_field(s547, 0), 1);
      t548 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_ps), f_list_46__43__43__2(f_check_46_fields_of_4(v_c, v_su, v_t, v_h), v_rest));
    }
    if ((!t548) && u_is_con(s547, &n_option_46_None)) {
      u_value s549 = f_check_46_constructors_of_3(v_c, v_su, v_t);
      u_value t550 = NULL;
      if ((!t550) && u_is_con(s549, &n_option_46_Some)) {
        u_value v_ks = u_field(s549, 0);
        u_value t551;
        if (u_truth(f_check_46_covers_2(f_check_46_heads_1(v_rows), v_ks))) {
          t551 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_ks, v_ps, v_rest);
        } else {
          t551 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
        }
        t550 = t551;
      }
      if ((!t550) && u_is_con(s549, &n_option_46_None)) {
        t550 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
      }
      if (!t550) {
        u_no_case(s549);
      }
      t548 = t550;
    }
    if (!t548) {
      u_no_case(s547);
    }
    t546 = t548;
  }
  if ((!t546) && (u_is_con(u_field(s545, 0), &n_list_46_Cons) && u_is_con(u_field(s545, 1), &n_list_46_Nil))) {
    t546 = u_mk_bool(0);
  }
  if (!t546) {
    u_no_case(s545);
  }
  return t546;
}

u_value f_check_46_useful_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_ps, u_value v_rest) {
  u_value s552 = v_ks;
  u_value t553 = NULL;
  if ((!t553) && u_is_con(s552, &n_list_46_Nil)) {
    t553 = u_mk_bool(0);
  }
  if ((!t553) && u_is_con(s552, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s552, 0), 0);
    u_value v_args = u_field(u_field(s552, 0), 1);
    u_value v_more = u_field(s552, 1);
    u_value t554 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_list_46__43__43__2(v_args, v_rest));
    if (!u_truth(t554)) {
      t554 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_more, v_ps, v_rest);
    }
    t553 = t554;
  }
  if (!t553) {
    u_no_case(s552);
  }
  return t553;
}

u_value f_check_46_witness_2(u_value v_h, u_value v_args) {
  u_value s555 = v_h;
  u_value t556 = NULL;
  if ((!t556) && u_is_con(s555, &n_check_46_HCon)) {
    u_value v_n = u_field(s555, 0);
    u_value t557;
    if (u_truth(u_eq(v_n, f_syntax_46_tuple_name_0()))) {
      t557 = u_mk_con(&n_syntax_46_PTuple, 1, v_args);
    } else {
      t557 = u_mk_con(&n_syntax_46_PApp, 2, v_n, v_args);
    }
    t556 = t557;
  }
  if ((!t556) && u_is_con(s555, &n_check_46_HLit)) {
    u_value v_v = u_field(s555, 0);
    t556 = u_mk_con(&n_syntax_46_PLit, 1, v_v);
  }
  if (!t556) {
    u_no_case(s555);
  }
  return t556;
}

u_value f_check_46_missing_4(u_value v_c, u_value v_su, u_value v_rows, u_value v_ts) {
  u_value s558 = v_ts;
  u_value t559 = NULL;
  if ((!t559) && u_is_con(s558, &n_list_46_Nil)) {
    u_value t560;
    if (u_truth(u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0)))) {
      t560 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
    } else {
      t560 = u_mk_con(&n_option_46_None, 0);
    }
    t559 = t560;
  }
  if ((!t559) && u_is_con(s558, &n_list_46_Cons)) {
    u_value v_t = u_field(s558, 0);
    u_value v_rest = u_field(s558, 1);
    t559 = f_check_46_missing_in_6(v_c, v_su, v_rows, f_check_46_heads_1(v_rows), f_check_46_constructors_of_3(v_c, v_su, v_t), v_rest);
  }
  if (!t559) {
    u_no_case(s558);
  }
  return t559;
}

u_value f_check_46_missing_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s561 = v_cs;
  u_value t562 = NULL;
  if ((!t562) && u_is_con(s561, &n_option_46_Some)) {
    u_value v_ks = u_field(s561, 0);
    u_value t563;
    if (u_truth(f_check_46_covers_2(v_hs, v_ks))) {
      t563 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_ks, v_rest);
    } else {
      t563 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
    }
    t562 = t563;
  }
  if ((!t562) && u_is_con(s561, &n_option_46_None)) {
    t562 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
  }
  if (!t562) {
    u_no_case(s561);
  }
  return t562;
}

u_value f_check_46_first_missing_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_rest) {
  u_value s564 = v_ks;
  u_value t565 = NULL;
  if ((!t565) && u_is_con(s564, &n_list_46_Nil)) {
    t565 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t565) && u_is_con(s564, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s564, 0), 0);
    u_value v_args = u_field(u_field(s564, 0), 1);
    u_value v_more = u_field(s564, 1);
    u_value s566 = f_check_46_missing_4(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_rest));
    u_value t567 = NULL;
    if ((!t567) && u_is_con(s566, &n_option_46_Some)) {
      u_value v_ws = u_field(s566, 0);
      t567 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_witness_2(v_h, f_list_46_take_2(f_list_46_length_1(v_args), v_ws)), f_list_46_drop_2(f_list_46_length_1(v_args), v_ws)));
    }
    if ((!t567) && u_is_con(s566, &n_option_46_None)) {
      t567 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_more, v_rest);
    }
    if (!t567) {
      u_no_case(s566);
    }
    t565 = t567;
  }
  if (!t565) {
    u_no_case(s564);
  }
  return t565;
}

u_value f_check_46_other_missing_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s568 = f_check_46_missing_4(v_c, v_su, f_check_46_defaults_1(v_rows), v_rest);
  u_value t569 = NULL;
  if ((!t569) && u_is_con(s568, &n_option_46_Some)) {
    u_value v_ws = u_field(s568, 0);
    t569 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_uncovered_2(v_hs, v_cs), v_ws));
  }
  if ((!t569) && u_is_con(s568, &n_option_46_None)) {
    t569 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t569) {
    u_no_case(s568);
  }
  return t569;
}

u_value f_check_46_uncovered_2(u_value v_hs, u_value v_cs) {
  u_value s570 = v_cs;
  u_value t571 = NULL;
  if ((!t571) && u_is_con(s570, &n_option_46_Some)) {
    u_value v_ks = u_field(s570, 0);
    u_value t572;
    if (u_truth(u_eq(v_hs, u_mk_con(&n_list_46_Nil, 0)))) {
      t572 = u_mk_con(&n_syntax_46_PWild, 0);
    } else {
      t572 = f_check_46_first_uncovered_2(v_hs, v_ks);
    }
    t571 = t572;
  }
  if ((!t571) && u_is_con(s570, &n_option_46_None)) {
    t571 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if (!t571) {
    u_no_case(s570);
  }
  return t571;
}

u_value f_check_46_first_uncovered_2(u_value v_hs, u_value v_ks) {
  u_value s573 = v_ks;
  u_value t574 = NULL;
  if ((!t574) && u_is_con(s573, &n_list_46_Nil)) {
    t574 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if ((!t574) && u_is_con(s573, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s573, 0), 0);
    u_value v_args = u_field(u_field(s573, 0), 1);
    u_value v_rest = u_field(s573, 1);
    u_value t575;
    if (u_truth(f_list_46_contains_2(v_h, v_hs))) {
      t575 = f_check_46_first_uncovered_2(v_hs, v_rest);
    } else {
      t575 = f_check_46_witness_2(v_h, f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)));
    }
    t574 = t575;
  }
  if (!t574) {
    u_no_case(s573);
  }
  return t574;
}

void p_check_46_fresh(u_value *r_t, u_value *r_s) {
  {
    u_value t576 = u_mk_con(&n_syntax_46_TMeta, 1, u_add(f_check_46_State_46_count_1(*r_s), u_mk_int(1)));
    u_value t577 = f_check_46_State_46_count_58__61__2(*r_s, u_add(f_check_46_State_46_count_1(*r_s), u_mk_int(1)));
    *r_t = t576;
    *r_s = t577;
  }
}

void p_check_46_bind_meta(u_value v_i, u_value v_t, u_value *r_s) {
  {
    if (u_truth(f_check_46_occurs_3(f_check_46_State_46_sub_1(*r_s), v_i, v_t))) {
      {
        u_raise(u_mk_con(&n_check_46_InfiniteType, 2, u_mk_con(&n_syntax_46_TMeta, 1, v_i), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t)), "check.TypeError");
      }
    }
    u_value t578 = f_check_46_State_46_sub_58__61__2(*r_s, f_map_46_put_3(v_i, v_t, f_check_46_State_46_sub_1(*r_s)));
    *r_s = t578;
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
    if ((!d582) && (u_is_con(u_field(s581, 0), &n_syntax_46_TProc) && u_is_con(u_field(s581, 1), &n_syntax_46_TProc))) {
      u_value v_xs = u_field(u_field(s581, 0), 0);
      u_value v_ys = u_field(u_field(s581, 0), 1);
      u_value v_zs = u_field(u_field(s581, 0), 2);
      u_value v_us = u_field(u_field(s581, 1), 0);
      u_value v_vs = u_field(u_field(s581, 1), 1);
      u_value v_ws = u_field(u_field(s581, 1), 2);
      d582 = 1;
      {
        u_value t584 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_us));
        if (u_truth(t584)) {
          t584 = u_eq(f_list_46_length_1(v_ys), f_list_46_length_1(v_vs));
        }
        u_value t585 = t584;
        if (u_truth(t585)) {
          t585 = u_eq(f_list_46_length_1(v_zs), f_list_46_length_1(v_ws));
        }
        if (u_truth(t585)) {
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
    u_value s586 = u_mk_tuple(2, v_xs, v_ys);
    int d587 = 0;
    if ((!d587) && (u_is_con(u_field(s586, 0), &n_list_46_Cons) && u_is_con(u_field(s586, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s586, 0), 0);
      u_value v_xs1 = u_field(u_field(s586, 0), 1);
      u_value v_y = u_field(u_field(s586, 1), 0);
      u_value v_ys1 = u_field(u_field(s586, 1), 1);
      d587 = 1;
      {
        p_check_46_unify(v_x, v_y, r_s);
        p_check_46_unifies(v_xs1, v_ys1, r_s);
      }
    }
    if ((!d587) && (u_is_con(u_field(s586, 0), &n_list_46_Nil) && u_is_con(u_field(s586, 1), &n_list_46_Nil))) {
      d587 = 1;
      {
      }
    }
    if (!d587) {
      d587 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)), "check.TypeError");
      }
    }
    if (!d587) {
      u_no_case(s586);
    }
  }
}

void p_check_46_fresh_list(u_value v_xs, u_value *r_ts, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s588 = v_xs;
    int d589 = 0;
    if ((!d589) && u_is_con(s588, &n_list_46_Nil)) {
      d589 = 1;
      {
        u_value t590 = u_mk_con(&n_list_46_Nil, 0);
        *r_ts = t590;
      }
    }
    if ((!d589) && u_is_con(s588, &n_list_46_Cons)) {
      u_value v_more = u_field(s588, 1);
      d589 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_fresh_list(v_more, &v_rest, r_s);
        u_value t591 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rest);
        *r_ts = t591;
      }
    }
    if (!d589) {
      u_no_case(s588);
    }
  }
}

void p_check_46_instance(u_value v_ps, u_value *r_m, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s592 = v_ps;
    int d593 = 0;
    if ((!d593) && u_is_con(s592, &n_list_46_Nil)) {
      d593 = 1;
      {
        u_value t594 = u_mk_con(&n_list_46_Nil, 0);
        *r_m = t594;
      }
    }
    if ((!d593) && u_is_con(s592, &n_list_46_Cons)) {
      u_value v_p = u_field(s592, 0);
      u_value v_qs = u_field(s592, 1);
      d593 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_instance(v_qs, &v_rest, r_s);
        u_value t595 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_p, v_t), v_rest);
        *r_m = t595;
      }
    }
    if (!d593) {
      u_no_case(s592);
    }
  }
}

void p_check_46_instantiate(u_value v_sc, u_value *r_t, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value s596 = v_sc;
    int d597 = 0;
    if (!d597) {
      u_value v_ps = u_field(s596, 0);
      u_value v_u = u_field(s596, 1);
      d597 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        u_value t598 = f_check_46_subst_2(f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u), v_m);
        *r_t = t598;
      }
    }
    if (!d597) {
      u_no_case(s596);
    }
  }
}

void p_check_46_unique(u_value v_ns) {
  {
    u_value s599 = f_list_46_repeated_1(v_ns);
    int d600 = 0;
    if ((!d600) && u_is_con(s599, &n_option_46_Some)) {
      u_value v_n = u_field(s599, 0);
      d600 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, f_syntax_46_qualified_1(v_n)), "check.TypeError");
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

void p_check_46_unique_ids(u_value v_ns) {
  {
    u_value s601 = f_list_46_repeated_1(v_ns);
    int d602 = 0;
    if ((!d602) && u_is_con(s601, &n_option_46_Some)) {
      u_value v_n = u_field(s601, 0);
      d602 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, v_n), "check.TypeError");
      }
    }
    if ((!d602) && u_is_con(s601, &n_option_46_None)) {
      d602 = 1;
      {
      }
    }
    if (!d602) {
      u_no_case(s601);
    }
  }
}

void p_check_46_one_of(u_value v_n, u_value v_ns, u_value *r_m) {
  {
    u_value s603 = v_ns;
    int d604 = 0;
    if ((!d604) && (u_is_con(s603, &n_list_46_Cons) && u_is_con(u_field(s603, 1), &n_list_46_Nil))) {
      u_value v_k = u_field(s603, 0);
      d604 = 1;
      {
        u_value t605 = v_k;
        *r_m = t605;
      }
    }
    if ((!d604) && u_is_con(s603, &n_list_46_Nil)) {
      d604 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if (!d604) {
      d604 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ns), "check.TypeError");
      }
    }
    if (!d604) {
      u_no_case(s603);
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
    u_value s606 = v_t;
    int d607 = 0;
    if ((!d607) && u_is_con(s606, &n_syntax_46_THole)) {
      d607 = 1;
      {
        p_check_46_fresh(r_u, r_s);
      }
    }
    if ((!d607) && u_is_con(s606, &n_syntax_46_TParam)) {
      d607 = 1;
      {
        u_value t608 = v_t;
        *r_u = t608;
      }
    }
    if ((!d607) && u_is_con(s606, &n_syntax_46_TMeta)) {
      d607 = 1;
      {
        u_value t609 = v_t;
        *r_u = t609;
      }
    }
    if ((!d607) && u_is_con(s606, &n_syntax_46_TFun)) {
      u_value v_args = u_field(s606, 0);
      u_value v_r = u_field(s606, 1);
      d607 = 1;
      {
        p_check_46_normalizes(v_args, &v_us, r_s);
        p_check_46_normalize(v_r, &v_v, r_s);
        u_value t610 = u_mk_con(&n_syntax_46_TFun, 2, v_us, v_v);
        *r_u = t610;
      }
    }
    if ((!d607) && u_is_con(s606, &n_syntax_46_TTuple)) {
      u_value v_ts = u_field(s606, 0);
      d607 = 1;
      {
        p_check_46_normalizes(v_ts, &v_us, r_s);
        u_value t611 = u_mk_con(&n_syntax_46_TTuple, 1, v_us);
        *r_u = t611;
      }
    }
    if ((!d607) && u_is_con(s606, &n_syntax_46_TProc)) {
      u_value v_is = u_field(s606, 0);
      u_value v_os = u_field(s606, 1);
      u_value v_rs = u_field(s606, 2);
      d607 = 1;
      {
        p_check_46_normalizes(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs)), &v_us, r_s);
        u_value t612 = u_mk_con(&n_syntax_46_TProc, 3, f_list_46_take_2(f_list_46_length_1(v_is), v_us), f_list_46_take_2(f_list_46_length_1(v_os), f_list_46_drop_2(f_list_46_length_1(v_is), v_us)), f_list_46_drop_2(u_add(f_list_46_length_1(v_is), f_list_46_length_1(v_os)), v_us));
        *r_u = t612;
      }
    }
    if ((!d607) && u_is_con(s606, &n_syntax_46_TSort)) {
      u_value v_n = u_field(s606, 0);
      u_value v_args = u_field(s606, 1);
      d607 = 1;
      {
        if (u_truth(f_check_46_basic_1(v_n))) {
          {
            p_check_46_arguments(v_n, u_mk_int(0), v_args);
            u_value t613 = v_t;
            *r_u = t613;
          }
        } else {
          {
            u_value s615 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam614, 1, *r_s));
            int d616 = 0;
            if ((!d616) && (u_is_con(s615, &n_list_46_Cons) && u_is_con(u_field(s615, 1), &n_list_46_Nil))) {
              u_value v_m = u_field(s615, 0);
              d616 = 1;
              {
                p_check_46_normalize_sort(v_m, v_args, r_u, r_s);
              }
            }
            if ((!d616) && u_is_con(s615, &n_list_46_Nil)) {
              d616 = 1;
              {
                u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
              }
            }
            if (!d616) {
              u_value v_ms = s615;
              d616 = 1;
              {
                u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ms), "check.TypeError");
              }
            }
            if (!d616) {
              u_no_case(s615);
            }
          }
        }
      }
    }
    if (!d607) {
      u_no_case(s606);
    }
  }
}

void p_check_46_normalize_sort(u_value v_n, u_value v_args, u_value *r_u, u_value *r_s) {
  {
    u_value v_us = NULL;
    u_value s617 = f_ctx_46_data_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
    int d618 = 0;
    if ((!d618) && u_is_con(s617, &n_option_46_Some)) {
      u_value v_k = u_field(s617, 0);
      d618 = 1;
      {
        p_check_46_arguments(v_n, v_k, v_args);
        p_check_46_normalizes(v_args, &v_us, r_s);
        u_value t619 = u_mk_con(&n_syntax_46_TSort, 2, v_n, v_us);
        *r_u = t619;
      }
    }
    if ((!d618) && u_is_con(s617, &n_option_46_None)) {
      d618 = 1;
      {
        u_value s620 = f_ctx_46_alias_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
        int d621 = 0;
        if ((!d621) && u_is_con(s620, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s620, 0), 0);
          u_value v_rhs = u_field(u_field(s620, 0), 1);
          d621 = 1;
          {
            p_check_46_arguments(v_n, f_list_46_length_1(v_ps), v_args);
            p_check_46_normalizes(v_args, &v_us, r_s);
            u_value t622 = f_check_46_subst_2(v_rhs, f_list_46_zip_2(f_check_46_params_1(v_ps), v_us));
            *r_u = t622;
          }
        }
        if ((!d621) && u_is_con(s620, &n_option_46_None)) {
          d621 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
          }
        }
        if (!d621) {
          u_no_case(s620);
        }
      }
    }
    if (!d618) {
      u_no_case(s617);
    }
  }
}

void p_check_46_normalizes(u_value v_ts, u_value *r_us, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_rest = NULL;
    u_value s623 = v_ts;
    int d624 = 0;
    if ((!d624) && u_is_con(s623, &n_list_46_Nil)) {
      d624 = 1;
      {
        u_value t625 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t625;
      }
    }
    if ((!d624) && u_is_con(s623, &n_list_46_Cons)) {
      u_value v_t = u_field(s623, 0);
      u_value v_more = u_field(s623, 1);
      d624 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalizes(v_more, &v_rest, r_s);
        u_value t626 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t626;
      }
    }
    if (!d624) {
      u_no_case(s623);
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
    u_value s627 = v_ts;
    int d628 = 0;
    if ((!d628) && u_is_con(s627, &n_list_46_Nil)) {
      d628 = 1;
      {
        u_value t629 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t629;
      }
    }
    if ((!d628) && u_is_con(s627, &n_list_46_Cons)) {
      u_value v_t = u_field(s627, 0);
      u_value v_more = u_field(s627, 1);
      d628 = 1;
      {
        p_check_46_known(v_t, v_what, &v_u, r_s);
        p_check_46_knowns(v_more, v_what, &v_rest, r_s);
        u_value t630 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t630;
      }
    }
    if (!d628) {
      u_no_case(s627);
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
    u_value s631 = v_xs;
    int d632 = 0;
    if ((!d632) && u_is_con(s631, &n_list_46_Nil)) {
      d632 = 1;
      {
        u_value t633 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t633;
      }
    }
    if ((!d632) && (u_is_con(s631, &n_list_46_Cons) && u_is_con(u_field(s631, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s631, 0), 0);
      u_value v_t = u_field(u_field(s631, 0), 1);
      u_value v_more = u_field(s631, 1);
      d632 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalize_vars(v_more, &v_rest, r_s);
        u_value t634 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t634;
      }
    }
    if (!d632) {
      u_no_case(s631);
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
    u_value s635 = v_xs;
    int d636 = 0;
    if ((!d636) && u_is_con(s635, &n_list_46_Nil)) {
      d636 = 1;
      {
        u_value t637 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t637;
      }
    }
    if ((!d636) && (u_is_con(s635, &n_list_46_Cons) && u_is_con(u_field(s635, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s635, 0), 0);
      u_value v_t = u_field(u_field(s635, 0), 1);
      u_value v_more = u_field(s635, 1);
      d636 = 1;
      {
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_known_vars(v_more, &v_rest, r_s);
        u_value t638 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t638;
      }
    }
    if (!d636) {
      u_no_case(s635);
    }
  }
}

void p_check_46_resolve_name(u_value v_s, u_value v_env, u_value v_n, u_value v_k, u_value *r_m, u_value *r_sc) {
  {
    u_value s639 = f_check_46_candidates_3(v_s, v_env, v_n);
    int d640 = 0;
    if ((!d640) && u_is_con(s639, &n_list_46_Nil)) {
      d640 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if ((!d640) && (u_is_con(s639, &n_list_46_Cons) && u_is_con(u_field(s639, 1), &n_list_46_Nil))) {
      u_value v_m1 = u_field(u_field(s639, 0), 0);
      u_value v_one = u_field(u_field(s639, 0), 1);
      d640 = 1;
      {
        u_value t641 = v_m1;
        u_value t642 = v_one;
        *r_m = t641;
        *r_sc = t642;
      }
    }
    if (!d640) {
      u_value v_cs = s639;
      d640 = 1;
      {
        if (u_truth(u_lt(v_k, u_mk_int(0)))) {
          {
            u_raise(u_mk_con(&n_check_46_OverloadedUnapplied, 1, v_n), "check.TypeError");
          }
        }
        u_value s643 = f_check_46_with_arity_2(v_cs, v_k);
        int d644 = 0;
        if ((!d644) && (u_is_con(s643, &n_list_46_Cons) && u_is_con(u_field(s643, 1), &n_list_46_Nil))) {
          u_value v_m1 = u_field(u_field(s643, 0), 0);
          u_value v_one = u_field(u_field(s643, 0), 1);
          d644 = 1;
          {
            u_value t645 = v_m1;
            u_value t646 = v_one;
            *r_m = t645;
            *r_sc = t646;
          }
        }
        if ((!d644) && u_is_con(s643, &n_list_46_Nil)) {
          d644 = 1;
          {
            u_raise(u_mk_con(&n_check_46_WrongArity, 2, v_n, v_k), "check.TypeError");
          }
        }
        if (!d644) {
          u_value v_more = s643;
          d644 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, f_list_46_firsts_1(v_more)), "check.TypeError");
          }
        }
        if (!d644) {
          u_no_case(s643);
        }
      }
    }
    if (!d640) {
      u_no_case(s639);
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
    u_value t647 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_u);
    *r_x = t647;
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
    u_value s648 = v_e;
    int d649 = 0;
    if ((!d649) && u_is_con(s648, &n_syntax_46_ELit)) {
      u_value v_v = u_field(s648, 0);
      d649 = 1;
      {
        u_value t650 = v_e;
        u_value t651 = f_check_46_literal_1(v_v);
        *r_e1 = t650;
        *r_t = t651;
      }
    }
    if ((!d649) && (u_is_con(s648, &n_syntax_46_EVar) && u_is_con(u_field(s648, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s648, 0), 0);
      u_value v_a = u_field(u_field(s648, 0), 1);
      d649 = 1;
      {
        p_check_46_resolve_name(*r_s, v_env, v_n, u_mk_int(-1), &v_m, &v_sc);
        p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
        u_value t652 = u_mk_con(&n_syntax_46_EVar, 1, v_x);
        u_value t653 = f_syntax_46_Var_46_typ_1(v_x);
        *r_e1 = t652;
        *r_t = t653;
      }
    }
    if ((!d649) && u_is_con(s648, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s648, 0);
      u_value v_args = u_field(s648, 1);
      d649 = 1;
      {
        p_check_46_application(v_env, v_f, v_args, r_e1, r_t, r_s);
      }
    }
    if ((!d649) && u_is_con(s648, &n_syntax_46_ELam)) {
      u_value v_bound = u_field(s648, 0);
      u_value v_body = u_field(s648, 1);
      d649 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t654 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, v_b);
        u_value t655 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), v_u);
        *r_e1 = t654;
        *r_t = t655;
      }
    }
    if ((!d649) && u_is_con(s648, &n_syntax_46_EProc)) {
      u_value v_is = u_field(s648, 0);
      u_value v_os = u_field(s648, 1);
      u_value v_rs = u_field(s648, 2);
      u_value v_body = u_field(s648, 3);
      d649 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))));
        p_check_46_formals(v_is, &v_xs, r_s);
        p_check_46_formals(v_os, &v_ys, r_s);
        p_check_46_formals(v_rs, &v_zs, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(f_list_46__43__43__2(v_xs, f_list_46__43__43__2(v_ys, v_zs))), v_env), f_syntax_46_names_1(f_list_46__43__43__2(v_ys, v_zs)), v_body, &v_y, r_s);
        u_value t656 = u_mk_con(&n_syntax_46_EProc, 4, v_xs, v_ys, v_zs, v_y);
        u_value t657 = u_mk_con(&n_syntax_46_TProc, 3, f_syntax_46_types_1(v_xs), f_syntax_46_types_1(v_ys), f_syntax_46_types_1(v_zs));
        *r_e1 = t656;
        *r_t = t657;
      }
    }
    if ((!d649) && u_is_con(s648, &n_syntax_46_ELet)) {
      u_value v_bound = u_field(s648, 0);
      u_value v_body = u_field(s648, 1);
      d649 = 1;
      {
        p_check_46_formals(f_list_46_firsts_1(v_bound), &v_xs, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_bound), f_syntax_46_types_1(v_xs), &v_es, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_generalized_3(f_check_46_State_46_sub_1(*r_s), v_env, v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t658 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        u_value t659 = v_u;
        *r_e1 = t658;
        *r_t = t659;
      }
    }
    if ((!d649) && u_is_con(s648, &n_syntax_46_EIte)) {
      u_value v_cond = u_field(s648, 0);
      u_value v_left = u_field(s648, 1);
      u_value v_right = u_field(s648, 2);
      d649 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(v_env, v_left, &v_l, &v_u, r_s);
        p_check_46_check(v_env, v_right, v_u, &v_b, r_s);
        u_value t660 = u_mk_con(&n_syntax_46_EIte, 3, v_c, v_l, v_b);
        u_value t661 = v_u;
        *r_e1 = t660;
        *r_t = t661;
      }
    }
    if ((!d649) && u_is_con(s648, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s648, 0);
      u_value v_bound = u_field(s648, 1);
      u_value v_lists = u_field(s648, 2);
      u_value v_cond = u_field(s648, 3);
      d649 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_same_length(f_list_46_length_1(v_xs), f_list_46_length_1(v_lists));
        p_check_46_generators(v_env, v_xs, v_lists, &v_es, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_result, &v_b, &v_u, r_s);
        u_value t662 = u_mk_con(&n_syntax_46_EComp, 4, v_b, v_xs, v_es, v_c);
        u_value t663 = f_check_46_tlist_1(v_u);
        *r_e1 = t662;
        *r_t = t663;
      }
    }
    if ((!d649) && u_is_con(s648, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s648, 0);
      d649 = 1;
      {
        p_check_46_infers(v_env, v_items, &v_es, &v_ts, r_s);
        u_value t664 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
        u_value t665 = u_mk_con(&n_syntax_46_TTuple, 1, v_ts);
        *r_e1 = t664;
        *r_t = t665;
      }
    }
    if ((!d649) && u_is_con(s648, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s648, 0);
      u_value v_a = u_field(s648, 1);
      d649 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_normalize(v_a, &v_w, r_s);
        p_check_46_unify(v_u, v_w, r_s);
        p_check_46_fresh(r_t, r_s);
        u_value t666 = u_mk_con(&n_syntax_46_ERaise, 2, v_c, v_u);
        *r_e1 = t666;
      }
    }
    if ((!d649) && u_is_con(s648, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s648, 0);
      u_value v_cases = u_field(s648, 1);
      d649 = 1;
      {
        p_check_46_fresh(r_t, r_s);
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_expr_branches(v_env, v_cases, v_u, *r_t, &v_bs, r_s);
        u_value t667 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e668 = &t667;
        u_value l669 = v_bs;
        while (u_is_con(l669, &n_list_46_Cons)) {
          u_value v_br = u_field(l669, 0);
          if (u_truth(u_mk_bool(1))) {
            e668 = u_append(e668, f_check_46_case_of_1(v_br));
          }
          l669 = u_field(l669, 1);
        }
        p_check_46_coverage(*r_s, v_u, t667);
        u_value t670 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, v_bs);
        *r_e1 = t670;
      }
    }
    if (!d649) {
      u_no_case(s648);
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
    u_value s671 = v_f;
    int d672 = 0;
    if ((!d672) && (u_is_con(s671, &n_syntax_46_EVar) && (u_is_con(u_field(s671, 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(s671, 0), 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(u_field(s671, 0), 0), 0), &n_option_46_None))))) {
      u_value v_b = u_field(u_field(u_field(s671, 0), 0), 1);
      u_value v_a = u_field(u_field(s671, 0), 1);
      u_value t673 = f_prelude_46__62__2(u_str_size(v_b), u_mk_int(1));
      if (u_truth(t673)) {
        t673 = u_eq(u_str_get(v_b, u_mk_int(0)), u_mk_char(46));
      }
      if (u_truth(t673)) {
        d672 = 1;
        {
          p_check_46_field_access(v_env, v_b, v_a, v_args, r_e1, r_t, r_s);
        }
      }
    }
    if ((!d672) && (u_is_con(s671, &n_syntax_46_EVar) && u_is_con(u_field(s671, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s671, 0), 0);
      u_value v_a = u_field(u_field(s671, 0), 1);
      d672 = 1;
      {
        if (u_truth(f_prelude_46__62__2(f_list_46_length_1(f_check_46_candidates_3(*r_s, v_env, v_n)), u_mk_int(1)))) {
          {
            p_check_46_resolve_name(*r_s, v_env, v_n, f_list_46_length_1(v_args), &v_m, &v_sc);
            p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
            p_check_46_apply(v_env, f_syntax_46_Var_46_typ_1(v_x), v_args, &v_args1, r_t, r_s);
            u_value t674 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), v_args1);
            *r_e1 = t674;
          }
        } else {
          {
            p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
            p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
            u_value t675 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
            *r_e1 = t675;
          }
        }
      }
    }
    if (!d672) {
      d672 = 1;
      {
        p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
        p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
        u_value t676 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
        *r_e1 = t676;
      }
    }
    if (!d672) {
      u_no_case(s671);
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
    u_value s677 = v_args;
    int d678 = 0;
    if ((!d678) && u_is_con(s677, &n_list_46_Cons)) {
      u_value v_arg = u_field(s677, 0);
      u_value v_rest = u_field(s677, 1);
      d678 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_arg1, &v_u, r_s);
        u_value s679 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u);
        int d680 = 0;
        if ((!d680) && (u_is_con(s679, &n_syntax_46_TSort) && (u_is_con(u_field(s679, 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(s679, 0), 0), &n_option_46_Some)))) {
          u_value v_m = u_field(u_field(u_field(s679, 0), 0), 0);
          u_value v_tn = u_field(u_field(s679, 0), 1);
          d680 = 1;
          {
            u_value t681 = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), u_concat(v_tn, v_f));
            v_acc = t681;
          }
        }
        if (!d680) {
          u_value v_v = s679;
          d680 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownFieldType, 2, v_f, v_v), "check.TypeError");
          }
        }
        if (!d680) {
          u_no_case(s679);
        }
        u_value s682 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(*r_s), v_acc);
        int d683 = 0;
        if ((!d683) && (u_is_con(s682, &n_list_46_Cons) && u_is_con(u_field(s682, 1), &n_list_46_Nil))) {
          u_value v_sc = u_field(s682, 0);
          d683 = 1;
          {
            p_check_46_use(v_acc, v_a, v_sc, &v_x, r_s);
          }
        }
        if (!d683) {
          d683 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownField, 1, v_acc), "check.TypeError");
          }
        }
        if (!d683) {
          u_no_case(s682);
        }
        p_check_46_infers(v_env, v_rest, &v_rest1, &v_us, r_s);
        p_check_46_fresh(r_t, r_s);
        p_check_46_unify(f_syntax_46_Var_46_typ_1(v_x), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_u, v_us), *r_t), r_s);
        u_value t684 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), u_mk_con(&n_list_46_Cons, 2, v_arg1, v_rest1));
        *r_e1 = t684;
      }
    }
    if (!d678) {
      d678 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, u_mk_int(1), f_list_46_length_1(v_args)), "check.TypeError");
      }
    }
    if (!d678) {
      u_no_case(s677);
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
    u_value s685 = u_mk_tuple(2, v_es, v_ts);
    int d686 = 0;
    if ((!d686) && (u_is_con(u_field(s685, 0), &n_list_46_Cons) && u_is_con(u_field(s685, 1), &n_list_46_Cons))) {
      u_value v_e = u_field(u_field(s685, 0), 0);
      u_value v_more = u_field(u_field(s685, 0), 1);
      u_value v_t = u_field(u_field(s685, 1), 0);
      u_value v_ts1 = u_field(u_field(s685, 1), 1);
      d686 = 1;
      {
        p_check_46_check(v_env, v_e, v_t, &v_e1, r_s);
        p_check_46_checks(v_env, v_more, v_ts1, &v_rest, r_s);
        u_value t687 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        *r_es1 = t687;
      }
    }
    if ((!d686) && (u_is_con(u_field(s685, 0), &n_list_46_Nil) && u_is_con(u_field(s685, 1), &n_list_46_Nil))) {
      d686 = 1;
      {
        u_value t688 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t688;
      }
    }
    if (!d686) {
      d686 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_es)), "check.TypeError");
      }
    }
    if (!d686) {
      u_no_case(s685);
    }
  }
}

void p_check_46_infers(u_value v_env, u_value v_es, u_value *r_es1, u_value *r_ts, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value v_us = NULL;
    u_value s689 = v_es;
    int d690 = 0;
    if ((!d690) && u_is_con(s689, &n_list_46_Nil)) {
      d690 = 1;
      {
        u_value t691 = u_mk_con(&n_list_46_Nil, 0);
        u_value t692 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t691;
        *r_ts = t692;
      }
    }
    if ((!d690) && u_is_con(s689, &n_list_46_Cons)) {
      u_value v_e = u_field(s689, 0);
      u_value v_more = u_field(s689, 1);
      d690 = 1;
      {
        p_check_46_infer(v_env, v_e, &v_e1, &v_t, r_s);
        p_check_46_infers(v_env, v_more, &v_rest, &v_us, r_s);
        u_value t693 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        u_value t694 = u_mk_con(&n_list_46_Cons, 2, v_t, v_us);
        *r_es1 = t693;
        *r_ts = t694;
      }
    }
    if (!d690) {
      u_no_case(s689);
    }
  }
}

void p_check_46_generators(u_value v_env, u_value v_xs, u_value v_lists, u_value *r_lists1, u_value *r_s) {
  {
    u_value v_l = NULL;
    u_value v_rest = NULL;
    u_value s695 = u_mk_tuple(2, v_xs, v_lists);
    int d696 = 0;
    if ((!d696) && (u_is_con(u_field(s695, 0), &n_list_46_Cons) && u_is_con(u_field(s695, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s695, 0), 0);
      u_value v_ys = u_field(u_field(s695, 0), 1);
      u_value v_e = u_field(u_field(s695, 1), 0);
      u_value v_es = u_field(u_field(s695, 1), 1);
      d696 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tlist_1(f_syntax_46_Var_46_typ_1(v_x)), &v_l, r_s);
        p_check_46_generators(f_list_46__43__43__2(f_check_46_bind_1(u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0))), v_env), v_ys, v_es, &v_rest, r_s);
        u_value t697 = u_mk_con(&n_list_46_Cons, 2, v_l, v_rest);
        *r_lists1 = t697;
      }
    }
    if (!d696) {
      d696 = 1;
      {
        u_value t698 = u_mk_con(&n_list_46_Nil, 0);
        *r_lists1 = t698;
      }
    }
    if (!d696) {
      u_no_case(s695);
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
    u_value s699 = v_cases;
    int d700 = 0;
    if ((!d700) && u_is_con(s699, &n_list_46_Nil)) {
      d700 = 1;
      {
        u_value t701 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t701;
      }
    }
    if ((!d700) && (u_is_con(s699, &n_list_46_Cons) && u_is_con(u_field(s699, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s699, 0), 0);
      u_value v_g = u_field(u_field(s699, 0), 1);
      u_value v_b = u_field(u_field(s699, 0), 2);
      u_value v_more = u_field(s699, 1);
      d700 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_b, v_r, &v_b1, r_s);
        p_check_46_expr_branches(v_env, v_more, v_t, v_r, &v_rest, r_s);
        u_value t702 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t702;
      }
    }
    if (!d700) {
      u_no_case(s699);
    }
  }
}

void p_check_46_guard(u_value v_env, u_value v_g, u_value *r_g1, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value s703 = v_g;
    int d704 = 0;
    if ((!d704) && u_is_con(s703, &n_option_46_Some)) {
      u_value v_e = u_field(s703, 0);
      d704 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tbool_0(), &v_e1, r_s);
        u_value t705 = u_mk_con(&n_option_46_Some, 1, v_e1);
        *r_g1 = t705;
      }
    }
    if ((!d704) && u_is_con(s703, &n_option_46_None)) {
      d704 = 1;
      {
        u_value t706 = u_mk_con(&n_option_46_None, 0);
        *r_g1 = t706;
      }
    }
    if (!d704) {
      u_no_case(s703);
    }
  }
}

void p_check_46_coverage(u_value v_s, u_value v_t, u_value v_cs) {
  {
    p_check_46_reachable(v_s, v_t, u_mk_con(&n_list_46_Nil, 0), v_cs);
    u_value t707 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e708 = &t707;
    u_value l709 = f_check_46_unguarded_1(v_cs);
    while (u_is_con(l709, &n_list_46_Cons)) {
      u_value v_p = u_field(l709, 0);
      if (u_truth(u_mk_bool(1))) {
        e708 = u_append(e708, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
      }
      l709 = u_field(l709, 1);
    }
    u_value s710 = f_check_46_missing_4(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t707, u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
    int d711 = 0;
    if ((!d711) && (u_is_con(s710, &n_option_46_Some) && u_is_con(u_field(s710, 0), &n_list_46_Cons))) {
      u_value v_w = u_field(u_field(s710, 0), 0);
      d711 = 1;
      {
        u_raise(u_mk_con(&n_check_46_NotExhaustive, 1, v_w), "check.TypeError");
      }
    }
    if (!d711) {
      d711 = 1;
      {
      }
    }
    if (!d711) {
      u_no_case(s710);
    }
  }
}

u_value f_check_46_unguarded_1(u_value v_cs) {
  u_value s712 = v_cs;
  u_value t713 = NULL;
  if ((!t713) && u_is_con(s712, &n_list_46_Nil)) {
    t713 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t713) && (u_is_con(s712, &n_list_46_Cons) && u_equals(u_field(u_field(s712, 0), 1), u_mk_bool(1)))) {
    u_value v_p = u_field(u_field(s712, 0), 0);
    u_value v_rest = u_field(s712, 1);
    t713 = u_mk_con(&n_list_46_Cons, 2, v_p, f_check_46_unguarded_1(v_rest));
  }
  if ((!t713) && (u_is_con(s712, &n_list_46_Cons) && u_equals(u_field(u_field(s712, 0), 1), u_mk_bool(0)))) {
    u_value v_rest = u_field(s712, 1);
    t713 = f_check_46_unguarded_1(v_rest);
  }
  if (!t713) {
    u_no_case(s712);
  }
  return t713;
}

void p_check_46_reachable(u_value v_s, u_value v_t, u_value v_before, u_value v_cs) {
  {
    u_value s714 = v_cs;
    int d715 = 0;
    if ((!d715) && u_is_con(s714, &n_list_46_Nil)) {
      d715 = 1;
      {
      }
    }
    if ((!d715) && u_is_con(s714, &n_list_46_Cons)) {
      u_value v_p = u_field(u_field(s714, 0), 0);
      u_value v_open = u_field(u_field(s714, 0), 1);
      u_value v_rest = u_field(s714, 1);
      d715 = 1;
      {
        u_value t716 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e717 = &t716;
        u_value l718 = v_before;
        while (u_is_con(l718, &n_list_46_Cons)) {
          u_value v_q = u_field(l718, 0);
          if (u_truth(u_mk_bool(1))) {
            e717 = u_append(e717, u_mk_con(&n_list_46_Cons, 2, v_q, u_mk_con(&n_list_46_Nil, 0)));
          }
          l718 = u_field(l718, 1);
        }
        if (u_truth(f_prelude_46_not_1(f_check_46_useful_5(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t716, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)))))) {
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
    if (!d715) {
      u_no_case(s714);
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
    u_value s719 = v_p;
    int d720 = 0;
    if ((!d720) && u_is_con(s719, &n_syntax_46_PWild)) {
      d720 = 1;
      {
        u_value t721 = v_p;
        u_value t722 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t721;
        *r_xs = t722;
      }
    }
    if ((!d720) && u_is_con(s719, &n_syntax_46_PLit)) {
      u_value v_v = u_field(s719, 0);
      d720 = 1;
      {
        p_check_46_unify(f_check_46_literal_1(v_v), v_t, r_s);
        u_value t723 = v_p;
        u_value t724 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t723;
        *r_xs = t724;
      }
    }
    if ((!d720) && (u_is_con(s719, &n_syntax_46_PVar) && u_is_con(u_field(s719, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s719, 0), 0);
      u_value v_a = u_field(u_field(s719, 0), 1);
      d720 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam725, 1, *r_s)), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            p_check_46_binders(u_mk_con(&n_syntax_46_PApp, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_t, r_p1, r_xs, r_s);
          }
        } else {
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_u, v_t, r_s);
            u_value t726 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u));
            u_value t727 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), u_mk_con(&n_list_46_Nil, 0));
            *r_p1 = t726;
            *r_xs = t727;
          }
        }
      }
    }
    if ((!d720) && u_is_con(s719, &n_syntax_46_PApp)) {
      u_value v_n = u_field(s719, 0);
      u_value v_args = u_field(s719, 1);
      d720 = 1;
      {
        u_value s729 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam728, 1, *r_s));
        int d730 = 0;
        if ((!d730) && (u_is_con(s729, &n_list_46_Cons) && u_is_con(u_field(s729, 1), &n_list_46_Nil))) {
          u_value v_k1 = u_field(s729, 0);
          d730 = 1;
          {
            u_value t731 = v_k1;
            v_k = t731;
          }
        }
        if ((!d730) && u_is_con(s729, &n_list_46_Nil)) {
          d730 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d730) {
          u_value v_ks = s729;
          d730 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ks), "check.TypeError");
          }
        }
        if (!d730) {
          u_no_case(s729);
        }
        u_value s732 = f_ctx_46_constr_of_2(f_check_46_State_46_ctx_1(*r_s), v_k);
        int d733 = 0;
        if ((!d733) && u_is_con(s732, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s732, 0), 0);
          u_value v_ks = u_field(u_field(s732, 0), 1);
          u_value v_r = u_field(u_field(s732, 0), 2);
          d733 = 1;
          {
            p_check_46_same_length(f_list_46_length_1(v_ks), f_list_46_length_1(v_args));
            p_check_46_instance(v_ps, &v_m, r_s);
            p_check_46_unify(f_check_46_subst_2(v_r, v_m), v_t, r_s);
            u_value t734 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e735 = &t734;
            u_value l736 = v_ks;
            while (u_is_con(l736, &n_list_46_Cons)) {
              u_value v_kt = u_field(l736, 0);
              if (u_truth(u_mk_bool(1))) {
                e735 = u_append(e735, f_check_46_subst_2(v_kt, v_m));
              }
              l736 = u_field(l736, 1);
            }
            p_check_46_binder_list(v_args, t734, &v_qs, r_xs, r_s);
            u_value t737 = u_mk_con(&n_syntax_46_PApp, 2, v_k, v_qs);
            *r_p1 = t737;
          }
        }
        if ((!d733) && u_is_con(s732, &n_option_46_None)) {
          d733 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d733) {
          u_no_case(s732);
        }
      }
    }
    if ((!d720) && u_is_con(s719, &n_syntax_46_PTuple)) {
      u_value v_items = u_field(s719, 0);
      d720 = 1;
      {
        p_check_46_fresh_list(v_items, &v_ts, r_s);
        p_check_46_unify(u_mk_con(&n_syntax_46_TTuple, 1, v_ts), v_t, r_s);
        p_check_46_binder_list(v_items, v_ts, &v_qs, r_xs, r_s);
        u_value t738 = u_mk_con(&n_syntax_46_PTuple, 1, v_qs);
        *r_p1 = t738;
      }
    }
    if (!d720) {
      u_no_case(s719);
    }
  }
}

void p_check_46_binder_list(u_value v_ps, u_value v_ts, u_value *r_qs, u_value *r_xs, u_value *r_s) {
  {
    u_value v_q = NULL;
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value v_zs = NULL;
    u_value s739 = u_mk_tuple(2, v_ps, v_ts);
    int d740 = 0;
    if ((!d740) && (u_is_con(u_field(s739, 0), &n_list_46_Cons) && u_is_con(u_field(s739, 1), &n_list_46_Cons))) {
      u_value v_p = u_field(u_field(s739, 0), 0);
      u_value v_more = u_field(u_field(s739, 0), 1);
      u_value v_t = u_field(u_field(s739, 1), 0);
      u_value v_ts1 = u_field(u_field(s739, 1), 1);
      d740 = 1;
      {
        p_check_46_binders(v_p, v_t, &v_q, &v_ys, r_s);
        p_check_46_binder_list(v_more, v_ts1, &v_rest, &v_zs, r_s);
        u_value t741 = u_mk_con(&n_list_46_Cons, 2, v_q, v_rest);
        u_value t742 = f_list_46__43__43__2(v_ys, v_zs);
        *r_qs = t741;
        *r_xs = t742;
      }
    }
    if ((!d740) && (u_is_con(u_field(s739, 0), &n_list_46_Nil) && u_is_con(u_field(s739, 1), &n_list_46_Nil))) {
      d740 = 1;
      {
        u_value t743 = u_mk_con(&n_list_46_Nil, 0);
        u_value t744 = u_mk_con(&n_list_46_Nil, 0);
        *r_qs = t743;
        *r_xs = t744;
      }
    }
    if (!d740) {
      d740 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_ps)), "check.TypeError");
      }
    }
    if (!d740) {
      u_no_case(s739);
    }
  }
}

void p_check_46_variable(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s745 = v_x;
    int d746 = 0;
    if ((!d746) && u_is_con(s745, &n_syntax_46_Var)) {
      u_value v_n = u_field(s745, 0);
      u_value v_a = u_field(s745, 1);
      d746 = 1;
      {
        if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, v_assignable)))) {
          {
            u_raise(u_mk_con(&n_check_46_NotAssignable, 1, v_n), "check.TypeError");
          }
        }
        u_value s747 = f_list_46_lookup_2(v_n, v_env);
        int d748 = 0;
        if ((!d748) && u_is_con(s747, &n_option_46_Some)) {
          u_value v_t = u_field(u_field(s747, 0), 1);
          d748 = 1;
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_t, v_u, r_s);
            u_value t749 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_t);
            *r_y = t749;
          }
        }
        if ((!d748) && u_is_con(s747, &n_option_46_None)) {
          d748 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
          }
        }
        if (!d748) {
          u_no_case(s747);
        }
      }
    }
    if (!d746) {
      u_no_case(s745);
    }
  }
}

void p_check_46_variables(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s750 = v_xs;
    int d751 = 0;
    if ((!d751) && u_is_con(s750, &n_list_46_Nil)) {
      d751 = 1;
      {
        u_value t752 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t752;
      }
    }
    if ((!d751) && u_is_con(s750, &n_list_46_Cons)) {
      u_value v_x = u_field(s750, 0);
      u_value v_more = u_field(s750, 1);
      d751 = 1;
      {
        p_check_46_variable(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_variables(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t753 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t753;
      }
    }
    if (!d751) {
      u_no_case(s750);
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
    u_value s754 = v_x;
    int d755 = 0;
    if ((!d755) && u_is_con(s754, &n_syntax_46_SSkip)) {
      d755 = 1;
      {
        u_value t756 = v_x;
        *r_y = t756;
      }
    }
    if ((!d755) && u_is_con(s754, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s754, 0);
      d755 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)));
        p_check_46_variables(v_env, v_assignable, f_list_46_firsts_1(v_pairs), &v_ys, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_pairs), f_syntax_46_types_1(v_ys), &v_es, r_s);
        u_value t757 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_zip_2(v_ys, v_es));
        *r_y = t757;
      }
    }
    if ((!d755) && u_is_con(s754, &n_syntax_46_SBlock)) {
      u_value v_locals = u_field(s754, 0);
      u_value v_body = u_field(s754, 1);
      d755 = 1;
      {
        p_check_46_untyped(v_locals, f_check_46_initialized_1(v_body));
        p_check_46_formals(v_locals, &v_ys, r_s);
        p_check_46_statements(f_list_46__43__43__2(f_check_46_bind_1(v_ys), v_env), f_list_46__43__43__2(f_syntax_46_names_1(v_ys), v_assignable), v_body, &v_xs, r_s);
        u_value t758 = u_mk_con(&n_syntax_46_SBlock, 2, v_ys, v_xs);
        *r_y = t758;
      }
    }
    if ((!d755) && u_is_con(s754, &n_syntax_46_SIf)) {
      u_value v_cond = u_field(s754, 0);
      u_value v_left = u_field(s754, 1);
      u_value v_right = u_field(s754, 2);
      d755 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_left, &v_l, r_s);
        p_check_46_statement(v_env, v_assignable, v_right, &v_r, r_s);
        u_value t759 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
        *r_y = t759;
      }
    }
    if ((!d755) && u_is_con(s754, &n_syntax_46_SWhile)) {
      u_value v_cond = u_field(s754, 0);
      u_value v_body = u_field(s754, 1);
      d755 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        u_value t760 = u_mk_con(&n_syntax_46_SWhile, 2, v_c, v_l);
        *r_y = t760;
      }
    }
    if ((!d755) && u_is_con(s754, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s754, 0);
      u_value v_ins = u_field(s754, 1);
      u_value v_outs = u_field(s754, 2);
      u_value v_refs = u_field(s754, 3);
      d755 = 1;
      {
        p_check_46_call_proc(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d755) && u_is_con(s754, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s754, 0);
      u_value v_cases = u_field(s754, 1);
      d755 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_cases, v_t, &v_bs, r_s);
        u_value t761 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e762 = &t761;
        u_value l763 = v_bs;
        while (u_is_con(l763, &n_list_46_Cons)) {
          u_value v_br = u_field(l763, 0);
          if (u_truth(u_mk_bool(1))) {
            e762 = u_append(e762, f_check_46_case_of_1(v_br));
          }
          l763 = u_field(l763, 1);
        }
        p_check_46_coverage(*r_s, v_t, t761);
        u_value t764 = u_mk_con(&n_syntax_46_SMatch, 2, v_c, v_bs);
        *r_y = t764;
      }
    }
    if ((!d755) && u_is_con(s754, &n_syntax_46_SAssert)) {
      u_value v_cond = u_field(s754, 0);
      d755 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        u_value t765 = u_mk_con(&n_syntax_46_SAssert, 1, v_c);
        *r_y = t765;
      }
    }
    if ((!d755) && u_is_con(s754, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s754, 0);
      u_value v_a = u_field(s754, 1);
      d755 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_unify(v_t, v_u, r_s);
        u_value t766 = u_mk_con(&n_syntax_46_SRaise, 2, v_c, v_t);
        *r_y = t766;
      }
    }
    if ((!d755) && u_is_con(s754, &n_syntax_46_STry)) {
      u_value v_body = u_field(s754, 0);
      u_value v_handlers = u_field(s754, 1);
      d755 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_handlers, &v_hs, r_s);
        u_value t767 = u_mk_con(&n_syntax_46_STry, 2, v_l, v_hs);
        *r_y = t767;
      }
    }
    if (!d755) {
      u_no_case(s754);
    }
  }
}

u_value f_check_46_initialized_1(u_value v_xs) {
  u_value s768 = v_xs;
  u_value t769 = NULL;
  if ((!t769) && (u_is_con(s768, &n_list_46_Cons) && (u_is_con(u_field(s768, 0), &n_syntax_46_SAssign) && (u_is_con(u_field(u_field(s768, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s768, 0), 0), 1), &n_list_46_Nil))))) {
    u_value v_x = u_field(u_field(u_field(u_field(s768, 0), 0), 0), 0);
    u_value v_rest = u_field(s768, 1);
    t769 = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), f_check_46_initialized_1(v_rest));
  }
  if (!t769) {
    t769 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t769) {
    u_no_case(s768);
  }
  return t769;
}

void p_check_46_untyped(u_value v_xs, u_value v_inits) {
  {
    u_value s770 = v_xs;
    int d771 = 0;
    if ((!d771) && u_is_con(s770, &n_list_46_Nil)) {
      d771 = 1;
      {
      }
    }
    if ((!d771) && (u_is_con(s770, &n_list_46_Cons) && u_is_con(u_field(s770, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s770, 0), 0);
      u_value v_t = u_field(u_field(s770, 0), 1);
      u_value v_rest = u_field(s770, 1);
      d771 = 1;
      {
        u_value t772 = f_check_46_holes_1(v_t);
        if (u_truth(t772)) {
          t772 = f_prelude_46_not_1(f_list_46_contains_2(v_n, v_inits));
        }
        if (u_truth(t772)) {
          {
            u_raise(u_mk_con(&n_check_46_MissingType, 1, v_n), "check.TypeError");
          }
        }
        p_check_46_untyped(v_rest, v_inits);
      }
    }
    if (!d771) {
      u_no_case(s770);
    }
  }
}

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s773 = v_xs;
    int d774 = 0;
    if ((!d774) && u_is_con(s773, &n_list_46_Nil)) {
      d774 = 1;
      {
        u_value t775 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t775;
      }
    }
    if ((!d774) && u_is_con(s773, &n_list_46_Cons)) {
      u_value v_x = u_field(s773, 0);
      u_value v_more = u_field(s773, 1);
      d774 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_statements(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t776 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t776;
      }
    }
    if (!d774) {
      u_no_case(s773);
    }
  }
}

u_value f_check_46_os_procs_1(u_value v_c) {
  u_value t781;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_777 = U_LIT("os_args");
    static struct u_object l_os_read_778 = U_LIT("os_read");
    static struct u_object l_os_write_779 = U_LIT("os_write");
    static struct u_object l_os_library_780 = U_LIT("os_library");
    t781 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_777), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_778), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_779), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_780), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)))));
  } else {
    t781 = u_mk_con(&n_list_46_Nil, 0);
  }
  return t781;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t782 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e783 = &t782;
  static struct u_object l_Program_784 = U_LIT("Program");
  u_value l785 = f_ctx_46_names_of_2(v_c, &l_Program_784);
  while (u_is_con(l785, &n_list_46_Cons)) {
    u_value v_d = u_field(l785, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e783 = u_append(e783, v_d);
    }
    l785 = u_field(l785, 1);
  }
  u_value s786 = t782;
  u_value t787 = NULL;
  if ((!t787) && u_is_con(s786, &n_list_46_Cons)) {
    u_value v_d = u_field(s786, 0);
    static struct u_object l_read_788 = U_LIT("read");
    static struct u_object l_library_789 = U_LIT("library");
    t787 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_788), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_789), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t787) {
    t787 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t787) {
    u_no_case(s786);
  }
  return t787;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value s790 = f_list_46_lookup_2(v_p, v_env);
    int d791 = 0;
    if ((!d791) && u_is_con(s790, &n_option_46_Some)) {
      u_value v_sc = u_field(s790, 0);
      d791 = 1;
      {
        p_check_46_call_variable(v_env, v_assignable, v_p, v_sc, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d791) && u_is_con(s790, &n_option_46_None)) {
      d791 = 1;
      {
        p_check_46_call_declared(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if (!d791) {
      u_no_case(s790);
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
    p_check_46_use(v_p, u_mk_con(&n_syntax_46_THole, 0), v_sc, &v_x, r_s);
    p_check_46_fresh_list(v_ins, &v_is, r_s);
    p_check_46_fresh_list(v_outs, &v_os, r_s);
    p_check_46_fresh_list(v_refs, &v_rs, r_s);
    p_check_46_unify(f_syntax_46_Var_46_typ_1(v_x), u_mk_con(&n_syntax_46_TProc, 3, v_is, v_os, v_rs), r_s);
    p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
    p_check_46_checks(v_env, v_ins, v_is, &v_ins1, r_s);
    p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
    p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
    p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), f_list_46__43__43__2(v_os, v_rs), r_s);
    u_value t792 = u_mk_con(&n_syntax_46_SCall, 4, v_p, v_ins1, v_outs1, v_refs1);
    *r_y = t792;
  }
}

void p_check_46_call_declared(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s794 = f_check_46_visible_names_3(*r_s, v_p, u_mk_closure(lam793, 1, *r_s));
    int d795 = 0;
    if ((!d795) && (u_is_con(s794, &n_list_46_Cons) && u_is_con(u_field(s794, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s794, 0);
      d795 = 1;
      {
        u_value t796 = v_q1;
        v_q = t796;
      }
    }
    if ((!d795) && u_is_con(s794, &n_list_46_Nil)) {
      d795 = 1;
      {
        u_value t797 = v_p;
        v_q = t797;
      }
    }
    if (!d795) {
      u_value v_qs = s794;
      d795 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d795) {
      u_no_case(s794);
    }
    u_value s798 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_State_46_ctx_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_State_46_ctx_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_State_46_ctx_1(*r_s)))));
    int d799 = 0;
    if ((!d799) && u_is_con(s798, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s798, 0), 0);
      u_value v_is = u_field(u_field(s798, 0), 1);
      u_value v_os = u_field(u_field(s798, 0), 2);
      u_value v_rs = u_field(u_field(s798, 0), 3);
      d799 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t800 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e801 = &t800;
        u_value l802 = v_is;
        while (u_is_con(l802, &n_list_46_Cons)) {
          u_value v_t = u_field(l802, 0);
          if (u_truth(u_mk_bool(1))) {
            e801 = u_append(e801, f_check_46_subst_2(v_t, v_m));
          }
          l802 = u_field(l802, 1);
        }
        p_check_46_checks(v_env, v_ins, t800, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t803 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e804 = &t803;
        u_value l805 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l805, &n_list_46_Cons)) {
          u_value v_t = u_field(l805, 0);
          if (u_truth(u_mk_bool(1))) {
            e804 = u_append(e804, f_check_46_subst_2(v_t, v_m));
          }
          l805 = u_field(l805, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t803, r_s);
        u_value t806 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t806;
      }
    }
    if ((!d799) && u_is_con(s798, &n_option_46_None)) {
      d799 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d799) {
      u_no_case(s798);
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
    u_value s807 = v_cases;
    int d808 = 0;
    if ((!d808) && u_is_con(s807, &n_list_46_Nil)) {
      d808 = 1;
      {
        u_value t809 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t809;
      }
    }
    if ((!d808) && (u_is_con(s807, &n_list_46_Cons) && u_is_con(u_field(s807, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s807, 0), 0);
      u_value v_g = u_field(u_field(s807, 0), 1);
      u_value v_b = u_field(u_field(s807, 0), 2);
      u_value v_more = u_field(s807, 1);
      d808 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t810 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t810;
      }
    }
    if (!d808) {
      u_no_case(s807);
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
    u_value s811 = v_hs;
    int d812 = 0;
    if ((!d812) && u_is_con(s811, &n_list_46_Nil)) {
      d812 = 1;
      {
        u_value t813 = u_mk_con(&n_list_46_Nil, 0);
        *r_hs1 = t813;
      }
    }
    if ((!d812) && (u_is_con(s811, &n_list_46_Cons) && u_is_con(u_field(s811, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s811, 0), 0);
      u_value v_p = u_field(u_field(s811, 0), 1);
      u_value v_g = u_field(u_field(s811, 0), 2);
      u_value v_b = u_field(u_field(s811, 0), 3);
      u_value v_more = u_field(s811, 1);
      d812 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t814 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t814;
      }
    }
    if (!d812) {
      u_no_case(s811);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t815 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t815)) {
      t815 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t816 = t815;
    if (!u_truth(t816)) {
      t816 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t817 = t816;
    if (!u_truth(t817)) {
      t817 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n))), u_mk_con(&n_list_46_Nil, 0));
    }
    if (u_truth(t817)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t818 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t818)) {
      t818 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t819 = t818;
    if (!u_truth(t819)) {
      t819 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t819)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_820 = U_LIT("Bool");
    static struct u_object l_Int_821 = U_LIT("Int");
    static struct u_object l_Str_822 = U_LIT("Str");
    static struct u_object l_Char_823 = U_LIT("Char");
    static struct u_object l__45__62__824 = U_LIT("->");
    u_value t825 = f_list_46_contains_2(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_820, u_mk_con(&n_list_46_Cons, 2, &l_Int_821, u_mk_con(&n_list_46_Cons, 2, &l_Str_822, u_mk_con(&n_list_46_Cons, 2, &l_Char_823, u_mk_con(&n_list_46_Cons, 2, &l__45__62__824, u_mk_con(&n_list_46_Nil, 0)))))));
    if (!u_truth(t825)) {
      t825 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t826 = t825;
    if (!u_truth(t826)) {
      t826 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t826)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t827 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e828 = &t827;
    u_value l829 = v_ts;
    while (u_is_con(l829, &n_list_46_Cons)) {
      u_value v_t = u_field(l829, 0);
      if (u_truth(u_mk_bool(1))) {
        e828 = u_append(e828, f_check_46_free_1(v_t));
      }
      l829 = u_field(l829, 1);
    }
    u_value t830 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t827)), f_check_46_params_1(v_ps));
    v_undeclared = t830;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, u_mk_con(&n_list_46_Nil, 0)))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
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
      u_value v_rest = u_field(s831, 1);
      d832 = 1;
      {
        u_value t833 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t833;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        u_value t834 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t834;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d832) && u_is_con(s831, &n_list_46_Cons)) {
      u_value v_rest = u_field(s831, 1);
      d832 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d832) {
      u_no_case(s831);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s835 = v_cs;
    int d836 = 0;
    if ((!d836) && u_is_con(s835, &n_list_46_Nil)) {
      d836 = 1;
      {
      }
    }
    if ((!d836) && (u_is_con(s835, &n_list_46_Cons) && u_is_con(u_field(u_field(s835, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s835, 0), 0);
      u_value v_n = u_field(u_field(u_field(s835, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s835, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s835, 0), 1), 2);
      u_value v_rest = u_field(s835, 1);
      d836 = 1;
      {
        u_value t837 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t837;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)));
        u_value t838 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_State_46_ctx_1(*r_s), v_n, v_ps, v_u));
        *r_s = t838;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d836) && u_is_con(s835, &n_list_46_Cons)) {
      u_value v_rest = u_field(s835, 1);
      d836 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d836) {
      u_no_case(s835);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
  {
    u_value s839 = v_cs;
    int d840 = 0;
    if ((!d840) && u_is_con(s839, &n_list_46_Nil)) {
      d840 = 1;
      {
      }
    }
    if ((!d840) && (u_is_con(s839, &n_list_46_Cons) && u_is_con(u_field(u_field(s839, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s839, 0), 0);
      u_value v_n = u_field(u_field(u_field(s839, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s839, 0), 1), 1);
      u_value v_ks = u_field(u_field(u_field(s839, 0), 1), 2);
      u_value v_rest = u_field(s839, 1);
      d840 = 1;
      {
        u_value t841 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t841;
        p_check_46_unique_ids(v_ps);
        u_value t842 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e843 = &t842;
        u_value l844 = v_ks;
        while (u_is_con(l844, &n_list_46_Cons)) {
          u_value v_k = u_field(l844, 0);
          if (u_truth(u_mk_bool(1))) {
            e843 = u_append(e843, f_syntax_46_Constr_46_name_1(v_k));
          }
          l844 = u_field(l844, 1);
        }
        u_value t845 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_State_46_ctx_1(*r_s), v_n, t842));
        *r_s = t845;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d840) && u_is_con(s839, &n_list_46_Cons)) {
      u_value v_rest = u_field(s839, 1);
      d840 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d840) {
      u_no_case(s839);
    }
  }
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s846 = v_ks;
    int d847 = 0;
    if ((!d847) && u_is_con(s846, &n_list_46_Nil)) {
      d847 = 1;
      {
      }
    }
    if ((!d847) && (u_is_con(s846, &n_list_46_Cons) && u_is_con(u_field(s846, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s846, 0), 0);
      u_value v_args = u_field(u_field(s846, 0), 1);
      u_value v_rest = u_field(s846, 1);
      d847 = 1;
      {
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t848 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_State_46_ctx_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t848;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d847) {
      u_no_case(s846);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s849 = v_xss;
  u_value t850 = NULL;
  if ((!t850) && u_is_con(s849, &n_list_46_Nil)) {
    t850 = v_r;
  }
  if ((!t850) && u_is_con(s849, &n_list_46_Cons)) {
    u_value v_xs = u_field(s849, 0);
    u_value v_rest = u_field(s849, 1);
    t850 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t850) {
    u_no_case(s849);
  }
  return t850;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s851 = v_xss;
  u_value t852 = NULL;
  if ((!t852) && u_is_con(s851, &n_list_46_Nil)) {
    t852 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t852) && u_is_con(s851, &n_list_46_Cons)) {
    u_value v_xs = u_field(s851, 0);
    t852 = v_xs;
  }
  if (!t852) {
    u_no_case(s851);
  }
  return t852;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s853 = v_xss;
    int d854 = 0;
    if ((!d854) && u_is_con(s853, &n_list_46_Nil)) {
      d854 = 1;
      {
        u_value t855 = u_mk_con(&n_list_46_Nil, 0);
        *r_yss = t855;
      }
    }
    if ((!d854) && u_is_con(s853, &n_list_46_Cons)) {
      u_value v_xs = u_field(s853, 0);
      u_value v_more = u_field(s853, 1);
      d854 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t856 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t856;
      }
    }
    if (!d854) {
      u_no_case(s853);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s857 = v_cs;
    int d858 = 0;
    if ((!d858) && u_is_con(s857, &n_list_46_Nil)) {
      d858 = 1;
      {
      }
    }
    if ((!d858) && (u_is_con(s857, &n_list_46_Cons) && u_is_con(u_field(u_field(s857, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s857, 0), 0);
      u_value v_n = u_field(u_field(u_field(s857, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s857, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s857, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s857, 0), 1), 3);
      u_value v_rest = u_field(s857, 1);
      d858 = 1;
      {
        u_value t859 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t859;
        p_check_46_overload(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), u_mk_con(&n_list_46_Nil, 0)));
        u_value t860 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t860;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d858) && u_is_con(s857, &n_list_46_Cons)) {
      u_value v_rest = u_field(s857, 1);
      d858 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d858) {
      u_no_case(s857);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s861 = v_cs;
    int d862 = 0;
    if ((!d862) && u_is_con(s861, &n_list_46_Nil)) {
      d862 = 1;
      {
      }
    }
    if ((!d862) && (u_is_con(s861, &n_list_46_Cons) && u_is_con(u_field(u_field(s861, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s861, 0), 0);
      u_value v_n = u_field(u_field(u_field(s861, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s861, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s861, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s861, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s861, 0), 1), 4);
      u_value v_rest = u_field(s861, 1);
      d862 = 1;
      {
        u_value t863 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t863;
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t864 = f_syntax_46_types_1(v_xs);
        v_ts = t864;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t865 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t865;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d862) && u_is_con(s861, &n_list_46_Cons)) {
      u_value v_rest = u_field(s861, 1);
      d862 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d862) {
      u_no_case(s861);
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
    u_value s866 = v_c;
    int d867 = 0;
    if ((!d867) && u_is_con(s866, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s866, 0);
      u_value v_ps = u_field(s866, 1);
      u_value v_bound = u_field(s866, 2);
      u_value v_res = u_field(s866, 3);
      u_value v_body = u_field(s866, 4);
      d867 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t868 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t868;
      }
    }
    if ((!d867) && u_is_con(s866, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s866, 0);
      u_value v_ps = u_field(s866, 1);
      u_value v_ins = u_field(s866, 2);
      u_value v_outs = u_field(s866, 3);
      u_value v_refs = u_field(s866, 4);
      u_value v_body = u_field(s866, 5);
      d867 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t869 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t869;
      }
    }
    if ((!d867) && u_is_con(s866, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s866, 0);
      d867 = 1;
      {
        p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_b, &v_t, r_s);
        u_value t870 = u_mk_con(&n_syntax_46_CEval, 1, v_b);
        *r_c1 = t870;
      }
    }
    if ((!d867) && u_is_con(s866, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s866, 0);
      u_value v_body = u_field(s866, 1);
      d867 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t871 = u_mk_con(&n_syntax_46_CRun, 2, v_n, v_y);
        *r_c1 = t871;
      }
    }
    if ((!d867) && u_is_con(s866, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s866, 0);
      u_value v_body = u_field(s866, 1);
      d867 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t872 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t872;
      }
    }
    if (!d867) {
      d867 = 1;
      {
        u_value t873 = v_c;
        *r_c1 = t873;
      }
    }
    if (!d867) {
      u_no_case(s866);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s874 = v_cs;
    int d875 = 0;
    if ((!d875) && u_is_con(s874, &n_list_46_Nil)) {
      d875 = 1;
      {
        u_value t876 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t876;
      }
    }
    if ((!d875) && u_is_con(s874, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s874, 0), 0);
      u_value v_c = u_field(u_field(s874, 0), 1);
      u_value v_more = u_field(s874, 1);
      d875 = 1;
      {
        u_value t877 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t877;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t878 = f_check_46_zonk_cmd_2(f_check_46_State_46_sub_1(*r_s), v_c1);
        v_c2 = t878;
        u_value s879 = f_check_46_unknown_in_cmd_1(v_c2);
        int d880 = 0;
        if ((!d880) && u_is_con(s879, &n_option_46_Some)) {
          u_value v_t = u_field(s879, 0);
          d880 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d880) && u_is_con(s879, &n_option_46_None)) {
          d880 = 1;
          {
          }
        }
        if (!d880) {
          u_no_case(s879);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t881 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t881;
      }
    }
    if (!d875) {
      u_no_case(s874);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s882 = v_cs;
  u_value t883 = NULL;
  if ((!t883) && u_is_con(s882, &n_list_46_Nil)) {
    t883 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t883) && (u_is_con(s882, &n_list_46_Cons) && u_is_con(u_field(u_field(s882, 0), 1), &n_syntax_46_CRun))) {
    u_value v_n = u_field(u_field(u_field(s882, 0), 1), 0);
    u_value v_more = u_field(s882, 1);
    t883 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t883) && (u_is_con(s882, &n_list_46_Cons) && u_is_con(u_field(u_field(s882, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s882, 0), 1), 0);
    u_value v_more = u_field(s882, 1);
    t883 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t883) && u_is_con(s882, &n_list_46_Cons)) {
    u_value v_more = u_field(s882, 1);
    t883 = f_check_46_block_names_1(v_more);
  }
  if (!t883) {
    u_no_case(s882);
  }
  return t883;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_s = NULL;
    u_value v_ds = NULL;
    u_value v_cs = NULL;
    u_value s884 = v_p;
    int d885 = 0;
    if ((!d885) && u_is_con(s884, &n_syntax_46_Program)) {
      u_value v_cmds = u_field(s884, 0);
      d885 = 1;
      {
        u_value t886 = f_check_46_flatten_2(v_main, v_cmds);
        v_ds = t886;
        u_value t887 = f_check_46_start_2(v_main, f_check_46_visibility_2(v_ds, v_main));
        v_s = t887;
        p_check_46_declare_datas(v_ds, &v_s);
        p_check_46_declare_sorts(v_ds, &v_s);
        p_check_46_declare_constrs(v_ds, &v_s);
        p_check_46_declare_funs(v_ds, &v_s);
        p_check_46_declare_procs(v_ds, &v_s);
        p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
        p_check_46_commands(v_ds, &v_cs, &v_s);
        u_value t888 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
        *r_q = t888;
      }
    }
    if (!d885) {
      u_no_case(s884);
    }
  }
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_889 = U_LIT("main");
    u_value t890 = f_check_46_start_2(&l_main_889, u_mk_con(&n_list_46_Nil, 0));
    v_s = t890;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t891 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(v_s), v_u);
    *r_t = t891;
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
  static struct u_object l_List_892 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_892), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_893 = U_LIT("list");
  static struct u_object l_List_894 = U_LIT("List");
  static struct u_object l_A_895 = U_LIT("A");
  static struct u_object l_Nil_896 = U_LIT("Nil");
  static struct u_object l_Cons_897 = U_LIT("Cons");
  static struct u_object l_A_898 = U_LIT("A");
  static struct u_object l_A_899 = U_LIT("A");
  static struct u_object l_list_900 = U_LIT("list");
  static struct u_object l_len_901 = U_LIT("len");
  static struct u_object l_A_902 = U_LIT("A");
  static struct u_object l_xs_903 = U_LIT("xs");
  static struct u_object l_A_904 = U_LIT("A");
  static struct u_object l_xs_905 = U_LIT("xs");
  static struct u_object l_Nil_906 = U_LIT("Nil");
  static struct u_object l_Cons_907 = U_LIT("Cons");
  static struct u_object l_ys_908 = U_LIT("ys");
  static struct u_object l__43__909 = U_LIT("+");
  static struct u_object l_len_910 = U_LIT("len");
  static struct u_object l_ys_911 = U_LIT("ys");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_893, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_894), u_mk_con(&n_list_46_Cons, 2, &l_A_895, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_896), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_897), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_898), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_899)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_900), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_901), u_mk_con(&n_list_46_Cons, 2, &l_A_902, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_903), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_904))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_905), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_906), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_907), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_908), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__909, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_910, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_911), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s912 = v_t;
  u_value t913 = NULL;
  if ((!t913) && u_is_con(s912, &n_c_46_CtVoid)) {
    static struct u_object l_void_914 = U_LIT("void");
    t913 = &l_void_914;
  }
  if ((!t913) && u_is_con(s912, &n_c_46_CtInt)) {
    static struct u_object l_int_915 = U_LIT("int");
    t913 = &l_int_915;
  }
  if ((!t913) && u_is_con(s912, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s912, 0);
    t913 = v_n;
  }
  if ((!t913) && (u_is_con(s912, &n_c_46_CtPtr) && u_is_con(u_field(s912, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s912, 0), 0);
    static struct u_object l__42__916 = U_LIT("*");
    t913 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__916);
  }
  if ((!t913) && u_is_con(s912, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s912, 0);
    static struct u_object l__32__42__917 = U_LIT(" *");
    t913 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__917);
  }
  if (!t913) {
    u_no_case(s912);
  }
  return t913;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s918 = v_t;
  u_value t919 = NULL;
  if ((!t919) && u_is_con(s918, &n_c_46_CtPtr)) {
    t919 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t919) {
    static struct u_object l__32__920 = U_LIT(" ");
    t919 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__920, v_x));
  }
  if (!t919) {
    u_no_case(s918);
  }
  return t919;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s921 = v_e;
  u_value t922 = NULL;
  if ((!t922) && u_is_con(s921, &n_c_46_CeVar)) {
    u_value v_x = u_field(s921, 0);
    t922 = v_x;
  }
  if ((!t922) && u_is_con(s921, &n_c_46_CeNum)) {
    u_value v_n = u_field(s921, 0);
    t922 = u_str_show(v_n);
  }
  if ((!t922) && u_is_con(s921, &n_c_46_CeStr)) {
    u_value v_s = u_field(s921, 0);
    t922 = u_str_quote(v_s);
  }
  if ((!t922) && u_is_con(s921, &n_c_46_CeCall)) {
    u_value v_f = u_field(s921, 0);
    u_value v_args = u_field(s921, 1);
    static struct u_object l__40__923 = U_LIT("(");
    static struct u_object l__44__32__924 = U_LIT(", ");
    u_value t925 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e926 = &t925;
    u_value l927 = v_args;
    while (u_is_con(l927, &n_list_46_Cons)) {
      u_value v_a = u_field(l927, 0);
      if (u_truth(u_mk_bool(1))) {
        e926 = u_append(e926, f_c_46_c_expr_1(v_a));
      }
      l927 = u_field(l927, 1);
    }
    static struct u_object l__41__928 = U_LIT(")");
    t922 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__923, u_concat(u_str_join(&l__44__32__924, t925), &l__41__928)));
  }
  if ((!t922) && u_is_con(s921, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s921, 0);
    u_value v_a = u_field(s921, 1);
    t922 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t922) && u_is_con(s921, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s921, 0);
    u_value v_a = u_field(s921, 1);
    u_value v_b = u_field(s921, 2);
    static struct u_object l__32__929 = U_LIT(" ");
    static struct u_object l__32__930 = U_LIT(" ");
    t922 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__929, u_concat(v_o, u_concat(&l__32__930, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t922) && u_is_con(s921, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s921, 0);
    u_value v_i = u_field(s921, 1);
    static struct u_object l__91__931 = U_LIT("[");
    static struct u_object l__93__932 = U_LIT("]");
    t922 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__931, u_concat(f_c_46_c_expr_1(v_i), &l__93__932)));
  }
  if ((!t922) && u_is_con(s921, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s921, 0);
    u_value v_f = u_field(s921, 1);
    static struct u_object l__45__62__933 = U_LIT("->");
    t922 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__933, v_f));
  }
  if ((!t922) && u_is_con(s921, &n_c_46_CeCast)) {
    u_value v_t = u_field(s921, 0);
    u_value v_a = u_field(s921, 1);
    static struct u_object l__40__934 = U_LIT("(");
    static struct u_object l__41__935 = U_LIT(")");
    t922 = u_concat(&l__40__934, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__935, f_c_46_c_operand_1(v_a))));
  }
  if ((!t922) && u_is_con(s921, &n_c_46_CeCond)) {
    u_value v_c = u_field(s921, 0);
    u_value v_a = u_field(s921, 1);
    u_value v_b = u_field(s921, 2);
    static struct u_object l__32__63__32__936 = U_LIT(" ? ");
    static struct u_object l__32__58__32__937 = U_LIT(" : ");
    t922 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__936, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__937, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t922) && u_is_con(s921, &n_c_46_CeInit)) {
    u_value v_es = u_field(s921, 0);
    static struct u_object l__123__938 = U_LIT("{");
    static struct u_object l__44__32__939 = U_LIT(", ");
    u_value t940 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e941 = &t940;
    u_value l942 = v_es;
    while (u_is_con(l942, &n_list_46_Cons)) {
      u_value v_a = u_field(l942, 0);
      if (u_truth(u_mk_bool(1))) {
        e941 = u_append(e941, f_c_46_c_expr_1(v_a));
      }
      l942 = u_field(l942, 1);
    }
    static struct u_object l__125__943 = U_LIT("}");
    t922 = u_concat(&l__123__938, u_concat(u_str_join(&l__44__32__939, t940), &l__125__943));
  }
  if (!t922) {
    u_no_case(s921);
  }
  return t922;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s944 = v_e;
  u_value t945 = NULL;
  if ((!t945) && u_is_con(s944, &n_c_46_CeVar)) {
    t945 = f_c_46_c_expr_1(v_e);
  }
  if ((!t945) && u_is_con(s944, &n_c_46_CeNum)) {
    u_value v_n = u_field(s944, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t945 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t945) && u_is_con(s944, &n_c_46_CeStr)) {
    t945 = f_c_46_c_expr_1(v_e);
  }
  if ((!t945) && u_is_con(s944, &n_c_46_CeCall)) {
    t945 = f_c_46_c_expr_1(v_e);
  }
  if ((!t945) && u_is_con(s944, &n_c_46_CeIndex)) {
    t945 = f_c_46_c_expr_1(v_e);
  }
  if ((!t945) && u_is_con(s944, &n_c_46_CeArrow)) {
    t945 = f_c_46_c_expr_1(v_e);
  }
  if (!t945) {
    static struct u_object l__40__946 = U_LIT("(");
    static struct u_object l__41__947 = U_LIT(")");
    t945 = u_concat(&l__40__946, u_concat(f_c_46_c_expr_1(v_e), &l__41__947));
  }
  if (!t945) {
    u_no_case(s944);
  }
  return t945;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s948 = v_s;
  u_value t949 = NULL;
  if ((!t949) && u_is_con(s948, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s948, 0);
    static struct u_object l__59__950 = U_LIT(";");
    t949 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__950)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t949) && (u_is_con(s948, &n_c_46_CsDecl) && u_is_con(u_field(s948, 2), &n_option_46_None))) {
    u_value v_t = u_field(s948, 0);
    u_value v_x = u_field(s948, 1);
    static struct u_object l__59__951 = U_LIT(";");
    t949 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__951)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t949) && (u_is_con(s948, &n_c_46_CsDecl) && u_is_con(u_field(s948, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s948, 0);
    u_value v_x = u_field(s948, 1);
    u_value v_e = u_field(u_field(s948, 2), 0);
    static struct u_object l__32__61__32__952 = U_LIT(" = ");
    static struct u_object l__59__953 = U_LIT(";");
    t949 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__952, u_concat(f_c_46_c_expr_1(v_e), &l__59__953)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t949) && u_is_con(s948, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s948, 0);
    u_value v_e = u_field(s948, 1);
    static struct u_object l__32__61__32__954 = U_LIT(" = ");
    static struct u_object l__59__955 = U_LIT(";");
    t949 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__954, u_concat(f_c_46_c_expr_1(v_e), &l__59__955)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t949) && (u_is_con(s948, &n_c_46_CsIf) && u_is_con(u_field(s948, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s948, 0);
    u_value v_l = u_field(s948, 1);
    static struct u_object l_if_32__40__956 = U_LIT("if (");
    static struct u_object l__41__32__123__957 = U_LIT(") {");
    static struct u_object l__32__32__958 = U_LIT("  ");
    static struct u_object l__125__959 = U_LIT("}");
    t949 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__956, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__957))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__958), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__959), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t949) && u_is_con(s948, &n_c_46_CsIf)) {
    u_value v_c = u_field(s948, 0);
    u_value v_l = u_field(s948, 1);
    u_value v_r = u_field(s948, 2);
    static struct u_object l_if_32__40__960 = U_LIT("if (");
    static struct u_object l__41__32__123__961 = U_LIT(") {");
    static struct u_object l__32__32__962 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__963 = U_LIT("} else {");
    static struct u_object l__32__32__964 = U_LIT("  ");
    static struct u_object l__125__965 = U_LIT("}");
    t949 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__960, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__961))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__962), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__963), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__964), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__965), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t949) && u_is_con(s948, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s948, 0);
    u_value v_b = u_field(s948, 1);
    static struct u_object l_while_32__40__966 = U_LIT("while (");
    static struct u_object l__41__32__123__967 = U_LIT(") {");
    static struct u_object l__32__32__968 = U_LIT("  ");
    static struct u_object l__125__969 = U_LIT("}");
    t949 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__966, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__967))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__968), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__969), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t949) && (u_is_con(s948, &n_c_46_CsReturn) && u_is_con(u_field(s948, 0), &n_option_46_None))) {
    static struct u_object l_return_59__970 = U_LIT("return;");
    t949 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__970), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t949) && (u_is_con(s948, &n_c_46_CsReturn) && u_is_con(u_field(s948, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s948, 0), 0);
    static struct u_object l_return_32__971 = U_LIT("return ");
    static struct u_object l__59__972 = U_LIT(";");
    t949 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__971, u_concat(f_c_46_c_expr_1(v_e), &l__59__972))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t949) && u_is_con(s948, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s948, 0);
    static struct u_object l__123__973 = U_LIT("{");
    static struct u_object l__32__32__974 = U_LIT("  ");
    static struct u_object l__125__975 = U_LIT("}");
    t949 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__973), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__974), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__975), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t949) {
    u_no_case(s948);
  }
  return t949;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t976 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e977 = &t976;
  u_value l978 = v_ss;
  while (u_is_con(l978, &n_list_46_Cons)) {
    u_value v_s = u_field(l978, 0);
    if (u_truth(u_mk_bool(1))) {
      e977 = u_append(e977, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l978 = u_field(l978, 1);
  }
  return f_list_46_concat_1(t976);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s979 = v_ps;
  u_value t980 = NULL;
  if ((!t980) && u_is_con(s979, &n_list_46_Nil)) {
    static struct u_object l_void_981 = U_LIT("void");
    t980 = &l_void_981;
  }
  if (!t980) {
    static struct u_object l__44__32__982 = U_LIT(", ");
    u_value t983 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e984 = &t983;
    u_value l985 = v_ps;
    while (u_is_con(l985, &n_list_46_Cons)) {
      u_value v_p = u_field(l985, 0);
      if (u_truth(u_mk_bool(1))) {
        e984 = u_append(e984, f_c_46_c_param_1(v_p));
      }
      l985 = u_field(l985, 1);
    }
    t980 = u_str_join(&l__44__32__982, t983);
  }
  if (!t980) {
    u_no_case(s979);
  }
  return t980;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s986 = v_p;
  u_value t987 = NULL;
  if (!t987) {
    u_value v_t = u_field(s986, 0);
    u_value v_x = u_field(s986, 1);
    t987 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t987) {
    u_no_case(s986);
  }
  return t987;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s988 = v_d;
  u_value t989 = NULL;
  if ((!t989) && u_is_con(s988, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s988, 0);
    static struct u_object l__35_include_32__990 = U_LIT("#include ");
    t989 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__990, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t989) && u_is_con(s988, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s988, 0);
    u_value v_f = u_field(s988, 1);
    u_value v_ps = u_field(s988, 2);
    static struct u_object l__40__991 = U_LIT("(");
    static struct u_object l__41__59__992 = U_LIT(");");
    t989 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__991, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__992))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t989) && u_is_con(s988, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s988, 0);
    u_value v_f = u_field(s988, 1);
    u_value v_ps = u_field(s988, 2);
    u_value v_b = u_field(s988, 3);
    static struct u_object l__40__993 = U_LIT("(");
    static struct u_object l__41__32__123__994 = U_LIT(") {");
    static struct u_object l__32__32__995 = U_LIT("  ");
    static struct u_object l__125__996 = U_LIT("}");
    t989 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__993, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__994))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__995, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__996, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t989) && (u_is_con(s988, &n_c_46_CdGlobal) && u_is_con(u_field(s988, 2), &n_option_46_None))) {
    u_value v_t = u_field(s988, 0);
    u_value v_x = u_field(s988, 1);
    static struct u_object l__59__997 = U_LIT(";");
    t989 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__997), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t989) && (u_is_con(s988, &n_c_46_CdGlobal) && u_is_con(u_field(s988, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s988, 0);
    u_value v_x = u_field(s988, 1);
    u_value v_e = u_field(u_field(s988, 2), 0);
    static struct u_object l__32__61__32__998 = U_LIT(" = ");
    static struct u_object l__59__999 = U_LIT(";");
    t989 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__998, u_concat(f_c_46_c_expr_1(v_e), &l__59__999))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t989) {
    u_no_case(s988);
  }
  return t989;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s1000 = v_f;
  u_value t1001 = NULL;
  if ((!t1001) && u_is_con(s1000, &n_c_46_CFile)) {
    u_value v_ds = u_field(s1000, 0);
    static struct u_object l__10__10__1002 = U_LIT("\n\n");
    u_value t1003 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1004 = &t1003;
    u_value l1005 = v_ds;
    while (u_is_con(l1005, &n_list_46_Cons)) {
      u_value v_d = u_field(l1005, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__1006 = U_LIT("\n");
        e1004 = u_append(e1004, u_str_join(&l__10__1006, f_c_46_c_decl_1(v_d)));
      }
      l1005 = u_field(l1005, 1);
    }
    static struct u_object l__10__1007 = U_LIT("\n");
    t1001 = u_concat(u_str_join(&l__10__10__1002, t1003), &l__10__1007);
  }
  if (!t1001) {
    u_no_case(s1000);
  }
  return t1001;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__1008 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__1008, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__123__125__44__59__46__1009 = U_LIT("()[]{},;.");
  return u_str_contains(&l__40__41__91__93__123__125__44__59__46__1009, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t1010 = u_is_letter(v_c);
  if (!u_truth(t1010)) {
    t1010 = u_is_digit(v_c);
  }
  u_value t1011 = t1010;
  if (!u_truth(t1011)) {
    t1011 = u_eq(v_c, u_mk_char(95));
  }
  return t1011;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t1012;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t1012 = u_str_get(v_s, v_i);
  } else {
    t1012 = u_chr(u_mk_int(0));
  }
  return t1012;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s1013 = v_c;
  u_value t1014 = NULL;
  if ((!t1014) && u_equals(s1013, u_mk_char(34))) {
    t1014 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t1014) && u_equals(s1013, u_mk_char(39))) {
    t1014 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t1014) && u_equals(s1013, u_mk_char(92))) {
    t1014 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t1014) && u_equals(s1013, u_mk_char(110))) {
    t1014 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t1014) && u_equals(s1013, u_mk_char(116))) {
    t1014 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t1014) && u_equals(s1013, u_mk_char(114))) {
    t1014 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t1014) {
    t1014 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1014) {
    u_no_case(s1013);
  }
  return t1014;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t1015;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t1015 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t1015 = v_i;
  }
  return t1015;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t1016 = u_mk_int(0);
    u_value t1017 = u_mk_int(1);
    u_value t1018 = u_mk_int(1);
    v_j = t1016;
    *r_line = t1017;
    *r_column = t1018;
    u_value t1023 = u_lt(v_j, v_i);
    while (u_truth(t1023)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t1019 = u_add(*r_line, u_mk_int(1));
            u_value t1020 = u_mk_int(1);
            *r_line = t1019;
            *r_column = t1020;
          }
        } else {
          {
            u_value t1021 = u_add(*r_column, u_mk_int(1));
            *r_column = t1021;
          }
        }
        u_value t1022 = u_add(v_j, u_mk_int(1));
        v_j = t1022;
      }
      t1023 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t1024 = *r_i;
    u_value t1025 = u_add(*r_i, u_mk_int(1));
    u_value t1026 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t1024;
    *r_i = t1025;
    v_parts = t1026;
    u_value t1027 = *r_i;
    v_from = t1027;
    u_value t1038 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t1038)) {
      {
        u_value t1028 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t1028;
        u_value t1029 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t1029)) {
          t1029 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t1029)) {
          {
            static struct u_object l_unterminated_32_lit_1030 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_1030, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s1031 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d1032 = 0;
            if ((!d1032) && u_is_con(s1031, &n_option_46_Some)) {
              u_value v_d = u_field(s1031, 0);
              d1032 = 1;
              {
                u_value t1033 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t1034 = u_add(*r_i, u_mk_int(2));
                v_parts = t1033;
                *r_i = t1034;
                u_value t1035 = *r_i;
                v_from = t1035;
              }
            }
            if ((!d1032) && u_is_con(s1031, &n_option_46_None)) {
              d1032 = 1;
              {
                static struct u_object l_invalid_32_escape_1036 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_1036, *r_i), "lex.LexError");
              }
            }
            if (!d1032) {
              u_no_case(s1031);
            }
          }
        } else {
          {
            u_value t1037 = u_add(*r_i, u_mk_int(1));
            *r_i = t1037;
          }
        }
      }
      t1038 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__1039 = U_LIT("");
    u_value t1040 = u_str_join(&l__1039, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t1041 = u_add(*r_i, u_mk_int(1));
    *r_text = t1040;
    *r_i = t1041;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1042 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t1042;
    u_value t1043 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1044 = v_j;
    *r_t = t1043;
    *r_i = t1044;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1045 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t1045;
    u_value s1046 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d1047 = 0;
    if ((!d1047) && u_is_con(s1046, &n_option_46_Some)) {
      u_value v_n = u_field(s1046, 0);
      d1047 = 1;
      {
        u_value t1048 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t1049 = v_j;
        *r_t = t1048;
        *r_i = t1049;
      }
    }
    if ((!d1047) && u_is_con(s1046, &n_option_46_None)) {
      d1047 = 1;
      {
        static struct u_object l_invalid_32_number_1050 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_1050, *r_i), "lex.LexError");
      }
    }
    if (!d1047) {
      u_no_case(s1046);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1051 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t1051;
    u_value t1052 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1053 = v_j;
    *r_t = t1052;
    *r_i = t1053;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t1054 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t1054;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t1055 = *r_i;
    v_start = t1055;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_1056 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_1056, v_start), "lex.LexError");
      }
    }
    u_value t1057 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t1057;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t1058 = u_str_get(v_s, *r_i);
    v_c = t1058;
    u_value s1059 = v_c;
    int d1060 = 0;
    if (!d1060) {
      u_value t1061 = u_is_letter(v_c);
      if (!u_truth(t1061)) {
        t1061 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t1061)) {
        d1060 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d1060) {
      if (u_truth(u_is_digit(v_c))) {
        d1060 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d1060) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d1060 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d1060) && u_equals(s1059, u_mk_char(34))) {
      d1060 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d1060) && u_equals(s1059, u_mk_char(39))) {
      d1060 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d1060) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d1060 = 1;
        {
          u_value t1062 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t1063 = u_add(*r_i, u_mk_int(1));
          *r_t = t1062;
          *r_i = t1063;
        }
      }
    }
    if (!d1060) {
      d1060 = 1;
      {
        static struct u_object l_unexpected_32_chara_1064 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_1064, *r_i), "lex.LexError");
      }
    }
    if (!d1060) {
      u_no_case(s1059);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t1065 = u_mk_int(0);
    v_i = t1065;
    u_value t1066 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1066;
    u_value t1073 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t1073)) {
      {
        u_value t1067 = v_i;
        v_start = t1067;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t1068 = u_add(v_i, u_mk_int(1));
            v_i = t1068;
          }
        } else {
          u_value t1069 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t1069)) {
            t1069 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t1069)) {
            {
              u_value t1071 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam1070, 0));
              v_i = t1071;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t1072 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t1072;
            }
          }
        }
      }
      t1073 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t1074 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t1074;
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
  u_value s1075 = v_s;
  u_value t1076 = NULL;
  if ((!t1076) && u_is_con(s1075, &n_set_46_SLeaf)) {
    t1076 = u_mk_bool(0);
  }
  if ((!t1076) && u_is_con(s1075, &n_set_46_SNode)) {
    u_value v_l = u_field(s1075, 0);
    u_value v_y = u_field(s1075, 1);
    u_value v_r = u_field(s1075, 2);
    u_value s1077 = u_compare(v_x, v_y);
    u_value t1078 = NULL;
    if ((!t1078) && u_equals(s1077, u_mk_int(-1))) {
      t1078 = f_set_46_member_2(v_x, v_l);
    }
    if ((!t1078) && u_equals(s1077, u_mk_int(1))) {
      t1078 = f_set_46_member_2(v_x, v_r);
    }
    if (!t1078) {
      t1078 = u_mk_bool(1);
    }
    if (!t1078) {
      u_no_case(s1077);
    }
    t1076 = t1078;
  }
  if (!t1076) {
    u_no_case(s1075);
  }
  return t1076;
}

u_value f_set_46_insert_2(u_value v_x, u_value v_s) {
  u_value s1079 = v_s;
  u_value t1080 = NULL;
  if ((!t1080) && u_is_con(s1079, &n_set_46_SLeaf)) {
    t1080 = u_mk_con(&n_set_46_SNode, 4, u_mk_con(&n_set_46_SLeaf, 0), v_x, u_mk_con(&n_set_46_SLeaf, 0), u_mk_int(1));
  }
  if ((!t1080) && u_is_con(s1079, &n_set_46_SNode)) {
    u_value v_l = u_field(s1079, 0);
    u_value v_y = u_field(s1079, 1);
    u_value v_r = u_field(s1079, 2);
    u_value s1081 = u_compare(v_x, v_y);
    u_value t1082 = NULL;
    if ((!t1082) && u_equals(s1081, u_mk_int(-1))) {
      t1082 = f_set_46_rebalance_3(f_set_46_insert_2(v_x, v_l), v_y, v_r);
    }
    if ((!t1082) && u_equals(s1081, u_mk_int(1))) {
      t1082 = f_set_46_rebalance_3(v_l, v_y, f_set_46_insert_2(v_x, v_r));
    }
    if (!t1082) {
      t1082 = v_s;
    }
    if (!t1082) {
      u_no_case(s1081);
    }
    t1080 = t1082;
  }
  if (!t1080) {
    u_no_case(s1079);
  }
  return t1080;
}

u_value f_set_46_from_list_1(u_value v_xs) {
  return f_set_46_insert_all_2(v_xs, u_mk_con(&n_set_46_SLeaf, 0));
}

u_value f_set_46_insert_all_2(u_value v_xs, u_value v_s) {
  u_value s1083 = v_xs;
  u_value t1084 = NULL;
  if ((!t1084) && u_is_con(s1083, &n_list_46_Nil)) {
    t1084 = v_s;
  }
  if ((!t1084) && u_is_con(s1083, &n_list_46_Cons)) {
    u_value v_x = u_field(s1083, 0);
    u_value v_rest = u_field(s1083, 1);
    t1084 = f_set_46_insert_all_2(v_rest, f_set_46_insert_2(v_x, v_s));
  }
  if (!t1084) {
    u_no_case(s1083);
  }
  return t1084;
}

u_value f_set_46_elements_1(u_value v_s) {
  return f_set_46_elements_onto_2(v_s, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_set_46_elements_onto_2(u_value v_s, u_value v_rest) {
  u_value s1085 = v_s;
  u_value t1086 = NULL;
  if ((!t1086) && u_is_con(s1085, &n_set_46_SLeaf)) {
    t1086 = v_rest;
  }
  if ((!t1086) && u_is_con(s1085, &n_set_46_SNode)) {
    u_value v_l = u_field(s1085, 0);
    u_value v_x = u_field(s1085, 1);
    u_value v_r = u_field(s1085, 2);
    t1086 = f_set_46_elements_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, v_x, f_set_46_elements_onto_2(v_r, v_rest)));
  }
  if (!t1086) {
    u_no_case(s1085);
  }
  return t1086;
}

u_value f_set_46_count_1(u_value v_s) {
  u_value s1087 = v_s;
  u_value t1088 = NULL;
  if ((!t1088) && u_is_con(s1087, &n_set_46_SLeaf)) {
    t1088 = u_mk_int(0);
  }
  if ((!t1088) && u_is_con(s1087, &n_set_46_SNode)) {
    u_value v_l = u_field(s1087, 0);
    u_value v_r = u_field(s1087, 2);
    t1088 = u_add(u_add(f_set_46_count_1(v_l), u_mk_int(1)), f_set_46_count_1(v_r));
  }
  if (!t1088) {
    u_no_case(s1087);
  }
  return t1088;
}

u_value f_set_46_depth_1(u_value v_s) {
  u_value s1089 = v_s;
  u_value t1090 = NULL;
  if ((!t1090) && u_is_con(s1089, &n_set_46_SLeaf)) {
    t1090 = u_mk_int(0);
  }
  if ((!t1090) && u_is_con(s1089, &n_set_46_SNode)) {
    u_value v_d = u_field(s1089, 3);
    t1090 = v_d;
  }
  if (!t1090) {
    u_no_case(s1089);
  }
  return t1090;
}

u_value f_set_46_snode_depth_2(u_value v_l, u_value v_r) {
  u_value t1091;
  if (u_truth(u_lt(f_set_46_depth_1(v_l), f_set_46_depth_1(v_r)))) {
    t1091 = f_set_46_depth_1(v_r);
  } else {
    t1091 = f_set_46_depth_1(v_l);
  }
  return u_add(u_mk_int(1), t1091);
}

u_value f_set_46_snode_3(u_value v_l, u_value v_x, u_value v_r) {
  return u_mk_con(&n_set_46_SNode, 4, v_l, v_x, v_r, f_set_46_snode_depth_2(v_l, v_r));
}

u_value f_set_46_turn_left_1(u_value v_s) {
  u_value s1092 = v_s;
  u_value t1093 = NULL;
  if ((!t1093) && (u_is_con(s1092, &n_set_46_SNode) && u_is_con(u_field(s1092, 2), &n_set_46_SNode))) {
    u_value v_a = u_field(s1092, 0);
    u_value v_x = u_field(s1092, 1);
    u_value v_b = u_field(u_field(s1092, 2), 0);
    u_value v_y = u_field(u_field(s1092, 2), 1);
    u_value v_c = u_field(u_field(s1092, 2), 2);
    t1093 = f_set_46_snode_3(f_set_46_snode_3(v_a, v_x, v_b), v_y, v_c);
  }
  if (!t1093) {
    t1093 = v_s;
  }
  if (!t1093) {
    u_no_case(s1092);
  }
  return t1093;
}

u_value f_set_46_turn_right_1(u_value v_s) {
  u_value s1094 = v_s;
  u_value t1095 = NULL;
  if ((!t1095) && (u_is_con(s1094, &n_set_46_SNode) && u_is_con(u_field(s1094, 0), &n_set_46_SNode))) {
    u_value v_a = u_field(u_field(s1094, 0), 0);
    u_value v_x = u_field(u_field(s1094, 0), 1);
    u_value v_b = u_field(u_field(s1094, 0), 2);
    u_value v_y = u_field(s1094, 1);
    u_value v_c = u_field(s1094, 2);
    t1095 = f_set_46_snode_3(v_a, v_x, f_set_46_snode_3(v_b, v_y, v_c));
  }
  if (!t1095) {
    t1095 = v_s;
  }
  if (!t1095) {
    u_no_case(s1094);
  }
  return t1095;
}

u_value f_set_46_smaller_1(u_value v_s) {
  u_value s1096 = v_s;
  u_value t1097 = NULL;
  if ((!t1097) && u_is_con(s1096, &n_set_46_SNode)) {
    u_value v_l = u_field(s1096, 0);
    t1097 = v_l;
  }
  if (!t1097) {
    t1097 = v_s;
  }
  if (!t1097) {
    u_no_case(s1096);
  }
  return t1097;
}

u_value f_set_46_greater_1(u_value v_s) {
  u_value s1098 = v_s;
  u_value t1099 = NULL;
  if ((!t1099) && u_is_con(s1098, &n_set_46_SNode)) {
    u_value v_r = u_field(s1098, 2);
    t1099 = v_r;
  }
  if (!t1099) {
    t1099 = v_s;
  }
  if (!t1099) {
    u_no_case(s1098);
  }
  return t1099;
}

u_value f_set_46_rebalance_3(u_value v_l, u_value v_x, u_value v_r) {
  u_value t1103;
  if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1))))) {
    u_value t1100;
    if (u_truth(u_lt(f_set_46_depth_1(f_set_46_smaller_1(v_l)), f_set_46_depth_1(f_set_46_greater_1(v_l))))) {
      t1100 = f_set_46_turn_left_1(v_l);
    } else {
      t1100 = v_l;
    }
    t1103 = f_set_46_turn_right_1(f_set_46_snode_3(t1100, v_x, v_r));
  } else {
    u_value t1102;
    if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1))))) {
      u_value t1101;
      if (u_truth(u_lt(f_set_46_depth_1(f_set_46_greater_1(v_r)), f_set_46_depth_1(f_set_46_smaller_1(v_r))))) {
        t1101 = f_set_46_turn_right_1(v_r);
      } else {
        t1101 = v_r;
      }
      t1102 = f_set_46_turn_left_1(f_set_46_snode_3(v_l, v_x, t1101));
    } else {
      t1102 = f_set_46_snode_3(v_l, v_x, v_r);
    }
    t1103 = t1102;
  }
  return t1103;
}

u_value f_set_46_well_formed_1(u_value v_s) {
  u_value s1104 = v_s;
  u_value t1105 = NULL;
  if ((!t1105) && u_is_con(s1104, &n_set_46_SLeaf)) {
    t1105 = u_mk_bool(1);
  }
  if ((!t1105) && u_is_con(s1104, &n_set_46_SNode)) {
    u_value v_l = u_field(s1104, 0);
    u_value v_r = u_field(s1104, 2);
    u_value v_d = u_field(s1104, 3);
    u_value t1106 = f_set_46_well_formed_1(v_l);
    if (u_truth(t1106)) {
      t1106 = f_set_46_well_formed_1(v_r);
    }
    u_value t1107 = t1106;
    if (u_truth(t1107)) {
      t1107 = u_eq(v_d, f_set_46_snode_depth_2(v_l, v_r));
    }
    u_value t1108 = t1107;
    if (u_truth(t1108)) {
      t1108 = f_prelude_46__60__61__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1)));
    }
    u_value t1109 = t1108;
    if (u_truth(t1109)) {
      t1109 = f_prelude_46__60__61__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1)));
    }
    t1105 = t1109;
  }
  if (!t1105) {
    u_no_case(s1104);
  }
  return t1105;
}

u_value f_set_46_upto_1(u_value v_n) {
  u_value t1110;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t1110 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1110 = f_list_46__43__43__2(f_set_46_upto_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t1110;
}

u_value f_parse_46_Notations_46_cmds_1(u_value v_x) {
  u_value s1111 = v_x;
  u_value t1112 = NULL;
  if ((!t1112) && u_is_con(s1111, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1111, 0);
    t1112 = v_y;
  }
  if (!t1112) {
    u_no_case(s1111);
  }
  return t1112;
}

u_value f_parse_46_Notations_46_ops_1(u_value v_x) {
  u_value s1113 = v_x;
  u_value t1114 = NULL;
  if ((!t1114) && u_is_con(s1113, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1113, 1);
    t1114 = v_y;
  }
  if (!t1114) {
    u_no_case(s1113);
  }
  return t1114;
}

u_value f_parse_46_Notations_46_cmds_58__61__2(u_value v_x, u_value v_y) {
  u_value s1115 = v_x;
  u_value t1116 = NULL;
  if ((!t1116) && u_is_con(s1115, &n_parse_46_Notations)) {
    u_value v_a1 = u_field(s1115, 1);
    t1116 = u_mk_con(&n_parse_46_Notations, 2, v_y, v_a1);
  }
  if (!t1116) {
    u_no_case(s1115);
  }
  return t1116;
}

u_value f_parse_46_Notations_46_ops_58__61__2(u_value v_x, u_value v_y) {
  u_value s1117 = v_x;
  u_value t1118 = NULL;
  if ((!t1118) && u_is_con(s1117, &n_parse_46_Notations)) {
    u_value v_a0 = u_field(s1117, 0);
    t1118 = u_mk_con(&n_parse_46_Notations, 2, v_a0, v_y);
  }
  if (!t1118) {
    u_no_case(s1117);
  }
  return t1118;
}

u_value f_parse_46_Scope_46_params_1(u_value v_x) {
  u_value s1119 = v_x;
  u_value t1120 = NULL;
  if ((!t1120) && u_is_con(s1119, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1119, 0);
    t1120 = v_y;
  }
  if (!t1120) {
    u_no_case(s1119);
  }
  return t1120;
}

u_value f_parse_46_Scope_46_notations_1(u_value v_x) {
  u_value s1121 = v_x;
  u_value t1122 = NULL;
  if ((!t1122) && u_is_con(s1121, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1121, 1);
    t1122 = v_y;
  }
  if (!t1122) {
    u_no_case(s1121);
  }
  return t1122;
}

u_value f_parse_46_Scope_46_params_58__61__2(u_value v_x, u_value v_y) {
  u_value s1123 = v_x;
  u_value t1124 = NULL;
  if ((!t1124) && u_is_con(s1123, &n_parse_46_Scope)) {
    u_value v_a1 = u_field(s1123, 1);
    t1124 = u_mk_con(&n_parse_46_Scope, 2, v_y, v_a1);
  }
  if (!t1124) {
    u_no_case(s1123);
  }
  return t1124;
}

u_value f_parse_46_Scope_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1125 = v_x;
  u_value t1126 = NULL;
  if ((!t1126) && u_is_con(s1125, &n_parse_46_Scope)) {
    u_value v_a0 = u_field(s1125, 0);
    t1126 = u_mk_con(&n_parse_46_Scope, 2, v_a0, v_y);
  }
  if (!t1126) {
    u_no_case(s1125);
  }
  return t1126;
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
  static struct u_object l___1127 = U_LIT("_");
  static struct u_object l_assert_1128 = U_LIT("assert");
  static struct u_object l_assign_1129 = U_LIT("assign");
  static struct u_object l_call_1130 = U_LIT("call");
  static struct u_object l_case_1131 = U_LIT("case");
  static struct u_object l_catch_1132 = U_LIT("catch");
  static struct u_object l_data_1133 = U_LIT("data");
  static struct u_object l_else_1134 = U_LIT("else");
  static struct u_object l_end_1135 = U_LIT("end");
  static struct u_object l_eval_1136 = U_LIT("eval");
  static struct u_object l_false_1137 = U_LIT("false");
  static struct u_object l_for_1138 = U_LIT("for");
  static struct u_object l_function_1139 = U_LIT("function");
  static struct u_object l_if_1140 = U_LIT("if");
  static struct u_object l_import_1141 = U_LIT("import");
  static struct u_object l_in_1142 = U_LIT("in");
  static struct u_object l_infix_1143 = U_LIT("infix");
  static struct u_object l_lambda_1144 = U_LIT("lambda");
  static struct u_object l_let_1145 = U_LIT("let");
  static struct u_object l_local_1146 = U_LIT("local");
  static struct u_object l_match_1147 = U_LIT("match");
  static struct u_object l_out_1148 = U_LIT("out");
  static struct u_object l_postfix_1149 = U_LIT("postfix");
  static struct u_object l_prefix_1150 = U_LIT("prefix");
  static struct u_object l_procedure_1151 = U_LIT("procedure");
  static struct u_object l_raise_1152 = U_LIT("raise");
  static struct u_object l_ref_1153 = U_LIT("ref");
  static struct u_object l_run_1154 = U_LIT("run");
  static struct u_object l_skip_1155 = U_LIT("skip");
  static struct u_object l_test_1156 = U_LIT("test");
  static struct u_object l_then_1157 = U_LIT("then");
  static struct u_object l_true_1158 = U_LIT("true");
  static struct u_object l_type_1159 = U_LIT("type");
  static struct u_object l_try_1160 = U_LIT("try");
  static struct u_object l_while_1161 = U_LIT("while");
  static struct u_object l_with_1162 = U_LIT("with");
  u_pop(handler);
  cache = f_set_46_from_list_1(u_mk_con(&n_list_46_Cons, 2, &l___1127, u_mk_con(&n_list_46_Cons, 2, &l_assert_1128, u_mk_con(&n_list_46_Cons, 2, &l_assign_1129, u_mk_con(&n_list_46_Cons, 2, &l_call_1130, u_mk_con(&n_list_46_Cons, 2, &l_case_1131, u_mk_con(&n_list_46_Cons, 2, &l_catch_1132, u_mk_con(&n_list_46_Cons, 2, &l_data_1133, u_mk_con(&n_list_46_Cons, 2, &l_else_1134, u_mk_con(&n_list_46_Cons, 2, &l_end_1135, u_mk_con(&n_list_46_Cons, 2, &l_eval_1136, u_mk_con(&n_list_46_Cons, 2, &l_false_1137, u_mk_con(&n_list_46_Cons, 2, &l_for_1138, u_mk_con(&n_list_46_Cons, 2, &l_function_1139, u_mk_con(&n_list_46_Cons, 2, &l_if_1140, u_mk_con(&n_list_46_Cons, 2, &l_import_1141, u_mk_con(&n_list_46_Cons, 2, &l_in_1142, u_mk_con(&n_list_46_Cons, 2, &l_infix_1143, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1144, u_mk_con(&n_list_46_Cons, 2, &l_let_1145, u_mk_con(&n_list_46_Cons, 2, &l_local_1146, u_mk_con(&n_list_46_Cons, 2, &l_match_1147, u_mk_con(&n_list_46_Cons, 2, &l_out_1148, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1149, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1150, u_mk_con(&n_list_46_Cons, 2, &l_procedure_1151, u_mk_con(&n_list_46_Cons, 2, &l_raise_1152, u_mk_con(&n_list_46_Cons, 2, &l_ref_1153, u_mk_con(&n_list_46_Cons, 2, &l_run_1154, u_mk_con(&n_list_46_Cons, 2, &l_skip_1155, u_mk_con(&n_list_46_Cons, 2, &l_test_1156, u_mk_con(&n_list_46_Cons, 2, &l_then_1157, u_mk_con(&n_list_46_Cons, 2, &l_true_1158, u_mk_con(&n_list_46_Cons, 2, &l_type_1159, u_mk_con(&n_list_46_Cons, 2, &l_try_1160, u_mk_con(&n_list_46_Cons, 2, &l_while_1161, u_mk_con(&n_list_46_Cons, 2, &l_with_1162, u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_set_46_member_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1163 = U_LIT("=");
  static struct u_object l__61__62__1164 = U_LIT("=>");
  static struct u_object l__58__61__1165 = U_LIT(":=");
  static struct u_object l__58__1166 = U_LIT(":");
  static struct u_object l__124__1167 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1163, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1164, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1165, u_mk_con(&n_list_46_Cons, 2, &l__58__1166, u_mk_con(&n_list_46_Cons, 2, &l__124__1167, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1168 = v_ns;
  u_value t1169 = NULL;
  if ((!t1169) && u_is_con(s1168, &n_list_46_Nil)) {
    t1169 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1169) && (u_is_con(s1168, &n_list_46_Cons) && (u_is_con(u_field(s1168, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1168, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1168, 0), 1);
    u_value v_ops = u_field(u_field(s1168, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1169 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1169) && u_is_con(s1168, &n_list_46_Cons)) {
    u_value v_more = u_field(s1168, 1);
    t1169 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1169) {
    u_no_case(s1168);
  }
  return t1169;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1170 = v_ns;
  u_value t1171 = NULL;
  if ((!t1171) && u_is_con(s1170, &n_list_46_Nil)) {
    t1171 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1171) && (u_is_con(s1170, &n_list_46_Cons) && (u_is_con(u_field(s1170, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1170, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1170, 0), 1);
    u_value v_ops = u_field(u_field(s1170, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1171 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1171) && u_is_con(s1170, &n_list_46_Cons)) {
    u_value v_more = u_field(s1170, 1);
    t1171 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1171) {
    u_no_case(s1170);
  }
  return t1171;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1172 = v_ns;
  u_value t1173 = NULL;
  if ((!t1173) && u_is_con(s1172, &n_list_46_Nil)) {
    t1173 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1173) && (u_is_con(s1172, &n_list_46_Cons) && u_is_con(u_field(s1172, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1172, 0), 0);
    u_value v_p = u_field(u_field(s1172, 0), 1);
    u_value v_ops = u_field(u_field(s1172, 0), 2);
    u_value t1174 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1174)) {
      t1174 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1175 = t1174;
    if (u_truth(t1175)) {
      t1175 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1175)) {
      t1173 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1173) && u_is_con(s1172, &n_list_46_Cons)) {
    u_value v_more = u_field(s1172, 1);
    t1173 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1173) {
    u_no_case(s1172);
  }
  return t1173;
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
  u_value s1176 = v_cs;
  u_value t1177 = NULL;
  if ((!t1177) && u_is_con(s1176, &n_list_46_Nil)) {
    t1177 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1177) && (u_is_con(s1176, &n_list_46_Cons) && u_is_con(u_field(s1176, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1176, 0), 2);
    u_value v_more = u_field(s1176, 1);
    t1177 = f_list_46__43__43__2(v_ops, f_parse_46_operators_of_1(v_more));
  }
  if ((!t1177) && u_is_con(s1176, &n_list_46_Cons)) {
    u_value v_more = u_field(s1176, 1);
    t1177 = f_parse_46_operators_of_1(v_more);
  }
  if (!t1177) {
    u_no_case(s1176);
  }
  return t1177;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1178 = v_ts;
  u_value t1179 = NULL;
  if ((!t1179) && u_is_con(s1178, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1178, 0), 0);
    t1179 = v_t;
  }
  if (!t1179) {
    t1179 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1179) {
    u_no_case(s1178);
  }
  return t1179;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1180 = v_ts;
  u_value t1181 = NULL;
  if ((!t1181) && (u_is_con(s1180, &n_list_46_Cons) && u_is_con(u_field(s1180, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1180, 1), 0), 0);
    t1181 = v_t;
  }
  if (!t1181) {
    t1181 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1181) {
    u_no_case(s1180);
  }
  return t1181;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1182 = v_ts;
  u_value t1183 = NULL;
  if ((!t1183) && u_is_con(s1182, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1182, 0), 1);
    t1183 = v_i;
  }
  if (!t1183) {
    t1183 = u_mk_int(0);
  }
  if (!t1183) {
    u_no_case(s1182);
  }
  return t1183;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1184 = v_t;
  u_value t1185 = NULL;
  if ((!t1185) && u_is_con(s1184, &n_lex_46_TName)) {
    u_value v_n = u_field(s1184, 0);
    t1185 = v_n;
  }
  if ((!t1185) && u_is_con(s1184, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1184, 0);
    t1185 = v_s;
  }
  if ((!t1185) && u_is_con(s1184, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1184, 0);
    t1185 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1185) && u_is_con(s1184, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1184, 0);
    t1185 = u_str_show(v_n);
  }
  if ((!t1185) && u_is_con(s1184, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1184, 0);
    t1185 = u_str_quote(v_s);
  }
  if ((!t1185) && u_is_con(s1184, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1184, 0);
    static struct u_object l__39__1186 = U_LIT("'");
    static struct u_object l__39__1187 = U_LIT("'");
    t1185 = u_concat(&l__39__1186, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1187));
  }
  if ((!t1185) && u_is_con(s1184, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1188 = U_LIT("end of input");
    t1185 = &l_end_32_of_32_input_1188;
  }
  if (!t1185) {
    u_no_case(s1184);
  }
  return t1185;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1189 = f_parse_46_head_token_1(v_ts);
  u_value t1190 = NULL;
  if ((!t1190) && u_is_con(s1189, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1189, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1190 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1190) && u_is_con(s1189, &n_lex_46_TName)) {
    u_value v_n = u_field(s1189, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1190 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1190) {
    t1190 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1190) {
    u_no_case(s1189);
  }
  return t1190;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1191 = *r_ts;
    int d1192 = 0;
    if ((!d1192) && u_is_con(s1191, &n_list_46_Cons)) {
      u_value v_more = u_field(s1191, 1);
      d1192 = 1;
      {
        u_value t1193 = v_more;
        *r_ts = t1193;
      }
    }
    if (!d1192) {
      d1192 = 1;
      {
      }
    }
    if (!d1192) {
      u_no_case(s1191);
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
        static struct u_object l__32_expected_32_but_32__1194 = U_LIT(" expected but ");
        static struct u_object l__32_found_1195 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1194, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1195))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1196 = f_parse_46_head_token_1(*r_ts);
    int d1197 = 0;
    if ((!d1197) && u_is_con(s1196, &n_lex_46_TName)) {
      u_value v_m = u_field(s1196, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1197 = 1;
        {
          u_value t1198 = v_m;
          *r_n = t1198;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1197) {
      d1197 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1199 = U_LIT("name expected but ");
        static struct u_object l__32_found_1200 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1199, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1200)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1197) {
      u_no_case(s1196);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1201 = f_parse_46_head_token_1(*r_ts);
    int d1202 = 0;
    if ((!d1202) && u_is_con(s1201, &n_lex_46_TName)) {
      u_value v_m = u_field(s1201, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_m))) {
        d1202 = 1;
        {
          static struct u_object l_operator_32__1203 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1204 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1203, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1204)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1205 = f_parse_46_head_token_1(*r_ts);
    int d1206 = 0;
    if ((!d1206) && u_is_con(s1205, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1205, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1206 = 1;
        {
          u_value t1207 = v_s;
          *r_n = t1207;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1206) {
      d1206 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1206) {
      u_no_case(s1205);
    }
  }
}

u_value f_parse_46_Ops_46_notations_1(u_value v_x) {
  u_value s1208 = v_x;
  u_value t1209 = NULL;
  if ((!t1209) && u_is_con(s1208, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1208, 0);
    t1209 = v_y;
  }
  if (!t1209) {
    u_no_case(s1208);
  }
  return t1209;
}

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x) {
  u_value s1210 = v_x;
  u_value t1211 = NULL;
  if ((!t1211) && u_is_con(s1210, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1210, 1);
    t1211 = v_y;
  }
  if (!t1211) {
    u_no_case(s1210);
  }
  return t1211;
}

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x) {
  u_value s1212 = v_x;
  u_value t1213 = NULL;
  if ((!t1213) && u_is_con(s1212, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1212, 2);
    t1213 = v_y;
  }
  if (!t1213) {
    u_no_case(s1212);
  }
  return t1213;
}

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x) {
  u_value s1214 = v_x;
  u_value t1215 = NULL;
  if ((!t1215) && u_is_con(s1214, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1214, 3);
    t1215 = v_y;
  }
  if (!t1215) {
    u_no_case(s1214);
  }
  return t1215;
}

u_value f_parse_46_Ops_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1216 = v_x;
  u_value t1217 = NULL;
  if ((!t1217) && u_is_con(s1216, &n_parse_46_Ops)) {
    u_value v_a1 = u_field(s1216, 1);
    u_value v_a2 = u_field(s1216, 2);
    u_value v_a3 = u_field(s1216, 3);
    t1217 = u_mk_con(&n_parse_46_Ops, 4, v_y, v_a1, v_a2, v_a3);
  }
  if (!t1217) {
    u_no_case(s1216);
  }
  return t1217;
}

u_value f_parse_46_Ops_46_prefix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1218 = v_x;
  u_value t1219 = NULL;
  if ((!t1219) && u_is_con(s1218, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1218, 0);
    u_value v_a2 = u_field(s1218, 2);
    u_value v_a3 = u_field(s1218, 3);
    t1219 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_y, v_a2, v_a3);
  }
  if (!t1219) {
    u_no_case(s1218);
  }
  return t1219;
}

u_value f_parse_46_Ops_46_postfix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1220 = v_x;
  u_value t1221 = NULL;
  if ((!t1221) && u_is_con(s1220, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1220, 0);
    u_value v_a1 = u_field(s1220, 1);
    u_value v_a3 = u_field(s1220, 3);
    t1221 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_y, v_a3);
  }
  if (!t1221) {
    u_no_case(s1220);
  }
  return t1221;
}

u_value f_parse_46_Ops_46_infix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1222 = v_x;
  u_value t1223 = NULL;
  if ((!t1223) && u_is_con(s1222, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1222, 0);
    u_value v_a1 = u_field(s1222, 1);
    u_value v_a2 = u_field(s1222, 2);
    t1223 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_a2, v_y);
  }
  if (!t1223) {
    u_no_case(s1222);
  }
  return t1223;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1224 = v_rest;
    int d1225 = 0;
    if ((!d1225) && u_is_con(s1224, &n_list_46_Nil)) {
      d1225 = 1;
      {
      }
    }
    if ((!d1225) && (u_is_con(s1224, &n_list_46_Cons) && u_is_con(u_field(s1224, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1224, 0), 0);
      u_value v_i = u_field(u_field(s1224, 0), 1);
      d1225 = 1;
      {
        static struct u_object l_unexpected_32_opera_1226 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1226, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1225) {
      d1225 = 1;
      {
        static struct u_object l_missing_32_operator_1227 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1227, v_at), "parse.ParseError");
      }
    }
    if (!d1225) {
      u_no_case(s1224);
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
    u_value s1228 = v_items;
    int d1229 = 0;
    if ((!d1229) && (u_is_con(s1228, &n_list_46_Cons) && u_is_con(u_field(s1228, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1228, 0), 0);
      u_value v_more = u_field(s1228, 1);
      d1229 = 1;
      {
        u_value t1230 = v_x;
        u_value t1231 = v_more;
        *r_a = t1230;
        *r_rest = t1231;
      }
    }
    if ((!d1229) && (u_is_con(s1228, &n_list_46_Cons) && u_is_con(u_field(s1228, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1228, 0), 0);
      u_value v_i = u_field(u_field(s1228, 0), 1);
      u_value v_more = u_field(s1228, 1);
      d1229 = 1;
      {
        u_value s1232 = f_parse_46_prefix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op);
        int d1233 = 0;
        if ((!d1233) && u_is_con(s1232, &n_option_46_Some)) {
          u_value v_p = u_field(s1232, 0);
          d1233 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1234 = u_apply(f_parse_46_Ops_46_prefix_op_1(v_o), 2, v_op, v_b);
            *r_a = t1234;
          }
        }
        if ((!d1233) && u_is_con(s1232, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op))) {
            d1233 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1235 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1235), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1233) && u_is_con(s1232, &n_option_46_None)) {
          d1233 = 1;
          {
            static struct u_object l_undeclared_32_opera_1236 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1236, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1233) {
          u_no_case(s1232);
        }
      }
    }
    if ((!d1229) && u_is_con(s1228, &n_list_46_Nil)) {
      d1229 = 1;
      {
        static struct u_object l_missing_32_operand_1237 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1237, v_at), "parse.ParseError");
      }
    }
    if (!d1229) {
      u_no_case(s1228);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1238 = v_items;
    int d1239 = 0;
    if ((!d1239) && (u_is_con(s1238, &n_list_46_Cons) && u_is_con(u_field(s1238, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1238, 0), 0);
      u_value v_i = u_field(u_field(s1238, 0), 1);
      u_value v_tail = u_field(s1238, 1);
      d1239 = 1;
      {
        u_value s1240 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op), f_parse_46_infix_of_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op));
        int d1241 = 0;
        if ((!d1241) && u_is_con(u_field(s1240, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1240, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1241 = 1;
            {
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_postfix_op_1(v_o), 2, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1241) && u_is_con(u_field(s1240, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1240, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1240, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1241 = 1;
            {
              u_value t1242;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1242 = v_p;
              } else {
                t1242 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1242, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)));
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_infix_op_1(v_o), 3, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1241) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op)))) {
            d1241 = 1;
            {
              static struct u_object l_undeclared_32_opera_1243 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1243, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1241) {
          d1241 = 1;
          {
            u_value t1244 = v_lhs;
            u_value t1245 = v_items;
            *r_a = t1244;
            *r_rest = t1245;
          }
        }
        if (!d1241) {
          u_no_case(s1240);
        }
      }
    }
    if (!d1239) {
      d1239 = 1;
      {
        u_value t1246 = v_lhs;
        u_value t1247 = v_items;
        *r_a = t1246;
        *r_rest = t1247;
      }
    }
    if (!d1239) {
      u_no_case(s1238);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1248 = v_rest;
    int d1249 = 0;
    if ((!d1249) && (u_is_con(s1248, &n_list_46_Cons) && u_is_con(u_field(s1248, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1248, 0), 0);
      u_value v_i = u_field(u_field(s1248, 0), 1);
      u_value t1252 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1252)) {
        t1252 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1252)) {
        d1249 = 1;
        {
          static struct u_object l_operator_32__1250 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1251 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1250, u_concat(v_op, &l__32_is_32_not_32_associat_1251)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1249) {
      d1249 = 1;
      {
      }
    }
    if (!d1249) {
      u_no_case(s1248);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1253 = v_fp;
  u_value t1254 = NULL;
  if ((!t1254) && u_is_con(s1253, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1253, 0), 1);
    t1254 = u_eq(v_q, v_p);
  }
  if (!t1254) {
    t1254 = u_mk_bool(0);
  }
  if (!t1254) {
    u_no_case(s1253);
  }
  return t1254;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1255 = v_ts;
  u_value t1256 = NULL;
  if ((!t1256) && (u_is_con(s1255, &n_list_46_Cons) && (u_is_con(u_field(s1255, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1255, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1255, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1255, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1255, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1255, 0), 1);
    t1256 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1256) && (u_is_con(s1255, &n_list_46_Cons) && u_is_con(u_field(s1255, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1255, 0);
    t1256 = v_t;
  }
  if ((!t1256) && u_is_con(s1255, &n_list_46_Nil)) {
    t1256 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1256) {
    static struct u_object l__40_invalid_41__1257 = U_LIT("(invalid)");
    t1256 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1257), v_ts);
  }
  if (!t1256) {
    u_no_case(s1255);
  }
  return t1256;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1258 = v_ts;
  u_value t1259 = NULL;
  if ((!t1259) && (u_is_con(s1258, &n_list_46_Cons) && (u_is_con(u_field(s1258, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1258, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1258, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1258, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1258, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1258, 0), 1);
    t1259 = v_cs;
  }
  if (!t1259) {
    t1259 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1259) {
    u_no_case(s1258);
  }
  return t1259;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1260 = U_LIT("->");
  u_value t1267;
  if (u_truth(u_eq(v_op, &l__45__62__1260))) {
    u_value t1261 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1262 = &t1261;
    u_value l1263 = v_l;
    while (u_is_con(l1263, &n_list_46_Cons)) {
      u_value v_t = u_field(l1263, 0);
      if (u_truth(u_mk_bool(1))) {
        e1262 = u_append(e1262, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1263 = u_field(l1263, 1);
    }
    t1267 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1261, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1264 = U_LIT("*");
    u_value t1266;
    if (u_truth(u_eq(v_op, &l__42__1264))) {
      static struct u_object l__40_product_41__1265 = U_LIT("(product)");
      t1266 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1265), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1266 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1267 = t1266;
  }
  return t1267;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1268 = v_t;
  u_value t1269 = NULL;
  if ((!t1269) && (u_is_con(s1268, &n_syntax_46_TSort) && (u_is_con(u_field(s1268, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1268, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1268, 0), 1), u_mk_lit("(invalid)")))))) {
    t1269 = u_mk_bool(1);
  }
  if ((!t1269) && u_is_con(s1268, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1268, 1);
    t1269 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1269) && u_is_con(s1268, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1268, 0);
    u_value v_r = u_field(s1268, 1);
    u_value t1270 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1270)) {
      t1270 = f_parse_46_has_invalid_1(v_r);
    }
    t1269 = t1270;
  }
  if ((!t1269) && u_is_con(s1268, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1268, 0);
    t1269 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1269) {
    t1269 = u_mk_bool(0);
  }
  if (!t1269) {
    u_no_case(s1268);
  }
  return t1269;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1271 = v_ts;
  u_value t1272 = NULL;
  if ((!t1272) && u_is_con(s1271, &n_list_46_Nil)) {
    t1272 = u_mk_bool(0);
  }
  if ((!t1272) && u_is_con(s1271, &n_list_46_Cons)) {
    u_value v_t = u_field(s1271, 0);
    u_value v_more = u_field(s1271, 1);
    u_value t1273 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1273)) {
      t1273 = f_parse_46_any_invalid_1(v_more);
    }
    t1272 = t1273;
  }
  if (!t1272) {
    u_no_case(s1271);
  }
  return t1272;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1274 = v_t;
  u_value t1275 = NULL;
  if ((!t1275) && u_is_con(s1274, &n_lex_46_TName)) {
    u_value v_n = u_field(s1274, 0);
    u_value t1276 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1276)) {
      t1276 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1275 = t1276;
  }
  if ((!t1275) && (u_is_con(s1274, &n_lex_46_TPunct) && u_equals(u_field(s1274, 0), u_mk_char(40)))) {
    t1275 = u_mk_bool(1);
  }
  if (!t1275) {
    t1275 = u_mk_bool(0);
  }
  if (!t1275) {
    u_no_case(s1274);
  }
  return t1275;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1277 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1277;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1278 = f_parse_46_type_one_1(v_r);
    *r_t = t1278;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1279 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1279, v_start), "parse.ParseError");
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
    u_value t1280 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1280;
    u_value t1281 = u_mk_bool(1);
    v_next = t1281;
    u_value t1282 = u_mk_bool(1);
    v_go = t1282;
    u_value t1291 = v_go;
    while (u_truth(t1291)) {
      {
        u_value s1283 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1284 = 0;
        if ((!d1284) && u_is_con(s1283, &n_option_46_Some)) {
          u_value v_op = u_field(s1283, 0);
          d1284 = 1;
          {
            u_value t1285 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1286 = u_mk_bool(1);
            v_rev = t1285;
            v_next = t1286;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1284) && u_is_con(s1283, &n_option_46_None)) {
          u_value t1289 = v_next;
          if (u_truth(t1289)) {
            t1289 = f_parse_46_starts_type_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1289)) {
            d1284 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1287 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1288 = u_mk_bool(0);
              v_rev = t1287;
              v_next = t1288;
            }
          }
        }
        if (!d1284) {
          d1284 = 1;
          {
            u_value t1290 = u_mk_bool(0);
            v_go = t1290;
          }
        }
        if (!d1284) {
          u_no_case(s1283);
        }
      }
      t1291 = v_go;
    }
    u_value s1292 = v_rev;
    int d1293 = 0;
    if ((!d1293) && u_is_con(s1292, &n_list_46_Nil)) {
      d1293 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1294 = U_LIT("type expected but ");
        static struct u_object l__32_found_1295 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1294, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1295)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1293) {
      d1293 = 1;
      {
        u_value t1296 = f_list_46_reverse_1(v_rev);
        *r_items = t1296;
      }
    }
    if (!d1293) {
      u_no_case(s1292);
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
    u_value t1300 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (u_truth(t1300)) {
      static struct u_object l_in_1297 = U_LIT("in");
      static struct u_object l_out_1298 = U_LIT("out");
      static struct u_object l_ref_1299 = U_LIT("ref");
      t1300 = f_list_46_contains_2(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_in_1297), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_out_1298), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_ref_1299), u_mk_con(&n_list_46_Nil, 0)))));
    }
    if (u_truth(t1300)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_type_sections(v_sc, &v_ins, &v_outs, &v_refs, r_ts);
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
        u_value t1301 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TProc, 3, v_ins, v_outs, v_refs), u_mk_con(&n_list_46_Nil, 0));
        *r_r = t1301;
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
              u_value t1302 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1302;
            }
          } else {
            if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Scope_46_params_1(v_sc)))) {
              {
                u_value t1303 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
                *r_r = t1303;
              }
            } else {
              {
                u_value t1304 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
                *r_r = t1304;
              }
            }
          }
        }
      }
    }
  }
}

void p_parse_46_type_sections(u_value v_sc, u_value *r_ins, u_value *r_outs, u_value *r_refs, u_value *r_ts) {
  {
    u_value v_first = NULL;
    u_value t1305 = u_mk_bool(1);
    v_first = t1305;
    u_value t1306 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1307 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1308 = u_mk_con(&n_list_46_Nil, 0);
    *r_ins = t1306;
    *r_outs = t1307;
    *r_refs = t1308;
    static struct u_object l_in_1309 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1309))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_types1(v_sc, r_ins, r_ts);
        u_value t1310 = u_mk_bool(0);
        v_first = t1310;
      }
    }
    static struct u_object l_out_1311 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1311, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_types1(v_sc, r_outs, r_ts);
        u_value t1312 = u_mk_bool(0);
        v_first = t1312;
      }
    }
    static struct u_object l_ref_1313 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1313, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_types1(v_sc, r_refs, r_ts);
      }
    }
  }
}

void p_parse_46_types1(u_value v_sc, u_value *r_us, u_value *r_ts) {
  {
    u_value v_t = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_type(v_sc, &v_t, r_ts);
    u_value t1314 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1314;
    u_value t1316 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1316)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1315 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
        v_rev = t1315;
      }
      t1316 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1317 = f_list_46_reverse_1(v_rev);
    *r_us = t1317;
  }
}

void p_parse_46_type_list(u_value v_sc, u_value v_close, u_value *r_r, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_t = NULL;
    u_value t1318 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1318;
    u_value t1319 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1319)) {
      t1319 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1319)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1320 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1320;
        u_value t1322 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1322)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1321 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1321;
          }
          t1322 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1323 = f_list_46_reverse_1(v_rev);
    *r_r = t1323;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_prefix_2(u_value v_op, u_value v_a) {
  u_value s1324 = v_a;
  u_value t1325 = NULL;
  if ((!t1325) && (u_is_con(s1324, &n_syntax_46_ELit) && u_is_con(u_field(s1324, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1324, 0), 0);
    static struct u_object l__45__1326 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1326))) {
      t1325 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1325) {
    t1325 = f_parse_46_expr_op1_2(v_op, v_a);
  }
  if (!t1325) {
    u_no_case(s1324);
  }
  return t1325;
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1327 = v_es;
  u_value t1328 = NULL;
  if ((!t1328) && u_is_con(s1327, &n_list_46_Nil)) {
    t1328 = v_rest;
  }
  if ((!t1328) && u_is_con(s1327, &n_list_46_Cons)) {
    u_value v_e = u_field(s1327, 0);
    u_value v_more = u_field(s1327, 1);
    static struct u_object l_Cons_1329 = U_LIT("Cons");
    t1328 = f_parse_46_expr_op2_3(&l_Cons_1329, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1328) {
    u_no_case(s1327);
  }
  return t1328;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1330 = v_t;
  u_value t1331 = NULL;
  if ((!t1331) && u_is_con(s1330, &n_lex_46_TName)) {
    u_value v_n = u_field(s1330, 0);
    static struct u_object l_let_1332 = U_LIT("let");
    static struct u_object l_if_1333 = U_LIT("if");
    static struct u_object l_lambda_1334 = U_LIT("lambda");
    static struct u_object l_match_1335 = U_LIT("match");
    static struct u_object l_true_1336 = U_LIT("true");
    static struct u_object l_false_1337 = U_LIT("false");
    u_value t1339 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1332, u_mk_con(&n_list_46_Cons, 2, &l_if_1333, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1334, u_mk_con(&n_list_46_Cons, 2, &l_match_1335, u_mk_con(&n_list_46_Cons, 2, &l_true_1336, u_mk_con(&n_list_46_Cons, 2, &l_false_1337, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1339)) {
      u_value t1338 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1338)) {
        t1338 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1339 = t1338;
    }
    t1331 = t1339;
  }
  if ((!t1331) && u_is_con(s1330, &n_lex_46_TNum)) {
    t1331 = u_mk_bool(1);
  }
  if ((!t1331) && u_is_con(s1330, &n_lex_46_TStr)) {
    t1331 = u_mk_bool(1);
  }
  if ((!t1331) && u_is_con(s1330, &n_lex_46_TChar)) {
    t1331 = u_mk_bool(1);
  }
  if ((!t1331) && u_is_con(s1330, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1330, 0);
    u_value t1340 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1340)) {
      t1340 = u_eq(v_c, u_mk_char(91));
    }
    u_value t1341 = t1340;
    if (!u_truth(t1341)) {
      t1341 = u_eq(v_c, u_mk_char(123));
    }
    t1331 = t1341;
  }
  if (!t1331) {
    t1331 = u_mk_bool(0);
  }
  if (!t1331) {
    u_no_case(s1330);
  }
  return t1331;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    static struct u_object l_raise_1342 = U_LIT("raise");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_raise_1342))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, r_e, r_ts);
        u_value t1343 = u_mk_con(&n_syntax_46_ERaise, 2, *r_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_e = t1343;
      }
    } else {
      {
        p_parse_46_operators(v_sc, r_e, r_ts);
        static struct u_object l_with_1344 = U_LIT("with");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_with_1344))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_update(v_sc, r_e, r_ts);
            u_value t1345 = f_parse_46_at_update_1(*r_ts);
            while (u_truth(t1345)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_update(v_sc, r_e, r_ts);
              }
              t1345 = f_parse_46_at_update_1(*r_ts);
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
    static struct u_object l__58__61__1346 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1346), r_ts);
    p_parse_46_operators(v_sc, &v_a, r_ts);
    static struct u_object l__46__1347 = U_LIT(".");
    static struct u_object l__58__61__1348 = U_LIT(":=");
    u_value t1349 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1347, u_concat(v_f, &l__58__61__1348))), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1349;
  }
}

u_value f_parse_46_at_update_1(u_value v_ts) {
  u_value s1350 = v_ts;
  u_value t1351 = NULL;
  if ((!t1351) && (u_is_con(s1350, &n_list_46_Cons) && (u_is_con(u_field(u_field(s1350, 0), 0), &n_lex_46_TPunct) && (u_equals(u_field(u_field(u_field(s1350, 0), 0), 0), u_mk_char(44)) && (u_is_con(u_field(s1350, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s1350, 1), 0), 0), &n_lex_46_TName) && (u_is_con(u_field(u_field(s1350, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(u_field(s1350, 1), 1), 0), 0), &n_lex_46_TSym) && u_equals(u_field(u_field(u_field(u_field(u_field(s1350, 1), 1), 0), 0), 0), u_mk_lit(":=")))))))))) {
    t1351 = u_mk_bool(1);
  }
  if (!t1351) {
    t1351 = u_mk_bool(0);
  }
  if (!t1351) {
    u_no_case(s1350);
  }
  return t1351;
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
    u_value t1352 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1352;
    u_value t1353 = u_mk_bool(1);
    v_next = t1353;
    u_value t1354 = u_mk_bool(1);
    v_go = t1354;
    u_value t1363 = v_go;
    while (u_truth(t1363)) {
      {
        u_value s1355 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1356 = 0;
        if ((!d1356) && u_is_con(s1355, &n_option_46_Some)) {
          u_value v_op = u_field(s1355, 0);
          d1356 = 1;
          {
            u_value t1357 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1358 = u_mk_bool(1);
            v_rev = t1357;
            v_next = t1358;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1356) && u_is_con(s1355, &n_option_46_None)) {
          u_value t1361 = v_next;
          if (u_truth(t1361)) {
            t1361 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1361)) {
            d1356 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1359 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1360 = u_mk_bool(0);
              v_rev = t1359;
              v_next = t1360;
            }
          }
        }
        if (!d1356) {
          d1356 = 1;
          {
            u_value t1362 = u_mk_bool(0);
            v_go = t1362;
          }
        }
        if (!d1356) {
          u_no_case(s1355);
        }
      }
      t1363 = v_go;
    }
    u_value s1364 = v_rev;
    int d1365 = 0;
    if ((!d1365) && u_is_con(s1364, &n_list_46_Nil)) {
      d1365 = 1;
      {
        static struct u_object l_expression_32_expec_1366 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1367 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1366, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1367)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1365) {
      d1365 = 1;
      {
        u_value t1368 = f_list_46_reverse_1(v_rev);
        *r_items = t1368;
      }
    }
    if (!d1365) {
      u_no_case(s1364);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1369 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1369;
    u_value t1371 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1371)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1370 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1370;
      }
      t1371 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1372 = f_list_46_reverse_1(v_rev);
    *r_es = t1372;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1373 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1373;
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
    u_value t1374 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1374)) {
      t1374 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1378 = t1374;
    while (u_truth(t1378)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1375 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1375;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1376 = U_LIT(".");
            u_value t1377 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1376, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1377;
          }
        }
      }
      u_value t1379 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1379)) {
        t1379 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1378 = t1379;
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
    u_value s1380 = f_parse_46_head_token_1(*r_ts);
    int d1381 = 0;
    if ((!d1381) && (u_is_con(s1380, &n_lex_46_TName) && u_equals(u_field(s1380, 0), u_mk_lit("let")))) {
      d1381 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1381) && (u_is_con(s1380, &n_lex_46_TName) && u_equals(u_field(s1380, 0), u_mk_lit("if")))) {
      d1381 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1382 = U_LIT("then");
        p_parse_46_keyword(&l_then_1382, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1383 = U_LIT("else");
        p_parse_46_keyword(&l_else_1383, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1384 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1384;
      }
    }
    if ((!d1381) && (u_is_con(s1380, &n_lex_46_TName) && u_equals(u_field(s1380, 0), u_mk_lit("lambda")))) {
      d1381 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1381) && (u_is_con(s1380, &n_lex_46_TName) && u_equals(u_field(s1380, 0), u_mk_lit("match")))) {
      d1381 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1385 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1385;
      }
    }
    if ((!d1381) && (u_is_con(s1380, &n_lex_46_TName) && u_equals(u_field(s1380, 0), u_mk_lit("true")))) {
      d1381 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1386 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1386;
      }
    }
    if ((!d1381) && (u_is_con(s1380, &n_lex_46_TName) && u_equals(u_field(s1380, 0), u_mk_lit("false")))) {
      d1381 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1387 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1387;
      }
    }
    if ((!d1381) && u_is_con(s1380, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1380, 0);
      d1381 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1388 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1388;
      }
    }
    if ((!d1381) && u_is_con(s1380, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1380, 0);
      d1381 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1389 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1389;
      }
    }
    if ((!d1381) && u_is_con(s1380, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1380, 0);
      d1381 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1390 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1390;
      }
    }
    if ((!d1381) && (u_is_con(s1380, &n_lex_46_TPunct) && u_equals(u_field(s1380, 0), u_mk_char(40)))) {
      d1381 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1391 = v_es;
        int d1392 = 0;
        if ((!d1392) && (u_is_con(s1391, &n_list_46_Cons) && u_is_con(u_field(s1391, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1391, 0);
          d1392 = 1;
          {
            u_value t1393 = v_x;
            *r_e = t1393;
          }
        }
        if (!d1392) {
          d1392 = 1;
          {
            u_value t1394 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1394;
          }
        }
        if (!d1392) {
          u_no_case(s1391);
        }
      }
    }
    if ((!d1381) && (u_is_con(s1380, &n_lex_46_TPunct) && u_equals(u_field(s1380, 0), u_mk_char(91)))) {
      d1381 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1381) && (u_is_con(s1380, &n_lex_46_TPunct) && u_equals(u_field(s1380, 0), u_mk_char(123)))) {
      d1381 = 1;
      {
        p_parse_46_parse_braces(v_sc, r_e, r_ts);
      }
    }
    if ((!d1381) && u_is_con(s1380, &n_lex_46_TName)) {
      u_value v_n = u_field(s1380, 0);
      u_value t1396 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1396)) {
        t1396 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t1396)) {
        d1381 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1395 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1395;
        }
      }
    }
    if (!d1381) {
      d1381 = 1;
      {
        static struct u_object l_expression_32_expec_1397 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1398 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1397, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1398)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1381) {
      u_no_case(s1380);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1399 = v_ps;
  u_value t1400 = NULL;
  if ((!t1400) && u_is_con(s1399, &n_list_46_Nil)) {
    t1400 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1400) && (u_is_con(s1399, &n_list_46_Cons) && u_is_con(u_field(s1399, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1399, 0), 0);
    u_value v_more = u_field(s1399, 1);
    u_value s1401 = f_parse_46_pattern_vars_1(v_more);
    u_value t1402 = NULL;
    if ((!t1402) && u_is_con(s1401, &n_option_46_Some)) {
      u_value v_xs = u_field(s1401, 0);
      t1402 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1402) && u_is_con(s1401, &n_option_46_None)) {
      t1402 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1402) {
      u_no_case(s1401);
    }
    t1400 = t1402;
  }
  if (!t1400) {
    t1400 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1400) {
    u_no_case(s1399);
  }
  return t1400;
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
    static struct u_object l__58__61__1403 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1403), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1404 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1405 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1404;
    v_es = t1405;
    u_value t1409 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1409)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1406 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1406), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1407 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1408 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1407;
        v_es = t1408;
      }
      t1409 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1410 = U_LIT("in");
    p_parse_46_keyword(&l_in_1410, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1411 = f_list_46_reverse_1(v_ps);
    u_value t1412 = f_list_46_reverse_1(v_es);
    v_ps = t1411;
    v_es = t1412;
    u_value s1413 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1414 = 0;
    if ((!d1414) && u_is_con(u_field(s1413, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1413, 0), 0);
      d1414 = 1;
      {
        u_value t1415 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1415;
      }
    }
    if ((!d1414) && (u_is_con(u_field(s1413, 0), &n_option_46_None) && (u_is_con(u_field(s1413, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1413, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1413, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1413, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1413, 1), 0);
      u_value v_c = u_field(u_field(s1413, 2), 0);
      d1414 = 1;
      {
        u_value t1416 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1416;
      }
    }
    if (!d1414) {
      d1414 = 1;
      {
        u_value t1417 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1417;
      }
    }
    if (!d1414) {
      u_no_case(s1413);
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
        p_parse_46_formal_sections(v_sc, &v_ins, &v_outs, &v_refs, r_ts);
        static struct u_object l__61__62__1418 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1418), r_ts);
        p_parse_46_parse_body(v_sc, &v_body, r_ts);
        static struct u_object l_end_1419 = U_LIT("end");
        p_parse_46_keyword(&l_end_1419, r_ts);
        u_value t1420 = u_mk_con(&n_syntax_46_EProc, 4, v_ins, v_outs, v_refs, v_body);
        *r_e = t1420;
      }
    } else {
      {
        p_parse_46_lambda_function(v_sc, r_e, r_ts);
      }
    }
  }
}

u_value f_parse_46_at_section_1(u_value v_ts) {
  static struct u_object l_in_1421 = U_LIT("in");
  u_value t1423 = f_parse_46_at_keyword_2(v_ts, &l_in_1421);
  if (!u_truth(t1423)) {
    static struct u_object l_out_1422 = U_LIT("out");
    t1423 = f_parse_46_at_keyword_2(v_ts, &l_out_1422);
  }
  u_value t1425 = t1423;
  if (!u_truth(t1425)) {
    static struct u_object l_ref_1424 = U_LIT("ref");
    t1425 = f_parse_46_at_keyword_2(v_ts, &l_ref_1424);
  }
  return t1425;
}

void p_parse_46_lambda_function(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    u_value t1426 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1426;
    static struct u_object l__61__62__1427 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1427)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1428 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1428;
        u_value t1430 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1430)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1429 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1429;
          }
          t1430 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1431 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1431), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1432 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1432;
  }
}

void p_parse_46_parse_braces(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_at = NULL;
    u_value v_k = NULL;
    u_value v_v = NULL;
    u_value v_entries = NULL;
    u_value v_rev = NULL;
    u_value t1433 = f_parse_46_head_offset_1(*r_ts);
    v_at = t1433;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_expr(v_sc, &v_k, r_ts);
    static struct u_object l__61__62__1434 = U_LIT("=>");
    u_value t1435 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1434));
    v_entries = t1435;
    if (u_truth(v_entries)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_v, r_ts);
      }
    }
    static struct u_object l_for_1436 = U_LIT("for");
    u_value t1437 = f_parse_46_at_keyword_2(*r_ts, &l_for_1436);
    if (u_truth(t1437)) {
      t1437 = v_entries;
    }
    if (u_truth(t1437)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_comprehension(v_sc, v_k, v_v, r_e, r_ts);
      }
    } else {
      static struct u_object l_for_1438 = U_LIT("for");
      if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1438))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_set_comprehension(v_sc, v_k, r_e, r_ts);
        }
      } else {
        {
          u_value t1439;
          if (u_truth(v_entries)) {
            t1439 = u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))));
          } else {
            t1439 = v_k;
          }
          u_value t1440 = u_mk_con(&n_list_46_Cons, 2, t1439, u_mk_con(&n_list_46_Nil, 0));
          v_rev = t1440;
          u_value t1445 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          while (u_truth(t1445)) {
            {
              p_parse_46_advance(r_ts);
              p_parse_46_parse_expr(v_sc, &v_k, r_ts);
              static struct u_object l__61__62__1441 = U_LIT("=>");
              if (u_truth(f_prelude_46__33__61__2(v_entries, u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1441))))) {
                {
                  static struct u_object l_a_32_literal_32__123__46__46__46__125__32__1442 = U_LIT("a literal {...} has either elements a or entries k => v");
                  u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_a_32_literal_32__123__46__46__46__125__32__1442, v_at), "parse.ParseError");
                }
              }
              if (u_truth(v_entries)) {
                {
                  p_parse_46_advance(r_ts);
                  p_parse_46_parse_expr(v_sc, &v_v, r_ts);
                  u_value t1443 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)))), v_rev);
                  v_rev = t1443;
                }
              } else {
                {
                  u_value t1444 = u_mk_con(&n_list_46_Cons, 2, v_k, v_rev);
                  v_rev = t1444;
                }
              }
            }
            t1445 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          }
          u_value t1452;
          if (u_truth(v_entries)) {
            static struct u_object l_map_1446 = U_LIT("map");
            static struct u_object l_put_all_1447 = U_LIT("put_all");
            static struct u_object l_map_1448 = U_LIT("map");
            static struct u_object l_empty_1449 = U_LIT("empty");
            t1452 = f_parse_46_in_library_3(&l_map_1446, &l_put_all_1447, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1448, &l_empty_1449, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
          } else {
            static struct u_object l_set_1450 = U_LIT("set");
            static struct u_object l_from_list_1451 = U_LIT("from_list");
            t1452 = f_parse_46_in_library_3(&l_set_1450, &l_from_list_1451, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Nil, 0)));
          }
          u_value t1453 = t1452;
          *r_e = t1453;
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
  static struct u_object l_Nil_1454 = U_LIT("Nil");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1454), u_mk_con(&n_syntax_46_THole, 0)));
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
    u_value t1455 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1455;
    u_value t1456 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1456;
    u_value t1457 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1457;
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l_in_1458 = U_LIT("in");
    p_parse_46_keyword(&l_in_1458, r_ts);
    p_parse_46_parse_expr(v_sc, &v_l, r_ts);
    static struct u_object l_set_1459 = U_LIT("set");
    static struct u_object l_elements_1460 = U_LIT("elements");
    u_value t1461 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    u_value t1462 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1459, &l_elements_1460, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
    v_xs = t1461;
    v_ls = t1462;
    u_value t1468 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1468)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l_in_1463 = U_LIT("in");
        p_parse_46_keyword(&l_in_1463, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_set_1464 = U_LIT("set");
        static struct u_object l_elements_1465 = U_LIT("elements");
        u_value t1466 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
        u_value t1467 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1464, &l_elements_1465, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), v_ls);
        v_xs = t1466;
        v_ls = t1467;
      }
      t1468 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1469 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1469))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
      }
    }
    static struct u_object l_set_1470 = U_LIT("set");
    static struct u_object l_from_list_1471 = U_LIT("from_list");
    u_value t1472 = f_parse_46_in_library_3(&l_set_1470, &l_from_list_1471, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, v_r, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Nil, 0)));
    *r_e = t1472;
  }
}

void p_parse_46_map_comprehension(u_value v_sc, u_value v_k, u_value v_v, u_value *r_e, u_value *r_ts) {
  {
    u_value v_gs = NULL;
    u_value v_ls = NULL;
    u_value v_c = NULL;
    u_value t1473 = u_mk_con(&n_list_46_Nil, 0);
    v_gs = t1473;
    u_value t1474 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1474;
    u_value t1475 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1475;
    p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
    u_value t1476 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1476)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
      }
      t1476 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1477 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1477))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
        u_value t1478 = f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), v_c);
        v_c = t1478;
      }
    }
    static struct u_object l_map_1479 = U_LIT("map");
    static struct u_object l_put_all_1480 = U_LIT("put_all");
    static struct u_object l_map_1481 = U_LIT("map");
    static struct u_object l_empty_1482 = U_LIT("empty");
    u_value t1483 = f_parse_46_in_library_3(&l_map_1479, &l_put_all_1480, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))))), f_list_46_reverse_1(f_parse_46_hidden_vars_1(v_gs)), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1481, &l_empty_1482, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1483;
  }
}

void p_parse_46_map_generator(u_value v_sc, u_value *r_gs, u_value *r_ls, u_value *r_ts) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value v_m = NULL;
    p_parse_46_parse_plain(v_sc, &v_a, r_ts);
    static struct u_object l__61__62__1484 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1484), r_ts);
    p_parse_46_parse_plain(v_sc, &v_b, r_ts);
    static struct u_object l_in_1485 = U_LIT("in");
    p_parse_46_keyword(&l_in_1485, r_ts);
    p_parse_46_parse_expr(v_sc, &v_m, r_ts);
    static struct u_object l_map_1486 = U_LIT("map");
    static struct u_object l_entries_1487 = U_LIT("entries");
    u_value t1488 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1486, &l_entries_1487, u_mk_con(&n_list_46_Cons, 2, f_parse_46_unpack_2(f_list_46_reverse_1(*r_gs), v_m), u_mk_con(&n_list_46_Nil, 0))), *r_ls);
    *r_ls = t1488;
    static struct u_object l__40_entry_32__1489 = U_LIT("(entry ");
    static struct u_object l__41__1490 = U_LIT(")");
    u_value t1491 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(3, v_a, v_b, u_concat(&l__40_entry_32__1489, u_concat(u_str_show(u_add(f_list_46_length_1(*r_gs), u_mk_int(1))), &l__41__1490))), *r_gs);
    *r_gs = t1491;
  }
}

u_value f_parse_46_hidden_vars_1(u_value v_gs) {
  u_value s1492 = v_gs;
  u_value t1493 = NULL;
  if ((!t1493) && u_is_con(s1492, &n_list_46_Nil)) {
    t1493 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1493) && u_is_con(s1492, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s1492, 0), 2);
    u_value v_rest = u_field(s1492, 1);
    t1493 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0)), f_parse_46_hidden_vars_1(v_rest));
  }
  if (!t1493) {
    u_no_case(s1492);
  }
  return t1493;
}

u_value f_parse_46_unpack_2(u_value v_gs, u_value v_e) {
  u_value s1494 = v_gs;
  u_value t1495 = NULL;
  if ((!t1495) && u_is_con(s1494, &n_list_46_Nil)) {
    t1495 = v_e;
  }
  if ((!t1495) && u_is_con(s1494, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s1494, 0), 0);
    u_value v_v = u_field(u_field(s1494, 0), 1);
    u_value v_h = u_field(u_field(s1494, 0), 2);
    u_value v_rest = u_field(s1494, 1);
    t1495 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_k), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_v), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_parse_46_unpack_2(v_rest, v_e)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1495) {
    u_no_case(s1494);
  }
  return t1495;
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
        static struct u_object l_Nil_1496 = U_LIT("Nil");
        u_value t1497 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1496), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1497;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1498 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1498))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1499 = U_LIT("in");
            p_parse_46_keyword(&l_in_1499, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1500 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1501 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1500;
            v_lists = t1501;
            u_value t1505 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1505)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1502 = U_LIT("in");
                p_parse_46_keyword(&l_in_1502, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1503 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1504 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1503;
                v_lists = t1504;
              }
              t1505 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1506 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1506;
            static struct u_object l_if_1507 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1507))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1508 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1508;
          }
        } else {
          {
            u_value t1509 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1509;
            u_value t1511 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1511)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1510 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1510;
              }
              t1511 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1512 = U_LIT("Nil");
            u_value t1513 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1512), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1513;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1514 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1514;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1515 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1515))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1516 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1516;
      }
    } else {
      {
        u_value t1517 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1517;
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
    u_value t1518 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1518;
    static struct u_object l_case_1519 = U_LIT("case");
    u_value t1522 = f_parse_46_at_keyword_2(*r_ts, &l_case_1519);
    while (u_truth(t1522)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1520 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1520), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1521 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1521;
      }
      static struct u_object l_case_1523 = U_LIT("case");
      t1522 = f_parse_46_at_keyword_2(*r_ts, &l_case_1523);
    }
    static struct u_object l_else_1524 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1524))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1525 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1525;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1526 = U_LIT("end");
    p_parse_46_keyword(&l_end_1526, r_ts);
    u_value t1527 = f_list_46_reverse_1(v_rev);
    *r_cs = t1527;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1528 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1528))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1529 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1529, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
  }
}

u_value f_parse_46_pat_op1_2(u_value v_op, u_value v_p) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_pat_prefix_2(u_value v_op, u_value v_p) {
  u_value s1530 = v_p;
  u_value t1531 = NULL;
  if ((!t1531) && (u_is_con(s1530, &n_syntax_46_PLit) && u_is_con(u_field(s1530, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1530, 0), 0);
    static struct u_object l__45__1532 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1532))) {
      t1531 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1531) {
    t1531 = f_parse_46_pat_op1_2(v_op, v_p);
  }
  if (!t1531) {
    u_no_case(s1530);
  }
  return t1531;
}

u_value f_parse_46_pat_op2_3(u_value v_op, u_value v_l, u_value v_r) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_pats_2(u_value v_ps, u_value v_rest) {
  u_value s1533 = v_ps;
  u_value t1534 = NULL;
  if ((!t1534) && u_is_con(s1533, &n_list_46_Nil)) {
    t1534 = v_rest;
  }
  if ((!t1534) && u_is_con(s1533, &n_list_46_Cons)) {
    u_value v_p = u_field(s1533, 0);
    u_value v_more = u_field(s1533, 1);
    static struct u_object l_Cons_1535 = U_LIT("Cons");
    t1534 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1535), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1534) {
    u_no_case(s1533);
  }
  return t1534;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1536 = v_t;
  u_value t1537 = NULL;
  if ((!t1537) && u_is_con(s1536, &n_lex_46_TName)) {
    u_value v_n = u_field(s1536, 0);
    static struct u_object l___1538 = U_LIT("_");
    static struct u_object l_true_1539 = U_LIT("true");
    static struct u_object l_false_1540 = U_LIT("false");
    u_value t1542 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1538, u_mk_con(&n_list_46_Cons, 2, &l_true_1539, u_mk_con(&n_list_46_Cons, 2, &l_false_1540, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1542)) {
      u_value t1541 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1541)) {
        t1541 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1542 = t1541;
    }
    t1537 = t1542;
  }
  if ((!t1537) && u_is_con(s1536, &n_lex_46_TNum)) {
    t1537 = u_mk_bool(1);
  }
  if ((!t1537) && u_is_con(s1536, &n_lex_46_TStr)) {
    t1537 = u_mk_bool(1);
  }
  if ((!t1537) && u_is_con(s1536, &n_lex_46_TChar)) {
    t1537 = u_mk_bool(1);
  }
  if ((!t1537) && u_is_con(s1536, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1536, 0);
    u_value t1543 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1543)) {
      t1543 = u_eq(v_c, u_mk_char(91));
    }
    t1537 = t1543;
  }
  if (!t1537) {
    t1537 = u_mk_bool(0);
  }
  if (!t1537) {
    u_no_case(s1536);
  }
  return t1537;
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
    u_value t1544 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1544;
    u_value t1545 = u_mk_bool(1);
    v_next = t1545;
    u_value t1546 = u_mk_bool(1);
    v_go = t1546;
    u_value t1555 = v_go;
    while (u_truth(t1555)) {
      {
        u_value s1547 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1548 = 0;
        if ((!d1548) && u_is_con(s1547, &n_option_46_Some)) {
          u_value v_op = u_field(s1547, 0);
          d1548 = 1;
          {
            u_value t1549 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1550 = u_mk_bool(1);
            v_rev = t1549;
            v_next = t1550;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1548) && u_is_con(s1547, &n_option_46_None)) {
          u_value t1553 = v_next;
          if (u_truth(t1553)) {
            t1553 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1553)) {
            d1548 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1551 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1552 = u_mk_bool(0);
              v_rev = t1551;
              v_next = t1552;
            }
          }
        }
        if (!d1548) {
          d1548 = 1;
          {
            u_value t1554 = u_mk_bool(0);
            v_go = t1554;
          }
        }
        if (!d1548) {
          u_no_case(s1547);
        }
      }
      t1555 = v_go;
    }
    u_value s1556 = v_rev;
    int d1557 = 0;
    if ((!d1557) && u_is_con(s1556, &n_list_46_Nil)) {
      d1557 = 1;
      {
        static struct u_object l_pattern_32_expected_1558 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1559 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1558, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1559)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1557) {
      d1557 = 1;
      {
        u_value t1560 = f_list_46_reverse_1(v_rev);
        *r_items = t1560;
      }
    }
    if (!d1557) {
      u_no_case(s1556);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1561 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1561;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1562 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1562;
        u_value t1564 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1564)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1563 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1563;
          }
          t1564 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1565 = f_list_46_reverse_1(v_rev);
    *r_ps = t1565;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1566 = f_parse_46_head_token_1(*r_ts);
    int d1567 = 0;
    if ((!d1567) && (u_is_con(s1566, &n_lex_46_TName) && u_equals(u_field(s1566, 0), u_mk_lit("_")))) {
      d1567 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1568 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1568;
      }
    }
    if ((!d1567) && (u_is_con(s1566, &n_lex_46_TName) && u_equals(u_field(s1566, 0), u_mk_lit("true")))) {
      d1567 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1569 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1569;
      }
    }
    if ((!d1567) && (u_is_con(s1566, &n_lex_46_TName) && u_equals(u_field(s1566, 0), u_mk_lit("false")))) {
      d1567 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1570 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1570;
      }
    }
    if ((!d1567) && u_is_con(s1566, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1566, 0);
      d1567 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1571 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1571;
      }
    }
    if ((!d1567) && u_is_con(s1566, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1566, 0);
      d1567 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1572 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1572;
      }
    }
    if ((!d1567) && u_is_con(s1566, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1566, 0);
      d1567 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1573 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1573;
      }
    }
    if ((!d1567) && (u_is_con(s1566, &n_lex_46_TPunct) && u_equals(u_field(s1566, 0), u_mk_char(40)))) {
      d1567 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1574 = v_ps;
        int d1575 = 0;
        if ((!d1575) && (u_is_con(s1574, &n_list_46_Cons) && u_is_con(u_field(s1574, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1574, 0);
          d1575 = 1;
          {
            u_value t1576 = v_q;
            *r_p = t1576;
          }
        }
        if (!d1575) {
          d1575 = 1;
          {
            u_value t1577 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1577;
          }
        }
        if (!d1575) {
          u_no_case(s1574);
        }
      }
    }
    if ((!d1567) && (u_is_con(s1566, &n_lex_46_TPunct) && u_equals(u_field(s1566, 0), u_mk_char(91)))) {
      d1567 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1578 = U_LIT("Nil");
            u_value t1579 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1578), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1579;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1580 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1580;
            u_value t1582 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1582)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1581 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1581;
              }
              t1582 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1583 = U_LIT("Nil");
            u_value t1584 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1583), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1584;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1585 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1585;
          }
        }
      }
    }
    if (!d1567) {
      d1567 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1586 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1586;
          }
        } else {
          {
            u_value t1587 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1587;
          }
        }
      }
    }
    if (!d1567) {
      u_no_case(s1566);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1588 = v_t;
  u_value t1589 = NULL;
  if ((!t1589) && u_is_con(s1588, &n_lex_46_TName)) {
    u_value v_n = u_field(s1588, 0);
    static struct u_object l_skip_1590 = U_LIT("skip");
    static struct u_object l_if_1591 = U_LIT("if");
    static struct u_object l_while_1592 = U_LIT("while");
    static struct u_object l_call_1593 = U_LIT("call");
    static struct u_object l_match_1594 = U_LIT("match");
    static struct u_object l_assert_1595 = U_LIT("assert");
    static struct u_object l_assign_1596 = U_LIT("assign");
    static struct u_object l_raise_1597 = U_LIT("raise");
    static struct u_object l_try_1598 = U_LIT("try");
    t1589 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1590, u_mk_con(&n_list_46_Cons, 2, &l_if_1591, u_mk_con(&n_list_46_Cons, 2, &l_while_1592, u_mk_con(&n_list_46_Cons, 2, &l_call_1593, u_mk_con(&n_list_46_Cons, 2, &l_match_1594, u_mk_con(&n_list_46_Cons, 2, &l_assert_1595, u_mk_con(&n_list_46_Cons, 2, &l_assign_1596, u_mk_con(&n_list_46_Cons, 2, &l_raise_1597, u_mk_con(&n_list_46_Cons, 2, &l_try_1598, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1589) {
    t1589 = u_mk_bool(0);
  }
  if (!t1589) {
    u_no_case(s1588);
  }
  return t1589;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1599 = v_cs;
  u_value t1600 = NULL;
  if ((!t1600) && u_is_con(s1599, &n_list_46_Nil)) {
    t1600 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1600) && (u_is_con(s1599, &n_list_46_Cons) && u_is_con(u_field(s1599, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1599, 0), 0);
    u_value v_g = u_field(u_field(s1599, 0), 1);
    u_value v_b = u_field(u_field(s1599, 0), 2);
    u_value v_more = u_field(s1599, 1);
    t1600 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1600) {
    u_no_case(s1599);
  }
  return t1600;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1601 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1601;
    u_value t1602 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1602;
    u_value t1603 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1603;
    static struct u_object l_local_1604 = U_LIT("local");
    u_value t1606 = f_parse_46_at_keyword_2(*r_ts, &l_local_1604);
    while (u_truth(t1606)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1605 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1605)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1605 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1607 = U_LIT("local");
      t1606 = f_parse_46_at_keyword_2(*r_ts, &l_local_1607);
    }
    u_value t1609 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1609)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1608 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1608;
      }
      t1609 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1610 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1610;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
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
    static struct u_object l__58__61__1613 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1613)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1614 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1614;
      }
    }
    u_value t1615 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1615;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1616 = f_parse_46_head_token_1(*r_ts);
    int d1617 = 0;
    if ((!d1617) && (u_is_con(s1616, &n_lex_46_TName) && u_equals(u_field(s1616, 0), u_mk_lit("skip")))) {
      d1617 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1618 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1618;
      }
    }
    if ((!d1617) && (u_is_con(s1616, &n_lex_46_TName) && u_equals(u_field(s1616, 0), u_mk_lit("if")))) {
      d1617 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1617) && (u_is_con(s1616, &n_lex_46_TName) && u_equals(u_field(s1616, 0), u_mk_lit("while")))) {
      d1617 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1619 = U_LIT("end");
        p_parse_46_keyword(&l_end_1619, r_ts);
        u_value t1620 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1620;
      }
    }
    if ((!d1617) && (u_is_con(s1616, &n_lex_46_TName) && u_equals(u_field(s1616, 0), u_mk_lit("call")))) {
      d1617 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1617) && (u_is_con(s1616, &n_lex_46_TName) && u_equals(u_field(s1616, 0), u_mk_lit("match")))) {
      d1617 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1621 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1621;
      }
    }
    if ((!d1617) && (u_is_con(s1616, &n_lex_46_TName) && u_equals(u_field(s1616, 0), u_mk_lit("assert")))) {
      d1617 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1622 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1622;
      }
    }
    if ((!d1617) && (u_is_con(s1616, &n_lex_46_TName) && u_equals(u_field(s1616, 0), u_mk_lit("assign")))) {
      d1617 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1617) && (u_is_con(s1616, &n_lex_46_TName) && u_equals(u_field(s1616, 0), u_mk_lit("raise")))) {
      d1617 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1623 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1623;
      }
    }
    if ((!d1617) && (u_is_con(s1616, &n_lex_46_TName) && u_equals(u_field(s1616, 0), u_mk_lit("try")))) {
      d1617 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1624 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1624, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1625 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1625;
      }
    }
    if (!d1617) {
      d1617 = 1;
      {
        static struct u_object l_statement_32_expect_1626 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1627 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1626, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1627)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1617) {
      u_no_case(s1616);
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
    static struct u_object l_else_1628 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1628)))) {
      {
        static struct u_object l_end_1629 = U_LIT("end");
        p_parse_46_keyword(&l_end_1629, r_ts);
        u_value t1630 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1630;
      }
    } else {
      static struct u_object l_if_1631 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1631)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1632 = U_LIT("end");
          p_parse_46_keyword(&l_end_1632, r_ts);
        }
      }
    }
    u_value t1633 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1633;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1634 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1634), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1635 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1635;
    u_value t1638 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1638)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1636 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1636), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1637 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1637;
      }
      t1638 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1639 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1639;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1640 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1640;
    static struct u_object l_case_1641 = U_LIT("case");
    u_value t1644 = f_parse_46_at_keyword_2(*r_ts, &l_case_1641);
    while (u_truth(t1644)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1642 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1642), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1643 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1643;
      }
      static struct u_object l_case_1645 = U_LIT("case");
      t1644 = f_parse_46_at_keyword_2(*r_ts, &l_case_1645);
    }
    static struct u_object l_else_1646 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1646))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1647 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1647;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1648 = U_LIT("end");
    p_parse_46_keyword(&l_end_1648, r_ts);
    u_value t1649 = f_list_46_reverse_1(v_rev);
    *r_cs = t1649;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1651;
  if (u_truth(v_first)) {
    t1651 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1650 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1650)) {
      t1650 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1651 = t1650;
  }
  return t1651;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1653 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1653)) {
    static struct u_object l_out_1652 = U_LIT("out");
    t1653 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1652));
  }
  u_value t1655 = t1653;
  if (u_truth(t1655)) {
    static struct u_object l_ref_1654 = U_LIT("ref");
    t1655 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1654));
  }
  return t1655;
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
    u_value t1656 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1657 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1658 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1659 = u_mk_bool(1);
    v_ins = t1656;
    v_outs = t1657;
    v_refs = t1658;
    v_first = t1659;
    static struct u_object l_in_1660 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1660))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1661 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
        u_value t1662 = u_mk_bool(0);
        v_ins = t1661;
        v_first = t1662;
        u_value t1664 = f_parse_46_item_follows_1(*r_ts);
        while (u_truth(t1664)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_expr(v_sc, &v_e, r_ts);
            u_value t1663 = u_mk_con(&n_list_46_Cons, 2, v_e, v_ins);
            v_ins = t1663;
          }
          t1664 = f_parse_46_item_follows_1(*r_ts);
        }
      }
    }
    static struct u_object l_out_1665 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1665, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_outs, r_ts);
        u_value t1666 = u_mk_bool(0);
        v_first = t1666;
      }
    }
    static struct u_object l_ref_1667 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1667, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    u_value t1668 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), f_list_46_reverse_1(v_ins), v_outs, v_refs);
    *r_s = t1668;
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
    u_value t1669 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1669;
    u_value t1671 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1671)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1670 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_rev);
        v_rev = t1670;
      }
      t1671 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1672 = f_list_46_reverse_1(v_rev);
    *r_xs = t1672;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1673 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1673;
    static struct u_object l__58__1674 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1674)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1675 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1675;
  }
}

void p_parse_46_parse_formals1(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_formal(v_sc, &v_x, r_ts);
    u_value t1676 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1676;
    u_value t1678 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1678)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formal(v_sc, &v_x, r_ts);
        u_value t1677 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1677;
      }
      t1678 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1679 = f_list_46_reverse_1(v_rev);
    *r_xs = t1679;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1680 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1680;
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
  u_value s1681 = v_cs;
  u_value t1682 = NULL;
  if ((!t1682) && u_is_con(s1681, &n_list_46_Nil)) {
    t1682 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1682) && (u_is_con(s1681, &n_list_46_Cons) && u_is_con(u_field(s1681, 0), &n_syntax_46_CEval))) {
    u_value v_more = u_field(s1681, 1);
    t1682 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1682) && (u_is_con(s1681, &n_list_46_Cons) && u_is_con(u_field(s1681, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1681, 1);
    t1682 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1682) && (u_is_con(s1681, &n_list_46_Cons) && u_is_con(u_field(s1681, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1681, 1);
    t1682 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1682) && u_is_con(s1681, &n_list_46_Cons)) {
    u_value v_c = u_field(s1681, 0);
    u_value v_more = u_field(s1681, 1);
    t1682 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1682) {
    u_no_case(s1681);
  }
  return t1682;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1683 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1684 = &t1683;
  u_value l1685 = v_cs;
  while (u_is_con(l1685, &n_list_46_Cons)) {
    u_value v_c = u_field(l1685, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1684 = u_append(e1684, v_c);
    }
    l1685 = u_field(l1685, 1);
  }
  return t1683;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1686 = v_c;
  u_value t1687 = NULL;
  if ((!t1687) && u_is_con(s1686, &n_syntax_46_CNotation)) {
    t1687 = u_mk_bool(1);
  }
  if (!t1687) {
    t1687 = u_mk_bool(0);
  }
  if (!t1687) {
    u_no_case(s1686);
  }
  return t1687;
}

u_value f_parse_46_Context_46_notations_1(u_value v_x) {
  u_value s1688 = v_x;
  u_value t1689 = NULL;
  if ((!t1689) && u_is_con(s1688, &n_parse_46_Context)) {
    u_value v_y = u_field(s1688, 0);
    t1689 = v_y;
  }
  if (!t1689) {
    u_no_case(s1688);
  }
  return t1689;
}

u_value f_parse_46_Context_46_modules_1(u_value v_x) {
  u_value s1690 = v_x;
  u_value t1691 = NULL;
  if ((!t1691) && u_is_con(s1690, &n_parse_46_Context)) {
    u_value v_y = u_field(s1690, 1);
    t1691 = v_y;
  }
  if (!t1691) {
    u_no_case(s1690);
  }
  return t1691;
}

u_value f_parse_46_Context_46_loading_1(u_value v_x) {
  u_value s1692 = v_x;
  u_value t1693 = NULL;
  if ((!t1693) && u_is_con(s1692, &n_parse_46_Context)) {
    u_value v_y = u_field(s1692, 2);
    t1693 = v_y;
  }
  if (!t1693) {
    u_no_case(s1692);
  }
  return t1693;
}

u_value f_parse_46_Context_46_base_1(u_value v_x) {
  u_value s1694 = v_x;
  u_value t1695 = NULL;
  if ((!t1695) && u_is_con(s1694, &n_parse_46_Context)) {
    u_value v_y = u_field(s1694, 3);
    t1695 = v_y;
  }
  if (!t1695) {
    u_no_case(s1694);
  }
  return t1695;
}

u_value f_parse_46_Context_46_dir_1(u_value v_x) {
  u_value s1696 = v_x;
  u_value t1697 = NULL;
  if ((!t1697) && u_is_con(s1696, &n_parse_46_Context)) {
    u_value v_y = u_field(s1696, 4);
    t1697 = v_y;
  }
  if (!t1697) {
    u_no_case(s1696);
  }
  return t1697;
}

u_value f_parse_46_Context_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1698 = v_x;
  u_value t1699 = NULL;
  if ((!t1699) && u_is_con(s1698, &n_parse_46_Context)) {
    u_value v_a1 = u_field(s1698, 1);
    u_value v_a2 = u_field(s1698, 2);
    u_value v_a3 = u_field(s1698, 3);
    u_value v_a4 = u_field(s1698, 4);
    t1699 = u_mk_con(&n_parse_46_Context, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1699) {
    u_no_case(s1698);
  }
  return t1699;
}

u_value f_parse_46_Context_46_modules_58__61__2(u_value v_x, u_value v_y) {
  u_value s1700 = v_x;
  u_value t1701 = NULL;
  if ((!t1701) && u_is_con(s1700, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1700, 0);
    u_value v_a2 = u_field(s1700, 2);
    u_value v_a3 = u_field(s1700, 3);
    u_value v_a4 = u_field(s1700, 4);
    t1701 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t1701) {
    u_no_case(s1700);
  }
  return t1701;
}

u_value f_parse_46_Context_46_loading_58__61__2(u_value v_x, u_value v_y) {
  u_value s1702 = v_x;
  u_value t1703 = NULL;
  if ((!t1703) && u_is_con(s1702, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1702, 0);
    u_value v_a1 = u_field(s1702, 1);
    u_value v_a3 = u_field(s1702, 3);
    u_value v_a4 = u_field(s1702, 4);
    t1703 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t1703) {
    u_no_case(s1702);
  }
  return t1703;
}

u_value f_parse_46_Context_46_base_58__61__2(u_value v_x, u_value v_y) {
  u_value s1704 = v_x;
  u_value t1705 = NULL;
  if ((!t1705) && u_is_con(s1704, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1704, 0);
    u_value v_a1 = u_field(s1704, 1);
    u_value v_a2 = u_field(s1704, 2);
    u_value v_a4 = u_field(s1704, 4);
    t1705 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t1705) {
    u_no_case(s1704);
  }
  return t1705;
}

u_value f_parse_46_Context_46_dir_58__61__2(u_value v_x, u_value v_y) {
  u_value s1706 = v_x;
  u_value t1707 = NULL;
  if ((!t1707) && u_is_con(s1706, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1706, 0);
    u_value v_a1 = u_field(s1706, 1);
    u_value v_a2 = u_field(s1706, 2);
    u_value v_a3 = u_field(s1706, 3);
    t1707 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t1707) {
    u_no_case(s1706);
  }
  return t1707;
}

u_value f_parse_46_with_commands_2(u_value v_cx, u_value v_cs) {
  return f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_with_notations_2(f_parse_46_Context_46_notations_1(v_cx), v_cs));
}

u_value f_parse_46_with_module_2(u_value v_cx, u_value v_c) {
  return f_parse_46_Context_46_modules_58__61__2(v_cx, f_list_46__43__43__2(f_parse_46_modules_of_1(v_c), f_parse_46_Context_46_modules_1(v_cx)));
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1708 = v_c;
  u_value t1709 = NULL;
  if ((!t1709) && u_is_con(s1708, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1708, 0);
    u_value v_cs = u_field(s1708, 1);
    u_value t1710 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1711 = &t1710;
    u_value l1712 = v_cs;
    while (u_is_con(l1712, &n_list_46_Cons)) {
      u_value v_d = u_field(l1712, 0);
      if (u_truth(u_mk_bool(1))) {
        e1711 = u_append(e1711, f_parse_46_modules_of_1(v_d));
      }
      l1712 = u_field(l1712, 1);
    }
    t1709 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1710));
  }
  if (!t1709) {
    t1709 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1709) {
    u_no_case(s1708);
  }
  return t1709;
}

u_value f_parse_46_own_notations_2(u_value v_cx, u_value v_n) {
  u_value s1713 = f_list_46_lookup_2(v_n, f_parse_46_Context_46_modules_1(v_cx));
  u_value t1714 = NULL;
  if ((!t1714) && u_is_con(s1713, &n_option_46_Some)) {
    u_value v_ns = u_field(s1713, 0);
    t1714 = v_ns;
  }
  if ((!t1714) && u_is_con(s1713, &n_option_46_None)) {
    t1714 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1714) {
    u_no_case(s1713);
  }
  return t1714;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1715 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1715;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1716 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1716;
        u_value t1718 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1718)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1717 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1717;
          }
          t1718 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1719 = f_list_46_reverse_1(v_rev);
    *r_ps = t1719;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1720 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1720;
    u_value s1721 = f_parse_46_head_token_1(*r_ts);
    int d1722 = 0;
    if ((!d1722) && (u_is_con(s1721, &n_lex_46_TName) && u_equals(u_field(s1721, 0), u_mk_lit("prefix")))) {
      d1722 = 1;
      {
        u_value t1723 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1723;
      }
    }
    if ((!d1722) && (u_is_con(s1721, &n_lex_46_TName) && u_equals(u_field(s1721, 0), u_mk_lit("postfix")))) {
      d1722 = 1;
      {
        u_value t1724 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1724;
      }
    }
    if (!d1722) {
      d1722 = 1;
      {
        u_value s1725 = f_parse_46_second_token_1(*r_ts);
        int d1726 = 0;
        if ((!d1726) && (u_is_con(s1725, &n_lex_46_TName) && u_equals(u_field(s1725, 0), u_mk_lit("left")))) {
          d1726 = 1;
          {
            u_value t1727 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1727;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1726) && (u_is_con(s1725, &n_lex_46_TName) && u_equals(u_field(s1725, 0), u_mk_lit("right")))) {
          d1726 = 1;
          {
            u_value t1728 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1728;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1726) {
          d1726 = 1;
          {
          }
        }
        if (!d1726) {
          u_no_case(s1725);
        }
      }
    }
    if (!d1722) {
      u_no_case(s1721);
    }
    p_parse_46_advance(r_ts);
    u_value s1729 = f_parse_46_head_token_1(*r_ts);
    int d1730 = 0;
    if ((!d1730) && u_is_con(s1729, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1729, 0);
      d1730 = 1;
      {
        u_value t1731 = v_n;
        v_p = t1731;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1730) {
      d1730 = 1;
      {
        static struct u_object l_precedence_32_expec_1732 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1733 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1732, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1733)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1730) {
      u_no_case(s1729);
    }
    u_value t1734 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1735 = u_mk_bool(1);
    v_ops = t1734;
    v_go = t1735;
    u_value t1741 = v_go;
    while (u_truth(t1741)) {
      {
        u_value s1736 = f_parse_46_head_token_1(*r_ts);
        int d1737 = 0;
        if ((!d1737) && u_is_con(s1736, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1736, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1737 = 1;
            {
              u_value t1738 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1738;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1737) && u_is_con(s1736, &n_lex_46_TName)) {
          u_value v_n = u_field(s1736, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1737 = 1;
            {
              u_value t1739 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1739;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1737) {
          d1737 = 1;
          {
            u_value t1740 = u_mk_bool(0);
            v_go = t1740;
          }
        }
        if (!d1737) {
          u_no_case(s1736);
        }
      }
      t1741 = v_go;
    }
    u_value s1742 = v_ops;
    int d1743 = 0;
    if ((!d1743) && u_is_con(s1742, &n_list_46_Nil)) {
      d1743 = 1;
      {
        static struct u_object l_operator_32_expecte_1744 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1745 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1744, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1745)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1743) {
      d1743 = 1;
      {
        u_value t1746 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1746;
      }
    }
    if (!d1743) {
      u_no_case(s1742);
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
    u_value t1747 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1748 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1747;
    *r_fs = t1748;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1749 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
            u_value t1750 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
            v_args = t1749;
            *r_fs = t1750;
            u_value t1753 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1753)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t1751 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t1752 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t1751;
                *r_fs = t1752;
              }
              t1753 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1754 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t1755 = f_list_46_reverse_1(*r_fs);
    *r_k = t1754;
    *r_fs = t1755;
    u_value s1756 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d1757 = 0;
    if ((!d1757) && u_is_con(s1756, &n_option_46_Some)) {
      u_value v_f = u_field(s1756, 0);
      d1757 = 1;
      {
        static struct u_object l_repeated_32_field_32__1758 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__1759 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__1758, u_concat(v_f, u_concat(&l__32_of_32__1759, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1757) && u_is_con(s1756, &n_option_46_None)) {
      d1757 = 1;
      {
      }
    }
    if (!d1757) {
      u_no_case(s1756);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1760 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1761 = 0;
    if ((!d1761) && (u_is_con(u_field(s1760, 0), &n_lex_46_TName) && (u_is_con(u_field(s1760, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1760, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1760, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1761 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1762 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1762;
        }
      }
    }
    if (!d1761) {
      d1761 = 1;
      {
        u_value t1763 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1763;
      }
    }
    if (!d1761) {
      u_no_case(s1760);
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
    static struct u_object l__58__61__1764 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1764), r_ts);
    static struct u_object l__124__1765 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1765)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1766 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1766;
    static struct u_object l__124__1767 = U_LIT("|");
    u_value t1769 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1767));
    while (u_truth(t1769)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1768 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1768;
      }
      static struct u_object l__124__1770 = U_LIT("|");
      t1769 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1770));
    }
    u_value s1771 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d1772 = 0;
    if ((!d1772) && u_is_con(s1771, &n_option_46_Some)) {
      u_value v_f = u_field(s1771, 0);
      d1772 = 1;
      {
        static struct u_object l_field_32__1773 = U_LIT("field ");
        static struct u_object l__32_of_32__1774 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_1775 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__1773, u_concat(v_f, u_concat(&l__32_of_32__1774, u_concat(v_n, &l__32_has_32_different_32_t_1775)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1772) && u_is_con(s1771, &n_option_46_None)) {
      d1772 = 1;
      {
      }
    }
    if (!d1772) {
      u_no_case(s1771);
    }
    u_value t1776 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_list_46__43__43__2(f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)), f_parse_46_updaters_3(v_n, v_ps, f_list_46_reverse_1(v_rev))));
    *r_cs = t1776;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s1777 = v_fs;
  u_value t1778 = NULL;
  if ((!t1778) && u_is_con(s1777, &n_list_46_Nil)) {
    t1778 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1778) && (u_is_con(s1777, &n_list_46_Cons) && u_is_con(u_field(s1777, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s1777, 0), 0);
    u_value v_rest = u_field(s1777, 1);
    t1778 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t1778) && (u_is_con(s1777, &n_list_46_Cons) && u_is_con(u_field(s1777, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1777, 1);
    t1778 = f_parse_46_field_names_1(v_rest);
  }
  if (!t1778) {
    u_no_case(s1777);
  }
  return t1778;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s1779 = v_fs;
  u_value t1780 = NULL;
  if ((!t1780) && u_is_con(s1779, &n_list_46_Nil)) {
    t1780 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1780) && u_is_con(s1779, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1779, 0), 0);
    u_value v_t = u_field(u_field(u_field(s1779, 0), 1), 3);
    u_value v_rest = u_field(s1779, 1);
    u_value t1781 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1782 = &t1781;
    u_value l1783 = f_parse_46_fields_named_2(v_f, v_rest);
    while (u_is_con(l1783, &n_list_46_Cons)) {
      u_value v_e = u_field(l1783, 0);
      if (u_truth(u_mk_bool(1))) {
        e1782 = u_append(e1782, f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t));
      }
      l1783 = u_field(l1783, 1);
    }
    u_value t1784;
    if (u_truth(f_list_46_contains_2(u_mk_bool(1), t1781))) {
      t1784 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t1784 = f_parse_46_differing_1(v_rest);
    }
    t1780 = t1784;
  }
  if (!t1780) {
    u_no_case(s1779);
  }
  return t1780;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s1785 = v_e;
  u_value t1786 = NULL;
  if (!t1786) {
    u_value v_t = u_field(u_field(s1785, 1), 3);
    t1786 = v_t;
  }
  if (!t1786) {
    u_no_case(s1785);
  }
  return t1786;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s1787 = v_fs;
  u_value t1788 = NULL;
  if ((!t1788) && u_is_con(s1787, &n_list_46_Nil)) {
    t1788 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1788) && u_is_con(s1787, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s1787, 0), 0);
    u_value v_d = u_field(u_field(s1787, 0), 1);
    u_value v_rest = u_field(s1787, 1);
    u_value t1789;
    if (u_truth(u_eq(v_g, v_f))) {
      t1789 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t1789 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t1788 = t1789;
  }
  if (!t1788) {
    u_no_case(s1787);
  }
  return t1788;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1790 = v_ks;
  u_value t1791 = NULL;
  if ((!t1791) && u_is_con(s1790, &n_list_46_Nil)) {
    t1791 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1791) && (u_is_con(s1790, &n_list_46_Cons) && u_is_con(u_field(u_field(s1790, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1790, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1790, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1790, 0), 1);
    u_value v_rest = u_field(s1790, 1);
    t1791 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1791) {
    u_no_case(s1790);
  }
  return t1791;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1792 = v_fs;
  u_value t1793 = NULL;
  if ((!t1793) && u_is_con(s1792, &n_list_46_Nil)) {
    t1793 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1793) && (u_is_con(s1792, &n_list_46_Cons) && u_is_con(u_field(u_field(s1792, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1792, 0), 0), 0);
    u_value v_t = u_field(u_field(s1792, 0), 1);
    u_value v_rest = u_field(s1792, 1);
    t1793 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1793) && (u_is_con(s1792, &n_list_46_Cons) && u_is_con(u_field(u_field(s1792, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1792, 1);
    t1793 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1793) {
    u_no_case(s1792);
  }
  return t1793;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1798 = f_parse_46_fields_of_1(v_ks);
  u_value t1797;
  {
    u_value v_fs = t1798;
    u_value t1794 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1795 = &t1794;
    u_value l1796 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1796, &n_list_46_Cons)) {
      u_value v_f = u_field(l1796, 0);
      if (u_truth(u_mk_bool(1))) {
        e1795 = u_append(e1795, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1796 = u_field(l1796, 1);
    }
    t1797 = t1794;
  }
  return t1797;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1799 = U_LIT("x");
  u_value t1800 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1801 = &t1800;
  u_value l1802 = v_ps;
  while (u_is_con(l1802, &n_list_46_Cons)) {
    u_value v_p = u_field(l1802, 0);
    if (u_truth(u_mk_bool(1))) {
      e1801 = u_append(e1801, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1802 = u_field(l1802, 1);
  }
  u_value t1810 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1799), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1811 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1800);
  u_value t1809;
  {
    u_value v_x = t1810;
    u_value v_owner = t1811;
    static struct u_object l__46__1803 = U_LIT(".");
    static struct u_object l_x_1804 = U_LIT("x");
    u_value t1805 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1806 = &t1805;
    u_value l1807 = v_es;
    while (u_is_con(l1807, &n_list_46_Cons)) {
      u_value v_e = u_field(l1807, 0);
      if (u_truth(u_mk_bool(1))) {
        e1806 = u_append(e1806, f_parse_46_field_case_1(v_e));
      }
      l1807 = u_field(l1807, 1);
    }
    u_value t1808;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1808 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1808 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1809 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1803, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1804), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1805, t1808)));
  }
  return t1809;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1812 = v_es;
  u_value t1813 = NULL;
  if ((!t1813) && u_is_con(s1812, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1812, 0), 1), 3);
    t1813 = v_t;
  }
  if (!t1813) {
    t1813 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1813) {
    u_no_case(s1812);
  }
  return t1813;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s1814 = v_e;
  u_value t1815 = NULL;
  if (!t1815) {
    u_value v_k = u_field(u_field(s1814, 1), 0);
    u_value v_i = u_field(u_field(s1814, 1), 1);
    u_value v_n = u_field(u_field(s1814, 1), 2);
    u_value t1816 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1817 = &t1816;
    u_value l1818 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1818, &n_list_46_Cons)) {
      u_value v_j = u_field(l1818, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1820;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1819 = U_LIT("y");
          t1820 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1819), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1820 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1817 = u_append(e1817, t1820);
      }
      l1818 = u_field(l1818, 1);
    }
    static struct u_object l_y_1821 = U_LIT("y");
    t1815 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1816), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1821), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1815) {
    u_no_case(s1814);
  }
  return t1815;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_MissingField_1822 = U_LIT("MissingField");
  static struct u_object l__46__1823 = U_LIT(".");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_ERaise, 2, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_MissingField_1822), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_t, u_concat(&l__46__1823, v_f)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_parse_46_updaters_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1828 = f_parse_46_fields_of_1(v_ks);
  u_value t1827;
  {
    u_value v_fs = t1828;
    u_value t1824 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1825 = &t1824;
    u_value l1826 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1826, &n_list_46_Cons)) {
      u_value v_f = u_field(l1826, 0);
      if (u_truth(u_mk_bool(1))) {
        e1825 = u_append(e1825, f_parse_46_updater_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1826 = u_field(l1826, 1);
    }
    t1827 = t1824;
  }
  return t1827;
}

u_value f_parse_46_updater_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1829 = U_LIT("x");
  u_value t1830 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1831 = &t1830;
  u_value l1832 = v_ps;
  while (u_is_con(l1832, &n_list_46_Cons)) {
    u_value v_p = u_field(l1832, 0);
    if (u_truth(u_mk_bool(1))) {
      e1831 = u_append(e1831, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1832 = u_field(l1832, 1);
  }
  u_value t1842 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1829), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1843 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1830);
  u_value t1841;
  {
    u_value v_x = t1842;
    u_value v_owner = t1843;
    static struct u_object l__46__1833 = U_LIT(".");
    static struct u_object l__58__61__1834 = U_LIT(":=");
    static struct u_object l_x_1835 = U_LIT("x");
    static struct u_object l_y_1836 = U_LIT("y");
    u_value t1837 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1838 = &t1837;
    u_value l1839 = v_es;
    while (u_is_con(l1839, &n_list_46_Cons)) {
      u_value v_e = u_field(l1839, 0);
      if (u_truth(u_mk_bool(1))) {
        e1838 = u_append(e1838, f_parse_46_update_case_1(v_e));
      }
      l1839 = u_field(l1839, 1);
    }
    u_value t1840;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1840 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1840 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1841 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1833, u_concat(v_f, &l__58__61__1834)))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1835), v_owner), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1836), f_parse_46_field_type_1(v_es)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), v_owner, u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1837, t1840)));
  }
  return t1841;
}

u_value f_parse_46_update_case_1(u_value v_e) {
  u_value s1844 = v_e;
  u_value t1845 = NULL;
  if (!t1845) {
    u_value v_k = u_field(u_field(s1844, 1), 0);
    u_value v_i = u_field(u_field(s1844, 1), 1);
    u_value v_n = u_field(u_field(s1844, 1), 2);
    u_value t1846 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1847 = &t1846;
    u_value l1848 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1848, &n_list_46_Cons)) {
      u_value v_j = u_field(l1848, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1850;
        if (u_truth(u_eq(v_j, v_i))) {
          t1850 = u_mk_con(&n_syntax_46_PWild, 0);
        } else {
          static struct u_object l_a_1849 = U_LIT("a");
          t1850 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l_a_1849, u_str_show(v_j))), u_mk_con(&n_syntax_46_THole, 0)));
        }
        e1847 = u_append(e1847, t1850);
      }
      l1848 = u_field(l1848, 1);
    }
    u_value t1851 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1852 = &t1851;
    u_value l1853 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1853, &n_list_46_Cons)) {
      u_value v_j = u_field(l1853, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1856;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1854 = U_LIT("y");
          t1856 = &l_y_1854;
        } else {
          static struct u_object l_a_1855 = U_LIT("a");
          t1856 = u_concat(&l_a_1855, u_str_show(v_j));
        }
        e1852 = u_append(e1852, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(t1856), u_mk_con(&n_syntax_46_THole, 0))));
      }
      l1853 = u_field(l1853, 1);
    }
    t1845 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1846), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_k, u_mk_con(&n_syntax_46_THole, 0))), t1851));
  }
  if (!t1845) {
    u_no_case(s1844);
  }
  return t1845;
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t1857;
  if (u_truth(u_eq(v_i, v_n))) {
    t1857 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1857 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t1857;
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
    u_value t1858 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1858;
    u_value t1860 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1860)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1859 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1859;
      }
      t1860 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1861 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1861), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1862 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1862), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1863 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1863;
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
    p_parse_46_formal_sections(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_ins, &v_outs, &v_refs, r_ts);
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    static struct u_object l__58__61__1864 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1864), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1865 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1865;
  }
}

void p_parse_46_formal_sections(u_value v_sc, u_value *r_ins, u_value *r_outs, u_value *r_refs, u_value *r_ts) {
  {
    u_value v_first = NULL;
    u_value t1866 = u_mk_bool(1);
    v_first = t1866;
    u_value t1867 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1868 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1869 = u_mk_con(&n_list_46_Nil, 0);
    *r_ins = t1867;
    *r_outs = t1868;
    *r_refs = t1869;
    static struct u_object l_in_1870 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1870))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formals1(v_sc, r_ins, r_ts);
        u_value t1871 = u_mk_bool(0);
        v_first = t1871;
      }
    }
    static struct u_object l_out_1872 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1872, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(v_sc, r_outs, r_ts);
        u_value t1873 = u_mk_bool(0);
        v_first = t1873;
      }
    }
    static struct u_object l_ref_1874 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1874, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(v_sc, r_refs, r_ts);
      }
    }
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
    u_value t1875 = f_parse_46_Context_46_notations_1(v_cx);
    v_ns = t1875;
    u_value s1876 = f_parse_46_head_token_1(*r_ts);
    int d1877 = 0;
    if ((!d1877) && (u_is_con(s1876, &n_lex_46_TName) && u_equals(u_field(s1876, 0), u_mk_lit("import")))) {
      d1877 = 1;
      {
        u_value t1878 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1878;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Context_46_modules_1(v_cx))))) {
          {
            u_value t1879 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1879;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_cx, v_at, r_cs);
            u_value t1880 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1880;
          }
        }
      }
    }
    if ((!d1877) && u_is_con(s1876, &n_lex_46_TName)) {
      u_value v_w = u_field(s1876, 0);
      static struct u_object l_prefix_1882 = U_LIT("prefix");
      static struct u_object l_postfix_1883 = U_LIT("postfix");
      static struct u_object l_infix_1884 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1882, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1883, u_mk_con(&n_list_46_Cons, 2, &l_infix_1884, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1877 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1881 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1881;
        }
      }
    }
    if ((!d1877) && (u_is_con(s1876, &n_lex_46_TName) && u_equals(u_field(s1876, 0), u_mk_lit("type")))) {
      d1877 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1885 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1885), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1886 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1886;
      }
    }
    if ((!d1877) && (u_is_con(s1876, &n_lex_46_TName) && u_equals(u_field(s1876, 0), u_mk_lit("data")))) {
      d1877 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1877) && (u_is_con(s1876, &n_lex_46_TName) && u_equals(u_field(s1876, 0), u_mk_lit("function")))) {
      d1877 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1887 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1887;
      }
    }
    if ((!d1877) && (u_is_con(s1876, &n_lex_46_TName) && u_equals(u_field(s1876, 0), u_mk_lit("procedure")))) {
      d1877 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1888 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1888;
      }
    }
    if ((!d1877) && (u_is_con(s1876, &n_lex_46_TName) && u_equals(u_field(s1876, 0), u_mk_lit("eval")))) {
      d1877 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_e, r_ts);
        u_value t1889 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CEval, 1, v_e), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1889;
      }
    }
    if ((!d1877) && (u_is_con(s1876, &n_lex_46_TName) && u_equals(u_field(s1876, 0), u_mk_lit("run")))) {
      d1877 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1890 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1890), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1891 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1891;
      }
    }
    if ((!d1877) && (u_is_con(s1876, &n_lex_46_TName) && u_equals(u_field(s1876, 0), u_mk_lit("test")))) {
      d1877 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1892 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1892), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1893 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1893;
      }
    }
    if (!d1877) {
      d1877 = 1;
      {
        static struct u_object l_declaration_32_expe_1894 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1895 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1894, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1895)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1877) {
      u_no_case(s1876);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_cx) {
  {
    u_value s1896 = v_cs;
    int d1897 = 0;
    if ((!d1897) && u_is_con(s1896, &n_list_46_Nil)) {
      d1897 = 1;
      {
      }
    }
    if ((!d1897) && (u_is_con(s1896, &n_list_46_Cons) && u_is_con(u_field(s1896, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1896, 0), 0);
      u_value v_p = u_field(u_field(s1896, 0), 1);
      u_value v_ops = u_field(u_field(s1896, 0), 2);
      u_value v_more = u_field(s1896, 1);
      d1897 = 1;
      {
        u_value t1898 = f_parse_46_with_commands_2(*r_cx, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_cx = t1898;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1897) && (u_is_con(s1896, &n_list_46_Cons) && u_is_con(u_field(s1896, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1896, 0), 0);
      u_value v_ds = u_field(u_field(s1896, 0), 1);
      u_value v_more = u_field(s1896, 1);
      d1897 = 1;
      {
        u_value t1899 = f_parse_46_with_module_2(*r_cx, u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds));
        *r_cx = t1899;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1897) && (u_is_con(s1896, &n_list_46_Cons) && u_is_con(u_field(s1896, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1896, 0), 0);
      u_value v_more = u_field(s1896, 1);
      d1897 = 1;
      {
        u_value t1900 = f_parse_46_with_commands_2(*r_cx, f_parse_46_own_notations_2(*r_cx, v_n));
        *r_cx = t1900;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1897) && u_is_con(s1896, &n_list_46_Cons)) {
      u_value v_more = u_field(s1896, 1);
      d1897 = 1;
      {
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if (!d1897) {
      u_no_case(s1896);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_cx, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1901 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1901;
    u_value t1903 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1903)) {
      {
        p_parse_46_parse_command(*r_cx, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_cx);
        u_value t1902 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1902;
      }
      t1903 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1904 = f_list_46_reverse_1(v_rev);
    *r_cs = t1904;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1905 = v_dir;
    int d1906 = 0;
    if ((!d1906) && u_is_con(s1905, &n_option_46_Some)) {
      u_value v_d = u_field(s1905, 0);
      d1906 = 1;
      {
        {
          struct u_handler *h1909 = u_push();
          if (setjmp(h1909->env) == 0) {
            {
              static struct u_object l__47__1907 = U_LIT("/");
              static struct u_object l__46_u_1908 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1907, u_concat(v_n, &l__46_u_1908))), r_text);
            }
            u_pop(h1909);
          } else {
            u_pop(h1909);
            int d1910 = 0;
            if ((!d1910) && (u_has_type(h1909, "os.OsError") && u_is_con(u_caught(h1909), &n_os_46_OsError))) {
              d1910 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1910) {
              u_reraise(h1909);
            }
          }
        }
      }
    }
    if ((!d1906) && u_is_con(s1905, &n_option_46_None)) {
      d1906 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1906) {
      u_no_case(s1905);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1911 = u_push();
      if (setjmp(h1911->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1911);
      } else {
        u_pop(h1911);
        int d1912 = 0;
        if ((!d1912) && (u_has_type(h1911, "os.OsError") && u_is_con(u_caught(h1911), &n_os_46_OsError))) {
          d1912 = 1;
          {
            static struct u_object l_unknown_32_module_32__1913 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1913, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1912) {
          u_reraise(h1911);
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
        static struct u_object l_cyclic_32_import_32_of_1914 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1914, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Context_46_dir_1(v_cx), v_at, &v_text);
    u_value t1915 = f_parse_46_Context_46_loading_58__61__2(f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_base_notations_1(v_cx)), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Context_46_loading_1(v_cx)));
    v_mcx = t1915;
    {
      struct u_handler *h1916 = u_push();
      if (setjmp(h1916->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mcx, &v_mts);
        }
        u_pop(h1916);
      } else {
        u_pop(h1916);
        int d1917 = 0;
        if ((!d1917) && (u_has_type(h1916, "lex.LexError") && u_is_con(u_caught(h1916), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1916), 0);
          u_value v_i = u_field(u_caught(h1916), 1);
          d1917 = 1;
          {
            static struct u_object l__32_in_32__1918 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1918, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1917) && (u_has_type(h1916, "parse.ParseError") && u_is_con(u_caught(h1916), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1916), 0);
          u_value v_i = u_field(u_caught(h1916), 1);
          d1917 = 1;
          {
            static struct u_object l__32_in_32__1919 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1919, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1917) {
          u_reraise(h1916);
        }
      }
    }
  }
}

u_value f_parse_46_base_notations_1(u_value v_cx) {
  u_value s1920 = f_parse_46_Context_46_base_1(v_cx);
  u_value t1921 = NULL;
  if ((!t1921) && u_is_con(s1920, &n_option_46_Some)) {
    u_value v_b = u_field(s1920, 0);
    t1921 = v_b;
  }
  if ((!t1921) && u_is_con(s1920, &n_option_46_None)) {
    t1921 = f_parse_46_Context_46_notations_1(v_cx);
  }
  if (!t1921) {
    u_no_case(s1920);
  }
  return t1921;
}

void p_parse_46_parse_text(u_value v_text, u_value v_cx, u_value *r_p) {
  {
    u_value v_ts = NULL;
    u_value v_cs = NULL;
    u_value v_after = NULL;
    u_value t1922 = v_cx;
    v_after = t1922;
    {
      struct u_handler *h1923 = u_push();
      if (setjmp(h1923->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1923);
      } else {
        u_pop(h1923);
        int d1924 = 0;
        if ((!d1924) && (u_has_type(h1923, "lex.LexError") && u_is_con(u_caught(h1923), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1923), 0);
          u_value v_i = u_field(u_caught(h1923), 1);
          d1924 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1924) {
          u_reraise(h1923);
        }
      }
    }
    p_parse_46_parse_commands(&v_cs, &v_after, &v_ts);
    u_value t1925 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1925;
  }
}

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p) {
  {
    u_value v_cx = NULL;
    u_value t1926 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    v_cx = t1926;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), &v_cx);
    p_parse_46_parse_text(v_text, f_parse_46_Context_46_dir_58__61__2(f_parse_46_Context_46_base_58__61__2(f_parse_46_Context_46_loading_58__61__2(v_cx, u_mk_con(&n_list_46_Cons, 2, v_name, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_option_46_Some, 1, f_parse_46_Context_46_notations_1(v_cx))), v_dir), r_p);
  }
}

void p_parse_46_parse_library(u_value v_n, u_value *r_p) {
  {
    u_value v_text = NULL;
    u_value v_pre = NULL;
    p_parse_46_library_text(v_n, u_mk_int(0), &v_text);
    static struct u_object l_prelude_1927 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1927))) {
      {
        static struct u_object l_prelude_1928 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1928, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1929 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1929, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1930 = v_xs;
  u_value t1931 = NULL;
  if ((!t1931) && (u_is_con(s1930, &n_list_46_Cons) && u_is_con(u_field(s1930, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1930, 0);
    t1931 = v_x;
  }
  if ((!t1931) && u_is_con(s1930, &n_list_46_Cons)) {
    u_value v_more = u_field(s1930, 1);
    t1931 = f_parse_46_last_part_1(v_more);
  }
  if (!t1931) {
    static struct u_object l__1932 = U_LIT("");
    t1931 = &l__1932;
  }
  if (!t1931) {
    u_no_case(s1930);
  }
  return t1931;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1937 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1936;
  {
    u_value v_f = t1937;
    u_value t1934 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1934)) {
      static struct u_object l__46_u_1933 = U_LIT(".u");
      t1934 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1933);
    }
    u_value t1935;
    if (u_truth(t1934)) {
      t1935 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1935 = v_f;
    }
    t1936 = t1935;
  }
  return t1936;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1942 = u_str_split(v_path, u_mk_char(47));
  u_value t1941;
  {
    u_value v_parts = t1942;
    u_value t1940;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1938 = U_LIT(".");
      t1940 = &l__46__1938;
    } else {
      static struct u_object l__47__1939 = U_LIT("/");
      t1940 = u_str_join(&l__47__1939, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1941 = t1940;
  }
  return t1941;
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
    static struct u_object l_prelude_1943 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1943, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1944 = u_mk_tuple(2, v_cs, v_ds);
    int d1945 = 0;
    if ((!d1945) && (u_is_con(u_field(s1944, 0), &n_list_46_Cons) && u_is_con(u_field(s1944, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1944, 0), 0);
      u_value v_more = u_field(u_field(s1944, 0), 1);
      u_value v_d = u_field(u_field(s1944, 1), 0);
      u_value v_rest = u_field(u_field(s1944, 1), 1);
      d1945 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1945) {
      d1945 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1945) {
      u_no_case(s1944);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1946 = v_paths;
    int d1947 = 0;
    if ((!d1947) && u_is_con(s1946, &n_list_46_Nil)) {
      d1947 = 1;
      {
      }
    }
    if ((!d1947) && u_is_con(s1946, &n_list_46_Cons)) {
      u_value v_path = u_field(s1946, 0);
      u_value v_more = u_field(s1946, 1);
      d1947 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1947) {
      u_no_case(s1946);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1948 = v_paths;
    int d1949 = 0;
    if ((!d1949) && u_is_con(s1948, &n_list_46_Nil)) {
      d1949 = 1;
      {
      }
    }
    if ((!d1949) && u_is_con(s1948, &n_list_46_Cons)) {
      u_value v_path = u_field(s1948, 0);
      u_value v_more = u_field(s1948, 1);
      d1949 = 1;
      {
        u_value t1950 = u_mk_bool(0);
        v_failed = t1950;
        {
          struct u_handler *h1951 = u_push();
          if (setjmp(h1951->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1951);
          } else {
            u_pop(h1951);
            int d1952 = 0;
            if ((!d1952) && (u_has_type(h1951, "parse.ParseError") && u_is_con(u_caught(h1951), &n_parse_46_ParseError))) {
              d1952 = 1;
              {
                u_value t1953 = u_mk_bool(1);
                v_failed = t1953;
              }
            }
            if (!d1952) {
              u_reraise(h1951);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1949) {
      u_no_case(s1948);
    }
  }
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s1954 = v_x;
  u_value t1955 = NULL;
  if ((!t1955) && u_is_con(s1954, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1954, 0);
    t1955 = v_y;
  }
  if (!t1955) {
    u_no_case(s1954);
  }
  return t1955;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s1956 = v_x;
  u_value t1957 = NULL;
  if ((!t1957) && u_is_con(s1956, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1956, 1);
    t1957 = v_y;
  }
  if (!t1957) {
    u_no_case(s1956);
  }
  return t1957;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s1958 = v_x;
  u_value t1959 = NULL;
  if ((!t1959) && u_is_con(s1958, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1958, 2);
    t1959 = v_y;
  }
  if (!t1959) {
    u_no_case(s1958);
  }
  return t1959;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s1960 = v_x;
  u_value t1961 = NULL;
  if ((!t1961) && u_is_con(s1960, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1960, 3);
    t1961 = v_y;
  }
  if (!t1961) {
    u_no_case(s1960);
  }
  return t1961;
}

u_value f_compile_46_Globals_46_procs_1(u_value v_x) {
  u_value s1962 = v_x;
  u_value t1963 = NULL;
  if ((!t1963) && u_is_con(s1962, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1962, 4);
    t1963 = v_y;
  }
  if (!t1963) {
    u_no_case(s1962);
  }
  return t1963;
}

u_value f_compile_46_Globals_46_funs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1964 = v_x;
  u_value t1965 = NULL;
  if ((!t1965) && u_is_con(s1964, &n_compile_46_Globals)) {
    u_value v_a1 = u_field(s1964, 1);
    u_value v_a2 = u_field(s1964, 2);
    u_value v_a3 = u_field(s1964, 3);
    u_value v_a4 = u_field(s1964, 4);
    t1965 = u_mk_con(&n_compile_46_Globals, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1965) {
    u_no_case(s1964);
  }
  return t1965;
}

u_value f_compile_46_Globals_46_constrs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1966 = v_x;
  u_value t1967 = NULL;
  if ((!t1967) && u_is_con(s1966, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1966, 0);
    u_value v_a2 = u_field(s1966, 2);
    u_value v_a3 = u_field(s1966, 3);
    u_value v_a4 = u_field(s1966, 4);
    t1967 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t1967) {
    u_no_case(s1966);
  }
  return t1967;
}

u_value f_compile_46_Globals_46_arities_58__61__2(u_value v_x, u_value v_y) {
  u_value s1968 = v_x;
  u_value t1969 = NULL;
  if ((!t1969) && u_is_con(s1968, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1968, 0);
    u_value v_a1 = u_field(s1968, 1);
    u_value v_a3 = u_field(s1968, 3);
    u_value v_a4 = u_field(s1968, 4);
    t1969 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t1969) {
    u_no_case(s1968);
  }
  return t1969;
}

u_value f_compile_46_Globals_46_fields_58__61__2(u_value v_x, u_value v_y) {
  u_value s1970 = v_x;
  u_value t1971 = NULL;
  if ((!t1971) && u_is_con(s1970, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1970, 0);
    u_value v_a1 = u_field(s1970, 1);
    u_value v_a2 = u_field(s1970, 2);
    u_value v_a4 = u_field(s1970, 4);
    t1971 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t1971) {
    u_no_case(s1970);
  }
  return t1971;
}

u_value f_compile_46_Globals_46_procs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1972 = v_x;
  u_value t1973 = NULL;
  if ((!t1973) && u_is_con(s1972, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1972, 0);
    u_value v_a1 = u_field(s1972, 1);
    u_value v_a2 = u_field(s1972, 2);
    u_value v_a3 = u_field(s1972, 3);
    t1973 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t1973) {
    u_no_case(s1972);
  }
  return t1973;
}

u_value f_compile_46_Gen_46_count_1(u_value v_x) {
  u_value s1974 = v_x;
  u_value t1975 = NULL;
  if ((!t1975) && u_is_con(s1974, &n_compile_46_Gen)) {
    u_value v_y = u_field(s1974, 0);
    t1975 = v_y;
  }
  if (!t1975) {
    u_no_case(s1974);
  }
  return t1975;
}

u_value f_compile_46_Gen_46_lifted_1(u_value v_x) {
  u_value s1976 = v_x;
  u_value t1977 = NULL;
  if ((!t1977) && u_is_con(s1976, &n_compile_46_Gen)) {
    u_value v_y = u_field(s1976, 1);
    t1977 = v_y;
  }
  if (!t1977) {
    u_no_case(s1976);
  }
  return t1977;
}

u_value f_compile_46_Gen_46_count_58__61__2(u_value v_x, u_value v_y) {
  u_value s1978 = v_x;
  u_value t1979 = NULL;
  if ((!t1979) && u_is_con(s1978, &n_compile_46_Gen)) {
    u_value v_a1 = u_field(s1978, 1);
    t1979 = u_mk_con(&n_compile_46_Gen, 2, v_y, v_a1);
  }
  if (!t1979) {
    u_no_case(s1978);
  }
  return t1979;
}

u_value f_compile_46_Gen_46_lifted_58__61__2(u_value v_x, u_value v_y) {
  u_value s1980 = v_x;
  u_value t1981 = NULL;
  if ((!t1981) && u_is_con(s1980, &n_compile_46_Gen)) {
    u_value v_a0 = u_field(s1980, 0);
    t1981 = u_mk_con(&n_compile_46_Gen, 2, v_a0, v_y);
  }
  if (!t1981) {
    u_no_case(s1980);
  }
  return t1981;
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
  static struct u_object l_u_value_1982 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1982);
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
  static struct u_object l_u_value_1983 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1983));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t1984 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1985 = &t1984;
  u_value l1986 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l1986, &n_list_46_Cons)) {
    u_value v_c = u_field(l1986, 0);
    if (u_truth(u_mk_bool(1))) {
      e1985 = u_append(e1985, f_compile_46_mangle_char_1(v_c));
    }
    l1986 = u_field(l1986, 1);
  }
  return u_string(f_list_46_concat_1(t1984));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t1987 = u_is_letter(v_c);
  if (!u_truth(t1987)) {
    t1987 = u_is_digit(v_c);
  }
  u_value t1988 = t1987;
  if (!u_truth(t1988)) {
    t1988 = u_eq(v_c, u_mk_char(95));
  }
  u_value t1991;
  if (u_truth(t1988)) {
    t1991 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___1989 = U_LIT("_");
    static struct u_object l___1990 = U_LIT("_");
    t1991 = u_chars(u_concat(&l___1989, u_concat(u_str_show(u_ord(v_c)), &l___1990)));
  }
  return t1991;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__1992 = U_LIT("f_");
  static struct u_object l___1993 = U_LIT("_");
  return u_concat(&l_f__1992, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___1993, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__1994 = U_LIT("p_");
  return u_concat(&l_p__1994, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__1995 = U_LIT("v_");
  return u_concat(&l_v__1995, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__1996 = U_LIT("r_");
  return u_concat(&l_r__1996, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__1997 = U_LIT("+");
  static struct u_object l_u_add_1998 = U_LIT("u_add");
  static struct u_object l__45__1999 = U_LIT("-");
  static struct u_object l_u_sub_2000 = U_LIT("u_sub");
  static struct u_object l__42__2001 = U_LIT("*");
  static struct u_object l_u_mul_2002 = U_LIT("u_mul");
  static struct u_object l__47__2003 = U_LIT("/");
  static struct u_object l_u_div_2004 = U_LIT("u_div");
  static struct u_object l__37__2005 = U_LIT("%");
  static struct u_object l_u_mod_2006 = U_LIT("u_mod");
  static struct u_object l__60__2007 = U_LIT("<");
  static struct u_object l_u_lt_2008 = U_LIT("u_lt");
  static struct u_object l__61__61__2009 = U_LIT("==");
  static struct u_object l_u_eq_2010 = U_LIT("u_eq");
  static struct u_object l_compare_2011 = U_LIT("compare");
  static struct u_object l_u_compare_2012 = U_LIT("u_compare");
  static struct u_object l_chars_2013 = U_LIT("chars");
  static struct u_object l_u_chars_2014 = U_LIT("u_chars");
  static struct u_object l_string_2015 = U_LIT("string");
  static struct u_object l_u_string_2016 = U_LIT("u_string");
  static struct u_object l_ord_2017 = U_LIT("ord");
  static struct u_object l_u_ord_2018 = U_LIT("u_ord");
  static struct u_object l_chr_2019 = U_LIT("chr");
  static struct u_object l_u_chr_2020 = U_LIT("u_chr");
  static struct u_object l__94__2021 = U_LIT("^");
  static struct u_object l_u_concat_2022 = U_LIT("u_concat");
  static struct u_object l_str_size_2023 = U_LIT("str_size");
  static struct u_object l_u_str_size_2024 = U_LIT("u_str_size");
  static struct u_object l_str_join_2025 = U_LIT("str_join");
  static struct u_object l_u_str_join_2026 = U_LIT("u_str_join");
  static struct u_object l_str_slice_2027 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_2028 = U_LIT("u_str_slice");
  static struct u_object l_str_split_2029 = U_LIT("str_split");
  static struct u_object l_u_str_split_2030 = U_LIT("u_str_split");
  static struct u_object l_str_get_2031 = U_LIT("str_get");
  static struct u_object l_u_str_get_2032 = U_LIT("u_str_get");
  static struct u_object l_str_contains_2033 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_2034 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_2035 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_2036 = U_LIT("u_str_compare");
  static struct u_object l_str_show_2037 = U_LIT("str_show");
  static struct u_object l_u_str_show_2038 = U_LIT("u_str_show");
  static struct u_object l_str_quote_2039 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_2040 = U_LIT("u_str_quote");
  static struct u_object l_str_number_2041 = U_LIT("str_number");
  static struct u_object l_u_str_number_2042 = U_LIT("u_str_number");
  static struct u_object l_is_digit_2043 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_2044 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_2045 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_2046 = U_LIT("u_is_letter");
  static struct u_object l_is_space_2047 = U_LIT("is_space");
  static struct u_object l_u_is_space_2048 = U_LIT("u_is_space");
  u_pop(handler);
  cache = f_map_46_put_all_2(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__1997), u_mk_tuple(2, &l_u_add_1998, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__1999), u_mk_tuple(2, &l_u_sub_2000, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__2001), u_mk_tuple(2, &l_u_mul_2002, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__2003), u_mk_tuple(2, &l_u_div_2004, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__2005), u_mk_tuple(2, &l_u_mod_2006, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__2007), u_mk_tuple(2, &l_u_lt_2008, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__2009), u_mk_tuple(2, &l_u_eq_2010, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_2011), u_mk_tuple(2, &l_u_compare_2012, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_2013), u_mk_tuple(2, &l_u_chars_2014, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_2015), u_mk_tuple(2, &l_u_string_2016, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_2017), u_mk_tuple(2, &l_u_ord_2018, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_2019), u_mk_tuple(2, &l_u_chr_2020, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__2021), u_mk_tuple(2, &l_u_concat_2022, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_2023), u_mk_tuple(2, &l_u_str_size_2024, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_2025), u_mk_tuple(2, &l_u_str_join_2026, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_2027), u_mk_tuple(2, &l_u_str_slice_2028, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_2029), u_mk_tuple(2, &l_u_str_split_2030, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_2031), u_mk_tuple(2, &l_u_str_get_2032, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_2033), u_mk_tuple(2, &l_u_str_contains_2034, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_2035), u_mk_tuple(2, &l_u_str_compare_2036, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_2037), u_mk_tuple(2, &l_u_str_show_2038, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_2039), u_mk_tuple(2, &l_u_str_quote_2040, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_2041), u_mk_tuple(2, &l_u_str_number_2042, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_2043), u_mk_tuple(2, &l_u_is_digit_2044, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_2045), u_mk_tuple(2, &l_u_is_letter_2046, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_2047), u_mk_tuple(2, &l_u_is_space_2048, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))), f_map_46_empty_0());
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__2049 = U_LIT("&");
  static struct u_object l_n__2050 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__2049, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__2050, f_compile_46_mangle_1(v_n))));
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
  u_value s2051 = v_n;
  u_value t2052 = NULL;
  if ((!t2052) && (u_is_con(s2051, &n_syntax_46_Name) && u_is_con(u_field(s2051, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s2051, 0), 0);
    u_value v_b = u_field(s2051, 1);
    static struct u_object l_const_32_struct_32_u_n_2053 = U_LIT("const struct u_name");
    static struct u_object l_n__2054 = U_LIT("n_");
    t2052 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2053), u_concat(&l_n__2054, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t2052) && (u_is_con(s2051, &n_syntax_46_Name) && u_is_con(u_field(s2051, 0), &n_option_46_None))) {
    u_value v_b = u_field(s2051, 1);
    static struct u_object l_const_32_struct_32_u_n_2055 = U_LIT("const struct u_name");
    static struct u_object l_n__2056 = U_LIT("n_");
    static struct u_object l_NULL_2057 = U_LIT("NULL");
    t2052 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2055), u_concat(&l_n__2056, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2057), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t2052) {
    u_no_case(s2051);
  }
  return t2052;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s2058 = v_cs;
  u_value t2059 = NULL;
  if ((!t2059) && u_is_con(s2058, &n_list_46_Nil)) {
    t2059 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2059) && (u_is_con(s2058, &n_list_46_Cons) && u_is_con(u_field(s2058, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s2058, 0), 2);
    u_value v_rest = u_field(s2058, 1);
    u_value t2060 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2061 = &t2060;
    u_value l2062 = v_ks;
    while (u_is_con(l2062, &n_list_46_Cons)) {
      u_value v_k = u_field(l2062, 0);
      if (u_truth(u_mk_bool(1))) {
        e2061 = u_append(e2061, f_syntax_46_Constr_46_name_1(v_k));
      }
      l2062 = u_field(l2062, 1);
    }
    t2059 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t2060, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t2059) && u_is_con(s2058, &n_list_46_Cons)) {
    u_value v_rest = u_field(s2058, 1);
    t2059 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t2059) {
    u_no_case(s2058);
  }
  return t2059;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s2063 = v_ks;
  u_value t2064 = NULL;
  if ((!t2064) && u_is_con(s2063, &n_list_46_Nil)) {
    t2064 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2064) && u_is_con(s2063, &n_list_46_Cons)) {
    u_value v_k = u_field(s2063, 0);
    u_value v_rest = u_field(s2063, 1);
    u_value t2065;
    if (u_truth(f_set_46_member_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t2065 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t2065 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t2064 = t2065;
  }
  if (!t2064) {
    u_no_case(s2063);
  }
  return t2064;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t2066 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2067 = &t2066;
  u_value l2068 = v_xs;
  while (u_is_con(l2068, &n_list_46_Cons)) {
    u_value v_x = u_field(l2068, 0);
    if (u_truth(u_mk_bool(1))) {
      e2067 = u_append(e2067, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l2068 = u_field(l2068, 1);
  }
  return t2066;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__2069 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__2069, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s2070 = f_list_46_lookup_2(v_x, v_env);
  u_value t2071 = NULL;
  if ((!t2071) && u_is_con(s2070, &n_option_46_Some)) {
    u_value v_c = u_field(s2070, 0);
    t2071 = v_c;
  }
  if (!t2071) {
    t2071 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t2071) {
    u_no_case(s2070);
  }
  return t2071;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s2072 = v_c;
  u_value t2073 = NULL;
  if ((!t2073) && (u_is_con(s2072, &n_c_46_CeUnary) && u_equals(u_field(s2072, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s2072, 1);
    t2073 = v_p;
  }
  if (!t2073) {
    static struct u_object l__38__2074 = U_LIT("&");
    t2073 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2074, v_c);
  }
  if (!t2073) {
    u_no_case(s2072);
  }
  return t2073;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s2075 = v_t;
  u_value t2076 = NULL;
  if ((!t2076) && (u_is_con(s2075, &n_syntax_46_TSort) && u_is_con(u_field(s2075, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s2075, 0);
    t2076 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t2076) && u_is_con(s2075, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s2075, 0);
    u_value v_args = u_field(s2075, 1);
    static struct u_object l__91__2077 = U_LIT("[");
    static struct u_object l__44__32__2078 = U_LIT(", ");
    u_value t2079 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2080 = &t2079;
    u_value l2081 = v_args;
    while (u_is_con(l2081, &n_list_46_Cons)) {
      u_value v_a = u_field(l2081, 0);
      if (u_truth(u_mk_bool(1))) {
        e2080 = u_append(e2080, f_compile_46_type_key_1(v_a));
      }
      l2081 = u_field(l2081, 1);
    }
    static struct u_object l__93__2082 = U_LIT("]");
    t2076 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__2077, u_concat(u_str_join(&l__44__32__2078, t2079), &l__93__2082)));
  }
  if ((!t2076) && u_is_con(s2075, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s2075, 0);
    u_value v_r = u_field(s2075, 1);
    static struct u_object l__40__2083 = U_LIT("(");
    static struct u_object l__44__32__2084 = U_LIT(", ");
    u_value t2085 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2086 = &t2085;
    u_value l2087 = v_args;
    while (u_is_con(l2087, &n_list_46_Cons)) {
      u_value v_a = u_field(l2087, 0);
      if (u_truth(u_mk_bool(1))) {
        e2086 = u_append(e2086, f_compile_46_type_key_1(v_a));
      }
      l2087 = u_field(l2087, 1);
    }
    static struct u_object l__41__32__45__62__32__2088 = U_LIT(") -> ");
    t2076 = u_concat(&l__40__2083, u_concat(u_str_join(&l__44__32__2084, t2085), u_concat(&l__41__32__45__62__32__2088, f_compile_46_type_key_1(v_r))));
  }
  if ((!t2076) && u_is_con(s2075, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s2075, 0);
    u_value v_os = u_field(s2075, 1);
    u_value v_rs = u_field(s2075, 2);
    static struct u_object l__40_in_32__2089 = U_LIT("(in ");
    static struct u_object l__44__32__2090 = U_LIT(", ");
    u_value t2091 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2092 = &t2091;
    u_value l2093 = v_is;
    while (u_is_con(l2093, &n_list_46_Cons)) {
      u_value v_a = u_field(l2093, 0);
      if (u_truth(u_mk_bool(1))) {
        e2092 = u_append(e2092, f_compile_46_type_key_1(v_a));
      }
      l2093 = u_field(l2093, 1);
    }
    static struct u_object l__44__32_out_32__2094 = U_LIT(", out ");
    static struct u_object l__44__32__2095 = U_LIT(", ");
    u_value t2096 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2097 = &t2096;
    u_value l2098 = v_os;
    while (u_is_con(l2098, &n_list_46_Cons)) {
      u_value v_a = u_field(l2098, 0);
      if (u_truth(u_mk_bool(1))) {
        e2097 = u_append(e2097, f_compile_46_type_key_1(v_a));
      }
      l2098 = u_field(l2098, 1);
    }
    static struct u_object l__44__32_ref_32__2099 = U_LIT(", ref ");
    static struct u_object l__44__32__2100 = U_LIT(", ");
    u_value t2101 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2102 = &t2101;
    u_value l2103 = v_rs;
    while (u_is_con(l2103, &n_list_46_Cons)) {
      u_value v_a = u_field(l2103, 0);
      if (u_truth(u_mk_bool(1))) {
        e2102 = u_append(e2102, f_compile_46_type_key_1(v_a));
      }
      l2103 = u_field(l2103, 1);
    }
    static struct u_object l__41__2104 = U_LIT(")");
    t2076 = u_concat(&l__40_in_32__2089, u_concat(u_str_join(&l__44__32__2090, t2091), u_concat(&l__44__32_out_32__2094, u_concat(u_str_join(&l__44__32__2095, t2096), u_concat(&l__44__32_ref_32__2099, u_concat(u_str_join(&l__44__32__2100, t2101), &l__41__2104))))));
  }
  if ((!t2076) && u_is_con(s2075, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s2075, 0);
    static struct u_object l__40__2105 = U_LIT("(");
    static struct u_object l__32__42__32__2106 = U_LIT(" * ");
    u_value t2107 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2108 = &t2107;
    u_value l2109 = v_ts;
    while (u_is_con(l2109, &n_list_46_Cons)) {
      u_value v_a = u_field(l2109, 0);
      if (u_truth(u_mk_bool(1))) {
        e2108 = u_append(e2108, f_compile_46_type_key_1(v_a));
      }
      l2109 = u_field(l2109, 1);
    }
    static struct u_object l__41__2110 = U_LIT(")");
    t2076 = u_concat(&l__40__2105, u_concat(u_str_join(&l__32__42__32__2106, t2107), &l__41__2110));
  }
  if (!t2076) {
    static struct u_object l__63__2111 = U_LIT("?");
    t2076 = &l__63__2111;
  }
  if (!t2076) {
    u_no_case(s2075);
  }
  return t2076;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t2112 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2113 = &t2112;
  u_value l2114 = v_cs;
  while (u_is_con(l2114, &n_list_46_Cons)) {
    u_value v_c = u_field(l2114, 0);
    if (u_truth(u_mk_bool(1))) {
      e2113 = u_append(e2113, f_compile_46_fun_arities_1(v_c));
    }
    l2114 = u_field(l2114, 1);
  }
  u_value t2115 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2116 = &t2115;
  u_value l2117 = v_cs;
  while (u_is_con(l2117, &n_list_46_Cons)) {
    u_value v_c = u_field(l2117, 0);
    if (u_truth(u_mk_bool(1))) {
      e2116 = u_append(e2116, f_compile_46_constr_arities_1(v_c));
    }
    l2117 = u_field(l2117, 1);
  }
  u_value t2122 = f_list_46_concat_1(t2112);
  u_value t2123 = f_list_46_concat_1(t2115);
  u_value t2121;
  {
    u_value v_fs = t2122;
    u_value v_ks = t2123;
    u_value t2118 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2119 = &t2118;
    u_value l2120 = v_cs;
    while (u_is_con(l2120, &n_list_46_Cons)) {
      u_value v_c = u_field(l2120, 0);
      if (u_truth(u_mk_bool(1))) {
        e2119 = u_append(e2119, f_compile_46_proc_arities_1(v_c));
      }
      l2120 = u_field(l2120, 1);
    }
    t2121 = u_mk_con(&n_compile_46_Globals, 5, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()), f_map_46_put_all_2(f_list_46_concat_1(t2118), f_map_46_empty_0()));
  }
  return t2121;
}

u_value f_compile_46_proc_arities_1(u_value v_c) {
  u_value s2124 = v_c;
  u_value t2125 = NULL;
  if ((!t2125) && u_is_con(s2124, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s2124, 0);
    u_value v_ins = u_field(s2124, 2);
    u_value v_outs = u_field(s2124, 3);
    u_value v_refs = u_field(s2124, 4);
    t2125 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, f_list_46_length_1(v_ins), u_add(f_list_46_length_1(v_outs), f_list_46_length_1(v_refs)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2125) {
    t2125 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2125) {
    u_no_case(s2124);
  }
  return t2125;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s2126 = v_fs;
  u_value t2127 = NULL;
  if ((!t2127) && u_is_con(s2126, &n_list_46_Nil)) {
    t2127 = v_m;
  }
  if ((!t2127) && u_is_con(s2126, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s2126, 0), 0);
    u_value v_k = u_field(u_field(s2126, 0), 1);
    u_value v_rest = u_field(s2126, 1);
    u_value s2128 = f_map_46_get_2(v_f, v_m);
    u_value t2129 = NULL;
    if ((!t2129) && u_is_con(s2128, &n_option_46_Some)) {
      u_value v_ks = u_field(s2128, 0);
      t2129 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t2129) && u_is_con(s2128, &n_option_46_None)) {
      t2129 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t2129) {
      u_no_case(s2128);
    }
    t2127 = t2129;
  }
  if (!t2127) {
    u_no_case(s2126);
  }
  return t2127;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s2130 = v_gl;
  u_value t2131 = NULL;
  if ((!t2131) && u_is_con(s2130, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s2130, 2);
    u_value s2132 = f_map_46_get_2(v_n, v_fm);
    u_value t2133 = NULL;
    if ((!t2133) && u_is_con(s2132, &n_option_46_Some)) {
      u_value v_ks = u_field(s2132, 0);
      t2133 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t2133) && u_is_con(s2132, &n_option_46_None)) {
      t2133 = u_mk_bool(0);
    }
    if (!t2133) {
      u_no_case(s2132);
    }
    t2131 = t2133;
  }
  if (!t2131) {
    u_no_case(s2130);
  }
  return t2131;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s2134 = v_gl;
  u_value t2135 = NULL;
  if ((!t2135) && u_is_con(s2134, &n_compile_46_Globals)) {
    u_value v_km = u_field(s2134, 3);
    t2135 = f_map_46_get_2(v_n, v_km);
  }
  if (!t2135) {
    u_no_case(s2134);
  }
  return t2135;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s2136 = v_c;
  u_value t2137 = NULL;
  if ((!t2137) && u_is_con(s2136, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s2136, 0);
    u_value v_xss = u_field(s2136, 2);
    t2137 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2137) {
    t2137 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2137) {
    u_no_case(s2136);
  }
  return t2137;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s2138 = v_c;
  u_value t2139 = NULL;
  if ((!t2139) && u_is_con(s2138, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s2138, 2);
    u_value t2140 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2141 = &t2140;
    u_value l2142 = v_ks;
    while (u_is_con(l2142, &n_list_46_Cons)) {
      u_value v_k = u_field(l2142, 0);
      if (u_truth(u_mk_bool(1))) {
        e2141 = u_append(e2141, f_compile_46_constr_arity_1(v_k));
      }
      l2142 = u_field(l2142, 1);
    }
    t2139 = t2140;
  }
  if (!t2139) {
    t2139 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2139) {
    u_no_case(s2138);
  }
  return t2139;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s2143 = v_k;
  u_value t2144 = NULL;
  if ((!t2144) && u_is_con(s2143, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s2143, 0);
    u_value v_ts = u_field(s2143, 1);
    t2144 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t2144) {
    u_no_case(s2143);
  }
  return t2144;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s2145 = v_xs;
  u_value t2146 = NULL;
  if ((!t2146) && u_is_con(s2145, &n_list_46_Nil)) {
    t2146 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2146) && u_is_con(s2145, &n_list_46_Cons)) {
    u_value v_x = u_field(s2145, 0);
    u_value v_rest = u_field(s2145, 1);
    t2146 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t2146) {
    u_no_case(s2145);
  }
  return t2146;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_2147 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_2147, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_2148 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_2148, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s2149 = v_cs;
  u_value t2150 = NULL;
  if ((!t2150) && u_is_con(s2149, &n_list_46_Nil)) {
    t2150 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t2150) && (u_is_con(s2149, &n_list_46_Cons) && u_is_con(u_field(s2149, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s2149, 0);
    t2150 = v_d;
  }
  if ((!t2150) && u_is_con(s2149, &n_list_46_Cons)) {
    u_value v_d = u_field(s2149, 0);
    u_value v_rest = u_field(s2149, 1);
    static struct u_object l__38__38__2151 = U_LIT("&&");
    t2150 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__2151, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t2150) {
    u_no_case(s2149);
  }
  return t2150;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s2152 = v_l;
  u_value t2153 = NULL;
  if ((!t2153) && u_is_con(s2152, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_2154 = U_LIT("u_mk_tuple");
    t2153 = f_c_46_c_call_2(&l_u_mk_tuple_2154, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2153) && u_is_con(s2152, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s2152, 0);
    static struct u_object l_u_mk_bool_2155 = U_LIT("u_mk_bool");
    u_value t2156;
    if (u_truth(v_b)) {
      t2156 = u_mk_int(1);
    } else {
      t2156 = u_mk_int(0);
    }
    t2153 = f_c_46_c_call_2(&l_u_mk_bool_2155, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t2156), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2153) && u_is_con(s2152, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s2152, 0);
    u_value t2157 = u_lt(u_mk_int_str("-1000000000000000000"), v_n);
    if (u_truth(t2157)) {
      t2157 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t2160;
    if (u_truth(t2157)) {
      static struct u_object l_u_mk_int_2158 = U_LIT("u_mk_int");
      t2160 = f_c_46_c_call_2(&l_u_mk_int_2158, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_2159 = U_LIT("u_mk_int_str");
      t2160 = f_c_46_c_call_2(&l_u_mk_int_str_2159, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t2153 = t2160;
  }
  if ((!t2153) && u_is_con(s2152, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s2152, 0);
    static struct u_object l_u_mk_lit_2161 = U_LIT("u_mk_lit");
    t2153 = f_c_46_c_call_2(&l_u_mk_lit_2161, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2153) && u_is_con(s2152, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s2152, 0);
    static struct u_object l_u_mk_char_2162 = U_LIT("u_mk_char");
    t2153 = f_c_46_c_call_2(&l_u_mk_char_2162, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2153) {
    u_no_case(s2152);
  }
  return t2153;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s2163 = v_p;
  u_value t2164 = NULL;
  if ((!t2164) && u_is_con(s2163, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s2163, 0);
    static struct u_object l_u_equals_2165 = U_LIT("u_equals");
    t2164 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_2165, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2164) && u_is_con(s2163, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s2163, 0);
    u_value v_ps = u_field(s2163, 1);
    static struct u_object l_u_is_con_2166 = U_LIT("u_is_con");
    t2164 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_2166, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t2164) && u_is_con(s2163, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2163, 0);
    t2164 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2164) {
    t2164 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2164) {
    u_no_case(s2163);
  }
  return t2164;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s2167 = v_ps;
  u_value t2168 = NULL;
  if ((!t2168) && u_is_con(s2167, &n_list_46_Nil)) {
    t2168 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2168) && u_is_con(s2167, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2167, 0), 0);
    u_value v_i = u_field(u_field(s2167, 0), 1);
    u_value v_rest = u_field(s2167, 1);
    t2168 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t2168) {
    u_no_case(s2167);
  }
  return t2168;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s2169 = v_p;
  u_value t2170 = NULL;
  if ((!t2170) && (u_is_con(s2169, &n_syntax_46_PVar) && u_is_con(u_field(s2169, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2169, 0), 0);
    t2170 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2170) && u_is_con(s2169, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2169, 1);
    t2170 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t2170) && u_is_con(s2169, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2169, 0);
    t2170 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2170) {
    t2170 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2170) {
    u_no_case(s2169);
  }
  return t2170;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s2171 = v_ps;
  u_value t2172 = NULL;
  if ((!t2172) && u_is_con(s2171, &n_list_46_Nil)) {
    t2172 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2172) && u_is_con(s2171, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2171, 0), 0);
    u_value v_i = u_field(u_field(s2171, 0), 1);
    u_value v_rest = u_field(s2171, 1);
    t2172 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t2172) {
    u_no_case(s2171);
  }
  return t2172;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s2173 = v_p;
  u_value t2174 = NULL;
  if ((!t2174) && (u_is_con(s2173, &n_syntax_46_PVar) && u_is_con(u_field(s2173, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2173, 0), 0);
    t2174 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2174) && u_is_con(s2173, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2173, 1);
    u_value t2175 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2176 = &t2175;
    u_value l2177 = v_ps;
    while (u_is_con(l2177, &n_list_46_Cons)) {
      u_value v_q = u_field(l2177, 0);
      if (u_truth(u_mk_bool(1))) {
        e2176 = u_append(e2176, f_compile_46_pattern_names_1(v_q));
      }
      l2177 = u_field(l2177, 1);
    }
    t2174 = f_list_46_concat_1(t2175);
  }
  if ((!t2174) && u_is_con(s2173, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2173, 0);
    u_value t2178 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2179 = &t2178;
    u_value l2180 = v_ps;
    while (u_is_con(l2180, &n_list_46_Cons)) {
      u_value v_q = u_field(l2180, 0);
      if (u_truth(u_mk_bool(1))) {
        e2179 = u_append(e2179, f_compile_46_pattern_names_1(v_q));
      }
      l2180 = u_field(l2180, 1);
    }
    t2174 = f_list_46_concat_1(t2178);
  }
  if (!t2174) {
    t2174 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2174) {
    u_no_case(s2173);
  }
  return t2174;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s2181 = v_e;
  u_value t2182 = NULL;
  if ((!t2182) && u_is_con(s2181, &n_syntax_46_ELit)) {
    t2182 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2182) && (u_is_con(s2181, &n_syntax_46_EVar) && u_is_con(u_field(s2181, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2181, 0), 0);
    u_value t2183;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t2183 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t2183 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t2182 = t2183;
  }
  if ((!t2182) && u_is_con(s2181, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s2181, 0);
    u_value v_args = u_field(s2181, 1);
    u_value t2184 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2185 = &t2184;
    u_value l2186 = v_args;
    while (u_is_con(l2186, &n_list_46_Cons)) {
      u_value v_a = u_field(l2186, 0);
      if (u_truth(u_mk_bool(1))) {
        e2185 = u_append(e2185, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2186 = u_field(l2186, 1);
    }
    t2182 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t2184));
  }
  if ((!t2182) && u_is_con(s2181, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s2181, 0);
    u_value v_b = u_field(s2181, 1);
    t2182 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t2182) && u_is_con(s2181, &n_syntax_46_EProc)) {
    u_value v_is = u_field(s2181, 0);
    u_value v_os = u_field(s2181, 1);
    u_value v_rs = u_field(s2181, 2);
    u_value v_b = u_field(s2181, 3);
    t2182 = f_compile_46_free_in_stmt_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), v_bound));
  }
  if ((!t2182) && u_is_con(s2181, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s2181, 0);
    u_value v_b = u_field(s2181, 1);
    u_value t2187 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2188 = &t2187;
    u_value l2189 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l2189, &n_list_46_Cons)) {
      u_value v_a = u_field(l2189, 0);
      if (u_truth(u_mk_bool(1))) {
        e2188 = u_append(e2188, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2189 = u_field(l2189, 1);
    }
    t2182 = f_list_46__43__43__2(f_list_46_concat_1(t2187), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t2182) && u_is_con(s2181, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s2181, 0);
    u_value v_l = u_field(s2181, 1);
    u_value v_r = u_field(s2181, 2);
    t2182 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t2182) && u_is_con(s2181, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s2181, 0);
    u_value v_cs = u_field(s2181, 1);
    u_value t2190 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2191 = &t2190;
    u_value l2192 = v_cs;
    while (u_is_con(l2192, &n_list_46_Cons)) {
      u_value v_b = u_field(l2192, 0);
      if (u_truth(u_mk_bool(1))) {
        e2191 = u_append(e2191, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l2192 = u_field(l2192, 1);
    }
    t2182 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2190));
  }
  if ((!t2182) && u_is_con(s2181, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s2181, 0);
    u_value v_xs = u_field(s2181, 1);
    u_value v_ls = u_field(s2181, 2);
    u_value v_c = u_field(s2181, 3);
    u_value t2193 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2194 = &t2193;
    u_value l2195 = v_ls;
    while (u_is_con(l2195, &n_list_46_Cons)) {
      u_value v_l = u_field(l2195, 0);
      if (u_truth(u_mk_bool(1))) {
        e2194 = u_append(e2194, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2195 = u_field(l2195, 1);
    }
    t2182 = f_list_46__43__43__2(f_list_46_concat_1(t2193), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t2182) && u_is_con(s2181, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s2181, 0);
    u_value t2196 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2197 = &t2196;
    u_value l2198 = v_es;
    while (u_is_con(l2198, &n_list_46_Cons)) {
      u_value v_a = u_field(l2198, 0);
      if (u_truth(u_mk_bool(1))) {
        e2197 = u_append(e2197, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2198 = u_field(l2198, 1);
    }
    t2182 = f_list_46_concat_1(t2196);
  }
  if ((!t2182) && u_is_con(s2181, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s2181, 0);
    t2182 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if (!t2182) {
    u_no_case(s2181);
  }
  return t2182;
}

u_value f_compile_46_free_in_stmt_2(u_value v_x, u_value v_bound) {
  u_value s2199 = v_x;
  u_value t2200 = NULL;
  if ((!t2200) && u_is_con(s2199, &n_syntax_46_SSkip)) {
    t2200 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2200) && u_is_con(s2199, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s2199, 0);
    u_value t2201 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2202 = &t2201;
    u_value l2203 = f_syntax_46_names_1(f_list_46_firsts_1(v_ps));
    while (u_is_con(l2203, &n_list_46_Cons)) {
      u_value v_y = u_field(l2203, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2202 = u_append(e2202, v_y);
      }
      l2203 = u_field(l2203, 1);
    }
    u_value t2204 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2205 = &t2204;
    u_value l2206 = f_list_46_seconds_1(v_ps);
    while (u_is_con(l2206, &n_list_46_Cons)) {
      u_value v_a = u_field(l2206, 0);
      if (u_truth(u_mk_bool(1))) {
        e2205 = u_append(e2205, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2206 = u_field(l2206, 1);
    }
    t2200 = f_list_46__43__43__2(t2201, f_list_46_concat_1(t2204));
  }
  if ((!t2200) && u_is_con(s2199, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s2199, 0);
    u_value v_ys = u_field(s2199, 1);
    u_value t2207 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2208 = &t2207;
    u_value l2209 = v_ys;
    while (u_is_con(l2209, &n_list_46_Cons)) {
      u_value v_y = u_field(l2209, 0);
      if (u_truth(u_mk_bool(1))) {
        e2208 = u_append(e2208, f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2209 = u_field(l2209, 1);
    }
    t2200 = f_list_46_concat_1(t2207);
  }
  if ((!t2200) && u_is_con(s2199, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s2199, 0);
    u_value v_l = u_field(s2199, 1);
    u_value v_r = u_field(s2199, 2);
    t2200 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_l, v_bound), f_compile_46_free_in_stmt_2(v_r, v_bound)));
  }
  if ((!t2200) && u_is_con(s2199, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s2199, 0);
    u_value v_b = u_field(s2199, 1);
    t2200 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_compile_46_free_in_stmt_2(v_b, v_bound));
  }
  if ((!t2200) && u_is_con(s2199, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s2199, 0);
    u_value v_ins = u_field(s2199, 1);
    u_value v_outs = u_field(s2199, 2);
    u_value v_refs = u_field(s2199, 3);
    u_value t2210 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2211 = &t2210;
    u_value l2212 = u_mk_con(&n_list_46_Cons, 2, v_p, f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
    while (u_is_con(l2212, &n_list_46_Cons)) {
      u_value v_y = u_field(l2212, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2211 = u_append(e2211, v_y);
      }
      l2212 = u_field(l2212, 1);
    }
    u_value t2213 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2214 = &t2213;
    u_value l2215 = v_ins;
    while (u_is_con(l2215, &n_list_46_Cons)) {
      u_value v_a = u_field(l2215, 0);
      if (u_truth(u_mk_bool(1))) {
        e2214 = u_append(e2214, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2215 = u_field(l2215, 1);
    }
    t2200 = f_list_46__43__43__2(t2210, f_list_46_concat_1(t2213));
  }
  if ((!t2200) && u_is_con(s2199, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s2199, 0);
    u_value v_bs = u_field(s2199, 1);
    u_value t2216 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2217 = &t2216;
    u_value l2218 = v_bs;
    while (u_is_con(l2218, &n_list_46_Cons)) {
      u_value v_b = u_field(l2218, 0);
      if (u_truth(u_mk_bool(1))) {
        e2217 = u_append(e2217, f_compile_46_free_in_stmt_branch_2(v_b, v_bound));
      }
      l2218 = u_field(l2218, 1);
    }
    t2200 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2216));
  }
  if ((!t2200) && u_is_con(s2199, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s2199, 0);
    t2200 = f_compile_46_free_vars_2(v_c, v_bound);
  }
  if ((!t2200) && u_is_con(s2199, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s2199, 0);
    t2200 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if ((!t2200) && u_is_con(s2199, &n_syntax_46_STry)) {
    u_value v_b = u_field(s2199, 0);
    u_value v_hs = u_field(s2199, 1);
    u_value t2219 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2220 = &t2219;
    u_value l2221 = v_hs;
    while (u_is_con(l2221, &n_list_46_Cons)) {
      u_value v_h = u_field(l2221, 0);
      if (u_truth(u_mk_bool(1))) {
        e2220 = u_append(e2220, f_compile_46_free_in_handler_2(v_h, v_bound));
      }
      l2221 = u_field(l2221, 1);
    }
    t2200 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_b, v_bound), f_list_46_concat_1(t2219));
  }
  if (!t2200) {
    u_no_case(s2199);
  }
  return t2200;
}

u_value f_compile_46_free_in_stmt_branch_2(u_value v_b, u_value v_bound) {
  u_value s2222 = v_b;
  u_value t2223 = NULL;
  if ((!t2223) && (u_is_con(s2222, &n_syntax_46_Branch) && u_is_con(u_field(s2222, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2222, 0);
    u_value v_g = u_field(u_field(s2222, 1), 0);
    u_value v_y = u_field(s2222, 2);
    t2223 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2223) && (u_is_con(s2222, &n_syntax_46_Branch) && u_is_con(u_field(s2222, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2222, 0);
    u_value v_y = u_field(s2222, 2);
    t2223 = f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2223) {
    u_no_case(s2222);
  }
  return t2223;
}

u_value f_compile_46_free_in_handler_2(u_value v_h, u_value v_bound) {
  u_value t2227 = f_list_46__43__43__2(f_compile_46_pattern_names_1(f_syntax_46_Handler_46_pat_1(v_h)), v_bound);
  u_value t2226;
  {
    u_value v_inner = t2227;
    u_value s2224 = f_syntax_46_Handler_46_guard_1(v_h);
    u_value t2225 = NULL;
    if ((!t2225) && u_is_con(s2224, &n_option_46_Some)) {
      u_value v_g = u_field(s2224, 0);
      t2225 = f_compile_46_free_vars_2(v_g, v_inner);
    }
    if ((!t2225) && u_is_con(s2224, &n_option_46_None)) {
      t2225 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t2225) {
      u_no_case(s2224);
    }
    t2226 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(f_syntax_46_Handler_46_body_1(v_h), v_inner), t2225);
  }
  return t2226;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s2228 = v_b;
  u_value t2229 = NULL;
  if ((!t2229) && (u_is_con(s2228, &n_syntax_46_Branch) && u_is_con(u_field(s2228, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2228, 0);
    u_value v_g = u_field(u_field(s2228, 1), 0);
    u_value v_e = u_field(s2228, 2);
    t2229 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2229) && (u_is_con(s2228, &n_syntax_46_Branch) && u_is_con(u_field(s2228, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2228, 0);
    u_value v_e = u_field(s2228, 2);
    t2229 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2229) {
    u_no_case(s2228);
  }
  return t2229;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s2230 = v_xss;
  u_value t2231 = NULL;
  if ((!t2231) && u_is_con(s2230, &n_list_46_Nil)) {
    t2231 = v_b;
  }
  if ((!t2231) && u_is_con(s2230, &n_list_46_Cons)) {
    u_value v_xs = u_field(s2230, 0);
    u_value v_rest = u_field(s2230, 1);
    t2231 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t2231) {
    u_no_case(s2230);
  }
  return t2231;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value t2232 = u_concat(v_stem, u_str_show(f_compile_46_Gen_46_count_1(*r_g)));
    u_value t2233 = f_compile_46_Gen_46_count_58__61__2(*r_g, u_add(f_compile_46_Gen_46_count_1(*r_g), u_mk_int(1)));
    *r_x = t2232;
    *r_g = t2233;
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value t2234 = f_compile_46_Gen_46_lifted_58__61__2(*r_g, f_list_46__43__43__2(f_compile_46_Gen_46_lifted_1(*r_g), u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
    *r_g = t2234;
  }
}

void p_compile_46_global_value(u_value v_gl, u_value v_n, u_value v_t, u_value *r_r) {
  {
    if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
      {
        static struct u_object l_u_mk_closure_2235 = U_LIT("u_mk_closure");
        static struct u_object l_w__2236 = U_LIT("w_");
        u_value t2237 = f_c_46_c_call_2(&l_u_mk_closure_2235, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__2236, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        *r_r = t2237;
      }
    } else {
      {
        u_value s2238 = f_compile_46_fields_count_2(v_gl, v_n);
        int d2239 = 0;
        if ((!d2239) && (u_is_con(s2238, &n_option_46_Some) && u_equals(u_field(s2238, 0), u_mk_int(0)))) {
          d2239 = 1;
          {
            static struct u_object l_u_mk_con_2240 = U_LIT("u_mk_con");
            u_value t2241 = f_c_46_c_call_2(&l_u_mk_con_2240, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t2241;
          }
        }
        if ((!d2239) && u_is_con(s2238, &n_option_46_Some)) {
          d2239 = 1;
          {
            static struct u_object l_u_mk_closure_2242 = U_LIT("u_mk_closure");
            static struct u_object l_wc__2243 = U_LIT("wc_");
            u_value t2244 = f_c_46_c_call_2(&l_u_mk_closure_2242, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__2243, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t2244;
          }
        }
        if ((!d2239) && u_is_con(s2238, &n_option_46_None)) {
          d2239 = 1;
          {
            u_value s2245 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
            int d2246 = 0;
            if ((!d2246) && u_is_con(s2245, &n_option_46_Some)) {
              u_value v_f = u_field(u_field(s2245, 0), 0);
              d2246 = 1;
              {
                static struct u_object l_u_mk_closure_2247 = U_LIT("u_mk_closure");
                static struct u_object l_w_2248 = U_LIT("w");
                u_value t2249 = f_c_46_c_call_2(&l_u_mk_closure_2247, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_2248, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
                *r_r = t2249;
              }
            }
            if ((!d2246) && u_is_con(s2245, &n_option_46_None)) {
              if (u_truth(f_map_46_has_2(v_n, f_compile_46_Globals_46_procs_1(v_gl)))) {
                d2246 = 1;
                {
                  static struct u_object l_u_mk_proc_2250 = U_LIT("u_mk_proc");
                  static struct u_object l_wp__2251 = U_LIT("wp_");
                  u_value t2252 = f_c_46_c_call_2(&l_u_mk_proc_2250, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wp__2251, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
                  *r_r = t2252;
                }
              }
            }
            if ((!d2246) && u_is_con(s2245, &n_option_46_None)) {
              d2246 = 1;
              {
                static struct u_object l_the_32_variable_32__2253 = U_LIT("the variable ");
                u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__2253, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
              }
            }
            if (!d2246) {
              u_no_case(s2245);
            }
          }
        }
        if (!d2239) {
          u_no_case(s2238);
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
    u_value s2254 = v_e;
    int d2255 = 0;
    if ((!d2255) && (u_is_con(s2254, &n_syntax_46_ELit) && u_is_con(u_field(s2254, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s2254, 0), 0);
      d2255 = 1;
      {
        static struct u_object l_l__2256 = U_LIT("l_");
        u_value t2257;
        if (u_truth(u_lt(u_str_size(v_s), u_mk_int(16)))) {
          t2257 = u_str_size(v_s);
        } else {
          t2257 = u_mk_int(16);
        }
        static struct u_object l___2258 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__2256, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), t2257))), &l___2258)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__2259 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_2260 = U_LIT("U_LIT");
        static struct u_object l__38__2261 = U_LIT("&");
        u_value t2262 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__2259), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_2260, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t2263 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2261, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t2262;
        *r_r = t2263;
      }
    }
    if ((!d2255) && u_is_con(s2254, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s2254, 0);
      d2255 = 1;
      {
        u_value t2264 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2265 = f_compile_46_literal_value_1(v_l);
        *r_ss = t2264;
        *r_r = t2265;
      }
    }
    if ((!d2255) && (u_is_con(s2254, &n_syntax_46_EVar) && u_is_con(u_field(s2254, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s2254, 0), 0);
      u_value v_ty = u_field(u_field(s2254, 0), 1);
      d2255 = 1;
      {
        u_value s2266 = f_list_46_lookup_2(v_n, v_env);
        int d2267 = 0;
        if ((!d2267) && u_is_con(s2266, &n_option_46_Some)) {
          u_value v_c = u_field(s2266, 0);
          d2267 = 1;
          {
            u_value t2268 = u_mk_con(&n_list_46_Nil, 0);
            u_value t2269 = v_c;
            *r_ss = t2268;
            *r_r = t2269;
          }
        }
        if ((!d2267) && u_is_con(s2266, &n_option_46_None)) {
          d2267 = 1;
          {
            p_compile_46_global_value(v_gl, v_n, v_ty, r_r);
            u_value t2270 = u_mk_con(&n_list_46_Nil, 0);
            *r_ss = t2270;
          }
        }
        if (!d2267) {
          u_no_case(s2266);
        }
      }
    }
    if ((!d2255) && (u_is_con(s2254, &n_syntax_46_EApp) && (u_is_con(u_field(s2254, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s2254, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s2254, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s2254, 0), 0), 1);
      u_value v_args = u_field(s2254, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d2255 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d2255) && u_is_con(s2254, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s2254, 0);
      u_value v_args = u_field(s2254, 1);
      d2255 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_2271 = U_LIT("u_apply");
        u_value t2272 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2273 = f_c_46_c_call_2(&l_u_apply_2271, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t2272;
        *r_r = t2273;
      }
    }
    if ((!d2255) && u_is_con(s2254, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s2254, 0);
      u_value v_b = u_field(s2254, 1);
      d2255 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2255) && u_is_con(s2254, &n_syntax_46_EProc)) {
      u_value v_is = u_field(s2254, 0);
      u_value v_os = u_field(s2254, 1);
      u_value v_rs = u_field(s2254, 2);
      u_value v_b = u_field(s2254, 3);
      d2255 = 1;
      {
        p_compile_46_compile_proc_lambda(v_gl, v_env, v_is, f_list_46__43__43__2(v_os, v_rs), v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2255) && u_is_con(s2254, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s2254, 0);
      u_value v_b = u_field(s2254, 1);
      d2255 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2274 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2274, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2275 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t2275;
        u_value t2276 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2276;
      }
    }
    if ((!d2255) && u_is_con(s2254, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s2254, 0);
      u_value v_l = u_field(s2254, 1);
      u_value v_rr = u_field(s2254, 2);
      d2255 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2277 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2277, &v_t, r_g);
        u_value t2278 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2278;
        u_value t2279 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2279;
      }
    }
    if ((!d2255) && u_is_con(s2254, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s2254, 0);
      u_value v_cases = u_field(s2254, 1);
      d2255 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2280 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2280, &v_d, r_g);
        static struct u_object l_t_2281 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2281, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_2282 = U_LIT("NULL");
        u_value t2283 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2282))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2283;
        static struct u_object l__33__2284 = U_LIT("!");
        static struct u_object l_u_no_case_2285 = U_LIT("u_no_case");
        u_value t2286 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2284, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2285, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2286;
        u_value t2287 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2287;
      }
    }
    if ((!d2255) && u_is_con(s2254, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s2254, 0);
      u_value v_xs = u_field(s2254, 1);
      u_value v_lists = u_field(s2254, 2);
      u_value v_c = u_field(s2254, 3);
      d2255 = 1;
      {
        static struct u_object l_t_2288 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2288, &v_t, r_g);
        static struct u_object l_e_2289 = U_LIT("e");
        p_compile_46_gen_name(&l_e_2289, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_2290 = U_LIT("u_mk_con");
        static struct u_object l__38__2291 = U_LIT("&");
        u_value t2292 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_2290, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__2291, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t2292;
        u_value t2293 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2293;
      }
    }
    if ((!d2255) && u_is_con(s2254, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s2254, 0);
      d2255 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_2294 = U_LIT("u_mk_tuple");
        u_value t2295 = f_c_46_c_call_2(&l_u_mk_tuple_2294, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t2295;
      }
    }
    if ((!d2255) && u_is_con(s2254, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s2254, 0);
      u_value v_ty = u_field(s2254, 1);
      d2255 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2296 = U_LIT("u_raise");
        static struct u_object l_NULL_2297 = U_LIT("NULL");
        u_value t2298 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2296, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        u_value t2299 = u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2297);
        *r_ss = t2298;
        *r_r = t2299;
      }
    }
    if (!d2255) {
      u_no_case(s2254);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s2300 = v_es;
    int d2301 = 0;
    if ((!d2301) && u_is_con(s2300, &n_list_46_Nil)) {
      d2301 = 1;
      {
        u_value t2302 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2303 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2302;
        *r_rs = t2303;
      }
    }
    if ((!d2301) && u_is_con(s2300, &n_list_46_Cons)) {
      u_value v_e = u_field(s2300, 0);
      u_value v_rest = u_field(s2300, 1);
      d2301 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t2304 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2305 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t2304;
        *r_rs = t2305;
      }
    }
    if (!d2301) {
      u_no_case(s2300);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s2306 = v_rs;
    int d2307 = 0;
    if ((!d2307) && u_is_con(s2306, &n_list_46_Nil)) {
      d2307 = 1;
      {
        u_value t2308 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2308;
      }
    }
    if ((!d2307) && u_is_con(s2306, &n_list_46_Cons)) {
      u_value v_r = u_field(s2306, 0);
      u_value v_rest = u_field(s2306, 1);
      d2307 = 1;
      {
        static struct u_object l_t_2309 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2309, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t2310 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t2310;
      }
    }
    if (!d2307) {
      u_no_case(s2306);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s2311 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2312 = NULL;
  if ((!t2312) && (u_is_con(u_field(s2311, 0), &n_list_46_Cons) && (u_is_con(u_field(s2311, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2311, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2311, 0), 0);
    u_value v_more = u_field(u_field(s2311, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2311, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2311, 1), 1);
    t2312 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t2312) {
    t2312 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2312) {
    u_no_case(s2311);
  }
  return t2312;
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
    u_value s2313 = v_args;
    int d2314 = 0;
    if ((!d2314) && (u_is_con(s2313, &n_list_46_Cons) && (u_is_con(u_field(s2313, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2313, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s2313, 0);
      u_value v_b = u_field(u_field(s2313, 1), 0);
      static struct u_object l_and_2321 = U_LIT("and");
      u_value t2323 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2321));
      if (!u_truth(t2323)) {
        static struct u_object l_or_2322 = U_LIT("or");
        t2323 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_2322));
      }
      if (u_truth(t2323)) {
        d2314 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_2315 = U_LIT("t");
          p_compile_46_gen_name(&l_t_2315, &v_t, r_g);
          static struct u_object l_and_2316 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2316)))) {
            {
              u_value t2317 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2317;
            }
          } else {
            {
              static struct u_object l__33__2318 = U_LIT("!");
              u_value t2319 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2318, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2319;
            }
          }
          u_value t2320 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t2320;
        }
      }
    }
    if (!d2314) {
      d2314 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t2324 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t2324)) {
          t2324 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t2324)) {
          {
            u_value t2325 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t2325;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_2326 = U_LIT("u_mk_con");
              u_value t2327 = f_c_46_c_call_2(&l_u_mk_con_2326, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t2327;
            }
          } else {
            {
              u_value s2328 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
              int d2329 = 0;
              if ((!d2329) && u_is_con(s2328, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s2328, 0), 0);
                u_value v_k = u_field(u_field(s2328, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d2329 = 1;
                  {
                    u_value t2330 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t2330;
                  }
                }
              }
              if (!d2329) {
                d2329 = 1;
                {
                  p_compile_46_global_value(v_gl, v_n, v_ty, &v_f);
                  static struct u_object l_u_apply_2331 = U_LIT("u_apply");
                  u_value t2332 = f_c_46_c_call_2(&l_u_apply_2331, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t2332;
                }
              }
              if (!d2329) {
                u_no_case(s2328);
              }
            }
          }
        }
      }
    }
    if (!d2314) {
      u_no_case(s2313);
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
    u_value t2333 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2334 = &t2333;
    u_value l2335 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l2335, &n_list_46_Cons)) {
      u_value v_x = u_field(l2335, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2334 = u_append(e2334, v_x);
      }
      l2335 = u_field(l2335, 1);
    }
    u_value t2336 = t2333;
    v_caps = t2336;
    static struct u_object l_lam_2337 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_2337, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_2338 = U_LIT("env");
    static struct u_object l_args_2339 = U_LIT("args");
    u_value t2340 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2338), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_2339));
    v_decls = t2340;
    static struct u_object l_env_2341 = U_LIT("env");
    static struct u_object l_args_2342 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2341), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2342), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_2343 = U_LIT("u_mk_closure");
    u_value t2344 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2345 = &t2344;
    u_value l2346 = v_caps;
    while (u_is_con(l2346, &n_list_46_Cons)) {
      u_value v_x = u_field(l2346, 0);
      if (u_truth(u_mk_bool(1))) {
        e2345 = u_append(e2345, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2346 = u_field(l2346, 1);
    }
    u_value t2347 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2348 = f_c_46_c_call_2(&l_u_mk_closure_2343, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2344));
    *r_ss = t2347;
    *r_r = t2348;
  }
}

void p_compile_46_compile_proc_lambda(u_value v_gl, u_value v_env, u_value v_ins, u_value v_outs, u_value v_b, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_caps = NULL;
    u_value v_f = NULL;
    u_value v_bs = NULL;
    u_value v_decls = NULL;
    u_value t2349 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2350 = &t2349;
    u_value l2351 = f_list_46_distinct_1(f_compile_46_free_in_stmt_2(v_b, f_syntax_46_names_1(f_list_46__43__43__2(v_ins, v_outs))));
    while (u_is_con(l2351, &n_list_46_Cons)) {
      u_value v_x = u_field(l2351, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2350 = u_append(e2350, v_x);
      }
      l2351 = u_field(l2351, 1);
    }
    u_value t2352 = t2349;
    v_caps = t2352;
    static struct u_object l_proc_2353 = U_LIT("proc");
    p_compile_46_gen_name(&l_proc_2353, &v_f, r_g);
    u_value t2354 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2355 = &t2354;
    u_value l2356 = f_syntax_46_names_1(v_outs);
    while (u_is_con(l2356, &n_list_46_Cons)) {
      u_value v_x = u_field(l2356, 0);
      if (u_truth(u_mk_bool(1))) {
        e2355 = u_append(e2355, f_compile_46_pointer_1(v_x));
      }
      l2356 = u_field(l2356, 1);
    }
    p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_ins))), t2354), v_b, &v_bs, r_g);
    static struct u_object l_env_2357 = U_LIT("env");
    static struct u_object l_ins_2358 = U_LIT("ins");
    u_value t2359 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2357), f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_ins), u_mk_int(0)), &l_ins_2358), f_compile_46_unpack_pointers_1(f_compile_46_numbered_2(f_syntax_46_names_1(v_outs), u_mk_int(0)))));
    v_decls = t2359;
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), v_f, f_compile_46_proc_params_0(), f_list_46__43__43__2(v_decls, v_bs)), r_g);
    static struct u_object l_u_mk_proc_2360 = U_LIT("u_mk_proc");
    u_value t2361 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2362 = &t2361;
    u_value l2363 = v_caps;
    while (u_is_con(l2363, &n_list_46_Cons)) {
      u_value v_x = u_field(l2363, 0);
      if (u_truth(u_mk_bool(1))) {
        e2362 = u_append(e2362, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2363 = u_field(l2363, 1);
    }
    u_value t2364 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2365 = f_c_46_c_call_2(&l_u_mk_proc_2360, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2361));
    *r_ss = t2364;
    *r_r = t2365;
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
  static struct u_object l_env_2366 = U_LIT("env");
  static struct u_object l_ins_2367 = U_LIT("ins");
  static struct u_object l_outs_2368 = U_LIT("outs");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2366), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_ins_2367), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, f_compile_46_value_ptr_0()), &l_outs_2368), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_compile_46_unpack_pointers_1(u_value v_xs) {
  u_value s2369 = v_xs;
  u_value t2370 = NULL;
  if ((!t2370) && u_is_con(s2369, &n_list_46_Nil)) {
    t2370 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2370) && u_is_con(s2369, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2369, 0), 0);
    u_value v_i = u_field(u_field(s2369, 0), 1);
    u_value v_rest = u_field(s2369, 1);
    static struct u_object l_outs_2371 = U_LIT("outs");
    t2370 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(&l_outs_2371, v_i))), f_compile_46_unpack_pointers_1(v_rest));
  }
  if (!t2370) {
    u_no_case(s2369);
  }
  return t2370;
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s2372 = v_xs;
  u_value t2373 = NULL;
  if ((!t2373) && u_is_con(s2372, &n_list_46_Nil)) {
    t2373 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2373) && u_is_con(s2372, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2372, 0), 0);
    u_value v_i = u_field(u_field(s2372, 0), 1);
    u_value v_rest = u_field(s2372, 1);
    t2373 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t2373) {
    u_no_case(s2372);
  }
  return t2373;
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
    u_value s2374 = v_cases;
    int d2375 = 0;
    if ((!d2375) && u_is_con(s2374, &n_list_46_Nil)) {
      d2375 = 1;
      {
        u_value t2376 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2376;
      }
    }
    if ((!d2375) && (u_is_con(s2374, &n_list_46_Cons) && u_is_con(u_field(s2374, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2374, 0), 0);
      u_value v_guard = u_field(u_field(s2374, 0), 1);
      u_value v_e = u_field(u_field(s2374, 0), 2);
      u_value v_more = u_field(s2374, 1);
      d2375 = 1;
      {
        u_value t2377 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2377;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2378 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t2378;
        u_value s2379 = v_guard;
        int d2380 = 0;
        if ((!d2380) && u_is_con(s2379, &n_option_46_Some)) {
          u_value v_c = u_field(s2379, 0);
          d2380 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2381 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2381;
          }
        }
        if ((!d2380) && u_is_con(s2379, &n_option_46_None)) {
          d2380 = 1;
          {
          }
        }
        if (!d2380) {
          u_no_case(s2379);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2382 = U_LIT("!");
        u_value t2383 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2382, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2383;
      }
    }
    if (!d2375) {
      u_no_case(s2374);
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
    u_value s2384 = u_mk_tuple(2, v_xs, v_lists);
    int d2385 = 0;
    if ((!d2385) && (u_is_con(u_field(s2384, 0), &n_list_46_Cons) && u_is_con(u_field(s2384, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2384, 0), 0);
      u_value v_more = u_field(u_field(s2384, 0), 1);
      u_value v_list = u_field(u_field(s2384, 1), 0);
      u_value v_others = u_field(u_field(s2384, 1), 1);
      d2385 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2386 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2386, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2387 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2387;
        static struct u_object l_u_is_con_2388 = U_LIT("u_is_con");
        u_value t2389 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2388, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2389;
      }
    }
    if (!d2385) {
      d2385 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2390 = U_LIT("u_append");
        u_value t2391 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2390, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2391;
      }
    }
    if (!d2385) {
      u_no_case(s2384);
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
    u_value s2392 = v_s;
    int d2393 = 0;
    if ((!d2393) && u_is_con(s2392, &n_syntax_46_SSkip)) {
      d2393 = 1;
      {
        u_value t2394 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2394;
      }
    }
    if ((!d2393) && u_is_con(s2392, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2392, 0);
      d2393 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2395 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2395;
      }
    }
    if ((!d2393) && u_is_con(s2392, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2392, 0);
      u_value v_body = u_field(s2392, 1);
      d2393 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t2396 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2397 = &t2396;
        u_value l2398 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2398, &n_list_46_Cons)) {
          u_value v_x = u_field(l2398, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2399 = U_LIT("NULL");
            e2397 = u_append(e2397, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2399))));
          }
          l2398 = u_field(l2398, 1);
        }
        u_value t2400 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2396, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2400;
      }
    }
    if ((!d2393) && u_is_con(s2392, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2392, 0);
      u_value v_l = u_field(s2392, 1);
      u_value v_r = u_field(s2392, 2);
      d2393 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2401 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2401;
      }
    }
    if ((!d2393) && u_is_con(s2392, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2392, 0);
      u_value v_b = u_field(s2392, 1);
      d2393 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2402 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2402, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2403 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2403;
      }
    }
    if ((!d2393) && u_is_con(s2392, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2392, 0);
      u_value v_ins = u_field(s2392, 1);
      u_value v_outs = u_field(s2392, 2);
      u_value v_refs = u_field(s2392, 3);
      d2393 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        if (u_truth(f_list_46_has_key_2(v_p, v_env))) {
          {
            static struct u_object l_u_call_proc_2404 = U_LIT("u_call_proc");
            u_value t2405 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e2406 = &t2405;
            u_value l2407 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
            while (u_is_con(l2407, &n_list_46_Cons)) {
              u_value v_x = u_field(l2407, 0);
              if (u_truth(u_mk_bool(1))) {
                e2406 = u_append(e2406, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
              }
              l2407 = u_field(l2407, 1);
            }
            u_value t2408 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_call_proc_2404, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_var_expr_2(v_env, v_p), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_ins)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(f_list_46__43__43__2(v_outs, v_refs))), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(v_rs, t2405)))), u_mk_con(&n_list_46_Nil, 0)));
            *r_ss = t2408;
          }
        } else {
          {
            u_value t2409 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e2410 = &t2409;
            u_value l2411 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
            while (u_is_con(l2411, &n_list_46_Cons)) {
              u_value v_x = u_field(l2411, 0);
              if (u_truth(u_mk_bool(1))) {
                e2410 = u_append(e2410, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
              }
              l2411 = u_field(l2411, 1);
            }
            u_value t2412 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2409))), u_mk_con(&n_list_46_Nil, 0)));
            *r_ss = t2412;
          }
        }
      }
    }
    if ((!d2393) && u_is_con(s2392, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2392, 0);
      u_value v_cases = u_field(s2392, 1);
      d2393 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2413 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2413, &v_d, r_g);
        static struct u_object l_d_2414 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2414, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2415 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2415;
        static struct u_object l__33__2416 = U_LIT("!");
        static struct u_object l_u_no_case_2417 = U_LIT("u_no_case");
        u_value t2418 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2416, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2417, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2418;
      }
    }
    if ((!d2393) && (u_is_con(s2392, &n_syntax_46_SAssert) && (u_is_con(u_field(s2392, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2392, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2392, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2392, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2392, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2392, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2392, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2392, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2392, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2392, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2392, 0), 1), 1), 0);
      d2393 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2419 = U_LIT("u_assert_equal");
        u_value t2420 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2419, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2420;
      }
    }
    if ((!d2393) && u_is_con(s2392, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2392, 0);
      d2393 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2421 = U_LIT("u_assert_true");
        u_value t2422 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2421, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2422;
      }
    }
    if ((!d2393) && u_is_con(s2392, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2392, 0);
      u_value v_ty = u_field(s2392, 1);
      d2393 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2423 = U_LIT("u_raise");
        u_value t2424 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2423, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2424;
      }
    }
    if ((!d2393) && u_is_con(s2392, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2392, 0);
      u_value v_hs = u_field(s2392, 1);
      d2393 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2425 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2425, &v_d, r_g);
        static struct u_object l_d_2426 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2426, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2427 = U_LIT("u_pop");
        static struct u_object l__33__2428 = U_LIT("!");
        static struct u_object l_u_reraise_2429 = U_LIT("u_reraise");
        u_value t2430 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2427, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2428, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2429, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2430;
        static struct u_object l_struct_32_u_handler_2431 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2432 = U_LIT("u_push");
        static struct u_object l__61__61__2433 = U_LIT("==");
        static struct u_object l_setjmp_2434 = U_LIT("setjmp");
        static struct u_object l_env_2435 = U_LIT("env");
        static struct u_object l_u_pop_2436 = U_LIT("u_pop");
        u_value t2437 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2431)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2432, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2433, f_c_46_c_call_2(&l_setjmp_2434, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2435), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2436, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2437;
      }
    }
    if (!d2393) {
      u_no_case(s2392);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2438 = v_xs;
    int d2439 = 0;
    if ((!d2439) && u_is_con(s2438, &n_list_46_Nil)) {
      d2439 = 1;
      {
        u_value t2440 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2440;
      }
    }
    if ((!d2439) && u_is_con(s2438, &n_list_46_Cons)) {
      u_value v_x = u_field(s2438, 0);
      u_value v_rest = u_field(s2438, 1);
      d2439 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2441 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2441;
      }
    }
    if (!d2439) {
      u_no_case(s2438);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2442 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2443 = NULL;
  if ((!t2443) && (u_is_con(u_field(s2442, 0), &n_list_46_Cons) && (u_is_con(u_field(s2442, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2442, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2442, 0), 0);
    u_value v_more = u_field(u_field(s2442, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2442, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2442, 1), 1);
    t2443 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2443) {
    t2443 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2443) {
    u_no_case(s2442);
  }
  return t2443;
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
    static struct u_object l_u_caught_2444 = U_LIT("u_caught");
    u_value t2445 = f_c_46_c_call_2(&l_u_caught_2444, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2445;
    u_value s2446 = v_hs;
    int d2447 = 0;
    if ((!d2447) && u_is_con(s2446, &n_list_46_Nil)) {
      d2447 = 1;
      {
        u_value t2448 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2448;
      }
    }
    if ((!d2447) && (u_is_con(s2446, &n_list_46_Cons) && u_is_con(u_field(s2446, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2446, 0), 0);
      u_value v_p = u_field(u_field(s2446, 0), 1);
      u_value v_guard = u_field(u_field(s2446, 0), 2);
      u_value v_s = u_field(u_field(s2446, 0), 3);
      u_value v_more = u_field(s2446, 1);
      d2447 = 1;
      {
        u_value t2449 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2449;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2450 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2450;
        u_value s2451 = v_guard;
        int d2452 = 0;
        if ((!d2452) && u_is_con(s2451, &n_option_46_Some)) {
          u_value v_c = u_field(s2451, 0);
          d2452 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2453 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2453;
          }
        }
        if ((!d2452) && u_is_con(s2451, &n_option_46_None)) {
          d2452 = 1;
          {
          }
        }
        if (!d2452) {
          u_no_case(s2451);
        }
        u_value s2454 = v_p;
        int d2455 = 0;
        if ((!d2455) && u_is_con(s2454, &n_syntax_46_PWild)) {
          d2455 = 1;
          {
            u_value t2456 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2456;
          }
        }
        if (!d2455) {
          d2455 = 1;
          {
            static struct u_object l_u_has_type_2457 = U_LIT("u_has_type");
            u_value t2458 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2457, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2458;
          }
        }
        if (!d2455) {
          u_no_case(s2454);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2459 = U_LIT("!");
        u_value t2460 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2459, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2460;
      }
    }
    if (!d2447) {
      u_no_case(s2446);
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
    u_value s2461 = v_cases;
    int d2462 = 0;
    if ((!d2462) && u_is_con(s2461, &n_list_46_Nil)) {
      d2462 = 1;
      {
        u_value t2463 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2463;
      }
    }
    if ((!d2462) && (u_is_con(s2461, &n_list_46_Cons) && u_is_con(u_field(s2461, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2461, 0), 0);
      u_value v_guard = u_field(u_field(s2461, 0), 1);
      u_value v_s = u_field(u_field(s2461, 0), 2);
      u_value v_more = u_field(s2461, 1);
      d2462 = 1;
      {
        u_value t2464 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2464;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2465 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2465;
        u_value s2466 = v_guard;
        int d2467 = 0;
        if ((!d2467) && u_is_con(s2466, &n_option_46_Some)) {
          u_value v_c = u_field(s2466, 0);
          d2467 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2468 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2468;
          }
        }
        if ((!d2467) && u_is_con(s2466, &n_option_46_None)) {
          d2467 = 1;
          {
          }
        }
        if (!d2467) {
          u_no_case(s2466);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2469 = U_LIT("!");
        u_value t2470 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2469, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2470;
      }
    }
    if (!d2462) {
      u_no_case(s2461);
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
    u_value s2471 = v_c;
    int d2472 = 0;
    if ((!d2472) && u_is_con(s2471, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2471, 0);
      u_value v_xss = u_field(s2471, 2);
      u_value v_b = u_field(s2471, 4);
      d2472 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2473 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2474 = &t2473;
        u_value l2475 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2475, &n_list_46_Cons)) {
          u_value v_x = u_field(l2475, 0);
          if (u_truth(u_mk_bool(1))) {
            e2474 = u_append(e2474, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2475 = u_field(l2475, 1);
        }
        u_value t2476 = t2473;
        v_params = t2476;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2477 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2477;
          }
        } else {
          {
            u_value t2478 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2478;
          }
        }
        u_value t2479 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2480 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2479;
        *r_ms = t2480;
      }
    }
    if ((!d2472) && u_is_con(s2471, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2471, 0);
      u_value v_ins = u_field(s2471, 2);
      u_value v_outs = u_field(s2471, 3);
      u_value v_refs = u_field(s2471, 4);
      u_value v_body = u_field(s2471, 5);
      d2472 = 1;
      {
        u_value t2481 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2482 = &t2481;
        u_value l2483 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2483, &n_list_46_Cons)) {
          u_value v_x = u_field(l2483, 0);
          if (u_truth(u_mk_bool(1))) {
            e2482 = u_append(e2482, f_compile_46_pointer_1(v_x));
          }
          l2483 = u_field(l2483, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2481), v_body, &v_bs, r_g);
        u_value t2484 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2485 = &t2484;
        u_value l2486 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2486, &n_list_46_Cons)) {
          u_value v_x = u_field(l2486, 0);
          if (u_truth(u_mk_bool(1))) {
            e2485 = u_append(e2485, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2486 = u_field(l2486, 1);
        }
        u_value t2487 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2488 = &t2487;
        u_value l2489 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2489, &n_list_46_Cons)) {
          u_value v_x = u_field(l2489, 0);
          if (u_truth(u_mk_bool(1))) {
            e2488 = u_append(e2488, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2489 = u_field(l2489, 1);
        }
        u_value t2490 = f_list_46__43__43__2(t2484, t2487);
        v_params = t2490;
        u_value t2491 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2492 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2491;
        *r_ms = t2492;
      }
    }
    if ((!d2472) && u_is_con(s2471, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s2471, 0);
      d2472 = 1;
      {
        p_compile_46_compile_expr(v_gl, u_mk_con(&n_list_46_Nil, 0), v_e, &v_bs, &v_rb, r_g);
        static struct u_object l_u_print_line_2493 = U_LIT("u_print_line");
        u_value t2494 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2495 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_line_2493, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2494;
        *r_ms = t2495;
      }
    }
    if ((!d2472) && (u_is_con(s2471, &n_syntax_46_CRun) && u_is_con(u_field(s2471, 1), &n_syntax_46_SBlock))) {
      u_value v_xs = u_field(u_field(s2471, 1), 0);
      u_value v_body = u_field(u_field(s2471, 1), 1);
      d2472 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_body, &v_bs, r_g);
        u_value t2496 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2497 = &t2496;
        u_value l2498 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2498, &n_list_46_Cons)) {
          u_value v_x = u_field(l2498, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2499 = U_LIT("NULL");
            e2497 = u_append(e2497, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2499))));
          }
          l2498 = u_field(l2498, 1);
        }
        u_value t2500 = t2496;
        v_copy = t2500;
        u_value t2501 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2502 = &t2501;
        u_value l2503 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2503, &n_list_46_Cons)) {
          u_value v_x = u_field(l2503, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_u_print_binding_2504 = U_LIT("u_print_binding");
            e2502 = u_append(e2502, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_binding_2504, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_syntax_46_Name_46_base_1(v_x)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x)), u_mk_con(&n_list_46_Nil, 0))))));
          }
          l2503 = u_field(l2503, 1);
        }
        u_value t2505 = t2501;
        v_back = t2505;
        u_value t2506 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2507 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_copy, f_list_46__43__43__2(v_bs, v_back))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2506;
        *r_ms = t2507;
      }
    }
    if ((!d2472) && u_is_con(s2471, &n_syntax_46_CRun)) {
      u_value v_body = u_field(s2471, 1);
      d2472 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2508 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2509 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2508;
        *r_ms = t2509;
      }
    }
    if ((!d2472) && u_is_con(s2471, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s2471, 1);
      d2472 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2510 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2511 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2510;
        *r_ms = t2511;
      }
    }
    if (!d2472) {
      d2472 = 1;
      {
        u_value t2512 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2513 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2512;
        *r_ms = t2513;
      }
    }
    if (!d2472) {
      u_no_case(s2471);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2514 = v_cs;
    int d2515 = 0;
    if ((!d2515) && u_is_con(s2514, &n_list_46_Nil)) {
      d2515 = 1;
      {
        u_value t2516 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2517 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2516;
        *r_ms = t2517;
      }
    }
    if ((!d2515) && u_is_con(s2514, &n_list_46_Cons)) {
      u_value v_c = u_field(s2514, 0);
      u_value v_rest = u_field(s2514, 1);
      d2515 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_rest, &v_d2, &v_m2, r_g);
        u_value t2518 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2519 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2518;
        *r_ms = t2519;
      }
    }
    if (!d2515) {
      u_no_case(s2514);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_handler_2520 = U_LIT("handler");
  u_value t2547 = u_mk_con(&n_c_46_CeVar, 1, &l_handler_2520);
  u_value t2546;
  {
    u_value v_h = t2547;
    static struct u_object l_static_32_u_value_2521 = U_LIT("static u_value");
    static struct u_object l_cache_2522 = U_LIT("cache");
    static struct u_object l_NULL_2523 = U_LIT("NULL");
    static struct u_object l__61__61__2524 = U_LIT("==");
    static struct u_object l_cache_2525 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2526 = U_LIT("U_COMPUTING");
    static struct u_object l_u_fail_2527 = U_LIT("u_fail");
    static struct u_object l_recursive_32_initia_2528 = U_LIT("recursive initialization of ");
    static struct u_object l_cache_2529 = U_LIT("cache");
    static struct u_object l_cache_2530 = U_LIT("cache");
    static struct u_object l_cache_2531 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2532 = U_LIT("U_COMPUTING");
    static struct u_object l_struct_32_u_handler_2533 = U_LIT("struct u_handler");
    static struct u_object l_handler_2534 = U_LIT("handler");
    static struct u_object l_u_push_2535 = U_LIT("u_push");
    static struct u_object l__33__61__2536 = U_LIT("!=");
    static struct u_object l_setjmp_2537 = U_LIT("setjmp");
    static struct u_object l_env_2538 = U_LIT("env");
    static struct u_object l_u_pop_2539 = U_LIT("u_pop");
    static struct u_object l_cache_2540 = U_LIT("cache");
    static struct u_object l_NULL_2541 = U_LIT("NULL");
    static struct u_object l_u_reraise_2542 = U_LIT("u_reraise");
    static struct u_object l_u_pop_2543 = U_LIT("u_pop");
    static struct u_object l_cache_2544 = U_LIT("cache");
    static struct u_object l_cache_2545 = U_LIT("cache");
    t2546 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2521), &l_cache_2522, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2523))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2524, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2525), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2526)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2527, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2528, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2529), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2530))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2531), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2532)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2533)), &l_handler_2534, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2535, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__33__61__2536, f_c_46_c_call_2(&l_setjmp_2537, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, v_h, &l_env_2538), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2539, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2540), u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2541)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2542, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2543, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2544), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2545))), u_mk_con(&n_list_46_Nil, 0))))));
  }
  return t2546;
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2548 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2549 = &t2548;
  u_value l2550 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l2550, &n_list_46_Cons)) {
    u_value v_p = u_field(l2550, 0);
    if (u_truth(u_mk_bool(1))) {
      e2549 = u_append(e2549, f_compile_46_fun_wrapper_1(v_p));
    }
    l2550 = u_field(l2550, 1);
  }
  u_value t2551 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2552 = &t2551;
  u_value l2553 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l2553, &n_list_46_Cons)) {
    u_value v_p = u_field(l2553, 0);
    if (u_truth(u_mk_bool(1))) {
      e2552 = u_append(e2552, f_compile_46_constr_wrapper_1(v_p));
    }
    l2553 = u_field(l2553, 1);
  }
  u_value t2554 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2555 = &t2554;
  u_value l2556 = f_list_46_seconds_1(f_map_46_entries_1(f_compile_46_builtin_functions_0()));
  while (u_is_con(l2556, &n_list_46_Cons)) {
    u_value v_p = u_field(l2556, 0);
    if (u_truth(u_mk_bool(1))) {
      e2555 = u_append(e2555, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2556 = u_field(l2556, 1);
  }
  u_value t2557 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2558 = &t2557;
  u_value l2559 = f_map_46_entries_1(f_compile_46_Globals_46_procs_1(v_gl));
  while (u_is_con(l2559, &n_list_46_Cons)) {
    u_value v_p = u_field(l2559, 0);
    if (u_truth(u_mk_bool(1))) {
      e2558 = u_append(e2558, f_compile_46_proc_wrapper_1(v_p));
    }
    l2559 = u_field(l2559, 1);
  }
  return f_list_46__43__43__2(t2548, f_list_46__43__43__2(f_list_46_concat_1(t2551), f_list_46__43__43__2(t2554, t2557)));
}

u_value f_compile_46_proc_wrapper_1(u_value v_p) {
  u_value s2560 = v_p;
  u_value t2561 = NULL;
  if (!t2561) {
    u_value v_n = u_field(s2560, 0);
    u_value v_k = u_field(u_field(s2560, 1), 0);
    u_value v_m = u_field(u_field(s2560, 1), 1);
    static struct u_object l_wp__2562 = U_LIT("wp_");
    u_value t2563 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2564 = &t2563;
    u_value l2565 = f_compile_46_range_to_1(v_k);
    while (u_is_con(l2565, &n_list_46_Cons)) {
      u_value v_i = u_field(l2565, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_ins_2566 = U_LIT("ins");
        e2564 = u_append(e2564, f_compile_46_index_2(&l_ins_2566, v_i));
      }
      l2565 = u_field(l2565, 1);
    }
    u_value t2567 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2568 = &t2567;
    u_value l2569 = f_compile_46_range_to_1(v_m);
    while (u_is_con(l2569, &n_list_46_Cons)) {
      u_value v_i = u_field(l2569, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_outs_2570 = U_LIT("outs");
        e2568 = u_append(e2568, f_compile_46_index_2(&l_outs_2570, v_i));
      }
      l2569 = u_field(l2569, 1);
    }
    t2561 = u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), u_concat(&l_wp__2562, f_compile_46_mangle_1(v_n)), f_compile_46_proc_params_0(), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_n), f_list_46__43__43__2(t2563, t2567))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2561) {
    u_no_case(s2560);
  }
  return t2561;
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2571 = v_p;
  u_value t2572 = NULL;
  if (!t2572) {
    u_value v_n = u_field(s2571, 0);
    u_value v_k = u_field(s2571, 1);
    static struct u_object l_w__2573 = U_LIT("w_");
    t2572 = f_compile_46_wrapper_3(u_concat(&l_w__2573, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2572) {
    u_no_case(s2571);
  }
  return t2572;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2574 = v_p;
  u_value t2575 = NULL;
  if (!t2575) {
    u_value v_f = u_field(s2574, 0);
    u_value v_k = u_field(s2574, 1);
    static struct u_object l_w_2576 = U_LIT("w");
    t2575 = f_compile_46_wrapper_3(u_concat(&l_w_2576, v_f), v_f, v_k);
  }
  if (!t2575) {
    u_no_case(s2574);
  }
  return t2575;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2577 = U_LIT("env");
  static struct u_object l_args_2578 = U_LIT("args");
  u_value t2579 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2580 = &t2579;
  u_value l2581 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2581, &n_list_46_Cons)) {
    u_value v_i = u_field(l2581, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2582 = U_LIT("args");
      e2580 = u_append(e2580, f_compile_46_index_2(&l_args_2582, v_i));
    }
    l2581 = u_field(l2581, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2577), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2578), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2579))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2583 = v_p;
  u_value t2584 = NULL;
  if ((!t2584) && u_equals(u_field(s2583, 1), u_mk_int(0))) {
    t2584 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2584) {
    u_value v_n = u_field(s2583, 0);
    u_value v_k = u_field(s2583, 1);
    static struct u_object l_wc__2585 = U_LIT("wc_");
    static struct u_object l_env_2586 = U_LIT("env");
    static struct u_object l_args_2587 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2588 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2589 = U_LIT("args");
    t2584 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2585, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2586), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2587), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2588, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2589), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2584) {
    u_no_case(s2583);
  }
  return t2584;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2590;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2590 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2590 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2590;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2591 = v_d;
  u_value t2592 = NULL;
  if ((!t2592) && u_is_con(s2591, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2591, 0);
    u_value v_f = u_field(s2591, 1);
    u_value v_ps = u_field(s2591, 2);
    t2592 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2592) {
    t2592 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2592) {
    u_no_case(s2591);
  }
  return t2592;
}

void p_compile_46_compile_program(u_value v_q, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2593 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2593;
    u_value t2594 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t2594;
    p_compile_46_compile_cmds(v_gl, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value t2595 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, f_compile_46_Gen_46_lifted_1(v_g)));
    v_fs = t2595;
    static struct u_object l_ulang_46_h_2596 = U_LIT("ulang.h");
    u_value t2597 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2598 = &t2597;
    u_value l2599 = v_fs;
    while (u_is_con(l2599, &n_list_46_Cons)) {
      u_value v_d = u_field(l2599, 0);
      if (u_truth(u_mk_bool(1))) {
        e2598 = u_append(e2598, f_compile_46_prototype_1(v_d));
      }
      l2599 = u_field(l2599, 1);
    }
    static struct u_object l_main_2600 = U_LIT("main");
    static struct u_object l_argc_2601 = U_LIT("argc");
    static struct u_object l_char_2602 = U_LIT("char");
    static struct u_object l_argv_2603 = U_LIT("argv");
    static struct u_object l_u_args_2604 = U_LIT("u_args");
    static struct u_object l_argc_2605 = U_LIT("argc");
    static struct u_object l_argv_2606 = U_LIT("argv");
    u_value t2607 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2596), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2597), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2600, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2601), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2602))), &l_argv_2603), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2604, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2605), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2606), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2607;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_target) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2608 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2608, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2609 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2609, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q);
    p_compile_46_compile_program(v_q, &v_f);
    p_os_write(v_target, f_c_46_c_file_1(v_f));
  }
}

u_value lam356(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam357(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam359(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam360(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam370(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_f = args[0];
  return f_ctx_46_is_fun_2(f_check_46_State_46_ctx_1(v_s), v_f);
}

u_value lam377(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam381(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_p = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_p);
}

u_value lam614(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_t = args[0];
  return f_ctx_46_is_sort_2(f_check_46_State_46_ctx_1(v_s), v_t);
}

u_value lam725(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam728(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam793(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_q = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_q);
}

u_value lam1070(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  {
    u_value v_args = NULL;
    p_os_args(&v_args);
    u_value s2610 = v_args;
    int d2611 = 0;
    if ((!d2611) && u_is_con(s2610, &n_list_46_Nil)) {
      d2611 = 1;
      {
      }
    }
    if ((!d2611) && (u_is_con(s2610, &n_list_46_Cons) && (u_is_con(u_field(s2610, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2610, 1), 1), &n_list_46_Nil)))) {
      u_value v_source = u_field(s2610, 0);
      u_value v_target = u_field(u_field(s2610, 1), 0);
      d2611 = 1;
      {
        p_compile_46_compile_file(v_source, v_target);
      }
    }
    if (!d2611) {
      d2611 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_sou_2612 = U_LIT("usage: ulang source.u target.c");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_sou_2612), "os.OsError");
      }
    }
    if (!d2611) {
      u_no_case(s2610);
    }
    u_print_binding("args", v_args);
  }
  return 0;
}
