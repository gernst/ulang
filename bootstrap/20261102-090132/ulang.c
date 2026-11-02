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
        p_parse_46_sections(u_mk_proc(wp_parse_46_parse_type, 0), u_mk_proc(wp_parse_46_parse_type, 0), v_sc, &v_ins, &v_outs, &v_refs, r_ts);
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

void p_parse_46_type_list(u_value v_sc, u_value v_close, u_value *r_r, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_t = NULL;
    u_value t1305 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1305;
    u_value t1306 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1306)) {
      t1306 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1306)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1307 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1307;
        u_value t1309 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1309)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1308 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1308;
          }
          t1309 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1310 = f_list_46_reverse_1(v_rev);
    *r_r = t1310;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_prefix_2(u_value v_op, u_value v_a) {
  u_value s1311 = v_a;
  u_value t1312 = NULL;
  if ((!t1312) && (u_is_con(s1311, &n_syntax_46_ELit) && u_is_con(u_field(s1311, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1311, 0), 0);
    static struct u_object l__45__1313 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1313))) {
      t1312 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1312) {
    t1312 = f_parse_46_expr_op1_2(v_op, v_a);
  }
  if (!t1312) {
    u_no_case(s1311);
  }
  return t1312;
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1314 = v_es;
  u_value t1315 = NULL;
  if ((!t1315) && u_is_con(s1314, &n_list_46_Nil)) {
    t1315 = v_rest;
  }
  if ((!t1315) && u_is_con(s1314, &n_list_46_Cons)) {
    u_value v_e = u_field(s1314, 0);
    u_value v_more = u_field(s1314, 1);
    static struct u_object l_Cons_1316 = U_LIT("Cons");
    t1315 = f_parse_46_expr_op2_3(&l_Cons_1316, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1315) {
    u_no_case(s1314);
  }
  return t1315;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1317 = v_t;
  u_value t1318 = NULL;
  if ((!t1318) && u_is_con(s1317, &n_lex_46_TName)) {
    u_value v_n = u_field(s1317, 0);
    static struct u_object l_let_1319 = U_LIT("let");
    static struct u_object l_if_1320 = U_LIT("if");
    static struct u_object l_lambda_1321 = U_LIT("lambda");
    static struct u_object l_match_1322 = U_LIT("match");
    static struct u_object l_true_1323 = U_LIT("true");
    static struct u_object l_false_1324 = U_LIT("false");
    u_value t1326 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1319, u_mk_con(&n_list_46_Cons, 2, &l_if_1320, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1321, u_mk_con(&n_list_46_Cons, 2, &l_match_1322, u_mk_con(&n_list_46_Cons, 2, &l_true_1323, u_mk_con(&n_list_46_Cons, 2, &l_false_1324, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1326)) {
      u_value t1325 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1325)) {
        t1325 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1326 = t1325;
    }
    t1318 = t1326;
  }
  if ((!t1318) && u_is_con(s1317, &n_lex_46_TNum)) {
    t1318 = u_mk_bool(1);
  }
  if ((!t1318) && u_is_con(s1317, &n_lex_46_TStr)) {
    t1318 = u_mk_bool(1);
  }
  if ((!t1318) && u_is_con(s1317, &n_lex_46_TChar)) {
    t1318 = u_mk_bool(1);
  }
  if ((!t1318) && u_is_con(s1317, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1317, 0);
    u_value t1327 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1327)) {
      t1327 = u_eq(v_c, u_mk_char(91));
    }
    u_value t1328 = t1327;
    if (!u_truth(t1328)) {
      t1328 = u_eq(v_c, u_mk_char(123));
    }
    t1318 = t1328;
  }
  if (!t1318) {
    t1318 = u_mk_bool(0);
  }
  if (!t1318) {
    u_no_case(s1317);
  }
  return t1318;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    static struct u_object l_raise_1329 = U_LIT("raise");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_raise_1329))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, r_e, r_ts);
        u_value t1330 = u_mk_con(&n_syntax_46_ERaise, 2, *r_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_e = t1330;
      }
    } else {
      {
        p_parse_46_operators(v_sc, r_e, r_ts);
        static struct u_object l_with_1331 = U_LIT("with");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_with_1331))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_update(v_sc, r_e, r_ts);
            u_value t1332 = f_parse_46_at_update_1(*r_ts);
            while (u_truth(t1332)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_update(v_sc, r_e, r_ts);
              }
              t1332 = f_parse_46_at_update_1(*r_ts);
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
    static struct u_object l__58__61__1333 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1333), r_ts);
    p_parse_46_operators(v_sc, &v_a, r_ts);
    static struct u_object l__46__1334 = U_LIT(".");
    static struct u_object l__58__61__1335 = U_LIT(":=");
    u_value t1336 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1334, u_concat(v_f, &l__58__61__1335))), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1336;
  }
}

u_value f_parse_46_at_update_1(u_value v_ts) {
  u_value s1337 = v_ts;
  u_value t1338 = NULL;
  if ((!t1338) && (u_is_con(s1337, &n_list_46_Cons) && (u_is_con(u_field(u_field(s1337, 0), 0), &n_lex_46_TPunct) && (u_equals(u_field(u_field(u_field(s1337, 0), 0), 0), u_mk_char(44)) && (u_is_con(u_field(s1337, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s1337, 1), 0), 0), &n_lex_46_TName) && (u_is_con(u_field(u_field(s1337, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(u_field(s1337, 1), 1), 0), 0), &n_lex_46_TSym) && u_equals(u_field(u_field(u_field(u_field(u_field(s1337, 1), 1), 0), 0), 0), u_mk_lit(":=")))))))))) {
    t1338 = u_mk_bool(1);
  }
  if (!t1338) {
    t1338 = u_mk_bool(0);
  }
  if (!t1338) {
    u_no_case(s1337);
  }
  return t1338;
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
    u_value t1339 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1339;
    u_value t1340 = u_mk_bool(1);
    v_next = t1340;
    u_value t1341 = u_mk_bool(1);
    v_go = t1341;
    u_value t1350 = v_go;
    while (u_truth(t1350)) {
      {
        u_value s1342 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1343 = 0;
        if ((!d1343) && u_is_con(s1342, &n_option_46_Some)) {
          u_value v_op = u_field(s1342, 0);
          d1343 = 1;
          {
            u_value t1344 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1345 = u_mk_bool(1);
            v_rev = t1344;
            v_next = t1345;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1343) && u_is_con(s1342, &n_option_46_None)) {
          u_value t1348 = v_next;
          if (u_truth(t1348)) {
            t1348 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1348)) {
            d1343 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1346 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1347 = u_mk_bool(0);
              v_rev = t1346;
              v_next = t1347;
            }
          }
        }
        if (!d1343) {
          d1343 = 1;
          {
            u_value t1349 = u_mk_bool(0);
            v_go = t1349;
          }
        }
        if (!d1343) {
          u_no_case(s1342);
        }
      }
      t1350 = v_go;
    }
    u_value s1351 = v_rev;
    int d1352 = 0;
    if ((!d1352) && u_is_con(s1351, &n_list_46_Nil)) {
      d1352 = 1;
      {
        static struct u_object l_expression_32_expec_1353 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1354 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1353, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1354)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1352) {
      d1352 = 1;
      {
        u_value t1355 = f_list_46_reverse_1(v_rev);
        *r_items = t1355;
      }
    }
    if (!d1352) {
      u_no_case(s1351);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1356 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1356;
    u_value t1358 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1358)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1357 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1357;
      }
      t1358 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1359 = f_list_46_reverse_1(v_rev);
    *r_es = t1359;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1360 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1360;
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
    u_value t1361 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1361)) {
      t1361 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1365 = t1361;
    while (u_truth(t1365)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1362 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1362;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1363 = U_LIT(".");
            u_value t1364 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1363, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1364;
          }
        }
      }
      u_value t1366 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1366)) {
        t1366 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1365 = t1366;
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
    u_value s1367 = f_parse_46_head_token_1(*r_ts);
    int d1368 = 0;
    if ((!d1368) && (u_is_con(s1367, &n_lex_46_TName) && u_equals(u_field(s1367, 0), u_mk_lit("let")))) {
      d1368 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1368) && (u_is_con(s1367, &n_lex_46_TName) && u_equals(u_field(s1367, 0), u_mk_lit("if")))) {
      d1368 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1369 = U_LIT("then");
        p_parse_46_keyword(&l_then_1369, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1370 = U_LIT("else");
        p_parse_46_keyword(&l_else_1370, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1371 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1371;
      }
    }
    if ((!d1368) && (u_is_con(s1367, &n_lex_46_TName) && u_equals(u_field(s1367, 0), u_mk_lit("lambda")))) {
      d1368 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1368) && (u_is_con(s1367, &n_lex_46_TName) && u_equals(u_field(s1367, 0), u_mk_lit("match")))) {
      d1368 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1372 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1372;
      }
    }
    if ((!d1368) && (u_is_con(s1367, &n_lex_46_TName) && u_equals(u_field(s1367, 0), u_mk_lit("true")))) {
      d1368 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1373 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1373;
      }
    }
    if ((!d1368) && (u_is_con(s1367, &n_lex_46_TName) && u_equals(u_field(s1367, 0), u_mk_lit("false")))) {
      d1368 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1374 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1374;
      }
    }
    if ((!d1368) && u_is_con(s1367, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1367, 0);
      d1368 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1375 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1375;
      }
    }
    if ((!d1368) && u_is_con(s1367, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1367, 0);
      d1368 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1376 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1376;
      }
    }
    if ((!d1368) && u_is_con(s1367, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1367, 0);
      d1368 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1377 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1377;
      }
    }
    if ((!d1368) && (u_is_con(s1367, &n_lex_46_TPunct) && u_equals(u_field(s1367, 0), u_mk_char(40)))) {
      d1368 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1378 = v_es;
        int d1379 = 0;
        if ((!d1379) && (u_is_con(s1378, &n_list_46_Cons) && u_is_con(u_field(s1378, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1378, 0);
          d1379 = 1;
          {
            u_value t1380 = v_x;
            *r_e = t1380;
          }
        }
        if (!d1379) {
          d1379 = 1;
          {
            u_value t1381 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1381;
          }
        }
        if (!d1379) {
          u_no_case(s1378);
        }
      }
    }
    if ((!d1368) && (u_is_con(s1367, &n_lex_46_TPunct) && u_equals(u_field(s1367, 0), u_mk_char(91)))) {
      d1368 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1368) && (u_is_con(s1367, &n_lex_46_TPunct) && u_equals(u_field(s1367, 0), u_mk_char(123)))) {
      d1368 = 1;
      {
        p_parse_46_parse_braces(v_sc, r_e, r_ts);
      }
    }
    if ((!d1368) && u_is_con(s1367, &n_lex_46_TName)) {
      u_value v_n = u_field(s1367, 0);
      u_value t1383 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1383)) {
        t1383 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t1383)) {
        d1368 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1382 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1382;
        }
      }
    }
    if (!d1368) {
      d1368 = 1;
      {
        static struct u_object l_expression_32_expec_1384 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1385 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1384, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1385)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1368) {
      u_no_case(s1367);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1386 = v_ps;
  u_value t1387 = NULL;
  if ((!t1387) && u_is_con(s1386, &n_list_46_Nil)) {
    t1387 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1387) && (u_is_con(s1386, &n_list_46_Cons) && u_is_con(u_field(s1386, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1386, 0), 0);
    u_value v_more = u_field(s1386, 1);
    u_value s1388 = f_parse_46_pattern_vars_1(v_more);
    u_value t1389 = NULL;
    if ((!t1389) && u_is_con(s1388, &n_option_46_Some)) {
      u_value v_xs = u_field(s1388, 0);
      t1389 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1389) && u_is_con(s1388, &n_option_46_None)) {
      t1389 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1389) {
      u_no_case(s1388);
    }
    t1387 = t1389;
  }
  if (!t1387) {
    t1387 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1387) {
    u_no_case(s1386);
  }
  return t1387;
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
    static struct u_object l__58__61__1390 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1390), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1391 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1392 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1391;
    v_es = t1392;
    u_value t1396 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1396)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1393 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1393), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1394 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1395 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1394;
        v_es = t1395;
      }
      t1396 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1397 = U_LIT("in");
    p_parse_46_keyword(&l_in_1397, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1398 = f_list_46_reverse_1(v_ps);
    u_value t1399 = f_list_46_reverse_1(v_es);
    v_ps = t1398;
    v_es = t1399;
    u_value s1400 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1401 = 0;
    if ((!d1401) && u_is_con(u_field(s1400, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1400, 0), 0);
      d1401 = 1;
      {
        u_value t1402 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1402;
      }
    }
    if ((!d1401) && (u_is_con(u_field(s1400, 0), &n_option_46_None) && (u_is_con(u_field(s1400, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1400, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1400, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1400, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1400, 1), 0);
      u_value v_c = u_field(u_field(s1400, 2), 0);
      d1401 = 1;
      {
        u_value t1403 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1403;
      }
    }
    if (!d1401) {
      d1401 = 1;
      {
        u_value t1404 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1404;
      }
    }
    if (!d1401) {
      u_no_case(s1400);
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
        static struct u_object l__61__62__1405 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1405), r_ts);
        p_parse_46_parse_body(v_sc, &v_body, r_ts);
        static struct u_object l_end_1406 = U_LIT("end");
        p_parse_46_keyword(&l_end_1406, r_ts);
        u_value t1407 = u_mk_con(&n_syntax_46_EProc, 4, v_ins, v_outs, v_refs, v_body);
        *r_e = t1407;
      }
    } else {
      {
        p_parse_46_lambda_function(v_sc, r_e, r_ts);
      }
    }
  }
}

u_value f_parse_46_at_section_1(u_value v_ts) {
  static struct u_object l_in_1408 = U_LIT("in");
  u_value t1410 = f_parse_46_at_keyword_2(v_ts, &l_in_1408);
  if (!u_truth(t1410)) {
    static struct u_object l_out_1409 = U_LIT("out");
    t1410 = f_parse_46_at_keyword_2(v_ts, &l_out_1409);
  }
  u_value t1412 = t1410;
  if (!u_truth(t1412)) {
    static struct u_object l_ref_1411 = U_LIT("ref");
    t1412 = f_parse_46_at_keyword_2(v_ts, &l_ref_1411);
  }
  return t1412;
}

void p_parse_46_lambda_function(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    u_value t1413 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1413;
    static struct u_object l__61__62__1414 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1414)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1415 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1415;
        u_value t1417 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1417)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1416 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1416;
          }
          t1417 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1418 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1418), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1419 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1419;
  }
}

void p_parse_46_parse_braces(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_at = NULL;
    u_value v_k = NULL;
    u_value v_v = NULL;
    u_value v_entries = NULL;
    u_value v_rev = NULL;
    u_value t1420 = f_parse_46_head_offset_1(*r_ts);
    v_at = t1420;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_expr(v_sc, &v_k, r_ts);
    static struct u_object l__61__62__1421 = U_LIT("=>");
    u_value t1422 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1421));
    v_entries = t1422;
    if (u_truth(v_entries)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_v, r_ts);
      }
    }
    static struct u_object l_for_1423 = U_LIT("for");
    u_value t1424 = f_parse_46_at_keyword_2(*r_ts, &l_for_1423);
    if (u_truth(t1424)) {
      t1424 = v_entries;
    }
    if (u_truth(t1424)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_comprehension(v_sc, v_k, v_v, r_e, r_ts);
      }
    } else {
      static struct u_object l_for_1425 = U_LIT("for");
      if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1425))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_set_comprehension(v_sc, v_k, r_e, r_ts);
        }
      } else {
        {
          u_value t1426;
          if (u_truth(v_entries)) {
            t1426 = u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))));
          } else {
            t1426 = v_k;
          }
          u_value t1427 = u_mk_con(&n_list_46_Cons, 2, t1426, u_mk_con(&n_list_46_Nil, 0));
          v_rev = t1427;
          u_value t1432 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          while (u_truth(t1432)) {
            {
              p_parse_46_advance(r_ts);
              p_parse_46_parse_expr(v_sc, &v_k, r_ts);
              static struct u_object l__61__62__1428 = U_LIT("=>");
              if (u_truth(f_prelude_46__33__61__2(v_entries, u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1428))))) {
                {
                  static struct u_object l_a_32_literal_32__123__46__46__46__125__32__1429 = U_LIT("a literal {...} has either elements a or entries k => v");
                  u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_a_32_literal_32__123__46__46__46__125__32__1429, v_at), "parse.ParseError");
                }
              }
              if (u_truth(v_entries)) {
                {
                  p_parse_46_advance(r_ts);
                  p_parse_46_parse_expr(v_sc, &v_v, r_ts);
                  u_value t1430 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)))), v_rev);
                  v_rev = t1430;
                }
              } else {
                {
                  u_value t1431 = u_mk_con(&n_list_46_Cons, 2, v_k, v_rev);
                  v_rev = t1431;
                }
              }
            }
            t1432 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          }
          u_value t1439;
          if (u_truth(v_entries)) {
            static struct u_object l_map_1433 = U_LIT("map");
            static struct u_object l_put_all_1434 = U_LIT("put_all");
            static struct u_object l_map_1435 = U_LIT("map");
            static struct u_object l_empty_1436 = U_LIT("empty");
            t1439 = f_parse_46_in_library_3(&l_map_1433, &l_put_all_1434, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1435, &l_empty_1436, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
          } else {
            static struct u_object l_set_1437 = U_LIT("set");
            static struct u_object l_from_list_1438 = U_LIT("from_list");
            t1439 = f_parse_46_in_library_3(&l_set_1437, &l_from_list_1438, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Nil, 0)));
          }
          u_value t1440 = t1439;
          *r_e = t1440;
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
  static struct u_object l_Nil_1441 = U_LIT("Nil");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1441), u_mk_con(&n_syntax_46_THole, 0)));
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
    u_value t1442 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1442;
    u_value t1443 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1443;
    u_value t1444 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1444;
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l_in_1445 = U_LIT("in");
    p_parse_46_keyword(&l_in_1445, r_ts);
    p_parse_46_parse_expr(v_sc, &v_l, r_ts);
    static struct u_object l_set_1446 = U_LIT("set");
    static struct u_object l_elements_1447 = U_LIT("elements");
    u_value t1448 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    u_value t1449 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1446, &l_elements_1447, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
    v_xs = t1448;
    v_ls = t1449;
    u_value t1455 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1455)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l_in_1450 = U_LIT("in");
        p_parse_46_keyword(&l_in_1450, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_set_1451 = U_LIT("set");
        static struct u_object l_elements_1452 = U_LIT("elements");
        u_value t1453 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
        u_value t1454 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1451, &l_elements_1452, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), v_ls);
        v_xs = t1453;
        v_ls = t1454;
      }
      t1455 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1456 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1456))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
      }
    }
    static struct u_object l_set_1457 = U_LIT("set");
    static struct u_object l_from_list_1458 = U_LIT("from_list");
    u_value t1459 = f_parse_46_in_library_3(&l_set_1457, &l_from_list_1458, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, v_r, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Nil, 0)));
    *r_e = t1459;
  }
}

void p_parse_46_map_comprehension(u_value v_sc, u_value v_k, u_value v_v, u_value *r_e, u_value *r_ts) {
  {
    u_value v_gs = NULL;
    u_value v_ls = NULL;
    u_value v_c = NULL;
    u_value t1460 = u_mk_con(&n_list_46_Nil, 0);
    v_gs = t1460;
    u_value t1461 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1461;
    u_value t1462 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1462;
    p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
    u_value t1463 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1463)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
      }
      t1463 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1464 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1464))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
        u_value t1465 = f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), v_c);
        v_c = t1465;
      }
    }
    static struct u_object l_map_1466 = U_LIT("map");
    static struct u_object l_put_all_1467 = U_LIT("put_all");
    static struct u_object l_map_1468 = U_LIT("map");
    static struct u_object l_empty_1469 = U_LIT("empty");
    u_value t1470 = f_parse_46_in_library_3(&l_map_1466, &l_put_all_1467, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))))), f_list_46_reverse_1(f_parse_46_hidden_vars_1(v_gs)), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1468, &l_empty_1469, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1470;
  }
}

void p_parse_46_map_generator(u_value v_sc, u_value *r_gs, u_value *r_ls, u_value *r_ts) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value v_m = NULL;
    p_parse_46_parse_plain(v_sc, &v_a, r_ts);
    static struct u_object l__61__62__1471 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1471), r_ts);
    p_parse_46_parse_plain(v_sc, &v_b, r_ts);
    static struct u_object l_in_1472 = U_LIT("in");
    p_parse_46_keyword(&l_in_1472, r_ts);
    p_parse_46_parse_expr(v_sc, &v_m, r_ts);
    static struct u_object l_map_1473 = U_LIT("map");
    static struct u_object l_entries_1474 = U_LIT("entries");
    u_value t1475 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1473, &l_entries_1474, u_mk_con(&n_list_46_Cons, 2, f_parse_46_unpack_2(f_list_46_reverse_1(*r_gs), v_m), u_mk_con(&n_list_46_Nil, 0))), *r_ls);
    *r_ls = t1475;
    static struct u_object l__40_entry_32__1476 = U_LIT("(entry ");
    static struct u_object l__41__1477 = U_LIT(")");
    u_value t1478 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(3, v_a, v_b, u_concat(&l__40_entry_32__1476, u_concat(u_str_show(u_add(f_list_46_length_1(*r_gs), u_mk_int(1))), &l__41__1477))), *r_gs);
    *r_gs = t1478;
  }
}

u_value f_parse_46_hidden_vars_1(u_value v_gs) {
  u_value s1479 = v_gs;
  u_value t1480 = NULL;
  if ((!t1480) && u_is_con(s1479, &n_list_46_Nil)) {
    t1480 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1480) && u_is_con(s1479, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s1479, 0), 2);
    u_value v_rest = u_field(s1479, 1);
    t1480 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0)), f_parse_46_hidden_vars_1(v_rest));
  }
  if (!t1480) {
    u_no_case(s1479);
  }
  return t1480;
}

u_value f_parse_46_unpack_2(u_value v_gs, u_value v_e) {
  u_value s1481 = v_gs;
  u_value t1482 = NULL;
  if ((!t1482) && u_is_con(s1481, &n_list_46_Nil)) {
    t1482 = v_e;
  }
  if ((!t1482) && u_is_con(s1481, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s1481, 0), 0);
    u_value v_v = u_field(u_field(s1481, 0), 1);
    u_value v_h = u_field(u_field(s1481, 0), 2);
    u_value v_rest = u_field(s1481, 1);
    t1482 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_k), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_v), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_parse_46_unpack_2(v_rest, v_e)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1482) {
    u_no_case(s1481);
  }
  return t1482;
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
        static struct u_object l_Nil_1483 = U_LIT("Nil");
        u_value t1484 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1483), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1484;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1485 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1485))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1486 = U_LIT("in");
            p_parse_46_keyword(&l_in_1486, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1487 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1488 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1487;
            v_lists = t1488;
            u_value t1492 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1492)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1489 = U_LIT("in");
                p_parse_46_keyword(&l_in_1489, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1490 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1491 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1490;
                v_lists = t1491;
              }
              t1492 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1493 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1493;
            static struct u_object l_if_1494 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1494))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1495 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1495;
          }
        } else {
          {
            u_value t1496 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1496;
            u_value t1498 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1498)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1497 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1497;
              }
              t1498 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1499 = U_LIT("Nil");
            u_value t1500 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1499), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1500;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1501 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1501;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1502 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1502))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1503 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1503;
      }
    } else {
      {
        u_value t1504 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1504;
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
    u_value t1505 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1505;
    static struct u_object l_case_1506 = U_LIT("case");
    u_value t1509 = f_parse_46_at_keyword_2(*r_ts, &l_case_1506);
    while (u_truth(t1509)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1507 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1507), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1508 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1508;
      }
      static struct u_object l_case_1510 = U_LIT("case");
      t1509 = f_parse_46_at_keyword_2(*r_ts, &l_case_1510);
    }
    static struct u_object l_else_1511 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1511))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1512 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1512;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1513 = U_LIT("end");
    p_parse_46_keyword(&l_end_1513, r_ts);
    u_value t1514 = f_list_46_reverse_1(v_rev);
    *r_cs = t1514;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1515 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1515))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1516 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1516, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
  }
}

u_value f_parse_46_pat_op1_2(u_value v_op, u_value v_p) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_pat_prefix_2(u_value v_op, u_value v_p) {
  u_value s1517 = v_p;
  u_value t1518 = NULL;
  if ((!t1518) && (u_is_con(s1517, &n_syntax_46_PLit) && u_is_con(u_field(s1517, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1517, 0), 0);
    static struct u_object l__45__1519 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1519))) {
      t1518 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1518) {
    t1518 = f_parse_46_pat_op1_2(v_op, v_p);
  }
  if (!t1518) {
    u_no_case(s1517);
  }
  return t1518;
}

u_value f_parse_46_pat_op2_3(u_value v_op, u_value v_l, u_value v_r) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_pats_2(u_value v_ps, u_value v_rest) {
  u_value s1520 = v_ps;
  u_value t1521 = NULL;
  if ((!t1521) && u_is_con(s1520, &n_list_46_Nil)) {
    t1521 = v_rest;
  }
  if ((!t1521) && u_is_con(s1520, &n_list_46_Cons)) {
    u_value v_p = u_field(s1520, 0);
    u_value v_more = u_field(s1520, 1);
    static struct u_object l_Cons_1522 = U_LIT("Cons");
    t1521 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1522), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1521) {
    u_no_case(s1520);
  }
  return t1521;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1523 = v_t;
  u_value t1524 = NULL;
  if ((!t1524) && u_is_con(s1523, &n_lex_46_TName)) {
    u_value v_n = u_field(s1523, 0);
    static struct u_object l___1525 = U_LIT("_");
    static struct u_object l_true_1526 = U_LIT("true");
    static struct u_object l_false_1527 = U_LIT("false");
    u_value t1529 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1525, u_mk_con(&n_list_46_Cons, 2, &l_true_1526, u_mk_con(&n_list_46_Cons, 2, &l_false_1527, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1529)) {
      u_value t1528 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1528)) {
        t1528 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1529 = t1528;
    }
    t1524 = t1529;
  }
  if ((!t1524) && u_is_con(s1523, &n_lex_46_TNum)) {
    t1524 = u_mk_bool(1);
  }
  if ((!t1524) && u_is_con(s1523, &n_lex_46_TStr)) {
    t1524 = u_mk_bool(1);
  }
  if ((!t1524) && u_is_con(s1523, &n_lex_46_TChar)) {
    t1524 = u_mk_bool(1);
  }
  if ((!t1524) && u_is_con(s1523, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1523, 0);
    u_value t1530 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1530)) {
      t1530 = u_eq(v_c, u_mk_char(91));
    }
    t1524 = t1530;
  }
  if (!t1524) {
    t1524 = u_mk_bool(0);
  }
  if (!t1524) {
    u_no_case(s1523);
  }
  return t1524;
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
    u_value t1531 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1531;
    u_value t1532 = u_mk_bool(1);
    v_next = t1532;
    u_value t1533 = u_mk_bool(1);
    v_go = t1533;
    u_value t1542 = v_go;
    while (u_truth(t1542)) {
      {
        u_value s1534 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1535 = 0;
        if ((!d1535) && u_is_con(s1534, &n_option_46_Some)) {
          u_value v_op = u_field(s1534, 0);
          d1535 = 1;
          {
            u_value t1536 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1537 = u_mk_bool(1);
            v_rev = t1536;
            v_next = t1537;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1535) && u_is_con(s1534, &n_option_46_None)) {
          u_value t1540 = v_next;
          if (u_truth(t1540)) {
            t1540 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1540)) {
            d1535 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1538 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1539 = u_mk_bool(0);
              v_rev = t1538;
              v_next = t1539;
            }
          }
        }
        if (!d1535) {
          d1535 = 1;
          {
            u_value t1541 = u_mk_bool(0);
            v_go = t1541;
          }
        }
        if (!d1535) {
          u_no_case(s1534);
        }
      }
      t1542 = v_go;
    }
    u_value s1543 = v_rev;
    int d1544 = 0;
    if ((!d1544) && u_is_con(s1543, &n_list_46_Nil)) {
      d1544 = 1;
      {
        static struct u_object l_pattern_32_expected_1545 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1546 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1545, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1546)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1544) {
      d1544 = 1;
      {
        u_value t1547 = f_list_46_reverse_1(v_rev);
        *r_items = t1547;
      }
    }
    if (!d1544) {
      u_no_case(s1543);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1548 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1548;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1549 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1549;
        u_value t1551 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1551)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1550 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1550;
          }
          t1551 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1552 = f_list_46_reverse_1(v_rev);
    *r_ps = t1552;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1553 = f_parse_46_head_token_1(*r_ts);
    int d1554 = 0;
    if ((!d1554) && (u_is_con(s1553, &n_lex_46_TName) && u_equals(u_field(s1553, 0), u_mk_lit("_")))) {
      d1554 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1555 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1555;
      }
    }
    if ((!d1554) && (u_is_con(s1553, &n_lex_46_TName) && u_equals(u_field(s1553, 0), u_mk_lit("true")))) {
      d1554 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1556 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1556;
      }
    }
    if ((!d1554) && (u_is_con(s1553, &n_lex_46_TName) && u_equals(u_field(s1553, 0), u_mk_lit("false")))) {
      d1554 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1557 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1557;
      }
    }
    if ((!d1554) && u_is_con(s1553, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1553, 0);
      d1554 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1558 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1558;
      }
    }
    if ((!d1554) && u_is_con(s1553, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1553, 0);
      d1554 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1559 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1559;
      }
    }
    if ((!d1554) && u_is_con(s1553, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1553, 0);
      d1554 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1560 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1560;
      }
    }
    if ((!d1554) && (u_is_con(s1553, &n_lex_46_TPunct) && u_equals(u_field(s1553, 0), u_mk_char(40)))) {
      d1554 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1561 = v_ps;
        int d1562 = 0;
        if ((!d1562) && (u_is_con(s1561, &n_list_46_Cons) && u_is_con(u_field(s1561, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1561, 0);
          d1562 = 1;
          {
            u_value t1563 = v_q;
            *r_p = t1563;
          }
        }
        if (!d1562) {
          d1562 = 1;
          {
            u_value t1564 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1564;
          }
        }
        if (!d1562) {
          u_no_case(s1561);
        }
      }
    }
    if ((!d1554) && (u_is_con(s1553, &n_lex_46_TPunct) && u_equals(u_field(s1553, 0), u_mk_char(91)))) {
      d1554 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1565 = U_LIT("Nil");
            u_value t1566 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1565), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1566;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1567 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1567;
            u_value t1569 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1569)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1568 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1568;
              }
              t1569 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1570 = U_LIT("Nil");
            u_value t1571 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1570), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1571;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1572 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1572;
          }
        }
      }
    }
    if (!d1554) {
      d1554 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1573 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1573;
          }
        } else {
          {
            u_value t1574 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1574;
          }
        }
      }
    }
    if (!d1554) {
      u_no_case(s1553);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1575 = v_t;
  u_value t1576 = NULL;
  if ((!t1576) && u_is_con(s1575, &n_lex_46_TName)) {
    u_value v_n = u_field(s1575, 0);
    static struct u_object l_skip_1577 = U_LIT("skip");
    static struct u_object l_if_1578 = U_LIT("if");
    static struct u_object l_while_1579 = U_LIT("while");
    static struct u_object l_call_1580 = U_LIT("call");
    static struct u_object l_match_1581 = U_LIT("match");
    static struct u_object l_assert_1582 = U_LIT("assert");
    static struct u_object l_assign_1583 = U_LIT("assign");
    static struct u_object l_raise_1584 = U_LIT("raise");
    static struct u_object l_try_1585 = U_LIT("try");
    t1576 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1577, u_mk_con(&n_list_46_Cons, 2, &l_if_1578, u_mk_con(&n_list_46_Cons, 2, &l_while_1579, u_mk_con(&n_list_46_Cons, 2, &l_call_1580, u_mk_con(&n_list_46_Cons, 2, &l_match_1581, u_mk_con(&n_list_46_Cons, 2, &l_assert_1582, u_mk_con(&n_list_46_Cons, 2, &l_assign_1583, u_mk_con(&n_list_46_Cons, 2, &l_raise_1584, u_mk_con(&n_list_46_Cons, 2, &l_try_1585, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1576) {
    t1576 = u_mk_bool(0);
  }
  if (!t1576) {
    u_no_case(s1575);
  }
  return t1576;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1586 = v_cs;
  u_value t1587 = NULL;
  if ((!t1587) && u_is_con(s1586, &n_list_46_Nil)) {
    t1587 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1587) && (u_is_con(s1586, &n_list_46_Cons) && u_is_con(u_field(s1586, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1586, 0), 0);
    u_value v_g = u_field(u_field(s1586, 0), 1);
    u_value v_b = u_field(u_field(s1586, 0), 2);
    u_value v_more = u_field(s1586, 1);
    t1587 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1587) {
    u_no_case(s1586);
  }
  return t1587;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1588 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1588;
    u_value t1589 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1589;
    u_value t1590 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1590;
    static struct u_object l_local_1591 = U_LIT("local");
    u_value t1593 = f_parse_46_at_keyword_2(*r_ts, &l_local_1591);
    while (u_truth(t1593)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1592 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1592)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1592 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1594 = U_LIT("local");
      t1593 = f_parse_46_at_keyword_2(*r_ts, &l_local_1594);
    }
    u_value t1596 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1596)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1595 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1595;
      }
      t1596 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1597 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1597;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1598 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1598;
    static struct u_object l__58__1599 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1599)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__1600 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1600)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1601 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1601;
      }
    }
    u_value t1602 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1602;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1603 = f_parse_46_head_token_1(*r_ts);
    int d1604 = 0;
    if ((!d1604) && (u_is_con(s1603, &n_lex_46_TName) && u_equals(u_field(s1603, 0), u_mk_lit("skip")))) {
      d1604 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1605 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1605;
      }
    }
    if ((!d1604) && (u_is_con(s1603, &n_lex_46_TName) && u_equals(u_field(s1603, 0), u_mk_lit("if")))) {
      d1604 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1604) && (u_is_con(s1603, &n_lex_46_TName) && u_equals(u_field(s1603, 0), u_mk_lit("while")))) {
      d1604 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1606 = U_LIT("end");
        p_parse_46_keyword(&l_end_1606, r_ts);
        u_value t1607 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1607;
      }
    }
    if ((!d1604) && (u_is_con(s1603, &n_lex_46_TName) && u_equals(u_field(s1603, 0), u_mk_lit("call")))) {
      d1604 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1604) && (u_is_con(s1603, &n_lex_46_TName) && u_equals(u_field(s1603, 0), u_mk_lit("match")))) {
      d1604 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1608 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1608;
      }
    }
    if ((!d1604) && (u_is_con(s1603, &n_lex_46_TName) && u_equals(u_field(s1603, 0), u_mk_lit("assert")))) {
      d1604 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1609 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1609;
      }
    }
    if ((!d1604) && (u_is_con(s1603, &n_lex_46_TName) && u_equals(u_field(s1603, 0), u_mk_lit("assign")))) {
      d1604 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1604) && (u_is_con(s1603, &n_lex_46_TName) && u_equals(u_field(s1603, 0), u_mk_lit("raise")))) {
      d1604 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1610 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1610;
      }
    }
    if ((!d1604) && (u_is_con(s1603, &n_lex_46_TName) && u_equals(u_field(s1603, 0), u_mk_lit("try")))) {
      d1604 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1611 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1611, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1612 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1612;
      }
    }
    if (!d1604) {
      d1604 = 1;
      {
        static struct u_object l_statement_32_expect_1613 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1614 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1613, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1614)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1604) {
      u_no_case(s1603);
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
    static struct u_object l_else_1615 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1615)))) {
      {
        static struct u_object l_end_1616 = U_LIT("end");
        p_parse_46_keyword(&l_end_1616, r_ts);
        u_value t1617 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1617;
      }
    } else {
      static struct u_object l_if_1618 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1618)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1619 = U_LIT("end");
          p_parse_46_keyword(&l_end_1619, r_ts);
        }
      }
    }
    u_value t1620 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1620;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1621 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1621), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1622 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1622;
    u_value t1625 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1625)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1623 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1623), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1624 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1624;
      }
      t1625 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1626 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1626;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1627 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1627;
    static struct u_object l_case_1628 = U_LIT("case");
    u_value t1631 = f_parse_46_at_keyword_2(*r_ts, &l_case_1628);
    while (u_truth(t1631)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1629 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1629), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1630 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1630;
      }
      static struct u_object l_case_1632 = U_LIT("case");
      t1631 = f_parse_46_at_keyword_2(*r_ts, &l_case_1632);
    }
    static struct u_object l_else_1633 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1633))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1634 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1634;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1635 = U_LIT("end");
    p_parse_46_keyword(&l_end_1635, r_ts);
    u_value t1636 = f_list_46_reverse_1(v_rev);
    *r_cs = t1636;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1638;
  if (u_truth(v_first)) {
    t1638 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1637 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1637)) {
      t1637 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1638 = t1637;
  }
  return t1638;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1640 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1640)) {
    static struct u_object l_out_1639 = U_LIT("out");
    t1640 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1639));
  }
  u_value t1642 = t1640;
  if (u_truth(t1642)) {
    static struct u_object l_ref_1641 = U_LIT("ref");
    t1642 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1641));
  }
  return t1642;
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
    u_value t1643 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), v_ins, v_outs, v_refs);
    *r_s = t1643;
  }
}

void p_parse_46_sections(u_value v_item_in, u_value v_item, u_value v_sc, u_value *r_ins, u_value *r_outs, u_value *r_refs, u_value *r_ts) {
  {
    u_value v_first = NULL;
    u_value t1644 = u_mk_bool(1);
    v_first = t1644;
    u_value t1645 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1646 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1647 = u_mk_con(&n_list_46_Nil, 0);
    *r_ins = t1645;
    *r_outs = t1646;
    *r_refs = t1647;
    static struct u_object l_in_1648 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1648))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_section_items(v_item_in, v_sc, r_ins, r_ts);
        u_value t1649 = u_mk_bool(0);
        v_first = t1649;
      }
    }
    static struct u_object l_out_1650 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1650, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_section_items(v_item, v_sc, r_outs, r_ts);
        u_value t1651 = u_mk_bool(0);
        v_first = t1651;
      }
    }
    static struct u_object l_ref_1652 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1652, v_first))) {
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
    u_value t1653 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1653;
    u_value t1655 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1655)) {
      {
        p_parse_46_advance(r_ts);
        u_call_proc(v_item, 1, 2, v_sc, &v_x, r_ts);
        u_value t1654 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1654;
      }
      t1655 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1656 = f_list_46_reverse_1(v_rev);
    *r_xs = t1656;
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
    u_value t1657 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0));
    *r_x = t1657;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1658 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1658;
    static struct u_object l__58__1659 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1659)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1660 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1660;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1661 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1661;
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
  u_value s1662 = v_cs;
  u_value t1663 = NULL;
  if ((!t1663) && u_is_con(s1662, &n_list_46_Nil)) {
    t1663 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1663) && (u_is_con(s1662, &n_list_46_Cons) && u_is_con(u_field(s1662, 0), &n_syntax_46_CEval))) {
    u_value v_more = u_field(s1662, 1);
    t1663 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1663) && (u_is_con(s1662, &n_list_46_Cons) && u_is_con(u_field(s1662, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1662, 1);
    t1663 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1663) && (u_is_con(s1662, &n_list_46_Cons) && u_is_con(u_field(s1662, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1662, 1);
    t1663 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1663) && u_is_con(s1662, &n_list_46_Cons)) {
    u_value v_c = u_field(s1662, 0);
    u_value v_more = u_field(s1662, 1);
    t1663 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1663) {
    u_no_case(s1662);
  }
  return t1663;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1664 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1665 = &t1664;
  u_value l1666 = v_cs;
  while (u_is_con(l1666, &n_list_46_Cons)) {
    u_value v_c = u_field(l1666, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1665 = u_append(e1665, v_c);
    }
    l1666 = u_field(l1666, 1);
  }
  return t1664;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1667 = v_c;
  u_value t1668 = NULL;
  if ((!t1668) && u_is_con(s1667, &n_syntax_46_CNotation)) {
    t1668 = u_mk_bool(1);
  }
  if (!t1668) {
    t1668 = u_mk_bool(0);
  }
  if (!t1668) {
    u_no_case(s1667);
  }
  return t1668;
}

u_value f_parse_46_Context_46_notations_1(u_value v_x) {
  u_value s1669 = v_x;
  u_value t1670 = NULL;
  if ((!t1670) && u_is_con(s1669, &n_parse_46_Context)) {
    u_value v_y = u_field(s1669, 0);
    t1670 = v_y;
  }
  if (!t1670) {
    u_no_case(s1669);
  }
  return t1670;
}

u_value f_parse_46_Context_46_modules_1(u_value v_x) {
  u_value s1671 = v_x;
  u_value t1672 = NULL;
  if ((!t1672) && u_is_con(s1671, &n_parse_46_Context)) {
    u_value v_y = u_field(s1671, 1);
    t1672 = v_y;
  }
  if (!t1672) {
    u_no_case(s1671);
  }
  return t1672;
}

u_value f_parse_46_Context_46_loading_1(u_value v_x) {
  u_value s1673 = v_x;
  u_value t1674 = NULL;
  if ((!t1674) && u_is_con(s1673, &n_parse_46_Context)) {
    u_value v_y = u_field(s1673, 2);
    t1674 = v_y;
  }
  if (!t1674) {
    u_no_case(s1673);
  }
  return t1674;
}

u_value f_parse_46_Context_46_base_1(u_value v_x) {
  u_value s1675 = v_x;
  u_value t1676 = NULL;
  if ((!t1676) && u_is_con(s1675, &n_parse_46_Context)) {
    u_value v_y = u_field(s1675, 3);
    t1676 = v_y;
  }
  if (!t1676) {
    u_no_case(s1675);
  }
  return t1676;
}

u_value f_parse_46_Context_46_dir_1(u_value v_x) {
  u_value s1677 = v_x;
  u_value t1678 = NULL;
  if ((!t1678) && u_is_con(s1677, &n_parse_46_Context)) {
    u_value v_y = u_field(s1677, 4);
    t1678 = v_y;
  }
  if (!t1678) {
    u_no_case(s1677);
  }
  return t1678;
}

u_value f_parse_46_Context_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1679 = v_x;
  u_value t1680 = NULL;
  if ((!t1680) && u_is_con(s1679, &n_parse_46_Context)) {
    u_value v_a1 = u_field(s1679, 1);
    u_value v_a2 = u_field(s1679, 2);
    u_value v_a3 = u_field(s1679, 3);
    u_value v_a4 = u_field(s1679, 4);
    t1680 = u_mk_con(&n_parse_46_Context, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1680) {
    u_no_case(s1679);
  }
  return t1680;
}

u_value f_parse_46_Context_46_modules_58__61__2(u_value v_x, u_value v_y) {
  u_value s1681 = v_x;
  u_value t1682 = NULL;
  if ((!t1682) && u_is_con(s1681, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1681, 0);
    u_value v_a2 = u_field(s1681, 2);
    u_value v_a3 = u_field(s1681, 3);
    u_value v_a4 = u_field(s1681, 4);
    t1682 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t1682) {
    u_no_case(s1681);
  }
  return t1682;
}

u_value f_parse_46_Context_46_loading_58__61__2(u_value v_x, u_value v_y) {
  u_value s1683 = v_x;
  u_value t1684 = NULL;
  if ((!t1684) && u_is_con(s1683, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1683, 0);
    u_value v_a1 = u_field(s1683, 1);
    u_value v_a3 = u_field(s1683, 3);
    u_value v_a4 = u_field(s1683, 4);
    t1684 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t1684) {
    u_no_case(s1683);
  }
  return t1684;
}

u_value f_parse_46_Context_46_base_58__61__2(u_value v_x, u_value v_y) {
  u_value s1685 = v_x;
  u_value t1686 = NULL;
  if ((!t1686) && u_is_con(s1685, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1685, 0);
    u_value v_a1 = u_field(s1685, 1);
    u_value v_a2 = u_field(s1685, 2);
    u_value v_a4 = u_field(s1685, 4);
    t1686 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t1686) {
    u_no_case(s1685);
  }
  return t1686;
}

u_value f_parse_46_Context_46_dir_58__61__2(u_value v_x, u_value v_y) {
  u_value s1687 = v_x;
  u_value t1688 = NULL;
  if ((!t1688) && u_is_con(s1687, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1687, 0);
    u_value v_a1 = u_field(s1687, 1);
    u_value v_a2 = u_field(s1687, 2);
    u_value v_a3 = u_field(s1687, 3);
    t1688 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t1688) {
    u_no_case(s1687);
  }
  return t1688;
}

u_value f_parse_46_with_commands_2(u_value v_cx, u_value v_cs) {
  return f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_with_notations_2(f_parse_46_Context_46_notations_1(v_cx), v_cs));
}

u_value f_parse_46_with_module_2(u_value v_cx, u_value v_c) {
  return f_parse_46_Context_46_modules_58__61__2(v_cx, f_list_46__43__43__2(f_parse_46_modules_of_1(v_c), f_parse_46_Context_46_modules_1(v_cx)));
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1689 = v_c;
  u_value t1690 = NULL;
  if ((!t1690) && u_is_con(s1689, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1689, 0);
    u_value v_cs = u_field(s1689, 1);
    u_value t1691 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1692 = &t1691;
    u_value l1693 = v_cs;
    while (u_is_con(l1693, &n_list_46_Cons)) {
      u_value v_d = u_field(l1693, 0);
      if (u_truth(u_mk_bool(1))) {
        e1692 = u_append(e1692, f_parse_46_modules_of_1(v_d));
      }
      l1693 = u_field(l1693, 1);
    }
    t1690 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1691));
  }
  if (!t1690) {
    t1690 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1690) {
    u_no_case(s1689);
  }
  return t1690;
}

u_value f_parse_46_own_notations_2(u_value v_cx, u_value v_n) {
  u_value s1694 = f_list_46_lookup_2(v_n, f_parse_46_Context_46_modules_1(v_cx));
  u_value t1695 = NULL;
  if ((!t1695) && u_is_con(s1694, &n_option_46_Some)) {
    u_value v_ns = u_field(s1694, 0);
    t1695 = v_ns;
  }
  if ((!t1695) && u_is_con(s1694, &n_option_46_None)) {
    t1695 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1695) {
    u_no_case(s1694);
  }
  return t1695;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1696 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1696;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1697 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1697;
        u_value t1699 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1699)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1698 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1698;
          }
          t1699 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1700 = f_list_46_reverse_1(v_rev);
    *r_ps = t1700;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1701 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1701;
    u_value s1702 = f_parse_46_head_token_1(*r_ts);
    int d1703 = 0;
    if ((!d1703) && (u_is_con(s1702, &n_lex_46_TName) && u_equals(u_field(s1702, 0), u_mk_lit("prefix")))) {
      d1703 = 1;
      {
        u_value t1704 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1704;
      }
    }
    if ((!d1703) && (u_is_con(s1702, &n_lex_46_TName) && u_equals(u_field(s1702, 0), u_mk_lit("postfix")))) {
      d1703 = 1;
      {
        u_value t1705 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1705;
      }
    }
    if (!d1703) {
      d1703 = 1;
      {
        u_value s1706 = f_parse_46_second_token_1(*r_ts);
        int d1707 = 0;
        if ((!d1707) && (u_is_con(s1706, &n_lex_46_TName) && u_equals(u_field(s1706, 0), u_mk_lit("left")))) {
          d1707 = 1;
          {
            u_value t1708 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1708;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1707) && (u_is_con(s1706, &n_lex_46_TName) && u_equals(u_field(s1706, 0), u_mk_lit("right")))) {
          d1707 = 1;
          {
            u_value t1709 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1709;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1707) {
          d1707 = 1;
          {
          }
        }
        if (!d1707) {
          u_no_case(s1706);
        }
      }
    }
    if (!d1703) {
      u_no_case(s1702);
    }
    p_parse_46_advance(r_ts);
    u_value s1710 = f_parse_46_head_token_1(*r_ts);
    int d1711 = 0;
    if ((!d1711) && u_is_con(s1710, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1710, 0);
      d1711 = 1;
      {
        u_value t1712 = v_n;
        v_p = t1712;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1711) {
      d1711 = 1;
      {
        static struct u_object l_precedence_32_expec_1713 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1714 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1713, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1714)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1711) {
      u_no_case(s1710);
    }
    u_value t1715 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1716 = u_mk_bool(1);
    v_ops = t1715;
    v_go = t1716;
    u_value t1722 = v_go;
    while (u_truth(t1722)) {
      {
        u_value s1717 = f_parse_46_head_token_1(*r_ts);
        int d1718 = 0;
        if ((!d1718) && u_is_con(s1717, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1717, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1718 = 1;
            {
              u_value t1719 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1719;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1718) && u_is_con(s1717, &n_lex_46_TName)) {
          u_value v_n = u_field(s1717, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1718 = 1;
            {
              u_value t1720 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1720;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1718) {
          d1718 = 1;
          {
            u_value t1721 = u_mk_bool(0);
            v_go = t1721;
          }
        }
        if (!d1718) {
          u_no_case(s1717);
        }
      }
      t1722 = v_go;
    }
    u_value s1723 = v_ops;
    int d1724 = 0;
    if ((!d1724) && u_is_con(s1723, &n_list_46_Nil)) {
      d1724 = 1;
      {
        static struct u_object l_operator_32_expecte_1725 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1726 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1725, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1726)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1724) {
      d1724 = 1;
      {
        u_value t1727 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1727;
      }
    }
    if (!d1724) {
      u_no_case(s1723);
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
    u_value t1728 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1729 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1728;
    *r_fs = t1729;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1730 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
            u_value t1731 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
            v_args = t1730;
            *r_fs = t1731;
            u_value t1734 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1734)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t1732 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t1733 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t1732;
                *r_fs = t1733;
              }
              t1734 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1735 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t1736 = f_list_46_reverse_1(*r_fs);
    *r_k = t1735;
    *r_fs = t1736;
    u_value s1737 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d1738 = 0;
    if ((!d1738) && u_is_con(s1737, &n_option_46_Some)) {
      u_value v_f = u_field(s1737, 0);
      d1738 = 1;
      {
        static struct u_object l_repeated_32_field_32__1739 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__1740 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__1739, u_concat(v_f, u_concat(&l__32_of_32__1740, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1738) && u_is_con(s1737, &n_option_46_None)) {
      d1738 = 1;
      {
      }
    }
    if (!d1738) {
      u_no_case(s1737);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1741 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1742 = 0;
    if ((!d1742) && (u_is_con(u_field(s1741, 0), &n_lex_46_TName) && (u_is_con(u_field(s1741, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1741, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1741, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1742 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1743 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1743;
        }
      }
    }
    if (!d1742) {
      d1742 = 1;
      {
        u_value t1744 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1744;
      }
    }
    if (!d1742) {
      u_no_case(s1741);
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
    static struct u_object l__58__61__1745 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1745), r_ts);
    static struct u_object l__124__1746 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1746)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1747 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1747;
    static struct u_object l__124__1748 = U_LIT("|");
    u_value t1750 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1748));
    while (u_truth(t1750)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1749 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1749;
      }
      static struct u_object l__124__1751 = U_LIT("|");
      t1750 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1751));
    }
    u_value s1752 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d1753 = 0;
    if ((!d1753) && u_is_con(s1752, &n_option_46_Some)) {
      u_value v_f = u_field(s1752, 0);
      d1753 = 1;
      {
        static struct u_object l_field_32__1754 = U_LIT("field ");
        static struct u_object l__32_of_32__1755 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_1756 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__1754, u_concat(v_f, u_concat(&l__32_of_32__1755, u_concat(v_n, &l__32_has_32_different_32_t_1756)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1753) && u_is_con(s1752, &n_option_46_None)) {
      d1753 = 1;
      {
      }
    }
    if (!d1753) {
      u_no_case(s1752);
    }
    u_value t1757 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_list_46__43__43__2(f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)), f_parse_46_updaters_3(v_n, v_ps, f_list_46_reverse_1(v_rev))));
    *r_cs = t1757;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s1758 = v_fs;
  u_value t1759 = NULL;
  if ((!t1759) && u_is_con(s1758, &n_list_46_Nil)) {
    t1759 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1759) && (u_is_con(s1758, &n_list_46_Cons) && u_is_con(u_field(s1758, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s1758, 0), 0);
    u_value v_rest = u_field(s1758, 1);
    t1759 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t1759) && (u_is_con(s1758, &n_list_46_Cons) && u_is_con(u_field(s1758, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1758, 1);
    t1759 = f_parse_46_field_names_1(v_rest);
  }
  if (!t1759) {
    u_no_case(s1758);
  }
  return t1759;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s1760 = v_fs;
  u_value t1761 = NULL;
  if ((!t1761) && u_is_con(s1760, &n_list_46_Nil)) {
    t1761 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1761) && u_is_con(s1760, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1760, 0), 0);
    u_value v_t = u_field(u_field(u_field(s1760, 0), 1), 3);
    u_value v_rest = u_field(s1760, 1);
    u_value t1762 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1763 = &t1762;
    u_value l1764 = f_parse_46_fields_named_2(v_f, v_rest);
    while (u_is_con(l1764, &n_list_46_Cons)) {
      u_value v_e = u_field(l1764, 0);
      if (u_truth(u_mk_bool(1))) {
        e1763 = u_append(e1763, f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t));
      }
      l1764 = u_field(l1764, 1);
    }
    u_value t1765;
    if (u_truth(f_list_46_contains_2(u_mk_bool(1), t1762))) {
      t1765 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t1765 = f_parse_46_differing_1(v_rest);
    }
    t1761 = t1765;
  }
  if (!t1761) {
    u_no_case(s1760);
  }
  return t1761;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s1766 = v_e;
  u_value t1767 = NULL;
  if (!t1767) {
    u_value v_t = u_field(u_field(s1766, 1), 3);
    t1767 = v_t;
  }
  if (!t1767) {
    u_no_case(s1766);
  }
  return t1767;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s1768 = v_fs;
  u_value t1769 = NULL;
  if ((!t1769) && u_is_con(s1768, &n_list_46_Nil)) {
    t1769 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1769) && u_is_con(s1768, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s1768, 0), 0);
    u_value v_d = u_field(u_field(s1768, 0), 1);
    u_value v_rest = u_field(s1768, 1);
    u_value t1770;
    if (u_truth(u_eq(v_g, v_f))) {
      t1770 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t1770 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t1769 = t1770;
  }
  if (!t1769) {
    u_no_case(s1768);
  }
  return t1769;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1771 = v_ks;
  u_value t1772 = NULL;
  if ((!t1772) && u_is_con(s1771, &n_list_46_Nil)) {
    t1772 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1772) && (u_is_con(s1771, &n_list_46_Cons) && u_is_con(u_field(u_field(s1771, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1771, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1771, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1771, 0), 1);
    u_value v_rest = u_field(s1771, 1);
    t1772 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1772) {
    u_no_case(s1771);
  }
  return t1772;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1773 = v_fs;
  u_value t1774 = NULL;
  if ((!t1774) && u_is_con(s1773, &n_list_46_Nil)) {
    t1774 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1774) && (u_is_con(s1773, &n_list_46_Cons) && u_is_con(u_field(u_field(s1773, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1773, 0), 0), 0);
    u_value v_t = u_field(u_field(s1773, 0), 1);
    u_value v_rest = u_field(s1773, 1);
    t1774 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1774) && (u_is_con(s1773, &n_list_46_Cons) && u_is_con(u_field(u_field(s1773, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1773, 1);
    t1774 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1774) {
    u_no_case(s1773);
  }
  return t1774;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1779 = f_parse_46_fields_of_1(v_ks);
  u_value t1778;
  {
    u_value v_fs = t1779;
    u_value t1775 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1776 = &t1775;
    u_value l1777 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1777, &n_list_46_Cons)) {
      u_value v_f = u_field(l1777, 0);
      if (u_truth(u_mk_bool(1))) {
        e1776 = u_append(e1776, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1777 = u_field(l1777, 1);
    }
    t1778 = t1775;
  }
  return t1778;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1780 = U_LIT("x");
  u_value t1781 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1782 = &t1781;
  u_value l1783 = v_ps;
  while (u_is_con(l1783, &n_list_46_Cons)) {
    u_value v_p = u_field(l1783, 0);
    if (u_truth(u_mk_bool(1))) {
      e1782 = u_append(e1782, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1783 = u_field(l1783, 1);
  }
  u_value t1791 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1780), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1792 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1781);
  u_value t1790;
  {
    u_value v_x = t1791;
    u_value v_owner = t1792;
    static struct u_object l__46__1784 = U_LIT(".");
    static struct u_object l_x_1785 = U_LIT("x");
    u_value t1786 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1787 = &t1786;
    u_value l1788 = v_es;
    while (u_is_con(l1788, &n_list_46_Cons)) {
      u_value v_e = u_field(l1788, 0);
      if (u_truth(u_mk_bool(1))) {
        e1787 = u_append(e1787, f_parse_46_field_case_1(v_e));
      }
      l1788 = u_field(l1788, 1);
    }
    u_value t1789;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1789 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1789 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1790 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1784, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1785), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1786, t1789)));
  }
  return t1790;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1793 = v_es;
  u_value t1794 = NULL;
  if ((!t1794) && u_is_con(s1793, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1793, 0), 1), 3);
    t1794 = v_t;
  }
  if (!t1794) {
    t1794 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1794) {
    u_no_case(s1793);
  }
  return t1794;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s1795 = v_e;
  u_value t1796 = NULL;
  if (!t1796) {
    u_value v_k = u_field(u_field(s1795, 1), 0);
    u_value v_i = u_field(u_field(s1795, 1), 1);
    u_value v_n = u_field(u_field(s1795, 1), 2);
    u_value t1797 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1798 = &t1797;
    u_value l1799 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1799, &n_list_46_Cons)) {
      u_value v_j = u_field(l1799, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1801;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1800 = U_LIT("y");
          t1801 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1800), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1801 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1798 = u_append(e1798, t1801);
      }
      l1799 = u_field(l1799, 1);
    }
    static struct u_object l_y_1802 = U_LIT("y");
    t1796 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1797), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1802), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1796) {
    u_no_case(s1795);
  }
  return t1796;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_MissingField_1803 = U_LIT("MissingField");
  static struct u_object l__46__1804 = U_LIT(".");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_ERaise, 2, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_MissingField_1803), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_t, u_concat(&l__46__1804, v_f)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_parse_46_updaters_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1809 = f_parse_46_fields_of_1(v_ks);
  u_value t1808;
  {
    u_value v_fs = t1809;
    u_value t1805 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1806 = &t1805;
    u_value l1807 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1807, &n_list_46_Cons)) {
      u_value v_f = u_field(l1807, 0);
      if (u_truth(u_mk_bool(1))) {
        e1806 = u_append(e1806, f_parse_46_updater_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1807 = u_field(l1807, 1);
    }
    t1808 = t1805;
  }
  return t1808;
}

u_value f_parse_46_updater_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1810 = U_LIT("x");
  u_value t1811 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1812 = &t1811;
  u_value l1813 = v_ps;
  while (u_is_con(l1813, &n_list_46_Cons)) {
    u_value v_p = u_field(l1813, 0);
    if (u_truth(u_mk_bool(1))) {
      e1812 = u_append(e1812, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1813 = u_field(l1813, 1);
  }
  u_value t1823 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1810), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1824 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1811);
  u_value t1822;
  {
    u_value v_x = t1823;
    u_value v_owner = t1824;
    static struct u_object l__46__1814 = U_LIT(".");
    static struct u_object l__58__61__1815 = U_LIT(":=");
    static struct u_object l_x_1816 = U_LIT("x");
    static struct u_object l_y_1817 = U_LIT("y");
    u_value t1818 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1819 = &t1818;
    u_value l1820 = v_es;
    while (u_is_con(l1820, &n_list_46_Cons)) {
      u_value v_e = u_field(l1820, 0);
      if (u_truth(u_mk_bool(1))) {
        e1819 = u_append(e1819, f_parse_46_update_case_1(v_e));
      }
      l1820 = u_field(l1820, 1);
    }
    u_value t1821;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1821 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1821 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1822 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1814, u_concat(v_f, &l__58__61__1815)))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1816), v_owner), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1817), f_parse_46_field_type_1(v_es)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), v_owner, u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1818, t1821)));
  }
  return t1822;
}

u_value f_parse_46_update_case_1(u_value v_e) {
  u_value s1825 = v_e;
  u_value t1826 = NULL;
  if (!t1826) {
    u_value v_k = u_field(u_field(s1825, 1), 0);
    u_value v_i = u_field(u_field(s1825, 1), 1);
    u_value v_n = u_field(u_field(s1825, 1), 2);
    u_value t1827 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1828 = &t1827;
    u_value l1829 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1829, &n_list_46_Cons)) {
      u_value v_j = u_field(l1829, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1831;
        if (u_truth(u_eq(v_j, v_i))) {
          t1831 = u_mk_con(&n_syntax_46_PWild, 0);
        } else {
          static struct u_object l_a_1830 = U_LIT("a");
          t1831 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l_a_1830, u_str_show(v_j))), u_mk_con(&n_syntax_46_THole, 0)));
        }
        e1828 = u_append(e1828, t1831);
      }
      l1829 = u_field(l1829, 1);
    }
    u_value t1832 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1833 = &t1832;
    u_value l1834 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1834, &n_list_46_Cons)) {
      u_value v_j = u_field(l1834, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1837;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1835 = U_LIT("y");
          t1837 = &l_y_1835;
        } else {
          static struct u_object l_a_1836 = U_LIT("a");
          t1837 = u_concat(&l_a_1836, u_str_show(v_j));
        }
        e1833 = u_append(e1833, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(t1837), u_mk_con(&n_syntax_46_THole, 0))));
      }
      l1834 = u_field(l1834, 1);
    }
    t1826 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1827), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_k, u_mk_con(&n_syntax_46_THole, 0))), t1832));
  }
  if (!t1826) {
    u_no_case(s1825);
  }
  return t1826;
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t1838;
  if (u_truth(u_eq(v_i, v_n))) {
    t1838 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1838 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t1838;
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
    u_value t1839 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1839;
    u_value t1841 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1841)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1840 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1840;
      }
      t1841 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1842 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1842), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1843 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1843), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1844 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1844;
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
    static struct u_object l__58__61__1845 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1845), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1846 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1846;
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
    u_value t1847 = f_parse_46_Context_46_notations_1(v_cx);
    v_ns = t1847;
    u_value s1848 = f_parse_46_head_token_1(*r_ts);
    int d1849 = 0;
    if ((!d1849) && (u_is_con(s1848, &n_lex_46_TName) && u_equals(u_field(s1848, 0), u_mk_lit("import")))) {
      d1849 = 1;
      {
        u_value t1850 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1850;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Context_46_modules_1(v_cx))))) {
          {
            u_value t1851 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1851;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_cx, v_at, r_cs);
            u_value t1852 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1852;
          }
        }
      }
    }
    if ((!d1849) && u_is_con(s1848, &n_lex_46_TName)) {
      u_value v_w = u_field(s1848, 0);
      static struct u_object l_prefix_1854 = U_LIT("prefix");
      static struct u_object l_postfix_1855 = U_LIT("postfix");
      static struct u_object l_infix_1856 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1854, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1855, u_mk_con(&n_list_46_Cons, 2, &l_infix_1856, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1849 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1853 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1853;
        }
      }
    }
    if ((!d1849) && (u_is_con(s1848, &n_lex_46_TName) && u_equals(u_field(s1848, 0), u_mk_lit("type")))) {
      d1849 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1857 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1857), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1858 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1858;
      }
    }
    if ((!d1849) && (u_is_con(s1848, &n_lex_46_TName) && u_equals(u_field(s1848, 0), u_mk_lit("data")))) {
      d1849 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1849) && (u_is_con(s1848, &n_lex_46_TName) && u_equals(u_field(s1848, 0), u_mk_lit("function")))) {
      d1849 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1859 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1859;
      }
    }
    if ((!d1849) && (u_is_con(s1848, &n_lex_46_TName) && u_equals(u_field(s1848, 0), u_mk_lit("procedure")))) {
      d1849 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1860 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1860;
      }
    }
    if ((!d1849) && (u_is_con(s1848, &n_lex_46_TName) && u_equals(u_field(s1848, 0), u_mk_lit("eval")))) {
      d1849 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_e, r_ts);
        u_value t1861 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CEval, 1, v_e), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1861;
      }
    }
    if ((!d1849) && (u_is_con(s1848, &n_lex_46_TName) && u_equals(u_field(s1848, 0), u_mk_lit("run")))) {
      d1849 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1862 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1862), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1863 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1863;
      }
    }
    if ((!d1849) && (u_is_con(s1848, &n_lex_46_TName) && u_equals(u_field(s1848, 0), u_mk_lit("test")))) {
      d1849 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1864 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1864), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1865 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1865;
      }
    }
    if (!d1849) {
      d1849 = 1;
      {
        static struct u_object l_declaration_32_expe_1866 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1867 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1866, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1867)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1849) {
      u_no_case(s1848);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_cx) {
  {
    u_value s1868 = v_cs;
    int d1869 = 0;
    if ((!d1869) && u_is_con(s1868, &n_list_46_Nil)) {
      d1869 = 1;
      {
      }
    }
    if ((!d1869) && (u_is_con(s1868, &n_list_46_Cons) && u_is_con(u_field(s1868, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1868, 0), 0);
      u_value v_p = u_field(u_field(s1868, 0), 1);
      u_value v_ops = u_field(u_field(s1868, 0), 2);
      u_value v_more = u_field(s1868, 1);
      d1869 = 1;
      {
        u_value t1870 = f_parse_46_with_commands_2(*r_cx, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_cx = t1870;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1869) && (u_is_con(s1868, &n_list_46_Cons) && u_is_con(u_field(s1868, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1868, 0), 0);
      u_value v_ds = u_field(u_field(s1868, 0), 1);
      u_value v_more = u_field(s1868, 1);
      d1869 = 1;
      {
        u_value t1871 = f_parse_46_with_module_2(*r_cx, u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds));
        *r_cx = t1871;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1869) && (u_is_con(s1868, &n_list_46_Cons) && u_is_con(u_field(s1868, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1868, 0), 0);
      u_value v_more = u_field(s1868, 1);
      d1869 = 1;
      {
        u_value t1872 = f_parse_46_with_commands_2(*r_cx, f_parse_46_own_notations_2(*r_cx, v_n));
        *r_cx = t1872;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1869) && u_is_con(s1868, &n_list_46_Cons)) {
      u_value v_more = u_field(s1868, 1);
      d1869 = 1;
      {
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if (!d1869) {
      u_no_case(s1868);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_cx, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1873 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1873;
    u_value t1875 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1875)) {
      {
        p_parse_46_parse_command(*r_cx, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_cx);
        u_value t1874 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1874;
      }
      t1875 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1876 = f_list_46_reverse_1(v_rev);
    *r_cs = t1876;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1877 = v_dir;
    int d1878 = 0;
    if ((!d1878) && u_is_con(s1877, &n_option_46_Some)) {
      u_value v_d = u_field(s1877, 0);
      d1878 = 1;
      {
        {
          struct u_handler *h1881 = u_push();
          if (setjmp(h1881->env) == 0) {
            {
              static struct u_object l__47__1879 = U_LIT("/");
              static struct u_object l__46_u_1880 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1879, u_concat(v_n, &l__46_u_1880))), r_text);
            }
            u_pop(h1881);
          } else {
            u_pop(h1881);
            int d1882 = 0;
            if ((!d1882) && (u_has_type(h1881, "os.OsError") && u_is_con(u_caught(h1881), &n_os_46_OsError))) {
              d1882 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1882) {
              u_reraise(h1881);
            }
          }
        }
      }
    }
    if ((!d1878) && u_is_con(s1877, &n_option_46_None)) {
      d1878 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1878) {
      u_no_case(s1877);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1883 = u_push();
      if (setjmp(h1883->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1883);
      } else {
        u_pop(h1883);
        int d1884 = 0;
        if ((!d1884) && (u_has_type(h1883, "os.OsError") && u_is_con(u_caught(h1883), &n_os_46_OsError))) {
          d1884 = 1;
          {
            static struct u_object l_unknown_32_module_32__1885 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1885, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1884) {
          u_reraise(h1883);
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
        static struct u_object l_cyclic_32_import_32_of_1886 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1886, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Context_46_dir_1(v_cx), v_at, &v_text);
    u_value t1887 = f_parse_46_Context_46_loading_58__61__2(f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_base_notations_1(v_cx)), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Context_46_loading_1(v_cx)));
    v_mcx = t1887;
    {
      struct u_handler *h1888 = u_push();
      if (setjmp(h1888->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mcx, &v_mts);
        }
        u_pop(h1888);
      } else {
        u_pop(h1888);
        int d1889 = 0;
        if ((!d1889) && (u_has_type(h1888, "lex.LexError") && u_is_con(u_caught(h1888), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1888), 0);
          u_value v_i = u_field(u_caught(h1888), 1);
          d1889 = 1;
          {
            static struct u_object l__32_in_32__1890 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1890, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1889) && (u_has_type(h1888, "parse.ParseError") && u_is_con(u_caught(h1888), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1888), 0);
          u_value v_i = u_field(u_caught(h1888), 1);
          d1889 = 1;
          {
            static struct u_object l__32_in_32__1891 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1891, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1889) {
          u_reraise(h1888);
        }
      }
    }
  }
}

u_value f_parse_46_base_notations_1(u_value v_cx) {
  u_value s1892 = f_parse_46_Context_46_base_1(v_cx);
  u_value t1893 = NULL;
  if ((!t1893) && u_is_con(s1892, &n_option_46_Some)) {
    u_value v_b = u_field(s1892, 0);
    t1893 = v_b;
  }
  if ((!t1893) && u_is_con(s1892, &n_option_46_None)) {
    t1893 = f_parse_46_Context_46_notations_1(v_cx);
  }
  if (!t1893) {
    u_no_case(s1892);
  }
  return t1893;
}

void p_parse_46_parse_text(u_value v_text, u_value v_cx, u_value *r_p) {
  {
    u_value v_ts = NULL;
    u_value v_cs = NULL;
    u_value v_after = NULL;
    u_value t1894 = v_cx;
    v_after = t1894;
    {
      struct u_handler *h1895 = u_push();
      if (setjmp(h1895->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1895);
      } else {
        u_pop(h1895);
        int d1896 = 0;
        if ((!d1896) && (u_has_type(h1895, "lex.LexError") && u_is_con(u_caught(h1895), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1895), 0);
          u_value v_i = u_field(u_caught(h1895), 1);
          d1896 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1896) {
          u_reraise(h1895);
        }
      }
    }
    p_parse_46_parse_commands(&v_cs, &v_after, &v_ts);
    u_value t1897 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1897;
  }
}

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p) {
  {
    u_value v_cx = NULL;
    u_value t1898 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    v_cx = t1898;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), &v_cx);
    p_parse_46_parse_text(v_text, f_parse_46_Context_46_dir_58__61__2(f_parse_46_Context_46_base_58__61__2(f_parse_46_Context_46_loading_58__61__2(v_cx, u_mk_con(&n_list_46_Cons, 2, v_name, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_option_46_Some, 1, f_parse_46_Context_46_notations_1(v_cx))), v_dir), r_p);
  }
}

void p_parse_46_parse_library(u_value v_n, u_value *r_p) {
  {
    u_value v_text = NULL;
    u_value v_pre = NULL;
    p_parse_46_library_text(v_n, u_mk_int(0), &v_text);
    static struct u_object l_prelude_1899 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1899))) {
      {
        static struct u_object l_prelude_1900 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1900, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1901 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1901, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1902 = v_xs;
  u_value t1903 = NULL;
  if ((!t1903) && (u_is_con(s1902, &n_list_46_Cons) && u_is_con(u_field(s1902, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1902, 0);
    t1903 = v_x;
  }
  if ((!t1903) && u_is_con(s1902, &n_list_46_Cons)) {
    u_value v_more = u_field(s1902, 1);
    t1903 = f_parse_46_last_part_1(v_more);
  }
  if (!t1903) {
    static struct u_object l__1904 = U_LIT("");
    t1903 = &l__1904;
  }
  if (!t1903) {
    u_no_case(s1902);
  }
  return t1903;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1909 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1908;
  {
    u_value v_f = t1909;
    u_value t1906 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1906)) {
      static struct u_object l__46_u_1905 = U_LIT(".u");
      t1906 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1905);
    }
    u_value t1907;
    if (u_truth(t1906)) {
      t1907 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1907 = v_f;
    }
    t1908 = t1907;
  }
  return t1908;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1914 = u_str_split(v_path, u_mk_char(47));
  u_value t1913;
  {
    u_value v_parts = t1914;
    u_value t1912;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1910 = U_LIT(".");
      t1912 = &l__46__1910;
    } else {
      static struct u_object l__47__1911 = U_LIT("/");
      t1912 = u_str_join(&l__47__1911, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1913 = t1912;
  }
  return t1913;
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
    static struct u_object l_prelude_1915 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1915, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1916 = u_mk_tuple(2, v_cs, v_ds);
    int d1917 = 0;
    if ((!d1917) && (u_is_con(u_field(s1916, 0), &n_list_46_Cons) && u_is_con(u_field(s1916, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1916, 0), 0);
      u_value v_more = u_field(u_field(s1916, 0), 1);
      u_value v_d = u_field(u_field(s1916, 1), 0);
      u_value v_rest = u_field(u_field(s1916, 1), 1);
      d1917 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1917) {
      d1917 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1917) {
      u_no_case(s1916);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1918 = v_paths;
    int d1919 = 0;
    if ((!d1919) && u_is_con(s1918, &n_list_46_Nil)) {
      d1919 = 1;
      {
      }
    }
    if ((!d1919) && u_is_con(s1918, &n_list_46_Cons)) {
      u_value v_path = u_field(s1918, 0);
      u_value v_more = u_field(s1918, 1);
      d1919 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1919) {
      u_no_case(s1918);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1920 = v_paths;
    int d1921 = 0;
    if ((!d1921) && u_is_con(s1920, &n_list_46_Nil)) {
      d1921 = 1;
      {
      }
    }
    if ((!d1921) && u_is_con(s1920, &n_list_46_Cons)) {
      u_value v_path = u_field(s1920, 0);
      u_value v_more = u_field(s1920, 1);
      d1921 = 1;
      {
        u_value t1922 = u_mk_bool(0);
        v_failed = t1922;
        {
          struct u_handler *h1923 = u_push();
          if (setjmp(h1923->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1923);
          } else {
            u_pop(h1923);
            int d1924 = 0;
            if ((!d1924) && (u_has_type(h1923, "parse.ParseError") && u_is_con(u_caught(h1923), &n_parse_46_ParseError))) {
              d1924 = 1;
              {
                u_value t1925 = u_mk_bool(1);
                v_failed = t1925;
              }
            }
            if (!d1924) {
              u_reraise(h1923);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1921) {
      u_no_case(s1920);
    }
  }
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s1926 = v_x;
  u_value t1927 = NULL;
  if ((!t1927) && u_is_con(s1926, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1926, 0);
    t1927 = v_y;
  }
  if (!t1927) {
    u_no_case(s1926);
  }
  return t1927;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s1928 = v_x;
  u_value t1929 = NULL;
  if ((!t1929) && u_is_con(s1928, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1928, 1);
    t1929 = v_y;
  }
  if (!t1929) {
    u_no_case(s1928);
  }
  return t1929;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s1930 = v_x;
  u_value t1931 = NULL;
  if ((!t1931) && u_is_con(s1930, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1930, 2);
    t1931 = v_y;
  }
  if (!t1931) {
    u_no_case(s1930);
  }
  return t1931;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s1932 = v_x;
  u_value t1933 = NULL;
  if ((!t1933) && u_is_con(s1932, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1932, 3);
    t1933 = v_y;
  }
  if (!t1933) {
    u_no_case(s1932);
  }
  return t1933;
}

u_value f_compile_46_Globals_46_procs_1(u_value v_x) {
  u_value s1934 = v_x;
  u_value t1935 = NULL;
  if ((!t1935) && u_is_con(s1934, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1934, 4);
    t1935 = v_y;
  }
  if (!t1935) {
    u_no_case(s1934);
  }
  return t1935;
}

u_value f_compile_46_Globals_46_funs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1936 = v_x;
  u_value t1937 = NULL;
  if ((!t1937) && u_is_con(s1936, &n_compile_46_Globals)) {
    u_value v_a1 = u_field(s1936, 1);
    u_value v_a2 = u_field(s1936, 2);
    u_value v_a3 = u_field(s1936, 3);
    u_value v_a4 = u_field(s1936, 4);
    t1937 = u_mk_con(&n_compile_46_Globals, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1937) {
    u_no_case(s1936);
  }
  return t1937;
}

u_value f_compile_46_Globals_46_constrs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1938 = v_x;
  u_value t1939 = NULL;
  if ((!t1939) && u_is_con(s1938, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1938, 0);
    u_value v_a2 = u_field(s1938, 2);
    u_value v_a3 = u_field(s1938, 3);
    u_value v_a4 = u_field(s1938, 4);
    t1939 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t1939) {
    u_no_case(s1938);
  }
  return t1939;
}

u_value f_compile_46_Globals_46_arities_58__61__2(u_value v_x, u_value v_y) {
  u_value s1940 = v_x;
  u_value t1941 = NULL;
  if ((!t1941) && u_is_con(s1940, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1940, 0);
    u_value v_a1 = u_field(s1940, 1);
    u_value v_a3 = u_field(s1940, 3);
    u_value v_a4 = u_field(s1940, 4);
    t1941 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t1941) {
    u_no_case(s1940);
  }
  return t1941;
}

u_value f_compile_46_Globals_46_fields_58__61__2(u_value v_x, u_value v_y) {
  u_value s1942 = v_x;
  u_value t1943 = NULL;
  if ((!t1943) && u_is_con(s1942, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1942, 0);
    u_value v_a1 = u_field(s1942, 1);
    u_value v_a2 = u_field(s1942, 2);
    u_value v_a4 = u_field(s1942, 4);
    t1943 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t1943) {
    u_no_case(s1942);
  }
  return t1943;
}

u_value f_compile_46_Globals_46_procs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1944 = v_x;
  u_value t1945 = NULL;
  if ((!t1945) && u_is_con(s1944, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1944, 0);
    u_value v_a1 = u_field(s1944, 1);
    u_value v_a2 = u_field(s1944, 2);
    u_value v_a3 = u_field(s1944, 3);
    t1945 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t1945) {
    u_no_case(s1944);
  }
  return t1945;
}

u_value f_compile_46_Gen_46_count_1(u_value v_x) {
  u_value s1946 = v_x;
  u_value t1947 = NULL;
  if ((!t1947) && u_is_con(s1946, &n_compile_46_Gen)) {
    u_value v_y = u_field(s1946, 0);
    t1947 = v_y;
  }
  if (!t1947) {
    u_no_case(s1946);
  }
  return t1947;
}

u_value f_compile_46_Gen_46_lifted_1(u_value v_x) {
  u_value s1948 = v_x;
  u_value t1949 = NULL;
  if ((!t1949) && u_is_con(s1948, &n_compile_46_Gen)) {
    u_value v_y = u_field(s1948, 1);
    t1949 = v_y;
  }
  if (!t1949) {
    u_no_case(s1948);
  }
  return t1949;
}

u_value f_compile_46_Gen_46_count_58__61__2(u_value v_x, u_value v_y) {
  u_value s1950 = v_x;
  u_value t1951 = NULL;
  if ((!t1951) && u_is_con(s1950, &n_compile_46_Gen)) {
    u_value v_a1 = u_field(s1950, 1);
    t1951 = u_mk_con(&n_compile_46_Gen, 2, v_y, v_a1);
  }
  if (!t1951) {
    u_no_case(s1950);
  }
  return t1951;
}

u_value f_compile_46_Gen_46_lifted_58__61__2(u_value v_x, u_value v_y) {
  u_value s1952 = v_x;
  u_value t1953 = NULL;
  if ((!t1953) && u_is_con(s1952, &n_compile_46_Gen)) {
    u_value v_a0 = u_field(s1952, 0);
    t1953 = u_mk_con(&n_compile_46_Gen, 2, v_a0, v_y);
  }
  if (!t1953) {
    u_no_case(s1952);
  }
  return t1953;
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
  static struct u_object l_u_value_1954 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1954);
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
  static struct u_object l_u_value_1955 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1955));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t1956 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1957 = &t1956;
  u_value l1958 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l1958, &n_list_46_Cons)) {
    u_value v_c = u_field(l1958, 0);
    if (u_truth(u_mk_bool(1))) {
      e1957 = u_append(e1957, f_compile_46_mangle_char_1(v_c));
    }
    l1958 = u_field(l1958, 1);
  }
  return u_string(f_list_46_concat_1(t1956));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t1959 = u_is_letter(v_c);
  if (!u_truth(t1959)) {
    t1959 = u_is_digit(v_c);
  }
  u_value t1960 = t1959;
  if (!u_truth(t1960)) {
    t1960 = u_eq(v_c, u_mk_char(95));
  }
  u_value t1963;
  if (u_truth(t1960)) {
    t1963 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___1961 = U_LIT("_");
    static struct u_object l___1962 = U_LIT("_");
    t1963 = u_chars(u_concat(&l___1961, u_concat(u_str_show(u_ord(v_c)), &l___1962)));
  }
  return t1963;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__1964 = U_LIT("f_");
  static struct u_object l___1965 = U_LIT("_");
  return u_concat(&l_f__1964, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___1965, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__1966 = U_LIT("p_");
  return u_concat(&l_p__1966, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__1967 = U_LIT("v_");
  return u_concat(&l_v__1967, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__1968 = U_LIT("r_");
  return u_concat(&l_r__1968, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__1969 = U_LIT("+");
  static struct u_object l_u_add_1970 = U_LIT("u_add");
  static struct u_object l__45__1971 = U_LIT("-");
  static struct u_object l_u_sub_1972 = U_LIT("u_sub");
  static struct u_object l__42__1973 = U_LIT("*");
  static struct u_object l_u_mul_1974 = U_LIT("u_mul");
  static struct u_object l__47__1975 = U_LIT("/");
  static struct u_object l_u_div_1976 = U_LIT("u_div");
  static struct u_object l__37__1977 = U_LIT("%");
  static struct u_object l_u_mod_1978 = U_LIT("u_mod");
  static struct u_object l__60__1979 = U_LIT("<");
  static struct u_object l_u_lt_1980 = U_LIT("u_lt");
  static struct u_object l__61__61__1981 = U_LIT("==");
  static struct u_object l_u_eq_1982 = U_LIT("u_eq");
  static struct u_object l_compare_1983 = U_LIT("compare");
  static struct u_object l_u_compare_1984 = U_LIT("u_compare");
  static struct u_object l_chars_1985 = U_LIT("chars");
  static struct u_object l_u_chars_1986 = U_LIT("u_chars");
  static struct u_object l_string_1987 = U_LIT("string");
  static struct u_object l_u_string_1988 = U_LIT("u_string");
  static struct u_object l_ord_1989 = U_LIT("ord");
  static struct u_object l_u_ord_1990 = U_LIT("u_ord");
  static struct u_object l_chr_1991 = U_LIT("chr");
  static struct u_object l_u_chr_1992 = U_LIT("u_chr");
  static struct u_object l__94__1993 = U_LIT("^");
  static struct u_object l_u_concat_1994 = U_LIT("u_concat");
  static struct u_object l_str_size_1995 = U_LIT("str_size");
  static struct u_object l_u_str_size_1996 = U_LIT("u_str_size");
  static struct u_object l_str_join_1997 = U_LIT("str_join");
  static struct u_object l_u_str_join_1998 = U_LIT("u_str_join");
  static struct u_object l_str_slice_1999 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_2000 = U_LIT("u_str_slice");
  static struct u_object l_str_split_2001 = U_LIT("str_split");
  static struct u_object l_u_str_split_2002 = U_LIT("u_str_split");
  static struct u_object l_str_get_2003 = U_LIT("str_get");
  static struct u_object l_u_str_get_2004 = U_LIT("u_str_get");
  static struct u_object l_str_contains_2005 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_2006 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_2007 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_2008 = U_LIT("u_str_compare");
  static struct u_object l_str_show_2009 = U_LIT("str_show");
  static struct u_object l_u_str_show_2010 = U_LIT("u_str_show");
  static struct u_object l_str_quote_2011 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_2012 = U_LIT("u_str_quote");
  static struct u_object l_str_number_2013 = U_LIT("str_number");
  static struct u_object l_u_str_number_2014 = U_LIT("u_str_number");
  static struct u_object l_is_digit_2015 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_2016 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_2017 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_2018 = U_LIT("u_is_letter");
  static struct u_object l_is_space_2019 = U_LIT("is_space");
  static struct u_object l_u_is_space_2020 = U_LIT("u_is_space");
  u_pop(handler);
  cache = f_map_46_put_all_2(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__1969), u_mk_tuple(2, &l_u_add_1970, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__1971), u_mk_tuple(2, &l_u_sub_1972, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__1973), u_mk_tuple(2, &l_u_mul_1974, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__1975), u_mk_tuple(2, &l_u_div_1976, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__1977), u_mk_tuple(2, &l_u_mod_1978, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__1979), u_mk_tuple(2, &l_u_lt_1980, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__1981), u_mk_tuple(2, &l_u_eq_1982, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_1983), u_mk_tuple(2, &l_u_compare_1984, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_1985), u_mk_tuple(2, &l_u_chars_1986, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_1987), u_mk_tuple(2, &l_u_string_1988, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_1989), u_mk_tuple(2, &l_u_ord_1990, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_1991), u_mk_tuple(2, &l_u_chr_1992, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__1993), u_mk_tuple(2, &l_u_concat_1994, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_1995), u_mk_tuple(2, &l_u_str_size_1996, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_1997), u_mk_tuple(2, &l_u_str_join_1998, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_1999), u_mk_tuple(2, &l_u_str_slice_2000, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_2001), u_mk_tuple(2, &l_u_str_split_2002, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_2003), u_mk_tuple(2, &l_u_str_get_2004, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_2005), u_mk_tuple(2, &l_u_str_contains_2006, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_2007), u_mk_tuple(2, &l_u_str_compare_2008, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_2009), u_mk_tuple(2, &l_u_str_show_2010, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_2011), u_mk_tuple(2, &l_u_str_quote_2012, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_2013), u_mk_tuple(2, &l_u_str_number_2014, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_2015), u_mk_tuple(2, &l_u_is_digit_2016, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_2017), u_mk_tuple(2, &l_u_is_letter_2018, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_2019), u_mk_tuple(2, &l_u_is_space_2020, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))), f_map_46_empty_0());
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__2021 = U_LIT("&");
  static struct u_object l_n__2022 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__2021, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__2022, f_compile_46_mangle_1(v_n))));
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
  u_value s2023 = v_n;
  u_value t2024 = NULL;
  if ((!t2024) && (u_is_con(s2023, &n_syntax_46_Name) && u_is_con(u_field(s2023, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s2023, 0), 0);
    u_value v_b = u_field(s2023, 1);
    static struct u_object l_const_32_struct_32_u_n_2025 = U_LIT("const struct u_name");
    static struct u_object l_n__2026 = U_LIT("n_");
    t2024 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2025), u_concat(&l_n__2026, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t2024) && (u_is_con(s2023, &n_syntax_46_Name) && u_is_con(u_field(s2023, 0), &n_option_46_None))) {
    u_value v_b = u_field(s2023, 1);
    static struct u_object l_const_32_struct_32_u_n_2027 = U_LIT("const struct u_name");
    static struct u_object l_n__2028 = U_LIT("n_");
    static struct u_object l_NULL_2029 = U_LIT("NULL");
    t2024 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2027), u_concat(&l_n__2028, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2029), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t2024) {
    u_no_case(s2023);
  }
  return t2024;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s2030 = v_cs;
  u_value t2031 = NULL;
  if ((!t2031) && u_is_con(s2030, &n_list_46_Nil)) {
    t2031 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2031) && (u_is_con(s2030, &n_list_46_Cons) && u_is_con(u_field(s2030, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s2030, 0), 2);
    u_value v_rest = u_field(s2030, 1);
    u_value t2032 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2033 = &t2032;
    u_value l2034 = v_ks;
    while (u_is_con(l2034, &n_list_46_Cons)) {
      u_value v_k = u_field(l2034, 0);
      if (u_truth(u_mk_bool(1))) {
        e2033 = u_append(e2033, f_syntax_46_Constr_46_name_1(v_k));
      }
      l2034 = u_field(l2034, 1);
    }
    t2031 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t2032, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t2031) && u_is_con(s2030, &n_list_46_Cons)) {
    u_value v_rest = u_field(s2030, 1);
    t2031 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t2031) {
    u_no_case(s2030);
  }
  return t2031;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s2035 = v_ks;
  u_value t2036 = NULL;
  if ((!t2036) && u_is_con(s2035, &n_list_46_Nil)) {
    t2036 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2036) && u_is_con(s2035, &n_list_46_Cons)) {
    u_value v_k = u_field(s2035, 0);
    u_value v_rest = u_field(s2035, 1);
    u_value t2037;
    if (u_truth(f_set_46_member_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t2037 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t2037 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t2036 = t2037;
  }
  if (!t2036) {
    u_no_case(s2035);
  }
  return t2036;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t2038 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2039 = &t2038;
  u_value l2040 = v_xs;
  while (u_is_con(l2040, &n_list_46_Cons)) {
    u_value v_x = u_field(l2040, 0);
    if (u_truth(u_mk_bool(1))) {
      e2039 = u_append(e2039, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l2040 = u_field(l2040, 1);
  }
  return t2038;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__2041 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__2041, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s2042 = f_list_46_lookup_2(v_x, v_env);
  u_value t2043 = NULL;
  if ((!t2043) && u_is_con(s2042, &n_option_46_Some)) {
    u_value v_c = u_field(s2042, 0);
    t2043 = v_c;
  }
  if (!t2043) {
    t2043 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t2043) {
    u_no_case(s2042);
  }
  return t2043;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s2044 = v_c;
  u_value t2045 = NULL;
  if ((!t2045) && (u_is_con(s2044, &n_c_46_CeUnary) && u_equals(u_field(s2044, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s2044, 1);
    t2045 = v_p;
  }
  if (!t2045) {
    static struct u_object l__38__2046 = U_LIT("&");
    t2045 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2046, v_c);
  }
  if (!t2045) {
    u_no_case(s2044);
  }
  return t2045;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s2047 = v_t;
  u_value t2048 = NULL;
  if ((!t2048) && (u_is_con(s2047, &n_syntax_46_TSort) && u_is_con(u_field(s2047, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s2047, 0);
    t2048 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t2048) && u_is_con(s2047, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s2047, 0);
    u_value v_args = u_field(s2047, 1);
    static struct u_object l__91__2049 = U_LIT("[");
    static struct u_object l__44__32__2050 = U_LIT(", ");
    u_value t2051 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2052 = &t2051;
    u_value l2053 = v_args;
    while (u_is_con(l2053, &n_list_46_Cons)) {
      u_value v_a = u_field(l2053, 0);
      if (u_truth(u_mk_bool(1))) {
        e2052 = u_append(e2052, f_compile_46_type_key_1(v_a));
      }
      l2053 = u_field(l2053, 1);
    }
    static struct u_object l__93__2054 = U_LIT("]");
    t2048 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__2049, u_concat(u_str_join(&l__44__32__2050, t2051), &l__93__2054)));
  }
  if ((!t2048) && u_is_con(s2047, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s2047, 0);
    u_value v_r = u_field(s2047, 1);
    static struct u_object l__40__2055 = U_LIT("(");
    static struct u_object l__44__32__2056 = U_LIT(", ");
    u_value t2057 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2058 = &t2057;
    u_value l2059 = v_args;
    while (u_is_con(l2059, &n_list_46_Cons)) {
      u_value v_a = u_field(l2059, 0);
      if (u_truth(u_mk_bool(1))) {
        e2058 = u_append(e2058, f_compile_46_type_key_1(v_a));
      }
      l2059 = u_field(l2059, 1);
    }
    static struct u_object l__41__32__45__62__32__2060 = U_LIT(") -> ");
    t2048 = u_concat(&l__40__2055, u_concat(u_str_join(&l__44__32__2056, t2057), u_concat(&l__41__32__45__62__32__2060, f_compile_46_type_key_1(v_r))));
  }
  if ((!t2048) && u_is_con(s2047, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s2047, 0);
    u_value v_os = u_field(s2047, 1);
    u_value v_rs = u_field(s2047, 2);
    static struct u_object l__40_in_32__2061 = U_LIT("(in ");
    static struct u_object l__44__32__2062 = U_LIT(", ");
    u_value t2063 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2064 = &t2063;
    u_value l2065 = v_is;
    while (u_is_con(l2065, &n_list_46_Cons)) {
      u_value v_a = u_field(l2065, 0);
      if (u_truth(u_mk_bool(1))) {
        e2064 = u_append(e2064, f_compile_46_type_key_1(v_a));
      }
      l2065 = u_field(l2065, 1);
    }
    static struct u_object l__44__32_out_32__2066 = U_LIT(", out ");
    static struct u_object l__44__32__2067 = U_LIT(", ");
    u_value t2068 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2069 = &t2068;
    u_value l2070 = v_os;
    while (u_is_con(l2070, &n_list_46_Cons)) {
      u_value v_a = u_field(l2070, 0);
      if (u_truth(u_mk_bool(1))) {
        e2069 = u_append(e2069, f_compile_46_type_key_1(v_a));
      }
      l2070 = u_field(l2070, 1);
    }
    static struct u_object l__44__32_ref_32__2071 = U_LIT(", ref ");
    static struct u_object l__44__32__2072 = U_LIT(", ");
    u_value t2073 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2074 = &t2073;
    u_value l2075 = v_rs;
    while (u_is_con(l2075, &n_list_46_Cons)) {
      u_value v_a = u_field(l2075, 0);
      if (u_truth(u_mk_bool(1))) {
        e2074 = u_append(e2074, f_compile_46_type_key_1(v_a));
      }
      l2075 = u_field(l2075, 1);
    }
    static struct u_object l__41__2076 = U_LIT(")");
    t2048 = u_concat(&l__40_in_32__2061, u_concat(u_str_join(&l__44__32__2062, t2063), u_concat(&l__44__32_out_32__2066, u_concat(u_str_join(&l__44__32__2067, t2068), u_concat(&l__44__32_ref_32__2071, u_concat(u_str_join(&l__44__32__2072, t2073), &l__41__2076))))));
  }
  if ((!t2048) && u_is_con(s2047, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s2047, 0);
    static struct u_object l__40__2077 = U_LIT("(");
    static struct u_object l__32__42__32__2078 = U_LIT(" * ");
    u_value t2079 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2080 = &t2079;
    u_value l2081 = v_ts;
    while (u_is_con(l2081, &n_list_46_Cons)) {
      u_value v_a = u_field(l2081, 0);
      if (u_truth(u_mk_bool(1))) {
        e2080 = u_append(e2080, f_compile_46_type_key_1(v_a));
      }
      l2081 = u_field(l2081, 1);
    }
    static struct u_object l__41__2082 = U_LIT(")");
    t2048 = u_concat(&l__40__2077, u_concat(u_str_join(&l__32__42__32__2078, t2079), &l__41__2082));
  }
  if (!t2048) {
    static struct u_object l__63__2083 = U_LIT("?");
    t2048 = &l__63__2083;
  }
  if (!t2048) {
    u_no_case(s2047);
  }
  return t2048;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t2084 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2085 = &t2084;
  u_value l2086 = v_cs;
  while (u_is_con(l2086, &n_list_46_Cons)) {
    u_value v_c = u_field(l2086, 0);
    if (u_truth(u_mk_bool(1))) {
      e2085 = u_append(e2085, f_compile_46_fun_arities_1(v_c));
    }
    l2086 = u_field(l2086, 1);
  }
  u_value t2087 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2088 = &t2087;
  u_value l2089 = v_cs;
  while (u_is_con(l2089, &n_list_46_Cons)) {
    u_value v_c = u_field(l2089, 0);
    if (u_truth(u_mk_bool(1))) {
      e2088 = u_append(e2088, f_compile_46_constr_arities_1(v_c));
    }
    l2089 = u_field(l2089, 1);
  }
  u_value t2094 = f_list_46_concat_1(t2084);
  u_value t2095 = f_list_46_concat_1(t2087);
  u_value t2093;
  {
    u_value v_fs = t2094;
    u_value v_ks = t2095;
    u_value t2090 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2091 = &t2090;
    u_value l2092 = v_cs;
    while (u_is_con(l2092, &n_list_46_Cons)) {
      u_value v_c = u_field(l2092, 0);
      if (u_truth(u_mk_bool(1))) {
        e2091 = u_append(e2091, f_compile_46_proc_arities_1(v_c));
      }
      l2092 = u_field(l2092, 1);
    }
    t2093 = u_mk_con(&n_compile_46_Globals, 5, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()), f_map_46_put_all_2(f_list_46_concat_1(t2090), f_map_46_empty_0()));
  }
  return t2093;
}

u_value f_compile_46_proc_arities_1(u_value v_c) {
  u_value s2096 = v_c;
  u_value t2097 = NULL;
  if ((!t2097) && u_is_con(s2096, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s2096, 0);
    u_value v_ins = u_field(s2096, 2);
    u_value v_outs = u_field(s2096, 3);
    u_value v_refs = u_field(s2096, 4);
    t2097 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, f_list_46_length_1(v_ins), u_add(f_list_46_length_1(v_outs), f_list_46_length_1(v_refs)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2097) {
    t2097 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2097) {
    u_no_case(s2096);
  }
  return t2097;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s2098 = v_fs;
  u_value t2099 = NULL;
  if ((!t2099) && u_is_con(s2098, &n_list_46_Nil)) {
    t2099 = v_m;
  }
  if ((!t2099) && u_is_con(s2098, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s2098, 0), 0);
    u_value v_k = u_field(u_field(s2098, 0), 1);
    u_value v_rest = u_field(s2098, 1);
    u_value s2100 = f_map_46_get_2(v_f, v_m);
    u_value t2101 = NULL;
    if ((!t2101) && u_is_con(s2100, &n_option_46_Some)) {
      u_value v_ks = u_field(s2100, 0);
      t2101 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t2101) && u_is_con(s2100, &n_option_46_None)) {
      t2101 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t2101) {
      u_no_case(s2100);
    }
    t2099 = t2101;
  }
  if (!t2099) {
    u_no_case(s2098);
  }
  return t2099;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s2102 = v_gl;
  u_value t2103 = NULL;
  if ((!t2103) && u_is_con(s2102, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s2102, 2);
    u_value s2104 = f_map_46_get_2(v_n, v_fm);
    u_value t2105 = NULL;
    if ((!t2105) && u_is_con(s2104, &n_option_46_Some)) {
      u_value v_ks = u_field(s2104, 0);
      t2105 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t2105) && u_is_con(s2104, &n_option_46_None)) {
      t2105 = u_mk_bool(0);
    }
    if (!t2105) {
      u_no_case(s2104);
    }
    t2103 = t2105;
  }
  if (!t2103) {
    u_no_case(s2102);
  }
  return t2103;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s2106 = v_gl;
  u_value t2107 = NULL;
  if ((!t2107) && u_is_con(s2106, &n_compile_46_Globals)) {
    u_value v_km = u_field(s2106, 3);
    t2107 = f_map_46_get_2(v_n, v_km);
  }
  if (!t2107) {
    u_no_case(s2106);
  }
  return t2107;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s2108 = v_c;
  u_value t2109 = NULL;
  if ((!t2109) && u_is_con(s2108, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s2108, 0);
    u_value v_xss = u_field(s2108, 2);
    t2109 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2109) {
    t2109 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2109) {
    u_no_case(s2108);
  }
  return t2109;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s2110 = v_c;
  u_value t2111 = NULL;
  if ((!t2111) && u_is_con(s2110, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s2110, 2);
    u_value t2112 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2113 = &t2112;
    u_value l2114 = v_ks;
    while (u_is_con(l2114, &n_list_46_Cons)) {
      u_value v_k = u_field(l2114, 0);
      if (u_truth(u_mk_bool(1))) {
        e2113 = u_append(e2113, f_compile_46_constr_arity_1(v_k));
      }
      l2114 = u_field(l2114, 1);
    }
    t2111 = t2112;
  }
  if (!t2111) {
    t2111 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2111) {
    u_no_case(s2110);
  }
  return t2111;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s2115 = v_k;
  u_value t2116 = NULL;
  if ((!t2116) && u_is_con(s2115, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s2115, 0);
    u_value v_ts = u_field(s2115, 1);
    t2116 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t2116) {
    u_no_case(s2115);
  }
  return t2116;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s2117 = v_xs;
  u_value t2118 = NULL;
  if ((!t2118) && u_is_con(s2117, &n_list_46_Nil)) {
    t2118 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2118) && u_is_con(s2117, &n_list_46_Cons)) {
    u_value v_x = u_field(s2117, 0);
    u_value v_rest = u_field(s2117, 1);
    t2118 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t2118) {
    u_no_case(s2117);
  }
  return t2118;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_2119 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_2119, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_2120 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_2120, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s2121 = v_cs;
  u_value t2122 = NULL;
  if ((!t2122) && u_is_con(s2121, &n_list_46_Nil)) {
    t2122 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t2122) && (u_is_con(s2121, &n_list_46_Cons) && u_is_con(u_field(s2121, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s2121, 0);
    t2122 = v_d;
  }
  if ((!t2122) && u_is_con(s2121, &n_list_46_Cons)) {
    u_value v_d = u_field(s2121, 0);
    u_value v_rest = u_field(s2121, 1);
    static struct u_object l__38__38__2123 = U_LIT("&&");
    t2122 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__2123, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t2122) {
    u_no_case(s2121);
  }
  return t2122;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s2124 = v_l;
  u_value t2125 = NULL;
  if ((!t2125) && u_is_con(s2124, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_2126 = U_LIT("u_mk_tuple");
    t2125 = f_c_46_c_call_2(&l_u_mk_tuple_2126, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2125) && u_is_con(s2124, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s2124, 0);
    static struct u_object l_u_mk_bool_2127 = U_LIT("u_mk_bool");
    u_value t2128;
    if (u_truth(v_b)) {
      t2128 = u_mk_int(1);
    } else {
      t2128 = u_mk_int(0);
    }
    t2125 = f_c_46_c_call_2(&l_u_mk_bool_2127, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t2128), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2125) && u_is_con(s2124, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s2124, 0);
    u_value t2129 = u_lt(u_mk_int_str("-1000000000000000000"), v_n);
    if (u_truth(t2129)) {
      t2129 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t2132;
    if (u_truth(t2129)) {
      static struct u_object l_u_mk_int_2130 = U_LIT("u_mk_int");
      t2132 = f_c_46_c_call_2(&l_u_mk_int_2130, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_2131 = U_LIT("u_mk_int_str");
      t2132 = f_c_46_c_call_2(&l_u_mk_int_str_2131, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t2125 = t2132;
  }
  if ((!t2125) && u_is_con(s2124, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s2124, 0);
    static struct u_object l_u_mk_lit_2133 = U_LIT("u_mk_lit");
    t2125 = f_c_46_c_call_2(&l_u_mk_lit_2133, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2125) && u_is_con(s2124, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s2124, 0);
    static struct u_object l_u_mk_char_2134 = U_LIT("u_mk_char");
    t2125 = f_c_46_c_call_2(&l_u_mk_char_2134, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2125) {
    u_no_case(s2124);
  }
  return t2125;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s2135 = v_p;
  u_value t2136 = NULL;
  if ((!t2136) && u_is_con(s2135, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s2135, 0);
    static struct u_object l_u_equals_2137 = U_LIT("u_equals");
    t2136 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_2137, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2136) && u_is_con(s2135, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s2135, 0);
    u_value v_ps = u_field(s2135, 1);
    static struct u_object l_u_is_con_2138 = U_LIT("u_is_con");
    t2136 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_2138, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t2136) && u_is_con(s2135, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2135, 0);
    t2136 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2136) {
    t2136 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2136) {
    u_no_case(s2135);
  }
  return t2136;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s2139 = v_ps;
  u_value t2140 = NULL;
  if ((!t2140) && u_is_con(s2139, &n_list_46_Nil)) {
    t2140 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2140) && u_is_con(s2139, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2139, 0), 0);
    u_value v_i = u_field(u_field(s2139, 0), 1);
    u_value v_rest = u_field(s2139, 1);
    t2140 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t2140) {
    u_no_case(s2139);
  }
  return t2140;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s2141 = v_p;
  u_value t2142 = NULL;
  if ((!t2142) && (u_is_con(s2141, &n_syntax_46_PVar) && u_is_con(u_field(s2141, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2141, 0), 0);
    t2142 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2142) && u_is_con(s2141, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2141, 1);
    t2142 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t2142) && u_is_con(s2141, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2141, 0);
    t2142 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2142) {
    t2142 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2142) {
    u_no_case(s2141);
  }
  return t2142;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s2143 = v_ps;
  u_value t2144 = NULL;
  if ((!t2144) && u_is_con(s2143, &n_list_46_Nil)) {
    t2144 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2144) && u_is_con(s2143, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2143, 0), 0);
    u_value v_i = u_field(u_field(s2143, 0), 1);
    u_value v_rest = u_field(s2143, 1);
    t2144 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t2144) {
    u_no_case(s2143);
  }
  return t2144;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s2145 = v_p;
  u_value t2146 = NULL;
  if ((!t2146) && (u_is_con(s2145, &n_syntax_46_PVar) && u_is_con(u_field(s2145, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2145, 0), 0);
    t2146 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2146) && u_is_con(s2145, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2145, 1);
    u_value t2147 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2148 = &t2147;
    u_value l2149 = v_ps;
    while (u_is_con(l2149, &n_list_46_Cons)) {
      u_value v_q = u_field(l2149, 0);
      if (u_truth(u_mk_bool(1))) {
        e2148 = u_append(e2148, f_compile_46_pattern_names_1(v_q));
      }
      l2149 = u_field(l2149, 1);
    }
    t2146 = f_list_46_concat_1(t2147);
  }
  if ((!t2146) && u_is_con(s2145, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2145, 0);
    u_value t2150 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2151 = &t2150;
    u_value l2152 = v_ps;
    while (u_is_con(l2152, &n_list_46_Cons)) {
      u_value v_q = u_field(l2152, 0);
      if (u_truth(u_mk_bool(1))) {
        e2151 = u_append(e2151, f_compile_46_pattern_names_1(v_q));
      }
      l2152 = u_field(l2152, 1);
    }
    t2146 = f_list_46_concat_1(t2150);
  }
  if (!t2146) {
    t2146 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2146) {
    u_no_case(s2145);
  }
  return t2146;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s2153 = v_e;
  u_value t2154 = NULL;
  if ((!t2154) && u_is_con(s2153, &n_syntax_46_ELit)) {
    t2154 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2154) && (u_is_con(s2153, &n_syntax_46_EVar) && u_is_con(u_field(s2153, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2153, 0), 0);
    u_value t2155;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t2155 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t2155 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t2154 = t2155;
  }
  if ((!t2154) && u_is_con(s2153, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s2153, 0);
    u_value v_args = u_field(s2153, 1);
    u_value t2156 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2157 = &t2156;
    u_value l2158 = v_args;
    while (u_is_con(l2158, &n_list_46_Cons)) {
      u_value v_a = u_field(l2158, 0);
      if (u_truth(u_mk_bool(1))) {
        e2157 = u_append(e2157, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2158 = u_field(l2158, 1);
    }
    t2154 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t2156));
  }
  if ((!t2154) && u_is_con(s2153, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s2153, 0);
    u_value v_b = u_field(s2153, 1);
    t2154 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t2154) && u_is_con(s2153, &n_syntax_46_EProc)) {
    u_value v_is = u_field(s2153, 0);
    u_value v_os = u_field(s2153, 1);
    u_value v_rs = u_field(s2153, 2);
    u_value v_b = u_field(s2153, 3);
    t2154 = f_compile_46_free_in_stmt_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), v_bound));
  }
  if ((!t2154) && u_is_con(s2153, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s2153, 0);
    u_value v_b = u_field(s2153, 1);
    u_value t2159 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2160 = &t2159;
    u_value l2161 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l2161, &n_list_46_Cons)) {
      u_value v_a = u_field(l2161, 0);
      if (u_truth(u_mk_bool(1))) {
        e2160 = u_append(e2160, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2161 = u_field(l2161, 1);
    }
    t2154 = f_list_46__43__43__2(f_list_46_concat_1(t2159), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t2154) && u_is_con(s2153, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s2153, 0);
    u_value v_l = u_field(s2153, 1);
    u_value v_r = u_field(s2153, 2);
    t2154 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t2154) && u_is_con(s2153, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s2153, 0);
    u_value v_cs = u_field(s2153, 1);
    u_value t2162 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2163 = &t2162;
    u_value l2164 = v_cs;
    while (u_is_con(l2164, &n_list_46_Cons)) {
      u_value v_b = u_field(l2164, 0);
      if (u_truth(u_mk_bool(1))) {
        e2163 = u_append(e2163, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l2164 = u_field(l2164, 1);
    }
    t2154 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2162));
  }
  if ((!t2154) && u_is_con(s2153, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s2153, 0);
    u_value v_xs = u_field(s2153, 1);
    u_value v_ls = u_field(s2153, 2);
    u_value v_c = u_field(s2153, 3);
    u_value t2165 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2166 = &t2165;
    u_value l2167 = v_ls;
    while (u_is_con(l2167, &n_list_46_Cons)) {
      u_value v_l = u_field(l2167, 0);
      if (u_truth(u_mk_bool(1))) {
        e2166 = u_append(e2166, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2167 = u_field(l2167, 1);
    }
    t2154 = f_list_46__43__43__2(f_list_46_concat_1(t2165), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t2154) && u_is_con(s2153, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s2153, 0);
    u_value t2168 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2169 = &t2168;
    u_value l2170 = v_es;
    while (u_is_con(l2170, &n_list_46_Cons)) {
      u_value v_a = u_field(l2170, 0);
      if (u_truth(u_mk_bool(1))) {
        e2169 = u_append(e2169, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2170 = u_field(l2170, 1);
    }
    t2154 = f_list_46_concat_1(t2168);
  }
  if ((!t2154) && u_is_con(s2153, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s2153, 0);
    t2154 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if (!t2154) {
    u_no_case(s2153);
  }
  return t2154;
}

u_value f_compile_46_free_in_stmt_2(u_value v_x, u_value v_bound) {
  u_value s2171 = v_x;
  u_value t2172 = NULL;
  if ((!t2172) && u_is_con(s2171, &n_syntax_46_SSkip)) {
    t2172 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2172) && u_is_con(s2171, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s2171, 0);
    u_value t2173 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2174 = &t2173;
    u_value l2175 = f_syntax_46_names_1(f_list_46_firsts_1(v_ps));
    while (u_is_con(l2175, &n_list_46_Cons)) {
      u_value v_y = u_field(l2175, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2174 = u_append(e2174, v_y);
      }
      l2175 = u_field(l2175, 1);
    }
    u_value t2176 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2177 = &t2176;
    u_value l2178 = f_list_46_seconds_1(v_ps);
    while (u_is_con(l2178, &n_list_46_Cons)) {
      u_value v_a = u_field(l2178, 0);
      if (u_truth(u_mk_bool(1))) {
        e2177 = u_append(e2177, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2178 = u_field(l2178, 1);
    }
    t2172 = f_list_46__43__43__2(t2173, f_list_46_concat_1(t2176));
  }
  if ((!t2172) && u_is_con(s2171, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s2171, 0);
    u_value v_ys = u_field(s2171, 1);
    u_value t2179 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2180 = &t2179;
    u_value l2181 = v_ys;
    while (u_is_con(l2181, &n_list_46_Cons)) {
      u_value v_y = u_field(l2181, 0);
      if (u_truth(u_mk_bool(1))) {
        e2180 = u_append(e2180, f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2181 = u_field(l2181, 1);
    }
    t2172 = f_list_46_concat_1(t2179);
  }
  if ((!t2172) && u_is_con(s2171, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s2171, 0);
    u_value v_l = u_field(s2171, 1);
    u_value v_r = u_field(s2171, 2);
    t2172 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_l, v_bound), f_compile_46_free_in_stmt_2(v_r, v_bound)));
  }
  if ((!t2172) && u_is_con(s2171, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s2171, 0);
    u_value v_b = u_field(s2171, 1);
    t2172 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_compile_46_free_in_stmt_2(v_b, v_bound));
  }
  if ((!t2172) && u_is_con(s2171, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s2171, 0);
    u_value v_ins = u_field(s2171, 1);
    u_value v_outs = u_field(s2171, 2);
    u_value v_refs = u_field(s2171, 3);
    u_value t2182 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2183 = &t2182;
    u_value l2184 = u_mk_con(&n_list_46_Cons, 2, v_p, f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
    while (u_is_con(l2184, &n_list_46_Cons)) {
      u_value v_y = u_field(l2184, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2183 = u_append(e2183, v_y);
      }
      l2184 = u_field(l2184, 1);
    }
    u_value t2185 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2186 = &t2185;
    u_value l2187 = v_ins;
    while (u_is_con(l2187, &n_list_46_Cons)) {
      u_value v_a = u_field(l2187, 0);
      if (u_truth(u_mk_bool(1))) {
        e2186 = u_append(e2186, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2187 = u_field(l2187, 1);
    }
    t2172 = f_list_46__43__43__2(t2182, f_list_46_concat_1(t2185));
  }
  if ((!t2172) && u_is_con(s2171, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s2171, 0);
    u_value v_bs = u_field(s2171, 1);
    u_value t2188 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2189 = &t2188;
    u_value l2190 = v_bs;
    while (u_is_con(l2190, &n_list_46_Cons)) {
      u_value v_b = u_field(l2190, 0);
      if (u_truth(u_mk_bool(1))) {
        e2189 = u_append(e2189, f_compile_46_free_in_stmt_branch_2(v_b, v_bound));
      }
      l2190 = u_field(l2190, 1);
    }
    t2172 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2188));
  }
  if ((!t2172) && u_is_con(s2171, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s2171, 0);
    t2172 = f_compile_46_free_vars_2(v_c, v_bound);
  }
  if ((!t2172) && u_is_con(s2171, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s2171, 0);
    t2172 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if ((!t2172) && u_is_con(s2171, &n_syntax_46_STry)) {
    u_value v_b = u_field(s2171, 0);
    u_value v_hs = u_field(s2171, 1);
    u_value t2191 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2192 = &t2191;
    u_value l2193 = v_hs;
    while (u_is_con(l2193, &n_list_46_Cons)) {
      u_value v_h = u_field(l2193, 0);
      if (u_truth(u_mk_bool(1))) {
        e2192 = u_append(e2192, f_compile_46_free_in_handler_2(v_h, v_bound));
      }
      l2193 = u_field(l2193, 1);
    }
    t2172 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_b, v_bound), f_list_46_concat_1(t2191));
  }
  if (!t2172) {
    u_no_case(s2171);
  }
  return t2172;
}

u_value f_compile_46_free_in_stmt_branch_2(u_value v_b, u_value v_bound) {
  u_value s2194 = v_b;
  u_value t2195 = NULL;
  if ((!t2195) && (u_is_con(s2194, &n_syntax_46_Branch) && u_is_con(u_field(s2194, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2194, 0);
    u_value v_g = u_field(u_field(s2194, 1), 0);
    u_value v_y = u_field(s2194, 2);
    t2195 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2195) && (u_is_con(s2194, &n_syntax_46_Branch) && u_is_con(u_field(s2194, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2194, 0);
    u_value v_y = u_field(s2194, 2);
    t2195 = f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2195) {
    u_no_case(s2194);
  }
  return t2195;
}

u_value f_compile_46_free_in_handler_2(u_value v_h, u_value v_bound) {
  u_value t2199 = f_list_46__43__43__2(f_compile_46_pattern_names_1(f_syntax_46_Handler_46_pat_1(v_h)), v_bound);
  u_value t2198;
  {
    u_value v_inner = t2199;
    u_value s2196 = f_syntax_46_Handler_46_guard_1(v_h);
    u_value t2197 = NULL;
    if ((!t2197) && u_is_con(s2196, &n_option_46_Some)) {
      u_value v_g = u_field(s2196, 0);
      t2197 = f_compile_46_free_vars_2(v_g, v_inner);
    }
    if ((!t2197) && u_is_con(s2196, &n_option_46_None)) {
      t2197 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t2197) {
      u_no_case(s2196);
    }
    t2198 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(f_syntax_46_Handler_46_body_1(v_h), v_inner), t2197);
  }
  return t2198;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s2200 = v_b;
  u_value t2201 = NULL;
  if ((!t2201) && (u_is_con(s2200, &n_syntax_46_Branch) && u_is_con(u_field(s2200, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2200, 0);
    u_value v_g = u_field(u_field(s2200, 1), 0);
    u_value v_e = u_field(s2200, 2);
    t2201 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2201) && (u_is_con(s2200, &n_syntax_46_Branch) && u_is_con(u_field(s2200, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2200, 0);
    u_value v_e = u_field(s2200, 2);
    t2201 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2201) {
    u_no_case(s2200);
  }
  return t2201;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s2202 = v_xss;
  u_value t2203 = NULL;
  if ((!t2203) && u_is_con(s2202, &n_list_46_Nil)) {
    t2203 = v_b;
  }
  if ((!t2203) && u_is_con(s2202, &n_list_46_Cons)) {
    u_value v_xs = u_field(s2202, 0);
    u_value v_rest = u_field(s2202, 1);
    t2203 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t2203) {
    u_no_case(s2202);
  }
  return t2203;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value t2204 = u_concat(v_stem, u_str_show(f_compile_46_Gen_46_count_1(*r_g)));
    u_value t2205 = f_compile_46_Gen_46_count_58__61__2(*r_g, u_add(f_compile_46_Gen_46_count_1(*r_g), u_mk_int(1)));
    *r_x = t2204;
    *r_g = t2205;
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value t2206 = f_compile_46_Gen_46_lifted_58__61__2(*r_g, f_list_46__43__43__2(f_compile_46_Gen_46_lifted_1(*r_g), u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
    *r_g = t2206;
  }
}

void p_compile_46_global_value(u_value v_gl, u_value v_n, u_value v_t, u_value *r_r) {
  {
    if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
      {
        static struct u_object l_u_mk_closure_2207 = U_LIT("u_mk_closure");
        static struct u_object l_w__2208 = U_LIT("w_");
        u_value t2209 = f_c_46_c_call_2(&l_u_mk_closure_2207, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__2208, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        *r_r = t2209;
      }
    } else {
      {
        u_value s2210 = f_compile_46_fields_count_2(v_gl, v_n);
        int d2211 = 0;
        if ((!d2211) && (u_is_con(s2210, &n_option_46_Some) && u_equals(u_field(s2210, 0), u_mk_int(0)))) {
          d2211 = 1;
          {
            static struct u_object l_u_mk_con_2212 = U_LIT("u_mk_con");
            u_value t2213 = f_c_46_c_call_2(&l_u_mk_con_2212, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t2213;
          }
        }
        if ((!d2211) && u_is_con(s2210, &n_option_46_Some)) {
          d2211 = 1;
          {
            static struct u_object l_u_mk_closure_2214 = U_LIT("u_mk_closure");
            static struct u_object l_wc__2215 = U_LIT("wc_");
            u_value t2216 = f_c_46_c_call_2(&l_u_mk_closure_2214, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__2215, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t2216;
          }
        }
        if ((!d2211) && u_is_con(s2210, &n_option_46_None)) {
          d2211 = 1;
          {
            u_value s2217 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
            int d2218 = 0;
            if ((!d2218) && u_is_con(s2217, &n_option_46_Some)) {
              u_value v_f = u_field(u_field(s2217, 0), 0);
              d2218 = 1;
              {
                static struct u_object l_u_mk_closure_2219 = U_LIT("u_mk_closure");
                static struct u_object l_w_2220 = U_LIT("w");
                u_value t2221 = f_c_46_c_call_2(&l_u_mk_closure_2219, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_2220, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
                *r_r = t2221;
              }
            }
            if ((!d2218) && u_is_con(s2217, &n_option_46_None)) {
              if (u_truth(f_map_46_has_2(v_n, f_compile_46_Globals_46_procs_1(v_gl)))) {
                d2218 = 1;
                {
                  static struct u_object l_u_mk_proc_2222 = U_LIT("u_mk_proc");
                  static struct u_object l_wp__2223 = U_LIT("wp_");
                  u_value t2224 = f_c_46_c_call_2(&l_u_mk_proc_2222, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wp__2223, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
                  *r_r = t2224;
                }
              }
            }
            if ((!d2218) && u_is_con(s2217, &n_option_46_None)) {
              d2218 = 1;
              {
                static struct u_object l_the_32_variable_32__2225 = U_LIT("the variable ");
                u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__2225, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
              }
            }
            if (!d2218) {
              u_no_case(s2217);
            }
          }
        }
        if (!d2211) {
          u_no_case(s2210);
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
    u_value s2226 = v_e;
    int d2227 = 0;
    if ((!d2227) && (u_is_con(s2226, &n_syntax_46_ELit) && u_is_con(u_field(s2226, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s2226, 0), 0);
      d2227 = 1;
      {
        static struct u_object l_l__2228 = U_LIT("l_");
        u_value t2229;
        if (u_truth(u_lt(u_str_size(v_s), u_mk_int(16)))) {
          t2229 = u_str_size(v_s);
        } else {
          t2229 = u_mk_int(16);
        }
        static struct u_object l___2230 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__2228, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), t2229))), &l___2230)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__2231 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_2232 = U_LIT("U_LIT");
        static struct u_object l__38__2233 = U_LIT("&");
        u_value t2234 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__2231), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_2232, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t2235 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2233, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t2234;
        *r_r = t2235;
      }
    }
    if ((!d2227) && u_is_con(s2226, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s2226, 0);
      d2227 = 1;
      {
        u_value t2236 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2237 = f_compile_46_literal_value_1(v_l);
        *r_ss = t2236;
        *r_r = t2237;
      }
    }
    if ((!d2227) && (u_is_con(s2226, &n_syntax_46_EVar) && u_is_con(u_field(s2226, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s2226, 0), 0);
      u_value v_ty = u_field(u_field(s2226, 0), 1);
      d2227 = 1;
      {
        u_value s2238 = f_list_46_lookup_2(v_n, v_env);
        int d2239 = 0;
        if ((!d2239) && u_is_con(s2238, &n_option_46_Some)) {
          u_value v_c = u_field(s2238, 0);
          d2239 = 1;
          {
            u_value t2240 = u_mk_con(&n_list_46_Nil, 0);
            u_value t2241 = v_c;
            *r_ss = t2240;
            *r_r = t2241;
          }
        }
        if ((!d2239) && u_is_con(s2238, &n_option_46_None)) {
          d2239 = 1;
          {
            p_compile_46_global_value(v_gl, v_n, v_ty, r_r);
            u_value t2242 = u_mk_con(&n_list_46_Nil, 0);
            *r_ss = t2242;
          }
        }
        if (!d2239) {
          u_no_case(s2238);
        }
      }
    }
    if ((!d2227) && (u_is_con(s2226, &n_syntax_46_EApp) && (u_is_con(u_field(s2226, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s2226, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s2226, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s2226, 0), 0), 1);
      u_value v_args = u_field(s2226, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d2227 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d2227) && u_is_con(s2226, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s2226, 0);
      u_value v_args = u_field(s2226, 1);
      d2227 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_2243 = U_LIT("u_apply");
        u_value t2244 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2245 = f_c_46_c_call_2(&l_u_apply_2243, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t2244;
        *r_r = t2245;
      }
    }
    if ((!d2227) && u_is_con(s2226, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s2226, 0);
      u_value v_b = u_field(s2226, 1);
      d2227 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2227) && u_is_con(s2226, &n_syntax_46_EProc)) {
      u_value v_is = u_field(s2226, 0);
      u_value v_os = u_field(s2226, 1);
      u_value v_rs = u_field(s2226, 2);
      u_value v_b = u_field(s2226, 3);
      d2227 = 1;
      {
        p_compile_46_compile_proc_lambda(v_gl, v_env, v_is, f_list_46__43__43__2(v_os, v_rs), v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2227) && u_is_con(s2226, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s2226, 0);
      u_value v_b = u_field(s2226, 1);
      d2227 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2246 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2246, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2247 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t2247;
        u_value t2248 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2248;
      }
    }
    if ((!d2227) && u_is_con(s2226, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s2226, 0);
      u_value v_l = u_field(s2226, 1);
      u_value v_rr = u_field(s2226, 2);
      d2227 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2249 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2249, &v_t, r_g);
        u_value t2250 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2250;
        u_value t2251 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2251;
      }
    }
    if ((!d2227) && u_is_con(s2226, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s2226, 0);
      u_value v_cases = u_field(s2226, 1);
      d2227 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2252 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2252, &v_d, r_g);
        static struct u_object l_t_2253 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2253, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_2254 = U_LIT("NULL");
        u_value t2255 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2254))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2255;
        static struct u_object l__33__2256 = U_LIT("!");
        static struct u_object l_u_no_case_2257 = U_LIT("u_no_case");
        u_value t2258 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2256, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2257, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2258;
        u_value t2259 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2259;
      }
    }
    if ((!d2227) && u_is_con(s2226, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s2226, 0);
      u_value v_xs = u_field(s2226, 1);
      u_value v_lists = u_field(s2226, 2);
      u_value v_c = u_field(s2226, 3);
      d2227 = 1;
      {
        static struct u_object l_t_2260 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2260, &v_t, r_g);
        static struct u_object l_e_2261 = U_LIT("e");
        p_compile_46_gen_name(&l_e_2261, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_2262 = U_LIT("u_mk_con");
        static struct u_object l__38__2263 = U_LIT("&");
        u_value t2264 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_2262, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__2263, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t2264;
        u_value t2265 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2265;
      }
    }
    if ((!d2227) && u_is_con(s2226, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s2226, 0);
      d2227 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_2266 = U_LIT("u_mk_tuple");
        u_value t2267 = f_c_46_c_call_2(&l_u_mk_tuple_2266, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t2267;
      }
    }
    if ((!d2227) && u_is_con(s2226, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s2226, 0);
      u_value v_ty = u_field(s2226, 1);
      d2227 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2268 = U_LIT("u_raise");
        static struct u_object l_NULL_2269 = U_LIT("NULL");
        u_value t2270 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2268, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        u_value t2271 = u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2269);
        *r_ss = t2270;
        *r_r = t2271;
      }
    }
    if (!d2227) {
      u_no_case(s2226);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s2272 = v_es;
    int d2273 = 0;
    if ((!d2273) && u_is_con(s2272, &n_list_46_Nil)) {
      d2273 = 1;
      {
        u_value t2274 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2275 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2274;
        *r_rs = t2275;
      }
    }
    if ((!d2273) && u_is_con(s2272, &n_list_46_Cons)) {
      u_value v_e = u_field(s2272, 0);
      u_value v_rest = u_field(s2272, 1);
      d2273 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t2276 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2277 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t2276;
        *r_rs = t2277;
      }
    }
    if (!d2273) {
      u_no_case(s2272);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s2278 = v_rs;
    int d2279 = 0;
    if ((!d2279) && u_is_con(s2278, &n_list_46_Nil)) {
      d2279 = 1;
      {
        u_value t2280 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2280;
      }
    }
    if ((!d2279) && u_is_con(s2278, &n_list_46_Cons)) {
      u_value v_r = u_field(s2278, 0);
      u_value v_rest = u_field(s2278, 1);
      d2279 = 1;
      {
        static struct u_object l_t_2281 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2281, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t2282 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t2282;
      }
    }
    if (!d2279) {
      u_no_case(s2278);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s2283 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2284 = NULL;
  if ((!t2284) && (u_is_con(u_field(s2283, 0), &n_list_46_Cons) && (u_is_con(u_field(s2283, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2283, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2283, 0), 0);
    u_value v_more = u_field(u_field(s2283, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2283, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2283, 1), 1);
    t2284 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t2284) {
    t2284 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2284) {
    u_no_case(s2283);
  }
  return t2284;
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
    u_value s2285 = v_args;
    int d2286 = 0;
    if ((!d2286) && (u_is_con(s2285, &n_list_46_Cons) && (u_is_con(u_field(s2285, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2285, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s2285, 0);
      u_value v_b = u_field(u_field(s2285, 1), 0);
      static struct u_object l_and_2293 = U_LIT("and");
      u_value t2295 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2293));
      if (!u_truth(t2295)) {
        static struct u_object l_or_2294 = U_LIT("or");
        t2295 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_2294));
      }
      if (u_truth(t2295)) {
        d2286 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_2287 = U_LIT("t");
          p_compile_46_gen_name(&l_t_2287, &v_t, r_g);
          static struct u_object l_and_2288 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2288)))) {
            {
              u_value t2289 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2289;
            }
          } else {
            {
              static struct u_object l__33__2290 = U_LIT("!");
              u_value t2291 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2290, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2291;
            }
          }
          u_value t2292 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t2292;
        }
      }
    }
    if (!d2286) {
      d2286 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t2296 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t2296)) {
          t2296 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t2296)) {
          {
            u_value t2297 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t2297;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_2298 = U_LIT("u_mk_con");
              u_value t2299 = f_c_46_c_call_2(&l_u_mk_con_2298, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t2299;
            }
          } else {
            {
              u_value s2300 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
              int d2301 = 0;
              if ((!d2301) && u_is_con(s2300, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s2300, 0), 0);
                u_value v_k = u_field(u_field(s2300, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d2301 = 1;
                  {
                    u_value t2302 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t2302;
                  }
                }
              }
              if (!d2301) {
                d2301 = 1;
                {
                  p_compile_46_global_value(v_gl, v_n, v_ty, &v_f);
                  static struct u_object l_u_apply_2303 = U_LIT("u_apply");
                  u_value t2304 = f_c_46_c_call_2(&l_u_apply_2303, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t2304;
                }
              }
              if (!d2301) {
                u_no_case(s2300);
              }
            }
          }
        }
      }
    }
    if (!d2286) {
      u_no_case(s2285);
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
    u_value t2305 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2306 = &t2305;
    u_value l2307 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l2307, &n_list_46_Cons)) {
      u_value v_x = u_field(l2307, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2306 = u_append(e2306, v_x);
      }
      l2307 = u_field(l2307, 1);
    }
    u_value t2308 = t2305;
    v_caps = t2308;
    static struct u_object l_lam_2309 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_2309, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_2310 = U_LIT("env");
    static struct u_object l_args_2311 = U_LIT("args");
    u_value t2312 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2310), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_2311));
    v_decls = t2312;
    static struct u_object l_env_2313 = U_LIT("env");
    static struct u_object l_args_2314 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2313), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2314), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_2315 = U_LIT("u_mk_closure");
    u_value t2316 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2317 = &t2316;
    u_value l2318 = v_caps;
    while (u_is_con(l2318, &n_list_46_Cons)) {
      u_value v_x = u_field(l2318, 0);
      if (u_truth(u_mk_bool(1))) {
        e2317 = u_append(e2317, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2318 = u_field(l2318, 1);
    }
    u_value t2319 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2320 = f_c_46_c_call_2(&l_u_mk_closure_2315, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2316));
    *r_ss = t2319;
    *r_r = t2320;
  }
}

void p_compile_46_compile_proc_lambda(u_value v_gl, u_value v_env, u_value v_ins, u_value v_outs, u_value v_b, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_caps = NULL;
    u_value v_f = NULL;
    u_value v_bs = NULL;
    u_value v_decls = NULL;
    u_value t2321 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2322 = &t2321;
    u_value l2323 = f_list_46_distinct_1(f_compile_46_free_in_stmt_2(v_b, f_syntax_46_names_1(f_list_46__43__43__2(v_ins, v_outs))));
    while (u_is_con(l2323, &n_list_46_Cons)) {
      u_value v_x = u_field(l2323, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2322 = u_append(e2322, v_x);
      }
      l2323 = u_field(l2323, 1);
    }
    u_value t2324 = t2321;
    v_caps = t2324;
    static struct u_object l_proc_2325 = U_LIT("proc");
    p_compile_46_gen_name(&l_proc_2325, &v_f, r_g);
    u_value t2326 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2327 = &t2326;
    u_value l2328 = f_syntax_46_names_1(v_outs);
    while (u_is_con(l2328, &n_list_46_Cons)) {
      u_value v_x = u_field(l2328, 0);
      if (u_truth(u_mk_bool(1))) {
        e2327 = u_append(e2327, f_compile_46_pointer_1(v_x));
      }
      l2328 = u_field(l2328, 1);
    }
    p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_ins))), t2326), v_b, &v_bs, r_g);
    static struct u_object l_env_2329 = U_LIT("env");
    static struct u_object l_ins_2330 = U_LIT("ins");
    u_value t2331 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2329), f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_ins), u_mk_int(0)), &l_ins_2330), f_compile_46_unpack_pointers_1(f_compile_46_numbered_2(f_syntax_46_names_1(v_outs), u_mk_int(0)))));
    v_decls = t2331;
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), v_f, f_compile_46_proc_params_0(), f_list_46__43__43__2(v_decls, v_bs)), r_g);
    static struct u_object l_u_mk_proc_2332 = U_LIT("u_mk_proc");
    u_value t2333 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2334 = &t2333;
    u_value l2335 = v_caps;
    while (u_is_con(l2335, &n_list_46_Cons)) {
      u_value v_x = u_field(l2335, 0);
      if (u_truth(u_mk_bool(1))) {
        e2334 = u_append(e2334, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2335 = u_field(l2335, 1);
    }
    u_value t2336 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2337 = f_c_46_c_call_2(&l_u_mk_proc_2332, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2333));
    *r_ss = t2336;
    *r_r = t2337;
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
  static struct u_object l_env_2338 = U_LIT("env");
  static struct u_object l_ins_2339 = U_LIT("ins");
  static struct u_object l_outs_2340 = U_LIT("outs");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2338), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_ins_2339), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, f_compile_46_value_ptr_0()), &l_outs_2340), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_compile_46_unpack_pointers_1(u_value v_xs) {
  u_value s2341 = v_xs;
  u_value t2342 = NULL;
  if ((!t2342) && u_is_con(s2341, &n_list_46_Nil)) {
    t2342 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2342) && u_is_con(s2341, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2341, 0), 0);
    u_value v_i = u_field(u_field(s2341, 0), 1);
    u_value v_rest = u_field(s2341, 1);
    static struct u_object l_outs_2343 = U_LIT("outs");
    t2342 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(&l_outs_2343, v_i))), f_compile_46_unpack_pointers_1(v_rest));
  }
  if (!t2342) {
    u_no_case(s2341);
  }
  return t2342;
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s2344 = v_xs;
  u_value t2345 = NULL;
  if ((!t2345) && u_is_con(s2344, &n_list_46_Nil)) {
    t2345 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2345) && u_is_con(s2344, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2344, 0), 0);
    u_value v_i = u_field(u_field(s2344, 0), 1);
    u_value v_rest = u_field(s2344, 1);
    t2345 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t2345) {
    u_no_case(s2344);
  }
  return t2345;
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
    u_value s2346 = v_cases;
    int d2347 = 0;
    if ((!d2347) && u_is_con(s2346, &n_list_46_Nil)) {
      d2347 = 1;
      {
        u_value t2348 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2348;
      }
    }
    if ((!d2347) && (u_is_con(s2346, &n_list_46_Cons) && u_is_con(u_field(s2346, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2346, 0), 0);
      u_value v_guard = u_field(u_field(s2346, 0), 1);
      u_value v_e = u_field(u_field(s2346, 0), 2);
      u_value v_more = u_field(s2346, 1);
      d2347 = 1;
      {
        u_value t2349 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2349;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2350 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t2350;
        u_value s2351 = v_guard;
        int d2352 = 0;
        if ((!d2352) && u_is_con(s2351, &n_option_46_Some)) {
          u_value v_c = u_field(s2351, 0);
          d2352 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2353 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2353;
          }
        }
        if ((!d2352) && u_is_con(s2351, &n_option_46_None)) {
          d2352 = 1;
          {
          }
        }
        if (!d2352) {
          u_no_case(s2351);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2354 = U_LIT("!");
        u_value t2355 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2354, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2355;
      }
    }
    if (!d2347) {
      u_no_case(s2346);
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
    u_value s2356 = u_mk_tuple(2, v_xs, v_lists);
    int d2357 = 0;
    if ((!d2357) && (u_is_con(u_field(s2356, 0), &n_list_46_Cons) && u_is_con(u_field(s2356, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2356, 0), 0);
      u_value v_more = u_field(u_field(s2356, 0), 1);
      u_value v_list = u_field(u_field(s2356, 1), 0);
      u_value v_others = u_field(u_field(s2356, 1), 1);
      d2357 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2358 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2358, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2359 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2359;
        static struct u_object l_u_is_con_2360 = U_LIT("u_is_con");
        u_value t2361 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2360, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2361;
      }
    }
    if (!d2357) {
      d2357 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2362 = U_LIT("u_append");
        u_value t2363 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2362, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2363;
      }
    }
    if (!d2357) {
      u_no_case(s2356);
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
    u_value s2364 = v_s;
    int d2365 = 0;
    if ((!d2365) && u_is_con(s2364, &n_syntax_46_SSkip)) {
      d2365 = 1;
      {
        u_value t2366 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2366;
      }
    }
    if ((!d2365) && u_is_con(s2364, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2364, 0);
      d2365 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2367 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2367;
      }
    }
    if ((!d2365) && u_is_con(s2364, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2364, 0);
      u_value v_body = u_field(s2364, 1);
      d2365 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t2368 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2369 = &t2368;
        u_value l2370 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2370, &n_list_46_Cons)) {
          u_value v_x = u_field(l2370, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2371 = U_LIT("NULL");
            e2369 = u_append(e2369, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2371))));
          }
          l2370 = u_field(l2370, 1);
        }
        u_value t2372 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2368, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2372;
      }
    }
    if ((!d2365) && u_is_con(s2364, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2364, 0);
      u_value v_l = u_field(s2364, 1);
      u_value v_r = u_field(s2364, 2);
      d2365 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2373 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2373;
      }
    }
    if ((!d2365) && u_is_con(s2364, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2364, 0);
      u_value v_b = u_field(s2364, 1);
      d2365 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2374 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2374, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2375 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2375;
      }
    }
    if ((!d2365) && u_is_con(s2364, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2364, 0);
      u_value v_ins = u_field(s2364, 1);
      u_value v_outs = u_field(s2364, 2);
      u_value v_refs = u_field(s2364, 3);
      d2365 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        if (u_truth(f_list_46_has_key_2(v_p, v_env))) {
          {
            static struct u_object l_u_call_proc_2376 = U_LIT("u_call_proc");
            u_value t2377 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e2378 = &t2377;
            u_value l2379 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
            while (u_is_con(l2379, &n_list_46_Cons)) {
              u_value v_x = u_field(l2379, 0);
              if (u_truth(u_mk_bool(1))) {
                e2378 = u_append(e2378, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
              }
              l2379 = u_field(l2379, 1);
            }
            u_value t2380 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_call_proc_2376, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_var_expr_2(v_env, v_p), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_ins)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(f_list_46__43__43__2(v_outs, v_refs))), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(v_rs, t2377)))), u_mk_con(&n_list_46_Nil, 0)));
            *r_ss = t2380;
          }
        } else {
          {
            u_value t2381 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e2382 = &t2381;
            u_value l2383 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
            while (u_is_con(l2383, &n_list_46_Cons)) {
              u_value v_x = u_field(l2383, 0);
              if (u_truth(u_mk_bool(1))) {
                e2382 = u_append(e2382, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
              }
              l2383 = u_field(l2383, 1);
            }
            u_value t2384 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2381))), u_mk_con(&n_list_46_Nil, 0)));
            *r_ss = t2384;
          }
        }
      }
    }
    if ((!d2365) && u_is_con(s2364, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2364, 0);
      u_value v_cases = u_field(s2364, 1);
      d2365 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2385 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2385, &v_d, r_g);
        static struct u_object l_d_2386 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2386, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2387 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2387;
        static struct u_object l__33__2388 = U_LIT("!");
        static struct u_object l_u_no_case_2389 = U_LIT("u_no_case");
        u_value t2390 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2388, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2389, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2390;
      }
    }
    if ((!d2365) && (u_is_con(s2364, &n_syntax_46_SAssert) && (u_is_con(u_field(s2364, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2364, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2364, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2364, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2364, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2364, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2364, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2364, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2364, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2364, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2364, 0), 1), 1), 0);
      d2365 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2391 = U_LIT("u_assert_equal");
        u_value t2392 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2391, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2392;
      }
    }
    if ((!d2365) && u_is_con(s2364, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2364, 0);
      d2365 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2393 = U_LIT("u_assert_true");
        u_value t2394 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2393, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2394;
      }
    }
    if ((!d2365) && u_is_con(s2364, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2364, 0);
      u_value v_ty = u_field(s2364, 1);
      d2365 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2395 = U_LIT("u_raise");
        u_value t2396 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2395, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2396;
      }
    }
    if ((!d2365) && u_is_con(s2364, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2364, 0);
      u_value v_hs = u_field(s2364, 1);
      d2365 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2397 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2397, &v_d, r_g);
        static struct u_object l_d_2398 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2398, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2399 = U_LIT("u_pop");
        static struct u_object l__33__2400 = U_LIT("!");
        static struct u_object l_u_reraise_2401 = U_LIT("u_reraise");
        u_value t2402 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2399, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2400, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2401, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2402;
        static struct u_object l_struct_32_u_handler_2403 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2404 = U_LIT("u_push");
        static struct u_object l__61__61__2405 = U_LIT("==");
        static struct u_object l_setjmp_2406 = U_LIT("setjmp");
        static struct u_object l_env_2407 = U_LIT("env");
        static struct u_object l_u_pop_2408 = U_LIT("u_pop");
        u_value t2409 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2403)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2404, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2405, f_c_46_c_call_2(&l_setjmp_2406, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2407), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2408, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2409;
      }
    }
    if (!d2365) {
      u_no_case(s2364);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2410 = v_xs;
    int d2411 = 0;
    if ((!d2411) && u_is_con(s2410, &n_list_46_Nil)) {
      d2411 = 1;
      {
        u_value t2412 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2412;
      }
    }
    if ((!d2411) && u_is_con(s2410, &n_list_46_Cons)) {
      u_value v_x = u_field(s2410, 0);
      u_value v_rest = u_field(s2410, 1);
      d2411 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2413 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2413;
      }
    }
    if (!d2411) {
      u_no_case(s2410);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2414 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2415 = NULL;
  if ((!t2415) && (u_is_con(u_field(s2414, 0), &n_list_46_Cons) && (u_is_con(u_field(s2414, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2414, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2414, 0), 0);
    u_value v_more = u_field(u_field(s2414, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2414, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2414, 1), 1);
    t2415 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2415) {
    t2415 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2415) {
    u_no_case(s2414);
  }
  return t2415;
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
    static struct u_object l_u_caught_2416 = U_LIT("u_caught");
    u_value t2417 = f_c_46_c_call_2(&l_u_caught_2416, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2417;
    u_value s2418 = v_hs;
    int d2419 = 0;
    if ((!d2419) && u_is_con(s2418, &n_list_46_Nil)) {
      d2419 = 1;
      {
        u_value t2420 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2420;
      }
    }
    if ((!d2419) && (u_is_con(s2418, &n_list_46_Cons) && u_is_con(u_field(s2418, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2418, 0), 0);
      u_value v_p = u_field(u_field(s2418, 0), 1);
      u_value v_guard = u_field(u_field(s2418, 0), 2);
      u_value v_s = u_field(u_field(s2418, 0), 3);
      u_value v_more = u_field(s2418, 1);
      d2419 = 1;
      {
        u_value t2421 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2421;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2422 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2422;
        u_value s2423 = v_guard;
        int d2424 = 0;
        if ((!d2424) && u_is_con(s2423, &n_option_46_Some)) {
          u_value v_c = u_field(s2423, 0);
          d2424 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2425 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2425;
          }
        }
        if ((!d2424) && u_is_con(s2423, &n_option_46_None)) {
          d2424 = 1;
          {
          }
        }
        if (!d2424) {
          u_no_case(s2423);
        }
        u_value s2426 = v_p;
        int d2427 = 0;
        if ((!d2427) && u_is_con(s2426, &n_syntax_46_PWild)) {
          d2427 = 1;
          {
            u_value t2428 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2428;
          }
        }
        if (!d2427) {
          d2427 = 1;
          {
            static struct u_object l_u_has_type_2429 = U_LIT("u_has_type");
            u_value t2430 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2429, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2430;
          }
        }
        if (!d2427) {
          u_no_case(s2426);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2431 = U_LIT("!");
        u_value t2432 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2431, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2432;
      }
    }
    if (!d2419) {
      u_no_case(s2418);
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
    u_value s2433 = v_cases;
    int d2434 = 0;
    if ((!d2434) && u_is_con(s2433, &n_list_46_Nil)) {
      d2434 = 1;
      {
        u_value t2435 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2435;
      }
    }
    if ((!d2434) && (u_is_con(s2433, &n_list_46_Cons) && u_is_con(u_field(s2433, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2433, 0), 0);
      u_value v_guard = u_field(u_field(s2433, 0), 1);
      u_value v_s = u_field(u_field(s2433, 0), 2);
      u_value v_more = u_field(s2433, 1);
      d2434 = 1;
      {
        u_value t2436 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2436;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2437 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2437;
        u_value s2438 = v_guard;
        int d2439 = 0;
        if ((!d2439) && u_is_con(s2438, &n_option_46_Some)) {
          u_value v_c = u_field(s2438, 0);
          d2439 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2440 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2440;
          }
        }
        if ((!d2439) && u_is_con(s2438, &n_option_46_None)) {
          d2439 = 1;
          {
          }
        }
        if (!d2439) {
          u_no_case(s2438);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2441 = U_LIT("!");
        u_value t2442 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2441, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2442;
      }
    }
    if (!d2434) {
      u_no_case(s2433);
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
    u_value s2443 = v_c;
    int d2444 = 0;
    if ((!d2444) && u_is_con(s2443, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2443, 0);
      u_value v_xss = u_field(s2443, 2);
      u_value v_b = u_field(s2443, 4);
      d2444 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2445 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2446 = &t2445;
        u_value l2447 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2447, &n_list_46_Cons)) {
          u_value v_x = u_field(l2447, 0);
          if (u_truth(u_mk_bool(1))) {
            e2446 = u_append(e2446, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2447 = u_field(l2447, 1);
        }
        u_value t2448 = t2445;
        v_params = t2448;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2449 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2449;
          }
        } else {
          {
            u_value t2450 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2450;
          }
        }
        u_value t2451 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2452 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2451;
        *r_ms = t2452;
      }
    }
    if ((!d2444) && u_is_con(s2443, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2443, 0);
      u_value v_ins = u_field(s2443, 2);
      u_value v_outs = u_field(s2443, 3);
      u_value v_refs = u_field(s2443, 4);
      u_value v_body = u_field(s2443, 5);
      d2444 = 1;
      {
        u_value t2453 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2454 = &t2453;
        u_value l2455 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2455, &n_list_46_Cons)) {
          u_value v_x = u_field(l2455, 0);
          if (u_truth(u_mk_bool(1))) {
            e2454 = u_append(e2454, f_compile_46_pointer_1(v_x));
          }
          l2455 = u_field(l2455, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2453), v_body, &v_bs, r_g);
        u_value t2456 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2457 = &t2456;
        u_value l2458 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2458, &n_list_46_Cons)) {
          u_value v_x = u_field(l2458, 0);
          if (u_truth(u_mk_bool(1))) {
            e2457 = u_append(e2457, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2458 = u_field(l2458, 1);
        }
        u_value t2459 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2460 = &t2459;
        u_value l2461 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2461, &n_list_46_Cons)) {
          u_value v_x = u_field(l2461, 0);
          if (u_truth(u_mk_bool(1))) {
            e2460 = u_append(e2460, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2461 = u_field(l2461, 1);
        }
        u_value t2462 = f_list_46__43__43__2(t2456, t2459);
        v_params = t2462;
        u_value t2463 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2464 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2463;
        *r_ms = t2464;
      }
    }
    if ((!d2444) && u_is_con(s2443, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s2443, 0);
      d2444 = 1;
      {
        p_compile_46_compile_expr(v_gl, u_mk_con(&n_list_46_Nil, 0), v_e, &v_bs, &v_rb, r_g);
        static struct u_object l_u_print_line_2465 = U_LIT("u_print_line");
        u_value t2466 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2467 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_line_2465, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2466;
        *r_ms = t2467;
      }
    }
    if ((!d2444) && (u_is_con(s2443, &n_syntax_46_CRun) && u_is_con(u_field(s2443, 1), &n_syntax_46_SBlock))) {
      u_value v_xs = u_field(u_field(s2443, 1), 0);
      u_value v_body = u_field(u_field(s2443, 1), 1);
      d2444 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_body, &v_bs, r_g);
        u_value t2468 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2469 = &t2468;
        u_value l2470 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2470, &n_list_46_Cons)) {
          u_value v_x = u_field(l2470, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2471 = U_LIT("NULL");
            e2469 = u_append(e2469, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2471))));
          }
          l2470 = u_field(l2470, 1);
        }
        u_value t2472 = t2468;
        v_copy = t2472;
        u_value t2473 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2474 = &t2473;
        u_value l2475 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2475, &n_list_46_Cons)) {
          u_value v_x = u_field(l2475, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_u_print_binding_2476 = U_LIT("u_print_binding");
            e2474 = u_append(e2474, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_binding_2476, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_syntax_46_Name_46_base_1(v_x)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x)), u_mk_con(&n_list_46_Nil, 0))))));
          }
          l2475 = u_field(l2475, 1);
        }
        u_value t2477 = t2473;
        v_back = t2477;
        u_value t2478 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2479 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_copy, f_list_46__43__43__2(v_bs, v_back))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2478;
        *r_ms = t2479;
      }
    }
    if ((!d2444) && u_is_con(s2443, &n_syntax_46_CRun)) {
      u_value v_body = u_field(s2443, 1);
      d2444 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2480 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2481 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2480;
        *r_ms = t2481;
      }
    }
    if ((!d2444) && u_is_con(s2443, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s2443, 1);
      d2444 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2482 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2483 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2482;
        *r_ms = t2483;
      }
    }
    if (!d2444) {
      d2444 = 1;
      {
        u_value t2484 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2485 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2484;
        *r_ms = t2485;
      }
    }
    if (!d2444) {
      u_no_case(s2443);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2486 = v_cs;
    int d2487 = 0;
    if ((!d2487) && u_is_con(s2486, &n_list_46_Nil)) {
      d2487 = 1;
      {
        u_value t2488 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2489 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2488;
        *r_ms = t2489;
      }
    }
    if ((!d2487) && u_is_con(s2486, &n_list_46_Cons)) {
      u_value v_c = u_field(s2486, 0);
      u_value v_rest = u_field(s2486, 1);
      d2487 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_rest, &v_d2, &v_m2, r_g);
        u_value t2490 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2491 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2490;
        *r_ms = t2491;
      }
    }
    if (!d2487) {
      u_no_case(s2486);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_handler_2492 = U_LIT("handler");
  u_value t2519 = u_mk_con(&n_c_46_CeVar, 1, &l_handler_2492);
  u_value t2518;
  {
    u_value v_h = t2519;
    static struct u_object l_static_32_u_value_2493 = U_LIT("static u_value");
    static struct u_object l_cache_2494 = U_LIT("cache");
    static struct u_object l_NULL_2495 = U_LIT("NULL");
    static struct u_object l__61__61__2496 = U_LIT("==");
    static struct u_object l_cache_2497 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2498 = U_LIT("U_COMPUTING");
    static struct u_object l_u_fail_2499 = U_LIT("u_fail");
    static struct u_object l_recursive_32_initia_2500 = U_LIT("recursive initialization of ");
    static struct u_object l_cache_2501 = U_LIT("cache");
    static struct u_object l_cache_2502 = U_LIT("cache");
    static struct u_object l_cache_2503 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2504 = U_LIT("U_COMPUTING");
    static struct u_object l_struct_32_u_handler_2505 = U_LIT("struct u_handler");
    static struct u_object l_handler_2506 = U_LIT("handler");
    static struct u_object l_u_push_2507 = U_LIT("u_push");
    static struct u_object l__33__61__2508 = U_LIT("!=");
    static struct u_object l_setjmp_2509 = U_LIT("setjmp");
    static struct u_object l_env_2510 = U_LIT("env");
    static struct u_object l_u_pop_2511 = U_LIT("u_pop");
    static struct u_object l_cache_2512 = U_LIT("cache");
    static struct u_object l_NULL_2513 = U_LIT("NULL");
    static struct u_object l_u_reraise_2514 = U_LIT("u_reraise");
    static struct u_object l_u_pop_2515 = U_LIT("u_pop");
    static struct u_object l_cache_2516 = U_LIT("cache");
    static struct u_object l_cache_2517 = U_LIT("cache");
    t2518 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2493), &l_cache_2494, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2495))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2496, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2497), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2498)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2499, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2500, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2501), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2502))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2503), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2504)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2505)), &l_handler_2506, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2507, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__33__61__2508, f_c_46_c_call_2(&l_setjmp_2509, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, v_h, &l_env_2510), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2511, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2512), u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2513)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2514, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2515, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2516), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2517))), u_mk_con(&n_list_46_Nil, 0))))));
  }
  return t2518;
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2520 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2521 = &t2520;
  u_value l2522 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l2522, &n_list_46_Cons)) {
    u_value v_p = u_field(l2522, 0);
    if (u_truth(u_mk_bool(1))) {
      e2521 = u_append(e2521, f_compile_46_fun_wrapper_1(v_p));
    }
    l2522 = u_field(l2522, 1);
  }
  u_value t2523 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2524 = &t2523;
  u_value l2525 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l2525, &n_list_46_Cons)) {
    u_value v_p = u_field(l2525, 0);
    if (u_truth(u_mk_bool(1))) {
      e2524 = u_append(e2524, f_compile_46_constr_wrapper_1(v_p));
    }
    l2525 = u_field(l2525, 1);
  }
  u_value t2526 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2527 = &t2526;
  u_value l2528 = f_list_46_seconds_1(f_map_46_entries_1(f_compile_46_builtin_functions_0()));
  while (u_is_con(l2528, &n_list_46_Cons)) {
    u_value v_p = u_field(l2528, 0);
    if (u_truth(u_mk_bool(1))) {
      e2527 = u_append(e2527, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2528 = u_field(l2528, 1);
  }
  u_value t2529 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2530 = &t2529;
  u_value l2531 = f_map_46_entries_1(f_compile_46_Globals_46_procs_1(v_gl));
  while (u_is_con(l2531, &n_list_46_Cons)) {
    u_value v_p = u_field(l2531, 0);
    if (u_truth(u_mk_bool(1))) {
      e2530 = u_append(e2530, f_compile_46_proc_wrapper_1(v_p));
    }
    l2531 = u_field(l2531, 1);
  }
  return f_list_46__43__43__2(t2520, f_list_46__43__43__2(f_list_46_concat_1(t2523), f_list_46__43__43__2(t2526, t2529)));
}

u_value f_compile_46_proc_wrapper_1(u_value v_p) {
  u_value s2532 = v_p;
  u_value t2533 = NULL;
  if (!t2533) {
    u_value v_n = u_field(s2532, 0);
    u_value v_k = u_field(u_field(s2532, 1), 0);
    u_value v_m = u_field(u_field(s2532, 1), 1);
    static struct u_object l_wp__2534 = U_LIT("wp_");
    u_value t2535 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2536 = &t2535;
    u_value l2537 = f_compile_46_range_to_1(v_k);
    while (u_is_con(l2537, &n_list_46_Cons)) {
      u_value v_i = u_field(l2537, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_ins_2538 = U_LIT("ins");
        e2536 = u_append(e2536, f_compile_46_index_2(&l_ins_2538, v_i));
      }
      l2537 = u_field(l2537, 1);
    }
    u_value t2539 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2540 = &t2539;
    u_value l2541 = f_compile_46_range_to_1(v_m);
    while (u_is_con(l2541, &n_list_46_Cons)) {
      u_value v_i = u_field(l2541, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_outs_2542 = U_LIT("outs");
        e2540 = u_append(e2540, f_compile_46_index_2(&l_outs_2542, v_i));
      }
      l2541 = u_field(l2541, 1);
    }
    t2533 = u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), u_concat(&l_wp__2534, f_compile_46_mangle_1(v_n)), f_compile_46_proc_params_0(), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_n), f_list_46__43__43__2(t2535, t2539))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2533) {
    u_no_case(s2532);
  }
  return t2533;
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2543 = v_p;
  u_value t2544 = NULL;
  if (!t2544) {
    u_value v_n = u_field(s2543, 0);
    u_value v_k = u_field(s2543, 1);
    static struct u_object l_w__2545 = U_LIT("w_");
    t2544 = f_compile_46_wrapper_3(u_concat(&l_w__2545, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2544) {
    u_no_case(s2543);
  }
  return t2544;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2546 = v_p;
  u_value t2547 = NULL;
  if (!t2547) {
    u_value v_f = u_field(s2546, 0);
    u_value v_k = u_field(s2546, 1);
    static struct u_object l_w_2548 = U_LIT("w");
    t2547 = f_compile_46_wrapper_3(u_concat(&l_w_2548, v_f), v_f, v_k);
  }
  if (!t2547) {
    u_no_case(s2546);
  }
  return t2547;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2549 = U_LIT("env");
  static struct u_object l_args_2550 = U_LIT("args");
  u_value t2551 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2552 = &t2551;
  u_value l2553 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2553, &n_list_46_Cons)) {
    u_value v_i = u_field(l2553, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2554 = U_LIT("args");
      e2552 = u_append(e2552, f_compile_46_index_2(&l_args_2554, v_i));
    }
    l2553 = u_field(l2553, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2549), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2550), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2551))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2555 = v_p;
  u_value t2556 = NULL;
  if ((!t2556) && u_equals(u_field(s2555, 1), u_mk_int(0))) {
    t2556 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2556) {
    u_value v_n = u_field(s2555, 0);
    u_value v_k = u_field(s2555, 1);
    static struct u_object l_wc__2557 = U_LIT("wc_");
    static struct u_object l_env_2558 = U_LIT("env");
    static struct u_object l_args_2559 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2560 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2561 = U_LIT("args");
    t2556 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2557, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2558), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2559), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2560, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2561), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2556) {
    u_no_case(s2555);
  }
  return t2556;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2562;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2562 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2562 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2562;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2563 = v_d;
  u_value t2564 = NULL;
  if ((!t2564) && u_is_con(s2563, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2563, 0);
    u_value v_f = u_field(s2563, 1);
    u_value v_ps = u_field(s2563, 2);
    t2564 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2564) {
    t2564 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2564) {
    u_no_case(s2563);
  }
  return t2564;
}

void p_compile_46_compile_program(u_value v_q, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2565 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2565;
    u_value t2566 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t2566;
    p_compile_46_compile_cmds(v_gl, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value t2567 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, f_compile_46_Gen_46_lifted_1(v_g)));
    v_fs = t2567;
    static struct u_object l_ulang_46_h_2568 = U_LIT("ulang.h");
    u_value t2569 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2570 = &t2569;
    u_value l2571 = v_fs;
    while (u_is_con(l2571, &n_list_46_Cons)) {
      u_value v_d = u_field(l2571, 0);
      if (u_truth(u_mk_bool(1))) {
        e2570 = u_append(e2570, f_compile_46_prototype_1(v_d));
      }
      l2571 = u_field(l2571, 1);
    }
    static struct u_object l_main_2572 = U_LIT("main");
    static struct u_object l_argc_2573 = U_LIT("argc");
    static struct u_object l_char_2574 = U_LIT("char");
    static struct u_object l_argv_2575 = U_LIT("argv");
    static struct u_object l_u_args_2576 = U_LIT("u_args");
    static struct u_object l_argc_2577 = U_LIT("argc");
    static struct u_object l_argv_2578 = U_LIT("argv");
    u_value t2579 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2568), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2569), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2572, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2573), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2574))), &l_argv_2575), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2576, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2577), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2578), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2579;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_target) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2580 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2580, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2581 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2581, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q);
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
    u_value s2582 = v_args;
    int d2583 = 0;
    if ((!d2583) && u_is_con(s2582, &n_list_46_Nil)) {
      d2583 = 1;
      {
      }
    }
    if ((!d2583) && (u_is_con(s2582, &n_list_46_Cons) && (u_is_con(u_field(s2582, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2582, 1), 1), &n_list_46_Nil)))) {
      u_value v_source = u_field(s2582, 0);
      u_value v_target = u_field(u_field(s2582, 1), 0);
      d2583 = 1;
      {
        p_compile_46_compile_file(v_source, v_target);
      }
    }
    if (!d2583) {
      d2583 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_sou_2584 = U_LIT("usage: ulang source.u target.c");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_sou_2584), "os.OsError");
      }
    }
    if (!d2583) {
      u_no_case(s2582);
    }
    u_print_binding("args", v_args);
  }
  return 0;
}
