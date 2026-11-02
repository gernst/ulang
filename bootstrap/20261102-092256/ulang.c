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

u_value lam611(u_value *env, u_value *args);

u_value lam722(u_value *env, u_value *args);

u_value lam725(u_value *env, u_value *args);

u_value lam790(u_value *env, u_value *args);

u_value lam1067(u_value *env, u_value *args);

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
    u_value s603 = v_t;
    int d604 = 0;
    if ((!d604) && u_is_con(s603, &n_syntax_46_THole)) {
      d604 = 1;
      {
        p_check_46_fresh(r_u, r_s);
      }
    }
    if ((!d604) && u_is_con(s603, &n_syntax_46_TParam)) {
      d604 = 1;
      {
        u_value t605 = v_t;
        *r_u = t605;
      }
    }
    if ((!d604) && u_is_con(s603, &n_syntax_46_TMeta)) {
      d604 = 1;
      {
        u_value t606 = v_t;
        *r_u = t606;
      }
    }
    if ((!d604) && u_is_con(s603, &n_syntax_46_TFun)) {
      u_value v_args = u_field(s603, 0);
      u_value v_r = u_field(s603, 1);
      d604 = 1;
      {
        p_check_46_normalizes(v_args, &v_us, r_s);
        p_check_46_normalize(v_r, &v_v, r_s);
        u_value t607 = u_mk_con(&n_syntax_46_TFun, 2, v_us, v_v);
        *r_u = t607;
      }
    }
    if ((!d604) && u_is_con(s603, &n_syntax_46_TTuple)) {
      u_value v_ts = u_field(s603, 0);
      d604 = 1;
      {
        p_check_46_normalizes(v_ts, &v_us, r_s);
        u_value t608 = u_mk_con(&n_syntax_46_TTuple, 1, v_us);
        *r_u = t608;
      }
    }
    if ((!d604) && u_is_con(s603, &n_syntax_46_TProc)) {
      u_value v_is = u_field(s603, 0);
      u_value v_os = u_field(s603, 1);
      u_value v_rs = u_field(s603, 2);
      d604 = 1;
      {
        p_check_46_normalizes(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs)), &v_us, r_s);
        u_value t609 = u_mk_con(&n_syntax_46_TProc, 3, f_list_46_take_2(f_list_46_length_1(v_is), v_us), f_list_46_take_2(f_list_46_length_1(v_os), f_list_46_drop_2(f_list_46_length_1(v_is), v_us)), f_list_46_drop_2(u_add(f_list_46_length_1(v_is), f_list_46_length_1(v_os)), v_us));
        *r_u = t609;
      }
    }
    if ((!d604) && u_is_con(s603, &n_syntax_46_TSort)) {
      u_value v_n = u_field(s603, 0);
      u_value v_args = u_field(s603, 1);
      d604 = 1;
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
    if (!d604) {
      u_no_case(s603);
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
        p_check_46_resolve_name(*r_s, v_env, v_n, u_mk_int(-1), &v_m, &v_sc);
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
    if ((!d646) && u_is_con(s645, &n_syntax_46_EProc)) {
      u_value v_is = u_field(s645, 0);
      u_value v_os = u_field(s645, 1);
      u_value v_rs = u_field(s645, 2);
      u_value v_body = u_field(s645, 3);
      d646 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))));
        p_check_46_formals(v_is, &v_xs, r_s);
        p_check_46_formals(v_os, &v_ys, r_s);
        p_check_46_formals(v_rs, &v_zs, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(f_list_46__43__43__2(v_xs, f_list_46__43__43__2(v_ys, v_zs))), v_env), f_syntax_46_names_1(f_list_46__43__43__2(v_ys, v_zs)), v_body, &v_y, r_s);
        u_value t653 = u_mk_con(&n_syntax_46_EProc, 4, v_xs, v_ys, v_zs, v_y);
        u_value t654 = u_mk_con(&n_syntax_46_TProc, 3, f_syntax_46_types_1(v_xs), f_syntax_46_types_1(v_ys), f_syntax_46_types_1(v_zs));
        *r_e1 = t653;
        *r_t = t654;
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
        u_value t655 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        u_value t656 = v_u;
        *r_e1 = t655;
        *r_t = t656;
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
        u_value t657 = u_mk_con(&n_syntax_46_EIte, 3, v_c, v_l, v_b);
        u_value t658 = v_u;
        *r_e1 = t657;
        *r_t = t658;
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
        u_value t659 = u_mk_con(&n_syntax_46_EComp, 4, v_b, v_xs, v_es, v_c);
        u_value t660 = f_check_46_tlist_1(v_u);
        *r_e1 = t659;
        *r_t = t660;
      }
    }
    if ((!d646) && u_is_con(s645, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s645, 0);
      d646 = 1;
      {
        p_check_46_infers(v_env, v_items, &v_es, &v_ts, r_s);
        u_value t661 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
        u_value t662 = u_mk_con(&n_syntax_46_TTuple, 1, v_ts);
        *r_e1 = t661;
        *r_t = t662;
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
        u_value t663 = u_mk_con(&n_syntax_46_ERaise, 2, v_c, v_u);
        *r_e1 = t663;
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
        u_value t664 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e665 = &t664;
        u_value l666 = v_bs;
        while (u_is_con(l666, &n_list_46_Cons)) {
          u_value v_br = u_field(l666, 0);
          if (u_truth(u_mk_bool(1))) {
            e665 = u_append(e665, f_check_46_case_of_1(v_br));
          }
          l666 = u_field(l666, 1);
        }
        p_check_46_coverage(*r_s, v_u, t664);
        u_value t667 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, v_bs);
        *r_e1 = t667;
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
    u_value s668 = v_f;
    int d669 = 0;
    if ((!d669) && (u_is_con(s668, &n_syntax_46_EVar) && (u_is_con(u_field(s668, 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(s668, 0), 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(u_field(s668, 0), 0), 0), &n_option_46_None))))) {
      u_value v_b = u_field(u_field(u_field(s668, 0), 0), 1);
      u_value v_a = u_field(u_field(s668, 0), 1);
      u_value t670 = f_prelude_46__62__2(u_str_size(v_b), u_mk_int(1));
      if (u_truth(t670)) {
        t670 = u_eq(u_str_get(v_b, u_mk_int(0)), u_mk_char(46));
      }
      if (u_truth(t670)) {
        d669 = 1;
        {
          p_check_46_field_access(v_env, v_b, v_a, v_args, r_e1, r_t, r_s);
        }
      }
    }
    if ((!d669) && (u_is_con(s668, &n_syntax_46_EVar) && u_is_con(u_field(s668, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s668, 0), 0);
      u_value v_a = u_field(u_field(s668, 0), 1);
      d669 = 1;
      {
        if (u_truth(f_prelude_46__62__2(f_list_46_length_1(f_check_46_candidates_3(*r_s, v_env, v_n)), u_mk_int(1)))) {
          {
            p_check_46_resolve_name(*r_s, v_env, v_n, f_list_46_length_1(v_args), &v_m, &v_sc);
            p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
            p_check_46_apply(v_env, f_syntax_46_Var_46_typ_1(v_x), v_args, &v_args1, r_t, r_s);
            u_value t671 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), v_args1);
            *r_e1 = t671;
          }
        } else {
          {
            p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
            p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
            u_value t672 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
            *r_e1 = t672;
          }
        }
      }
    }
    if (!d669) {
      d669 = 1;
      {
        p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
        p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
        u_value t673 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
        *r_e1 = t673;
      }
    }
    if (!d669) {
      u_no_case(s668);
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
    u_value s674 = v_args;
    int d675 = 0;
    if ((!d675) && u_is_con(s674, &n_list_46_Cons)) {
      u_value v_arg = u_field(s674, 0);
      u_value v_rest = u_field(s674, 1);
      d675 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_arg1, &v_u, r_s);
        u_value s676 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u);
        int d677 = 0;
        if ((!d677) && (u_is_con(s676, &n_syntax_46_TSort) && (u_is_con(u_field(s676, 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(s676, 0), 0), &n_option_46_Some)))) {
          u_value v_m = u_field(u_field(u_field(s676, 0), 0), 0);
          u_value v_tn = u_field(u_field(s676, 0), 1);
          d677 = 1;
          {
            u_value t678 = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), u_concat(v_tn, v_f));
            v_acc = t678;
          }
        }
        if (!d677) {
          u_value v_v = s676;
          d677 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownFieldType, 2, v_f, v_v), "check.TypeError");
          }
        }
        if (!d677) {
          u_no_case(s676);
        }
        u_value s679 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(*r_s), v_acc);
        int d680 = 0;
        if ((!d680) && (u_is_con(s679, &n_list_46_Cons) && u_is_con(u_field(s679, 1), &n_list_46_Nil))) {
          u_value v_sc = u_field(s679, 0);
          d680 = 1;
          {
            p_check_46_use(v_acc, v_a, v_sc, &v_x, r_s);
          }
        }
        if (!d680) {
          d680 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownField, 1, v_acc), "check.TypeError");
          }
        }
        if (!d680) {
          u_no_case(s679);
        }
        p_check_46_infers(v_env, v_rest, &v_rest1, &v_us, r_s);
        p_check_46_fresh(r_t, r_s);
        p_check_46_unify(f_syntax_46_Var_46_typ_1(v_x), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_u, v_us), *r_t), r_s);
        u_value t681 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), u_mk_con(&n_list_46_Cons, 2, v_arg1, v_rest1));
        *r_e1 = t681;
      }
    }
    if (!d675) {
      d675 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, u_mk_int(1), f_list_46_length_1(v_args)), "check.TypeError");
      }
    }
    if (!d675) {
      u_no_case(s674);
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
    u_value s682 = u_mk_tuple(2, v_es, v_ts);
    int d683 = 0;
    if ((!d683) && (u_is_con(u_field(s682, 0), &n_list_46_Cons) && u_is_con(u_field(s682, 1), &n_list_46_Cons))) {
      u_value v_e = u_field(u_field(s682, 0), 0);
      u_value v_more = u_field(u_field(s682, 0), 1);
      u_value v_t = u_field(u_field(s682, 1), 0);
      u_value v_ts1 = u_field(u_field(s682, 1), 1);
      d683 = 1;
      {
        p_check_46_check(v_env, v_e, v_t, &v_e1, r_s);
        p_check_46_checks(v_env, v_more, v_ts1, &v_rest, r_s);
        u_value t684 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        *r_es1 = t684;
      }
    }
    if ((!d683) && (u_is_con(u_field(s682, 0), &n_list_46_Nil) && u_is_con(u_field(s682, 1), &n_list_46_Nil))) {
      d683 = 1;
      {
        u_value t685 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t685;
      }
    }
    if (!d683) {
      d683 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_es)), "check.TypeError");
      }
    }
    if (!d683) {
      u_no_case(s682);
    }
  }
}

void p_check_46_infers(u_value v_env, u_value v_es, u_value *r_es1, u_value *r_ts, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value v_us = NULL;
    u_value s686 = v_es;
    int d687 = 0;
    if ((!d687) && u_is_con(s686, &n_list_46_Nil)) {
      d687 = 1;
      {
        u_value t688 = u_mk_con(&n_list_46_Nil, 0);
        u_value t689 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t688;
        *r_ts = t689;
      }
    }
    if ((!d687) && u_is_con(s686, &n_list_46_Cons)) {
      u_value v_e = u_field(s686, 0);
      u_value v_more = u_field(s686, 1);
      d687 = 1;
      {
        p_check_46_infer(v_env, v_e, &v_e1, &v_t, r_s);
        p_check_46_infers(v_env, v_more, &v_rest, &v_us, r_s);
        u_value t690 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        u_value t691 = u_mk_con(&n_list_46_Cons, 2, v_t, v_us);
        *r_es1 = t690;
        *r_ts = t691;
      }
    }
    if (!d687) {
      u_no_case(s686);
    }
  }
}

void p_check_46_generators(u_value v_env, u_value v_xs, u_value v_lists, u_value *r_lists1, u_value *r_s) {
  {
    u_value v_l = NULL;
    u_value v_rest = NULL;
    u_value s692 = u_mk_tuple(2, v_xs, v_lists);
    int d693 = 0;
    if ((!d693) && (u_is_con(u_field(s692, 0), &n_list_46_Cons) && u_is_con(u_field(s692, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s692, 0), 0);
      u_value v_ys = u_field(u_field(s692, 0), 1);
      u_value v_e = u_field(u_field(s692, 1), 0);
      u_value v_es = u_field(u_field(s692, 1), 1);
      d693 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tlist_1(f_syntax_46_Var_46_typ_1(v_x)), &v_l, r_s);
        p_check_46_generators(f_list_46__43__43__2(f_check_46_bind_1(u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0))), v_env), v_ys, v_es, &v_rest, r_s);
        u_value t694 = u_mk_con(&n_list_46_Cons, 2, v_l, v_rest);
        *r_lists1 = t694;
      }
    }
    if (!d693) {
      d693 = 1;
      {
        u_value t695 = u_mk_con(&n_list_46_Nil, 0);
        *r_lists1 = t695;
      }
    }
    if (!d693) {
      u_no_case(s692);
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
    u_value s696 = v_cases;
    int d697 = 0;
    if ((!d697) && u_is_con(s696, &n_list_46_Nil)) {
      d697 = 1;
      {
        u_value t698 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t698;
      }
    }
    if ((!d697) && (u_is_con(s696, &n_list_46_Cons) && u_is_con(u_field(s696, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s696, 0), 0);
      u_value v_g = u_field(u_field(s696, 0), 1);
      u_value v_b = u_field(u_field(s696, 0), 2);
      u_value v_more = u_field(s696, 1);
      d697 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_b, v_r, &v_b1, r_s);
        p_check_46_expr_branches(v_env, v_more, v_t, v_r, &v_rest, r_s);
        u_value t699 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t699;
      }
    }
    if (!d697) {
      u_no_case(s696);
    }
  }
}

void p_check_46_guard(u_value v_env, u_value v_g, u_value *r_g1, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value s700 = v_g;
    int d701 = 0;
    if ((!d701) && u_is_con(s700, &n_option_46_Some)) {
      u_value v_e = u_field(s700, 0);
      d701 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tbool_0(), &v_e1, r_s);
        u_value t702 = u_mk_con(&n_option_46_Some, 1, v_e1);
        *r_g1 = t702;
      }
    }
    if ((!d701) && u_is_con(s700, &n_option_46_None)) {
      d701 = 1;
      {
        u_value t703 = u_mk_con(&n_option_46_None, 0);
        *r_g1 = t703;
      }
    }
    if (!d701) {
      u_no_case(s700);
    }
  }
}

void p_check_46_coverage(u_value v_s, u_value v_t, u_value v_cs) {
  {
    p_check_46_reachable(v_s, v_t, u_mk_con(&n_list_46_Nil, 0), v_cs);
    u_value t704 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e705 = &t704;
    u_value l706 = f_check_46_unguarded_1(v_cs);
    while (u_is_con(l706, &n_list_46_Cons)) {
      u_value v_p = u_field(l706, 0);
      if (u_truth(u_mk_bool(1))) {
        e705 = u_append(e705, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
      }
      l706 = u_field(l706, 1);
    }
    u_value s707 = f_check_46_missing_4(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t704, u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
    int d708 = 0;
    if ((!d708) && (u_is_con(s707, &n_option_46_Some) && u_is_con(u_field(s707, 0), &n_list_46_Cons))) {
      u_value v_w = u_field(u_field(s707, 0), 0);
      d708 = 1;
      {
        u_raise(u_mk_con(&n_check_46_NotExhaustive, 1, v_w), "check.TypeError");
      }
    }
    if (!d708) {
      d708 = 1;
      {
      }
    }
    if (!d708) {
      u_no_case(s707);
    }
  }
}

u_value f_check_46_unguarded_1(u_value v_cs) {
  u_value s709 = v_cs;
  u_value t710 = NULL;
  if ((!t710) && u_is_con(s709, &n_list_46_Nil)) {
    t710 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t710) && (u_is_con(s709, &n_list_46_Cons) && u_equals(u_field(u_field(s709, 0), 1), u_mk_bool(1)))) {
    u_value v_p = u_field(u_field(s709, 0), 0);
    u_value v_rest = u_field(s709, 1);
    t710 = u_mk_con(&n_list_46_Cons, 2, v_p, f_check_46_unguarded_1(v_rest));
  }
  if ((!t710) && (u_is_con(s709, &n_list_46_Cons) && u_equals(u_field(u_field(s709, 0), 1), u_mk_bool(0)))) {
    u_value v_rest = u_field(s709, 1);
    t710 = f_check_46_unguarded_1(v_rest);
  }
  if (!t710) {
    u_no_case(s709);
  }
  return t710;
}

void p_check_46_reachable(u_value v_s, u_value v_t, u_value v_before, u_value v_cs) {
  {
    u_value s711 = v_cs;
    int d712 = 0;
    if ((!d712) && u_is_con(s711, &n_list_46_Nil)) {
      d712 = 1;
      {
      }
    }
    if ((!d712) && u_is_con(s711, &n_list_46_Cons)) {
      u_value v_p = u_field(u_field(s711, 0), 0);
      u_value v_open = u_field(u_field(s711, 0), 1);
      u_value v_rest = u_field(s711, 1);
      d712 = 1;
      {
        u_value t713 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e714 = &t713;
        u_value l715 = v_before;
        while (u_is_con(l715, &n_list_46_Cons)) {
          u_value v_q = u_field(l715, 0);
          if (u_truth(u_mk_bool(1))) {
            e714 = u_append(e714, u_mk_con(&n_list_46_Cons, 2, v_q, u_mk_con(&n_list_46_Nil, 0)));
          }
          l715 = u_field(l715, 1);
        }
        if (u_truth(f_prelude_46_not_1(f_check_46_useful_5(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t713, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)))))) {
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
    if (!d712) {
      u_no_case(s711);
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
    u_value s716 = v_p;
    int d717 = 0;
    if ((!d717) && u_is_con(s716, &n_syntax_46_PWild)) {
      d717 = 1;
      {
        u_value t718 = v_p;
        u_value t719 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t718;
        *r_xs = t719;
      }
    }
    if ((!d717) && u_is_con(s716, &n_syntax_46_PLit)) {
      u_value v_v = u_field(s716, 0);
      d717 = 1;
      {
        p_check_46_unify(f_check_46_literal_1(v_v), v_t, r_s);
        u_value t720 = v_p;
        u_value t721 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t720;
        *r_xs = t721;
      }
    }
    if ((!d717) && (u_is_con(s716, &n_syntax_46_PVar) && u_is_con(u_field(s716, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s716, 0), 0);
      u_value v_a = u_field(u_field(s716, 0), 1);
      d717 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam722, 1, *r_s)), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            p_check_46_binders(u_mk_con(&n_syntax_46_PApp, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_t, r_p1, r_xs, r_s);
          }
        } else {
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_u, v_t, r_s);
            u_value t723 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u));
            u_value t724 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), u_mk_con(&n_list_46_Nil, 0));
            *r_p1 = t723;
            *r_xs = t724;
          }
        }
      }
    }
    if ((!d717) && u_is_con(s716, &n_syntax_46_PApp)) {
      u_value v_n = u_field(s716, 0);
      u_value v_args = u_field(s716, 1);
      d717 = 1;
      {
        u_value s726 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam725, 1, *r_s));
        int d727 = 0;
        if ((!d727) && (u_is_con(s726, &n_list_46_Cons) && u_is_con(u_field(s726, 1), &n_list_46_Nil))) {
          u_value v_k1 = u_field(s726, 0);
          d727 = 1;
          {
            u_value t728 = v_k1;
            v_k = t728;
          }
        }
        if ((!d727) && u_is_con(s726, &n_list_46_Nil)) {
          d727 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d727) {
          u_value v_ks = s726;
          d727 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ks), "check.TypeError");
          }
        }
        if (!d727) {
          u_no_case(s726);
        }
        u_value s729 = f_ctx_46_constr_of_2(f_check_46_State_46_ctx_1(*r_s), v_k);
        int d730 = 0;
        if ((!d730) && u_is_con(s729, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s729, 0), 0);
          u_value v_ks = u_field(u_field(s729, 0), 1);
          u_value v_r = u_field(u_field(s729, 0), 2);
          d730 = 1;
          {
            p_check_46_same_length(f_list_46_length_1(v_ks), f_list_46_length_1(v_args));
            p_check_46_instance(v_ps, &v_m, r_s);
            p_check_46_unify(f_check_46_subst_2(v_r, v_m), v_t, r_s);
            u_value t731 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e732 = &t731;
            u_value l733 = v_ks;
            while (u_is_con(l733, &n_list_46_Cons)) {
              u_value v_kt = u_field(l733, 0);
              if (u_truth(u_mk_bool(1))) {
                e732 = u_append(e732, f_check_46_subst_2(v_kt, v_m));
              }
              l733 = u_field(l733, 1);
            }
            p_check_46_binder_list(v_args, t731, &v_qs, r_xs, r_s);
            u_value t734 = u_mk_con(&n_syntax_46_PApp, 2, v_k, v_qs);
            *r_p1 = t734;
          }
        }
        if ((!d730) && u_is_con(s729, &n_option_46_None)) {
          d730 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d730) {
          u_no_case(s729);
        }
      }
    }
    if ((!d717) && u_is_con(s716, &n_syntax_46_PTuple)) {
      u_value v_items = u_field(s716, 0);
      d717 = 1;
      {
        p_check_46_fresh_list(v_items, &v_ts, r_s);
        p_check_46_unify(u_mk_con(&n_syntax_46_TTuple, 1, v_ts), v_t, r_s);
        p_check_46_binder_list(v_items, v_ts, &v_qs, r_xs, r_s);
        u_value t735 = u_mk_con(&n_syntax_46_PTuple, 1, v_qs);
        *r_p1 = t735;
      }
    }
    if (!d717) {
      u_no_case(s716);
    }
  }
}

void p_check_46_binder_list(u_value v_ps, u_value v_ts, u_value *r_qs, u_value *r_xs, u_value *r_s) {
  {
    u_value v_q = NULL;
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value v_zs = NULL;
    u_value s736 = u_mk_tuple(2, v_ps, v_ts);
    int d737 = 0;
    if ((!d737) && (u_is_con(u_field(s736, 0), &n_list_46_Cons) && u_is_con(u_field(s736, 1), &n_list_46_Cons))) {
      u_value v_p = u_field(u_field(s736, 0), 0);
      u_value v_more = u_field(u_field(s736, 0), 1);
      u_value v_t = u_field(u_field(s736, 1), 0);
      u_value v_ts1 = u_field(u_field(s736, 1), 1);
      d737 = 1;
      {
        p_check_46_binders(v_p, v_t, &v_q, &v_ys, r_s);
        p_check_46_binder_list(v_more, v_ts1, &v_rest, &v_zs, r_s);
        u_value t738 = u_mk_con(&n_list_46_Cons, 2, v_q, v_rest);
        u_value t739 = f_list_46__43__43__2(v_ys, v_zs);
        *r_qs = t738;
        *r_xs = t739;
      }
    }
    if ((!d737) && (u_is_con(u_field(s736, 0), &n_list_46_Nil) && u_is_con(u_field(s736, 1), &n_list_46_Nil))) {
      d737 = 1;
      {
        u_value t740 = u_mk_con(&n_list_46_Nil, 0);
        u_value t741 = u_mk_con(&n_list_46_Nil, 0);
        *r_qs = t740;
        *r_xs = t741;
      }
    }
    if (!d737) {
      d737 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_ps)), "check.TypeError");
      }
    }
    if (!d737) {
      u_no_case(s736);
    }
  }
}

void p_check_46_variable(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s742 = v_x;
    int d743 = 0;
    if ((!d743) && u_is_con(s742, &n_syntax_46_Var)) {
      u_value v_n = u_field(s742, 0);
      u_value v_a = u_field(s742, 1);
      d743 = 1;
      {
        if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, v_assignable)))) {
          {
            u_raise(u_mk_con(&n_check_46_NotAssignable, 1, v_n), "check.TypeError");
          }
        }
        u_value s744 = f_list_46_lookup_2(v_n, v_env);
        int d745 = 0;
        if ((!d745) && u_is_con(s744, &n_option_46_Some)) {
          u_value v_t = u_field(u_field(s744, 0), 1);
          d745 = 1;
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_t, v_u, r_s);
            u_value t746 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_t);
            *r_y = t746;
          }
        }
        if ((!d745) && u_is_con(s744, &n_option_46_None)) {
          d745 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
          }
        }
        if (!d745) {
          u_no_case(s744);
        }
      }
    }
    if (!d743) {
      u_no_case(s742);
    }
  }
}

void p_check_46_variables(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s747 = v_xs;
    int d748 = 0;
    if ((!d748) && u_is_con(s747, &n_list_46_Nil)) {
      d748 = 1;
      {
        u_value t749 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t749;
      }
    }
    if ((!d748) && u_is_con(s747, &n_list_46_Cons)) {
      u_value v_x = u_field(s747, 0);
      u_value v_more = u_field(s747, 1);
      d748 = 1;
      {
        p_check_46_variable(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_variables(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t750 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t750;
      }
    }
    if (!d748) {
      u_no_case(s747);
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
    u_value s751 = v_x;
    int d752 = 0;
    if ((!d752) && u_is_con(s751, &n_syntax_46_SSkip)) {
      d752 = 1;
      {
        u_value t753 = v_x;
        *r_y = t753;
      }
    }
    if ((!d752) && u_is_con(s751, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s751, 0);
      d752 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)));
        p_check_46_variables(v_env, v_assignable, f_list_46_firsts_1(v_pairs), &v_ys, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_pairs), f_syntax_46_types_1(v_ys), &v_es, r_s);
        u_value t754 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_zip_2(v_ys, v_es));
        *r_y = t754;
      }
    }
    if ((!d752) && u_is_con(s751, &n_syntax_46_SBlock)) {
      u_value v_locals = u_field(s751, 0);
      u_value v_body = u_field(s751, 1);
      d752 = 1;
      {
        p_check_46_untyped(v_locals, f_check_46_initialized_1(v_body));
        p_check_46_formals(v_locals, &v_ys, r_s);
        p_check_46_statements(f_list_46__43__43__2(f_check_46_bind_1(v_ys), v_env), f_list_46__43__43__2(f_syntax_46_names_1(v_ys), v_assignable), v_body, &v_xs, r_s);
        u_value t755 = u_mk_con(&n_syntax_46_SBlock, 2, v_ys, v_xs);
        *r_y = t755;
      }
    }
    if ((!d752) && u_is_con(s751, &n_syntax_46_SIf)) {
      u_value v_cond = u_field(s751, 0);
      u_value v_left = u_field(s751, 1);
      u_value v_right = u_field(s751, 2);
      d752 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_left, &v_l, r_s);
        p_check_46_statement(v_env, v_assignable, v_right, &v_r, r_s);
        u_value t756 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
        *r_y = t756;
      }
    }
    if ((!d752) && u_is_con(s751, &n_syntax_46_SWhile)) {
      u_value v_cond = u_field(s751, 0);
      u_value v_body = u_field(s751, 1);
      d752 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        u_value t757 = u_mk_con(&n_syntax_46_SWhile, 2, v_c, v_l);
        *r_y = t757;
      }
    }
    if ((!d752) && u_is_con(s751, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s751, 0);
      u_value v_ins = u_field(s751, 1);
      u_value v_outs = u_field(s751, 2);
      u_value v_refs = u_field(s751, 3);
      d752 = 1;
      {
        p_check_46_call_proc(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d752) && u_is_con(s751, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s751, 0);
      u_value v_cases = u_field(s751, 1);
      d752 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_cases, v_t, &v_bs, r_s);
        u_value t758 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e759 = &t758;
        u_value l760 = v_bs;
        while (u_is_con(l760, &n_list_46_Cons)) {
          u_value v_br = u_field(l760, 0);
          if (u_truth(u_mk_bool(1))) {
            e759 = u_append(e759, f_check_46_case_of_1(v_br));
          }
          l760 = u_field(l760, 1);
        }
        p_check_46_coverage(*r_s, v_t, t758);
        u_value t761 = u_mk_con(&n_syntax_46_SMatch, 2, v_c, v_bs);
        *r_y = t761;
      }
    }
    if ((!d752) && u_is_con(s751, &n_syntax_46_SAssert)) {
      u_value v_cond = u_field(s751, 0);
      d752 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        u_value t762 = u_mk_con(&n_syntax_46_SAssert, 1, v_c);
        *r_y = t762;
      }
    }
    if ((!d752) && u_is_con(s751, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s751, 0);
      u_value v_a = u_field(s751, 1);
      d752 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_unify(v_t, v_u, r_s);
        u_value t763 = u_mk_con(&n_syntax_46_SRaise, 2, v_c, v_t);
        *r_y = t763;
      }
    }
    if ((!d752) && u_is_con(s751, &n_syntax_46_STry)) {
      u_value v_body = u_field(s751, 0);
      u_value v_handlers = u_field(s751, 1);
      d752 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_handlers, &v_hs, r_s);
        u_value t764 = u_mk_con(&n_syntax_46_STry, 2, v_l, v_hs);
        *r_y = t764;
      }
    }
    if (!d752) {
      u_no_case(s751);
    }
  }
}

u_value f_check_46_initialized_1(u_value v_xs) {
  u_value s765 = v_xs;
  u_value t766 = NULL;
  if ((!t766) && (u_is_con(s765, &n_list_46_Cons) && (u_is_con(u_field(s765, 0), &n_syntax_46_SAssign) && (u_is_con(u_field(u_field(s765, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s765, 0), 0), 1), &n_list_46_Nil))))) {
    u_value v_x = u_field(u_field(u_field(u_field(s765, 0), 0), 0), 0);
    u_value v_rest = u_field(s765, 1);
    t766 = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), f_check_46_initialized_1(v_rest));
  }
  if (!t766) {
    t766 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t766) {
    u_no_case(s765);
  }
  return t766;
}

void p_check_46_untyped(u_value v_xs, u_value v_inits) {
  {
    u_value s767 = v_xs;
    int d768 = 0;
    if ((!d768) && u_is_con(s767, &n_list_46_Nil)) {
      d768 = 1;
      {
      }
    }
    if ((!d768) && (u_is_con(s767, &n_list_46_Cons) && u_is_con(u_field(s767, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s767, 0), 0);
      u_value v_t = u_field(u_field(s767, 0), 1);
      u_value v_rest = u_field(s767, 1);
      d768 = 1;
      {
        u_value t769 = f_check_46_holes_1(v_t);
        if (u_truth(t769)) {
          t769 = f_prelude_46_not_1(f_list_46_contains_2(v_n, v_inits));
        }
        if (u_truth(t769)) {
          {
            u_raise(u_mk_con(&n_check_46_MissingType, 1, v_n), "check.TypeError");
          }
        }
        p_check_46_untyped(v_rest, v_inits);
      }
    }
    if (!d768) {
      u_no_case(s767);
    }
  }
}

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s770 = v_xs;
    int d771 = 0;
    if ((!d771) && u_is_con(s770, &n_list_46_Nil)) {
      d771 = 1;
      {
        u_value t772 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t772;
      }
    }
    if ((!d771) && u_is_con(s770, &n_list_46_Cons)) {
      u_value v_x = u_field(s770, 0);
      u_value v_more = u_field(s770, 1);
      d771 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_statements(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t773 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t773;
      }
    }
    if (!d771) {
      u_no_case(s770);
    }
  }
}

u_value f_check_46_os_procs_1(u_value v_c) {
  u_value t778;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_774 = U_LIT("os_args");
    static struct u_object l_os_read_775 = U_LIT("os_read");
    static struct u_object l_os_write_776 = U_LIT("os_write");
    static struct u_object l_os_library_777 = U_LIT("os_library");
    t778 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_774), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_775), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_776), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_777), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)))));
  } else {
    t778 = u_mk_con(&n_list_46_Nil, 0);
  }
  return t778;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t779 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e780 = &t779;
  static struct u_object l_Program_781 = U_LIT("Program");
  u_value l782 = f_ctx_46_names_of_2(v_c, &l_Program_781);
  while (u_is_con(l782, &n_list_46_Cons)) {
    u_value v_d = u_field(l782, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e780 = u_append(e780, v_d);
    }
    l782 = u_field(l782, 1);
  }
  u_value s783 = t779;
  u_value t784 = NULL;
  if ((!t784) && u_is_con(s783, &n_list_46_Cons)) {
    u_value v_d = u_field(s783, 0);
    static struct u_object l_read_785 = U_LIT("read");
    static struct u_object l_library_786 = U_LIT("library");
    t784 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_785), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_786), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t784) {
    t784 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t784) {
    u_no_case(s783);
  }
  return t784;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value s787 = f_list_46_lookup_2(v_p, v_env);
    int d788 = 0;
    if ((!d788) && u_is_con(s787, &n_option_46_Some)) {
      u_value v_sc = u_field(s787, 0);
      d788 = 1;
      {
        p_check_46_call_variable(v_env, v_assignable, v_p, v_sc, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d788) && u_is_con(s787, &n_option_46_None)) {
      d788 = 1;
      {
        p_check_46_call_declared(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if (!d788) {
      u_no_case(s787);
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
    u_value t789 = u_mk_con(&n_syntax_46_SCall, 4, v_p, v_ins1, v_outs1, v_refs1);
    *r_y = t789;
  }
}

void p_check_46_call_declared(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s791 = f_check_46_visible_names_3(*r_s, v_p, u_mk_closure(lam790, 1, *r_s));
    int d792 = 0;
    if ((!d792) && (u_is_con(s791, &n_list_46_Cons) && u_is_con(u_field(s791, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s791, 0);
      d792 = 1;
      {
        u_value t793 = v_q1;
        v_q = t793;
      }
    }
    if ((!d792) && u_is_con(s791, &n_list_46_Nil)) {
      d792 = 1;
      {
        u_value t794 = v_p;
        v_q = t794;
      }
    }
    if (!d792) {
      u_value v_qs = s791;
      d792 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d792) {
      u_no_case(s791);
    }
    u_value s795 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_State_46_ctx_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_State_46_ctx_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_State_46_ctx_1(*r_s)))));
    int d796 = 0;
    if ((!d796) && u_is_con(s795, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s795, 0), 0);
      u_value v_is = u_field(u_field(s795, 0), 1);
      u_value v_os = u_field(u_field(s795, 0), 2);
      u_value v_rs = u_field(u_field(s795, 0), 3);
      d796 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t797 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e798 = &t797;
        u_value l799 = v_is;
        while (u_is_con(l799, &n_list_46_Cons)) {
          u_value v_t = u_field(l799, 0);
          if (u_truth(u_mk_bool(1))) {
            e798 = u_append(e798, f_check_46_subst_2(v_t, v_m));
          }
          l799 = u_field(l799, 1);
        }
        p_check_46_checks(v_env, v_ins, t797, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t800 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e801 = &t800;
        u_value l802 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l802, &n_list_46_Cons)) {
          u_value v_t = u_field(l802, 0);
          if (u_truth(u_mk_bool(1))) {
            e801 = u_append(e801, f_check_46_subst_2(v_t, v_m));
          }
          l802 = u_field(l802, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t800, r_s);
        u_value t803 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t803;
      }
    }
    if ((!d796) && u_is_con(s795, &n_option_46_None)) {
      d796 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d796) {
      u_no_case(s795);
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
    u_value s804 = v_cases;
    int d805 = 0;
    if ((!d805) && u_is_con(s804, &n_list_46_Nil)) {
      d805 = 1;
      {
        u_value t806 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t806;
      }
    }
    if ((!d805) && (u_is_con(s804, &n_list_46_Cons) && u_is_con(u_field(s804, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s804, 0), 0);
      u_value v_g = u_field(u_field(s804, 0), 1);
      u_value v_b = u_field(u_field(s804, 0), 2);
      u_value v_more = u_field(s804, 1);
      d805 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t807 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t807;
      }
    }
    if (!d805) {
      u_no_case(s804);
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
    u_value s808 = v_hs;
    int d809 = 0;
    if ((!d809) && u_is_con(s808, &n_list_46_Nil)) {
      d809 = 1;
      {
        u_value t810 = u_mk_con(&n_list_46_Nil, 0);
        *r_hs1 = t810;
      }
    }
    if ((!d809) && (u_is_con(s808, &n_list_46_Cons) && u_is_con(u_field(s808, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s808, 0), 0);
      u_value v_p = u_field(u_field(s808, 0), 1);
      u_value v_g = u_field(u_field(s808, 0), 2);
      u_value v_b = u_field(u_field(s808, 0), 3);
      u_value v_more = u_field(s808, 1);
      d809 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t811 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t811;
      }
    }
    if (!d809) {
      u_no_case(s808);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t812 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t812)) {
      t812 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t813 = t812;
    if (!u_truth(t813)) {
      t813 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t814 = t813;
    if (!u_truth(t814)) {
      t814 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n))), u_mk_con(&n_list_46_Nil, 0));
    }
    if (u_truth(t814)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t815 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t815)) {
      t815 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t816 = t815;
    if (!u_truth(t816)) {
      t816 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t816)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_817 = U_LIT("Bool");
    static struct u_object l_Int_818 = U_LIT("Int");
    static struct u_object l_Str_819 = U_LIT("Str");
    static struct u_object l_Char_820 = U_LIT("Char");
    static struct u_object l__45__62__821 = U_LIT("->");
    u_value t822 = f_list_46_contains_2(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_817, u_mk_con(&n_list_46_Cons, 2, &l_Int_818, u_mk_con(&n_list_46_Cons, 2, &l_Str_819, u_mk_con(&n_list_46_Cons, 2, &l_Char_820, u_mk_con(&n_list_46_Cons, 2, &l__45__62__821, u_mk_con(&n_list_46_Nil, 0)))))));
    if (!u_truth(t822)) {
      t822 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t823 = t822;
    if (!u_truth(t823)) {
      t823 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t823)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t824 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e825 = &t824;
    u_value l826 = v_ts;
    while (u_is_con(l826, &n_list_46_Cons)) {
      u_value v_t = u_field(l826, 0);
      if (u_truth(u_mk_bool(1))) {
        e825 = u_append(e825, f_check_46_free_1(v_t));
      }
      l826 = u_field(l826, 1);
    }
    u_value t827 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t824)), f_check_46_params_1(v_ps));
    v_undeclared = t827;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, u_mk_con(&n_list_46_Nil, 0)))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
  {
    u_value s828 = v_cs;
    int d829 = 0;
    if ((!d829) && u_is_con(s828, &n_list_46_Nil)) {
      d829 = 1;
      {
      }
    }
    if ((!d829) && (u_is_con(s828, &n_list_46_Cons) && u_is_con(u_field(u_field(s828, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s828, 0), 0);
      u_value v_n = u_field(u_field(u_field(s828, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s828, 0), 1), 1);
      u_value v_rest = u_field(s828, 1);
      d829 = 1;
      {
        u_value t830 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t830;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        u_value t831 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t831;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d829) && u_is_con(s828, &n_list_46_Cons)) {
      u_value v_rest = u_field(s828, 1);
      d829 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d829) {
      u_no_case(s828);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s832 = v_cs;
    int d833 = 0;
    if ((!d833) && u_is_con(s832, &n_list_46_Nil)) {
      d833 = 1;
      {
      }
    }
    if ((!d833) && (u_is_con(s832, &n_list_46_Cons) && u_is_con(u_field(u_field(s832, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s832, 0), 0);
      u_value v_n = u_field(u_field(u_field(s832, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s832, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s832, 0), 1), 2);
      u_value v_rest = u_field(s832, 1);
      d833 = 1;
      {
        u_value t834 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t834;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)));
        u_value t835 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_State_46_ctx_1(*r_s), v_n, v_ps, v_u));
        *r_s = t835;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d833) && u_is_con(s832, &n_list_46_Cons)) {
      u_value v_rest = u_field(s832, 1);
      d833 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d833) {
      u_no_case(s832);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
  {
    u_value s836 = v_cs;
    int d837 = 0;
    if ((!d837) && u_is_con(s836, &n_list_46_Nil)) {
      d837 = 1;
      {
      }
    }
    if ((!d837) && (u_is_con(s836, &n_list_46_Cons) && u_is_con(u_field(u_field(s836, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s836, 0), 0);
      u_value v_n = u_field(u_field(u_field(s836, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s836, 0), 1), 1);
      u_value v_ks = u_field(u_field(u_field(s836, 0), 1), 2);
      u_value v_rest = u_field(s836, 1);
      d837 = 1;
      {
        u_value t838 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t838;
        p_check_46_unique_ids(v_ps);
        u_value t839 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e840 = &t839;
        u_value l841 = v_ks;
        while (u_is_con(l841, &n_list_46_Cons)) {
          u_value v_k = u_field(l841, 0);
          if (u_truth(u_mk_bool(1))) {
            e840 = u_append(e840, f_syntax_46_Constr_46_name_1(v_k));
          }
          l841 = u_field(l841, 1);
        }
        u_value t842 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_State_46_ctx_1(*r_s), v_n, t839));
        *r_s = t842;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d837) && u_is_con(s836, &n_list_46_Cons)) {
      u_value v_rest = u_field(s836, 1);
      d837 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d837) {
      u_no_case(s836);
    }
  }
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s843 = v_ks;
    int d844 = 0;
    if ((!d844) && u_is_con(s843, &n_list_46_Nil)) {
      d844 = 1;
      {
      }
    }
    if ((!d844) && (u_is_con(s843, &n_list_46_Cons) && u_is_con(u_field(s843, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s843, 0), 0);
      u_value v_args = u_field(u_field(s843, 0), 1);
      u_value v_rest = u_field(s843, 1);
      d844 = 1;
      {
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t845 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_State_46_ctx_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t845;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d844) {
      u_no_case(s843);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s846 = v_xss;
  u_value t847 = NULL;
  if ((!t847) && u_is_con(s846, &n_list_46_Nil)) {
    t847 = v_r;
  }
  if ((!t847) && u_is_con(s846, &n_list_46_Cons)) {
    u_value v_xs = u_field(s846, 0);
    u_value v_rest = u_field(s846, 1);
    t847 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t847) {
    u_no_case(s846);
  }
  return t847;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s848 = v_xss;
  u_value t849 = NULL;
  if ((!t849) && u_is_con(s848, &n_list_46_Nil)) {
    t849 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t849) && u_is_con(s848, &n_list_46_Cons)) {
    u_value v_xs = u_field(s848, 0);
    t849 = v_xs;
  }
  if (!t849) {
    u_no_case(s848);
  }
  return t849;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s850 = v_xss;
    int d851 = 0;
    if ((!d851) && u_is_con(s850, &n_list_46_Nil)) {
      d851 = 1;
      {
        u_value t852 = u_mk_con(&n_list_46_Nil, 0);
        *r_yss = t852;
      }
    }
    if ((!d851) && u_is_con(s850, &n_list_46_Cons)) {
      u_value v_xs = u_field(s850, 0);
      u_value v_more = u_field(s850, 1);
      d851 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t853 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t853;
      }
    }
    if (!d851) {
      u_no_case(s850);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s854 = v_cs;
    int d855 = 0;
    if ((!d855) && u_is_con(s854, &n_list_46_Nil)) {
      d855 = 1;
      {
      }
    }
    if ((!d855) && (u_is_con(s854, &n_list_46_Cons) && u_is_con(u_field(u_field(s854, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s854, 0), 0);
      u_value v_n = u_field(u_field(u_field(s854, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s854, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s854, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s854, 0), 1), 3);
      u_value v_rest = u_field(s854, 1);
      d855 = 1;
      {
        u_value t856 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t856;
        p_check_46_overload(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), u_mk_con(&n_list_46_Nil, 0)));
        u_value t857 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t857;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d855) && u_is_con(s854, &n_list_46_Cons)) {
      u_value v_rest = u_field(s854, 1);
      d855 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d855) {
      u_no_case(s854);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s858 = v_cs;
    int d859 = 0;
    if ((!d859) && u_is_con(s858, &n_list_46_Nil)) {
      d859 = 1;
      {
      }
    }
    if ((!d859) && (u_is_con(s858, &n_list_46_Cons) && u_is_con(u_field(u_field(s858, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s858, 0), 0);
      u_value v_n = u_field(u_field(u_field(s858, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s858, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s858, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s858, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s858, 0), 1), 4);
      u_value v_rest = u_field(s858, 1);
      d859 = 1;
      {
        u_value t860 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t860;
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t861 = f_syntax_46_types_1(v_xs);
        v_ts = t861;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t862 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t862;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d859) && u_is_con(s858, &n_list_46_Cons)) {
      u_value v_rest = u_field(s858, 1);
      d859 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d859) {
      u_no_case(s858);
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
    u_value s863 = v_c;
    int d864 = 0;
    if ((!d864) && u_is_con(s863, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s863, 0);
      u_value v_ps = u_field(s863, 1);
      u_value v_bound = u_field(s863, 2);
      u_value v_res = u_field(s863, 3);
      u_value v_body = u_field(s863, 4);
      d864 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t865 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t865;
      }
    }
    if ((!d864) && u_is_con(s863, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s863, 0);
      u_value v_ps = u_field(s863, 1);
      u_value v_ins = u_field(s863, 2);
      u_value v_outs = u_field(s863, 3);
      u_value v_refs = u_field(s863, 4);
      u_value v_body = u_field(s863, 5);
      d864 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t866 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t866;
      }
    }
    if ((!d864) && u_is_con(s863, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s863, 0);
      d864 = 1;
      {
        p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_b, &v_t, r_s);
        u_value t867 = u_mk_con(&n_syntax_46_CEval, 1, v_b);
        *r_c1 = t867;
      }
    }
    if ((!d864) && u_is_con(s863, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s863, 0);
      u_value v_body = u_field(s863, 1);
      d864 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t868 = u_mk_con(&n_syntax_46_CRun, 2, v_n, v_y);
        *r_c1 = t868;
      }
    }
    if ((!d864) && u_is_con(s863, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s863, 0);
      u_value v_body = u_field(s863, 1);
      d864 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t869 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t869;
      }
    }
    if (!d864) {
      d864 = 1;
      {
        u_value t870 = v_c;
        *r_c1 = t870;
      }
    }
    if (!d864) {
      u_no_case(s863);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s871 = v_cs;
    int d872 = 0;
    if ((!d872) && u_is_con(s871, &n_list_46_Nil)) {
      d872 = 1;
      {
        u_value t873 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t873;
      }
    }
    if ((!d872) && u_is_con(s871, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s871, 0), 0);
      u_value v_c = u_field(u_field(s871, 0), 1);
      u_value v_more = u_field(s871, 1);
      d872 = 1;
      {
        u_value t874 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t874;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t875 = f_check_46_zonk_cmd_2(f_check_46_State_46_sub_1(*r_s), v_c1);
        v_c2 = t875;
        u_value s876 = f_check_46_unknown_in_cmd_1(v_c2);
        int d877 = 0;
        if ((!d877) && u_is_con(s876, &n_option_46_Some)) {
          u_value v_t = u_field(s876, 0);
          d877 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d877) && u_is_con(s876, &n_option_46_None)) {
          d877 = 1;
          {
          }
        }
        if (!d877) {
          u_no_case(s876);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t878 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t878;
      }
    }
    if (!d872) {
      u_no_case(s871);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s879 = v_cs;
  u_value t880 = NULL;
  if ((!t880) && u_is_con(s879, &n_list_46_Nil)) {
    t880 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t880) && (u_is_con(s879, &n_list_46_Cons) && u_is_con(u_field(u_field(s879, 0), 1), &n_syntax_46_CRun))) {
    u_value v_n = u_field(u_field(u_field(s879, 0), 1), 0);
    u_value v_more = u_field(s879, 1);
    t880 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t880) && (u_is_con(s879, &n_list_46_Cons) && u_is_con(u_field(u_field(s879, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s879, 0), 1), 0);
    u_value v_more = u_field(s879, 1);
    t880 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t880) && u_is_con(s879, &n_list_46_Cons)) {
    u_value v_more = u_field(s879, 1);
    t880 = f_check_46_block_names_1(v_more);
  }
  if (!t880) {
    u_no_case(s879);
  }
  return t880;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_s = NULL;
    u_value v_ds = NULL;
    u_value v_cs = NULL;
    u_value s881 = v_p;
    int d882 = 0;
    if ((!d882) && u_is_con(s881, &n_syntax_46_Program)) {
      u_value v_cmds = u_field(s881, 0);
      d882 = 1;
      {
        u_value t883 = f_check_46_flatten_2(v_main, v_cmds);
        v_ds = t883;
        u_value t884 = f_check_46_start_2(v_main, f_check_46_visibility_2(v_ds, v_main));
        v_s = t884;
        p_check_46_declare_datas(v_ds, &v_s);
        p_check_46_declare_sorts(v_ds, &v_s);
        p_check_46_declare_constrs(v_ds, &v_s);
        p_check_46_declare_funs(v_ds, &v_s);
        p_check_46_declare_procs(v_ds, &v_s);
        p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
        p_check_46_commands(v_ds, &v_cs, &v_s);
        u_value t885 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
        *r_q = t885;
      }
    }
    if (!d882) {
      u_no_case(s881);
    }
  }
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_886 = U_LIT("main");
    u_value t887 = f_check_46_start_2(&l_main_886, u_mk_con(&n_list_46_Nil, 0));
    v_s = t887;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t888 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(v_s), v_u);
    *r_t = t888;
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
  static struct u_object l_List_889 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_889), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_890 = U_LIT("list");
  static struct u_object l_List_891 = U_LIT("List");
  static struct u_object l_A_892 = U_LIT("A");
  static struct u_object l_Nil_893 = U_LIT("Nil");
  static struct u_object l_Cons_894 = U_LIT("Cons");
  static struct u_object l_A_895 = U_LIT("A");
  static struct u_object l_A_896 = U_LIT("A");
  static struct u_object l_list_897 = U_LIT("list");
  static struct u_object l_len_898 = U_LIT("len");
  static struct u_object l_A_899 = U_LIT("A");
  static struct u_object l_xs_900 = U_LIT("xs");
  static struct u_object l_A_901 = U_LIT("A");
  static struct u_object l_xs_902 = U_LIT("xs");
  static struct u_object l_Nil_903 = U_LIT("Nil");
  static struct u_object l_Cons_904 = U_LIT("Cons");
  static struct u_object l_ys_905 = U_LIT("ys");
  static struct u_object l__43__906 = U_LIT("+");
  static struct u_object l_len_907 = U_LIT("len");
  static struct u_object l_ys_908 = U_LIT("ys");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_890, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_891), u_mk_con(&n_list_46_Cons, 2, &l_A_892, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_893), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_894), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_895), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_896)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_897), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_898), u_mk_con(&n_list_46_Cons, 2, &l_A_899, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_900), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_901))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_902), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_903), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_904), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_905), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__906, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_907, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_908), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s909 = v_t;
  u_value t910 = NULL;
  if ((!t910) && u_is_con(s909, &n_c_46_CtVoid)) {
    static struct u_object l_void_911 = U_LIT("void");
    t910 = &l_void_911;
  }
  if ((!t910) && u_is_con(s909, &n_c_46_CtInt)) {
    static struct u_object l_int_912 = U_LIT("int");
    t910 = &l_int_912;
  }
  if ((!t910) && u_is_con(s909, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s909, 0);
    t910 = v_n;
  }
  if ((!t910) && (u_is_con(s909, &n_c_46_CtPtr) && u_is_con(u_field(s909, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s909, 0), 0);
    static struct u_object l__42__913 = U_LIT("*");
    t910 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__913);
  }
  if ((!t910) && u_is_con(s909, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s909, 0);
    static struct u_object l__32__42__914 = U_LIT(" *");
    t910 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__914);
  }
  if (!t910) {
    u_no_case(s909);
  }
  return t910;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s915 = v_t;
  u_value t916 = NULL;
  if ((!t916) && u_is_con(s915, &n_c_46_CtPtr)) {
    t916 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t916) {
    static struct u_object l__32__917 = U_LIT(" ");
    t916 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__917, v_x));
  }
  if (!t916) {
    u_no_case(s915);
  }
  return t916;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s918 = v_e;
  u_value t919 = NULL;
  if ((!t919) && u_is_con(s918, &n_c_46_CeVar)) {
    u_value v_x = u_field(s918, 0);
    t919 = v_x;
  }
  if ((!t919) && u_is_con(s918, &n_c_46_CeNum)) {
    u_value v_n = u_field(s918, 0);
    t919 = u_str_show(v_n);
  }
  if ((!t919) && u_is_con(s918, &n_c_46_CeStr)) {
    u_value v_s = u_field(s918, 0);
    t919 = u_str_quote(v_s);
  }
  if ((!t919) && u_is_con(s918, &n_c_46_CeCall)) {
    u_value v_f = u_field(s918, 0);
    u_value v_args = u_field(s918, 1);
    static struct u_object l__40__920 = U_LIT("(");
    static struct u_object l__44__32__921 = U_LIT(", ");
    u_value t922 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e923 = &t922;
    u_value l924 = v_args;
    while (u_is_con(l924, &n_list_46_Cons)) {
      u_value v_a = u_field(l924, 0);
      if (u_truth(u_mk_bool(1))) {
        e923 = u_append(e923, f_c_46_c_expr_1(v_a));
      }
      l924 = u_field(l924, 1);
    }
    static struct u_object l__41__925 = U_LIT(")");
    t919 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__920, u_concat(u_str_join(&l__44__32__921, t922), &l__41__925)));
  }
  if ((!t919) && u_is_con(s918, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s918, 0);
    u_value v_a = u_field(s918, 1);
    t919 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t919) && u_is_con(s918, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s918, 0);
    u_value v_a = u_field(s918, 1);
    u_value v_b = u_field(s918, 2);
    static struct u_object l__32__926 = U_LIT(" ");
    static struct u_object l__32__927 = U_LIT(" ");
    t919 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__926, u_concat(v_o, u_concat(&l__32__927, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t919) && u_is_con(s918, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s918, 0);
    u_value v_i = u_field(s918, 1);
    static struct u_object l__91__928 = U_LIT("[");
    static struct u_object l__93__929 = U_LIT("]");
    t919 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__928, u_concat(f_c_46_c_expr_1(v_i), &l__93__929)));
  }
  if ((!t919) && u_is_con(s918, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s918, 0);
    u_value v_f = u_field(s918, 1);
    static struct u_object l__45__62__930 = U_LIT("->");
    t919 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__930, v_f));
  }
  if ((!t919) && u_is_con(s918, &n_c_46_CeCast)) {
    u_value v_t = u_field(s918, 0);
    u_value v_a = u_field(s918, 1);
    static struct u_object l__40__931 = U_LIT("(");
    static struct u_object l__41__932 = U_LIT(")");
    t919 = u_concat(&l__40__931, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__932, f_c_46_c_operand_1(v_a))));
  }
  if ((!t919) && u_is_con(s918, &n_c_46_CeCond)) {
    u_value v_c = u_field(s918, 0);
    u_value v_a = u_field(s918, 1);
    u_value v_b = u_field(s918, 2);
    static struct u_object l__32__63__32__933 = U_LIT(" ? ");
    static struct u_object l__32__58__32__934 = U_LIT(" : ");
    t919 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__933, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__934, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t919) && u_is_con(s918, &n_c_46_CeInit)) {
    u_value v_es = u_field(s918, 0);
    static struct u_object l__123__935 = U_LIT("{");
    static struct u_object l__44__32__936 = U_LIT(", ");
    u_value t937 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e938 = &t937;
    u_value l939 = v_es;
    while (u_is_con(l939, &n_list_46_Cons)) {
      u_value v_a = u_field(l939, 0);
      if (u_truth(u_mk_bool(1))) {
        e938 = u_append(e938, f_c_46_c_expr_1(v_a));
      }
      l939 = u_field(l939, 1);
    }
    static struct u_object l__125__940 = U_LIT("}");
    t919 = u_concat(&l__123__935, u_concat(u_str_join(&l__44__32__936, t937), &l__125__940));
  }
  if (!t919) {
    u_no_case(s918);
  }
  return t919;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s941 = v_e;
  u_value t942 = NULL;
  if ((!t942) && u_is_con(s941, &n_c_46_CeVar)) {
    t942 = f_c_46_c_expr_1(v_e);
  }
  if ((!t942) && u_is_con(s941, &n_c_46_CeNum)) {
    u_value v_n = u_field(s941, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t942 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t942) && u_is_con(s941, &n_c_46_CeStr)) {
    t942 = f_c_46_c_expr_1(v_e);
  }
  if ((!t942) && u_is_con(s941, &n_c_46_CeCall)) {
    t942 = f_c_46_c_expr_1(v_e);
  }
  if ((!t942) && u_is_con(s941, &n_c_46_CeIndex)) {
    t942 = f_c_46_c_expr_1(v_e);
  }
  if ((!t942) && u_is_con(s941, &n_c_46_CeArrow)) {
    t942 = f_c_46_c_expr_1(v_e);
  }
  if (!t942) {
    static struct u_object l__40__943 = U_LIT("(");
    static struct u_object l__41__944 = U_LIT(")");
    t942 = u_concat(&l__40__943, u_concat(f_c_46_c_expr_1(v_e), &l__41__944));
  }
  if (!t942) {
    u_no_case(s941);
  }
  return t942;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s945 = v_s;
  u_value t946 = NULL;
  if ((!t946) && u_is_con(s945, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s945, 0);
    static struct u_object l__59__947 = U_LIT(";");
    t946 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__947)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t946) && (u_is_con(s945, &n_c_46_CsDecl) && u_is_con(u_field(s945, 2), &n_option_46_None))) {
    u_value v_t = u_field(s945, 0);
    u_value v_x = u_field(s945, 1);
    static struct u_object l__59__948 = U_LIT(";");
    t946 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__948)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t946) && (u_is_con(s945, &n_c_46_CsDecl) && u_is_con(u_field(s945, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s945, 0);
    u_value v_x = u_field(s945, 1);
    u_value v_e = u_field(u_field(s945, 2), 0);
    static struct u_object l__32__61__32__949 = U_LIT(" = ");
    static struct u_object l__59__950 = U_LIT(";");
    t946 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__949, u_concat(f_c_46_c_expr_1(v_e), &l__59__950)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t946) && u_is_con(s945, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s945, 0);
    u_value v_e = u_field(s945, 1);
    static struct u_object l__32__61__32__951 = U_LIT(" = ");
    static struct u_object l__59__952 = U_LIT(";");
    t946 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__951, u_concat(f_c_46_c_expr_1(v_e), &l__59__952)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t946) && (u_is_con(s945, &n_c_46_CsIf) && u_is_con(u_field(s945, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s945, 0);
    u_value v_l = u_field(s945, 1);
    static struct u_object l_if_32__40__953 = U_LIT("if (");
    static struct u_object l__41__32__123__954 = U_LIT(") {");
    static struct u_object l__32__32__955 = U_LIT("  ");
    static struct u_object l__125__956 = U_LIT("}");
    t946 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__953, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__954))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__955), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__956), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t946) && u_is_con(s945, &n_c_46_CsIf)) {
    u_value v_c = u_field(s945, 0);
    u_value v_l = u_field(s945, 1);
    u_value v_r = u_field(s945, 2);
    static struct u_object l_if_32__40__957 = U_LIT("if (");
    static struct u_object l__41__32__123__958 = U_LIT(") {");
    static struct u_object l__32__32__959 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__960 = U_LIT("} else {");
    static struct u_object l__32__32__961 = U_LIT("  ");
    static struct u_object l__125__962 = U_LIT("}");
    t946 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__957, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__958))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__959), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__960), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__961), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__962), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t946) && u_is_con(s945, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s945, 0);
    u_value v_b = u_field(s945, 1);
    static struct u_object l_while_32__40__963 = U_LIT("while (");
    static struct u_object l__41__32__123__964 = U_LIT(") {");
    static struct u_object l__32__32__965 = U_LIT("  ");
    static struct u_object l__125__966 = U_LIT("}");
    t946 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__963, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__964))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__965), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__966), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t946) && (u_is_con(s945, &n_c_46_CsReturn) && u_is_con(u_field(s945, 0), &n_option_46_None))) {
    static struct u_object l_return_59__967 = U_LIT("return;");
    t946 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__967), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t946) && (u_is_con(s945, &n_c_46_CsReturn) && u_is_con(u_field(s945, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s945, 0), 0);
    static struct u_object l_return_32__968 = U_LIT("return ");
    static struct u_object l__59__969 = U_LIT(";");
    t946 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__968, u_concat(f_c_46_c_expr_1(v_e), &l__59__969))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t946) && u_is_con(s945, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s945, 0);
    static struct u_object l__123__970 = U_LIT("{");
    static struct u_object l__32__32__971 = U_LIT("  ");
    static struct u_object l__125__972 = U_LIT("}");
    t946 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__970), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__971), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__972), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t946) {
    u_no_case(s945);
  }
  return t946;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t973 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e974 = &t973;
  u_value l975 = v_ss;
  while (u_is_con(l975, &n_list_46_Cons)) {
    u_value v_s = u_field(l975, 0);
    if (u_truth(u_mk_bool(1))) {
      e974 = u_append(e974, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l975 = u_field(l975, 1);
  }
  return f_list_46_concat_1(t973);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s976 = v_ps;
  u_value t977 = NULL;
  if ((!t977) && u_is_con(s976, &n_list_46_Nil)) {
    static struct u_object l_void_978 = U_LIT("void");
    t977 = &l_void_978;
  }
  if (!t977) {
    static struct u_object l__44__32__979 = U_LIT(", ");
    u_value t980 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e981 = &t980;
    u_value l982 = v_ps;
    while (u_is_con(l982, &n_list_46_Cons)) {
      u_value v_p = u_field(l982, 0);
      if (u_truth(u_mk_bool(1))) {
        e981 = u_append(e981, f_c_46_c_param_1(v_p));
      }
      l982 = u_field(l982, 1);
    }
    t977 = u_str_join(&l__44__32__979, t980);
  }
  if (!t977) {
    u_no_case(s976);
  }
  return t977;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s983 = v_p;
  u_value t984 = NULL;
  if (!t984) {
    u_value v_t = u_field(s983, 0);
    u_value v_x = u_field(s983, 1);
    t984 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t984) {
    u_no_case(s983);
  }
  return t984;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s985 = v_d;
  u_value t986 = NULL;
  if ((!t986) && u_is_con(s985, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s985, 0);
    static struct u_object l__35_include_32__987 = U_LIT("#include ");
    t986 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__987, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t986) && u_is_con(s985, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s985, 0);
    u_value v_f = u_field(s985, 1);
    u_value v_ps = u_field(s985, 2);
    static struct u_object l__40__988 = U_LIT("(");
    static struct u_object l__41__59__989 = U_LIT(");");
    t986 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__988, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__989))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t986) && u_is_con(s985, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s985, 0);
    u_value v_f = u_field(s985, 1);
    u_value v_ps = u_field(s985, 2);
    u_value v_b = u_field(s985, 3);
    static struct u_object l__40__990 = U_LIT("(");
    static struct u_object l__41__32__123__991 = U_LIT(") {");
    static struct u_object l__32__32__992 = U_LIT("  ");
    static struct u_object l__125__993 = U_LIT("}");
    t986 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__990, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__991))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__992, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__993, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t986) && (u_is_con(s985, &n_c_46_CdGlobal) && u_is_con(u_field(s985, 2), &n_option_46_None))) {
    u_value v_t = u_field(s985, 0);
    u_value v_x = u_field(s985, 1);
    static struct u_object l__59__994 = U_LIT(";");
    t986 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__994), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t986) && (u_is_con(s985, &n_c_46_CdGlobal) && u_is_con(u_field(s985, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s985, 0);
    u_value v_x = u_field(s985, 1);
    u_value v_e = u_field(u_field(s985, 2), 0);
    static struct u_object l__32__61__32__995 = U_LIT(" = ");
    static struct u_object l__59__996 = U_LIT(";");
    t986 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__995, u_concat(f_c_46_c_expr_1(v_e), &l__59__996))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t986) {
    u_no_case(s985);
  }
  return t986;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s997 = v_f;
  u_value t998 = NULL;
  if ((!t998) && u_is_con(s997, &n_c_46_CFile)) {
    u_value v_ds = u_field(s997, 0);
    static struct u_object l__10__10__999 = U_LIT("\n\n");
    u_value t1000 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1001 = &t1000;
    u_value l1002 = v_ds;
    while (u_is_con(l1002, &n_list_46_Cons)) {
      u_value v_d = u_field(l1002, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__1003 = U_LIT("\n");
        e1001 = u_append(e1001, u_str_join(&l__10__1003, f_c_46_c_decl_1(v_d)));
      }
      l1002 = u_field(l1002, 1);
    }
    static struct u_object l__10__1004 = U_LIT("\n");
    t998 = u_concat(u_str_join(&l__10__10__999, t1000), &l__10__1004);
  }
  if (!t998) {
    u_no_case(s997);
  }
  return t998;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__1005 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__1005, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__123__125__44__59__46__1006 = U_LIT("()[]{},;.");
  return u_str_contains(&l__40__41__91__93__123__125__44__59__46__1006, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t1007 = u_is_letter(v_c);
  if (!u_truth(t1007)) {
    t1007 = u_is_digit(v_c);
  }
  u_value t1008 = t1007;
  if (!u_truth(t1008)) {
    t1008 = u_eq(v_c, u_mk_char(95));
  }
  return t1008;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t1009;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t1009 = u_str_get(v_s, v_i);
  } else {
    t1009 = u_chr(u_mk_int(0));
  }
  return t1009;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s1010 = v_c;
  u_value t1011 = NULL;
  if ((!t1011) && u_equals(s1010, u_mk_char(34))) {
    t1011 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t1011) && u_equals(s1010, u_mk_char(39))) {
    t1011 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t1011) && u_equals(s1010, u_mk_char(92))) {
    t1011 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t1011) && u_equals(s1010, u_mk_char(110))) {
    t1011 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t1011) && u_equals(s1010, u_mk_char(116))) {
    t1011 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t1011) && u_equals(s1010, u_mk_char(114))) {
    t1011 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t1011) {
    t1011 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1011) {
    u_no_case(s1010);
  }
  return t1011;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t1012;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t1012 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t1012 = v_i;
  }
  return t1012;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t1013 = u_mk_int(0);
    u_value t1014 = u_mk_int(1);
    u_value t1015 = u_mk_int(1);
    v_j = t1013;
    *r_line = t1014;
    *r_column = t1015;
    u_value t1020 = u_lt(v_j, v_i);
    while (u_truth(t1020)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t1016 = u_add(*r_line, u_mk_int(1));
            u_value t1017 = u_mk_int(1);
            *r_line = t1016;
            *r_column = t1017;
          }
        } else {
          {
            u_value t1018 = u_add(*r_column, u_mk_int(1));
            *r_column = t1018;
          }
        }
        u_value t1019 = u_add(v_j, u_mk_int(1));
        v_j = t1019;
      }
      t1020 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t1021 = *r_i;
    u_value t1022 = u_add(*r_i, u_mk_int(1));
    u_value t1023 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t1021;
    *r_i = t1022;
    v_parts = t1023;
    u_value t1024 = *r_i;
    v_from = t1024;
    u_value t1035 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t1035)) {
      {
        u_value t1025 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t1025;
        u_value t1026 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t1026)) {
          t1026 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t1026)) {
          {
            static struct u_object l_unterminated_32_lit_1027 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_1027, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s1028 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d1029 = 0;
            if ((!d1029) && u_is_con(s1028, &n_option_46_Some)) {
              u_value v_d = u_field(s1028, 0);
              d1029 = 1;
              {
                u_value t1030 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t1031 = u_add(*r_i, u_mk_int(2));
                v_parts = t1030;
                *r_i = t1031;
                u_value t1032 = *r_i;
                v_from = t1032;
              }
            }
            if ((!d1029) && u_is_con(s1028, &n_option_46_None)) {
              d1029 = 1;
              {
                static struct u_object l_invalid_32_escape_1033 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_1033, *r_i), "lex.LexError");
              }
            }
            if (!d1029) {
              u_no_case(s1028);
            }
          }
        } else {
          {
            u_value t1034 = u_add(*r_i, u_mk_int(1));
            *r_i = t1034;
          }
        }
      }
      t1035 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__1036 = U_LIT("");
    u_value t1037 = u_str_join(&l__1036, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t1038 = u_add(*r_i, u_mk_int(1));
    *r_text = t1037;
    *r_i = t1038;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1039 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t1039;
    u_value t1040 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1041 = v_j;
    *r_t = t1040;
    *r_i = t1041;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1042 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t1042;
    u_value s1043 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d1044 = 0;
    if ((!d1044) && u_is_con(s1043, &n_option_46_Some)) {
      u_value v_n = u_field(s1043, 0);
      d1044 = 1;
      {
        u_value t1045 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t1046 = v_j;
        *r_t = t1045;
        *r_i = t1046;
      }
    }
    if ((!d1044) && u_is_con(s1043, &n_option_46_None)) {
      d1044 = 1;
      {
        static struct u_object l_invalid_32_number_1047 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_1047, *r_i), "lex.LexError");
      }
    }
    if (!d1044) {
      u_no_case(s1043);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1048 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t1048;
    u_value t1049 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1050 = v_j;
    *r_t = t1049;
    *r_i = t1050;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t1051 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t1051;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t1052 = *r_i;
    v_start = t1052;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_1053 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_1053, v_start), "lex.LexError");
      }
    }
    u_value t1054 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t1054;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t1055 = u_str_get(v_s, *r_i);
    v_c = t1055;
    u_value s1056 = v_c;
    int d1057 = 0;
    if (!d1057) {
      u_value t1058 = u_is_letter(v_c);
      if (!u_truth(t1058)) {
        t1058 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t1058)) {
        d1057 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d1057) {
      if (u_truth(u_is_digit(v_c))) {
        d1057 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d1057) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d1057 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d1057) && u_equals(s1056, u_mk_char(34))) {
      d1057 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d1057) && u_equals(s1056, u_mk_char(39))) {
      d1057 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d1057) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d1057 = 1;
        {
          u_value t1059 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t1060 = u_add(*r_i, u_mk_int(1));
          *r_t = t1059;
          *r_i = t1060;
        }
      }
    }
    if (!d1057) {
      d1057 = 1;
      {
        static struct u_object l_unexpected_32_chara_1061 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_1061, *r_i), "lex.LexError");
      }
    }
    if (!d1057) {
      u_no_case(s1056);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t1062 = u_mk_int(0);
    v_i = t1062;
    u_value t1063 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1063;
    u_value t1070 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t1070)) {
      {
        u_value t1064 = v_i;
        v_start = t1064;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t1065 = u_add(v_i, u_mk_int(1));
            v_i = t1065;
          }
        } else {
          u_value t1066 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t1066)) {
            t1066 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t1066)) {
            {
              u_value t1068 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam1067, 0));
              v_i = t1068;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t1069 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t1069;
            }
          }
        }
      }
      t1070 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t1071 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t1071;
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
  u_value s1072 = v_s;
  u_value t1073 = NULL;
  if ((!t1073) && u_is_con(s1072, &n_set_46_SLeaf)) {
    t1073 = u_mk_bool(0);
  }
  if ((!t1073) && u_is_con(s1072, &n_set_46_SNode)) {
    u_value v_l = u_field(s1072, 0);
    u_value v_y = u_field(s1072, 1);
    u_value v_r = u_field(s1072, 2);
    u_value s1074 = u_compare(v_x, v_y);
    u_value t1075 = NULL;
    if ((!t1075) && u_equals(s1074, u_mk_int(-1))) {
      t1075 = f_set_46_member_2(v_x, v_l);
    }
    if ((!t1075) && u_equals(s1074, u_mk_int(1))) {
      t1075 = f_set_46_member_2(v_x, v_r);
    }
    if (!t1075) {
      t1075 = u_mk_bool(1);
    }
    if (!t1075) {
      u_no_case(s1074);
    }
    t1073 = t1075;
  }
  if (!t1073) {
    u_no_case(s1072);
  }
  return t1073;
}

u_value f_set_46_insert_2(u_value v_x, u_value v_s) {
  u_value s1076 = v_s;
  u_value t1077 = NULL;
  if ((!t1077) && u_is_con(s1076, &n_set_46_SLeaf)) {
    t1077 = u_mk_con(&n_set_46_SNode, 4, u_mk_con(&n_set_46_SLeaf, 0), v_x, u_mk_con(&n_set_46_SLeaf, 0), u_mk_int(1));
  }
  if ((!t1077) && u_is_con(s1076, &n_set_46_SNode)) {
    u_value v_l = u_field(s1076, 0);
    u_value v_y = u_field(s1076, 1);
    u_value v_r = u_field(s1076, 2);
    u_value s1078 = u_compare(v_x, v_y);
    u_value t1079 = NULL;
    if ((!t1079) && u_equals(s1078, u_mk_int(-1))) {
      t1079 = f_set_46_rebalance_3(f_set_46_insert_2(v_x, v_l), v_y, v_r);
    }
    if ((!t1079) && u_equals(s1078, u_mk_int(1))) {
      t1079 = f_set_46_rebalance_3(v_l, v_y, f_set_46_insert_2(v_x, v_r));
    }
    if (!t1079) {
      t1079 = v_s;
    }
    if (!t1079) {
      u_no_case(s1078);
    }
    t1077 = t1079;
  }
  if (!t1077) {
    u_no_case(s1076);
  }
  return t1077;
}

u_value f_set_46_from_list_1(u_value v_xs) {
  return f_set_46_insert_all_2(v_xs, u_mk_con(&n_set_46_SLeaf, 0));
}

u_value f_set_46_insert_all_2(u_value v_xs, u_value v_s) {
  u_value s1080 = v_xs;
  u_value t1081 = NULL;
  if ((!t1081) && u_is_con(s1080, &n_list_46_Nil)) {
    t1081 = v_s;
  }
  if ((!t1081) && u_is_con(s1080, &n_list_46_Cons)) {
    u_value v_x = u_field(s1080, 0);
    u_value v_rest = u_field(s1080, 1);
    t1081 = f_set_46_insert_all_2(v_rest, f_set_46_insert_2(v_x, v_s));
  }
  if (!t1081) {
    u_no_case(s1080);
  }
  return t1081;
}

u_value f_set_46_elements_1(u_value v_s) {
  return f_set_46_elements_onto_2(v_s, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_set_46_elements_onto_2(u_value v_s, u_value v_rest) {
  u_value s1082 = v_s;
  u_value t1083 = NULL;
  if ((!t1083) && u_is_con(s1082, &n_set_46_SLeaf)) {
    t1083 = v_rest;
  }
  if ((!t1083) && u_is_con(s1082, &n_set_46_SNode)) {
    u_value v_l = u_field(s1082, 0);
    u_value v_x = u_field(s1082, 1);
    u_value v_r = u_field(s1082, 2);
    t1083 = f_set_46_elements_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, v_x, f_set_46_elements_onto_2(v_r, v_rest)));
  }
  if (!t1083) {
    u_no_case(s1082);
  }
  return t1083;
}

u_value f_set_46_count_1(u_value v_s) {
  u_value s1084 = v_s;
  u_value t1085 = NULL;
  if ((!t1085) && u_is_con(s1084, &n_set_46_SLeaf)) {
    t1085 = u_mk_int(0);
  }
  if ((!t1085) && u_is_con(s1084, &n_set_46_SNode)) {
    u_value v_l = u_field(s1084, 0);
    u_value v_r = u_field(s1084, 2);
    t1085 = u_add(u_add(f_set_46_count_1(v_l), u_mk_int(1)), f_set_46_count_1(v_r));
  }
  if (!t1085) {
    u_no_case(s1084);
  }
  return t1085;
}

u_value f_set_46_depth_1(u_value v_s) {
  u_value s1086 = v_s;
  u_value t1087 = NULL;
  if ((!t1087) && u_is_con(s1086, &n_set_46_SLeaf)) {
    t1087 = u_mk_int(0);
  }
  if ((!t1087) && u_is_con(s1086, &n_set_46_SNode)) {
    u_value v_d = u_field(s1086, 3);
    t1087 = v_d;
  }
  if (!t1087) {
    u_no_case(s1086);
  }
  return t1087;
}

u_value f_set_46_snode_depth_2(u_value v_l, u_value v_r) {
  u_value t1088;
  if (u_truth(u_lt(f_set_46_depth_1(v_l), f_set_46_depth_1(v_r)))) {
    t1088 = f_set_46_depth_1(v_r);
  } else {
    t1088 = f_set_46_depth_1(v_l);
  }
  return u_add(u_mk_int(1), t1088);
}

u_value f_set_46_snode_3(u_value v_l, u_value v_x, u_value v_r) {
  return u_mk_con(&n_set_46_SNode, 4, v_l, v_x, v_r, f_set_46_snode_depth_2(v_l, v_r));
}

u_value f_set_46_turn_left_1(u_value v_s) {
  u_value s1089 = v_s;
  u_value t1090 = NULL;
  if ((!t1090) && (u_is_con(s1089, &n_set_46_SNode) && u_is_con(u_field(s1089, 2), &n_set_46_SNode))) {
    u_value v_a = u_field(s1089, 0);
    u_value v_x = u_field(s1089, 1);
    u_value v_b = u_field(u_field(s1089, 2), 0);
    u_value v_y = u_field(u_field(s1089, 2), 1);
    u_value v_c = u_field(u_field(s1089, 2), 2);
    t1090 = f_set_46_snode_3(f_set_46_snode_3(v_a, v_x, v_b), v_y, v_c);
  }
  if (!t1090) {
    t1090 = v_s;
  }
  if (!t1090) {
    u_no_case(s1089);
  }
  return t1090;
}

u_value f_set_46_turn_right_1(u_value v_s) {
  u_value s1091 = v_s;
  u_value t1092 = NULL;
  if ((!t1092) && (u_is_con(s1091, &n_set_46_SNode) && u_is_con(u_field(s1091, 0), &n_set_46_SNode))) {
    u_value v_a = u_field(u_field(s1091, 0), 0);
    u_value v_x = u_field(u_field(s1091, 0), 1);
    u_value v_b = u_field(u_field(s1091, 0), 2);
    u_value v_y = u_field(s1091, 1);
    u_value v_c = u_field(s1091, 2);
    t1092 = f_set_46_snode_3(v_a, v_x, f_set_46_snode_3(v_b, v_y, v_c));
  }
  if (!t1092) {
    t1092 = v_s;
  }
  if (!t1092) {
    u_no_case(s1091);
  }
  return t1092;
}

u_value f_set_46_smaller_1(u_value v_s) {
  u_value s1093 = v_s;
  u_value t1094 = NULL;
  if ((!t1094) && u_is_con(s1093, &n_set_46_SNode)) {
    u_value v_l = u_field(s1093, 0);
    t1094 = v_l;
  }
  if (!t1094) {
    t1094 = v_s;
  }
  if (!t1094) {
    u_no_case(s1093);
  }
  return t1094;
}

u_value f_set_46_greater_1(u_value v_s) {
  u_value s1095 = v_s;
  u_value t1096 = NULL;
  if ((!t1096) && u_is_con(s1095, &n_set_46_SNode)) {
    u_value v_r = u_field(s1095, 2);
    t1096 = v_r;
  }
  if (!t1096) {
    t1096 = v_s;
  }
  if (!t1096) {
    u_no_case(s1095);
  }
  return t1096;
}

u_value f_set_46_rebalance_3(u_value v_l, u_value v_x, u_value v_r) {
  u_value t1100;
  if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1))))) {
    u_value t1097;
    if (u_truth(u_lt(f_set_46_depth_1(f_set_46_smaller_1(v_l)), f_set_46_depth_1(f_set_46_greater_1(v_l))))) {
      t1097 = f_set_46_turn_left_1(v_l);
    } else {
      t1097 = v_l;
    }
    t1100 = f_set_46_turn_right_1(f_set_46_snode_3(t1097, v_x, v_r));
  } else {
    u_value t1099;
    if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1))))) {
      u_value t1098;
      if (u_truth(u_lt(f_set_46_depth_1(f_set_46_greater_1(v_r)), f_set_46_depth_1(f_set_46_smaller_1(v_r))))) {
        t1098 = f_set_46_turn_right_1(v_r);
      } else {
        t1098 = v_r;
      }
      t1099 = f_set_46_turn_left_1(f_set_46_snode_3(v_l, v_x, t1098));
    } else {
      t1099 = f_set_46_snode_3(v_l, v_x, v_r);
    }
    t1100 = t1099;
  }
  return t1100;
}

u_value f_set_46_well_formed_1(u_value v_s) {
  u_value s1101 = v_s;
  u_value t1102 = NULL;
  if ((!t1102) && u_is_con(s1101, &n_set_46_SLeaf)) {
    t1102 = u_mk_bool(1);
  }
  if ((!t1102) && u_is_con(s1101, &n_set_46_SNode)) {
    u_value v_l = u_field(s1101, 0);
    u_value v_r = u_field(s1101, 2);
    u_value v_d = u_field(s1101, 3);
    u_value t1103 = f_set_46_well_formed_1(v_l);
    if (u_truth(t1103)) {
      t1103 = f_set_46_well_formed_1(v_r);
    }
    u_value t1104 = t1103;
    if (u_truth(t1104)) {
      t1104 = u_eq(v_d, f_set_46_snode_depth_2(v_l, v_r));
    }
    u_value t1105 = t1104;
    if (u_truth(t1105)) {
      t1105 = f_prelude_46__60__61__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1)));
    }
    u_value t1106 = t1105;
    if (u_truth(t1106)) {
      t1106 = f_prelude_46__60__61__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1)));
    }
    t1102 = t1106;
  }
  if (!t1102) {
    u_no_case(s1101);
  }
  return t1102;
}

u_value f_set_46_upto_1(u_value v_n) {
  u_value t1107;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t1107 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1107 = f_list_46__43__43__2(f_set_46_upto_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t1107;
}

u_value f_parse_46_Notations_46_cmds_1(u_value v_x) {
  u_value s1108 = v_x;
  u_value t1109 = NULL;
  if ((!t1109) && u_is_con(s1108, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1108, 0);
    t1109 = v_y;
  }
  if (!t1109) {
    u_no_case(s1108);
  }
  return t1109;
}

u_value f_parse_46_Notations_46_ops_1(u_value v_x) {
  u_value s1110 = v_x;
  u_value t1111 = NULL;
  if ((!t1111) && u_is_con(s1110, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1110, 1);
    t1111 = v_y;
  }
  if (!t1111) {
    u_no_case(s1110);
  }
  return t1111;
}

u_value f_parse_46_Notations_46_cmds_58__61__2(u_value v_x, u_value v_y) {
  u_value s1112 = v_x;
  u_value t1113 = NULL;
  if ((!t1113) && u_is_con(s1112, &n_parse_46_Notations)) {
    u_value v_a1 = u_field(s1112, 1);
    t1113 = u_mk_con(&n_parse_46_Notations, 2, v_y, v_a1);
  }
  if (!t1113) {
    u_no_case(s1112);
  }
  return t1113;
}

u_value f_parse_46_Notations_46_ops_58__61__2(u_value v_x, u_value v_y) {
  u_value s1114 = v_x;
  u_value t1115 = NULL;
  if ((!t1115) && u_is_con(s1114, &n_parse_46_Notations)) {
    u_value v_a0 = u_field(s1114, 0);
    t1115 = u_mk_con(&n_parse_46_Notations, 2, v_a0, v_y);
  }
  if (!t1115) {
    u_no_case(s1114);
  }
  return t1115;
}

u_value f_parse_46_Scope_46_params_1(u_value v_x) {
  u_value s1116 = v_x;
  u_value t1117 = NULL;
  if ((!t1117) && u_is_con(s1116, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1116, 0);
    t1117 = v_y;
  }
  if (!t1117) {
    u_no_case(s1116);
  }
  return t1117;
}

u_value f_parse_46_Scope_46_notations_1(u_value v_x) {
  u_value s1118 = v_x;
  u_value t1119 = NULL;
  if ((!t1119) && u_is_con(s1118, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1118, 1);
    t1119 = v_y;
  }
  if (!t1119) {
    u_no_case(s1118);
  }
  return t1119;
}

u_value f_parse_46_Scope_46_params_58__61__2(u_value v_x, u_value v_y) {
  u_value s1120 = v_x;
  u_value t1121 = NULL;
  if ((!t1121) && u_is_con(s1120, &n_parse_46_Scope)) {
    u_value v_a1 = u_field(s1120, 1);
    t1121 = u_mk_con(&n_parse_46_Scope, 2, v_y, v_a1);
  }
  if (!t1121) {
    u_no_case(s1120);
  }
  return t1121;
}

u_value f_parse_46_Scope_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1122 = v_x;
  u_value t1123 = NULL;
  if ((!t1123) && u_is_con(s1122, &n_parse_46_Scope)) {
    u_value v_a0 = u_field(s1122, 0);
    t1123 = u_mk_con(&n_parse_46_Scope, 2, v_a0, v_y);
  }
  if (!t1123) {
    u_no_case(s1122);
  }
  return t1123;
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
  static struct u_object l___1124 = U_LIT("_");
  static struct u_object l_assert_1125 = U_LIT("assert");
  static struct u_object l_assign_1126 = U_LIT("assign");
  static struct u_object l_call_1127 = U_LIT("call");
  static struct u_object l_case_1128 = U_LIT("case");
  static struct u_object l_catch_1129 = U_LIT("catch");
  static struct u_object l_data_1130 = U_LIT("data");
  static struct u_object l_else_1131 = U_LIT("else");
  static struct u_object l_end_1132 = U_LIT("end");
  static struct u_object l_eval_1133 = U_LIT("eval");
  static struct u_object l_false_1134 = U_LIT("false");
  static struct u_object l_for_1135 = U_LIT("for");
  static struct u_object l_function_1136 = U_LIT("function");
  static struct u_object l_if_1137 = U_LIT("if");
  static struct u_object l_import_1138 = U_LIT("import");
  static struct u_object l_in_1139 = U_LIT("in");
  static struct u_object l_infix_1140 = U_LIT("infix");
  static struct u_object l_lambda_1141 = U_LIT("lambda");
  static struct u_object l_let_1142 = U_LIT("let");
  static struct u_object l_local_1143 = U_LIT("local");
  static struct u_object l_match_1144 = U_LIT("match");
  static struct u_object l_out_1145 = U_LIT("out");
  static struct u_object l_postfix_1146 = U_LIT("postfix");
  static struct u_object l_prefix_1147 = U_LIT("prefix");
  static struct u_object l_procedure_1148 = U_LIT("procedure");
  static struct u_object l_raise_1149 = U_LIT("raise");
  static struct u_object l_ref_1150 = U_LIT("ref");
  static struct u_object l_run_1151 = U_LIT("run");
  static struct u_object l_skip_1152 = U_LIT("skip");
  static struct u_object l_test_1153 = U_LIT("test");
  static struct u_object l_then_1154 = U_LIT("then");
  static struct u_object l_true_1155 = U_LIT("true");
  static struct u_object l_type_1156 = U_LIT("type");
  static struct u_object l_try_1157 = U_LIT("try");
  static struct u_object l_while_1158 = U_LIT("while");
  static struct u_object l_with_1159 = U_LIT("with");
  u_pop(handler);
  cache = f_set_46_from_list_1(u_mk_con(&n_list_46_Cons, 2, &l___1124, u_mk_con(&n_list_46_Cons, 2, &l_assert_1125, u_mk_con(&n_list_46_Cons, 2, &l_assign_1126, u_mk_con(&n_list_46_Cons, 2, &l_call_1127, u_mk_con(&n_list_46_Cons, 2, &l_case_1128, u_mk_con(&n_list_46_Cons, 2, &l_catch_1129, u_mk_con(&n_list_46_Cons, 2, &l_data_1130, u_mk_con(&n_list_46_Cons, 2, &l_else_1131, u_mk_con(&n_list_46_Cons, 2, &l_end_1132, u_mk_con(&n_list_46_Cons, 2, &l_eval_1133, u_mk_con(&n_list_46_Cons, 2, &l_false_1134, u_mk_con(&n_list_46_Cons, 2, &l_for_1135, u_mk_con(&n_list_46_Cons, 2, &l_function_1136, u_mk_con(&n_list_46_Cons, 2, &l_if_1137, u_mk_con(&n_list_46_Cons, 2, &l_import_1138, u_mk_con(&n_list_46_Cons, 2, &l_in_1139, u_mk_con(&n_list_46_Cons, 2, &l_infix_1140, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1141, u_mk_con(&n_list_46_Cons, 2, &l_let_1142, u_mk_con(&n_list_46_Cons, 2, &l_local_1143, u_mk_con(&n_list_46_Cons, 2, &l_match_1144, u_mk_con(&n_list_46_Cons, 2, &l_out_1145, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1146, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1147, u_mk_con(&n_list_46_Cons, 2, &l_procedure_1148, u_mk_con(&n_list_46_Cons, 2, &l_raise_1149, u_mk_con(&n_list_46_Cons, 2, &l_ref_1150, u_mk_con(&n_list_46_Cons, 2, &l_run_1151, u_mk_con(&n_list_46_Cons, 2, &l_skip_1152, u_mk_con(&n_list_46_Cons, 2, &l_test_1153, u_mk_con(&n_list_46_Cons, 2, &l_then_1154, u_mk_con(&n_list_46_Cons, 2, &l_true_1155, u_mk_con(&n_list_46_Cons, 2, &l_type_1156, u_mk_con(&n_list_46_Cons, 2, &l_try_1157, u_mk_con(&n_list_46_Cons, 2, &l_while_1158, u_mk_con(&n_list_46_Cons, 2, &l_with_1159, u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_set_46_member_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1160 = U_LIT("=");
  static struct u_object l__61__62__1161 = U_LIT("=>");
  static struct u_object l__58__61__1162 = U_LIT(":=");
  static struct u_object l__58__1163 = U_LIT(":");
  static struct u_object l__124__1164 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1160, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1161, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1162, u_mk_con(&n_list_46_Cons, 2, &l__58__1163, u_mk_con(&n_list_46_Cons, 2, &l__124__1164, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1165 = v_ns;
  u_value t1166 = NULL;
  if ((!t1166) && u_is_con(s1165, &n_list_46_Nil)) {
    t1166 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1166) && (u_is_con(s1165, &n_list_46_Cons) && (u_is_con(u_field(s1165, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1165, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1165, 0), 1);
    u_value v_ops = u_field(u_field(s1165, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1166 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1166) && u_is_con(s1165, &n_list_46_Cons)) {
    u_value v_more = u_field(s1165, 1);
    t1166 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1166) {
    u_no_case(s1165);
  }
  return t1166;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1167 = v_ns;
  u_value t1168 = NULL;
  if ((!t1168) && u_is_con(s1167, &n_list_46_Nil)) {
    t1168 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1168) && (u_is_con(s1167, &n_list_46_Cons) && (u_is_con(u_field(s1167, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1167, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1167, 0), 1);
    u_value v_ops = u_field(u_field(s1167, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1168 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1168) && u_is_con(s1167, &n_list_46_Cons)) {
    u_value v_more = u_field(s1167, 1);
    t1168 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1168) {
    u_no_case(s1167);
  }
  return t1168;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1169 = v_ns;
  u_value t1170 = NULL;
  if ((!t1170) && u_is_con(s1169, &n_list_46_Nil)) {
    t1170 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1170) && (u_is_con(s1169, &n_list_46_Cons) && u_is_con(u_field(s1169, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1169, 0), 0);
    u_value v_p = u_field(u_field(s1169, 0), 1);
    u_value v_ops = u_field(u_field(s1169, 0), 2);
    u_value t1171 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1171)) {
      t1171 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1172 = t1171;
    if (u_truth(t1172)) {
      t1172 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1172)) {
      t1170 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1170) && u_is_con(s1169, &n_list_46_Cons)) {
    u_value v_more = u_field(s1169, 1);
    t1170 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1170) {
    u_no_case(s1169);
  }
  return t1170;
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
  u_value s1173 = v_cs;
  u_value t1174 = NULL;
  if ((!t1174) && u_is_con(s1173, &n_list_46_Nil)) {
    t1174 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1174) && (u_is_con(s1173, &n_list_46_Cons) && u_is_con(u_field(s1173, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1173, 0), 2);
    u_value v_more = u_field(s1173, 1);
    t1174 = f_list_46__43__43__2(v_ops, f_parse_46_operators_of_1(v_more));
  }
  if ((!t1174) && u_is_con(s1173, &n_list_46_Cons)) {
    u_value v_more = u_field(s1173, 1);
    t1174 = f_parse_46_operators_of_1(v_more);
  }
  if (!t1174) {
    u_no_case(s1173);
  }
  return t1174;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1175 = v_ts;
  u_value t1176 = NULL;
  if ((!t1176) && u_is_con(s1175, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1175, 0), 0);
    t1176 = v_t;
  }
  if (!t1176) {
    t1176 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1176) {
    u_no_case(s1175);
  }
  return t1176;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1177 = v_ts;
  u_value t1178 = NULL;
  if ((!t1178) && (u_is_con(s1177, &n_list_46_Cons) && u_is_con(u_field(s1177, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1177, 1), 0), 0);
    t1178 = v_t;
  }
  if (!t1178) {
    t1178 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1178) {
    u_no_case(s1177);
  }
  return t1178;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1179 = v_ts;
  u_value t1180 = NULL;
  if ((!t1180) && u_is_con(s1179, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1179, 0), 1);
    t1180 = v_i;
  }
  if (!t1180) {
    t1180 = u_mk_int(0);
  }
  if (!t1180) {
    u_no_case(s1179);
  }
  return t1180;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1181 = v_t;
  u_value t1182 = NULL;
  if ((!t1182) && u_is_con(s1181, &n_lex_46_TName)) {
    u_value v_n = u_field(s1181, 0);
    t1182 = v_n;
  }
  if ((!t1182) && u_is_con(s1181, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1181, 0);
    t1182 = v_s;
  }
  if ((!t1182) && u_is_con(s1181, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1181, 0);
    t1182 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1182) && u_is_con(s1181, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1181, 0);
    t1182 = u_str_show(v_n);
  }
  if ((!t1182) && u_is_con(s1181, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1181, 0);
    t1182 = u_str_quote(v_s);
  }
  if ((!t1182) && u_is_con(s1181, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1181, 0);
    static struct u_object l__39__1183 = U_LIT("'");
    static struct u_object l__39__1184 = U_LIT("'");
    t1182 = u_concat(&l__39__1183, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1184));
  }
  if ((!t1182) && u_is_con(s1181, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1185 = U_LIT("end of input");
    t1182 = &l_end_32_of_32_input_1185;
  }
  if (!t1182) {
    u_no_case(s1181);
  }
  return t1182;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1186 = f_parse_46_head_token_1(v_ts);
  u_value t1187 = NULL;
  if ((!t1187) && u_is_con(s1186, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1186, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1187 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1187) && u_is_con(s1186, &n_lex_46_TName)) {
    u_value v_n = u_field(s1186, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1187 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1187) {
    t1187 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1187) {
    u_no_case(s1186);
  }
  return t1187;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1188 = *r_ts;
    int d1189 = 0;
    if ((!d1189) && u_is_con(s1188, &n_list_46_Cons)) {
      u_value v_more = u_field(s1188, 1);
      d1189 = 1;
      {
        u_value t1190 = v_more;
        *r_ts = t1190;
      }
    }
    if (!d1189) {
      d1189 = 1;
      {
      }
    }
    if (!d1189) {
      u_no_case(s1188);
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
        static struct u_object l__32_expected_32_but_32__1191 = U_LIT(" expected but ");
        static struct u_object l__32_found_1192 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1191, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1192))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1193 = f_parse_46_head_token_1(*r_ts);
    int d1194 = 0;
    if ((!d1194) && u_is_con(s1193, &n_lex_46_TName)) {
      u_value v_m = u_field(s1193, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1194 = 1;
        {
          u_value t1195 = v_m;
          *r_n = t1195;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1194) {
      d1194 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1196 = U_LIT("name expected but ");
        static struct u_object l__32_found_1197 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1196, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1197)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1194) {
      u_no_case(s1193);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1198 = f_parse_46_head_token_1(*r_ts);
    int d1199 = 0;
    if ((!d1199) && u_is_con(s1198, &n_lex_46_TName)) {
      u_value v_m = u_field(s1198, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_m))) {
        d1199 = 1;
        {
          static struct u_object l_operator_32__1200 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1201 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1200, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1201)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1199) {
      d1199 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1199) {
      u_no_case(s1198);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1202 = f_parse_46_head_token_1(*r_ts);
    int d1203 = 0;
    if ((!d1203) && u_is_con(s1202, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1202, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1203 = 1;
        {
          u_value t1204 = v_s;
          *r_n = t1204;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1203) {
      d1203 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1203) {
      u_no_case(s1202);
    }
  }
}

u_value f_parse_46_Ops_46_notations_1(u_value v_x) {
  u_value s1205 = v_x;
  u_value t1206 = NULL;
  if ((!t1206) && u_is_con(s1205, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1205, 0);
    t1206 = v_y;
  }
  if (!t1206) {
    u_no_case(s1205);
  }
  return t1206;
}

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x) {
  u_value s1207 = v_x;
  u_value t1208 = NULL;
  if ((!t1208) && u_is_con(s1207, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1207, 1);
    t1208 = v_y;
  }
  if (!t1208) {
    u_no_case(s1207);
  }
  return t1208;
}

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x) {
  u_value s1209 = v_x;
  u_value t1210 = NULL;
  if ((!t1210) && u_is_con(s1209, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1209, 2);
    t1210 = v_y;
  }
  if (!t1210) {
    u_no_case(s1209);
  }
  return t1210;
}

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x) {
  u_value s1211 = v_x;
  u_value t1212 = NULL;
  if ((!t1212) && u_is_con(s1211, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1211, 3);
    t1212 = v_y;
  }
  if (!t1212) {
    u_no_case(s1211);
  }
  return t1212;
}

u_value f_parse_46_Ops_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1213 = v_x;
  u_value t1214 = NULL;
  if ((!t1214) && u_is_con(s1213, &n_parse_46_Ops)) {
    u_value v_a1 = u_field(s1213, 1);
    u_value v_a2 = u_field(s1213, 2);
    u_value v_a3 = u_field(s1213, 3);
    t1214 = u_mk_con(&n_parse_46_Ops, 4, v_y, v_a1, v_a2, v_a3);
  }
  if (!t1214) {
    u_no_case(s1213);
  }
  return t1214;
}

u_value f_parse_46_Ops_46_prefix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1215 = v_x;
  u_value t1216 = NULL;
  if ((!t1216) && u_is_con(s1215, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1215, 0);
    u_value v_a2 = u_field(s1215, 2);
    u_value v_a3 = u_field(s1215, 3);
    t1216 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_y, v_a2, v_a3);
  }
  if (!t1216) {
    u_no_case(s1215);
  }
  return t1216;
}

u_value f_parse_46_Ops_46_postfix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1217 = v_x;
  u_value t1218 = NULL;
  if ((!t1218) && u_is_con(s1217, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1217, 0);
    u_value v_a1 = u_field(s1217, 1);
    u_value v_a3 = u_field(s1217, 3);
    t1218 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_y, v_a3);
  }
  if (!t1218) {
    u_no_case(s1217);
  }
  return t1218;
}

u_value f_parse_46_Ops_46_infix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1219 = v_x;
  u_value t1220 = NULL;
  if ((!t1220) && u_is_con(s1219, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1219, 0);
    u_value v_a1 = u_field(s1219, 1);
    u_value v_a2 = u_field(s1219, 2);
    t1220 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_a2, v_y);
  }
  if (!t1220) {
    u_no_case(s1219);
  }
  return t1220;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1221 = v_rest;
    int d1222 = 0;
    if ((!d1222) && u_is_con(s1221, &n_list_46_Nil)) {
      d1222 = 1;
      {
      }
    }
    if ((!d1222) && (u_is_con(s1221, &n_list_46_Cons) && u_is_con(u_field(s1221, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1221, 0), 0);
      u_value v_i = u_field(u_field(s1221, 0), 1);
      d1222 = 1;
      {
        static struct u_object l_unexpected_32_opera_1223 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1223, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1222) {
      d1222 = 1;
      {
        static struct u_object l_missing_32_operator_1224 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1224, v_at), "parse.ParseError");
      }
    }
    if (!d1222) {
      u_no_case(s1221);
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
    u_value s1225 = v_items;
    int d1226 = 0;
    if ((!d1226) && (u_is_con(s1225, &n_list_46_Cons) && u_is_con(u_field(s1225, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1225, 0), 0);
      u_value v_more = u_field(s1225, 1);
      d1226 = 1;
      {
        u_value t1227 = v_x;
        u_value t1228 = v_more;
        *r_a = t1227;
        *r_rest = t1228;
      }
    }
    if ((!d1226) && (u_is_con(s1225, &n_list_46_Cons) && u_is_con(u_field(s1225, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1225, 0), 0);
      u_value v_i = u_field(u_field(s1225, 0), 1);
      u_value v_more = u_field(s1225, 1);
      d1226 = 1;
      {
        u_value s1229 = f_parse_46_prefix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op);
        int d1230 = 0;
        if ((!d1230) && u_is_con(s1229, &n_option_46_Some)) {
          u_value v_p = u_field(s1229, 0);
          d1230 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1231 = u_apply(f_parse_46_Ops_46_prefix_op_1(v_o), 2, v_op, v_b);
            *r_a = t1231;
          }
        }
        if ((!d1230) && u_is_con(s1229, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op))) {
            d1230 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1232 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1232), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1230) && u_is_con(s1229, &n_option_46_None)) {
          d1230 = 1;
          {
            static struct u_object l_undeclared_32_opera_1233 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1233, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1230) {
          u_no_case(s1229);
        }
      }
    }
    if ((!d1226) && u_is_con(s1225, &n_list_46_Nil)) {
      d1226 = 1;
      {
        static struct u_object l_missing_32_operand_1234 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1234, v_at), "parse.ParseError");
      }
    }
    if (!d1226) {
      u_no_case(s1225);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1235 = v_items;
    int d1236 = 0;
    if ((!d1236) && (u_is_con(s1235, &n_list_46_Cons) && u_is_con(u_field(s1235, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1235, 0), 0);
      u_value v_i = u_field(u_field(s1235, 0), 1);
      u_value v_tail = u_field(s1235, 1);
      d1236 = 1;
      {
        u_value s1237 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op), f_parse_46_infix_of_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op));
        int d1238 = 0;
        if ((!d1238) && u_is_con(u_field(s1237, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1237, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1238 = 1;
            {
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_postfix_op_1(v_o), 2, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1238) && u_is_con(u_field(s1237, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1237, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1237, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1238 = 1;
            {
              u_value t1239;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1239 = v_p;
              } else {
                t1239 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1239, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)));
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_infix_op_1(v_o), 3, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1238) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op)))) {
            d1238 = 1;
            {
              static struct u_object l_undeclared_32_opera_1240 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1240, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1238) {
          d1238 = 1;
          {
            u_value t1241 = v_lhs;
            u_value t1242 = v_items;
            *r_a = t1241;
            *r_rest = t1242;
          }
        }
        if (!d1238) {
          u_no_case(s1237);
        }
      }
    }
    if (!d1236) {
      d1236 = 1;
      {
        u_value t1243 = v_lhs;
        u_value t1244 = v_items;
        *r_a = t1243;
        *r_rest = t1244;
      }
    }
    if (!d1236) {
      u_no_case(s1235);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1245 = v_rest;
    int d1246 = 0;
    if ((!d1246) && (u_is_con(s1245, &n_list_46_Cons) && u_is_con(u_field(s1245, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1245, 0), 0);
      u_value v_i = u_field(u_field(s1245, 0), 1);
      u_value t1249 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1249)) {
        t1249 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1249)) {
        d1246 = 1;
        {
          static struct u_object l_operator_32__1247 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1248 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1247, u_concat(v_op, &l__32_is_32_not_32_associat_1248)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1246) {
      d1246 = 1;
      {
      }
    }
    if (!d1246) {
      u_no_case(s1245);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1250 = v_fp;
  u_value t1251 = NULL;
  if ((!t1251) && u_is_con(s1250, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1250, 0), 1);
    t1251 = u_eq(v_q, v_p);
  }
  if (!t1251) {
    t1251 = u_mk_bool(0);
  }
  if (!t1251) {
    u_no_case(s1250);
  }
  return t1251;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1252 = v_ts;
  u_value t1253 = NULL;
  if ((!t1253) && (u_is_con(s1252, &n_list_46_Cons) && (u_is_con(u_field(s1252, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1252, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1252, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1252, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1252, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1252, 0), 1);
    t1253 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1253) && (u_is_con(s1252, &n_list_46_Cons) && u_is_con(u_field(s1252, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1252, 0);
    t1253 = v_t;
  }
  if ((!t1253) && u_is_con(s1252, &n_list_46_Nil)) {
    t1253 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1253) {
    static struct u_object l__40_invalid_41__1254 = U_LIT("(invalid)");
    t1253 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1254), v_ts);
  }
  if (!t1253) {
    u_no_case(s1252);
  }
  return t1253;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1255 = v_ts;
  u_value t1256 = NULL;
  if ((!t1256) && (u_is_con(s1255, &n_list_46_Cons) && (u_is_con(u_field(s1255, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1255, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1255, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1255, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1255, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1255, 0), 1);
    t1256 = v_cs;
  }
  if (!t1256) {
    t1256 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1256) {
    u_no_case(s1255);
  }
  return t1256;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1257 = U_LIT("->");
  u_value t1264;
  if (u_truth(u_eq(v_op, &l__45__62__1257))) {
    u_value t1258 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1259 = &t1258;
    u_value l1260 = v_l;
    while (u_is_con(l1260, &n_list_46_Cons)) {
      u_value v_t = u_field(l1260, 0);
      if (u_truth(u_mk_bool(1))) {
        e1259 = u_append(e1259, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1260 = u_field(l1260, 1);
    }
    t1264 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1258, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1261 = U_LIT("*");
    u_value t1263;
    if (u_truth(u_eq(v_op, &l__42__1261))) {
      static struct u_object l__40_product_41__1262 = U_LIT("(product)");
      t1263 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1262), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1263 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1264 = t1263;
  }
  return t1264;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1265 = v_t;
  u_value t1266 = NULL;
  if ((!t1266) && (u_is_con(s1265, &n_syntax_46_TSort) && (u_is_con(u_field(s1265, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1265, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1265, 0), 1), u_mk_lit("(invalid)")))))) {
    t1266 = u_mk_bool(1);
  }
  if ((!t1266) && u_is_con(s1265, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1265, 1);
    t1266 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1266) && u_is_con(s1265, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1265, 0);
    u_value v_r = u_field(s1265, 1);
    u_value t1267 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1267)) {
      t1267 = f_parse_46_has_invalid_1(v_r);
    }
    t1266 = t1267;
  }
  if ((!t1266) && u_is_con(s1265, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1265, 0);
    t1266 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1266) {
    t1266 = u_mk_bool(0);
  }
  if (!t1266) {
    u_no_case(s1265);
  }
  return t1266;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1268 = v_ts;
  u_value t1269 = NULL;
  if ((!t1269) && u_is_con(s1268, &n_list_46_Nil)) {
    t1269 = u_mk_bool(0);
  }
  if ((!t1269) && u_is_con(s1268, &n_list_46_Cons)) {
    u_value v_t = u_field(s1268, 0);
    u_value v_more = u_field(s1268, 1);
    u_value t1270 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1270)) {
      t1270 = f_parse_46_any_invalid_1(v_more);
    }
    t1269 = t1270;
  }
  if (!t1269) {
    u_no_case(s1268);
  }
  return t1269;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1271 = v_t;
  u_value t1272 = NULL;
  if ((!t1272) && u_is_con(s1271, &n_lex_46_TName)) {
    u_value v_n = u_field(s1271, 0);
    u_value t1273 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1273)) {
      t1273 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1272 = t1273;
  }
  if ((!t1272) && (u_is_con(s1271, &n_lex_46_TPunct) && u_equals(u_field(s1271, 0), u_mk_char(40)))) {
    t1272 = u_mk_bool(1);
  }
  if (!t1272) {
    t1272 = u_mk_bool(0);
  }
  if (!t1272) {
    u_no_case(s1271);
  }
  return t1272;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1274 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1274;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1275 = f_parse_46_type_one_1(v_r);
    *r_t = t1275;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1276 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1276, v_start), "parse.ParseError");
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
    u_value t1277 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1277;
    u_value t1278 = u_mk_bool(1);
    v_next = t1278;
    u_value t1279 = u_mk_bool(1);
    v_go = t1279;
    u_value t1288 = v_go;
    while (u_truth(t1288)) {
      {
        u_value s1280 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1281 = 0;
        if ((!d1281) && u_is_con(s1280, &n_option_46_Some)) {
          u_value v_op = u_field(s1280, 0);
          d1281 = 1;
          {
            u_value t1282 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1283 = u_mk_bool(1);
            v_rev = t1282;
            v_next = t1283;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1281) && u_is_con(s1280, &n_option_46_None)) {
          u_value t1286 = v_next;
          if (u_truth(t1286)) {
            t1286 = f_parse_46_starts_type_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1286)) {
            d1281 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1284 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1285 = u_mk_bool(0);
              v_rev = t1284;
              v_next = t1285;
            }
          }
        }
        if (!d1281) {
          d1281 = 1;
          {
            u_value t1287 = u_mk_bool(0);
            v_go = t1287;
          }
        }
        if (!d1281) {
          u_no_case(s1280);
        }
      }
      t1288 = v_go;
    }
    u_value s1289 = v_rev;
    int d1290 = 0;
    if ((!d1290) && u_is_con(s1289, &n_list_46_Nil)) {
      d1290 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1291 = U_LIT("type expected but ");
        static struct u_object l__32_found_1292 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1291, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1292)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1290) {
      d1290 = 1;
      {
        u_value t1293 = f_list_46_reverse_1(v_rev);
        *r_items = t1293;
      }
    }
    if (!d1290) {
      u_no_case(s1289);
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
    u_value t1297 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (u_truth(t1297)) {
      static struct u_object l_in_1294 = U_LIT("in");
      static struct u_object l_out_1295 = U_LIT("out");
      static struct u_object l_ref_1296 = U_LIT("ref");
      t1297 = f_list_46_contains_2(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_in_1294), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_out_1295), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_ref_1296), u_mk_con(&n_list_46_Nil, 0)))));
    }
    if (u_truth(t1297)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_sections(u_mk_proc(wp_parse_46_parse_type, 0), u_mk_proc(wp_parse_46_parse_type, 0), v_sc, &v_ins, &v_outs, &v_refs, r_ts);
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
        u_value t1298 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TProc, 3, v_ins, v_outs, v_refs), u_mk_con(&n_list_46_Nil, 0));
        *r_r = t1298;
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
              u_value t1299 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1299;
            }
          } else {
            if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Scope_46_params_1(v_sc)))) {
              {
                u_value t1300 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
                *r_r = t1300;
              }
            } else {
              {
                u_value t1301 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
                *r_r = t1301;
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
    u_value t1302 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1302;
    u_value t1303 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1303)) {
      t1303 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1303)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1304 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1304;
        u_value t1306 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1306)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1305 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1305;
          }
          t1306 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1307 = f_list_46_reverse_1(v_rev);
    *r_r = t1307;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_prefix_2(u_value v_op, u_value v_a) {
  u_value s1308 = v_a;
  u_value t1309 = NULL;
  if ((!t1309) && (u_is_con(s1308, &n_syntax_46_ELit) && u_is_con(u_field(s1308, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1308, 0), 0);
    static struct u_object l__45__1310 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1310))) {
      t1309 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1309) {
    t1309 = f_parse_46_expr_op1_2(v_op, v_a);
  }
  if (!t1309) {
    u_no_case(s1308);
  }
  return t1309;
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1311 = v_es;
  u_value t1312 = NULL;
  if ((!t1312) && u_is_con(s1311, &n_list_46_Nil)) {
    t1312 = v_rest;
  }
  if ((!t1312) && u_is_con(s1311, &n_list_46_Cons)) {
    u_value v_e = u_field(s1311, 0);
    u_value v_more = u_field(s1311, 1);
    static struct u_object l_Cons_1313 = U_LIT("Cons");
    t1312 = f_parse_46_expr_op2_3(&l_Cons_1313, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1312) {
    u_no_case(s1311);
  }
  return t1312;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1314 = v_t;
  u_value t1315 = NULL;
  if ((!t1315) && u_is_con(s1314, &n_lex_46_TName)) {
    u_value v_n = u_field(s1314, 0);
    static struct u_object l_let_1316 = U_LIT("let");
    static struct u_object l_if_1317 = U_LIT("if");
    static struct u_object l_lambda_1318 = U_LIT("lambda");
    static struct u_object l_match_1319 = U_LIT("match");
    static struct u_object l_true_1320 = U_LIT("true");
    static struct u_object l_false_1321 = U_LIT("false");
    u_value t1323 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1316, u_mk_con(&n_list_46_Cons, 2, &l_if_1317, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1318, u_mk_con(&n_list_46_Cons, 2, &l_match_1319, u_mk_con(&n_list_46_Cons, 2, &l_true_1320, u_mk_con(&n_list_46_Cons, 2, &l_false_1321, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1323)) {
      u_value t1322 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1322)) {
        t1322 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1323 = t1322;
    }
    t1315 = t1323;
  }
  if ((!t1315) && u_is_con(s1314, &n_lex_46_TNum)) {
    t1315 = u_mk_bool(1);
  }
  if ((!t1315) && u_is_con(s1314, &n_lex_46_TStr)) {
    t1315 = u_mk_bool(1);
  }
  if ((!t1315) && u_is_con(s1314, &n_lex_46_TChar)) {
    t1315 = u_mk_bool(1);
  }
  if ((!t1315) && u_is_con(s1314, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1314, 0);
    u_value t1324 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1324)) {
      t1324 = u_eq(v_c, u_mk_char(91));
    }
    u_value t1325 = t1324;
    if (!u_truth(t1325)) {
      t1325 = u_eq(v_c, u_mk_char(123));
    }
    t1315 = t1325;
  }
  if (!t1315) {
    t1315 = u_mk_bool(0);
  }
  if (!t1315) {
    u_no_case(s1314);
  }
  return t1315;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    static struct u_object l_raise_1326 = U_LIT("raise");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_raise_1326))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, r_e, r_ts);
        u_value t1327 = u_mk_con(&n_syntax_46_ERaise, 2, *r_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_e = t1327;
      }
    } else {
      {
        p_parse_46_operators(v_sc, r_e, r_ts);
        static struct u_object l_with_1328 = U_LIT("with");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_with_1328))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_update(v_sc, r_e, r_ts);
            u_value t1329 = f_parse_46_at_update_1(*r_ts);
            while (u_truth(t1329)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_update(v_sc, r_e, r_ts);
              }
              t1329 = f_parse_46_at_update_1(*r_ts);
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
    static struct u_object l__58__61__1330 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1330), r_ts);
    p_parse_46_operators(v_sc, &v_a, r_ts);
    static struct u_object l__46__1331 = U_LIT(".");
    static struct u_object l__58__61__1332 = U_LIT(":=");
    u_value t1333 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1331, u_concat(v_f, &l__58__61__1332))), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1333;
  }
}

u_value f_parse_46_at_update_1(u_value v_ts) {
  u_value s1334 = v_ts;
  u_value t1335 = NULL;
  if ((!t1335) && (u_is_con(s1334, &n_list_46_Cons) && (u_is_con(u_field(u_field(s1334, 0), 0), &n_lex_46_TPunct) && (u_equals(u_field(u_field(u_field(s1334, 0), 0), 0), u_mk_char(44)) && (u_is_con(u_field(s1334, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s1334, 1), 0), 0), &n_lex_46_TName) && (u_is_con(u_field(u_field(s1334, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(u_field(s1334, 1), 1), 0), 0), &n_lex_46_TSym) && u_equals(u_field(u_field(u_field(u_field(u_field(s1334, 1), 1), 0), 0), 0), u_mk_lit(":=")))))))))) {
    t1335 = u_mk_bool(1);
  }
  if (!t1335) {
    t1335 = u_mk_bool(0);
  }
  if (!t1335) {
    u_no_case(s1334);
  }
  return t1335;
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
    u_value t1336 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1336;
    u_value t1337 = u_mk_bool(1);
    v_next = t1337;
    u_value t1338 = u_mk_bool(1);
    v_go = t1338;
    u_value t1347 = v_go;
    while (u_truth(t1347)) {
      {
        u_value s1339 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1340 = 0;
        if ((!d1340) && u_is_con(s1339, &n_option_46_Some)) {
          u_value v_op = u_field(s1339, 0);
          d1340 = 1;
          {
            u_value t1341 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1342 = u_mk_bool(1);
            v_rev = t1341;
            v_next = t1342;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1340) && u_is_con(s1339, &n_option_46_None)) {
          u_value t1345 = v_next;
          if (u_truth(t1345)) {
            t1345 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1345)) {
            d1340 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1343 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1344 = u_mk_bool(0);
              v_rev = t1343;
              v_next = t1344;
            }
          }
        }
        if (!d1340) {
          d1340 = 1;
          {
            u_value t1346 = u_mk_bool(0);
            v_go = t1346;
          }
        }
        if (!d1340) {
          u_no_case(s1339);
        }
      }
      t1347 = v_go;
    }
    u_value s1348 = v_rev;
    int d1349 = 0;
    if ((!d1349) && u_is_con(s1348, &n_list_46_Nil)) {
      d1349 = 1;
      {
        static struct u_object l_expression_32_expec_1350 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1351 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1350, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1351)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1349) {
      d1349 = 1;
      {
        u_value t1352 = f_list_46_reverse_1(v_rev);
        *r_items = t1352;
      }
    }
    if (!d1349) {
      u_no_case(s1348);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1353 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1353;
    u_value t1355 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1355)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1354 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1354;
      }
      t1355 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1356 = f_list_46_reverse_1(v_rev);
    *r_es = t1356;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1357 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1357;
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
    u_value t1358 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1358)) {
      t1358 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1362 = t1358;
    while (u_truth(t1362)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1359 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1359;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1360 = U_LIT(".");
            u_value t1361 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1360, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1361;
          }
        }
      }
      u_value t1363 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1363)) {
        t1363 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1362 = t1363;
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
    u_value s1364 = f_parse_46_head_token_1(*r_ts);
    int d1365 = 0;
    if ((!d1365) && (u_is_con(s1364, &n_lex_46_TName) && u_equals(u_field(s1364, 0), u_mk_lit("let")))) {
      d1365 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1365) && (u_is_con(s1364, &n_lex_46_TName) && u_equals(u_field(s1364, 0), u_mk_lit("if")))) {
      d1365 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1366 = U_LIT("then");
        p_parse_46_keyword(&l_then_1366, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1367 = U_LIT("else");
        p_parse_46_keyword(&l_else_1367, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1368 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1368;
      }
    }
    if ((!d1365) && (u_is_con(s1364, &n_lex_46_TName) && u_equals(u_field(s1364, 0), u_mk_lit("lambda")))) {
      d1365 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1365) && (u_is_con(s1364, &n_lex_46_TName) && u_equals(u_field(s1364, 0), u_mk_lit("match")))) {
      d1365 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1369 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1369;
      }
    }
    if ((!d1365) && (u_is_con(s1364, &n_lex_46_TName) && u_equals(u_field(s1364, 0), u_mk_lit("true")))) {
      d1365 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1370 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1370;
      }
    }
    if ((!d1365) && (u_is_con(s1364, &n_lex_46_TName) && u_equals(u_field(s1364, 0), u_mk_lit("false")))) {
      d1365 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1371 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1371;
      }
    }
    if ((!d1365) && u_is_con(s1364, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1364, 0);
      d1365 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1372 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1372;
      }
    }
    if ((!d1365) && u_is_con(s1364, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1364, 0);
      d1365 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1373 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1373;
      }
    }
    if ((!d1365) && u_is_con(s1364, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1364, 0);
      d1365 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1374 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1374;
      }
    }
    if ((!d1365) && (u_is_con(s1364, &n_lex_46_TPunct) && u_equals(u_field(s1364, 0), u_mk_char(40)))) {
      d1365 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1375 = v_es;
        int d1376 = 0;
        if ((!d1376) && (u_is_con(s1375, &n_list_46_Cons) && u_is_con(u_field(s1375, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1375, 0);
          d1376 = 1;
          {
            u_value t1377 = v_x;
            *r_e = t1377;
          }
        }
        if (!d1376) {
          d1376 = 1;
          {
            u_value t1378 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1378;
          }
        }
        if (!d1376) {
          u_no_case(s1375);
        }
      }
    }
    if ((!d1365) && (u_is_con(s1364, &n_lex_46_TPunct) && u_equals(u_field(s1364, 0), u_mk_char(91)))) {
      d1365 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1365) && (u_is_con(s1364, &n_lex_46_TPunct) && u_equals(u_field(s1364, 0), u_mk_char(123)))) {
      d1365 = 1;
      {
        p_parse_46_parse_braces(v_sc, r_e, r_ts);
      }
    }
    if ((!d1365) && u_is_con(s1364, &n_lex_46_TName)) {
      u_value v_n = u_field(s1364, 0);
      u_value t1380 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1380)) {
        t1380 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t1380)) {
        d1365 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1379 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1379;
        }
      }
    }
    if (!d1365) {
      d1365 = 1;
      {
        static struct u_object l_expression_32_expec_1381 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1382 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1381, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1382)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1365) {
      u_no_case(s1364);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1383 = v_ps;
  u_value t1384 = NULL;
  if ((!t1384) && u_is_con(s1383, &n_list_46_Nil)) {
    t1384 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1384) && (u_is_con(s1383, &n_list_46_Cons) && u_is_con(u_field(s1383, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1383, 0), 0);
    u_value v_more = u_field(s1383, 1);
    u_value s1385 = f_parse_46_pattern_vars_1(v_more);
    u_value t1386 = NULL;
    if ((!t1386) && u_is_con(s1385, &n_option_46_Some)) {
      u_value v_xs = u_field(s1385, 0);
      t1386 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1386) && u_is_con(s1385, &n_option_46_None)) {
      t1386 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1386) {
      u_no_case(s1385);
    }
    t1384 = t1386;
  }
  if (!t1384) {
    t1384 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1384) {
    u_no_case(s1383);
  }
  return t1384;
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
    static struct u_object l__58__61__1387 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1387), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1388 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1389 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1388;
    v_es = t1389;
    u_value t1393 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1393)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1390 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1390), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1391 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1392 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1391;
        v_es = t1392;
      }
      t1393 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1394 = U_LIT("in");
    p_parse_46_keyword(&l_in_1394, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1395 = f_list_46_reverse_1(v_ps);
    u_value t1396 = f_list_46_reverse_1(v_es);
    v_ps = t1395;
    v_es = t1396;
    u_value s1397 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1398 = 0;
    if ((!d1398) && u_is_con(u_field(s1397, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1397, 0), 0);
      d1398 = 1;
      {
        u_value t1399 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1399;
      }
    }
    if ((!d1398) && (u_is_con(u_field(s1397, 0), &n_option_46_None) && (u_is_con(u_field(s1397, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1397, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1397, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1397, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1397, 1), 0);
      u_value v_c = u_field(u_field(s1397, 2), 0);
      d1398 = 1;
      {
        u_value t1400 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1400;
      }
    }
    if (!d1398) {
      d1398 = 1;
      {
        u_value t1401 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1401;
      }
    }
    if (!d1398) {
      u_no_case(s1397);
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
        static struct u_object l__61__62__1402 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1402), r_ts);
        p_parse_46_parse_body(v_sc, &v_body, r_ts);
        static struct u_object l_end_1403 = U_LIT("end");
        p_parse_46_keyword(&l_end_1403, r_ts);
        u_value t1404 = u_mk_con(&n_syntax_46_EProc, 4, v_ins, v_outs, v_refs, v_body);
        *r_e = t1404;
      }
    } else {
      {
        p_parse_46_lambda_function(v_sc, r_e, r_ts);
      }
    }
  }
}

u_value f_parse_46_at_section_1(u_value v_ts) {
  static struct u_object l_in_1405 = U_LIT("in");
  u_value t1407 = f_parse_46_at_keyword_2(v_ts, &l_in_1405);
  if (!u_truth(t1407)) {
    static struct u_object l_out_1406 = U_LIT("out");
    t1407 = f_parse_46_at_keyword_2(v_ts, &l_out_1406);
  }
  u_value t1409 = t1407;
  if (!u_truth(t1409)) {
    static struct u_object l_ref_1408 = U_LIT("ref");
    t1409 = f_parse_46_at_keyword_2(v_ts, &l_ref_1408);
  }
  return t1409;
}

void p_parse_46_lambda_function(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    u_value t1410 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1410;
    static struct u_object l__61__62__1411 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1411)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1412 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1412;
        u_value t1414 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1414)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1413 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1413;
          }
          t1414 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1415 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1415), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1416 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1416;
  }
}

void p_parse_46_parse_braces(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_at = NULL;
    u_value v_k = NULL;
    u_value v_v = NULL;
    u_value v_entries = NULL;
    u_value v_rev = NULL;
    u_value t1417 = f_parse_46_head_offset_1(*r_ts);
    v_at = t1417;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_expr(v_sc, &v_k, r_ts);
    static struct u_object l__61__62__1418 = U_LIT("=>");
    u_value t1419 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1418));
    v_entries = t1419;
    if (u_truth(v_entries)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_v, r_ts);
      }
    }
    static struct u_object l_for_1420 = U_LIT("for");
    u_value t1421 = f_parse_46_at_keyword_2(*r_ts, &l_for_1420);
    if (u_truth(t1421)) {
      t1421 = v_entries;
    }
    if (u_truth(t1421)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_comprehension(v_sc, v_k, v_v, r_e, r_ts);
      }
    } else {
      static struct u_object l_for_1422 = U_LIT("for");
      if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1422))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_set_comprehension(v_sc, v_k, r_e, r_ts);
        }
      } else {
        {
          u_value t1423;
          if (u_truth(v_entries)) {
            t1423 = u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))));
          } else {
            t1423 = v_k;
          }
          u_value t1424 = u_mk_con(&n_list_46_Cons, 2, t1423, u_mk_con(&n_list_46_Nil, 0));
          v_rev = t1424;
          u_value t1429 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          while (u_truth(t1429)) {
            {
              p_parse_46_advance(r_ts);
              p_parse_46_parse_expr(v_sc, &v_k, r_ts);
              static struct u_object l__61__62__1425 = U_LIT("=>");
              if (u_truth(f_prelude_46__33__61__2(v_entries, u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1425))))) {
                {
                  static struct u_object l_a_32_literal_32__123__46__46__46__125__32__1426 = U_LIT("a literal {...} has either elements a or entries k => v");
                  u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_a_32_literal_32__123__46__46__46__125__32__1426, v_at), "parse.ParseError");
                }
              }
              if (u_truth(v_entries)) {
                {
                  p_parse_46_advance(r_ts);
                  p_parse_46_parse_expr(v_sc, &v_v, r_ts);
                  u_value t1427 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)))), v_rev);
                  v_rev = t1427;
                }
              } else {
                {
                  u_value t1428 = u_mk_con(&n_list_46_Cons, 2, v_k, v_rev);
                  v_rev = t1428;
                }
              }
            }
            t1429 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          }
          u_value t1436;
          if (u_truth(v_entries)) {
            static struct u_object l_map_1430 = U_LIT("map");
            static struct u_object l_put_all_1431 = U_LIT("put_all");
            static struct u_object l_map_1432 = U_LIT("map");
            static struct u_object l_empty_1433 = U_LIT("empty");
            t1436 = f_parse_46_in_library_3(&l_map_1430, &l_put_all_1431, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1432, &l_empty_1433, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
          } else {
            static struct u_object l_set_1434 = U_LIT("set");
            static struct u_object l_from_list_1435 = U_LIT("from_list");
            t1436 = f_parse_46_in_library_3(&l_set_1434, &l_from_list_1435, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Nil, 0)));
          }
          u_value t1437 = t1436;
          *r_e = t1437;
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
  static struct u_object l_Nil_1438 = U_LIT("Nil");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1438), u_mk_con(&n_syntax_46_THole, 0)));
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
    u_value t1439 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1439;
    u_value t1440 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1440;
    u_value t1441 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1441;
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l_in_1442 = U_LIT("in");
    p_parse_46_keyword(&l_in_1442, r_ts);
    p_parse_46_parse_expr(v_sc, &v_l, r_ts);
    static struct u_object l_set_1443 = U_LIT("set");
    static struct u_object l_elements_1444 = U_LIT("elements");
    u_value t1445 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    u_value t1446 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1443, &l_elements_1444, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
    v_xs = t1445;
    v_ls = t1446;
    u_value t1452 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1452)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l_in_1447 = U_LIT("in");
        p_parse_46_keyword(&l_in_1447, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_set_1448 = U_LIT("set");
        static struct u_object l_elements_1449 = U_LIT("elements");
        u_value t1450 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
        u_value t1451 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1448, &l_elements_1449, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), v_ls);
        v_xs = t1450;
        v_ls = t1451;
      }
      t1452 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1453 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1453))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
      }
    }
    static struct u_object l_set_1454 = U_LIT("set");
    static struct u_object l_from_list_1455 = U_LIT("from_list");
    u_value t1456 = f_parse_46_in_library_3(&l_set_1454, &l_from_list_1455, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, v_r, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Nil, 0)));
    *r_e = t1456;
  }
}

void p_parse_46_map_comprehension(u_value v_sc, u_value v_k, u_value v_v, u_value *r_e, u_value *r_ts) {
  {
    u_value v_gs = NULL;
    u_value v_ls = NULL;
    u_value v_c = NULL;
    u_value t1457 = u_mk_con(&n_list_46_Nil, 0);
    v_gs = t1457;
    u_value t1458 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1458;
    u_value t1459 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1459;
    p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
    u_value t1460 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1460)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
      }
      t1460 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1461 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1461))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
        u_value t1462 = f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), v_c);
        v_c = t1462;
      }
    }
    static struct u_object l_map_1463 = U_LIT("map");
    static struct u_object l_put_all_1464 = U_LIT("put_all");
    static struct u_object l_map_1465 = U_LIT("map");
    static struct u_object l_empty_1466 = U_LIT("empty");
    u_value t1467 = f_parse_46_in_library_3(&l_map_1463, &l_put_all_1464, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))))), f_list_46_reverse_1(f_parse_46_hidden_vars_1(v_gs)), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1465, &l_empty_1466, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1467;
  }
}

void p_parse_46_map_generator(u_value v_sc, u_value *r_gs, u_value *r_ls, u_value *r_ts) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value v_m = NULL;
    p_parse_46_parse_plain(v_sc, &v_a, r_ts);
    static struct u_object l__61__62__1468 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1468), r_ts);
    p_parse_46_parse_plain(v_sc, &v_b, r_ts);
    static struct u_object l_in_1469 = U_LIT("in");
    p_parse_46_keyword(&l_in_1469, r_ts);
    p_parse_46_parse_expr(v_sc, &v_m, r_ts);
    static struct u_object l_map_1470 = U_LIT("map");
    static struct u_object l_entries_1471 = U_LIT("entries");
    u_value t1472 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1470, &l_entries_1471, u_mk_con(&n_list_46_Cons, 2, f_parse_46_unpack_2(f_list_46_reverse_1(*r_gs), v_m), u_mk_con(&n_list_46_Nil, 0))), *r_ls);
    *r_ls = t1472;
    static struct u_object l__40_entry_32__1473 = U_LIT("(entry ");
    static struct u_object l__41__1474 = U_LIT(")");
    u_value t1475 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(3, v_a, v_b, u_concat(&l__40_entry_32__1473, u_concat(u_str_show(u_add(f_list_46_length_1(*r_gs), u_mk_int(1))), &l__41__1474))), *r_gs);
    *r_gs = t1475;
  }
}

u_value f_parse_46_hidden_vars_1(u_value v_gs) {
  u_value s1476 = v_gs;
  u_value t1477 = NULL;
  if ((!t1477) && u_is_con(s1476, &n_list_46_Nil)) {
    t1477 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1477) && u_is_con(s1476, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s1476, 0), 2);
    u_value v_rest = u_field(s1476, 1);
    t1477 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0)), f_parse_46_hidden_vars_1(v_rest));
  }
  if (!t1477) {
    u_no_case(s1476);
  }
  return t1477;
}

u_value f_parse_46_unpack_2(u_value v_gs, u_value v_e) {
  u_value s1478 = v_gs;
  u_value t1479 = NULL;
  if ((!t1479) && u_is_con(s1478, &n_list_46_Nil)) {
    t1479 = v_e;
  }
  if ((!t1479) && u_is_con(s1478, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s1478, 0), 0);
    u_value v_v = u_field(u_field(s1478, 0), 1);
    u_value v_h = u_field(u_field(s1478, 0), 2);
    u_value v_rest = u_field(s1478, 1);
    t1479 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_k), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_v), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_parse_46_unpack_2(v_rest, v_e)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1479) {
    u_no_case(s1478);
  }
  return t1479;
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
        static struct u_object l_Nil_1480 = U_LIT("Nil");
        u_value t1481 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1480), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1481;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1482 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1482))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1483 = U_LIT("in");
            p_parse_46_keyword(&l_in_1483, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1484 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1485 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1484;
            v_lists = t1485;
            u_value t1489 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1489)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1486 = U_LIT("in");
                p_parse_46_keyword(&l_in_1486, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1487 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1488 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1487;
                v_lists = t1488;
              }
              t1489 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1490 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1490;
            static struct u_object l_if_1491 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1491))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1492 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1492;
          }
        } else {
          {
            u_value t1493 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1493;
            u_value t1495 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1495)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1494 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1494;
              }
              t1495 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1496 = U_LIT("Nil");
            u_value t1497 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1496), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1497;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1498 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1498;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1499 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1499))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1500 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1500;
      }
    } else {
      {
        u_value t1501 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1501;
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
    u_value t1502 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1502;
    static struct u_object l_case_1503 = U_LIT("case");
    u_value t1506 = f_parse_46_at_keyword_2(*r_ts, &l_case_1503);
    while (u_truth(t1506)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1504 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1504), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1505 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1505;
      }
      static struct u_object l_case_1507 = U_LIT("case");
      t1506 = f_parse_46_at_keyword_2(*r_ts, &l_case_1507);
    }
    static struct u_object l_else_1508 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1508))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1509 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1509;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1510 = U_LIT("end");
    p_parse_46_keyword(&l_end_1510, r_ts);
    u_value t1511 = f_list_46_reverse_1(v_rev);
    *r_cs = t1511;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1512 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1512))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1513 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1513, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
  }
}

u_value f_parse_46_pat_op1_2(u_value v_op, u_value v_p) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_pat_prefix_2(u_value v_op, u_value v_p) {
  u_value s1514 = v_p;
  u_value t1515 = NULL;
  if ((!t1515) && (u_is_con(s1514, &n_syntax_46_PLit) && u_is_con(u_field(s1514, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1514, 0), 0);
    static struct u_object l__45__1516 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1516))) {
      t1515 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1515) {
    t1515 = f_parse_46_pat_op1_2(v_op, v_p);
  }
  if (!t1515) {
    u_no_case(s1514);
  }
  return t1515;
}

u_value f_parse_46_pat_op2_3(u_value v_op, u_value v_l, u_value v_r) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_pats_2(u_value v_ps, u_value v_rest) {
  u_value s1517 = v_ps;
  u_value t1518 = NULL;
  if ((!t1518) && u_is_con(s1517, &n_list_46_Nil)) {
    t1518 = v_rest;
  }
  if ((!t1518) && u_is_con(s1517, &n_list_46_Cons)) {
    u_value v_p = u_field(s1517, 0);
    u_value v_more = u_field(s1517, 1);
    static struct u_object l_Cons_1519 = U_LIT("Cons");
    t1518 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1519), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1518) {
    u_no_case(s1517);
  }
  return t1518;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1520 = v_t;
  u_value t1521 = NULL;
  if ((!t1521) && u_is_con(s1520, &n_lex_46_TName)) {
    u_value v_n = u_field(s1520, 0);
    static struct u_object l___1522 = U_LIT("_");
    static struct u_object l_true_1523 = U_LIT("true");
    static struct u_object l_false_1524 = U_LIT("false");
    u_value t1526 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1522, u_mk_con(&n_list_46_Cons, 2, &l_true_1523, u_mk_con(&n_list_46_Cons, 2, &l_false_1524, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1526)) {
      u_value t1525 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1525)) {
        t1525 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1526 = t1525;
    }
    t1521 = t1526;
  }
  if ((!t1521) && u_is_con(s1520, &n_lex_46_TNum)) {
    t1521 = u_mk_bool(1);
  }
  if ((!t1521) && u_is_con(s1520, &n_lex_46_TStr)) {
    t1521 = u_mk_bool(1);
  }
  if ((!t1521) && u_is_con(s1520, &n_lex_46_TChar)) {
    t1521 = u_mk_bool(1);
  }
  if ((!t1521) && u_is_con(s1520, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1520, 0);
    u_value t1527 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1527)) {
      t1527 = u_eq(v_c, u_mk_char(91));
    }
    t1521 = t1527;
  }
  if (!t1521) {
    t1521 = u_mk_bool(0);
  }
  if (!t1521) {
    u_no_case(s1520);
  }
  return t1521;
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
    u_value t1528 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1528;
    u_value t1529 = u_mk_bool(1);
    v_next = t1529;
    u_value t1530 = u_mk_bool(1);
    v_go = t1530;
    u_value t1539 = v_go;
    while (u_truth(t1539)) {
      {
        u_value s1531 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1532 = 0;
        if ((!d1532) && u_is_con(s1531, &n_option_46_Some)) {
          u_value v_op = u_field(s1531, 0);
          d1532 = 1;
          {
            u_value t1533 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1534 = u_mk_bool(1);
            v_rev = t1533;
            v_next = t1534;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1532) && u_is_con(s1531, &n_option_46_None)) {
          u_value t1537 = v_next;
          if (u_truth(t1537)) {
            t1537 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1537)) {
            d1532 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1535 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1536 = u_mk_bool(0);
              v_rev = t1535;
              v_next = t1536;
            }
          }
        }
        if (!d1532) {
          d1532 = 1;
          {
            u_value t1538 = u_mk_bool(0);
            v_go = t1538;
          }
        }
        if (!d1532) {
          u_no_case(s1531);
        }
      }
      t1539 = v_go;
    }
    u_value s1540 = v_rev;
    int d1541 = 0;
    if ((!d1541) && u_is_con(s1540, &n_list_46_Nil)) {
      d1541 = 1;
      {
        static struct u_object l_pattern_32_expected_1542 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1543 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1542, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1543)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1541) {
      d1541 = 1;
      {
        u_value t1544 = f_list_46_reverse_1(v_rev);
        *r_items = t1544;
      }
    }
    if (!d1541) {
      u_no_case(s1540);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1545 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1545;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1546 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1546;
        u_value t1548 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1548)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1547 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1547;
          }
          t1548 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1549 = f_list_46_reverse_1(v_rev);
    *r_ps = t1549;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1550 = f_parse_46_head_token_1(*r_ts);
    int d1551 = 0;
    if ((!d1551) && (u_is_con(s1550, &n_lex_46_TName) && u_equals(u_field(s1550, 0), u_mk_lit("_")))) {
      d1551 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1552 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1552;
      }
    }
    if ((!d1551) && (u_is_con(s1550, &n_lex_46_TName) && u_equals(u_field(s1550, 0), u_mk_lit("true")))) {
      d1551 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1553 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1553;
      }
    }
    if ((!d1551) && (u_is_con(s1550, &n_lex_46_TName) && u_equals(u_field(s1550, 0), u_mk_lit("false")))) {
      d1551 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1554 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1554;
      }
    }
    if ((!d1551) && u_is_con(s1550, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1550, 0);
      d1551 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1555 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1555;
      }
    }
    if ((!d1551) && u_is_con(s1550, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1550, 0);
      d1551 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1556 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1556;
      }
    }
    if ((!d1551) && u_is_con(s1550, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1550, 0);
      d1551 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1557 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1557;
      }
    }
    if ((!d1551) && (u_is_con(s1550, &n_lex_46_TPunct) && u_equals(u_field(s1550, 0), u_mk_char(40)))) {
      d1551 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1558 = v_ps;
        int d1559 = 0;
        if ((!d1559) && (u_is_con(s1558, &n_list_46_Cons) && u_is_con(u_field(s1558, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1558, 0);
          d1559 = 1;
          {
            u_value t1560 = v_q;
            *r_p = t1560;
          }
        }
        if (!d1559) {
          d1559 = 1;
          {
            u_value t1561 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1561;
          }
        }
        if (!d1559) {
          u_no_case(s1558);
        }
      }
    }
    if ((!d1551) && (u_is_con(s1550, &n_lex_46_TPunct) && u_equals(u_field(s1550, 0), u_mk_char(91)))) {
      d1551 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1562 = U_LIT("Nil");
            u_value t1563 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1562), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1563;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1564 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1564;
            u_value t1566 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1566)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1565 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1565;
              }
              t1566 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1567 = U_LIT("Nil");
            u_value t1568 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1567), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1568;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1569 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1569;
          }
        }
      }
    }
    if (!d1551) {
      d1551 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1570 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1570;
          }
        } else {
          {
            u_value t1571 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1571;
          }
        }
      }
    }
    if (!d1551) {
      u_no_case(s1550);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1572 = v_t;
  u_value t1573 = NULL;
  if ((!t1573) && u_is_con(s1572, &n_lex_46_TName)) {
    u_value v_n = u_field(s1572, 0);
    static struct u_object l_skip_1574 = U_LIT("skip");
    static struct u_object l_if_1575 = U_LIT("if");
    static struct u_object l_while_1576 = U_LIT("while");
    static struct u_object l_call_1577 = U_LIT("call");
    static struct u_object l_match_1578 = U_LIT("match");
    static struct u_object l_assert_1579 = U_LIT("assert");
    static struct u_object l_assign_1580 = U_LIT("assign");
    static struct u_object l_raise_1581 = U_LIT("raise");
    static struct u_object l_try_1582 = U_LIT("try");
    t1573 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1574, u_mk_con(&n_list_46_Cons, 2, &l_if_1575, u_mk_con(&n_list_46_Cons, 2, &l_while_1576, u_mk_con(&n_list_46_Cons, 2, &l_call_1577, u_mk_con(&n_list_46_Cons, 2, &l_match_1578, u_mk_con(&n_list_46_Cons, 2, &l_assert_1579, u_mk_con(&n_list_46_Cons, 2, &l_assign_1580, u_mk_con(&n_list_46_Cons, 2, &l_raise_1581, u_mk_con(&n_list_46_Cons, 2, &l_try_1582, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1573) {
    t1573 = u_mk_bool(0);
  }
  if (!t1573) {
    u_no_case(s1572);
  }
  return t1573;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1583 = v_cs;
  u_value t1584 = NULL;
  if ((!t1584) && u_is_con(s1583, &n_list_46_Nil)) {
    t1584 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1584) && (u_is_con(s1583, &n_list_46_Cons) && u_is_con(u_field(s1583, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1583, 0), 0);
    u_value v_g = u_field(u_field(s1583, 0), 1);
    u_value v_b = u_field(u_field(s1583, 0), 2);
    u_value v_more = u_field(s1583, 1);
    t1584 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1584) {
    u_no_case(s1583);
  }
  return t1584;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1585 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1585;
    u_value t1586 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1586;
    u_value t1587 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1587;
    static struct u_object l_local_1588 = U_LIT("local");
    u_value t1590 = f_parse_46_at_keyword_2(*r_ts, &l_local_1588);
    while (u_truth(t1590)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1589 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1589)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1589 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1591 = U_LIT("local");
      t1590 = f_parse_46_at_keyword_2(*r_ts, &l_local_1591);
    }
    u_value t1593 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1593)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1592 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1592;
      }
      t1593 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1594 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1594;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1595 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1595;
    static struct u_object l__58__1596 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1596)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__1597 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1597)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1598 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1598;
      }
    }
    u_value t1599 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1599;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1600 = f_parse_46_head_token_1(*r_ts);
    int d1601 = 0;
    if ((!d1601) && (u_is_con(s1600, &n_lex_46_TName) && u_equals(u_field(s1600, 0), u_mk_lit("skip")))) {
      d1601 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1602 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1602;
      }
    }
    if ((!d1601) && (u_is_con(s1600, &n_lex_46_TName) && u_equals(u_field(s1600, 0), u_mk_lit("if")))) {
      d1601 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1601) && (u_is_con(s1600, &n_lex_46_TName) && u_equals(u_field(s1600, 0), u_mk_lit("while")))) {
      d1601 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1603 = U_LIT("end");
        p_parse_46_keyword(&l_end_1603, r_ts);
        u_value t1604 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1604;
      }
    }
    if ((!d1601) && (u_is_con(s1600, &n_lex_46_TName) && u_equals(u_field(s1600, 0), u_mk_lit("call")))) {
      d1601 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1601) && (u_is_con(s1600, &n_lex_46_TName) && u_equals(u_field(s1600, 0), u_mk_lit("match")))) {
      d1601 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1605 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1605;
      }
    }
    if ((!d1601) && (u_is_con(s1600, &n_lex_46_TName) && u_equals(u_field(s1600, 0), u_mk_lit("assert")))) {
      d1601 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1606 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1606;
      }
    }
    if ((!d1601) && (u_is_con(s1600, &n_lex_46_TName) && u_equals(u_field(s1600, 0), u_mk_lit("assign")))) {
      d1601 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1601) && (u_is_con(s1600, &n_lex_46_TName) && u_equals(u_field(s1600, 0), u_mk_lit("raise")))) {
      d1601 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1607 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1607;
      }
    }
    if ((!d1601) && (u_is_con(s1600, &n_lex_46_TName) && u_equals(u_field(s1600, 0), u_mk_lit("try")))) {
      d1601 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1608 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1608, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1609 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1609;
      }
    }
    if (!d1601) {
      d1601 = 1;
      {
        static struct u_object l_statement_32_expect_1610 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1611 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1610, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1611)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1601) {
      u_no_case(s1600);
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
    static struct u_object l_else_1612 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1612)))) {
      {
        static struct u_object l_end_1613 = U_LIT("end");
        p_parse_46_keyword(&l_end_1613, r_ts);
        u_value t1614 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1614;
      }
    } else {
      static struct u_object l_if_1615 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1615)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1616 = U_LIT("end");
          p_parse_46_keyword(&l_end_1616, r_ts);
        }
      }
    }
    u_value t1617 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1617;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1618 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1618), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1619 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1619;
    u_value t1622 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1622)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1620 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1620), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1621 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1621;
      }
      t1622 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1623 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1623;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1624 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1624;
    static struct u_object l_case_1625 = U_LIT("case");
    u_value t1628 = f_parse_46_at_keyword_2(*r_ts, &l_case_1625);
    while (u_truth(t1628)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1626 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1626), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1627 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1627;
      }
      static struct u_object l_case_1629 = U_LIT("case");
      t1628 = f_parse_46_at_keyword_2(*r_ts, &l_case_1629);
    }
    static struct u_object l_else_1630 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1630))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1631 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1631;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1632 = U_LIT("end");
    p_parse_46_keyword(&l_end_1632, r_ts);
    u_value t1633 = f_list_46_reverse_1(v_rev);
    *r_cs = t1633;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1635;
  if (u_truth(v_first)) {
    t1635 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1634 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1634)) {
      t1634 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1635 = t1634;
  }
  return t1635;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1637 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1637)) {
    static struct u_object l_out_1636 = U_LIT("out");
    t1637 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1636));
  }
  u_value t1639 = t1637;
  if (u_truth(t1639)) {
    static struct u_object l_ref_1638 = U_LIT("ref");
    t1639 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1638));
  }
  return t1639;
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
    u_value t1640 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), v_ins, v_outs, v_refs);
    *r_s = t1640;
  }
}

void p_parse_46_sections(u_value v_item_in, u_value v_item, u_value v_sc, u_value *r_ins, u_value *r_outs, u_value *r_refs, u_value *r_ts) {
  {
    u_value v_first = NULL;
    u_value t1641 = u_mk_bool(1);
    v_first = t1641;
    u_value t1642 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1643 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1644 = u_mk_con(&n_list_46_Nil, 0);
    *r_ins = t1642;
    *r_outs = t1643;
    *r_refs = t1644;
    static struct u_object l_in_1645 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1645))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_section_items(v_item_in, v_sc, r_ins, r_ts);
        u_value t1646 = u_mk_bool(0);
        v_first = t1646;
      }
    }
    static struct u_object l_out_1647 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1647, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_section_items(v_item, v_sc, r_outs, r_ts);
        u_value t1648 = u_mk_bool(0);
        v_first = t1648;
      }
    }
    static struct u_object l_ref_1649 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1649, v_first))) {
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
    u_value t1650 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1650;
    u_value t1652 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1652)) {
      {
        p_parse_46_advance(r_ts);
        u_call_proc(v_item, 1, 2, v_sc, &v_x, r_ts);
        u_value t1651 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1651;
      }
      t1652 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1653 = f_list_46_reverse_1(v_rev);
    *r_xs = t1653;
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
    u_value t1654 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0));
    *r_x = t1654;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1655 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1655;
    static struct u_object l__58__1656 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1656)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1657 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1657;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1658 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1658;
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
  u_value s1659 = v_cs;
  u_value t1660 = NULL;
  if ((!t1660) && u_is_con(s1659, &n_list_46_Nil)) {
    t1660 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1660) && (u_is_con(s1659, &n_list_46_Cons) && u_is_con(u_field(s1659, 0), &n_syntax_46_CEval))) {
    u_value v_more = u_field(s1659, 1);
    t1660 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1660) && (u_is_con(s1659, &n_list_46_Cons) && u_is_con(u_field(s1659, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1659, 1);
    t1660 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1660) && (u_is_con(s1659, &n_list_46_Cons) && u_is_con(u_field(s1659, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1659, 1);
    t1660 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1660) && u_is_con(s1659, &n_list_46_Cons)) {
    u_value v_c = u_field(s1659, 0);
    u_value v_more = u_field(s1659, 1);
    t1660 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1660) {
    u_no_case(s1659);
  }
  return t1660;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1661 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1662 = &t1661;
  u_value l1663 = v_cs;
  while (u_is_con(l1663, &n_list_46_Cons)) {
    u_value v_c = u_field(l1663, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1662 = u_append(e1662, v_c);
    }
    l1663 = u_field(l1663, 1);
  }
  return t1661;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1664 = v_c;
  u_value t1665 = NULL;
  if ((!t1665) && u_is_con(s1664, &n_syntax_46_CNotation)) {
    t1665 = u_mk_bool(1);
  }
  if (!t1665) {
    t1665 = u_mk_bool(0);
  }
  if (!t1665) {
    u_no_case(s1664);
  }
  return t1665;
}

u_value f_parse_46_Context_46_notations_1(u_value v_x) {
  u_value s1666 = v_x;
  u_value t1667 = NULL;
  if ((!t1667) && u_is_con(s1666, &n_parse_46_Context)) {
    u_value v_y = u_field(s1666, 0);
    t1667 = v_y;
  }
  if (!t1667) {
    u_no_case(s1666);
  }
  return t1667;
}

u_value f_parse_46_Context_46_modules_1(u_value v_x) {
  u_value s1668 = v_x;
  u_value t1669 = NULL;
  if ((!t1669) && u_is_con(s1668, &n_parse_46_Context)) {
    u_value v_y = u_field(s1668, 1);
    t1669 = v_y;
  }
  if (!t1669) {
    u_no_case(s1668);
  }
  return t1669;
}

u_value f_parse_46_Context_46_loading_1(u_value v_x) {
  u_value s1670 = v_x;
  u_value t1671 = NULL;
  if ((!t1671) && u_is_con(s1670, &n_parse_46_Context)) {
    u_value v_y = u_field(s1670, 2);
    t1671 = v_y;
  }
  if (!t1671) {
    u_no_case(s1670);
  }
  return t1671;
}

u_value f_parse_46_Context_46_base_1(u_value v_x) {
  u_value s1672 = v_x;
  u_value t1673 = NULL;
  if ((!t1673) && u_is_con(s1672, &n_parse_46_Context)) {
    u_value v_y = u_field(s1672, 3);
    t1673 = v_y;
  }
  if (!t1673) {
    u_no_case(s1672);
  }
  return t1673;
}

u_value f_parse_46_Context_46_dir_1(u_value v_x) {
  u_value s1674 = v_x;
  u_value t1675 = NULL;
  if ((!t1675) && u_is_con(s1674, &n_parse_46_Context)) {
    u_value v_y = u_field(s1674, 4);
    t1675 = v_y;
  }
  if (!t1675) {
    u_no_case(s1674);
  }
  return t1675;
}

u_value f_parse_46_Context_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1676 = v_x;
  u_value t1677 = NULL;
  if ((!t1677) && u_is_con(s1676, &n_parse_46_Context)) {
    u_value v_a1 = u_field(s1676, 1);
    u_value v_a2 = u_field(s1676, 2);
    u_value v_a3 = u_field(s1676, 3);
    u_value v_a4 = u_field(s1676, 4);
    t1677 = u_mk_con(&n_parse_46_Context, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1677) {
    u_no_case(s1676);
  }
  return t1677;
}

u_value f_parse_46_Context_46_modules_58__61__2(u_value v_x, u_value v_y) {
  u_value s1678 = v_x;
  u_value t1679 = NULL;
  if ((!t1679) && u_is_con(s1678, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1678, 0);
    u_value v_a2 = u_field(s1678, 2);
    u_value v_a3 = u_field(s1678, 3);
    u_value v_a4 = u_field(s1678, 4);
    t1679 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t1679) {
    u_no_case(s1678);
  }
  return t1679;
}

u_value f_parse_46_Context_46_loading_58__61__2(u_value v_x, u_value v_y) {
  u_value s1680 = v_x;
  u_value t1681 = NULL;
  if ((!t1681) && u_is_con(s1680, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1680, 0);
    u_value v_a1 = u_field(s1680, 1);
    u_value v_a3 = u_field(s1680, 3);
    u_value v_a4 = u_field(s1680, 4);
    t1681 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t1681) {
    u_no_case(s1680);
  }
  return t1681;
}

u_value f_parse_46_Context_46_base_58__61__2(u_value v_x, u_value v_y) {
  u_value s1682 = v_x;
  u_value t1683 = NULL;
  if ((!t1683) && u_is_con(s1682, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1682, 0);
    u_value v_a1 = u_field(s1682, 1);
    u_value v_a2 = u_field(s1682, 2);
    u_value v_a4 = u_field(s1682, 4);
    t1683 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t1683) {
    u_no_case(s1682);
  }
  return t1683;
}

u_value f_parse_46_Context_46_dir_58__61__2(u_value v_x, u_value v_y) {
  u_value s1684 = v_x;
  u_value t1685 = NULL;
  if ((!t1685) && u_is_con(s1684, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1684, 0);
    u_value v_a1 = u_field(s1684, 1);
    u_value v_a2 = u_field(s1684, 2);
    u_value v_a3 = u_field(s1684, 3);
    t1685 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t1685) {
    u_no_case(s1684);
  }
  return t1685;
}

u_value f_parse_46_with_commands_2(u_value v_cx, u_value v_cs) {
  return f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_with_notations_2(f_parse_46_Context_46_notations_1(v_cx), v_cs));
}

u_value f_parse_46_with_module_2(u_value v_cx, u_value v_c) {
  return f_parse_46_Context_46_modules_58__61__2(v_cx, f_list_46__43__43__2(f_parse_46_modules_of_1(v_c), f_parse_46_Context_46_modules_1(v_cx)));
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1686 = v_c;
  u_value t1687 = NULL;
  if ((!t1687) && u_is_con(s1686, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1686, 0);
    u_value v_cs = u_field(s1686, 1);
    u_value t1688 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1689 = &t1688;
    u_value l1690 = v_cs;
    while (u_is_con(l1690, &n_list_46_Cons)) {
      u_value v_d = u_field(l1690, 0);
      if (u_truth(u_mk_bool(1))) {
        e1689 = u_append(e1689, f_parse_46_modules_of_1(v_d));
      }
      l1690 = u_field(l1690, 1);
    }
    t1687 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1688));
  }
  if (!t1687) {
    t1687 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1687) {
    u_no_case(s1686);
  }
  return t1687;
}

u_value f_parse_46_own_notations_2(u_value v_cx, u_value v_n) {
  u_value s1691 = f_list_46_lookup_2(v_n, f_parse_46_Context_46_modules_1(v_cx));
  u_value t1692 = NULL;
  if ((!t1692) && u_is_con(s1691, &n_option_46_Some)) {
    u_value v_ns = u_field(s1691, 0);
    t1692 = v_ns;
  }
  if ((!t1692) && u_is_con(s1691, &n_option_46_None)) {
    t1692 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1692) {
    u_no_case(s1691);
  }
  return t1692;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1693 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1693;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1694 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1694;
        u_value t1696 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1696)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1695 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1695;
          }
          t1696 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1697 = f_list_46_reverse_1(v_rev);
    *r_ps = t1697;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1698 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1698;
    u_value s1699 = f_parse_46_head_token_1(*r_ts);
    int d1700 = 0;
    if ((!d1700) && (u_is_con(s1699, &n_lex_46_TName) && u_equals(u_field(s1699, 0), u_mk_lit("prefix")))) {
      d1700 = 1;
      {
        u_value t1701 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1701;
      }
    }
    if ((!d1700) && (u_is_con(s1699, &n_lex_46_TName) && u_equals(u_field(s1699, 0), u_mk_lit("postfix")))) {
      d1700 = 1;
      {
        u_value t1702 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1702;
      }
    }
    if (!d1700) {
      d1700 = 1;
      {
        u_value s1703 = f_parse_46_second_token_1(*r_ts);
        int d1704 = 0;
        if ((!d1704) && (u_is_con(s1703, &n_lex_46_TName) && u_equals(u_field(s1703, 0), u_mk_lit("left")))) {
          d1704 = 1;
          {
            u_value t1705 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1705;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1704) && (u_is_con(s1703, &n_lex_46_TName) && u_equals(u_field(s1703, 0), u_mk_lit("right")))) {
          d1704 = 1;
          {
            u_value t1706 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1706;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1704) {
          d1704 = 1;
          {
          }
        }
        if (!d1704) {
          u_no_case(s1703);
        }
      }
    }
    if (!d1700) {
      u_no_case(s1699);
    }
    p_parse_46_advance(r_ts);
    u_value s1707 = f_parse_46_head_token_1(*r_ts);
    int d1708 = 0;
    if ((!d1708) && u_is_con(s1707, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1707, 0);
      d1708 = 1;
      {
        u_value t1709 = v_n;
        v_p = t1709;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1708) {
      d1708 = 1;
      {
        static struct u_object l_precedence_32_expec_1710 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1711 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1710, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1711)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1708) {
      u_no_case(s1707);
    }
    u_value t1712 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1713 = u_mk_bool(1);
    v_ops = t1712;
    v_go = t1713;
    u_value t1719 = v_go;
    while (u_truth(t1719)) {
      {
        u_value s1714 = f_parse_46_head_token_1(*r_ts);
        int d1715 = 0;
        if ((!d1715) && u_is_con(s1714, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1714, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1715 = 1;
            {
              u_value t1716 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1716;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1715) && u_is_con(s1714, &n_lex_46_TName)) {
          u_value v_n = u_field(s1714, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1715 = 1;
            {
              u_value t1717 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1717;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1715) {
          d1715 = 1;
          {
            u_value t1718 = u_mk_bool(0);
            v_go = t1718;
          }
        }
        if (!d1715) {
          u_no_case(s1714);
        }
      }
      t1719 = v_go;
    }
    u_value s1720 = v_ops;
    int d1721 = 0;
    if ((!d1721) && u_is_con(s1720, &n_list_46_Nil)) {
      d1721 = 1;
      {
        static struct u_object l_operator_32_expecte_1722 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1723 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1722, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1723)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1721) {
      d1721 = 1;
      {
        u_value t1724 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1724;
      }
    }
    if (!d1721) {
      u_no_case(s1720);
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
    u_value t1725 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1726 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1725;
    *r_fs = t1726;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1727 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
            u_value t1728 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
            v_args = t1727;
            *r_fs = t1728;
            u_value t1731 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1731)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t1729 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t1730 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t1729;
                *r_fs = t1730;
              }
              t1731 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1732 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t1733 = f_list_46_reverse_1(*r_fs);
    *r_k = t1732;
    *r_fs = t1733;
    u_value s1734 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d1735 = 0;
    if ((!d1735) && u_is_con(s1734, &n_option_46_Some)) {
      u_value v_f = u_field(s1734, 0);
      d1735 = 1;
      {
        static struct u_object l_repeated_32_field_32__1736 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__1737 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__1736, u_concat(v_f, u_concat(&l__32_of_32__1737, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1735) && u_is_con(s1734, &n_option_46_None)) {
      d1735 = 1;
      {
      }
    }
    if (!d1735) {
      u_no_case(s1734);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1738 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1739 = 0;
    if ((!d1739) && (u_is_con(u_field(s1738, 0), &n_lex_46_TName) && (u_is_con(u_field(s1738, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1738, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1738, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1739 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1740 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1740;
        }
      }
    }
    if (!d1739) {
      d1739 = 1;
      {
        u_value t1741 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1741;
      }
    }
    if (!d1739) {
      u_no_case(s1738);
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
    static struct u_object l__58__61__1742 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1742), r_ts);
    static struct u_object l__124__1743 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1743)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1744 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1744;
    static struct u_object l__124__1745 = U_LIT("|");
    u_value t1747 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1745));
    while (u_truth(t1747)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1746 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1746;
      }
      static struct u_object l__124__1748 = U_LIT("|");
      t1747 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1748));
    }
    u_value s1749 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d1750 = 0;
    if ((!d1750) && u_is_con(s1749, &n_option_46_Some)) {
      u_value v_f = u_field(s1749, 0);
      d1750 = 1;
      {
        static struct u_object l_field_32__1751 = U_LIT("field ");
        static struct u_object l__32_of_32__1752 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_1753 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__1751, u_concat(v_f, u_concat(&l__32_of_32__1752, u_concat(v_n, &l__32_has_32_different_32_t_1753)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1750) && u_is_con(s1749, &n_option_46_None)) {
      d1750 = 1;
      {
      }
    }
    if (!d1750) {
      u_no_case(s1749);
    }
    u_value t1754 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_list_46__43__43__2(f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)), f_parse_46_updaters_3(v_n, v_ps, f_list_46_reverse_1(v_rev))));
    *r_cs = t1754;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s1755 = v_fs;
  u_value t1756 = NULL;
  if ((!t1756) && u_is_con(s1755, &n_list_46_Nil)) {
    t1756 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1756) && (u_is_con(s1755, &n_list_46_Cons) && u_is_con(u_field(s1755, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s1755, 0), 0);
    u_value v_rest = u_field(s1755, 1);
    t1756 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t1756) && (u_is_con(s1755, &n_list_46_Cons) && u_is_con(u_field(s1755, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1755, 1);
    t1756 = f_parse_46_field_names_1(v_rest);
  }
  if (!t1756) {
    u_no_case(s1755);
  }
  return t1756;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s1757 = v_fs;
  u_value t1758 = NULL;
  if ((!t1758) && u_is_con(s1757, &n_list_46_Nil)) {
    t1758 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1758) && u_is_con(s1757, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1757, 0), 0);
    u_value v_t = u_field(u_field(u_field(s1757, 0), 1), 3);
    u_value v_rest = u_field(s1757, 1);
    u_value t1759 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1760 = &t1759;
    u_value l1761 = f_parse_46_fields_named_2(v_f, v_rest);
    while (u_is_con(l1761, &n_list_46_Cons)) {
      u_value v_e = u_field(l1761, 0);
      if (u_truth(u_mk_bool(1))) {
        e1760 = u_append(e1760, f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t));
      }
      l1761 = u_field(l1761, 1);
    }
    u_value t1762;
    if (u_truth(f_list_46_contains_2(u_mk_bool(1), t1759))) {
      t1762 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t1762 = f_parse_46_differing_1(v_rest);
    }
    t1758 = t1762;
  }
  if (!t1758) {
    u_no_case(s1757);
  }
  return t1758;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s1763 = v_e;
  u_value t1764 = NULL;
  if (!t1764) {
    u_value v_t = u_field(u_field(s1763, 1), 3);
    t1764 = v_t;
  }
  if (!t1764) {
    u_no_case(s1763);
  }
  return t1764;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s1765 = v_fs;
  u_value t1766 = NULL;
  if ((!t1766) && u_is_con(s1765, &n_list_46_Nil)) {
    t1766 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1766) && u_is_con(s1765, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s1765, 0), 0);
    u_value v_d = u_field(u_field(s1765, 0), 1);
    u_value v_rest = u_field(s1765, 1);
    u_value t1767;
    if (u_truth(u_eq(v_g, v_f))) {
      t1767 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t1767 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t1766 = t1767;
  }
  if (!t1766) {
    u_no_case(s1765);
  }
  return t1766;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1768 = v_ks;
  u_value t1769 = NULL;
  if ((!t1769) && u_is_con(s1768, &n_list_46_Nil)) {
    t1769 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1769) && (u_is_con(s1768, &n_list_46_Cons) && u_is_con(u_field(u_field(s1768, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1768, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1768, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1768, 0), 1);
    u_value v_rest = u_field(s1768, 1);
    t1769 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1769) {
    u_no_case(s1768);
  }
  return t1769;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1770 = v_fs;
  u_value t1771 = NULL;
  if ((!t1771) && u_is_con(s1770, &n_list_46_Nil)) {
    t1771 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1771) && (u_is_con(s1770, &n_list_46_Cons) && u_is_con(u_field(u_field(s1770, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1770, 0), 0), 0);
    u_value v_t = u_field(u_field(s1770, 0), 1);
    u_value v_rest = u_field(s1770, 1);
    t1771 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1771) && (u_is_con(s1770, &n_list_46_Cons) && u_is_con(u_field(u_field(s1770, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1770, 1);
    t1771 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1771) {
    u_no_case(s1770);
  }
  return t1771;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1776 = f_parse_46_fields_of_1(v_ks);
  u_value t1775;
  {
    u_value v_fs = t1776;
    u_value t1772 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1773 = &t1772;
    u_value l1774 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1774, &n_list_46_Cons)) {
      u_value v_f = u_field(l1774, 0);
      if (u_truth(u_mk_bool(1))) {
        e1773 = u_append(e1773, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1774 = u_field(l1774, 1);
    }
    t1775 = t1772;
  }
  return t1775;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1777 = U_LIT("x");
  u_value t1778 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1779 = &t1778;
  u_value l1780 = v_ps;
  while (u_is_con(l1780, &n_list_46_Cons)) {
    u_value v_p = u_field(l1780, 0);
    if (u_truth(u_mk_bool(1))) {
      e1779 = u_append(e1779, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1780 = u_field(l1780, 1);
  }
  u_value t1788 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1777), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1789 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1778);
  u_value t1787;
  {
    u_value v_x = t1788;
    u_value v_owner = t1789;
    static struct u_object l__46__1781 = U_LIT(".");
    static struct u_object l_x_1782 = U_LIT("x");
    u_value t1783 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1784 = &t1783;
    u_value l1785 = v_es;
    while (u_is_con(l1785, &n_list_46_Cons)) {
      u_value v_e = u_field(l1785, 0);
      if (u_truth(u_mk_bool(1))) {
        e1784 = u_append(e1784, f_parse_46_field_case_1(v_e));
      }
      l1785 = u_field(l1785, 1);
    }
    u_value t1786;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1786 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1786 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1787 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1781, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1782), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1783, t1786)));
  }
  return t1787;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1790 = v_es;
  u_value t1791 = NULL;
  if ((!t1791) && u_is_con(s1790, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1790, 0), 1), 3);
    t1791 = v_t;
  }
  if (!t1791) {
    t1791 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1791) {
    u_no_case(s1790);
  }
  return t1791;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s1792 = v_e;
  u_value t1793 = NULL;
  if (!t1793) {
    u_value v_k = u_field(u_field(s1792, 1), 0);
    u_value v_i = u_field(u_field(s1792, 1), 1);
    u_value v_n = u_field(u_field(s1792, 1), 2);
    u_value t1794 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1795 = &t1794;
    u_value l1796 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1796, &n_list_46_Cons)) {
      u_value v_j = u_field(l1796, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1798;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1797 = U_LIT("y");
          t1798 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1797), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1798 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1795 = u_append(e1795, t1798);
      }
      l1796 = u_field(l1796, 1);
    }
    static struct u_object l_y_1799 = U_LIT("y");
    t1793 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1794), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1799), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1793) {
    u_no_case(s1792);
  }
  return t1793;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_MissingField_1800 = U_LIT("MissingField");
  static struct u_object l__46__1801 = U_LIT(".");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_ERaise, 2, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_MissingField_1800), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_t, u_concat(&l__46__1801, v_f)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_parse_46_updaters_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1806 = f_parse_46_fields_of_1(v_ks);
  u_value t1805;
  {
    u_value v_fs = t1806;
    u_value t1802 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1803 = &t1802;
    u_value l1804 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1804, &n_list_46_Cons)) {
      u_value v_f = u_field(l1804, 0);
      if (u_truth(u_mk_bool(1))) {
        e1803 = u_append(e1803, f_parse_46_updater_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1804 = u_field(l1804, 1);
    }
    t1805 = t1802;
  }
  return t1805;
}

u_value f_parse_46_updater_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1807 = U_LIT("x");
  u_value t1808 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1809 = &t1808;
  u_value l1810 = v_ps;
  while (u_is_con(l1810, &n_list_46_Cons)) {
    u_value v_p = u_field(l1810, 0);
    if (u_truth(u_mk_bool(1))) {
      e1809 = u_append(e1809, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1810 = u_field(l1810, 1);
  }
  u_value t1820 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1807), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1821 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1808);
  u_value t1819;
  {
    u_value v_x = t1820;
    u_value v_owner = t1821;
    static struct u_object l__46__1811 = U_LIT(".");
    static struct u_object l__58__61__1812 = U_LIT(":=");
    static struct u_object l_x_1813 = U_LIT("x");
    static struct u_object l_y_1814 = U_LIT("y");
    u_value t1815 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1816 = &t1815;
    u_value l1817 = v_es;
    while (u_is_con(l1817, &n_list_46_Cons)) {
      u_value v_e = u_field(l1817, 0);
      if (u_truth(u_mk_bool(1))) {
        e1816 = u_append(e1816, f_parse_46_update_case_1(v_e));
      }
      l1817 = u_field(l1817, 1);
    }
    u_value t1818;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1818 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1818 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1819 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1811, u_concat(v_f, &l__58__61__1812)))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1813), v_owner), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1814), f_parse_46_field_type_1(v_es)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), v_owner, u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1815, t1818)));
  }
  return t1819;
}

u_value f_parse_46_update_case_1(u_value v_e) {
  u_value s1822 = v_e;
  u_value t1823 = NULL;
  if (!t1823) {
    u_value v_k = u_field(u_field(s1822, 1), 0);
    u_value v_i = u_field(u_field(s1822, 1), 1);
    u_value v_n = u_field(u_field(s1822, 1), 2);
    u_value t1824 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1825 = &t1824;
    u_value l1826 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1826, &n_list_46_Cons)) {
      u_value v_j = u_field(l1826, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1828;
        if (u_truth(u_eq(v_j, v_i))) {
          t1828 = u_mk_con(&n_syntax_46_PWild, 0);
        } else {
          static struct u_object l_a_1827 = U_LIT("a");
          t1828 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l_a_1827, u_str_show(v_j))), u_mk_con(&n_syntax_46_THole, 0)));
        }
        e1825 = u_append(e1825, t1828);
      }
      l1826 = u_field(l1826, 1);
    }
    u_value t1829 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1830 = &t1829;
    u_value l1831 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1831, &n_list_46_Cons)) {
      u_value v_j = u_field(l1831, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1834;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1832 = U_LIT("y");
          t1834 = &l_y_1832;
        } else {
          static struct u_object l_a_1833 = U_LIT("a");
          t1834 = u_concat(&l_a_1833, u_str_show(v_j));
        }
        e1830 = u_append(e1830, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(t1834), u_mk_con(&n_syntax_46_THole, 0))));
      }
      l1831 = u_field(l1831, 1);
    }
    t1823 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1824), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_k, u_mk_con(&n_syntax_46_THole, 0))), t1829));
  }
  if (!t1823) {
    u_no_case(s1822);
  }
  return t1823;
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t1835;
  if (u_truth(u_eq(v_i, v_n))) {
    t1835 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1835 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t1835;
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
    u_value t1836 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1836;
    u_value t1838 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1838)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1837 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1837;
      }
      t1838 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1839 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1839), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1840 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1840), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1841 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1841;
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
    static struct u_object l__58__61__1842 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1842), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1843 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1843;
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
    u_value t1844 = f_parse_46_Context_46_notations_1(v_cx);
    v_ns = t1844;
    u_value s1845 = f_parse_46_head_token_1(*r_ts);
    int d1846 = 0;
    if ((!d1846) && (u_is_con(s1845, &n_lex_46_TName) && u_equals(u_field(s1845, 0), u_mk_lit("import")))) {
      d1846 = 1;
      {
        u_value t1847 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1847;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Context_46_modules_1(v_cx))))) {
          {
            u_value t1848 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1848;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_cx, v_at, r_cs);
            u_value t1849 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1849;
          }
        }
      }
    }
    if ((!d1846) && u_is_con(s1845, &n_lex_46_TName)) {
      u_value v_w = u_field(s1845, 0);
      static struct u_object l_prefix_1851 = U_LIT("prefix");
      static struct u_object l_postfix_1852 = U_LIT("postfix");
      static struct u_object l_infix_1853 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1851, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1852, u_mk_con(&n_list_46_Cons, 2, &l_infix_1853, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1846 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1850 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1850;
        }
      }
    }
    if ((!d1846) && (u_is_con(s1845, &n_lex_46_TName) && u_equals(u_field(s1845, 0), u_mk_lit("type")))) {
      d1846 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1854 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1854), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1855 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1855;
      }
    }
    if ((!d1846) && (u_is_con(s1845, &n_lex_46_TName) && u_equals(u_field(s1845, 0), u_mk_lit("data")))) {
      d1846 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1846) && (u_is_con(s1845, &n_lex_46_TName) && u_equals(u_field(s1845, 0), u_mk_lit("function")))) {
      d1846 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1856 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1856;
      }
    }
    if ((!d1846) && (u_is_con(s1845, &n_lex_46_TName) && u_equals(u_field(s1845, 0), u_mk_lit("procedure")))) {
      d1846 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1857 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1857;
      }
    }
    if ((!d1846) && (u_is_con(s1845, &n_lex_46_TName) && u_equals(u_field(s1845, 0), u_mk_lit("eval")))) {
      d1846 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_e, r_ts);
        u_value t1858 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CEval, 1, v_e), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1858;
      }
    }
    if ((!d1846) && (u_is_con(s1845, &n_lex_46_TName) && u_equals(u_field(s1845, 0), u_mk_lit("run")))) {
      d1846 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1859 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1859), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1860 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1860;
      }
    }
    if ((!d1846) && (u_is_con(s1845, &n_lex_46_TName) && u_equals(u_field(s1845, 0), u_mk_lit("test")))) {
      d1846 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1861 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1861), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1862 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1862;
      }
    }
    if (!d1846) {
      d1846 = 1;
      {
        static struct u_object l_declaration_32_expe_1863 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1864 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1863, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1864)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1846) {
      u_no_case(s1845);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_cx) {
  {
    u_value s1865 = v_cs;
    int d1866 = 0;
    if ((!d1866) && u_is_con(s1865, &n_list_46_Nil)) {
      d1866 = 1;
      {
      }
    }
    if ((!d1866) && (u_is_con(s1865, &n_list_46_Cons) && u_is_con(u_field(s1865, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1865, 0), 0);
      u_value v_p = u_field(u_field(s1865, 0), 1);
      u_value v_ops = u_field(u_field(s1865, 0), 2);
      u_value v_more = u_field(s1865, 1);
      d1866 = 1;
      {
        u_value t1867 = f_parse_46_with_commands_2(*r_cx, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_cx = t1867;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1866) && (u_is_con(s1865, &n_list_46_Cons) && u_is_con(u_field(s1865, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1865, 0), 0);
      u_value v_ds = u_field(u_field(s1865, 0), 1);
      u_value v_more = u_field(s1865, 1);
      d1866 = 1;
      {
        u_value t1868 = f_parse_46_with_module_2(*r_cx, u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds));
        *r_cx = t1868;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1866) && (u_is_con(s1865, &n_list_46_Cons) && u_is_con(u_field(s1865, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1865, 0), 0);
      u_value v_more = u_field(s1865, 1);
      d1866 = 1;
      {
        u_value t1869 = f_parse_46_with_commands_2(*r_cx, f_parse_46_own_notations_2(*r_cx, v_n));
        *r_cx = t1869;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1866) && u_is_con(s1865, &n_list_46_Cons)) {
      u_value v_more = u_field(s1865, 1);
      d1866 = 1;
      {
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if (!d1866) {
      u_no_case(s1865);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_cx, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1870 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1870;
    u_value t1872 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1872)) {
      {
        p_parse_46_parse_command(*r_cx, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_cx);
        u_value t1871 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1871;
      }
      t1872 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1873 = f_list_46_reverse_1(v_rev);
    *r_cs = t1873;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1874 = v_dir;
    int d1875 = 0;
    if ((!d1875) && u_is_con(s1874, &n_option_46_Some)) {
      u_value v_d = u_field(s1874, 0);
      d1875 = 1;
      {
        {
          struct u_handler *h1878 = u_push();
          if (setjmp(h1878->env) == 0) {
            {
              static struct u_object l__47__1876 = U_LIT("/");
              static struct u_object l__46_u_1877 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1876, u_concat(v_n, &l__46_u_1877))), r_text);
            }
            u_pop(h1878);
          } else {
            u_pop(h1878);
            int d1879 = 0;
            if ((!d1879) && (u_has_type(h1878, "os.OsError") && u_is_con(u_caught(h1878), &n_os_46_OsError))) {
              d1879 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1879) {
              u_reraise(h1878);
            }
          }
        }
      }
    }
    if ((!d1875) && u_is_con(s1874, &n_option_46_None)) {
      d1875 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1875) {
      u_no_case(s1874);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1880 = u_push();
      if (setjmp(h1880->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1880);
      } else {
        u_pop(h1880);
        int d1881 = 0;
        if ((!d1881) && (u_has_type(h1880, "os.OsError") && u_is_con(u_caught(h1880), &n_os_46_OsError))) {
          d1881 = 1;
          {
            static struct u_object l_unknown_32_module_32__1882 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1882, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1881) {
          u_reraise(h1880);
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
        static struct u_object l_cyclic_32_import_32_of_1883 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1883, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Context_46_dir_1(v_cx), v_at, &v_text);
    u_value t1884 = f_parse_46_Context_46_loading_58__61__2(f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_base_notations_1(v_cx)), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Context_46_loading_1(v_cx)));
    v_mcx = t1884;
    {
      struct u_handler *h1885 = u_push();
      if (setjmp(h1885->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mcx, &v_mts);
        }
        u_pop(h1885);
      } else {
        u_pop(h1885);
        int d1886 = 0;
        if ((!d1886) && (u_has_type(h1885, "lex.LexError") && u_is_con(u_caught(h1885), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1885), 0);
          u_value v_i = u_field(u_caught(h1885), 1);
          d1886 = 1;
          {
            static struct u_object l__32_in_32__1887 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1887, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1886) && (u_has_type(h1885, "parse.ParseError") && u_is_con(u_caught(h1885), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1885), 0);
          u_value v_i = u_field(u_caught(h1885), 1);
          d1886 = 1;
          {
            static struct u_object l__32_in_32__1888 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1888, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1886) {
          u_reraise(h1885);
        }
      }
    }
  }
}

u_value f_parse_46_base_notations_1(u_value v_cx) {
  u_value s1889 = f_parse_46_Context_46_base_1(v_cx);
  u_value t1890 = NULL;
  if ((!t1890) && u_is_con(s1889, &n_option_46_Some)) {
    u_value v_b = u_field(s1889, 0);
    t1890 = v_b;
  }
  if ((!t1890) && u_is_con(s1889, &n_option_46_None)) {
    t1890 = f_parse_46_Context_46_notations_1(v_cx);
  }
  if (!t1890) {
    u_no_case(s1889);
  }
  return t1890;
}

void p_parse_46_parse_text(u_value v_text, u_value v_cx, u_value *r_p) {
  {
    u_value v_ts = NULL;
    u_value v_cs = NULL;
    u_value v_after = NULL;
    u_value t1891 = v_cx;
    v_after = t1891;
    {
      struct u_handler *h1892 = u_push();
      if (setjmp(h1892->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1892);
      } else {
        u_pop(h1892);
        int d1893 = 0;
        if ((!d1893) && (u_has_type(h1892, "lex.LexError") && u_is_con(u_caught(h1892), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1892), 0);
          u_value v_i = u_field(u_caught(h1892), 1);
          d1893 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1893) {
          u_reraise(h1892);
        }
      }
    }
    p_parse_46_parse_commands(&v_cs, &v_after, &v_ts);
    u_value t1894 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1894;
  }
}

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p) {
  {
    u_value v_cx = NULL;
    u_value t1895 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    v_cx = t1895;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), &v_cx);
    p_parse_46_parse_text(v_text, f_parse_46_Context_46_dir_58__61__2(f_parse_46_Context_46_base_58__61__2(f_parse_46_Context_46_loading_58__61__2(v_cx, u_mk_con(&n_list_46_Cons, 2, v_name, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_option_46_Some, 1, f_parse_46_Context_46_notations_1(v_cx))), v_dir), r_p);
  }
}

void p_parse_46_parse_library(u_value v_n, u_value *r_p) {
  {
    u_value v_text = NULL;
    u_value v_pre = NULL;
    p_parse_46_library_text(v_n, u_mk_int(0), &v_text);
    static struct u_object l_prelude_1896 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1896))) {
      {
        static struct u_object l_prelude_1897 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1897, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1898 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1898, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1899 = v_xs;
  u_value t1900 = NULL;
  if ((!t1900) && (u_is_con(s1899, &n_list_46_Cons) && u_is_con(u_field(s1899, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1899, 0);
    t1900 = v_x;
  }
  if ((!t1900) && u_is_con(s1899, &n_list_46_Cons)) {
    u_value v_more = u_field(s1899, 1);
    t1900 = f_parse_46_last_part_1(v_more);
  }
  if (!t1900) {
    static struct u_object l__1901 = U_LIT("");
    t1900 = &l__1901;
  }
  if (!t1900) {
    u_no_case(s1899);
  }
  return t1900;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1906 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1905;
  {
    u_value v_f = t1906;
    u_value t1903 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1903)) {
      static struct u_object l__46_u_1902 = U_LIT(".u");
      t1903 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1902);
    }
    u_value t1904;
    if (u_truth(t1903)) {
      t1904 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1904 = v_f;
    }
    t1905 = t1904;
  }
  return t1905;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1911 = u_str_split(v_path, u_mk_char(47));
  u_value t1910;
  {
    u_value v_parts = t1911;
    u_value t1909;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1907 = U_LIT(".");
      t1909 = &l__46__1907;
    } else {
      static struct u_object l__47__1908 = U_LIT("/");
      t1909 = u_str_join(&l__47__1908, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1910 = t1909;
  }
  return t1910;
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
    static struct u_object l_prelude_1912 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1912, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1913 = u_mk_tuple(2, v_cs, v_ds);
    int d1914 = 0;
    if ((!d1914) && (u_is_con(u_field(s1913, 0), &n_list_46_Cons) && u_is_con(u_field(s1913, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1913, 0), 0);
      u_value v_more = u_field(u_field(s1913, 0), 1);
      u_value v_d = u_field(u_field(s1913, 1), 0);
      u_value v_rest = u_field(u_field(s1913, 1), 1);
      d1914 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1914) {
      d1914 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1914) {
      u_no_case(s1913);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1915 = v_paths;
    int d1916 = 0;
    if ((!d1916) && u_is_con(s1915, &n_list_46_Nil)) {
      d1916 = 1;
      {
      }
    }
    if ((!d1916) && u_is_con(s1915, &n_list_46_Cons)) {
      u_value v_path = u_field(s1915, 0);
      u_value v_more = u_field(s1915, 1);
      d1916 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1916) {
      u_no_case(s1915);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1917 = v_paths;
    int d1918 = 0;
    if ((!d1918) && u_is_con(s1917, &n_list_46_Nil)) {
      d1918 = 1;
      {
      }
    }
    if ((!d1918) && u_is_con(s1917, &n_list_46_Cons)) {
      u_value v_path = u_field(s1917, 0);
      u_value v_more = u_field(s1917, 1);
      d1918 = 1;
      {
        u_value t1919 = u_mk_bool(0);
        v_failed = t1919;
        {
          struct u_handler *h1920 = u_push();
          if (setjmp(h1920->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1920);
          } else {
            u_pop(h1920);
            int d1921 = 0;
            if ((!d1921) && (u_has_type(h1920, "parse.ParseError") && u_is_con(u_caught(h1920), &n_parse_46_ParseError))) {
              d1921 = 1;
              {
                u_value t1922 = u_mk_bool(1);
                v_failed = t1922;
              }
            }
            if (!d1921) {
              u_reraise(h1920);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1918) {
      u_no_case(s1917);
    }
  }
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s1923 = v_x;
  u_value t1924 = NULL;
  if ((!t1924) && u_is_con(s1923, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1923, 0);
    t1924 = v_y;
  }
  if (!t1924) {
    u_no_case(s1923);
  }
  return t1924;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s1925 = v_x;
  u_value t1926 = NULL;
  if ((!t1926) && u_is_con(s1925, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1925, 1);
    t1926 = v_y;
  }
  if (!t1926) {
    u_no_case(s1925);
  }
  return t1926;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s1927 = v_x;
  u_value t1928 = NULL;
  if ((!t1928) && u_is_con(s1927, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1927, 2);
    t1928 = v_y;
  }
  if (!t1928) {
    u_no_case(s1927);
  }
  return t1928;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s1929 = v_x;
  u_value t1930 = NULL;
  if ((!t1930) && u_is_con(s1929, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1929, 3);
    t1930 = v_y;
  }
  if (!t1930) {
    u_no_case(s1929);
  }
  return t1930;
}

u_value f_compile_46_Globals_46_procs_1(u_value v_x) {
  u_value s1931 = v_x;
  u_value t1932 = NULL;
  if ((!t1932) && u_is_con(s1931, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1931, 4);
    t1932 = v_y;
  }
  if (!t1932) {
    u_no_case(s1931);
  }
  return t1932;
}

u_value f_compile_46_Globals_46_funs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1933 = v_x;
  u_value t1934 = NULL;
  if ((!t1934) && u_is_con(s1933, &n_compile_46_Globals)) {
    u_value v_a1 = u_field(s1933, 1);
    u_value v_a2 = u_field(s1933, 2);
    u_value v_a3 = u_field(s1933, 3);
    u_value v_a4 = u_field(s1933, 4);
    t1934 = u_mk_con(&n_compile_46_Globals, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1934) {
    u_no_case(s1933);
  }
  return t1934;
}

u_value f_compile_46_Globals_46_constrs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1935 = v_x;
  u_value t1936 = NULL;
  if ((!t1936) && u_is_con(s1935, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1935, 0);
    u_value v_a2 = u_field(s1935, 2);
    u_value v_a3 = u_field(s1935, 3);
    u_value v_a4 = u_field(s1935, 4);
    t1936 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t1936) {
    u_no_case(s1935);
  }
  return t1936;
}

u_value f_compile_46_Globals_46_arities_58__61__2(u_value v_x, u_value v_y) {
  u_value s1937 = v_x;
  u_value t1938 = NULL;
  if ((!t1938) && u_is_con(s1937, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1937, 0);
    u_value v_a1 = u_field(s1937, 1);
    u_value v_a3 = u_field(s1937, 3);
    u_value v_a4 = u_field(s1937, 4);
    t1938 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t1938) {
    u_no_case(s1937);
  }
  return t1938;
}

u_value f_compile_46_Globals_46_fields_58__61__2(u_value v_x, u_value v_y) {
  u_value s1939 = v_x;
  u_value t1940 = NULL;
  if ((!t1940) && u_is_con(s1939, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1939, 0);
    u_value v_a1 = u_field(s1939, 1);
    u_value v_a2 = u_field(s1939, 2);
    u_value v_a4 = u_field(s1939, 4);
    t1940 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t1940) {
    u_no_case(s1939);
  }
  return t1940;
}

u_value f_compile_46_Globals_46_procs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1941 = v_x;
  u_value t1942 = NULL;
  if ((!t1942) && u_is_con(s1941, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1941, 0);
    u_value v_a1 = u_field(s1941, 1);
    u_value v_a2 = u_field(s1941, 2);
    u_value v_a3 = u_field(s1941, 3);
    t1942 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t1942) {
    u_no_case(s1941);
  }
  return t1942;
}

u_value f_compile_46_Gen_46_count_1(u_value v_x) {
  u_value s1943 = v_x;
  u_value t1944 = NULL;
  if ((!t1944) && u_is_con(s1943, &n_compile_46_Gen)) {
    u_value v_y = u_field(s1943, 0);
    t1944 = v_y;
  }
  if (!t1944) {
    u_no_case(s1943);
  }
  return t1944;
}

u_value f_compile_46_Gen_46_lifted_1(u_value v_x) {
  u_value s1945 = v_x;
  u_value t1946 = NULL;
  if ((!t1946) && u_is_con(s1945, &n_compile_46_Gen)) {
    u_value v_y = u_field(s1945, 1);
    t1946 = v_y;
  }
  if (!t1946) {
    u_no_case(s1945);
  }
  return t1946;
}

u_value f_compile_46_Gen_46_count_58__61__2(u_value v_x, u_value v_y) {
  u_value s1947 = v_x;
  u_value t1948 = NULL;
  if ((!t1948) && u_is_con(s1947, &n_compile_46_Gen)) {
    u_value v_a1 = u_field(s1947, 1);
    t1948 = u_mk_con(&n_compile_46_Gen, 2, v_y, v_a1);
  }
  if (!t1948) {
    u_no_case(s1947);
  }
  return t1948;
}

u_value f_compile_46_Gen_46_lifted_58__61__2(u_value v_x, u_value v_y) {
  u_value s1949 = v_x;
  u_value t1950 = NULL;
  if ((!t1950) && u_is_con(s1949, &n_compile_46_Gen)) {
    u_value v_a0 = u_field(s1949, 0);
    t1950 = u_mk_con(&n_compile_46_Gen, 2, v_a0, v_y);
  }
  if (!t1950) {
    u_no_case(s1949);
  }
  return t1950;
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
  static struct u_object l_u_value_1951 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1951);
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
  static struct u_object l_u_value_1952 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1952));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t1953 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1954 = &t1953;
  u_value l1955 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l1955, &n_list_46_Cons)) {
    u_value v_c = u_field(l1955, 0);
    if (u_truth(u_mk_bool(1))) {
      e1954 = u_append(e1954, f_compile_46_mangle_char_1(v_c));
    }
    l1955 = u_field(l1955, 1);
  }
  return u_string(f_list_46_concat_1(t1953));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t1956 = u_is_letter(v_c);
  if (!u_truth(t1956)) {
    t1956 = u_is_digit(v_c);
  }
  u_value t1957 = t1956;
  if (!u_truth(t1957)) {
    t1957 = u_eq(v_c, u_mk_char(95));
  }
  u_value t1960;
  if (u_truth(t1957)) {
    t1960 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___1958 = U_LIT("_");
    static struct u_object l___1959 = U_LIT("_");
    t1960 = u_chars(u_concat(&l___1958, u_concat(u_str_show(u_ord(v_c)), &l___1959)));
  }
  return t1960;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__1961 = U_LIT("f_");
  static struct u_object l___1962 = U_LIT("_");
  return u_concat(&l_f__1961, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___1962, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__1963 = U_LIT("p_");
  return u_concat(&l_p__1963, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__1964 = U_LIT("v_");
  return u_concat(&l_v__1964, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__1965 = U_LIT("r_");
  return u_concat(&l_r__1965, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__1966 = U_LIT("+");
  static struct u_object l_u_add_1967 = U_LIT("u_add");
  static struct u_object l__45__1968 = U_LIT("-");
  static struct u_object l_u_sub_1969 = U_LIT("u_sub");
  static struct u_object l__42__1970 = U_LIT("*");
  static struct u_object l_u_mul_1971 = U_LIT("u_mul");
  static struct u_object l__47__1972 = U_LIT("/");
  static struct u_object l_u_div_1973 = U_LIT("u_div");
  static struct u_object l__37__1974 = U_LIT("%");
  static struct u_object l_u_mod_1975 = U_LIT("u_mod");
  static struct u_object l__60__1976 = U_LIT("<");
  static struct u_object l_u_lt_1977 = U_LIT("u_lt");
  static struct u_object l__61__61__1978 = U_LIT("==");
  static struct u_object l_u_eq_1979 = U_LIT("u_eq");
  static struct u_object l_compare_1980 = U_LIT("compare");
  static struct u_object l_u_compare_1981 = U_LIT("u_compare");
  static struct u_object l_chars_1982 = U_LIT("chars");
  static struct u_object l_u_chars_1983 = U_LIT("u_chars");
  static struct u_object l_string_1984 = U_LIT("string");
  static struct u_object l_u_string_1985 = U_LIT("u_string");
  static struct u_object l_ord_1986 = U_LIT("ord");
  static struct u_object l_u_ord_1987 = U_LIT("u_ord");
  static struct u_object l_chr_1988 = U_LIT("chr");
  static struct u_object l_u_chr_1989 = U_LIT("u_chr");
  static struct u_object l__94__1990 = U_LIT("^");
  static struct u_object l_u_concat_1991 = U_LIT("u_concat");
  static struct u_object l_str_size_1992 = U_LIT("str_size");
  static struct u_object l_u_str_size_1993 = U_LIT("u_str_size");
  static struct u_object l_str_join_1994 = U_LIT("str_join");
  static struct u_object l_u_str_join_1995 = U_LIT("u_str_join");
  static struct u_object l_str_slice_1996 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_1997 = U_LIT("u_str_slice");
  static struct u_object l_str_split_1998 = U_LIT("str_split");
  static struct u_object l_u_str_split_1999 = U_LIT("u_str_split");
  static struct u_object l_str_get_2000 = U_LIT("str_get");
  static struct u_object l_u_str_get_2001 = U_LIT("u_str_get");
  static struct u_object l_str_contains_2002 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_2003 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_2004 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_2005 = U_LIT("u_str_compare");
  static struct u_object l_str_show_2006 = U_LIT("str_show");
  static struct u_object l_u_str_show_2007 = U_LIT("u_str_show");
  static struct u_object l_str_quote_2008 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_2009 = U_LIT("u_str_quote");
  static struct u_object l_str_number_2010 = U_LIT("str_number");
  static struct u_object l_u_str_number_2011 = U_LIT("u_str_number");
  static struct u_object l_is_digit_2012 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_2013 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_2014 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_2015 = U_LIT("u_is_letter");
  static struct u_object l_is_space_2016 = U_LIT("is_space");
  static struct u_object l_u_is_space_2017 = U_LIT("u_is_space");
  u_pop(handler);
  cache = f_map_46_put_all_2(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__1966), u_mk_tuple(2, &l_u_add_1967, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__1968), u_mk_tuple(2, &l_u_sub_1969, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__1970), u_mk_tuple(2, &l_u_mul_1971, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__1972), u_mk_tuple(2, &l_u_div_1973, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__1974), u_mk_tuple(2, &l_u_mod_1975, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__1976), u_mk_tuple(2, &l_u_lt_1977, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__1978), u_mk_tuple(2, &l_u_eq_1979, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_1980), u_mk_tuple(2, &l_u_compare_1981, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_1982), u_mk_tuple(2, &l_u_chars_1983, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_1984), u_mk_tuple(2, &l_u_string_1985, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_1986), u_mk_tuple(2, &l_u_ord_1987, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_1988), u_mk_tuple(2, &l_u_chr_1989, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__1990), u_mk_tuple(2, &l_u_concat_1991, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_1992), u_mk_tuple(2, &l_u_str_size_1993, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_1994), u_mk_tuple(2, &l_u_str_join_1995, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_1996), u_mk_tuple(2, &l_u_str_slice_1997, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_1998), u_mk_tuple(2, &l_u_str_split_1999, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_2000), u_mk_tuple(2, &l_u_str_get_2001, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_2002), u_mk_tuple(2, &l_u_str_contains_2003, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_2004), u_mk_tuple(2, &l_u_str_compare_2005, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_2006), u_mk_tuple(2, &l_u_str_show_2007, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_2008), u_mk_tuple(2, &l_u_str_quote_2009, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_2010), u_mk_tuple(2, &l_u_str_number_2011, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_2012), u_mk_tuple(2, &l_u_is_digit_2013, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_2014), u_mk_tuple(2, &l_u_is_letter_2015, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_2016), u_mk_tuple(2, &l_u_is_space_2017, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))), f_map_46_empty_0());
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__2018 = U_LIT("&");
  static struct u_object l_n__2019 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__2018, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__2019, f_compile_46_mangle_1(v_n))));
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
  u_value s2020 = v_n;
  u_value t2021 = NULL;
  if ((!t2021) && (u_is_con(s2020, &n_syntax_46_Name) && u_is_con(u_field(s2020, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s2020, 0), 0);
    u_value v_b = u_field(s2020, 1);
    static struct u_object l_const_32_struct_32_u_n_2022 = U_LIT("const struct u_name");
    static struct u_object l_n__2023 = U_LIT("n_");
    t2021 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2022), u_concat(&l_n__2023, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t2021) && (u_is_con(s2020, &n_syntax_46_Name) && u_is_con(u_field(s2020, 0), &n_option_46_None))) {
    u_value v_b = u_field(s2020, 1);
    static struct u_object l_const_32_struct_32_u_n_2024 = U_LIT("const struct u_name");
    static struct u_object l_n__2025 = U_LIT("n_");
    static struct u_object l_NULL_2026 = U_LIT("NULL");
    t2021 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2024), u_concat(&l_n__2025, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2026), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t2021) {
    u_no_case(s2020);
  }
  return t2021;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s2027 = v_cs;
  u_value t2028 = NULL;
  if ((!t2028) && u_is_con(s2027, &n_list_46_Nil)) {
    t2028 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2028) && (u_is_con(s2027, &n_list_46_Cons) && u_is_con(u_field(s2027, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s2027, 0), 2);
    u_value v_rest = u_field(s2027, 1);
    u_value t2029 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2030 = &t2029;
    u_value l2031 = v_ks;
    while (u_is_con(l2031, &n_list_46_Cons)) {
      u_value v_k = u_field(l2031, 0);
      if (u_truth(u_mk_bool(1))) {
        e2030 = u_append(e2030, f_syntax_46_Constr_46_name_1(v_k));
      }
      l2031 = u_field(l2031, 1);
    }
    t2028 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t2029, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t2028) && u_is_con(s2027, &n_list_46_Cons)) {
    u_value v_rest = u_field(s2027, 1);
    t2028 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t2028) {
    u_no_case(s2027);
  }
  return t2028;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s2032 = v_ks;
  u_value t2033 = NULL;
  if ((!t2033) && u_is_con(s2032, &n_list_46_Nil)) {
    t2033 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2033) && u_is_con(s2032, &n_list_46_Cons)) {
    u_value v_k = u_field(s2032, 0);
    u_value v_rest = u_field(s2032, 1);
    u_value t2034;
    if (u_truth(f_set_46_member_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t2034 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t2034 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t2033 = t2034;
  }
  if (!t2033) {
    u_no_case(s2032);
  }
  return t2033;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t2035 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2036 = &t2035;
  u_value l2037 = v_xs;
  while (u_is_con(l2037, &n_list_46_Cons)) {
    u_value v_x = u_field(l2037, 0);
    if (u_truth(u_mk_bool(1))) {
      e2036 = u_append(e2036, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l2037 = u_field(l2037, 1);
  }
  return t2035;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__2038 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__2038, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s2039 = f_list_46_lookup_2(v_x, v_env);
  u_value t2040 = NULL;
  if ((!t2040) && u_is_con(s2039, &n_option_46_Some)) {
    u_value v_c = u_field(s2039, 0);
    t2040 = v_c;
  }
  if (!t2040) {
    t2040 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t2040) {
    u_no_case(s2039);
  }
  return t2040;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s2041 = v_c;
  u_value t2042 = NULL;
  if ((!t2042) && (u_is_con(s2041, &n_c_46_CeUnary) && u_equals(u_field(s2041, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s2041, 1);
    t2042 = v_p;
  }
  if (!t2042) {
    static struct u_object l__38__2043 = U_LIT("&");
    t2042 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2043, v_c);
  }
  if (!t2042) {
    u_no_case(s2041);
  }
  return t2042;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s2044 = v_t;
  u_value t2045 = NULL;
  if ((!t2045) && (u_is_con(s2044, &n_syntax_46_TSort) && u_is_con(u_field(s2044, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s2044, 0);
    t2045 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t2045) && u_is_con(s2044, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s2044, 0);
    u_value v_args = u_field(s2044, 1);
    static struct u_object l__91__2046 = U_LIT("[");
    static struct u_object l__44__32__2047 = U_LIT(", ");
    u_value t2048 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2049 = &t2048;
    u_value l2050 = v_args;
    while (u_is_con(l2050, &n_list_46_Cons)) {
      u_value v_a = u_field(l2050, 0);
      if (u_truth(u_mk_bool(1))) {
        e2049 = u_append(e2049, f_compile_46_type_key_1(v_a));
      }
      l2050 = u_field(l2050, 1);
    }
    static struct u_object l__93__2051 = U_LIT("]");
    t2045 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__2046, u_concat(u_str_join(&l__44__32__2047, t2048), &l__93__2051)));
  }
  if ((!t2045) && u_is_con(s2044, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s2044, 0);
    u_value v_r = u_field(s2044, 1);
    static struct u_object l__40__2052 = U_LIT("(");
    static struct u_object l__44__32__2053 = U_LIT(", ");
    u_value t2054 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2055 = &t2054;
    u_value l2056 = v_args;
    while (u_is_con(l2056, &n_list_46_Cons)) {
      u_value v_a = u_field(l2056, 0);
      if (u_truth(u_mk_bool(1))) {
        e2055 = u_append(e2055, f_compile_46_type_key_1(v_a));
      }
      l2056 = u_field(l2056, 1);
    }
    static struct u_object l__41__32__45__62__32__2057 = U_LIT(") -> ");
    t2045 = u_concat(&l__40__2052, u_concat(u_str_join(&l__44__32__2053, t2054), u_concat(&l__41__32__45__62__32__2057, f_compile_46_type_key_1(v_r))));
  }
  if ((!t2045) && u_is_con(s2044, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s2044, 0);
    u_value v_os = u_field(s2044, 1);
    u_value v_rs = u_field(s2044, 2);
    static struct u_object l__40_in_32__2058 = U_LIT("(in ");
    static struct u_object l__44__32__2059 = U_LIT(", ");
    u_value t2060 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2061 = &t2060;
    u_value l2062 = v_is;
    while (u_is_con(l2062, &n_list_46_Cons)) {
      u_value v_a = u_field(l2062, 0);
      if (u_truth(u_mk_bool(1))) {
        e2061 = u_append(e2061, f_compile_46_type_key_1(v_a));
      }
      l2062 = u_field(l2062, 1);
    }
    static struct u_object l__44__32_out_32__2063 = U_LIT(", out ");
    static struct u_object l__44__32__2064 = U_LIT(", ");
    u_value t2065 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2066 = &t2065;
    u_value l2067 = v_os;
    while (u_is_con(l2067, &n_list_46_Cons)) {
      u_value v_a = u_field(l2067, 0);
      if (u_truth(u_mk_bool(1))) {
        e2066 = u_append(e2066, f_compile_46_type_key_1(v_a));
      }
      l2067 = u_field(l2067, 1);
    }
    static struct u_object l__44__32_ref_32__2068 = U_LIT(", ref ");
    static struct u_object l__44__32__2069 = U_LIT(", ");
    u_value t2070 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2071 = &t2070;
    u_value l2072 = v_rs;
    while (u_is_con(l2072, &n_list_46_Cons)) {
      u_value v_a = u_field(l2072, 0);
      if (u_truth(u_mk_bool(1))) {
        e2071 = u_append(e2071, f_compile_46_type_key_1(v_a));
      }
      l2072 = u_field(l2072, 1);
    }
    static struct u_object l__41__2073 = U_LIT(")");
    t2045 = u_concat(&l__40_in_32__2058, u_concat(u_str_join(&l__44__32__2059, t2060), u_concat(&l__44__32_out_32__2063, u_concat(u_str_join(&l__44__32__2064, t2065), u_concat(&l__44__32_ref_32__2068, u_concat(u_str_join(&l__44__32__2069, t2070), &l__41__2073))))));
  }
  if ((!t2045) && u_is_con(s2044, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s2044, 0);
    static struct u_object l__40__2074 = U_LIT("(");
    static struct u_object l__32__42__32__2075 = U_LIT(" * ");
    u_value t2076 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2077 = &t2076;
    u_value l2078 = v_ts;
    while (u_is_con(l2078, &n_list_46_Cons)) {
      u_value v_a = u_field(l2078, 0);
      if (u_truth(u_mk_bool(1))) {
        e2077 = u_append(e2077, f_compile_46_type_key_1(v_a));
      }
      l2078 = u_field(l2078, 1);
    }
    static struct u_object l__41__2079 = U_LIT(")");
    t2045 = u_concat(&l__40__2074, u_concat(u_str_join(&l__32__42__32__2075, t2076), &l__41__2079));
  }
  if (!t2045) {
    static struct u_object l__63__2080 = U_LIT("?");
    t2045 = &l__63__2080;
  }
  if (!t2045) {
    u_no_case(s2044);
  }
  return t2045;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t2081 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2082 = &t2081;
  u_value l2083 = v_cs;
  while (u_is_con(l2083, &n_list_46_Cons)) {
    u_value v_c = u_field(l2083, 0);
    if (u_truth(u_mk_bool(1))) {
      e2082 = u_append(e2082, f_compile_46_fun_arities_1(v_c));
    }
    l2083 = u_field(l2083, 1);
  }
  u_value t2084 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2085 = &t2084;
  u_value l2086 = v_cs;
  while (u_is_con(l2086, &n_list_46_Cons)) {
    u_value v_c = u_field(l2086, 0);
    if (u_truth(u_mk_bool(1))) {
      e2085 = u_append(e2085, f_compile_46_constr_arities_1(v_c));
    }
    l2086 = u_field(l2086, 1);
  }
  u_value t2091 = f_list_46_concat_1(t2081);
  u_value t2092 = f_list_46_concat_1(t2084);
  u_value t2090;
  {
    u_value v_fs = t2091;
    u_value v_ks = t2092;
    u_value t2087 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2088 = &t2087;
    u_value l2089 = v_cs;
    while (u_is_con(l2089, &n_list_46_Cons)) {
      u_value v_c = u_field(l2089, 0);
      if (u_truth(u_mk_bool(1))) {
        e2088 = u_append(e2088, f_compile_46_proc_arities_1(v_c));
      }
      l2089 = u_field(l2089, 1);
    }
    t2090 = u_mk_con(&n_compile_46_Globals, 5, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()), f_map_46_put_all_2(f_list_46_concat_1(t2087), f_map_46_empty_0()));
  }
  return t2090;
}

u_value f_compile_46_proc_arities_1(u_value v_c) {
  u_value s2093 = v_c;
  u_value t2094 = NULL;
  if ((!t2094) && u_is_con(s2093, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s2093, 0);
    u_value v_ins = u_field(s2093, 2);
    u_value v_outs = u_field(s2093, 3);
    u_value v_refs = u_field(s2093, 4);
    t2094 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, f_list_46_length_1(v_ins), u_add(f_list_46_length_1(v_outs), f_list_46_length_1(v_refs)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2094) {
    t2094 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2094) {
    u_no_case(s2093);
  }
  return t2094;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s2095 = v_fs;
  u_value t2096 = NULL;
  if ((!t2096) && u_is_con(s2095, &n_list_46_Nil)) {
    t2096 = v_m;
  }
  if ((!t2096) && u_is_con(s2095, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s2095, 0), 0);
    u_value v_k = u_field(u_field(s2095, 0), 1);
    u_value v_rest = u_field(s2095, 1);
    u_value s2097 = f_map_46_get_2(v_f, v_m);
    u_value t2098 = NULL;
    if ((!t2098) && u_is_con(s2097, &n_option_46_Some)) {
      u_value v_ks = u_field(s2097, 0);
      t2098 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t2098) && u_is_con(s2097, &n_option_46_None)) {
      t2098 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t2098) {
      u_no_case(s2097);
    }
    t2096 = t2098;
  }
  if (!t2096) {
    u_no_case(s2095);
  }
  return t2096;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s2099 = v_gl;
  u_value t2100 = NULL;
  if ((!t2100) && u_is_con(s2099, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s2099, 2);
    u_value s2101 = f_map_46_get_2(v_n, v_fm);
    u_value t2102 = NULL;
    if ((!t2102) && u_is_con(s2101, &n_option_46_Some)) {
      u_value v_ks = u_field(s2101, 0);
      t2102 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t2102) && u_is_con(s2101, &n_option_46_None)) {
      t2102 = u_mk_bool(0);
    }
    if (!t2102) {
      u_no_case(s2101);
    }
    t2100 = t2102;
  }
  if (!t2100) {
    u_no_case(s2099);
  }
  return t2100;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s2103 = v_gl;
  u_value t2104 = NULL;
  if ((!t2104) && u_is_con(s2103, &n_compile_46_Globals)) {
    u_value v_km = u_field(s2103, 3);
    t2104 = f_map_46_get_2(v_n, v_km);
  }
  if (!t2104) {
    u_no_case(s2103);
  }
  return t2104;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s2105 = v_c;
  u_value t2106 = NULL;
  if ((!t2106) && u_is_con(s2105, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s2105, 0);
    u_value v_xss = u_field(s2105, 2);
    t2106 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2106) {
    t2106 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2106) {
    u_no_case(s2105);
  }
  return t2106;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s2107 = v_c;
  u_value t2108 = NULL;
  if ((!t2108) && u_is_con(s2107, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s2107, 2);
    u_value t2109 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2110 = &t2109;
    u_value l2111 = v_ks;
    while (u_is_con(l2111, &n_list_46_Cons)) {
      u_value v_k = u_field(l2111, 0);
      if (u_truth(u_mk_bool(1))) {
        e2110 = u_append(e2110, f_compile_46_constr_arity_1(v_k));
      }
      l2111 = u_field(l2111, 1);
    }
    t2108 = t2109;
  }
  if (!t2108) {
    t2108 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2108) {
    u_no_case(s2107);
  }
  return t2108;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s2112 = v_k;
  u_value t2113 = NULL;
  if ((!t2113) && u_is_con(s2112, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s2112, 0);
    u_value v_ts = u_field(s2112, 1);
    t2113 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t2113) {
    u_no_case(s2112);
  }
  return t2113;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s2114 = v_xs;
  u_value t2115 = NULL;
  if ((!t2115) && u_is_con(s2114, &n_list_46_Nil)) {
    t2115 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2115) && u_is_con(s2114, &n_list_46_Cons)) {
    u_value v_x = u_field(s2114, 0);
    u_value v_rest = u_field(s2114, 1);
    t2115 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t2115) {
    u_no_case(s2114);
  }
  return t2115;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_2116 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_2116, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_2117 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_2117, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s2118 = v_cs;
  u_value t2119 = NULL;
  if ((!t2119) && u_is_con(s2118, &n_list_46_Nil)) {
    t2119 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t2119) && (u_is_con(s2118, &n_list_46_Cons) && u_is_con(u_field(s2118, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s2118, 0);
    t2119 = v_d;
  }
  if ((!t2119) && u_is_con(s2118, &n_list_46_Cons)) {
    u_value v_d = u_field(s2118, 0);
    u_value v_rest = u_field(s2118, 1);
    static struct u_object l__38__38__2120 = U_LIT("&&");
    t2119 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__2120, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t2119) {
    u_no_case(s2118);
  }
  return t2119;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s2121 = v_l;
  u_value t2122 = NULL;
  if ((!t2122) && u_is_con(s2121, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_2123 = U_LIT("u_mk_tuple");
    t2122 = f_c_46_c_call_2(&l_u_mk_tuple_2123, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2122) && u_is_con(s2121, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s2121, 0);
    static struct u_object l_u_mk_bool_2124 = U_LIT("u_mk_bool");
    u_value t2125;
    if (u_truth(v_b)) {
      t2125 = u_mk_int(1);
    } else {
      t2125 = u_mk_int(0);
    }
    t2122 = f_c_46_c_call_2(&l_u_mk_bool_2124, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t2125), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2122) && u_is_con(s2121, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s2121, 0);
    u_value t2126 = u_lt(u_mk_int_str("-1000000000000000000"), v_n);
    if (u_truth(t2126)) {
      t2126 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t2129;
    if (u_truth(t2126)) {
      static struct u_object l_u_mk_int_2127 = U_LIT("u_mk_int");
      t2129 = f_c_46_c_call_2(&l_u_mk_int_2127, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_2128 = U_LIT("u_mk_int_str");
      t2129 = f_c_46_c_call_2(&l_u_mk_int_str_2128, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t2122 = t2129;
  }
  if ((!t2122) && u_is_con(s2121, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s2121, 0);
    static struct u_object l_u_mk_lit_2130 = U_LIT("u_mk_lit");
    t2122 = f_c_46_c_call_2(&l_u_mk_lit_2130, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2122) && u_is_con(s2121, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s2121, 0);
    static struct u_object l_u_mk_char_2131 = U_LIT("u_mk_char");
    t2122 = f_c_46_c_call_2(&l_u_mk_char_2131, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2122) {
    u_no_case(s2121);
  }
  return t2122;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s2132 = v_p;
  u_value t2133 = NULL;
  if ((!t2133) && u_is_con(s2132, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s2132, 0);
    static struct u_object l_u_equals_2134 = U_LIT("u_equals");
    t2133 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_2134, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2133) && u_is_con(s2132, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s2132, 0);
    u_value v_ps = u_field(s2132, 1);
    static struct u_object l_u_is_con_2135 = U_LIT("u_is_con");
    t2133 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_2135, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t2133) && u_is_con(s2132, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2132, 0);
    t2133 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2133) {
    t2133 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2133) {
    u_no_case(s2132);
  }
  return t2133;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s2136 = v_ps;
  u_value t2137 = NULL;
  if ((!t2137) && u_is_con(s2136, &n_list_46_Nil)) {
    t2137 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2137) && u_is_con(s2136, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2136, 0), 0);
    u_value v_i = u_field(u_field(s2136, 0), 1);
    u_value v_rest = u_field(s2136, 1);
    t2137 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t2137) {
    u_no_case(s2136);
  }
  return t2137;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s2138 = v_p;
  u_value t2139 = NULL;
  if ((!t2139) && (u_is_con(s2138, &n_syntax_46_PVar) && u_is_con(u_field(s2138, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2138, 0), 0);
    t2139 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2139) && u_is_con(s2138, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2138, 1);
    t2139 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t2139) && u_is_con(s2138, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2138, 0);
    t2139 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2139) {
    t2139 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2139) {
    u_no_case(s2138);
  }
  return t2139;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s2140 = v_ps;
  u_value t2141 = NULL;
  if ((!t2141) && u_is_con(s2140, &n_list_46_Nil)) {
    t2141 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2141) && u_is_con(s2140, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2140, 0), 0);
    u_value v_i = u_field(u_field(s2140, 0), 1);
    u_value v_rest = u_field(s2140, 1);
    t2141 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t2141) {
    u_no_case(s2140);
  }
  return t2141;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s2142 = v_p;
  u_value t2143 = NULL;
  if ((!t2143) && (u_is_con(s2142, &n_syntax_46_PVar) && u_is_con(u_field(s2142, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2142, 0), 0);
    t2143 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2143) && u_is_con(s2142, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2142, 1);
    u_value t2144 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2145 = &t2144;
    u_value l2146 = v_ps;
    while (u_is_con(l2146, &n_list_46_Cons)) {
      u_value v_q = u_field(l2146, 0);
      if (u_truth(u_mk_bool(1))) {
        e2145 = u_append(e2145, f_compile_46_pattern_names_1(v_q));
      }
      l2146 = u_field(l2146, 1);
    }
    t2143 = f_list_46_concat_1(t2144);
  }
  if ((!t2143) && u_is_con(s2142, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2142, 0);
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
    t2143 = f_list_46_concat_1(t2147);
  }
  if (!t2143) {
    t2143 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2143) {
    u_no_case(s2142);
  }
  return t2143;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s2150 = v_e;
  u_value t2151 = NULL;
  if ((!t2151) && u_is_con(s2150, &n_syntax_46_ELit)) {
    t2151 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2151) && (u_is_con(s2150, &n_syntax_46_EVar) && u_is_con(u_field(s2150, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2150, 0), 0);
    u_value t2152;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t2152 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t2152 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t2151 = t2152;
  }
  if ((!t2151) && u_is_con(s2150, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s2150, 0);
    u_value v_args = u_field(s2150, 1);
    u_value t2153 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2154 = &t2153;
    u_value l2155 = v_args;
    while (u_is_con(l2155, &n_list_46_Cons)) {
      u_value v_a = u_field(l2155, 0);
      if (u_truth(u_mk_bool(1))) {
        e2154 = u_append(e2154, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2155 = u_field(l2155, 1);
    }
    t2151 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t2153));
  }
  if ((!t2151) && u_is_con(s2150, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s2150, 0);
    u_value v_b = u_field(s2150, 1);
    t2151 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t2151) && u_is_con(s2150, &n_syntax_46_EProc)) {
    u_value v_is = u_field(s2150, 0);
    u_value v_os = u_field(s2150, 1);
    u_value v_rs = u_field(s2150, 2);
    u_value v_b = u_field(s2150, 3);
    t2151 = f_compile_46_free_in_stmt_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), v_bound));
  }
  if ((!t2151) && u_is_con(s2150, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s2150, 0);
    u_value v_b = u_field(s2150, 1);
    u_value t2156 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2157 = &t2156;
    u_value l2158 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l2158, &n_list_46_Cons)) {
      u_value v_a = u_field(l2158, 0);
      if (u_truth(u_mk_bool(1))) {
        e2157 = u_append(e2157, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2158 = u_field(l2158, 1);
    }
    t2151 = f_list_46__43__43__2(f_list_46_concat_1(t2156), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t2151) && u_is_con(s2150, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s2150, 0);
    u_value v_l = u_field(s2150, 1);
    u_value v_r = u_field(s2150, 2);
    t2151 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t2151) && u_is_con(s2150, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s2150, 0);
    u_value v_cs = u_field(s2150, 1);
    u_value t2159 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2160 = &t2159;
    u_value l2161 = v_cs;
    while (u_is_con(l2161, &n_list_46_Cons)) {
      u_value v_b = u_field(l2161, 0);
      if (u_truth(u_mk_bool(1))) {
        e2160 = u_append(e2160, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l2161 = u_field(l2161, 1);
    }
    t2151 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2159));
  }
  if ((!t2151) && u_is_con(s2150, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s2150, 0);
    u_value v_xs = u_field(s2150, 1);
    u_value v_ls = u_field(s2150, 2);
    u_value v_c = u_field(s2150, 3);
    u_value t2162 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2163 = &t2162;
    u_value l2164 = v_ls;
    while (u_is_con(l2164, &n_list_46_Cons)) {
      u_value v_l = u_field(l2164, 0);
      if (u_truth(u_mk_bool(1))) {
        e2163 = u_append(e2163, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2164 = u_field(l2164, 1);
    }
    t2151 = f_list_46__43__43__2(f_list_46_concat_1(t2162), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t2151) && u_is_con(s2150, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s2150, 0);
    u_value t2165 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2166 = &t2165;
    u_value l2167 = v_es;
    while (u_is_con(l2167, &n_list_46_Cons)) {
      u_value v_a = u_field(l2167, 0);
      if (u_truth(u_mk_bool(1))) {
        e2166 = u_append(e2166, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2167 = u_field(l2167, 1);
    }
    t2151 = f_list_46_concat_1(t2165);
  }
  if ((!t2151) && u_is_con(s2150, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s2150, 0);
    t2151 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if (!t2151) {
    u_no_case(s2150);
  }
  return t2151;
}

u_value f_compile_46_free_in_stmt_2(u_value v_x, u_value v_bound) {
  u_value s2168 = v_x;
  u_value t2169 = NULL;
  if ((!t2169) && u_is_con(s2168, &n_syntax_46_SSkip)) {
    t2169 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2169) && u_is_con(s2168, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s2168, 0);
    u_value t2170 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2171 = &t2170;
    u_value l2172 = f_syntax_46_names_1(f_list_46_firsts_1(v_ps));
    while (u_is_con(l2172, &n_list_46_Cons)) {
      u_value v_y = u_field(l2172, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2171 = u_append(e2171, v_y);
      }
      l2172 = u_field(l2172, 1);
    }
    u_value t2173 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2174 = &t2173;
    u_value l2175 = f_list_46_seconds_1(v_ps);
    while (u_is_con(l2175, &n_list_46_Cons)) {
      u_value v_a = u_field(l2175, 0);
      if (u_truth(u_mk_bool(1))) {
        e2174 = u_append(e2174, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2175 = u_field(l2175, 1);
    }
    t2169 = f_list_46__43__43__2(t2170, f_list_46_concat_1(t2173));
  }
  if ((!t2169) && u_is_con(s2168, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s2168, 0);
    u_value v_ys = u_field(s2168, 1);
    u_value t2176 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2177 = &t2176;
    u_value l2178 = v_ys;
    while (u_is_con(l2178, &n_list_46_Cons)) {
      u_value v_y = u_field(l2178, 0);
      if (u_truth(u_mk_bool(1))) {
        e2177 = u_append(e2177, f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2178 = u_field(l2178, 1);
    }
    t2169 = f_list_46_concat_1(t2176);
  }
  if ((!t2169) && u_is_con(s2168, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s2168, 0);
    u_value v_l = u_field(s2168, 1);
    u_value v_r = u_field(s2168, 2);
    t2169 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_l, v_bound), f_compile_46_free_in_stmt_2(v_r, v_bound)));
  }
  if ((!t2169) && u_is_con(s2168, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s2168, 0);
    u_value v_b = u_field(s2168, 1);
    t2169 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_compile_46_free_in_stmt_2(v_b, v_bound));
  }
  if ((!t2169) && u_is_con(s2168, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s2168, 0);
    u_value v_ins = u_field(s2168, 1);
    u_value v_outs = u_field(s2168, 2);
    u_value v_refs = u_field(s2168, 3);
    u_value t2179 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2180 = &t2179;
    u_value l2181 = u_mk_con(&n_list_46_Cons, 2, v_p, f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
    while (u_is_con(l2181, &n_list_46_Cons)) {
      u_value v_y = u_field(l2181, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2180 = u_append(e2180, v_y);
      }
      l2181 = u_field(l2181, 1);
    }
    u_value t2182 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2183 = &t2182;
    u_value l2184 = v_ins;
    while (u_is_con(l2184, &n_list_46_Cons)) {
      u_value v_a = u_field(l2184, 0);
      if (u_truth(u_mk_bool(1))) {
        e2183 = u_append(e2183, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2184 = u_field(l2184, 1);
    }
    t2169 = f_list_46__43__43__2(t2179, f_list_46_concat_1(t2182));
  }
  if ((!t2169) && u_is_con(s2168, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s2168, 0);
    u_value v_bs = u_field(s2168, 1);
    u_value t2185 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2186 = &t2185;
    u_value l2187 = v_bs;
    while (u_is_con(l2187, &n_list_46_Cons)) {
      u_value v_b = u_field(l2187, 0);
      if (u_truth(u_mk_bool(1))) {
        e2186 = u_append(e2186, f_compile_46_free_in_stmt_branch_2(v_b, v_bound));
      }
      l2187 = u_field(l2187, 1);
    }
    t2169 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2185));
  }
  if ((!t2169) && u_is_con(s2168, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s2168, 0);
    t2169 = f_compile_46_free_vars_2(v_c, v_bound);
  }
  if ((!t2169) && u_is_con(s2168, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s2168, 0);
    t2169 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if ((!t2169) && u_is_con(s2168, &n_syntax_46_STry)) {
    u_value v_b = u_field(s2168, 0);
    u_value v_hs = u_field(s2168, 1);
    u_value t2188 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2189 = &t2188;
    u_value l2190 = v_hs;
    while (u_is_con(l2190, &n_list_46_Cons)) {
      u_value v_h = u_field(l2190, 0);
      if (u_truth(u_mk_bool(1))) {
        e2189 = u_append(e2189, f_compile_46_free_in_handler_2(v_h, v_bound));
      }
      l2190 = u_field(l2190, 1);
    }
    t2169 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_b, v_bound), f_list_46_concat_1(t2188));
  }
  if (!t2169) {
    u_no_case(s2168);
  }
  return t2169;
}

u_value f_compile_46_free_in_stmt_branch_2(u_value v_b, u_value v_bound) {
  u_value s2191 = v_b;
  u_value t2192 = NULL;
  if ((!t2192) && (u_is_con(s2191, &n_syntax_46_Branch) && u_is_con(u_field(s2191, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2191, 0);
    u_value v_g = u_field(u_field(s2191, 1), 0);
    u_value v_y = u_field(s2191, 2);
    t2192 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2192) && (u_is_con(s2191, &n_syntax_46_Branch) && u_is_con(u_field(s2191, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2191, 0);
    u_value v_y = u_field(s2191, 2);
    t2192 = f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2192) {
    u_no_case(s2191);
  }
  return t2192;
}

u_value f_compile_46_free_in_handler_2(u_value v_h, u_value v_bound) {
  u_value t2196 = f_list_46__43__43__2(f_compile_46_pattern_names_1(f_syntax_46_Handler_46_pat_1(v_h)), v_bound);
  u_value t2195;
  {
    u_value v_inner = t2196;
    u_value s2193 = f_syntax_46_Handler_46_guard_1(v_h);
    u_value t2194 = NULL;
    if ((!t2194) && u_is_con(s2193, &n_option_46_Some)) {
      u_value v_g = u_field(s2193, 0);
      t2194 = f_compile_46_free_vars_2(v_g, v_inner);
    }
    if ((!t2194) && u_is_con(s2193, &n_option_46_None)) {
      t2194 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t2194) {
      u_no_case(s2193);
    }
    t2195 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(f_syntax_46_Handler_46_body_1(v_h), v_inner), t2194);
  }
  return t2195;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s2197 = v_b;
  u_value t2198 = NULL;
  if ((!t2198) && (u_is_con(s2197, &n_syntax_46_Branch) && u_is_con(u_field(s2197, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2197, 0);
    u_value v_g = u_field(u_field(s2197, 1), 0);
    u_value v_e = u_field(s2197, 2);
    t2198 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2198) && (u_is_con(s2197, &n_syntax_46_Branch) && u_is_con(u_field(s2197, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2197, 0);
    u_value v_e = u_field(s2197, 2);
    t2198 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2198) {
    u_no_case(s2197);
  }
  return t2198;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s2199 = v_xss;
  u_value t2200 = NULL;
  if ((!t2200) && u_is_con(s2199, &n_list_46_Nil)) {
    t2200 = v_b;
  }
  if ((!t2200) && u_is_con(s2199, &n_list_46_Cons)) {
    u_value v_xs = u_field(s2199, 0);
    u_value v_rest = u_field(s2199, 1);
    t2200 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t2200) {
    u_no_case(s2199);
  }
  return t2200;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value t2201 = u_concat(v_stem, u_str_show(f_compile_46_Gen_46_count_1(*r_g)));
    u_value t2202 = f_compile_46_Gen_46_count_58__61__2(*r_g, u_add(f_compile_46_Gen_46_count_1(*r_g), u_mk_int(1)));
    *r_x = t2201;
    *r_g = t2202;
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value t2203 = f_compile_46_Gen_46_lifted_58__61__2(*r_g, f_list_46__43__43__2(f_compile_46_Gen_46_lifted_1(*r_g), u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
    *r_g = t2203;
  }
}

u_value f_compile_46_global_value_3(u_value v_gl, u_value v_n, u_value v_t) {
  u_value t2218;
  if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
    static struct u_object l_u_mk_closure_2204 = U_LIT("u_mk_closure");
    static struct u_object l_w__2205 = U_LIT("w_");
    t2218 = f_c_46_c_call_2(&l_u_mk_closure_2204, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__2205, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
  } else {
    u_value s2206 = f_compile_46_fields_count_2(v_gl, v_n);
    u_value t2207 = NULL;
    if ((!t2207) && (u_is_con(s2206, &n_option_46_Some) && u_equals(u_field(s2206, 0), u_mk_int(0)))) {
      static struct u_object l_u_mk_con_2208 = U_LIT("u_mk_con");
      t2207 = f_c_46_c_call_2(&l_u_mk_con_2208, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
    }
    if ((!t2207) && u_is_con(s2206, &n_option_46_Some)) {
      static struct u_object l_u_mk_closure_2209 = U_LIT("u_mk_closure");
      static struct u_object l_wc__2210 = U_LIT("wc_");
      t2207 = f_c_46_c_call_2(&l_u_mk_closure_2209, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__2210, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
    }
    if ((!t2207) && u_is_con(s2206, &n_option_46_None)) {
      u_value s2211 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
      u_value t2212 = NULL;
      if ((!t2212) && u_is_con(s2211, &n_option_46_Some)) {
        u_value v_f = u_field(u_field(s2211, 0), 0);
        static struct u_object l_u_mk_closure_2213 = U_LIT("u_mk_closure");
        static struct u_object l_w_2214 = U_LIT("w");
        t2212 = f_c_46_c_call_2(&l_u_mk_closure_2213, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_2214, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
      }
      if ((!t2212) && u_is_con(s2211, &n_option_46_None)) {
        if (u_truth(f_map_46_has_2(v_n, f_compile_46_Globals_46_procs_1(v_gl)))) {
          static struct u_object l_u_mk_proc_2215 = U_LIT("u_mk_proc");
          static struct u_object l_wp__2216 = U_LIT("wp_");
          t2212 = f_c_46_c_call_2(&l_u_mk_proc_2215, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wp__2216, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        }
      }
      if ((!t2212) && u_is_con(s2211, &n_option_46_None)) {
        static struct u_object l_the_32_variable_32__2217 = U_LIT("the variable ");
        u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__2217, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
        t2212 = NULL;
      }
      if (!t2212) {
        u_no_case(s2211);
      }
      t2207 = t2212;
    }
    if (!t2207) {
      u_no_case(s2206);
    }
    t2218 = t2207;
  }
  return t2218;
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
    u_value s2219 = v_e;
    int d2220 = 0;
    if ((!d2220) && (u_is_con(s2219, &n_syntax_46_ELit) && u_is_con(u_field(s2219, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s2219, 0), 0);
      d2220 = 1;
      {
        static struct u_object l_l__2221 = U_LIT("l_");
        u_value t2222;
        if (u_truth(u_lt(u_str_size(v_s), u_mk_int(16)))) {
          t2222 = u_str_size(v_s);
        } else {
          t2222 = u_mk_int(16);
        }
        static struct u_object l___2223 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__2221, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), t2222))), &l___2223)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__2224 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_2225 = U_LIT("U_LIT");
        static struct u_object l__38__2226 = U_LIT("&");
        u_value t2227 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__2224), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_2225, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t2228 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2226, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t2227;
        *r_r = t2228;
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s2219, 0);
      d2220 = 1;
      {
        u_value t2229 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2230 = f_compile_46_literal_value_1(v_l);
        *r_ss = t2229;
        *r_r = t2230;
      }
    }
    if ((!d2220) && (u_is_con(s2219, &n_syntax_46_EVar) && u_is_con(u_field(s2219, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s2219, 0), 0);
      u_value v_ty = u_field(u_field(s2219, 0), 1);
      d2220 = 1;
      {
        u_value s2231 = f_list_46_lookup_2(v_n, v_env);
        int d2232 = 0;
        if ((!d2232) && u_is_con(s2231, &n_option_46_Some)) {
          u_value v_c = u_field(s2231, 0);
          d2232 = 1;
          {
            u_value t2233 = u_mk_con(&n_list_46_Nil, 0);
            u_value t2234 = v_c;
            *r_ss = t2233;
            *r_r = t2234;
          }
        }
        if ((!d2232) && u_is_con(s2231, &n_option_46_None)) {
          d2232 = 1;
          {
            u_value t2235 = u_mk_con(&n_list_46_Nil, 0);
            u_value t2236 = f_compile_46_global_value_3(v_gl, v_n, v_ty);
            *r_ss = t2235;
            *r_r = t2236;
          }
        }
        if (!d2232) {
          u_no_case(s2231);
        }
      }
    }
    if ((!d2220) && (u_is_con(s2219, &n_syntax_46_EApp) && (u_is_con(u_field(s2219, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s2219, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s2219, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s2219, 0), 0), 1);
      u_value v_args = u_field(s2219, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d2220 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s2219, 0);
      u_value v_args = u_field(s2219, 1);
      d2220 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_2237 = U_LIT("u_apply");
        u_value t2238 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2239 = f_c_46_c_call_2(&l_u_apply_2237, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t2238;
        *r_r = t2239;
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s2219, 0);
      u_value v_b = u_field(s2219, 1);
      d2220 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_EProc)) {
      u_value v_is = u_field(s2219, 0);
      u_value v_os = u_field(s2219, 1);
      u_value v_rs = u_field(s2219, 2);
      u_value v_b = u_field(s2219, 3);
      d2220 = 1;
      {
        p_compile_46_compile_proc_lambda(v_gl, v_env, v_is, f_list_46__43__43__2(v_os, v_rs), v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s2219, 0);
      u_value v_b = u_field(s2219, 1);
      d2220 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2240 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2240, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2241 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t2241;
        u_value t2242 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2242;
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s2219, 0);
      u_value v_l = u_field(s2219, 1);
      u_value v_rr = u_field(s2219, 2);
      d2220 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2243 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2243, &v_t, r_g);
        u_value t2244 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2244;
        u_value t2245 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2245;
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s2219, 0);
      u_value v_cases = u_field(s2219, 1);
      d2220 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2246 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2246, &v_d, r_g);
        static struct u_object l_t_2247 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2247, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_2248 = U_LIT("NULL");
        u_value t2249 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2248))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2249;
        static struct u_object l__33__2250 = U_LIT("!");
        static struct u_object l_u_no_case_2251 = U_LIT("u_no_case");
        u_value t2252 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2250, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2251, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2252;
        u_value t2253 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2253;
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s2219, 0);
      u_value v_xs = u_field(s2219, 1);
      u_value v_lists = u_field(s2219, 2);
      u_value v_c = u_field(s2219, 3);
      d2220 = 1;
      {
        static struct u_object l_t_2254 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2254, &v_t, r_g);
        static struct u_object l_e_2255 = U_LIT("e");
        p_compile_46_gen_name(&l_e_2255, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_2256 = U_LIT("u_mk_con");
        static struct u_object l__38__2257 = U_LIT("&");
        u_value t2258 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_2256, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__2257, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t2258;
        u_value t2259 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2259;
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s2219, 0);
      d2220 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_2260 = U_LIT("u_mk_tuple");
        u_value t2261 = f_c_46_c_call_2(&l_u_mk_tuple_2260, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t2261;
      }
    }
    if ((!d2220) && u_is_con(s2219, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s2219, 0);
      u_value v_ty = u_field(s2219, 1);
      d2220 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2262 = U_LIT("u_raise");
        static struct u_object l_NULL_2263 = U_LIT("NULL");
        u_value t2264 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2262, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        u_value t2265 = u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2263);
        *r_ss = t2264;
        *r_r = t2265;
      }
    }
    if (!d2220) {
      u_no_case(s2219);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s2266 = v_es;
    int d2267 = 0;
    if ((!d2267) && u_is_con(s2266, &n_list_46_Nil)) {
      d2267 = 1;
      {
        u_value t2268 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2269 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2268;
        *r_rs = t2269;
      }
    }
    if ((!d2267) && u_is_con(s2266, &n_list_46_Cons)) {
      u_value v_e = u_field(s2266, 0);
      u_value v_rest = u_field(s2266, 1);
      d2267 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t2270 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2271 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t2270;
        *r_rs = t2271;
      }
    }
    if (!d2267) {
      u_no_case(s2266);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s2272 = v_rs;
    int d2273 = 0;
    if ((!d2273) && u_is_con(s2272, &n_list_46_Nil)) {
      d2273 = 1;
      {
        u_value t2274 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2274;
      }
    }
    if ((!d2273) && u_is_con(s2272, &n_list_46_Cons)) {
      u_value v_r = u_field(s2272, 0);
      u_value v_rest = u_field(s2272, 1);
      d2273 = 1;
      {
        static struct u_object l_t_2275 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2275, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t2276 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t2276;
      }
    }
    if (!d2273) {
      u_no_case(s2272);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s2277 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2278 = NULL;
  if ((!t2278) && (u_is_con(u_field(s2277, 0), &n_list_46_Cons) && (u_is_con(u_field(s2277, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2277, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2277, 0), 0);
    u_value v_more = u_field(u_field(s2277, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2277, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2277, 1), 1);
    t2278 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t2278) {
    t2278 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2278) {
    u_no_case(s2277);
  }
  return t2278;
}

void p_compile_46_compile_named(u_value v_gl, u_value v_env, u_value v_n, u_value v_ty, u_value v_args, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value v_ra = NULL;
    u_value v_rb = NULL;
    u_value v_rs = NULL;
    u_value v_t = NULL;
    u_value s2279 = v_args;
    int d2280 = 0;
    if ((!d2280) && (u_is_con(s2279, &n_list_46_Cons) && (u_is_con(u_field(s2279, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2279, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s2279, 0);
      u_value v_b = u_field(u_field(s2279, 1), 0);
      static struct u_object l_and_2287 = U_LIT("and");
      u_value t2289 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2287));
      if (!u_truth(t2289)) {
        static struct u_object l_or_2288 = U_LIT("or");
        t2289 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_2288));
      }
      if (u_truth(t2289)) {
        d2280 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_2281 = U_LIT("t");
          p_compile_46_gen_name(&l_t_2281, &v_t, r_g);
          static struct u_object l_and_2282 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2282)))) {
            {
              u_value t2283 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2283;
            }
          } else {
            {
              static struct u_object l__33__2284 = U_LIT("!");
              u_value t2285 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2284, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2285;
            }
          }
          u_value t2286 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t2286;
        }
      }
    }
    if (!d2280) {
      d2280 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t2290 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t2290)) {
          t2290 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t2290)) {
          {
            u_value t2291 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t2291;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_2292 = U_LIT("u_mk_con");
              u_value t2293 = f_c_46_c_call_2(&l_u_mk_con_2292, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t2293;
            }
          } else {
            {
              u_value s2294 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
              int d2295 = 0;
              if ((!d2295) && u_is_con(s2294, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s2294, 0), 0);
                u_value v_k = u_field(u_field(s2294, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d2295 = 1;
                  {
                    u_value t2296 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t2296;
                  }
                }
              }
              if (!d2295) {
                d2295 = 1;
                {
                  static struct u_object l_u_apply_2297 = U_LIT("u_apply");
                  u_value t2298 = f_c_46_c_call_2(&l_u_apply_2297, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_global_value_3(v_gl, v_n, v_ty), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t2298;
                }
              }
              if (!d2295) {
                u_no_case(s2294);
              }
            }
          }
        }
      }
    }
    if (!d2280) {
      u_no_case(s2279);
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
    u_value t2299 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2300 = &t2299;
    u_value l2301 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l2301, &n_list_46_Cons)) {
      u_value v_x = u_field(l2301, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2300 = u_append(e2300, v_x);
      }
      l2301 = u_field(l2301, 1);
    }
    u_value t2302 = t2299;
    v_caps = t2302;
    static struct u_object l_lam_2303 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_2303, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_2304 = U_LIT("env");
    static struct u_object l_args_2305 = U_LIT("args");
    u_value t2306 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2304), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_2305));
    v_decls = t2306;
    static struct u_object l_env_2307 = U_LIT("env");
    static struct u_object l_args_2308 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2307), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2308), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_2309 = U_LIT("u_mk_closure");
    u_value t2310 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2311 = &t2310;
    u_value l2312 = v_caps;
    while (u_is_con(l2312, &n_list_46_Cons)) {
      u_value v_x = u_field(l2312, 0);
      if (u_truth(u_mk_bool(1))) {
        e2311 = u_append(e2311, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2312 = u_field(l2312, 1);
    }
    u_value t2313 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2314 = f_c_46_c_call_2(&l_u_mk_closure_2309, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2310));
    *r_ss = t2313;
    *r_r = t2314;
  }
}

void p_compile_46_compile_proc_lambda(u_value v_gl, u_value v_env, u_value v_ins, u_value v_outs, u_value v_b, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_caps = NULL;
    u_value v_f = NULL;
    u_value v_bs = NULL;
    u_value v_decls = NULL;
    u_value t2315 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2316 = &t2315;
    u_value l2317 = f_list_46_distinct_1(f_compile_46_free_in_stmt_2(v_b, f_syntax_46_names_1(f_list_46__43__43__2(v_ins, v_outs))));
    while (u_is_con(l2317, &n_list_46_Cons)) {
      u_value v_x = u_field(l2317, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2316 = u_append(e2316, v_x);
      }
      l2317 = u_field(l2317, 1);
    }
    u_value t2318 = t2315;
    v_caps = t2318;
    static struct u_object l_proc_2319 = U_LIT("proc");
    p_compile_46_gen_name(&l_proc_2319, &v_f, r_g);
    u_value t2320 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2321 = &t2320;
    u_value l2322 = f_syntax_46_names_1(v_outs);
    while (u_is_con(l2322, &n_list_46_Cons)) {
      u_value v_x = u_field(l2322, 0);
      if (u_truth(u_mk_bool(1))) {
        e2321 = u_append(e2321, f_compile_46_pointer_1(v_x));
      }
      l2322 = u_field(l2322, 1);
    }
    p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_ins))), t2320), v_b, &v_bs, r_g);
    static struct u_object l_env_2323 = U_LIT("env");
    static struct u_object l_ins_2324 = U_LIT("ins");
    u_value t2325 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2323), f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_ins), u_mk_int(0)), &l_ins_2324), f_compile_46_unpack_pointers_1(f_compile_46_numbered_2(f_syntax_46_names_1(v_outs), u_mk_int(0)))));
    v_decls = t2325;
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), v_f, f_compile_46_proc_params_0(), f_list_46__43__43__2(v_decls, v_bs)), r_g);
    static struct u_object l_u_mk_proc_2326 = U_LIT("u_mk_proc");
    u_value t2327 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2328 = &t2327;
    u_value l2329 = v_caps;
    while (u_is_con(l2329, &n_list_46_Cons)) {
      u_value v_x = u_field(l2329, 0);
      if (u_truth(u_mk_bool(1))) {
        e2328 = u_append(e2328, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2329 = u_field(l2329, 1);
    }
    u_value t2330 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2331 = f_c_46_c_call_2(&l_u_mk_proc_2326, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2327));
    *r_ss = t2330;
    *r_r = t2331;
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
  static struct u_object l_env_2332 = U_LIT("env");
  static struct u_object l_ins_2333 = U_LIT("ins");
  static struct u_object l_outs_2334 = U_LIT("outs");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2332), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_ins_2333), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, f_compile_46_value_ptr_0()), &l_outs_2334), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_compile_46_unpack_pointers_1(u_value v_xs) {
  u_value s2335 = v_xs;
  u_value t2336 = NULL;
  if ((!t2336) && u_is_con(s2335, &n_list_46_Nil)) {
    t2336 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2336) && u_is_con(s2335, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2335, 0), 0);
    u_value v_i = u_field(u_field(s2335, 0), 1);
    u_value v_rest = u_field(s2335, 1);
    static struct u_object l_outs_2337 = U_LIT("outs");
    t2336 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(&l_outs_2337, v_i))), f_compile_46_unpack_pointers_1(v_rest));
  }
  if (!t2336) {
    u_no_case(s2335);
  }
  return t2336;
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s2338 = v_xs;
  u_value t2339 = NULL;
  if ((!t2339) && u_is_con(s2338, &n_list_46_Nil)) {
    t2339 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2339) && u_is_con(s2338, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2338, 0), 0);
    u_value v_i = u_field(u_field(s2338, 0), 1);
    u_value v_rest = u_field(s2338, 1);
    t2339 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t2339) {
    u_no_case(s2338);
  }
  return t2339;
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
    u_value s2340 = v_cases;
    int d2341 = 0;
    if ((!d2341) && u_is_con(s2340, &n_list_46_Nil)) {
      d2341 = 1;
      {
        u_value t2342 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2342;
      }
    }
    if ((!d2341) && (u_is_con(s2340, &n_list_46_Cons) && u_is_con(u_field(s2340, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2340, 0), 0);
      u_value v_guard = u_field(u_field(s2340, 0), 1);
      u_value v_e = u_field(u_field(s2340, 0), 2);
      u_value v_more = u_field(s2340, 1);
      d2341 = 1;
      {
        u_value t2343 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2343;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2344 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t2344;
        u_value s2345 = v_guard;
        int d2346 = 0;
        if ((!d2346) && u_is_con(s2345, &n_option_46_Some)) {
          u_value v_c = u_field(s2345, 0);
          d2346 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2347 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2347;
          }
        }
        if ((!d2346) && u_is_con(s2345, &n_option_46_None)) {
          d2346 = 1;
          {
          }
        }
        if (!d2346) {
          u_no_case(s2345);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2348 = U_LIT("!");
        u_value t2349 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2348, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2349;
      }
    }
    if (!d2341) {
      u_no_case(s2340);
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
    u_value s2350 = u_mk_tuple(2, v_xs, v_lists);
    int d2351 = 0;
    if ((!d2351) && (u_is_con(u_field(s2350, 0), &n_list_46_Cons) && u_is_con(u_field(s2350, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2350, 0), 0);
      u_value v_more = u_field(u_field(s2350, 0), 1);
      u_value v_list = u_field(u_field(s2350, 1), 0);
      u_value v_others = u_field(u_field(s2350, 1), 1);
      d2351 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2352 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2352, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2353 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2353;
        static struct u_object l_u_is_con_2354 = U_LIT("u_is_con");
        u_value t2355 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2354, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2355;
      }
    }
    if (!d2351) {
      d2351 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2356 = U_LIT("u_append");
        u_value t2357 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2356, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2357;
      }
    }
    if (!d2351) {
      u_no_case(s2350);
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
    u_value s2358 = v_s;
    int d2359 = 0;
    if ((!d2359) && u_is_con(s2358, &n_syntax_46_SSkip)) {
      d2359 = 1;
      {
        u_value t2360 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2360;
      }
    }
    if ((!d2359) && u_is_con(s2358, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2358, 0);
      d2359 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2361 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2361;
      }
    }
    if ((!d2359) && u_is_con(s2358, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2358, 0);
      u_value v_body = u_field(s2358, 1);
      d2359 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t2362 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2363 = &t2362;
        u_value l2364 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2364, &n_list_46_Cons)) {
          u_value v_x = u_field(l2364, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2365 = U_LIT("NULL");
            e2363 = u_append(e2363, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2365))));
          }
          l2364 = u_field(l2364, 1);
        }
        u_value t2366 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2362, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2366;
      }
    }
    if ((!d2359) && u_is_con(s2358, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2358, 0);
      u_value v_l = u_field(s2358, 1);
      u_value v_r = u_field(s2358, 2);
      d2359 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2367 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2367;
      }
    }
    if ((!d2359) && u_is_con(s2358, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2358, 0);
      u_value v_b = u_field(s2358, 1);
      d2359 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2368 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2368, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2369 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2369;
      }
    }
    if ((!d2359) && u_is_con(s2358, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2358, 0);
      u_value v_ins = u_field(s2358, 1);
      u_value v_outs = u_field(s2358, 2);
      u_value v_refs = u_field(s2358, 3);
      d2359 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        if (u_truth(f_list_46_has_key_2(v_p, v_env))) {
          {
            static struct u_object l_u_call_proc_2370 = U_LIT("u_call_proc");
            u_value t2371 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e2372 = &t2371;
            u_value l2373 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
            while (u_is_con(l2373, &n_list_46_Cons)) {
              u_value v_x = u_field(l2373, 0);
              if (u_truth(u_mk_bool(1))) {
                e2372 = u_append(e2372, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
              }
              l2373 = u_field(l2373, 1);
            }
            u_value t2374 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_call_proc_2370, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_var_expr_2(v_env, v_p), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_ins)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(f_list_46__43__43__2(v_outs, v_refs))), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(v_rs, t2371)))), u_mk_con(&n_list_46_Nil, 0)));
            *r_ss = t2374;
          }
        } else {
          {
            u_value t2375 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e2376 = &t2375;
            u_value l2377 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
            while (u_is_con(l2377, &n_list_46_Cons)) {
              u_value v_x = u_field(l2377, 0);
              if (u_truth(u_mk_bool(1))) {
                e2376 = u_append(e2376, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
              }
              l2377 = u_field(l2377, 1);
            }
            u_value t2378 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2375))), u_mk_con(&n_list_46_Nil, 0)));
            *r_ss = t2378;
          }
        }
      }
    }
    if ((!d2359) && u_is_con(s2358, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2358, 0);
      u_value v_cases = u_field(s2358, 1);
      d2359 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2379 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2379, &v_d, r_g);
        static struct u_object l_d_2380 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2380, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2381 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2381;
        static struct u_object l__33__2382 = U_LIT("!");
        static struct u_object l_u_no_case_2383 = U_LIT("u_no_case");
        u_value t2384 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2382, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2383, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2384;
      }
    }
    if ((!d2359) && (u_is_con(s2358, &n_syntax_46_SAssert) && (u_is_con(u_field(s2358, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2358, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2358, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2358, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2358, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2358, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2358, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2358, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2358, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2358, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2358, 0), 1), 1), 0);
      d2359 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2385 = U_LIT("u_assert_equal");
        u_value t2386 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2385, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2386;
      }
    }
    if ((!d2359) && u_is_con(s2358, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2358, 0);
      d2359 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2387 = U_LIT("u_assert_true");
        u_value t2388 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2387, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2388;
      }
    }
    if ((!d2359) && u_is_con(s2358, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2358, 0);
      u_value v_ty = u_field(s2358, 1);
      d2359 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2389 = U_LIT("u_raise");
        u_value t2390 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2389, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2390;
      }
    }
    if ((!d2359) && u_is_con(s2358, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2358, 0);
      u_value v_hs = u_field(s2358, 1);
      d2359 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2391 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2391, &v_d, r_g);
        static struct u_object l_d_2392 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2392, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2393 = U_LIT("u_pop");
        static struct u_object l__33__2394 = U_LIT("!");
        static struct u_object l_u_reraise_2395 = U_LIT("u_reraise");
        u_value t2396 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2393, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2394, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2395, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2396;
        static struct u_object l_struct_32_u_handler_2397 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2398 = U_LIT("u_push");
        static struct u_object l__61__61__2399 = U_LIT("==");
        static struct u_object l_setjmp_2400 = U_LIT("setjmp");
        static struct u_object l_env_2401 = U_LIT("env");
        static struct u_object l_u_pop_2402 = U_LIT("u_pop");
        u_value t2403 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2397)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2398, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2399, f_c_46_c_call_2(&l_setjmp_2400, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2401), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2402, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2403;
      }
    }
    if (!d2359) {
      u_no_case(s2358);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2404 = v_xs;
    int d2405 = 0;
    if ((!d2405) && u_is_con(s2404, &n_list_46_Nil)) {
      d2405 = 1;
      {
        u_value t2406 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2406;
      }
    }
    if ((!d2405) && u_is_con(s2404, &n_list_46_Cons)) {
      u_value v_x = u_field(s2404, 0);
      u_value v_rest = u_field(s2404, 1);
      d2405 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2407 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2407;
      }
    }
    if (!d2405) {
      u_no_case(s2404);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2408 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2409 = NULL;
  if ((!t2409) && (u_is_con(u_field(s2408, 0), &n_list_46_Cons) && (u_is_con(u_field(s2408, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2408, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2408, 0), 0);
    u_value v_more = u_field(u_field(s2408, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2408, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2408, 1), 1);
    t2409 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2409) {
    t2409 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2409) {
    u_no_case(s2408);
  }
  return t2409;
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
    static struct u_object l_u_caught_2410 = U_LIT("u_caught");
    u_value t2411 = f_c_46_c_call_2(&l_u_caught_2410, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2411;
    u_value s2412 = v_hs;
    int d2413 = 0;
    if ((!d2413) && u_is_con(s2412, &n_list_46_Nil)) {
      d2413 = 1;
      {
        u_value t2414 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2414;
      }
    }
    if ((!d2413) && (u_is_con(s2412, &n_list_46_Cons) && u_is_con(u_field(s2412, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2412, 0), 0);
      u_value v_p = u_field(u_field(s2412, 0), 1);
      u_value v_guard = u_field(u_field(s2412, 0), 2);
      u_value v_s = u_field(u_field(s2412, 0), 3);
      u_value v_more = u_field(s2412, 1);
      d2413 = 1;
      {
        u_value t2415 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2415;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2416 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2416;
        u_value s2417 = v_guard;
        int d2418 = 0;
        if ((!d2418) && u_is_con(s2417, &n_option_46_Some)) {
          u_value v_c = u_field(s2417, 0);
          d2418 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2419 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2419;
          }
        }
        if ((!d2418) && u_is_con(s2417, &n_option_46_None)) {
          d2418 = 1;
          {
          }
        }
        if (!d2418) {
          u_no_case(s2417);
        }
        u_value s2420 = v_p;
        int d2421 = 0;
        if ((!d2421) && u_is_con(s2420, &n_syntax_46_PWild)) {
          d2421 = 1;
          {
            u_value t2422 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2422;
          }
        }
        if (!d2421) {
          d2421 = 1;
          {
            static struct u_object l_u_has_type_2423 = U_LIT("u_has_type");
            u_value t2424 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2423, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2424;
          }
        }
        if (!d2421) {
          u_no_case(s2420);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2425 = U_LIT("!");
        u_value t2426 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2425, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2426;
      }
    }
    if (!d2413) {
      u_no_case(s2412);
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
    u_value s2427 = v_cases;
    int d2428 = 0;
    if ((!d2428) && u_is_con(s2427, &n_list_46_Nil)) {
      d2428 = 1;
      {
        u_value t2429 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2429;
      }
    }
    if ((!d2428) && (u_is_con(s2427, &n_list_46_Cons) && u_is_con(u_field(s2427, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2427, 0), 0);
      u_value v_guard = u_field(u_field(s2427, 0), 1);
      u_value v_s = u_field(u_field(s2427, 0), 2);
      u_value v_more = u_field(s2427, 1);
      d2428 = 1;
      {
        u_value t2430 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2430;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2431 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2431;
        u_value s2432 = v_guard;
        int d2433 = 0;
        if ((!d2433) && u_is_con(s2432, &n_option_46_Some)) {
          u_value v_c = u_field(s2432, 0);
          d2433 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2434 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2434;
          }
        }
        if ((!d2433) && u_is_con(s2432, &n_option_46_None)) {
          d2433 = 1;
          {
          }
        }
        if (!d2433) {
          u_no_case(s2432);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2435 = U_LIT("!");
        u_value t2436 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2435, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2436;
      }
    }
    if (!d2428) {
      u_no_case(s2427);
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
    u_value s2437 = v_c;
    int d2438 = 0;
    if ((!d2438) && u_is_con(s2437, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2437, 0);
      u_value v_xss = u_field(s2437, 2);
      u_value v_b = u_field(s2437, 4);
      d2438 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2439 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2440 = &t2439;
        u_value l2441 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2441, &n_list_46_Cons)) {
          u_value v_x = u_field(l2441, 0);
          if (u_truth(u_mk_bool(1))) {
            e2440 = u_append(e2440, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2441 = u_field(l2441, 1);
        }
        u_value t2442 = t2439;
        v_params = t2442;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2443 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2443;
          }
        } else {
          {
            u_value t2444 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2444;
          }
        }
        u_value t2445 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2446 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2445;
        *r_ms = t2446;
      }
    }
    if ((!d2438) && u_is_con(s2437, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2437, 0);
      u_value v_ins = u_field(s2437, 2);
      u_value v_outs = u_field(s2437, 3);
      u_value v_refs = u_field(s2437, 4);
      u_value v_body = u_field(s2437, 5);
      d2438 = 1;
      {
        u_value t2447 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2448 = &t2447;
        u_value l2449 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2449, &n_list_46_Cons)) {
          u_value v_x = u_field(l2449, 0);
          if (u_truth(u_mk_bool(1))) {
            e2448 = u_append(e2448, f_compile_46_pointer_1(v_x));
          }
          l2449 = u_field(l2449, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2447), v_body, &v_bs, r_g);
        u_value t2450 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2451 = &t2450;
        u_value l2452 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2452, &n_list_46_Cons)) {
          u_value v_x = u_field(l2452, 0);
          if (u_truth(u_mk_bool(1))) {
            e2451 = u_append(e2451, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2452 = u_field(l2452, 1);
        }
        u_value t2453 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2454 = &t2453;
        u_value l2455 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2455, &n_list_46_Cons)) {
          u_value v_x = u_field(l2455, 0);
          if (u_truth(u_mk_bool(1))) {
            e2454 = u_append(e2454, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2455 = u_field(l2455, 1);
        }
        u_value t2456 = f_list_46__43__43__2(t2450, t2453);
        v_params = t2456;
        u_value t2457 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2458 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2457;
        *r_ms = t2458;
      }
    }
    if ((!d2438) && u_is_con(s2437, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s2437, 0);
      d2438 = 1;
      {
        p_compile_46_compile_expr(v_gl, u_mk_con(&n_list_46_Nil, 0), v_e, &v_bs, &v_rb, r_g);
        static struct u_object l_u_print_line_2459 = U_LIT("u_print_line");
        u_value t2460 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2461 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_line_2459, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2460;
        *r_ms = t2461;
      }
    }
    if ((!d2438) && (u_is_con(s2437, &n_syntax_46_CRun) && u_is_con(u_field(s2437, 1), &n_syntax_46_SBlock))) {
      u_value v_xs = u_field(u_field(s2437, 1), 0);
      u_value v_body = u_field(u_field(s2437, 1), 1);
      d2438 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_body, &v_bs, r_g);
        u_value t2462 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2463 = &t2462;
        u_value l2464 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2464, &n_list_46_Cons)) {
          u_value v_x = u_field(l2464, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2465 = U_LIT("NULL");
            e2463 = u_append(e2463, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2465))));
          }
          l2464 = u_field(l2464, 1);
        }
        u_value t2466 = t2462;
        v_copy = t2466;
        u_value t2467 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2468 = &t2467;
        u_value l2469 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2469, &n_list_46_Cons)) {
          u_value v_x = u_field(l2469, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_u_print_binding_2470 = U_LIT("u_print_binding");
            e2468 = u_append(e2468, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_binding_2470, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_syntax_46_Name_46_base_1(v_x)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x)), u_mk_con(&n_list_46_Nil, 0))))));
          }
          l2469 = u_field(l2469, 1);
        }
        u_value t2471 = t2467;
        v_back = t2471;
        u_value t2472 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2473 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_copy, f_list_46__43__43__2(v_bs, v_back))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2472;
        *r_ms = t2473;
      }
    }
    if ((!d2438) && u_is_con(s2437, &n_syntax_46_CRun)) {
      u_value v_body = u_field(s2437, 1);
      d2438 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2474 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2475 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2474;
        *r_ms = t2475;
      }
    }
    if ((!d2438) && u_is_con(s2437, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s2437, 1);
      d2438 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2476 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2477 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2476;
        *r_ms = t2477;
      }
    }
    if (!d2438) {
      d2438 = 1;
      {
        u_value t2478 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2479 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2478;
        *r_ms = t2479;
      }
    }
    if (!d2438) {
      u_no_case(s2437);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2480 = v_cs;
    int d2481 = 0;
    if ((!d2481) && u_is_con(s2480, &n_list_46_Nil)) {
      d2481 = 1;
      {
        u_value t2482 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2483 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2482;
        *r_ms = t2483;
      }
    }
    if ((!d2481) && u_is_con(s2480, &n_list_46_Cons)) {
      u_value v_c = u_field(s2480, 0);
      u_value v_rest = u_field(s2480, 1);
      d2481 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_rest, &v_d2, &v_m2, r_g);
        u_value t2484 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2485 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2484;
        *r_ms = t2485;
      }
    }
    if (!d2481) {
      u_no_case(s2480);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_handler_2486 = U_LIT("handler");
  u_value t2513 = u_mk_con(&n_c_46_CeVar, 1, &l_handler_2486);
  u_value t2512;
  {
    u_value v_h = t2513;
    static struct u_object l_static_32_u_value_2487 = U_LIT("static u_value");
    static struct u_object l_cache_2488 = U_LIT("cache");
    static struct u_object l_NULL_2489 = U_LIT("NULL");
    static struct u_object l__61__61__2490 = U_LIT("==");
    static struct u_object l_cache_2491 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2492 = U_LIT("U_COMPUTING");
    static struct u_object l_u_fail_2493 = U_LIT("u_fail");
    static struct u_object l_recursive_32_initia_2494 = U_LIT("recursive initialization of ");
    static struct u_object l_cache_2495 = U_LIT("cache");
    static struct u_object l_cache_2496 = U_LIT("cache");
    static struct u_object l_cache_2497 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2498 = U_LIT("U_COMPUTING");
    static struct u_object l_struct_32_u_handler_2499 = U_LIT("struct u_handler");
    static struct u_object l_handler_2500 = U_LIT("handler");
    static struct u_object l_u_push_2501 = U_LIT("u_push");
    static struct u_object l__33__61__2502 = U_LIT("!=");
    static struct u_object l_setjmp_2503 = U_LIT("setjmp");
    static struct u_object l_env_2504 = U_LIT("env");
    static struct u_object l_u_pop_2505 = U_LIT("u_pop");
    static struct u_object l_cache_2506 = U_LIT("cache");
    static struct u_object l_NULL_2507 = U_LIT("NULL");
    static struct u_object l_u_reraise_2508 = U_LIT("u_reraise");
    static struct u_object l_u_pop_2509 = U_LIT("u_pop");
    static struct u_object l_cache_2510 = U_LIT("cache");
    static struct u_object l_cache_2511 = U_LIT("cache");
    t2512 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2487), &l_cache_2488, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2489))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2490, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2491), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2492)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2493, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2494, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2495), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2496))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2497), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2498)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2499)), &l_handler_2500, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2501, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__33__61__2502, f_c_46_c_call_2(&l_setjmp_2503, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, v_h, &l_env_2504), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2505, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2506), u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2507)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2508, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2509, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2510), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2511))), u_mk_con(&n_list_46_Nil, 0))))));
  }
  return t2512;
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2514 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2515 = &t2514;
  u_value l2516 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l2516, &n_list_46_Cons)) {
    u_value v_p = u_field(l2516, 0);
    if (u_truth(u_mk_bool(1))) {
      e2515 = u_append(e2515, f_compile_46_fun_wrapper_1(v_p));
    }
    l2516 = u_field(l2516, 1);
  }
  u_value t2517 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2518 = &t2517;
  u_value l2519 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l2519, &n_list_46_Cons)) {
    u_value v_p = u_field(l2519, 0);
    if (u_truth(u_mk_bool(1))) {
      e2518 = u_append(e2518, f_compile_46_constr_wrapper_1(v_p));
    }
    l2519 = u_field(l2519, 1);
  }
  u_value t2520 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2521 = &t2520;
  u_value l2522 = f_list_46_seconds_1(f_map_46_entries_1(f_compile_46_builtin_functions_0()));
  while (u_is_con(l2522, &n_list_46_Cons)) {
    u_value v_p = u_field(l2522, 0);
    if (u_truth(u_mk_bool(1))) {
      e2521 = u_append(e2521, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2522 = u_field(l2522, 1);
  }
  u_value t2523 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2524 = &t2523;
  u_value l2525 = f_map_46_entries_1(f_compile_46_Globals_46_procs_1(v_gl));
  while (u_is_con(l2525, &n_list_46_Cons)) {
    u_value v_p = u_field(l2525, 0);
    if (u_truth(u_mk_bool(1))) {
      e2524 = u_append(e2524, f_compile_46_proc_wrapper_1(v_p));
    }
    l2525 = u_field(l2525, 1);
  }
  return f_list_46__43__43__2(t2514, f_list_46__43__43__2(f_list_46_concat_1(t2517), f_list_46__43__43__2(t2520, t2523)));
}

u_value f_compile_46_proc_wrapper_1(u_value v_p) {
  u_value s2526 = v_p;
  u_value t2527 = NULL;
  if (!t2527) {
    u_value v_n = u_field(s2526, 0);
    u_value v_k = u_field(u_field(s2526, 1), 0);
    u_value v_m = u_field(u_field(s2526, 1), 1);
    static struct u_object l_wp__2528 = U_LIT("wp_");
    u_value t2529 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2530 = &t2529;
    u_value l2531 = f_compile_46_range_to_1(v_k);
    while (u_is_con(l2531, &n_list_46_Cons)) {
      u_value v_i = u_field(l2531, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_ins_2532 = U_LIT("ins");
        e2530 = u_append(e2530, f_compile_46_index_2(&l_ins_2532, v_i));
      }
      l2531 = u_field(l2531, 1);
    }
    u_value t2533 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2534 = &t2533;
    u_value l2535 = f_compile_46_range_to_1(v_m);
    while (u_is_con(l2535, &n_list_46_Cons)) {
      u_value v_i = u_field(l2535, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_outs_2536 = U_LIT("outs");
        e2534 = u_append(e2534, f_compile_46_index_2(&l_outs_2536, v_i));
      }
      l2535 = u_field(l2535, 1);
    }
    t2527 = u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), u_concat(&l_wp__2528, f_compile_46_mangle_1(v_n)), f_compile_46_proc_params_0(), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_n), f_list_46__43__43__2(t2529, t2533))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2527) {
    u_no_case(s2526);
  }
  return t2527;
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2537 = v_p;
  u_value t2538 = NULL;
  if (!t2538) {
    u_value v_n = u_field(s2537, 0);
    u_value v_k = u_field(s2537, 1);
    static struct u_object l_w__2539 = U_LIT("w_");
    t2538 = f_compile_46_wrapper_3(u_concat(&l_w__2539, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2538) {
    u_no_case(s2537);
  }
  return t2538;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2540 = v_p;
  u_value t2541 = NULL;
  if (!t2541) {
    u_value v_f = u_field(s2540, 0);
    u_value v_k = u_field(s2540, 1);
    static struct u_object l_w_2542 = U_LIT("w");
    t2541 = f_compile_46_wrapper_3(u_concat(&l_w_2542, v_f), v_f, v_k);
  }
  if (!t2541) {
    u_no_case(s2540);
  }
  return t2541;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2543 = U_LIT("env");
  static struct u_object l_args_2544 = U_LIT("args");
  u_value t2545 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2546 = &t2545;
  u_value l2547 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2547, &n_list_46_Cons)) {
    u_value v_i = u_field(l2547, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2548 = U_LIT("args");
      e2546 = u_append(e2546, f_compile_46_index_2(&l_args_2548, v_i));
    }
    l2547 = u_field(l2547, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2543), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2544), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2545))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2549 = v_p;
  u_value t2550 = NULL;
  if ((!t2550) && u_equals(u_field(s2549, 1), u_mk_int(0))) {
    t2550 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2550) {
    u_value v_n = u_field(s2549, 0);
    u_value v_k = u_field(s2549, 1);
    static struct u_object l_wc__2551 = U_LIT("wc_");
    static struct u_object l_env_2552 = U_LIT("env");
    static struct u_object l_args_2553 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2554 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2555 = U_LIT("args");
    t2550 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2551, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2552), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2553), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2554, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2555), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2550) {
    u_no_case(s2549);
  }
  return t2550;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2556;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2556 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2556 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2556;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2557 = v_d;
  u_value t2558 = NULL;
  if ((!t2558) && u_is_con(s2557, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2557, 0);
    u_value v_f = u_field(s2557, 1);
    u_value v_ps = u_field(s2557, 2);
    t2558 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2558) {
    t2558 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2558) {
    u_no_case(s2557);
  }
  return t2558;
}

void p_compile_46_compile_program(u_value v_q, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2559 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2559;
    u_value t2560 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t2560;
    p_compile_46_compile_cmds(v_gl, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value t2561 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, f_compile_46_Gen_46_lifted_1(v_g)));
    v_fs = t2561;
    static struct u_object l_ulang_46_h_2562 = U_LIT("ulang.h");
    u_value t2563 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2564 = &t2563;
    u_value l2565 = v_fs;
    while (u_is_con(l2565, &n_list_46_Cons)) {
      u_value v_d = u_field(l2565, 0);
      if (u_truth(u_mk_bool(1))) {
        e2564 = u_append(e2564, f_compile_46_prototype_1(v_d));
      }
      l2565 = u_field(l2565, 1);
    }
    static struct u_object l_main_2566 = U_LIT("main");
    static struct u_object l_argc_2567 = U_LIT("argc");
    static struct u_object l_char_2568 = U_LIT("char");
    static struct u_object l_argv_2569 = U_LIT("argv");
    static struct u_object l_u_args_2570 = U_LIT("u_args");
    static struct u_object l_argc_2571 = U_LIT("argc");
    static struct u_object l_argv_2572 = U_LIT("argv");
    u_value t2573 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2562), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2563), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2566, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2567), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2568))), &l_argv_2569), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2570, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2571), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2572), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2573;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_target) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2574 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2574, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2575 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2575, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q);
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

u_value lam611(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_t = args[0];
  return f_ctx_46_is_sort_2(f_check_46_State_46_ctx_1(v_s), v_t);
}

u_value lam722(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam725(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam790(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_q = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_q);
}

u_value lam1067(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  {
    u_value v_args = NULL;
    p_os_args(&v_args);
    u_value s2576 = v_args;
    int d2577 = 0;
    if ((!d2577) && u_is_con(s2576, &n_list_46_Nil)) {
      d2577 = 1;
      {
      }
    }
    if ((!d2577) && (u_is_con(s2576, &n_list_46_Cons) && (u_is_con(u_field(s2576, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2576, 1), 1), &n_list_46_Nil)))) {
      u_value v_source = u_field(s2576, 0);
      u_value v_target = u_field(u_field(s2576, 1), 0);
      d2577 = 1;
      {
        p_compile_46_compile_file(v_source, v_target);
      }
    }
    if (!d2577) {
      d2577 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_sou_2578 = U_LIT("usage: ulang source.u target.c");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_sou_2578), "os.OsError");
      }
    }
    if (!d2577) {
      u_no_case(s2576);
    }
    u_print_binding("args", v_args);
  }
  return 0;
}
