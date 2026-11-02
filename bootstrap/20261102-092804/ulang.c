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

u_value lam348(u_value *env, u_value *args);

u_value lam349(u_value *env, u_value *args);

u_value lam351(u_value *env, u_value *args);

u_value lam352(u_value *env, u_value *args);

u_value lam362(u_value *env, u_value *args);

u_value lam369(u_value *env, u_value *args);

u_value lam373(u_value *env, u_value *args);

u_value lam603(u_value *env, u_value *args);

u_value lam714(u_value *env, u_value *args);

u_value lam717(u_value *env, u_value *args);

u_value lam782(u_value *env, u_value *args);

u_value lam1059(u_value *env, u_value *args);

u_value lam1751(u_value *env, u_value *args);

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
    t318 = f_list_46_exists_2(v_args, u_mk_closure(w_f_check_46_holes_1, 0));
  }
  if ((!t318) && u_is_con(s317, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s317, 0);
    u_value v_r = u_field(s317, 1);
    u_value t319 = f_list_46_exists_2(v_args, u_mk_closure(w_f_check_46_holes_1, 0));
    if (!u_truth(t319)) {
      t319 = f_check_46_holes_1(v_r);
    }
    t318 = t319;
  }
  if ((!t318) && u_is_con(s317, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s317, 0);
    u_value v_os = u_field(s317, 1);
    u_value v_rs = u_field(s317, 2);
    t318 = f_list_46_exists_2(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs)), u_mk_closure(w_f_check_46_holes_1, 0));
  }
  if ((!t318) && u_is_con(s317, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s317, 0);
    t318 = f_list_46_exists_2(v_ts, u_mk_closure(w_f_check_46_holes_1, 0));
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
  u_value s320 = v_t;
  u_value t321 = NULL;
  if ((!t321) && u_is_con(s320, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s320, 0);
    u_value v_args = u_field(s320, 1);
    u_value t322 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e323 = &t322;
    u_value l324 = v_args;
    while (u_is_con(l324, &n_list_46_Cons)) {
      u_value v_a = u_field(l324, 0);
      if (u_truth(u_mk_bool(1))) {
        e323 = u_append(e323, f_check_46_subst_2(v_a, v_m));
      }
      l324 = u_field(l324, 1);
    }
    t321 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t322);
  }
  if ((!t321) && u_is_con(s320, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s320, 0);
    u_value v_r = u_field(s320, 1);
    u_value t325 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e326 = &t325;
    u_value l327 = v_args;
    while (u_is_con(l327, &n_list_46_Cons)) {
      u_value v_a = u_field(l327, 0);
      if (u_truth(u_mk_bool(1))) {
        e326 = u_append(e326, f_check_46_subst_2(v_a, v_m));
      }
      l327 = u_field(l327, 1);
    }
    t321 = u_mk_con(&n_syntax_46_TFun, 2, t325, f_check_46_subst_2(v_r, v_m));
  }
  if ((!t321) && u_is_con(s320, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s320, 0);
    u_value v_os = u_field(s320, 1);
    u_value v_rs = u_field(s320, 2);
    u_value t328 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e329 = &t328;
    u_value l330 = v_is;
    while (u_is_con(l330, &n_list_46_Cons)) {
      u_value v_a = u_field(l330, 0);
      if (u_truth(u_mk_bool(1))) {
        e329 = u_append(e329, f_check_46_subst_2(v_a, v_m));
      }
      l330 = u_field(l330, 1);
    }
    u_value t331 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e332 = &t331;
    u_value l333 = v_os;
    while (u_is_con(l333, &n_list_46_Cons)) {
      u_value v_a = u_field(l333, 0);
      if (u_truth(u_mk_bool(1))) {
        e332 = u_append(e332, f_check_46_subst_2(v_a, v_m));
      }
      l333 = u_field(l333, 1);
    }
    u_value t334 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e335 = &t334;
    u_value l336 = v_rs;
    while (u_is_con(l336, &n_list_46_Cons)) {
      u_value v_a = u_field(l336, 0);
      if (u_truth(u_mk_bool(1))) {
        e335 = u_append(e335, f_check_46_subst_2(v_a, v_m));
      }
      l336 = u_field(l336, 1);
    }
    t321 = u_mk_con(&n_syntax_46_TProc, 3, t328, t331, t334);
  }
  if ((!t321) && u_is_con(s320, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s320, 0);
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
    t321 = u_mk_con(&n_syntax_46_TTuple, 1, t337);
  }
  if (!t321) {
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
    t321 = t341;
  }
  if (!t321) {
    u_no_case(s320);
  }
  return t321;
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
  if ((!t347) && u_is_con(s346, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s346, 0);
    u_value v_os = u_field(s346, 1);
    u_value v_rs = u_field(s346, 2);
    t347 = f_list_46_exists_2(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs)), u_mk_closure(lam351, 2, v_su, v_i));
  }
  if ((!t347) && u_is_con(s346, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s346, 0);
    t347 = f_list_46_exists_2(v_ts, u_mk_closure(lam352, 2, v_su, v_i));
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
  u_value s353 = v_k;
  u_value t354 = NULL;
  if (!t354) {
    u_value v_ps = u_field(s353, 0);
    u_value v_ks = u_field(s353, 1);
    u_value v_r = u_field(s353, 2);
    u_value t355;
    if (u_truth(u_eq(v_ks, u_mk_con(&n_list_46_Nil, 0)))) {
      t355 = v_r;
    } else {
      t355 = f_check_46_fun_2(v_ks, v_r);
    }
    t354 = u_mk_tuple(2, v_ps, t355);
  }
  if (!t354) {
    u_no_case(s353);
  }
  return t354;
}

u_value f_check_46_constructor_of_2(u_value v_c, u_value v_k) {
  u_value s356 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t357 = NULL;
  if ((!t357) && u_is_con(s356, &n_option_46_Some)) {
    u_value v_kc = u_field(s356, 0);
    t357 = f_check_46_constructor_scheme_1(v_kc);
  }
  if ((!t357) && u_is_con(s356, &n_option_46_None)) {
    t357 = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t357) {
    u_no_case(s356);
  }
  return t357;
}

u_value f_check_46_candidates_3(u_value v_s, u_value v_env, u_value v_n) {
  u_value s358 = f_list_46_lookup_2(v_n, v_env);
  u_value t359 = NULL;
  if ((!t359) && u_is_con(s358, &n_option_46_Some)) {
    u_value v_sc = u_field(s358, 0);
    t359 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, v_sc), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t359) && u_is_con(s358, &n_option_46_None)) {
    u_value t360 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e361 = &t360;
    u_value l363 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam362, 1, v_s));
    while (u_is_con(l363, &n_list_46_Cons)) {
      u_value v_f = u_field(l363, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t364 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e365 = &t364;
        u_value l366 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(v_s), v_f);
        while (u_is_con(l366, &n_list_46_Cons)) {
          u_value v_sc = u_field(l366, 0);
          if (u_truth(u_mk_bool(1))) {
            e365 = u_append(e365, u_mk_tuple(2, v_f, v_sc));
          }
          l366 = u_field(l366, 1);
        }
        e361 = u_append(e361, t364);
      }
      l363 = u_field(l363, 1);
    }
    u_value t367 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e368 = &t367;
    u_value l370 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam369, 1, v_s));
    while (u_is_con(l370, &n_list_46_Cons)) {
      u_value v_k = u_field(l370, 0);
      if (u_truth(u_mk_bool(1))) {
        e368 = u_append(e368, u_mk_tuple(2, v_k, f_check_46_constructor_of_2(f_check_46_State_46_ctx_1(v_s), v_k)));
      }
      l370 = u_field(l370, 1);
    }
    u_value t371 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e372 = &t371;
    u_value l374 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam373, 1, v_s));
    while (u_is_con(l374, &n_list_46_Cons)) {
      u_value v_p = u_field(l374, 0);
      if (u_truth(u_mk_bool(1))) {
        e372 = u_append(e372, u_mk_tuple(2, v_p, f_check_46_proc_scheme_2(f_check_46_State_46_ctx_1(v_s), v_p)));
      }
      l374 = u_field(l374, 1);
    }
    u_value t375 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e376 = &t375;
    u_value l377 = f_check_46_builtin_1(v_n);
    while (u_is_con(l377, &n_list_46_Cons)) {
      u_value v_sc = u_field(l377, 0);
      if (u_truth(u_mk_bool(1))) {
        e376 = u_append(e376, u_mk_tuple(2, v_n, v_sc));
      }
      l377 = u_field(l377, 1);
    }
    t359 = f_list_46__43__43__2(f_list_46_concat_1(t360), f_list_46__43__43__2(t367, f_list_46__43__43__2(t371, t375)));
  }
  if (!t359) {
    u_no_case(s358);
  }
  return t359;
}

u_value f_check_46_proc_scheme_2(u_value v_c, u_value v_p) {
  u_value s378 = f_ctx_46_proc_of_2(v_c, v_p);
  u_value t379 = NULL;
  if ((!t379) && u_is_con(s378, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s378, 0), 0);
    u_value v_is = u_field(u_field(s378, 0), 1);
    u_value v_os = u_field(u_field(s378, 0), 2);
    u_value v_rs = u_field(u_field(s378, 0), 3);
    t379 = u_mk_tuple(2, v_ps, u_mk_con(&n_syntax_46_TProc, 3, v_is, v_os, v_rs));
  }
  if ((!t379) && u_is_con(s378, &n_option_46_None)) {
    t379 = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_syntax_46_THole, 0));
  }
  if (!t379) {
    u_no_case(s378);
  }
  return t379;
}

u_value f_check_46_with_arity_2(u_value v_cs, u_value v_k) {
  u_value s380 = v_cs;
  u_value t381 = NULL;
  if ((!t381) && u_is_con(s380, &n_list_46_Nil)) {
    t381 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t381) && u_is_con(s380, &n_list_46_Cons)) {
    u_value v_n = u_field(u_field(s380, 0), 0);
    u_value v_ps = u_field(u_field(u_field(s380, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s380, 0), 1), 1);
    u_value v_rest = u_field(s380, 1);
    u_value t382;
    if (u_truth(u_eq(f_syntax_46_arity_1(v_t), v_k))) {
      t382 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, v_ps, v_t)), f_check_46_with_arity_2(v_rest, v_k));
    } else {
      t382 = f_check_46_with_arity_2(v_rest, v_k);
    }
    t381 = t382;
  }
  if (!t381) {
    u_no_case(s380);
  }
  return t381;
}

u_value f_check_46_has_arity_2(u_value v_scs, u_value v_k) {
  u_value s383 = v_scs;
  u_value t384 = NULL;
  if ((!t384) && u_is_con(s383, &n_list_46_Nil)) {
    t384 = u_mk_bool(0);
  }
  if ((!t384) && u_is_con(s383, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s383, 0), 1);
    u_value v_rest = u_field(s383, 1);
    u_value t385 = u_eq(f_syntax_46_arity_1(v_t), v_k);
    if (!u_truth(t385)) {
      t385 = f_check_46_has_arity_2(v_rest, v_k);
    }
    t384 = t385;
  }
  if (!t384) {
    u_no_case(s383);
  }
  return t384;
}

u_value f_check_46_bind_1(u_value v_xs) {
  u_value t386 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e387 = &t386;
  u_value l388 = v_xs;
  while (u_is_con(l388, &n_list_46_Cons)) {
    u_value v_x = u_field(l388, 0);
    if (u_truth(u_mk_bool(1))) {
      e387 = u_append(e387, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_syntax_46_Var_46_typ_1(v_x))));
    }
    l388 = u_field(l388, 1);
  }
  return t386;
}

u_value f_check_46_fixed_2(u_value v_su, u_value v_env) {
  u_value s389 = v_env;
  u_value t390 = NULL;
  if ((!t390) && u_is_con(s389, &n_list_46_Nil)) {
    t390 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t390) && u_is_con(s389, &n_list_46_Cons)) {
    u_value v_ps = u_field(u_field(u_field(s389, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s389, 0), 1), 1);
    u_value v_rest = u_field(s389, 1);
    t390 = f_list_46__43__43__2(f_list_46_diff_2(f_check_46_free_1(f_check_46_zonk_type_2(v_su, v_t)), v_ps), f_check_46_fixed_2(v_su, v_rest));
  }
  if (!t390) {
    u_no_case(s389);
  }
  return t390;
}

u_value f_check_46_generalize_3(u_value v_su, u_value v_env, u_value v_t) {
  u_value t396 = f_check_46_fixed_2(v_su, v_env);
  u_value t397 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t395;
  {
    u_value v_fx = t396;
    u_value v_u = t397;
    u_value t391 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e392 = &t391;
    u_value l393 = f_check_46_free_1(v_u);
    while (u_is_con(l393, &n_list_46_Cons)) {
      u_value v_m = u_field(l393, 0);
      u_value t394 = f_check_46_is_meta_1(v_m);
      if (u_truth(t394)) {
        t394 = f_prelude_46_not_1(f_list_46_contains_2(v_m, v_fx));
      }
      if (u_truth(t394)) {
        e392 = u_append(e392, v_m);
      }
      l393 = u_field(l393, 1);
    }
    t395 = u_mk_tuple(2, t391, v_u);
  }
  return t395;
}

u_value f_check_46_generalized_3(u_value v_su, u_value v_env, u_value v_xs) {
  u_value t398 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e399 = &t398;
  u_value l400 = v_xs;
  while (u_is_con(l400, &n_list_46_Cons)) {
    u_value v_x = u_field(l400, 0);
    if (u_truth(u_mk_bool(1))) {
      e399 = u_append(e399, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), f_check_46_generalize_3(v_su, v_env, f_syntax_46_Var_46_typ_1(v_x))));
    }
    l400 = u_field(l400, 1);
  }
  return t398;
}

u_value f_check_46_case_of_1(u_value v_b) {
  u_value s401 = v_b;
  u_value t402 = NULL;
  if ((!t402) && u_is_con(s401, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s401, 0);
    u_value v_g = u_field(s401, 1);
    t402 = u_mk_tuple(2, v_p, u_eq(v_g, u_mk_con(&n_option_46_None, 0)));
  }
  if (!t402) {
    u_no_case(s401);
  }
  return t402;
}

u_value f_check_46_zonk_guard_2(u_value v_su, u_value v_g) {
  u_value s403 = v_g;
  u_value t404 = NULL;
  if ((!t404) && u_is_con(s403, &n_option_46_Some)) {
    u_value v_e = u_field(s403, 0);
    t404 = u_mk_con(&n_option_46_Some, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t404) && u_is_con(s403, &n_option_46_None)) {
    t404 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t404) {
    u_no_case(s403);
  }
  return t404;
}

u_value f_check_46_zonk_type_2(u_value v_su, u_value v_t) {
  u_value s405 = f_check_46_resolve_2(v_su, v_t);
  u_value t406 = NULL;
  if ((!t406) && u_is_con(s405, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s405, 0);
    u_value v_args = u_field(s405, 1);
    u_value t407 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e408 = &t407;
    u_value l409 = v_args;
    while (u_is_con(l409, &n_list_46_Cons)) {
      u_value v_a = u_field(l409, 0);
      if (u_truth(u_mk_bool(1))) {
        e408 = u_append(e408, f_check_46_zonk_type_2(v_su, v_a));
      }
      l409 = u_field(l409, 1);
    }
    t406 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t407);
  }
  if ((!t406) && u_is_con(s405, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s405, 0);
    u_value v_r = u_field(s405, 1);
    u_value t410 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e411 = &t410;
    u_value l412 = v_args;
    while (u_is_con(l412, &n_list_46_Cons)) {
      u_value v_a = u_field(l412, 0);
      if (u_truth(u_mk_bool(1))) {
        e411 = u_append(e411, f_check_46_zonk_type_2(v_su, v_a));
      }
      l412 = u_field(l412, 1);
    }
    t406 = u_mk_con(&n_syntax_46_TFun, 2, t410, f_check_46_zonk_type_2(v_su, v_r));
  }
  if ((!t406) && u_is_con(s405, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s405, 0);
    u_value v_os = u_field(s405, 1);
    u_value v_rs = u_field(s405, 2);
    u_value t413 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e414 = &t413;
    u_value l415 = v_is;
    while (u_is_con(l415, &n_list_46_Cons)) {
      u_value v_a = u_field(l415, 0);
      if (u_truth(u_mk_bool(1))) {
        e414 = u_append(e414, f_check_46_zonk_type_2(v_su, v_a));
      }
      l415 = u_field(l415, 1);
    }
    u_value t416 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e417 = &t416;
    u_value l418 = v_os;
    while (u_is_con(l418, &n_list_46_Cons)) {
      u_value v_a = u_field(l418, 0);
      if (u_truth(u_mk_bool(1))) {
        e417 = u_append(e417, f_check_46_zonk_type_2(v_su, v_a));
      }
      l418 = u_field(l418, 1);
    }
    u_value t419 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e420 = &t419;
    u_value l421 = v_rs;
    while (u_is_con(l421, &n_list_46_Cons)) {
      u_value v_a = u_field(l421, 0);
      if (u_truth(u_mk_bool(1))) {
        e420 = u_append(e420, f_check_46_zonk_type_2(v_su, v_a));
      }
      l421 = u_field(l421, 1);
    }
    t406 = u_mk_con(&n_syntax_46_TProc, 3, t413, t416, t419);
  }
  if ((!t406) && u_is_con(s405, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s405, 0);
    u_value t422 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e423 = &t422;
    u_value l424 = v_ts;
    while (u_is_con(l424, &n_list_46_Cons)) {
      u_value v_a = u_field(l424, 0);
      if (u_truth(u_mk_bool(1))) {
        e423 = u_append(e423, f_check_46_zonk_type_2(v_su, v_a));
      }
      l424 = u_field(l424, 1);
    }
    t406 = u_mk_con(&n_syntax_46_TTuple, 1, t422);
  }
  if (!t406) {
    u_value v_u = s405;
    t406 = v_u;
  }
  if (!t406) {
    u_no_case(s405);
  }
  return t406;
}

u_value f_check_46_zonk_var_2(u_value v_su, u_value v_x) {
  return f_syntax_46_Var_46_typ_58__61__2(v_x, f_check_46_zonk_type_2(v_su, f_syntax_46_Var_46_typ_1(v_x)));
}

u_value f_check_46_zonk_vars_2(u_value v_su, u_value v_xs) {
  u_value t425 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e426 = &t425;
  u_value l427 = v_xs;
  while (u_is_con(l427, &n_list_46_Cons)) {
    u_value v_x = u_field(l427, 0);
    if (u_truth(u_mk_bool(1))) {
      e426 = u_append(e426, f_check_46_zonk_var_2(v_su, v_x));
    }
    l427 = u_field(l427, 1);
  }
  return t425;
}

u_value f_check_46_zonk_expr_2(u_value v_su, u_value v_e) {
  u_value s428 = v_e;
  u_value t429 = NULL;
  if ((!t429) && u_is_con(s428, &n_syntax_46_ELit)) {
    t429 = v_e;
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_EVar)) {
    u_value v_x = u_field(s428, 0);
    t429 = u_mk_con(&n_syntax_46_EVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s428, 0);
    u_value v_args = u_field(s428, 1);
    u_value t430 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e431 = &t430;
    u_value l432 = v_args;
    while (u_is_con(l432, &n_list_46_Cons)) {
      u_value v_a = u_field(l432, 0);
      if (u_truth(u_mk_bool(1))) {
        e431 = u_append(e431, f_check_46_zonk_expr_2(v_su, v_a));
      }
      l432 = u_field(l432, 1);
    }
    t429 = u_mk_con(&n_syntax_46_EApp, 2, f_check_46_zonk_expr_2(v_su, v_f), t430);
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s428, 0);
    u_value v_b = u_field(s428, 1);
    t429 = u_mk_con(&n_syntax_46_ELam, 2, f_check_46_zonk_vars_2(v_su, v_xs), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_EProc)) {
    u_value v_is = u_field(s428, 0);
    u_value v_os = u_field(s428, 1);
    u_value v_rs = u_field(s428, 2);
    u_value v_b = u_field(s428, 3);
    t429 = u_mk_con(&n_syntax_46_EProc, 4, f_check_46_zonk_vars_2(v_su, v_is), f_check_46_zonk_vars_2(v_su, v_os), f_check_46_zonk_vars_2(v_su, v_rs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s428, 0);
    u_value v_b = u_field(s428, 1);
    u_value t433 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e434 = &t433;
    u_value l435 = v_ps;
    while (u_is_con(l435, &n_list_46_Cons)) {
      u_value v_p = u_field(l435, 0);
      if (u_truth(u_mk_bool(1))) {
        e434 = u_append(e434, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l435 = u_field(l435, 1);
    }
    t429 = u_mk_con(&n_syntax_46_ELet, 2, t433, f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s428, 0);
    u_value v_l = u_field(s428, 1);
    u_value v_r = u_field(s428, 2);
    t429 = u_mk_con(&n_syntax_46_EIte, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_expr_2(v_su, v_l), f_check_46_zonk_expr_2(v_su, v_r));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s428, 0);
    u_value v_bs = u_field(s428, 1);
    u_value t436 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e437 = &t436;
    u_value l438 = v_bs;
    while (u_is_con(l438, &n_list_46_Cons)) {
      u_value v_b = u_field(l438, 0);
      if (u_truth(u_mk_bool(1))) {
        e437 = u_append(e437, f_check_46_zonk_expr_branch_2(v_su, v_b));
      }
      l438 = u_field(l438, 1);
    }
    t429 = u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t436);
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s428, 0);
    u_value v_xs = u_field(s428, 1);
    u_value v_ls = u_field(s428, 2);
    u_value v_c = u_field(s428, 3);
    u_value t439 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e440 = &t439;
    u_value l441 = v_ls;
    while (u_is_con(l441, &n_list_46_Cons)) {
      u_value v_l = u_field(l441, 0);
      if (u_truth(u_mk_bool(1))) {
        e440 = u_append(e440, f_check_46_zonk_expr_2(v_su, v_l));
      }
      l441 = u_field(l441, 1);
    }
    t429 = u_mk_con(&n_syntax_46_EComp, 4, f_check_46_zonk_expr_2(v_su, v_r), f_check_46_zonk_vars_2(v_su, v_xs), t439, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s428, 0);
    u_value t442 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e443 = &t442;
    u_value l444 = v_es;
    while (u_is_con(l444, &n_list_46_Cons)) {
      u_value v_e = u_field(l444, 0);
      if (u_truth(u_mk_bool(1))) {
        e443 = u_append(e443, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l444 = u_field(l444, 1);
    }
    t429 = u_mk_con(&n_syntax_46_ETuple, 1, t442);
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s428, 0);
    u_value v_t = u_field(s428, 1);
    t429 = u_mk_con(&n_syntax_46_ERaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t429) {
    u_no_case(s428);
  }
  return t429;
}

u_value f_check_46_zonk_binding_2(u_value v_su, u_value v_p) {
  u_value s445 = v_p;
  u_value t446 = NULL;
  if (!t446) {
    u_value v_x = u_field(s445, 0);
    u_value v_e = u_field(s445, 1);
    t446 = u_mk_tuple(2, f_check_46_zonk_var_2(v_su, v_x), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t446) {
    u_no_case(s445);
  }
  return t446;
}

u_value f_check_46_zonk_expr_branch_2(u_value v_su, u_value v_b) {
  u_value s447 = v_b;
  u_value t448 = NULL;
  if ((!t448) && u_is_con(s447, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s447, 0);
    u_value v_g = u_field(s447, 1);
    u_value v_e = u_field(s447, 2);
    t448 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t448) {
    u_no_case(s447);
  }
  return t448;
}

u_value f_check_46_zonk_pat_2(u_value v_su, u_value v_p) {
  u_value s449 = v_p;
  u_value t450 = NULL;
  if ((!t450) && u_is_con(s449, &n_syntax_46_PVar)) {
    u_value v_x = u_field(s449, 0);
    t450 = u_mk_con(&n_syntax_46_PVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t450) && u_is_con(s449, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s449, 0);
    u_value v_args = u_field(s449, 1);
    u_value t451 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e452 = &t451;
    u_value l453 = v_args;
    while (u_is_con(l453, &n_list_46_Cons)) {
      u_value v_a = u_field(l453, 0);
      if (u_truth(u_mk_bool(1))) {
        e452 = u_append(e452, f_check_46_zonk_pat_2(v_su, v_a));
      }
      l453 = u_field(l453, 1);
    }
    t450 = u_mk_con(&n_syntax_46_PApp, 2, v_n, t451);
  }
  if ((!t450) && u_is_con(s449, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s449, 0);
    u_value t454 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e455 = &t454;
    u_value l456 = v_ps;
    while (u_is_con(l456, &n_list_46_Cons)) {
      u_value v_q = u_field(l456, 0);
      if (u_truth(u_mk_bool(1))) {
        e455 = u_append(e455, f_check_46_zonk_pat_2(v_su, v_q));
      }
      l456 = u_field(l456, 1);
    }
    t450 = u_mk_con(&n_syntax_46_PTuple, 1, t454);
  }
  if (!t450) {
    t450 = v_p;
  }
  if (!t450) {
    u_no_case(s449);
  }
  return t450;
}

u_value f_check_46_zonk_stmt_2(u_value v_su, u_value v_x) {
  u_value s457 = v_x;
  u_value t458 = NULL;
  if ((!t458) && u_is_con(s457, &n_syntax_46_SSkip)) {
    t458 = v_x;
  }
  if ((!t458) && u_is_con(s457, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s457, 0);
    u_value t459 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e460 = &t459;
    u_value l461 = v_ps;
    while (u_is_con(l461, &n_list_46_Cons)) {
      u_value v_p = u_field(l461, 0);
      if (u_truth(u_mk_bool(1))) {
        e460 = u_append(e460, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l461 = u_field(l461, 1);
    }
    t458 = u_mk_con(&n_syntax_46_SAssign, 1, t459);
  }
  if ((!t458) && u_is_con(s457, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s457, 0);
    u_value v_ys = u_field(s457, 1);
    u_value t462 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e463 = &t462;
    u_value l464 = v_ys;
    while (u_is_con(l464, &n_list_46_Cons)) {
      u_value v_y = u_field(l464, 0);
      if (u_truth(u_mk_bool(1))) {
        e463 = u_append(e463, f_check_46_zonk_stmt_2(v_su, v_y));
      }
      l464 = u_field(l464, 1);
    }
    t458 = u_mk_con(&n_syntax_46_SBlock, 2, f_check_46_zonk_vars_2(v_su, v_xs), t462);
  }
  if ((!t458) && u_is_con(s457, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s457, 0);
    u_value v_l = u_field(s457, 1);
    u_value v_r = u_field(s457, 2);
    t458 = u_mk_con(&n_syntax_46_SIf, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_l), f_check_46_zonk_stmt_2(v_su, v_r));
  }
  if ((!t458) && u_is_con(s457, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s457, 0);
    u_value v_b = u_field(s457, 1);
    t458 = u_mk_con(&n_syntax_46_SWhile, 2, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t458) && u_is_con(s457, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s457, 0);
    u_value v_ins = u_field(s457, 1);
    u_value v_outs = u_field(s457, 2);
    u_value v_refs = u_field(s457, 3);
    u_value t465 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e466 = &t465;
    u_value l467 = v_ins;
    while (u_is_con(l467, &n_list_46_Cons)) {
      u_value v_e = u_field(l467, 0);
      if (u_truth(u_mk_bool(1))) {
        e466 = u_append(e466, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l467 = u_field(l467, 1);
    }
    t458 = u_mk_con(&n_syntax_46_SCall, 4, v_p, t465, f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs));
  }
  if ((!t458) && u_is_con(s457, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s457, 0);
    u_value v_bs = u_field(s457, 1);
    u_value t468 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e469 = &t468;
    u_value l470 = v_bs;
    while (u_is_con(l470, &n_list_46_Cons)) {
      u_value v_b = u_field(l470, 0);
      if (u_truth(u_mk_bool(1))) {
        e469 = u_append(e469, f_check_46_zonk_stmt_branch_2(v_su, v_b));
      }
      l470 = u_field(l470, 1);
    }
    t458 = u_mk_con(&n_syntax_46_SMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t468);
  }
  if ((!t458) && u_is_con(s457, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s457, 0);
    t458 = u_mk_con(&n_syntax_46_SAssert, 1, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t458) && u_is_con(s457, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s457, 0);
    u_value v_t = u_field(s457, 1);
    t458 = u_mk_con(&n_syntax_46_SRaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if ((!t458) && u_is_con(s457, &n_syntax_46_STry)) {
    u_value v_b = u_field(s457, 0);
    u_value v_hs = u_field(s457, 1);
    u_value t471 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e472 = &t471;
    u_value l473 = v_hs;
    while (u_is_con(l473, &n_list_46_Cons)) {
      u_value v_h = u_field(l473, 0);
      if (u_truth(u_mk_bool(1))) {
        e472 = u_append(e472, f_check_46_zonk_handler_2(v_su, v_h));
      }
      l473 = u_field(l473, 1);
    }
    t458 = u_mk_con(&n_syntax_46_STry, 2, f_check_46_zonk_stmt_2(v_su, v_b), t471);
  }
  if (!t458) {
    u_no_case(s457);
  }
  return t458;
}

u_value f_check_46_zonk_stmt_branch_2(u_value v_su, u_value v_b) {
  u_value s474 = v_b;
  u_value t475 = NULL;
  if ((!t475) && u_is_con(s474, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s474, 0);
    u_value v_g = u_field(s474, 1);
    u_value v_y = u_field(s474, 2);
    t475 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_y));
  }
  if (!t475) {
    u_no_case(s474);
  }
  return t475;
}

u_value f_check_46_zonk_handler_2(u_value v_su, u_value v_h) {
  u_value s476 = v_h;
  u_value t477 = NULL;
  if ((!t477) && u_is_con(s476, &n_syntax_46_Handler)) {
    u_value v_t = u_field(s476, 0);
    u_value v_p = u_field(s476, 1);
    u_value v_g = u_field(s476, 2);
    u_value v_b = u_field(s476, 3);
    t477 = u_mk_con(&n_syntax_46_Handler, 4, f_check_46_zonk_type_2(v_su, v_t), f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t477) {
    u_no_case(s476);
  }
  return t477;
}

u_value f_check_46_zonk_cmd_2(u_value v_su, u_value v_c) {
  u_value s478 = v_c;
  u_value t479 = NULL;
  if ((!t479) && u_is_con(s478, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s478, 0);
    u_value v_ps = u_field(s478, 1);
    u_value v_xss = u_field(s478, 2);
    u_value v_r = u_field(s478, 3);
    u_value v_b = u_field(s478, 4);
    u_value t480 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e481 = &t480;
    u_value l482 = v_xss;
    while (u_is_con(l482, &n_list_46_Cons)) {
      u_value v_xs = u_field(l482, 0);
      if (u_truth(u_mk_bool(1))) {
        e481 = u_append(e481, f_check_46_zonk_vars_2(v_su, v_xs));
      }
      l482 = u_field(l482, 1);
    }
    t479 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, t480, f_check_46_zonk_type_2(v_su, v_r), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t479) && u_is_con(s478, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s478, 0);
    u_value v_ps = u_field(s478, 1);
    u_value v_ins = u_field(s478, 2);
    u_value v_outs = u_field(s478, 3);
    u_value v_refs = u_field(s478, 4);
    u_value v_b = u_field(s478, 5);
    t479 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, f_check_46_zonk_vars_2(v_su, v_ins), f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t479) && u_is_con(s478, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s478, 0);
    t479 = u_mk_con(&n_syntax_46_CEval, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t479) && u_is_con(s478, &n_syntax_46_CRun)) {
    u_value v_n = u_field(s478, 0);
    u_value v_b = u_field(s478, 1);
    t479 = u_mk_con(&n_syntax_46_CRun, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t479) && u_is_con(s478, &n_syntax_46_CTest)) {
    u_value v_n = u_field(s478, 0);
    u_value v_b = u_field(s478, 1);
    t479 = u_mk_con(&n_syntax_46_CTest, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t479) {
    t479 = v_c;
  }
  if (!t479) {
    u_no_case(s478);
  }
  return t479;
}

u_value f_check_46_unknown_type_1(u_value v_t) {
  u_value t483;
  if (u_truth(u_eq(f_check_46_free_1(v_t), u_mk_con(&n_list_46_Nil, 0)))) {
    t483 = u_mk_con(&n_option_46_None, 0);
  } else {
    t483 = u_mk_con(&n_option_46_Some, 1, v_t);
  }
  return t483;
}

u_value f_check_46_unknown_in_1(u_value v_x) {
  u_value s484 = v_x;
  u_value t485 = NULL;
  if ((!t485) && u_is_con(s484, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s484, 0);
    t485 = f_check_46_unknown_in_exprs_1(f_list_46_seconds_1(v_ps));
  }
  if ((!t485) && u_is_con(s484, &n_syntax_46_SBlock)) {
    u_value v_ys = u_field(s484, 1);
    t485 = f_check_46_unknown_in_all_1(v_ys);
  }
  if ((!t485) && u_is_con(s484, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s484, 0);
    u_value v_l = u_field(s484, 1);
    u_value v_r = u_field(s484, 2);
    t485 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_option_46_or_else_2(f_check_46_unknown_in_1(v_l), f_check_46_unknown_in_1(v_r)));
  }
  if ((!t485) && u_is_con(s484, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s484, 0);
    u_value v_b = u_field(s484, 1);
    t485 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_check_46_unknown_in_1(v_b));
  }
  if ((!t485) && u_is_con(s484, &n_syntax_46_SCall)) {
    u_value v_ins = u_field(s484, 1);
    t485 = f_check_46_unknown_in_exprs_1(v_ins);
  }
  if ((!t485) && u_is_con(s484, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s484, 0);
    u_value v_bs = u_field(s484, 1);
    t485 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_a), f_check_46_unknown_in_branches_1(v_bs));
  }
  if ((!t485) && u_is_con(s484, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s484, 0);
    t485 = f_check_46_unknown_in_expr_1(v_c);
  }
  if ((!t485) && u_is_con(s484, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s484, 0);
    u_value v_t = u_field(s484, 1);
    t485 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t485) && u_is_con(s484, &n_syntax_46_STry)) {
    u_value v_b = u_field(s484, 0);
    u_value v_hs = u_field(s484, 1);
    t485 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_hs));
  }
  if (!t485) {
    t485 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t485) {
    u_no_case(s484);
  }
  return t485;
}

u_value f_check_46_unknown_in_expr_1(u_value v_e) {
  u_value s486 = v_e;
  u_value t487 = NULL;
  if ((!t487) && u_is_con(s486, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s486, 0);
    u_value v_t = u_field(s486, 1);
    t487 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t487) && u_is_con(s486, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s486, 0);
    u_value v_args = u_field(s486, 1);
    t487 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_f, v_args));
  }
  if ((!t487) && u_is_con(s486, &n_syntax_46_ELam)) {
    u_value v_b = u_field(s486, 1);
    t487 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t487) && u_is_con(s486, &n_syntax_46_EProc)) {
    u_value v_b = u_field(s486, 3);
    t487 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t487) && u_is_con(s486, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s486, 0);
    u_value v_b = u_field(s486, 1);
    t487 = f_check_46_unknown_in_exprs_1(f_list_46__43__43__2(f_list_46_seconds_1(v_ps), u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t487) && u_is_con(s486, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s486, 0);
    u_value v_l = u_field(s486, 1);
    u_value v_r = u_field(s486, 2);
    t487 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)))));
  }
  if ((!t487) && u_is_con(s486, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s486, 0);
    u_value v_bs = u_field(s486, 1);
    u_value t488 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e489 = &t488;
    u_value l490 = v_bs;
    while (u_is_con(l490, &n_list_46_Cons)) {
      u_value v_b = u_field(l490, 0);
      if (u_truth(u_mk_bool(1))) {
        e489 = u_append(e489, f_check_46_branch_exprs_1(v_b));
      }
      l490 = u_field(l490, 1);
    }
    t487 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_a, f_list_46_concat_1(t488)));
  }
  if ((!t487) && u_is_con(s486, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s486, 0);
    u_value v_ls = u_field(s486, 2);
    u_value v_c = u_field(s486, 3);
    t487 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Cons, 2, v_c, v_ls)));
  }
  if ((!t487) && u_is_con(s486, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s486, 0);
    t487 = f_check_46_unknown_in_exprs_1(v_es);
  }
  if (!t487) {
    t487 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t487) {
    u_no_case(s486);
  }
  return t487;
}

u_value f_check_46_unknown_in_exprs_1(u_value v_es) {
  u_value s491 = v_es;
  u_value t492 = NULL;
  if ((!t492) && u_is_con(s491, &n_list_46_Nil)) {
    t492 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t492) && u_is_con(s491, &n_list_46_Cons)) {
    u_value v_e = u_field(s491, 0);
    u_value v_rest = u_field(s491, 1);
    t492 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_e), f_check_46_unknown_in_exprs_1(v_rest));
  }
  if (!t492) {
    u_no_case(s491);
  }
  return t492;
}

u_value f_check_46_branch_exprs_1(u_value v_b) {
  u_value s493 = v_b;
  u_value t494 = NULL;
  if ((!t494) && (u_is_con(s493, &n_syntax_46_Branch) && u_is_con(u_field(s493, 1), &n_option_46_Some))) {
    u_value v_g = u_field(u_field(s493, 1), 0);
    u_value v_e = u_field(s493, 2);
    t494 = u_mk_con(&n_list_46_Cons, 2, v_g, u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t494) && (u_is_con(s493, &n_syntax_46_Branch) && u_is_con(u_field(s493, 1), &n_option_46_None))) {
    u_value v_e = u_field(s493, 2);
    t494 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t494) {
    u_no_case(s493);
  }
  return t494;
}

u_value f_check_46_unknown_in_all_1(u_value v_xs) {
  u_value s495 = v_xs;
  u_value t496 = NULL;
  if ((!t496) && u_is_con(s495, &n_list_46_Nil)) {
    t496 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t496) && u_is_con(s495, &n_list_46_Cons)) {
    u_value v_y = u_field(s495, 0);
    u_value v_ys = u_field(s495, 1);
    t496 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_all_1(v_ys));
  }
  if (!t496) {
    u_no_case(s495);
  }
  return t496;
}

u_value f_check_46_unknown_in_branches_1(u_value v_bs) {
  u_value s497 = v_bs;
  u_value t498 = NULL;
  if ((!t498) && u_is_con(s497, &n_list_46_Nil)) {
    t498 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t498) && (u_is_con(s497, &n_list_46_Cons) && (u_is_con(u_field(s497, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s497, 0), 1), &n_option_46_Some)))) {
    u_value v_g = u_field(u_field(u_field(s497, 0), 1), 0);
    u_value v_y = u_field(u_field(s497, 0), 2);
    u_value v_rest = u_field(s497, 1);
    t498 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_g), f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest)));
  }
  if ((!t498) && (u_is_con(s497, &n_list_46_Cons) && (u_is_con(u_field(s497, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s497, 0), 1), &n_option_46_None)))) {
    u_value v_y = u_field(u_field(s497, 0), 2);
    u_value v_rest = u_field(s497, 1);
    t498 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest));
  }
  if (!t498) {
    u_no_case(s497);
  }
  return t498;
}

u_value f_check_46_unknown_in_handlers_1(u_value v_hs) {
  u_value s499 = v_hs;
  u_value t500 = NULL;
  if ((!t500) && u_is_con(s499, &n_list_46_Nil)) {
    t500 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t500) && (u_is_con(s499, &n_list_46_Cons) && (u_is_con(u_field(s499, 0), &n_syntax_46_Handler) && u_is_con(u_field(u_field(s499, 0), 1), &n_syntax_46_PWild)))) {
    u_value v_b = u_field(u_field(s499, 0), 3);
    u_value v_rest = u_field(s499, 1);
    t500 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest));
  }
  if ((!t500) && (u_is_con(s499, &n_list_46_Cons) && u_is_con(u_field(s499, 0), &n_syntax_46_Handler))) {
    u_value v_t = u_field(u_field(s499, 0), 0);
    u_value v_b = u_field(u_field(s499, 0), 3);
    u_value v_rest = u_field(s499, 1);
    t500 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest)));
  }
  if (!t500) {
    u_no_case(s499);
  }
  return t500;
}

u_value f_check_46_unknown_in_cmd_1(u_value v_c) {
  u_value s501 = v_c;
  u_value t502 = NULL;
  if ((!t502) && u_is_con(s501, &n_syntax_46_CFun)) {
    u_value v_b = u_field(s501, 4);
    t502 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t502) && u_is_con(s501, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s501, 0);
    t502 = f_check_46_unknown_in_expr_1(v_e);
  }
  if ((!t502) && u_is_con(s501, &n_syntax_46_CProc)) {
    u_value v_b = u_field(s501, 5);
    t502 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t502) && u_is_con(s501, &n_syntax_46_CRun)) {
    u_value v_b = u_field(s501, 1);
    t502 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t502) && u_is_con(s501, &n_syntax_46_CTest)) {
    u_value v_b = u_field(s501, 1);
    t502 = f_check_46_unknown_in_1(v_b);
  }
  if (!t502) {
    t502 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t502) {
    u_no_case(s501);
  }
  return t502;
}

u_value f_check_46_head_of_1(u_value v_p) {
  u_value s503 = v_p;
  u_value t504 = NULL;
  if ((!t504) && u_is_con(s503, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s503, 0);
    u_value v_args = u_field(s503, 1);
    t504 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_n), v_args));
  }
  if ((!t504) && u_is_con(s503, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s503, 0);
    t504 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ps));
  }
  if ((!t504) && u_is_con(s503, &n_syntax_46_PLit)) {
    u_value v_v = u_field(s503, 0);
    t504 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, v_v), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t504) {
    t504 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t504) {
    u_no_case(s503);
  }
  return t504;
}

u_value f_check_46_heads_1(u_value v_rows) {
  u_value s505 = v_rows;
  u_value t506 = NULL;
  if ((!t506) && u_is_con(s505, &n_list_46_Nil)) {
    t506 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t506) && (u_is_con(s505, &n_list_46_Cons) && u_is_con(u_field(s505, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s505, 0), 0);
    u_value v_rest = u_field(s505, 1);
    u_value s507 = f_check_46_head_of_1(v_p);
    u_value t508 = NULL;
    if ((!t508) && u_is_con(s507, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s507, 0), 0);
      t508 = u_mk_con(&n_list_46_Cons, 2, v_h, f_check_46_heads_1(v_rest));
    }
    if ((!t508) && u_is_con(s507, &n_option_46_None)) {
      t508 = f_check_46_heads_1(v_rest);
    }
    if (!t508) {
      u_no_case(s507);
    }
    t506 = t508;
  }
  if ((!t506) && (u_is_con(s505, &n_list_46_Cons) && u_is_con(u_field(s505, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s505, 1);
    t506 = f_check_46_heads_1(v_rest);
  }
  if (!t506) {
    u_no_case(s505);
  }
  return t506;
}

u_value f_check_46_field_types_3(u_value v_c, u_value v_k, u_value v_args) {
  u_value s509 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t510 = NULL;
  if ((!t510) && u_is_con(s509, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s509, 0), 0);
    u_value v_ks = u_field(u_field(s509, 0), 1);
    u_value t511 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e512 = &t511;
    u_value l513 = v_ks;
    while (u_is_con(l513, &n_list_46_Cons)) {
      u_value v_a = u_field(l513, 0);
      if (u_truth(u_mk_bool(1))) {
        e512 = u_append(e512, f_check_46_subst_2(v_a, f_list_46_zip_2(v_ps, v_args)));
      }
      l513 = u_field(l513, 1);
    }
    t510 = t511;
  }
  if ((!t510) && u_is_con(s509, &n_option_46_None)) {
    t510 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t510) {
    u_no_case(s509);
  }
  return t510;
}

u_value f_check_46_constructors_of_3(u_value v_c, u_value v_su, u_value v_t) {
  u_value s514 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t515 = NULL;
  if ((!t515) && (u_is_con(s514, &n_syntax_46_TSort) && (u_is_con(u_field(s514, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s514, 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(s514, 0), 1), u_mk_lit("Bool")) && u_is_con(u_field(s514, 1), &n_list_46_Nil)))))) {
    t515 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t515) && u_is_con(s514, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s514, 0);
    t515 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ts), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t515) && u_is_con(s514, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s514, 0);
    u_value v_args = u_field(s514, 1);
    u_value s516 = f_ctx_46_members_of_2(v_c, v_n);
    u_value t517 = NULL;
    if ((!t517) && u_is_con(s516, &n_option_46_Some)) {
      u_value v_ks = u_field(s516, 0);
      u_value t518 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e519 = &t518;
      u_value l520 = v_ks;
      while (u_is_con(l520, &n_list_46_Cons)) {
        u_value v_k = u_field(l520, 0);
        if (u_truth(u_mk_bool(1))) {
          e519 = u_append(e519, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_k), f_check_46_field_types_3(v_c, v_k, v_args)));
        }
        l520 = u_field(l520, 1);
      }
      t517 = u_mk_con(&n_option_46_Some, 1, t518);
    }
    if ((!t517) && u_is_con(s516, &n_option_46_None)) {
      t517 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t517) {
      u_no_case(s516);
    }
    t515 = t517;
  }
  if (!t515) {
    t515 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t515) {
    u_no_case(s514);
  }
  return t515;
}

u_value f_check_46_fields_of_4(u_value v_c, u_value v_su, u_value v_t, u_value v_h) {
  u_value s521 = f_check_46_constructors_of_3(v_c, v_su, v_t);
  u_value t522 = NULL;
  if ((!t522) && u_is_con(s521, &n_option_46_Some)) {
    u_value v_ks = u_field(s521, 0);
    u_value s523 = f_list_46_lookup_2(v_h, v_ks);
    u_value t524 = NULL;
    if ((!t524) && u_is_con(s523, &n_option_46_Some)) {
      u_value v_ts = u_field(s523, 0);
      t524 = v_ts;
    }
    if ((!t524) && u_is_con(s523, &n_option_46_None)) {
      t524 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t524) {
      u_no_case(s523);
    }
    t522 = t524;
  }
  if ((!t522) && u_is_con(s521, &n_option_46_None)) {
    t522 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t522) {
    u_no_case(s521);
  }
  return t522;
}

u_value f_check_46_covers_2(u_value v_hs, u_value v_ks) {
  u_value s525 = v_ks;
  u_value t526 = NULL;
  if ((!t526) && u_is_con(s525, &n_list_46_Nil)) {
    t526 = u_mk_bool(1);
  }
  if ((!t526) && u_is_con(s525, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s525, 0), 0);
    u_value v_rest = u_field(s525, 1);
    u_value t527 = f_list_46_contains_2(v_h, v_hs);
    if (u_truth(t527)) {
      t527 = f_check_46_covers_2(v_hs, v_rest);
    }
    t526 = t527;
  }
  if (!t526) {
    u_no_case(s525);
  }
  return t526;
}

u_value f_check_46_defaults_1(u_value v_rows) {
  u_value s528 = v_rows;
  u_value t529 = NULL;
  if ((!t529) && u_is_con(s528, &n_list_46_Nil)) {
    t529 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t529) && (u_is_con(s528, &n_list_46_Cons) && u_is_con(u_field(s528, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s528, 0), 0);
    u_value v_ps = u_field(u_field(s528, 0), 1);
    u_value v_rest = u_field(s528, 1);
    u_value s530 = f_check_46_head_of_1(v_p);
    u_value t531 = NULL;
    if ((!t531) && u_is_con(s530, &n_option_46_Some)) {
      t531 = f_check_46_defaults_1(v_rest);
    }
    if ((!t531) && u_is_con(s530, &n_option_46_None)) {
      t531 = u_mk_con(&n_list_46_Cons, 2, v_ps, f_check_46_defaults_1(v_rest));
    }
    if (!t531) {
      u_no_case(s530);
    }
    t529 = t531;
  }
  if ((!t529) && (u_is_con(s528, &n_list_46_Cons) && u_is_con(u_field(s528, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s528, 1);
    t529 = f_check_46_defaults_1(v_rest);
  }
  if (!t529) {
    u_no_case(s528);
  }
  return t529;
}

u_value f_check_46_specialize_3(u_value v_rows, u_value v_h, u_value v_n) {
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
      u_value v_g = u_field(u_field(s534, 0), 0);
      u_value v_args = u_field(u_field(s534, 0), 1);
      u_value t536;
      if (u_truth(u_eq(v_g, v_h))) {
        t536 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(v_args, v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
      } else {
        t536 = f_check_46_specialize_3(v_rest, v_h, v_n);
      }
      t535 = t536;
    }
    if ((!t535) && u_is_con(s534, &n_option_46_None)) {
      t535 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(f_list_46_replicate_2(v_n, u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
    }
    if (!t535) {
      u_no_case(s534);
    }
    t533 = t535;
  }
  if ((!t533) && (u_is_con(s532, &n_list_46_Cons) && u_is_con(u_field(s532, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s532, 1);
    t533 = f_check_46_specialize_3(v_rest, v_h, v_n);
  }
  if (!t533) {
    u_no_case(s532);
  }
  return t533;
}

u_value f_check_46_useful_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_q, u_value v_ts) {
  u_value s537 = u_mk_tuple(2, v_q, v_ts);
  u_value t538 = NULL;
  if ((!t538) && u_is_con(u_field(s537, 0), &n_list_46_Nil)) {
    t538 = u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t538) && (u_is_con(u_field(s537, 0), &n_list_46_Cons) && u_is_con(u_field(s537, 1), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s537, 0), 0);
    u_value v_ps = u_field(u_field(s537, 0), 1);
    u_value v_t = u_field(u_field(s537, 1), 0);
    u_value v_rest = u_field(u_field(s537, 1), 1);
    u_value s539 = f_check_46_head_of_1(v_p);
    u_value t540 = NULL;
    if ((!t540) && u_is_con(s539, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s539, 0), 0);
      u_value v_args = u_field(u_field(s539, 0), 1);
      t540 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_ps), f_list_46__43__43__2(f_check_46_fields_of_4(v_c, v_su, v_t, v_h), v_rest));
    }
    if ((!t540) && u_is_con(s539, &n_option_46_None)) {
      u_value s541 = f_check_46_constructors_of_3(v_c, v_su, v_t);
      u_value t542 = NULL;
      if ((!t542) && u_is_con(s541, &n_option_46_Some)) {
        u_value v_ks = u_field(s541, 0);
        u_value t543;
        if (u_truth(f_check_46_covers_2(f_check_46_heads_1(v_rows), v_ks))) {
          t543 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_ks, v_ps, v_rest);
        } else {
          t543 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
        }
        t542 = t543;
      }
      if ((!t542) && u_is_con(s541, &n_option_46_None)) {
        t542 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
      }
      if (!t542) {
        u_no_case(s541);
      }
      t540 = t542;
    }
    if (!t540) {
      u_no_case(s539);
    }
    t538 = t540;
  }
  if ((!t538) && (u_is_con(u_field(s537, 0), &n_list_46_Cons) && u_is_con(u_field(s537, 1), &n_list_46_Nil))) {
    t538 = u_mk_bool(0);
  }
  if (!t538) {
    u_no_case(s537);
  }
  return t538;
}

u_value f_check_46_useful_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_ps, u_value v_rest) {
  u_value s544 = v_ks;
  u_value t545 = NULL;
  if ((!t545) && u_is_con(s544, &n_list_46_Nil)) {
    t545 = u_mk_bool(0);
  }
  if ((!t545) && u_is_con(s544, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s544, 0), 0);
    u_value v_args = u_field(u_field(s544, 0), 1);
    u_value v_more = u_field(s544, 1);
    u_value t546 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_list_46__43__43__2(v_args, v_rest));
    if (!u_truth(t546)) {
      t546 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_more, v_ps, v_rest);
    }
    t545 = t546;
  }
  if (!t545) {
    u_no_case(s544);
  }
  return t545;
}

u_value f_check_46_witness_2(u_value v_h, u_value v_args) {
  u_value s547 = v_h;
  u_value t548 = NULL;
  if ((!t548) && u_is_con(s547, &n_check_46_HCon)) {
    u_value v_n = u_field(s547, 0);
    u_value t549;
    if (u_truth(u_eq(v_n, f_syntax_46_tuple_name_0()))) {
      t549 = u_mk_con(&n_syntax_46_PTuple, 1, v_args);
    } else {
      t549 = u_mk_con(&n_syntax_46_PApp, 2, v_n, v_args);
    }
    t548 = t549;
  }
  if ((!t548) && u_is_con(s547, &n_check_46_HLit)) {
    u_value v_v = u_field(s547, 0);
    t548 = u_mk_con(&n_syntax_46_PLit, 1, v_v);
  }
  if (!t548) {
    u_no_case(s547);
  }
  return t548;
}

u_value f_check_46_missing_4(u_value v_c, u_value v_su, u_value v_rows, u_value v_ts) {
  u_value s550 = v_ts;
  u_value t551 = NULL;
  if ((!t551) && u_is_con(s550, &n_list_46_Nil)) {
    u_value t552;
    if (u_truth(u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0)))) {
      t552 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
    } else {
      t552 = u_mk_con(&n_option_46_None, 0);
    }
    t551 = t552;
  }
  if ((!t551) && u_is_con(s550, &n_list_46_Cons)) {
    u_value v_t = u_field(s550, 0);
    u_value v_rest = u_field(s550, 1);
    t551 = f_check_46_missing_in_6(v_c, v_su, v_rows, f_check_46_heads_1(v_rows), f_check_46_constructors_of_3(v_c, v_su, v_t), v_rest);
  }
  if (!t551) {
    u_no_case(s550);
  }
  return t551;
}

u_value f_check_46_missing_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s553 = v_cs;
  u_value t554 = NULL;
  if ((!t554) && u_is_con(s553, &n_option_46_Some)) {
    u_value v_ks = u_field(s553, 0);
    u_value t555;
    if (u_truth(f_check_46_covers_2(v_hs, v_ks))) {
      t555 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_ks, v_rest);
    } else {
      t555 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
    }
    t554 = t555;
  }
  if ((!t554) && u_is_con(s553, &n_option_46_None)) {
    t554 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
  }
  if (!t554) {
    u_no_case(s553);
  }
  return t554;
}

u_value f_check_46_first_missing_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_rest) {
  u_value s556 = v_ks;
  u_value t557 = NULL;
  if ((!t557) && u_is_con(s556, &n_list_46_Nil)) {
    t557 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t557) && u_is_con(s556, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s556, 0), 0);
    u_value v_args = u_field(u_field(s556, 0), 1);
    u_value v_more = u_field(s556, 1);
    u_value s558 = f_check_46_missing_4(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_rest));
    u_value t559 = NULL;
    if ((!t559) && u_is_con(s558, &n_option_46_Some)) {
      u_value v_ws = u_field(s558, 0);
      t559 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_witness_2(v_h, f_list_46_take_2(f_list_46_length_1(v_args), v_ws)), f_list_46_drop_2(f_list_46_length_1(v_args), v_ws)));
    }
    if ((!t559) && u_is_con(s558, &n_option_46_None)) {
      t559 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_more, v_rest);
    }
    if (!t559) {
      u_no_case(s558);
    }
    t557 = t559;
  }
  if (!t557) {
    u_no_case(s556);
  }
  return t557;
}

u_value f_check_46_other_missing_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s560 = f_check_46_missing_4(v_c, v_su, f_check_46_defaults_1(v_rows), v_rest);
  u_value t561 = NULL;
  if ((!t561) && u_is_con(s560, &n_option_46_Some)) {
    u_value v_ws = u_field(s560, 0);
    t561 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_uncovered_2(v_hs, v_cs), v_ws));
  }
  if ((!t561) && u_is_con(s560, &n_option_46_None)) {
    t561 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t561) {
    u_no_case(s560);
  }
  return t561;
}

u_value f_check_46_uncovered_2(u_value v_hs, u_value v_cs) {
  u_value s562 = v_cs;
  u_value t563 = NULL;
  if ((!t563) && u_is_con(s562, &n_option_46_Some)) {
    u_value v_ks = u_field(s562, 0);
    u_value t564;
    if (u_truth(u_eq(v_hs, u_mk_con(&n_list_46_Nil, 0)))) {
      t564 = u_mk_con(&n_syntax_46_PWild, 0);
    } else {
      t564 = f_check_46_first_uncovered_2(v_hs, v_ks);
    }
    t563 = t564;
  }
  if ((!t563) && u_is_con(s562, &n_option_46_None)) {
    t563 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if (!t563) {
    u_no_case(s562);
  }
  return t563;
}

u_value f_check_46_first_uncovered_2(u_value v_hs, u_value v_ks) {
  u_value s565 = v_ks;
  u_value t566 = NULL;
  if ((!t566) && u_is_con(s565, &n_list_46_Nil)) {
    t566 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if ((!t566) && u_is_con(s565, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s565, 0), 0);
    u_value v_args = u_field(u_field(s565, 0), 1);
    u_value v_rest = u_field(s565, 1);
    u_value t567;
    if (u_truth(f_list_46_contains_2(v_h, v_hs))) {
      t567 = f_check_46_first_uncovered_2(v_hs, v_rest);
    } else {
      t567 = f_check_46_witness_2(v_h, f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)));
    }
    t566 = t567;
  }
  if (!t566) {
    u_no_case(s565);
  }
  return t566;
}

void p_check_46_fresh(u_value *r_t, u_value *r_s) {
  {
    u_value t568 = u_mk_con(&n_syntax_46_TMeta, 1, u_add(f_check_46_State_46_count_1(*r_s), u_mk_int(1)));
    u_value t569 = f_check_46_State_46_count_58__61__2(*r_s, u_add(f_check_46_State_46_count_1(*r_s), u_mk_int(1)));
    *r_t = t568;
    *r_s = t569;
  }
}

void p_check_46_bind_meta(u_value v_i, u_value v_t, u_value *r_s) {
  {
    if (u_truth(f_check_46_occurs_3(f_check_46_State_46_sub_1(*r_s), v_i, v_t))) {
      {
        u_raise(u_mk_con(&n_check_46_InfiniteType, 2, u_mk_con(&n_syntax_46_TMeta, 1, v_i), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t)), "check.TypeError");
      }
    }
    u_value t570 = f_check_46_State_46_sub_58__61__2(*r_s, f_map_46_put_3(v_i, v_t, f_check_46_State_46_sub_1(*r_s)));
    *r_s = t570;
  }
}

void p_check_46_unify(u_value v_t1, u_value v_t2, u_value *r_s) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value t571 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t1);
    v_a = t571;
    u_value t572 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t2);
    v_b = t572;
    u_value s573 = u_mk_tuple(2, v_a, v_b);
    int d574 = 0;
    if ((!d574) && (u_is_con(u_field(s573, 0), &n_syntax_46_TMeta) && u_is_con(u_field(s573, 1), &n_syntax_46_TMeta))) {
      u_value v_i = u_field(u_field(s573, 0), 0);
      u_value v_j = u_field(u_field(s573, 1), 0);
      d574 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_i, v_j))) {
          {
            p_check_46_bind_meta(v_i, v_b, r_s);
          }
        }
      }
    }
    if ((!d574) && u_is_con(u_field(s573, 0), &n_syntax_46_TMeta)) {
      u_value v_i = u_field(u_field(s573, 0), 0);
      d574 = 1;
      {
        p_check_46_bind_meta(v_i, v_b, r_s);
      }
    }
    if ((!d574) && u_is_con(u_field(s573, 1), &n_syntax_46_TMeta)) {
      u_value v_j = u_field(u_field(s573, 1), 0);
      d574 = 1;
      {
        p_check_46_bind_meta(v_j, v_a, r_s);
      }
    }
    if ((!d574) && (u_is_con(u_field(s573, 0), &n_syntax_46_TParam) && u_is_con(u_field(s573, 1), &n_syntax_46_TParam))) {
      u_value v_x = u_field(u_field(s573, 0), 0);
      u_value v_y = u_field(u_field(s573, 1), 0);
      d574 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_x, v_y))) {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, v_a, v_b), "check.TypeError");
          }
        }
      }
    }
    if ((!d574) && (u_is_con(u_field(s573, 0), &n_syntax_46_TSort) && u_is_con(u_field(s573, 1), &n_syntax_46_TSort))) {
      u_value v_f = u_field(u_field(s573, 0), 0);
      u_value v_xs = u_field(u_field(s573, 0), 1);
      u_value v_g = u_field(u_field(s573, 1), 0);
      u_value v_ys = u_field(u_field(s573, 1), 1);
      d574 = 1;
      {
        u_value t575 = u_eq(v_f, v_g);
        if (u_truth(t575)) {
          t575 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys));
        }
        if (u_truth(t575)) {
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
    if ((!d574) && (u_is_con(u_field(s573, 0), &n_syntax_46_TFun) && u_is_con(u_field(s573, 1), &n_syntax_46_TFun))) {
      u_value v_xs = u_field(u_field(s573, 0), 0);
      u_value v_r1 = u_field(u_field(s573, 0), 1);
      u_value v_ys = u_field(u_field(s573, 1), 0);
      u_value v_r2 = u_field(u_field(s573, 1), 1);
      d574 = 1;
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
    if ((!d574) && (u_is_con(u_field(s573, 0), &n_syntax_46_TTuple) && u_is_con(u_field(s573, 1), &n_syntax_46_TTuple))) {
      u_value v_xs = u_field(u_field(s573, 0), 0);
      u_value v_ys = u_field(u_field(s573, 1), 0);
      d574 = 1;
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
    if ((!d574) && (u_is_con(u_field(s573, 0), &n_syntax_46_TProc) && u_is_con(u_field(s573, 1), &n_syntax_46_TProc))) {
      u_value v_xs = u_field(u_field(s573, 0), 0);
      u_value v_ys = u_field(u_field(s573, 0), 1);
      u_value v_zs = u_field(u_field(s573, 0), 2);
      u_value v_us = u_field(u_field(s573, 1), 0);
      u_value v_vs = u_field(u_field(s573, 1), 1);
      u_value v_ws = u_field(u_field(s573, 1), 2);
      d574 = 1;
      {
        u_value t576 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_us));
        if (u_truth(t576)) {
          t576 = u_eq(f_list_46_length_1(v_ys), f_list_46_length_1(v_vs));
        }
        u_value t577 = t576;
        if (u_truth(t577)) {
          t577 = u_eq(f_list_46_length_1(v_zs), f_list_46_length_1(v_ws));
        }
        if (u_truth(t577)) {
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
    if (!d574) {
      d574 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t2)), "check.TypeError");
      }
    }
    if (!d574) {
      u_no_case(s573);
    }
  }
}

void p_check_46_unifies(u_value v_xs, u_value v_ys, u_value *r_s) {
  {
    u_value s578 = u_mk_tuple(2, v_xs, v_ys);
    int d579 = 0;
    if ((!d579) && (u_is_con(u_field(s578, 0), &n_list_46_Cons) && u_is_con(u_field(s578, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s578, 0), 0);
      u_value v_xs1 = u_field(u_field(s578, 0), 1);
      u_value v_y = u_field(u_field(s578, 1), 0);
      u_value v_ys1 = u_field(u_field(s578, 1), 1);
      d579 = 1;
      {
        p_check_46_unify(v_x, v_y, r_s);
        p_check_46_unifies(v_xs1, v_ys1, r_s);
      }
    }
    if ((!d579) && (u_is_con(u_field(s578, 0), &n_list_46_Nil) && u_is_con(u_field(s578, 1), &n_list_46_Nil))) {
      d579 = 1;
      {
      }
    }
    if (!d579) {
      d579 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)), "check.TypeError");
      }
    }
    if (!d579) {
      u_no_case(s578);
    }
  }
}

void p_check_46_fresh_list(u_value v_xs, u_value *r_ts, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s580 = v_xs;
    int d581 = 0;
    if ((!d581) && u_is_con(s580, &n_list_46_Nil)) {
      d581 = 1;
      {
        u_value t582 = u_mk_con(&n_list_46_Nil, 0);
        *r_ts = t582;
      }
    }
    if ((!d581) && u_is_con(s580, &n_list_46_Cons)) {
      u_value v_more = u_field(s580, 1);
      d581 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_fresh_list(v_more, &v_rest, r_s);
        u_value t583 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rest);
        *r_ts = t583;
      }
    }
    if (!d581) {
      u_no_case(s580);
    }
  }
}

void p_check_46_instance(u_value v_ps, u_value *r_m, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s584 = v_ps;
    int d585 = 0;
    if ((!d585) && u_is_con(s584, &n_list_46_Nil)) {
      d585 = 1;
      {
        u_value t586 = u_mk_con(&n_list_46_Nil, 0);
        *r_m = t586;
      }
    }
    if ((!d585) && u_is_con(s584, &n_list_46_Cons)) {
      u_value v_p = u_field(s584, 0);
      u_value v_qs = u_field(s584, 1);
      d585 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_instance(v_qs, &v_rest, r_s);
        u_value t587 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_p, v_t), v_rest);
        *r_m = t587;
      }
    }
    if (!d585) {
      u_no_case(s584);
    }
  }
}

void p_check_46_instantiate(u_value v_sc, u_value *r_t, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value s588 = v_sc;
    int d589 = 0;
    if (!d589) {
      u_value v_ps = u_field(s588, 0);
      u_value v_u = u_field(s588, 1);
      d589 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        u_value t590 = f_check_46_subst_2(f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u), v_m);
        *r_t = t590;
      }
    }
    if (!d589) {
      u_no_case(s588);
    }
  }
}

void p_check_46_unique(u_value v_ns) {
  {
    u_value s591 = f_list_46_repeated_1(v_ns);
    int d592 = 0;
    if ((!d592) && u_is_con(s591, &n_option_46_Some)) {
      u_value v_n = u_field(s591, 0);
      d592 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, f_syntax_46_qualified_1(v_n)), "check.TypeError");
      }
    }
    if ((!d592) && u_is_con(s591, &n_option_46_None)) {
      d592 = 1;
      {
      }
    }
    if (!d592) {
      u_no_case(s591);
    }
  }
}

void p_check_46_unique_ids(u_value v_ns) {
  {
    u_value s593 = f_list_46_repeated_1(v_ns);
    int d594 = 0;
    if ((!d594) && u_is_con(s593, &n_option_46_Some)) {
      u_value v_n = u_field(s593, 0);
      d594 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, v_n), "check.TypeError");
      }
    }
    if ((!d594) && u_is_con(s593, &n_option_46_None)) {
      d594 = 1;
      {
      }
    }
    if (!d594) {
      u_no_case(s593);
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
    u_value s595 = v_t;
    int d596 = 0;
    if ((!d596) && u_is_con(s595, &n_syntax_46_THole)) {
      d596 = 1;
      {
        p_check_46_fresh(r_u, r_s);
      }
    }
    if ((!d596) && u_is_con(s595, &n_syntax_46_TParam)) {
      d596 = 1;
      {
        u_value t597 = v_t;
        *r_u = t597;
      }
    }
    if ((!d596) && u_is_con(s595, &n_syntax_46_TMeta)) {
      d596 = 1;
      {
        u_value t598 = v_t;
        *r_u = t598;
      }
    }
    if ((!d596) && u_is_con(s595, &n_syntax_46_TFun)) {
      u_value v_args = u_field(s595, 0);
      u_value v_r = u_field(s595, 1);
      d596 = 1;
      {
        p_check_46_normalizes(v_args, &v_us, r_s);
        p_check_46_normalize(v_r, &v_v, r_s);
        u_value t599 = u_mk_con(&n_syntax_46_TFun, 2, v_us, v_v);
        *r_u = t599;
      }
    }
    if ((!d596) && u_is_con(s595, &n_syntax_46_TTuple)) {
      u_value v_ts = u_field(s595, 0);
      d596 = 1;
      {
        p_check_46_normalizes(v_ts, &v_us, r_s);
        u_value t600 = u_mk_con(&n_syntax_46_TTuple, 1, v_us);
        *r_u = t600;
      }
    }
    if ((!d596) && u_is_con(s595, &n_syntax_46_TProc)) {
      u_value v_is = u_field(s595, 0);
      u_value v_os = u_field(s595, 1);
      u_value v_rs = u_field(s595, 2);
      d596 = 1;
      {
        p_check_46_normalizes(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs)), &v_us, r_s);
        u_value t601 = u_mk_con(&n_syntax_46_TProc, 3, f_list_46_take_2(f_list_46_length_1(v_is), v_us), f_list_46_take_2(f_list_46_length_1(v_os), f_list_46_drop_2(f_list_46_length_1(v_is), v_us)), f_list_46_drop_2(u_add(f_list_46_length_1(v_is), f_list_46_length_1(v_os)), v_us));
        *r_u = t601;
      }
    }
    if ((!d596) && u_is_con(s595, &n_syntax_46_TSort)) {
      u_value v_n = u_field(s595, 0);
      u_value v_args = u_field(s595, 1);
      d596 = 1;
      {
        if (u_truth(f_check_46_basic_1(v_n))) {
          {
            p_check_46_arguments(v_n, u_mk_int(0), v_args);
            u_value t602 = v_t;
            *r_u = t602;
          }
        } else {
          {
            u_value s604 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam603, 1, *r_s));
            int d605 = 0;
            if ((!d605) && (u_is_con(s604, &n_list_46_Cons) && u_is_con(u_field(s604, 1), &n_list_46_Nil))) {
              u_value v_m = u_field(s604, 0);
              d605 = 1;
              {
                p_check_46_normalize_sort(v_m, v_args, r_u, r_s);
              }
            }
            if ((!d605) && u_is_con(s604, &n_list_46_Nil)) {
              d605 = 1;
              {
                u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
              }
            }
            if (!d605) {
              u_value v_ms = s604;
              d605 = 1;
              {
                u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ms), "check.TypeError");
              }
            }
            if (!d605) {
              u_no_case(s604);
            }
          }
        }
      }
    }
    if (!d596) {
      u_no_case(s595);
    }
  }
}

void p_check_46_normalize_sort(u_value v_n, u_value v_args, u_value *r_u, u_value *r_s) {
  {
    u_value v_us = NULL;
    u_value s606 = f_ctx_46_data_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
    int d607 = 0;
    if ((!d607) && u_is_con(s606, &n_option_46_Some)) {
      u_value v_k = u_field(s606, 0);
      d607 = 1;
      {
        p_check_46_arguments(v_n, v_k, v_args);
        p_check_46_normalizes(v_args, &v_us, r_s);
        u_value t608 = u_mk_con(&n_syntax_46_TSort, 2, v_n, v_us);
        *r_u = t608;
      }
    }
    if ((!d607) && u_is_con(s606, &n_option_46_None)) {
      d607 = 1;
      {
        u_value s609 = f_ctx_46_alias_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
        int d610 = 0;
        if ((!d610) && u_is_con(s609, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s609, 0), 0);
          u_value v_rhs = u_field(u_field(s609, 0), 1);
          d610 = 1;
          {
            p_check_46_arguments(v_n, f_list_46_length_1(v_ps), v_args);
            p_check_46_normalizes(v_args, &v_us, r_s);
            u_value t611 = f_check_46_subst_2(v_rhs, f_list_46_zip_2(f_check_46_params_1(v_ps), v_us));
            *r_u = t611;
          }
        }
        if ((!d610) && u_is_con(s609, &n_option_46_None)) {
          d610 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
          }
        }
        if (!d610) {
          u_no_case(s609);
        }
      }
    }
    if (!d607) {
      u_no_case(s606);
    }
  }
}

void p_check_46_normalizes(u_value v_ts, u_value *r_us, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_rest = NULL;
    u_value s612 = v_ts;
    int d613 = 0;
    if ((!d613) && u_is_con(s612, &n_list_46_Nil)) {
      d613 = 1;
      {
        u_value t614 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t614;
      }
    }
    if ((!d613) && u_is_con(s612, &n_list_46_Cons)) {
      u_value v_t = u_field(s612, 0);
      u_value v_more = u_field(s612, 1);
      d613 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalizes(v_more, &v_rest, r_s);
        u_value t615 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t615;
      }
    }
    if (!d613) {
      u_no_case(s612);
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
    u_value s616 = v_ts;
    int d617 = 0;
    if ((!d617) && u_is_con(s616, &n_list_46_Nil)) {
      d617 = 1;
      {
        u_value t618 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t618;
      }
    }
    if ((!d617) && u_is_con(s616, &n_list_46_Cons)) {
      u_value v_t = u_field(s616, 0);
      u_value v_more = u_field(s616, 1);
      d617 = 1;
      {
        p_check_46_known(v_t, v_what, &v_u, r_s);
        p_check_46_knowns(v_more, v_what, &v_rest, r_s);
        u_value t619 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t619;
      }
    }
    if (!d617) {
      u_no_case(s616);
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
    u_value s620 = v_xs;
    int d621 = 0;
    if ((!d621) && u_is_con(s620, &n_list_46_Nil)) {
      d621 = 1;
      {
        u_value t622 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t622;
      }
    }
    if ((!d621) && (u_is_con(s620, &n_list_46_Cons) && u_is_con(u_field(s620, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s620, 0), 0);
      u_value v_t = u_field(u_field(s620, 0), 1);
      u_value v_more = u_field(s620, 1);
      d621 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalize_vars(v_more, &v_rest, r_s);
        u_value t623 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t623;
      }
    }
    if (!d621) {
      u_no_case(s620);
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
    u_value s624 = v_xs;
    int d625 = 0;
    if ((!d625) && u_is_con(s624, &n_list_46_Nil)) {
      d625 = 1;
      {
        u_value t626 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t626;
      }
    }
    if ((!d625) && (u_is_con(s624, &n_list_46_Cons) && u_is_con(u_field(s624, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s624, 0), 0);
      u_value v_t = u_field(u_field(s624, 0), 1);
      u_value v_more = u_field(s624, 1);
      d625 = 1;
      {
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_known_vars(v_more, &v_rest, r_s);
        u_value t627 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t627;
      }
    }
    if (!d625) {
      u_no_case(s624);
    }
  }
}

void p_check_46_resolve_name(u_value v_s, u_value v_env, u_value v_n, u_value v_k, u_value *r_m, u_value *r_sc) {
  {
    u_value s628 = f_check_46_candidates_3(v_s, v_env, v_n);
    int d629 = 0;
    if ((!d629) && u_is_con(s628, &n_list_46_Nil)) {
      d629 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if ((!d629) && (u_is_con(s628, &n_list_46_Cons) && u_is_con(u_field(s628, 1), &n_list_46_Nil))) {
      u_value v_m1 = u_field(u_field(s628, 0), 0);
      u_value v_one = u_field(u_field(s628, 0), 1);
      d629 = 1;
      {
        u_value t630 = v_m1;
        u_value t631 = v_one;
        *r_m = t630;
        *r_sc = t631;
      }
    }
    if (!d629) {
      u_value v_cs = s628;
      d629 = 1;
      {
        if (u_truth(u_lt(v_k, u_mk_int(0)))) {
          {
            u_raise(u_mk_con(&n_check_46_OverloadedUnapplied, 1, v_n), "check.TypeError");
          }
        }
        u_value s632 = f_check_46_with_arity_2(v_cs, v_k);
        int d633 = 0;
        if ((!d633) && (u_is_con(s632, &n_list_46_Cons) && u_is_con(u_field(s632, 1), &n_list_46_Nil))) {
          u_value v_m1 = u_field(u_field(s632, 0), 0);
          u_value v_one = u_field(u_field(s632, 0), 1);
          d633 = 1;
          {
            u_value t634 = v_m1;
            u_value t635 = v_one;
            *r_m = t634;
            *r_sc = t635;
          }
        }
        if ((!d633) && u_is_con(s632, &n_list_46_Nil)) {
          d633 = 1;
          {
            u_raise(u_mk_con(&n_check_46_WrongArity, 2, v_n, v_k), "check.TypeError");
          }
        }
        if (!d633) {
          u_value v_more = s632;
          d633 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, f_list_46_firsts_1(v_more)), "check.TypeError");
          }
        }
        if (!d633) {
          u_no_case(s632);
        }
      }
    }
    if (!d629) {
      u_no_case(s628);
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
    u_value t636 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_u);
    *r_x = t636;
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
    u_value s637 = v_e;
    int d638 = 0;
    if ((!d638) && u_is_con(s637, &n_syntax_46_ELit)) {
      u_value v_v = u_field(s637, 0);
      d638 = 1;
      {
        u_value t639 = v_e;
        u_value t640 = f_check_46_literal_1(v_v);
        *r_e1 = t639;
        *r_t = t640;
      }
    }
    if ((!d638) && (u_is_con(s637, &n_syntax_46_EVar) && u_is_con(u_field(s637, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s637, 0), 0);
      u_value v_a = u_field(u_field(s637, 0), 1);
      d638 = 1;
      {
        p_check_46_resolve_name(*r_s, v_env, v_n, u_mk_int(-1), &v_m, &v_sc);
        p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
        u_value t641 = u_mk_con(&n_syntax_46_EVar, 1, v_x);
        u_value t642 = f_syntax_46_Var_46_typ_1(v_x);
        *r_e1 = t641;
        *r_t = t642;
      }
    }
    if ((!d638) && u_is_con(s637, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s637, 0);
      u_value v_args = u_field(s637, 1);
      d638 = 1;
      {
        p_check_46_application(v_env, v_f, v_args, r_e1, r_t, r_s);
      }
    }
    if ((!d638) && u_is_con(s637, &n_syntax_46_ELam)) {
      u_value v_bound = u_field(s637, 0);
      u_value v_body = u_field(s637, 1);
      d638 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t643 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, v_b);
        u_value t644 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), v_u);
        *r_e1 = t643;
        *r_t = t644;
      }
    }
    if ((!d638) && u_is_con(s637, &n_syntax_46_EProc)) {
      u_value v_is = u_field(s637, 0);
      u_value v_os = u_field(s637, 1);
      u_value v_rs = u_field(s637, 2);
      u_value v_body = u_field(s637, 3);
      d638 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))));
        p_check_46_formals(v_is, &v_xs, r_s);
        p_check_46_formals(v_os, &v_ys, r_s);
        p_check_46_formals(v_rs, &v_zs, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(f_list_46__43__43__2(v_xs, f_list_46__43__43__2(v_ys, v_zs))), v_env), f_syntax_46_names_1(f_list_46__43__43__2(v_ys, v_zs)), v_body, &v_y, r_s);
        u_value t645 = u_mk_con(&n_syntax_46_EProc, 4, v_xs, v_ys, v_zs, v_y);
        u_value t646 = u_mk_con(&n_syntax_46_TProc, 3, f_syntax_46_types_1(v_xs), f_syntax_46_types_1(v_ys), f_syntax_46_types_1(v_zs));
        *r_e1 = t645;
        *r_t = t646;
      }
    }
    if ((!d638) && u_is_con(s637, &n_syntax_46_ELet)) {
      u_value v_bound = u_field(s637, 0);
      u_value v_body = u_field(s637, 1);
      d638 = 1;
      {
        p_check_46_formals(f_list_46_firsts_1(v_bound), &v_xs, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_bound), f_syntax_46_types_1(v_xs), &v_es, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_generalized_3(f_check_46_State_46_sub_1(*r_s), v_env, v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t647 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        u_value t648 = v_u;
        *r_e1 = t647;
        *r_t = t648;
      }
    }
    if ((!d638) && u_is_con(s637, &n_syntax_46_EIte)) {
      u_value v_cond = u_field(s637, 0);
      u_value v_left = u_field(s637, 1);
      u_value v_right = u_field(s637, 2);
      d638 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(v_env, v_left, &v_l, &v_u, r_s);
        p_check_46_check(v_env, v_right, v_u, &v_b, r_s);
        u_value t649 = u_mk_con(&n_syntax_46_EIte, 3, v_c, v_l, v_b);
        u_value t650 = v_u;
        *r_e1 = t649;
        *r_t = t650;
      }
    }
    if ((!d638) && u_is_con(s637, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s637, 0);
      u_value v_bound = u_field(s637, 1);
      u_value v_lists = u_field(s637, 2);
      u_value v_cond = u_field(s637, 3);
      d638 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_same_length(f_list_46_length_1(v_xs), f_list_46_length_1(v_lists));
        p_check_46_generators(v_env, v_xs, v_lists, &v_es, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_result, &v_b, &v_u, r_s);
        u_value t651 = u_mk_con(&n_syntax_46_EComp, 4, v_b, v_xs, v_es, v_c);
        u_value t652 = f_check_46_tlist_1(v_u);
        *r_e1 = t651;
        *r_t = t652;
      }
    }
    if ((!d638) && u_is_con(s637, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s637, 0);
      d638 = 1;
      {
        p_check_46_infers(v_env, v_items, &v_es, &v_ts, r_s);
        u_value t653 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
        u_value t654 = u_mk_con(&n_syntax_46_TTuple, 1, v_ts);
        *r_e1 = t653;
        *r_t = t654;
      }
    }
    if ((!d638) && u_is_con(s637, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s637, 0);
      u_value v_a = u_field(s637, 1);
      d638 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_normalize(v_a, &v_w, r_s);
        p_check_46_unify(v_u, v_w, r_s);
        p_check_46_fresh(r_t, r_s);
        u_value t655 = u_mk_con(&n_syntax_46_ERaise, 2, v_c, v_u);
        *r_e1 = t655;
      }
    }
    if ((!d638) && u_is_con(s637, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s637, 0);
      u_value v_cases = u_field(s637, 1);
      d638 = 1;
      {
        p_check_46_fresh(r_t, r_s);
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_expr_branches(v_env, v_cases, v_u, *r_t, &v_bs, r_s);
        u_value t656 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e657 = &t656;
        u_value l658 = v_bs;
        while (u_is_con(l658, &n_list_46_Cons)) {
          u_value v_br = u_field(l658, 0);
          if (u_truth(u_mk_bool(1))) {
            e657 = u_append(e657, f_check_46_case_of_1(v_br));
          }
          l658 = u_field(l658, 1);
        }
        p_check_46_coverage(*r_s, v_u, t656);
        u_value t659 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, v_bs);
        *r_e1 = t659;
      }
    }
    if (!d638) {
      u_no_case(s637);
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
    u_value s660 = v_f;
    int d661 = 0;
    if ((!d661) && (u_is_con(s660, &n_syntax_46_EVar) && (u_is_con(u_field(s660, 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(s660, 0), 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(u_field(s660, 0), 0), 0), &n_option_46_None))))) {
      u_value v_b = u_field(u_field(u_field(s660, 0), 0), 1);
      u_value v_a = u_field(u_field(s660, 0), 1);
      u_value t662 = f_prelude_46__62__2(u_str_size(v_b), u_mk_int(1));
      if (u_truth(t662)) {
        t662 = u_eq(u_str_get(v_b, u_mk_int(0)), u_mk_char(46));
      }
      if (u_truth(t662)) {
        d661 = 1;
        {
          p_check_46_field_access(v_env, v_b, v_a, v_args, r_e1, r_t, r_s);
        }
      }
    }
    if ((!d661) && (u_is_con(s660, &n_syntax_46_EVar) && u_is_con(u_field(s660, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s660, 0), 0);
      u_value v_a = u_field(u_field(s660, 0), 1);
      d661 = 1;
      {
        if (u_truth(f_prelude_46__62__2(f_list_46_length_1(f_check_46_candidates_3(*r_s, v_env, v_n)), u_mk_int(1)))) {
          {
            p_check_46_resolve_name(*r_s, v_env, v_n, f_list_46_length_1(v_args), &v_m, &v_sc);
            p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
            p_check_46_apply(v_env, f_syntax_46_Var_46_typ_1(v_x), v_args, &v_args1, r_t, r_s);
            u_value t663 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), v_args1);
            *r_e1 = t663;
          }
        } else {
          {
            p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
            p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
            u_value t664 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
            *r_e1 = t664;
          }
        }
      }
    }
    if (!d661) {
      d661 = 1;
      {
        p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
        p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
        u_value t665 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
        *r_e1 = t665;
      }
    }
    if (!d661) {
      u_no_case(s660);
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
    u_value s666 = v_args;
    int d667 = 0;
    if ((!d667) && u_is_con(s666, &n_list_46_Cons)) {
      u_value v_arg = u_field(s666, 0);
      u_value v_rest = u_field(s666, 1);
      d667 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_arg1, &v_u, r_s);
        u_value s668 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u);
        int d669 = 0;
        if ((!d669) && (u_is_con(s668, &n_syntax_46_TSort) && (u_is_con(u_field(s668, 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(s668, 0), 0), &n_option_46_Some)))) {
          u_value v_m = u_field(u_field(u_field(s668, 0), 0), 0);
          u_value v_tn = u_field(u_field(s668, 0), 1);
          d669 = 1;
          {
            u_value t670 = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), u_concat(v_tn, v_f));
            v_acc = t670;
          }
        }
        if (!d669) {
          u_value v_v = s668;
          d669 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownFieldType, 2, v_f, v_v), "check.TypeError");
          }
        }
        if (!d669) {
          u_no_case(s668);
        }
        u_value s671 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(*r_s), v_acc);
        int d672 = 0;
        if ((!d672) && (u_is_con(s671, &n_list_46_Cons) && u_is_con(u_field(s671, 1), &n_list_46_Nil))) {
          u_value v_sc = u_field(s671, 0);
          d672 = 1;
          {
            p_check_46_use(v_acc, v_a, v_sc, &v_x, r_s);
          }
        }
        if (!d672) {
          d672 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownField, 1, v_acc), "check.TypeError");
          }
        }
        if (!d672) {
          u_no_case(s671);
        }
        p_check_46_infers(v_env, v_rest, &v_rest1, &v_us, r_s);
        p_check_46_fresh(r_t, r_s);
        p_check_46_unify(f_syntax_46_Var_46_typ_1(v_x), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_u, v_us), *r_t), r_s);
        u_value t673 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), u_mk_con(&n_list_46_Cons, 2, v_arg1, v_rest1));
        *r_e1 = t673;
      }
    }
    if (!d667) {
      d667 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, u_mk_int(1), f_list_46_length_1(v_args)), "check.TypeError");
      }
    }
    if (!d667) {
      u_no_case(s666);
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
    u_value s674 = u_mk_tuple(2, v_es, v_ts);
    int d675 = 0;
    if ((!d675) && (u_is_con(u_field(s674, 0), &n_list_46_Cons) && u_is_con(u_field(s674, 1), &n_list_46_Cons))) {
      u_value v_e = u_field(u_field(s674, 0), 0);
      u_value v_more = u_field(u_field(s674, 0), 1);
      u_value v_t = u_field(u_field(s674, 1), 0);
      u_value v_ts1 = u_field(u_field(s674, 1), 1);
      d675 = 1;
      {
        p_check_46_check(v_env, v_e, v_t, &v_e1, r_s);
        p_check_46_checks(v_env, v_more, v_ts1, &v_rest, r_s);
        u_value t676 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        *r_es1 = t676;
      }
    }
    if ((!d675) && (u_is_con(u_field(s674, 0), &n_list_46_Nil) && u_is_con(u_field(s674, 1), &n_list_46_Nil))) {
      d675 = 1;
      {
        u_value t677 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t677;
      }
    }
    if (!d675) {
      d675 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_es)), "check.TypeError");
      }
    }
    if (!d675) {
      u_no_case(s674);
    }
  }
}

void p_check_46_infers(u_value v_env, u_value v_es, u_value *r_es1, u_value *r_ts, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value v_us = NULL;
    u_value s678 = v_es;
    int d679 = 0;
    if ((!d679) && u_is_con(s678, &n_list_46_Nil)) {
      d679 = 1;
      {
        u_value t680 = u_mk_con(&n_list_46_Nil, 0);
        u_value t681 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t680;
        *r_ts = t681;
      }
    }
    if ((!d679) && u_is_con(s678, &n_list_46_Cons)) {
      u_value v_e = u_field(s678, 0);
      u_value v_more = u_field(s678, 1);
      d679 = 1;
      {
        p_check_46_infer(v_env, v_e, &v_e1, &v_t, r_s);
        p_check_46_infers(v_env, v_more, &v_rest, &v_us, r_s);
        u_value t682 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        u_value t683 = u_mk_con(&n_list_46_Cons, 2, v_t, v_us);
        *r_es1 = t682;
        *r_ts = t683;
      }
    }
    if (!d679) {
      u_no_case(s678);
    }
  }
}

void p_check_46_generators(u_value v_env, u_value v_xs, u_value v_lists, u_value *r_lists1, u_value *r_s) {
  {
    u_value v_l = NULL;
    u_value v_rest = NULL;
    u_value s684 = u_mk_tuple(2, v_xs, v_lists);
    int d685 = 0;
    if ((!d685) && (u_is_con(u_field(s684, 0), &n_list_46_Cons) && u_is_con(u_field(s684, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s684, 0), 0);
      u_value v_ys = u_field(u_field(s684, 0), 1);
      u_value v_e = u_field(u_field(s684, 1), 0);
      u_value v_es = u_field(u_field(s684, 1), 1);
      d685 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tlist_1(f_syntax_46_Var_46_typ_1(v_x)), &v_l, r_s);
        p_check_46_generators(f_list_46__43__43__2(f_check_46_bind_1(u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0))), v_env), v_ys, v_es, &v_rest, r_s);
        u_value t686 = u_mk_con(&n_list_46_Cons, 2, v_l, v_rest);
        *r_lists1 = t686;
      }
    }
    if (!d685) {
      d685 = 1;
      {
        u_value t687 = u_mk_con(&n_list_46_Nil, 0);
        *r_lists1 = t687;
      }
    }
    if (!d685) {
      u_no_case(s684);
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
    u_value s688 = v_cases;
    int d689 = 0;
    if ((!d689) && u_is_con(s688, &n_list_46_Nil)) {
      d689 = 1;
      {
        u_value t690 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t690;
      }
    }
    if ((!d689) && (u_is_con(s688, &n_list_46_Cons) && u_is_con(u_field(s688, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s688, 0), 0);
      u_value v_g = u_field(u_field(s688, 0), 1);
      u_value v_b = u_field(u_field(s688, 0), 2);
      u_value v_more = u_field(s688, 1);
      d689 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_b, v_r, &v_b1, r_s);
        p_check_46_expr_branches(v_env, v_more, v_t, v_r, &v_rest, r_s);
        u_value t691 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t691;
      }
    }
    if (!d689) {
      u_no_case(s688);
    }
  }
}

void p_check_46_guard(u_value v_env, u_value v_g, u_value *r_g1, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value s692 = v_g;
    int d693 = 0;
    if ((!d693) && u_is_con(s692, &n_option_46_Some)) {
      u_value v_e = u_field(s692, 0);
      d693 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tbool_0(), &v_e1, r_s);
        u_value t694 = u_mk_con(&n_option_46_Some, 1, v_e1);
        *r_g1 = t694;
      }
    }
    if ((!d693) && u_is_con(s692, &n_option_46_None)) {
      d693 = 1;
      {
        u_value t695 = u_mk_con(&n_option_46_None, 0);
        *r_g1 = t695;
      }
    }
    if (!d693) {
      u_no_case(s692);
    }
  }
}

void p_check_46_coverage(u_value v_s, u_value v_t, u_value v_cs) {
  {
    p_check_46_reachable(v_s, v_t, u_mk_con(&n_list_46_Nil, 0), v_cs);
    u_value t696 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e697 = &t696;
    u_value l698 = f_check_46_unguarded_1(v_cs);
    while (u_is_con(l698, &n_list_46_Cons)) {
      u_value v_p = u_field(l698, 0);
      if (u_truth(u_mk_bool(1))) {
        e697 = u_append(e697, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
      }
      l698 = u_field(l698, 1);
    }
    u_value s699 = f_check_46_missing_4(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t696, u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
    int d700 = 0;
    if ((!d700) && (u_is_con(s699, &n_option_46_Some) && u_is_con(u_field(s699, 0), &n_list_46_Cons))) {
      u_value v_w = u_field(u_field(s699, 0), 0);
      d700 = 1;
      {
        u_raise(u_mk_con(&n_check_46_NotExhaustive, 1, v_w), "check.TypeError");
      }
    }
    if (!d700) {
      d700 = 1;
      {
      }
    }
    if (!d700) {
      u_no_case(s699);
    }
  }
}

u_value f_check_46_unguarded_1(u_value v_cs) {
  u_value s701 = v_cs;
  u_value t702 = NULL;
  if ((!t702) && u_is_con(s701, &n_list_46_Nil)) {
    t702 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t702) && (u_is_con(s701, &n_list_46_Cons) && u_equals(u_field(u_field(s701, 0), 1), u_mk_bool(1)))) {
    u_value v_p = u_field(u_field(s701, 0), 0);
    u_value v_rest = u_field(s701, 1);
    t702 = u_mk_con(&n_list_46_Cons, 2, v_p, f_check_46_unguarded_1(v_rest));
  }
  if ((!t702) && (u_is_con(s701, &n_list_46_Cons) && u_equals(u_field(u_field(s701, 0), 1), u_mk_bool(0)))) {
    u_value v_rest = u_field(s701, 1);
    t702 = f_check_46_unguarded_1(v_rest);
  }
  if (!t702) {
    u_no_case(s701);
  }
  return t702;
}

void p_check_46_reachable(u_value v_s, u_value v_t, u_value v_before, u_value v_cs) {
  {
    u_value s703 = v_cs;
    int d704 = 0;
    if ((!d704) && u_is_con(s703, &n_list_46_Nil)) {
      d704 = 1;
      {
      }
    }
    if ((!d704) && u_is_con(s703, &n_list_46_Cons)) {
      u_value v_p = u_field(u_field(s703, 0), 0);
      u_value v_open = u_field(u_field(s703, 0), 1);
      u_value v_rest = u_field(s703, 1);
      d704 = 1;
      {
        u_value t705 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e706 = &t705;
        u_value l707 = v_before;
        while (u_is_con(l707, &n_list_46_Cons)) {
          u_value v_q = u_field(l707, 0);
          if (u_truth(u_mk_bool(1))) {
            e706 = u_append(e706, u_mk_con(&n_list_46_Cons, 2, v_q, u_mk_con(&n_list_46_Nil, 0)));
          }
          l707 = u_field(l707, 1);
        }
        if (u_truth(f_prelude_46_not_1(f_check_46_useful_5(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t705, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)))))) {
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
    if (!d704) {
      u_no_case(s703);
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
    u_value s708 = v_p;
    int d709 = 0;
    if ((!d709) && u_is_con(s708, &n_syntax_46_PWild)) {
      d709 = 1;
      {
        u_value t710 = v_p;
        u_value t711 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t710;
        *r_xs = t711;
      }
    }
    if ((!d709) && u_is_con(s708, &n_syntax_46_PLit)) {
      u_value v_v = u_field(s708, 0);
      d709 = 1;
      {
        p_check_46_unify(f_check_46_literal_1(v_v), v_t, r_s);
        u_value t712 = v_p;
        u_value t713 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t712;
        *r_xs = t713;
      }
    }
    if ((!d709) && (u_is_con(s708, &n_syntax_46_PVar) && u_is_con(u_field(s708, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s708, 0), 0);
      u_value v_a = u_field(u_field(s708, 0), 1);
      d709 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam714, 1, *r_s)), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            p_check_46_binders(u_mk_con(&n_syntax_46_PApp, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_t, r_p1, r_xs, r_s);
          }
        } else {
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_u, v_t, r_s);
            u_value t715 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u));
            u_value t716 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), u_mk_con(&n_list_46_Nil, 0));
            *r_p1 = t715;
            *r_xs = t716;
          }
        }
      }
    }
    if ((!d709) && u_is_con(s708, &n_syntax_46_PApp)) {
      u_value v_n = u_field(s708, 0);
      u_value v_args = u_field(s708, 1);
      d709 = 1;
      {
        u_value s718 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam717, 1, *r_s));
        int d719 = 0;
        if ((!d719) && (u_is_con(s718, &n_list_46_Cons) && u_is_con(u_field(s718, 1), &n_list_46_Nil))) {
          u_value v_k1 = u_field(s718, 0);
          d719 = 1;
          {
            u_value t720 = v_k1;
            v_k = t720;
          }
        }
        if ((!d719) && u_is_con(s718, &n_list_46_Nil)) {
          d719 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d719) {
          u_value v_ks = s718;
          d719 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ks), "check.TypeError");
          }
        }
        if (!d719) {
          u_no_case(s718);
        }
        u_value s721 = f_ctx_46_constr_of_2(f_check_46_State_46_ctx_1(*r_s), v_k);
        int d722 = 0;
        if ((!d722) && u_is_con(s721, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s721, 0), 0);
          u_value v_ks = u_field(u_field(s721, 0), 1);
          u_value v_r = u_field(u_field(s721, 0), 2);
          d722 = 1;
          {
            p_check_46_same_length(f_list_46_length_1(v_ks), f_list_46_length_1(v_args));
            p_check_46_instance(v_ps, &v_m, r_s);
            p_check_46_unify(f_check_46_subst_2(v_r, v_m), v_t, r_s);
            u_value t723 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e724 = &t723;
            u_value l725 = v_ks;
            while (u_is_con(l725, &n_list_46_Cons)) {
              u_value v_kt = u_field(l725, 0);
              if (u_truth(u_mk_bool(1))) {
                e724 = u_append(e724, f_check_46_subst_2(v_kt, v_m));
              }
              l725 = u_field(l725, 1);
            }
            p_check_46_binder_list(v_args, t723, &v_qs, r_xs, r_s);
            u_value t726 = u_mk_con(&n_syntax_46_PApp, 2, v_k, v_qs);
            *r_p1 = t726;
          }
        }
        if ((!d722) && u_is_con(s721, &n_option_46_None)) {
          d722 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d722) {
          u_no_case(s721);
        }
      }
    }
    if ((!d709) && u_is_con(s708, &n_syntax_46_PTuple)) {
      u_value v_items = u_field(s708, 0);
      d709 = 1;
      {
        p_check_46_fresh_list(v_items, &v_ts, r_s);
        p_check_46_unify(u_mk_con(&n_syntax_46_TTuple, 1, v_ts), v_t, r_s);
        p_check_46_binder_list(v_items, v_ts, &v_qs, r_xs, r_s);
        u_value t727 = u_mk_con(&n_syntax_46_PTuple, 1, v_qs);
        *r_p1 = t727;
      }
    }
    if (!d709) {
      u_no_case(s708);
    }
  }
}

void p_check_46_binder_list(u_value v_ps, u_value v_ts, u_value *r_qs, u_value *r_xs, u_value *r_s) {
  {
    u_value v_q = NULL;
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value v_zs = NULL;
    u_value s728 = u_mk_tuple(2, v_ps, v_ts);
    int d729 = 0;
    if ((!d729) && (u_is_con(u_field(s728, 0), &n_list_46_Cons) && u_is_con(u_field(s728, 1), &n_list_46_Cons))) {
      u_value v_p = u_field(u_field(s728, 0), 0);
      u_value v_more = u_field(u_field(s728, 0), 1);
      u_value v_t = u_field(u_field(s728, 1), 0);
      u_value v_ts1 = u_field(u_field(s728, 1), 1);
      d729 = 1;
      {
        p_check_46_binders(v_p, v_t, &v_q, &v_ys, r_s);
        p_check_46_binder_list(v_more, v_ts1, &v_rest, &v_zs, r_s);
        u_value t730 = u_mk_con(&n_list_46_Cons, 2, v_q, v_rest);
        u_value t731 = f_list_46__43__43__2(v_ys, v_zs);
        *r_qs = t730;
        *r_xs = t731;
      }
    }
    if ((!d729) && (u_is_con(u_field(s728, 0), &n_list_46_Nil) && u_is_con(u_field(s728, 1), &n_list_46_Nil))) {
      d729 = 1;
      {
        u_value t732 = u_mk_con(&n_list_46_Nil, 0);
        u_value t733 = u_mk_con(&n_list_46_Nil, 0);
        *r_qs = t732;
        *r_xs = t733;
      }
    }
    if (!d729) {
      d729 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_ps)), "check.TypeError");
      }
    }
    if (!d729) {
      u_no_case(s728);
    }
  }
}

void p_check_46_variable(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s734 = v_x;
    int d735 = 0;
    if ((!d735) && u_is_con(s734, &n_syntax_46_Var)) {
      u_value v_n = u_field(s734, 0);
      u_value v_a = u_field(s734, 1);
      d735 = 1;
      {
        if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, v_assignable)))) {
          {
            u_raise(u_mk_con(&n_check_46_NotAssignable, 1, v_n), "check.TypeError");
          }
        }
        u_value s736 = f_list_46_lookup_2(v_n, v_env);
        int d737 = 0;
        if ((!d737) && u_is_con(s736, &n_option_46_Some)) {
          u_value v_t = u_field(u_field(s736, 0), 1);
          d737 = 1;
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_t, v_u, r_s);
            u_value t738 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_t);
            *r_y = t738;
          }
        }
        if ((!d737) && u_is_con(s736, &n_option_46_None)) {
          d737 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
          }
        }
        if (!d737) {
          u_no_case(s736);
        }
      }
    }
    if (!d735) {
      u_no_case(s734);
    }
  }
}

void p_check_46_variables(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s739 = v_xs;
    int d740 = 0;
    if ((!d740) && u_is_con(s739, &n_list_46_Nil)) {
      d740 = 1;
      {
        u_value t741 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t741;
      }
    }
    if ((!d740) && u_is_con(s739, &n_list_46_Cons)) {
      u_value v_x = u_field(s739, 0);
      u_value v_more = u_field(s739, 1);
      d740 = 1;
      {
        p_check_46_variable(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_variables(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t742 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t742;
      }
    }
    if (!d740) {
      u_no_case(s739);
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
    u_value s743 = v_x;
    int d744 = 0;
    if ((!d744) && u_is_con(s743, &n_syntax_46_SSkip)) {
      d744 = 1;
      {
        u_value t745 = v_x;
        *r_y = t745;
      }
    }
    if ((!d744) && u_is_con(s743, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s743, 0);
      d744 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)));
        p_check_46_variables(v_env, v_assignable, f_list_46_firsts_1(v_pairs), &v_ys, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_pairs), f_syntax_46_types_1(v_ys), &v_es, r_s);
        u_value t746 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_zip_2(v_ys, v_es));
        *r_y = t746;
      }
    }
    if ((!d744) && u_is_con(s743, &n_syntax_46_SBlock)) {
      u_value v_locals = u_field(s743, 0);
      u_value v_body = u_field(s743, 1);
      d744 = 1;
      {
        p_check_46_untyped(v_locals, f_check_46_initialized_1(v_body));
        p_check_46_formals(v_locals, &v_ys, r_s);
        p_check_46_statements(f_list_46__43__43__2(f_check_46_bind_1(v_ys), v_env), f_list_46__43__43__2(f_syntax_46_names_1(v_ys), v_assignable), v_body, &v_xs, r_s);
        u_value t747 = u_mk_con(&n_syntax_46_SBlock, 2, v_ys, v_xs);
        *r_y = t747;
      }
    }
    if ((!d744) && u_is_con(s743, &n_syntax_46_SIf)) {
      u_value v_cond = u_field(s743, 0);
      u_value v_left = u_field(s743, 1);
      u_value v_right = u_field(s743, 2);
      d744 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_left, &v_l, r_s);
        p_check_46_statement(v_env, v_assignable, v_right, &v_r, r_s);
        u_value t748 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
        *r_y = t748;
      }
    }
    if ((!d744) && u_is_con(s743, &n_syntax_46_SWhile)) {
      u_value v_cond = u_field(s743, 0);
      u_value v_body = u_field(s743, 1);
      d744 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        u_value t749 = u_mk_con(&n_syntax_46_SWhile, 2, v_c, v_l);
        *r_y = t749;
      }
    }
    if ((!d744) && u_is_con(s743, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s743, 0);
      u_value v_ins = u_field(s743, 1);
      u_value v_outs = u_field(s743, 2);
      u_value v_refs = u_field(s743, 3);
      d744 = 1;
      {
        p_check_46_call_proc(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d744) && u_is_con(s743, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s743, 0);
      u_value v_cases = u_field(s743, 1);
      d744 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_cases, v_t, &v_bs, r_s);
        u_value t750 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e751 = &t750;
        u_value l752 = v_bs;
        while (u_is_con(l752, &n_list_46_Cons)) {
          u_value v_br = u_field(l752, 0);
          if (u_truth(u_mk_bool(1))) {
            e751 = u_append(e751, f_check_46_case_of_1(v_br));
          }
          l752 = u_field(l752, 1);
        }
        p_check_46_coverage(*r_s, v_t, t750);
        u_value t753 = u_mk_con(&n_syntax_46_SMatch, 2, v_c, v_bs);
        *r_y = t753;
      }
    }
    if ((!d744) && u_is_con(s743, &n_syntax_46_SAssert)) {
      u_value v_cond = u_field(s743, 0);
      d744 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        u_value t754 = u_mk_con(&n_syntax_46_SAssert, 1, v_c);
        *r_y = t754;
      }
    }
    if ((!d744) && u_is_con(s743, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s743, 0);
      u_value v_a = u_field(s743, 1);
      d744 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_unify(v_t, v_u, r_s);
        u_value t755 = u_mk_con(&n_syntax_46_SRaise, 2, v_c, v_t);
        *r_y = t755;
      }
    }
    if ((!d744) && u_is_con(s743, &n_syntax_46_STry)) {
      u_value v_body = u_field(s743, 0);
      u_value v_handlers = u_field(s743, 1);
      d744 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_handlers, &v_hs, r_s);
        u_value t756 = u_mk_con(&n_syntax_46_STry, 2, v_l, v_hs);
        *r_y = t756;
      }
    }
    if (!d744) {
      u_no_case(s743);
    }
  }
}

u_value f_check_46_initialized_1(u_value v_xs) {
  u_value s757 = v_xs;
  u_value t758 = NULL;
  if ((!t758) && (u_is_con(s757, &n_list_46_Cons) && (u_is_con(u_field(s757, 0), &n_syntax_46_SAssign) && (u_is_con(u_field(u_field(s757, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s757, 0), 0), 1), &n_list_46_Nil))))) {
    u_value v_x = u_field(u_field(u_field(u_field(s757, 0), 0), 0), 0);
    u_value v_rest = u_field(s757, 1);
    t758 = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), f_check_46_initialized_1(v_rest));
  }
  if (!t758) {
    t758 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t758) {
    u_no_case(s757);
  }
  return t758;
}

void p_check_46_untyped(u_value v_xs, u_value v_inits) {
  {
    u_value s759 = v_xs;
    int d760 = 0;
    if ((!d760) && u_is_con(s759, &n_list_46_Nil)) {
      d760 = 1;
      {
      }
    }
    if ((!d760) && (u_is_con(s759, &n_list_46_Cons) && u_is_con(u_field(s759, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s759, 0), 0);
      u_value v_t = u_field(u_field(s759, 0), 1);
      u_value v_rest = u_field(s759, 1);
      d760 = 1;
      {
        u_value t761 = f_check_46_holes_1(v_t);
        if (u_truth(t761)) {
          t761 = f_prelude_46_not_1(f_list_46_contains_2(v_n, v_inits));
        }
        if (u_truth(t761)) {
          {
            u_raise(u_mk_con(&n_check_46_MissingType, 1, v_n), "check.TypeError");
          }
        }
        p_check_46_untyped(v_rest, v_inits);
      }
    }
    if (!d760) {
      u_no_case(s759);
    }
  }
}

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s762 = v_xs;
    int d763 = 0;
    if ((!d763) && u_is_con(s762, &n_list_46_Nil)) {
      d763 = 1;
      {
        u_value t764 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t764;
      }
    }
    if ((!d763) && u_is_con(s762, &n_list_46_Cons)) {
      u_value v_x = u_field(s762, 0);
      u_value v_more = u_field(s762, 1);
      d763 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_statements(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t765 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t765;
      }
    }
    if (!d763) {
      u_no_case(s762);
    }
  }
}

u_value f_check_46_os_procs_1(u_value v_c) {
  u_value t770;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_766 = U_LIT("os_args");
    static struct u_object l_os_read_767 = U_LIT("os_read");
    static struct u_object l_os_write_768 = U_LIT("os_write");
    static struct u_object l_os_library_769 = U_LIT("os_library");
    t770 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_766), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_767), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_768), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_769), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)))));
  } else {
    t770 = u_mk_con(&n_list_46_Nil, 0);
  }
  return t770;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t771 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e772 = &t771;
  static struct u_object l_Program_773 = U_LIT("Program");
  u_value l774 = f_ctx_46_names_of_2(v_c, &l_Program_773);
  while (u_is_con(l774, &n_list_46_Cons)) {
    u_value v_d = u_field(l774, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e772 = u_append(e772, v_d);
    }
    l774 = u_field(l774, 1);
  }
  u_value s775 = t771;
  u_value t776 = NULL;
  if ((!t776) && u_is_con(s775, &n_list_46_Cons)) {
    u_value v_d = u_field(s775, 0);
    static struct u_object l_read_777 = U_LIT("read");
    static struct u_object l_library_778 = U_LIT("library");
    t776 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_777), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_778), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t776) {
    t776 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t776) {
    u_no_case(s775);
  }
  return t776;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value s779 = f_list_46_lookup_2(v_p, v_env);
    int d780 = 0;
    if ((!d780) && u_is_con(s779, &n_option_46_Some)) {
      u_value v_sc = u_field(s779, 0);
      d780 = 1;
      {
        p_check_46_call_variable(v_env, v_assignable, v_p, v_sc, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d780) && u_is_con(s779, &n_option_46_None)) {
      d780 = 1;
      {
        p_check_46_call_declared(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if (!d780) {
      u_no_case(s779);
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
    u_value t781 = u_mk_con(&n_syntax_46_SCall, 4, v_p, v_ins1, v_outs1, v_refs1);
    *r_y = t781;
  }
}

void p_check_46_call_declared(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s783 = f_check_46_visible_names_3(*r_s, v_p, u_mk_closure(lam782, 1, *r_s));
    int d784 = 0;
    if ((!d784) && (u_is_con(s783, &n_list_46_Cons) && u_is_con(u_field(s783, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s783, 0);
      d784 = 1;
      {
        u_value t785 = v_q1;
        v_q = t785;
      }
    }
    if ((!d784) && u_is_con(s783, &n_list_46_Nil)) {
      d784 = 1;
      {
        u_value t786 = v_p;
        v_q = t786;
      }
    }
    if (!d784) {
      u_value v_qs = s783;
      d784 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d784) {
      u_no_case(s783);
    }
    u_value s787 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_State_46_ctx_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_State_46_ctx_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_State_46_ctx_1(*r_s)))));
    int d788 = 0;
    if ((!d788) && u_is_con(s787, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s787, 0), 0);
      u_value v_is = u_field(u_field(s787, 0), 1);
      u_value v_os = u_field(u_field(s787, 0), 2);
      u_value v_rs = u_field(u_field(s787, 0), 3);
      d788 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t789 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e790 = &t789;
        u_value l791 = v_is;
        while (u_is_con(l791, &n_list_46_Cons)) {
          u_value v_t = u_field(l791, 0);
          if (u_truth(u_mk_bool(1))) {
            e790 = u_append(e790, f_check_46_subst_2(v_t, v_m));
          }
          l791 = u_field(l791, 1);
        }
        p_check_46_checks(v_env, v_ins, t789, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t792 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e793 = &t792;
        u_value l794 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l794, &n_list_46_Cons)) {
          u_value v_t = u_field(l794, 0);
          if (u_truth(u_mk_bool(1))) {
            e793 = u_append(e793, f_check_46_subst_2(v_t, v_m));
          }
          l794 = u_field(l794, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t792, r_s);
        u_value t795 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t795;
      }
    }
    if ((!d788) && u_is_con(s787, &n_option_46_None)) {
      d788 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d788) {
      u_no_case(s787);
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
    u_value s796 = v_cases;
    int d797 = 0;
    if ((!d797) && u_is_con(s796, &n_list_46_Nil)) {
      d797 = 1;
      {
        u_value t798 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t798;
      }
    }
    if ((!d797) && (u_is_con(s796, &n_list_46_Cons) && u_is_con(u_field(s796, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s796, 0), 0);
      u_value v_g = u_field(u_field(s796, 0), 1);
      u_value v_b = u_field(u_field(s796, 0), 2);
      u_value v_more = u_field(s796, 1);
      d797 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t799 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t799;
      }
    }
    if (!d797) {
      u_no_case(s796);
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
    u_value s800 = v_hs;
    int d801 = 0;
    if ((!d801) && u_is_con(s800, &n_list_46_Nil)) {
      d801 = 1;
      {
        u_value t802 = u_mk_con(&n_list_46_Nil, 0);
        *r_hs1 = t802;
      }
    }
    if ((!d801) && (u_is_con(s800, &n_list_46_Cons) && u_is_con(u_field(s800, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s800, 0), 0);
      u_value v_p = u_field(u_field(s800, 0), 1);
      u_value v_g = u_field(u_field(s800, 0), 2);
      u_value v_b = u_field(u_field(s800, 0), 3);
      u_value v_more = u_field(s800, 1);
      d801 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t803 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t803;
      }
    }
    if (!d801) {
      u_no_case(s800);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t804 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t804)) {
      t804 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t805 = t804;
    if (!u_truth(t805)) {
      t805 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t806 = t805;
    if (!u_truth(t806)) {
      t806 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n))), u_mk_con(&n_list_46_Nil, 0));
    }
    if (u_truth(t806)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t807 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t807)) {
      t807 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t808 = t807;
    if (!u_truth(t808)) {
      t808 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t808)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_809 = U_LIT("Bool");
    static struct u_object l_Int_810 = U_LIT("Int");
    static struct u_object l_Str_811 = U_LIT("Str");
    static struct u_object l_Char_812 = U_LIT("Char");
    static struct u_object l__45__62__813 = U_LIT("->");
    u_value t814 = f_list_46_contains_2(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_809, u_mk_con(&n_list_46_Cons, 2, &l_Int_810, u_mk_con(&n_list_46_Cons, 2, &l_Str_811, u_mk_con(&n_list_46_Cons, 2, &l_Char_812, u_mk_con(&n_list_46_Cons, 2, &l__45__62__813, u_mk_con(&n_list_46_Nil, 0)))))));
    if (!u_truth(t814)) {
      t814 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t815 = t814;
    if (!u_truth(t815)) {
      t815 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t815)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t816 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e817 = &t816;
    u_value l818 = v_ts;
    while (u_is_con(l818, &n_list_46_Cons)) {
      u_value v_t = u_field(l818, 0);
      if (u_truth(u_mk_bool(1))) {
        e817 = u_append(e817, f_check_46_free_1(v_t));
      }
      l818 = u_field(l818, 1);
    }
    u_value t819 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t816)), f_check_46_params_1(v_ps));
    v_undeclared = t819;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, u_mk_con(&n_list_46_Nil, 0)))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
  {
    u_value s820 = v_cs;
    int d821 = 0;
    if ((!d821) && u_is_con(s820, &n_list_46_Nil)) {
      d821 = 1;
      {
      }
    }
    if ((!d821) && (u_is_con(s820, &n_list_46_Cons) && u_is_con(u_field(u_field(s820, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s820, 0), 0);
      u_value v_n = u_field(u_field(u_field(s820, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s820, 0), 1), 1);
      u_value v_rest = u_field(s820, 1);
      d821 = 1;
      {
        u_value t822 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t822;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        u_value t823 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t823;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d821) && u_is_con(s820, &n_list_46_Cons)) {
      u_value v_rest = u_field(s820, 1);
      d821 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d821) {
      u_no_case(s820);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s824 = v_cs;
    int d825 = 0;
    if ((!d825) && u_is_con(s824, &n_list_46_Nil)) {
      d825 = 1;
      {
      }
    }
    if ((!d825) && (u_is_con(s824, &n_list_46_Cons) && u_is_con(u_field(u_field(s824, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s824, 0), 0);
      u_value v_n = u_field(u_field(u_field(s824, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s824, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s824, 0), 1), 2);
      u_value v_rest = u_field(s824, 1);
      d825 = 1;
      {
        u_value t826 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t826;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)));
        u_value t827 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_State_46_ctx_1(*r_s), v_n, v_ps, v_u));
        *r_s = t827;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d825) && u_is_con(s824, &n_list_46_Cons)) {
      u_value v_rest = u_field(s824, 1);
      d825 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d825) {
      u_no_case(s824);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
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
      u_value v_ks = u_field(u_field(u_field(s828, 0), 1), 2);
      u_value v_rest = u_field(s828, 1);
      d829 = 1;
      {
        u_value t830 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t830;
        p_check_46_unique_ids(v_ps);
        u_value t831 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e832 = &t831;
        u_value l833 = v_ks;
        while (u_is_con(l833, &n_list_46_Cons)) {
          u_value v_k = u_field(l833, 0);
          if (u_truth(u_mk_bool(1))) {
            e832 = u_append(e832, f_syntax_46_Constr_46_name_1(v_k));
          }
          l833 = u_field(l833, 1);
        }
        u_value t834 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_State_46_ctx_1(*r_s), v_n, t831));
        *r_s = t834;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d829) && u_is_con(s828, &n_list_46_Cons)) {
      u_value v_rest = u_field(s828, 1);
      d829 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d829) {
      u_no_case(s828);
    }
  }
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s835 = v_ks;
    int d836 = 0;
    if ((!d836) && u_is_con(s835, &n_list_46_Nil)) {
      d836 = 1;
      {
      }
    }
    if ((!d836) && (u_is_con(s835, &n_list_46_Cons) && u_is_con(u_field(s835, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s835, 0), 0);
      u_value v_args = u_field(u_field(s835, 0), 1);
      u_value v_rest = u_field(s835, 1);
      d836 = 1;
      {
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t837 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_State_46_ctx_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t837;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d836) {
      u_no_case(s835);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s838 = v_xss;
  u_value t839 = NULL;
  if ((!t839) && u_is_con(s838, &n_list_46_Nil)) {
    t839 = v_r;
  }
  if ((!t839) && u_is_con(s838, &n_list_46_Cons)) {
    u_value v_xs = u_field(s838, 0);
    u_value v_rest = u_field(s838, 1);
    t839 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t839) {
    u_no_case(s838);
  }
  return t839;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s840 = v_xss;
  u_value t841 = NULL;
  if ((!t841) && u_is_con(s840, &n_list_46_Nil)) {
    t841 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t841) && u_is_con(s840, &n_list_46_Cons)) {
    u_value v_xs = u_field(s840, 0);
    t841 = v_xs;
  }
  if (!t841) {
    u_no_case(s840);
  }
  return t841;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s842 = v_xss;
    int d843 = 0;
    if ((!d843) && u_is_con(s842, &n_list_46_Nil)) {
      d843 = 1;
      {
        u_value t844 = u_mk_con(&n_list_46_Nil, 0);
        *r_yss = t844;
      }
    }
    if ((!d843) && u_is_con(s842, &n_list_46_Cons)) {
      u_value v_xs = u_field(s842, 0);
      u_value v_more = u_field(s842, 1);
      d843 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t845 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t845;
      }
    }
    if (!d843) {
      u_no_case(s842);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s846 = v_cs;
    int d847 = 0;
    if ((!d847) && u_is_con(s846, &n_list_46_Nil)) {
      d847 = 1;
      {
      }
    }
    if ((!d847) && (u_is_con(s846, &n_list_46_Cons) && u_is_con(u_field(u_field(s846, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s846, 0), 0);
      u_value v_n = u_field(u_field(u_field(s846, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s846, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s846, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s846, 0), 1), 3);
      u_value v_rest = u_field(s846, 1);
      d847 = 1;
      {
        u_value t848 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t848;
        p_check_46_overload(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), u_mk_con(&n_list_46_Nil, 0)));
        u_value t849 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t849;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d847) && u_is_con(s846, &n_list_46_Cons)) {
      u_value v_rest = u_field(s846, 1);
      d847 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d847) {
      u_no_case(s846);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s850 = v_cs;
    int d851 = 0;
    if ((!d851) && u_is_con(s850, &n_list_46_Nil)) {
      d851 = 1;
      {
      }
    }
    if ((!d851) && (u_is_con(s850, &n_list_46_Cons) && u_is_con(u_field(u_field(s850, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s850, 0), 0);
      u_value v_n = u_field(u_field(u_field(s850, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s850, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s850, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s850, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s850, 0), 1), 4);
      u_value v_rest = u_field(s850, 1);
      d851 = 1;
      {
        u_value t852 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t852;
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t853 = f_syntax_46_types_1(v_xs);
        v_ts = t853;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t854 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t854;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d851) && u_is_con(s850, &n_list_46_Cons)) {
      u_value v_rest = u_field(s850, 1);
      d851 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d851) {
      u_no_case(s850);
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
    u_value s855 = v_c;
    int d856 = 0;
    if ((!d856) && u_is_con(s855, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s855, 0);
      u_value v_ps = u_field(s855, 1);
      u_value v_bound = u_field(s855, 2);
      u_value v_res = u_field(s855, 3);
      u_value v_body = u_field(s855, 4);
      d856 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t857 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t857;
      }
    }
    if ((!d856) && u_is_con(s855, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s855, 0);
      u_value v_ps = u_field(s855, 1);
      u_value v_ins = u_field(s855, 2);
      u_value v_outs = u_field(s855, 3);
      u_value v_refs = u_field(s855, 4);
      u_value v_body = u_field(s855, 5);
      d856 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t858 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t858;
      }
    }
    if ((!d856) && u_is_con(s855, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s855, 0);
      d856 = 1;
      {
        p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_b, &v_t, r_s);
        u_value t859 = u_mk_con(&n_syntax_46_CEval, 1, v_b);
        *r_c1 = t859;
      }
    }
    if ((!d856) && u_is_con(s855, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s855, 0);
      u_value v_body = u_field(s855, 1);
      d856 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t860 = u_mk_con(&n_syntax_46_CRun, 2, v_n, v_y);
        *r_c1 = t860;
      }
    }
    if ((!d856) && u_is_con(s855, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s855, 0);
      u_value v_body = u_field(s855, 1);
      d856 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t861 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t861;
      }
    }
    if (!d856) {
      d856 = 1;
      {
        u_value t862 = v_c;
        *r_c1 = t862;
      }
    }
    if (!d856) {
      u_no_case(s855);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s863 = v_cs;
    int d864 = 0;
    if ((!d864) && u_is_con(s863, &n_list_46_Nil)) {
      d864 = 1;
      {
        u_value t865 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t865;
      }
    }
    if ((!d864) && u_is_con(s863, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s863, 0), 0);
      u_value v_c = u_field(u_field(s863, 0), 1);
      u_value v_more = u_field(s863, 1);
      d864 = 1;
      {
        u_value t866 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t866;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t867 = f_check_46_zonk_cmd_2(f_check_46_State_46_sub_1(*r_s), v_c1);
        v_c2 = t867;
        u_value s868 = f_check_46_unknown_in_cmd_1(v_c2);
        int d869 = 0;
        if ((!d869) && u_is_con(s868, &n_option_46_Some)) {
          u_value v_t = u_field(s868, 0);
          d869 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d869) && u_is_con(s868, &n_option_46_None)) {
          d869 = 1;
          {
          }
        }
        if (!d869) {
          u_no_case(s868);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t870 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t870;
      }
    }
    if (!d864) {
      u_no_case(s863);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s871 = v_cs;
  u_value t872 = NULL;
  if ((!t872) && u_is_con(s871, &n_list_46_Nil)) {
    t872 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t872) && (u_is_con(s871, &n_list_46_Cons) && u_is_con(u_field(u_field(s871, 0), 1), &n_syntax_46_CRun))) {
    u_value v_n = u_field(u_field(u_field(s871, 0), 1), 0);
    u_value v_more = u_field(s871, 1);
    t872 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t872) && (u_is_con(s871, &n_list_46_Cons) && u_is_con(u_field(u_field(s871, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s871, 0), 1), 0);
    u_value v_more = u_field(s871, 1);
    t872 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t872) && u_is_con(s871, &n_list_46_Cons)) {
    u_value v_more = u_field(s871, 1);
    t872 = f_check_46_block_names_1(v_more);
  }
  if (!t872) {
    u_no_case(s871);
  }
  return t872;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_s = NULL;
    u_value v_ds = NULL;
    u_value v_cs = NULL;
    u_value s873 = v_p;
    int d874 = 0;
    if ((!d874) && u_is_con(s873, &n_syntax_46_Program)) {
      u_value v_cmds = u_field(s873, 0);
      d874 = 1;
      {
        u_value t875 = f_check_46_flatten_2(v_main, v_cmds);
        v_ds = t875;
        u_value t876 = f_check_46_start_2(v_main, f_check_46_visibility_2(v_ds, v_main));
        v_s = t876;
        p_check_46_declare_datas(v_ds, &v_s);
        p_check_46_declare_sorts(v_ds, &v_s);
        p_check_46_declare_constrs(v_ds, &v_s);
        p_check_46_declare_funs(v_ds, &v_s);
        p_check_46_declare_procs(v_ds, &v_s);
        p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
        p_check_46_commands(v_ds, &v_cs, &v_s);
        u_value t877 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
        *r_q = t877;
      }
    }
    if (!d874) {
      u_no_case(s873);
    }
  }
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_878 = U_LIT("main");
    u_value t879 = f_check_46_start_2(&l_main_878, u_mk_con(&n_list_46_Nil, 0));
    v_s = t879;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t880 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(v_s), v_u);
    *r_t = t880;
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
  static struct u_object l_List_881 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_881), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_882 = U_LIT("list");
  static struct u_object l_List_883 = U_LIT("List");
  static struct u_object l_A_884 = U_LIT("A");
  static struct u_object l_Nil_885 = U_LIT("Nil");
  static struct u_object l_Cons_886 = U_LIT("Cons");
  static struct u_object l_A_887 = U_LIT("A");
  static struct u_object l_A_888 = U_LIT("A");
  static struct u_object l_list_889 = U_LIT("list");
  static struct u_object l_len_890 = U_LIT("len");
  static struct u_object l_A_891 = U_LIT("A");
  static struct u_object l_xs_892 = U_LIT("xs");
  static struct u_object l_A_893 = U_LIT("A");
  static struct u_object l_xs_894 = U_LIT("xs");
  static struct u_object l_Nil_895 = U_LIT("Nil");
  static struct u_object l_Cons_896 = U_LIT("Cons");
  static struct u_object l_ys_897 = U_LIT("ys");
  static struct u_object l__43__898 = U_LIT("+");
  static struct u_object l_len_899 = U_LIT("len");
  static struct u_object l_ys_900 = U_LIT("ys");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_882, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_883), u_mk_con(&n_list_46_Cons, 2, &l_A_884, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_885), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_886), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_887), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_888)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_889), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_890), u_mk_con(&n_list_46_Cons, 2, &l_A_891, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_892), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_893))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_894), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_895), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_896), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_897), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__898, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_899, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_900), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s901 = v_t;
  u_value t902 = NULL;
  if ((!t902) && u_is_con(s901, &n_c_46_CtVoid)) {
    static struct u_object l_void_903 = U_LIT("void");
    t902 = &l_void_903;
  }
  if ((!t902) && u_is_con(s901, &n_c_46_CtInt)) {
    static struct u_object l_int_904 = U_LIT("int");
    t902 = &l_int_904;
  }
  if ((!t902) && u_is_con(s901, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s901, 0);
    t902 = v_n;
  }
  if ((!t902) && (u_is_con(s901, &n_c_46_CtPtr) && u_is_con(u_field(s901, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s901, 0), 0);
    static struct u_object l__42__905 = U_LIT("*");
    t902 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__905);
  }
  if ((!t902) && u_is_con(s901, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s901, 0);
    static struct u_object l__32__42__906 = U_LIT(" *");
    t902 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__906);
  }
  if (!t902) {
    u_no_case(s901);
  }
  return t902;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s907 = v_t;
  u_value t908 = NULL;
  if ((!t908) && u_is_con(s907, &n_c_46_CtPtr)) {
    t908 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t908) {
    static struct u_object l__32__909 = U_LIT(" ");
    t908 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__909, v_x));
  }
  if (!t908) {
    u_no_case(s907);
  }
  return t908;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s910 = v_e;
  u_value t911 = NULL;
  if ((!t911) && u_is_con(s910, &n_c_46_CeVar)) {
    u_value v_x = u_field(s910, 0);
    t911 = v_x;
  }
  if ((!t911) && u_is_con(s910, &n_c_46_CeNum)) {
    u_value v_n = u_field(s910, 0);
    t911 = u_str_show(v_n);
  }
  if ((!t911) && u_is_con(s910, &n_c_46_CeStr)) {
    u_value v_s = u_field(s910, 0);
    t911 = u_str_quote(v_s);
  }
  if ((!t911) && u_is_con(s910, &n_c_46_CeCall)) {
    u_value v_f = u_field(s910, 0);
    u_value v_args = u_field(s910, 1);
    static struct u_object l__40__912 = U_LIT("(");
    static struct u_object l__44__32__913 = U_LIT(", ");
    u_value t914 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e915 = &t914;
    u_value l916 = v_args;
    while (u_is_con(l916, &n_list_46_Cons)) {
      u_value v_a = u_field(l916, 0);
      if (u_truth(u_mk_bool(1))) {
        e915 = u_append(e915, f_c_46_c_expr_1(v_a));
      }
      l916 = u_field(l916, 1);
    }
    static struct u_object l__41__917 = U_LIT(")");
    t911 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__912, u_concat(u_str_join(&l__44__32__913, t914), &l__41__917)));
  }
  if ((!t911) && u_is_con(s910, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s910, 0);
    u_value v_a = u_field(s910, 1);
    t911 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t911) && u_is_con(s910, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s910, 0);
    u_value v_a = u_field(s910, 1);
    u_value v_b = u_field(s910, 2);
    static struct u_object l__32__918 = U_LIT(" ");
    static struct u_object l__32__919 = U_LIT(" ");
    t911 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__918, u_concat(v_o, u_concat(&l__32__919, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t911) && u_is_con(s910, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s910, 0);
    u_value v_i = u_field(s910, 1);
    static struct u_object l__91__920 = U_LIT("[");
    static struct u_object l__93__921 = U_LIT("]");
    t911 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__920, u_concat(f_c_46_c_expr_1(v_i), &l__93__921)));
  }
  if ((!t911) && u_is_con(s910, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s910, 0);
    u_value v_f = u_field(s910, 1);
    static struct u_object l__45__62__922 = U_LIT("->");
    t911 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__922, v_f));
  }
  if ((!t911) && u_is_con(s910, &n_c_46_CeCast)) {
    u_value v_t = u_field(s910, 0);
    u_value v_a = u_field(s910, 1);
    static struct u_object l__40__923 = U_LIT("(");
    static struct u_object l__41__924 = U_LIT(")");
    t911 = u_concat(&l__40__923, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__924, f_c_46_c_operand_1(v_a))));
  }
  if ((!t911) && u_is_con(s910, &n_c_46_CeCond)) {
    u_value v_c = u_field(s910, 0);
    u_value v_a = u_field(s910, 1);
    u_value v_b = u_field(s910, 2);
    static struct u_object l__32__63__32__925 = U_LIT(" ? ");
    static struct u_object l__32__58__32__926 = U_LIT(" : ");
    t911 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__925, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__926, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t911) && u_is_con(s910, &n_c_46_CeInit)) {
    u_value v_es = u_field(s910, 0);
    static struct u_object l__123__927 = U_LIT("{");
    static struct u_object l__44__32__928 = U_LIT(", ");
    u_value t929 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e930 = &t929;
    u_value l931 = v_es;
    while (u_is_con(l931, &n_list_46_Cons)) {
      u_value v_a = u_field(l931, 0);
      if (u_truth(u_mk_bool(1))) {
        e930 = u_append(e930, f_c_46_c_expr_1(v_a));
      }
      l931 = u_field(l931, 1);
    }
    static struct u_object l__125__932 = U_LIT("}");
    t911 = u_concat(&l__123__927, u_concat(u_str_join(&l__44__32__928, t929), &l__125__932));
  }
  if (!t911) {
    u_no_case(s910);
  }
  return t911;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s933 = v_e;
  u_value t934 = NULL;
  if ((!t934) && u_is_con(s933, &n_c_46_CeVar)) {
    t934 = f_c_46_c_expr_1(v_e);
  }
  if ((!t934) && u_is_con(s933, &n_c_46_CeNum)) {
    u_value v_n = u_field(s933, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t934 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t934) && u_is_con(s933, &n_c_46_CeStr)) {
    t934 = f_c_46_c_expr_1(v_e);
  }
  if ((!t934) && u_is_con(s933, &n_c_46_CeCall)) {
    t934 = f_c_46_c_expr_1(v_e);
  }
  if ((!t934) && u_is_con(s933, &n_c_46_CeIndex)) {
    t934 = f_c_46_c_expr_1(v_e);
  }
  if ((!t934) && u_is_con(s933, &n_c_46_CeArrow)) {
    t934 = f_c_46_c_expr_1(v_e);
  }
  if (!t934) {
    static struct u_object l__40__935 = U_LIT("(");
    static struct u_object l__41__936 = U_LIT(")");
    t934 = u_concat(&l__40__935, u_concat(f_c_46_c_expr_1(v_e), &l__41__936));
  }
  if (!t934) {
    u_no_case(s933);
  }
  return t934;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s937 = v_s;
  u_value t938 = NULL;
  if ((!t938) && u_is_con(s937, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s937, 0);
    static struct u_object l__59__939 = U_LIT(";");
    t938 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__939)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t938) && (u_is_con(s937, &n_c_46_CsDecl) && u_is_con(u_field(s937, 2), &n_option_46_None))) {
    u_value v_t = u_field(s937, 0);
    u_value v_x = u_field(s937, 1);
    static struct u_object l__59__940 = U_LIT(";");
    t938 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__940)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t938) && (u_is_con(s937, &n_c_46_CsDecl) && u_is_con(u_field(s937, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s937, 0);
    u_value v_x = u_field(s937, 1);
    u_value v_e = u_field(u_field(s937, 2), 0);
    static struct u_object l__32__61__32__941 = U_LIT(" = ");
    static struct u_object l__59__942 = U_LIT(";");
    t938 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__941, u_concat(f_c_46_c_expr_1(v_e), &l__59__942)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t938) && u_is_con(s937, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s937, 0);
    u_value v_e = u_field(s937, 1);
    static struct u_object l__32__61__32__943 = U_LIT(" = ");
    static struct u_object l__59__944 = U_LIT(";");
    t938 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__943, u_concat(f_c_46_c_expr_1(v_e), &l__59__944)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t938) && (u_is_con(s937, &n_c_46_CsIf) && u_is_con(u_field(s937, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s937, 0);
    u_value v_l = u_field(s937, 1);
    static struct u_object l_if_32__40__945 = U_LIT("if (");
    static struct u_object l__41__32__123__946 = U_LIT(") {");
    static struct u_object l__32__32__947 = U_LIT("  ");
    static struct u_object l__125__948 = U_LIT("}");
    t938 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__945, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__946))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__947), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__948), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t938) && u_is_con(s937, &n_c_46_CsIf)) {
    u_value v_c = u_field(s937, 0);
    u_value v_l = u_field(s937, 1);
    u_value v_r = u_field(s937, 2);
    static struct u_object l_if_32__40__949 = U_LIT("if (");
    static struct u_object l__41__32__123__950 = U_LIT(") {");
    static struct u_object l__32__32__951 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__952 = U_LIT("} else {");
    static struct u_object l__32__32__953 = U_LIT("  ");
    static struct u_object l__125__954 = U_LIT("}");
    t938 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__949, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__950))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__951), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__952), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__953), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__954), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t938) && u_is_con(s937, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s937, 0);
    u_value v_b = u_field(s937, 1);
    static struct u_object l_while_32__40__955 = U_LIT("while (");
    static struct u_object l__41__32__123__956 = U_LIT(") {");
    static struct u_object l__32__32__957 = U_LIT("  ");
    static struct u_object l__125__958 = U_LIT("}");
    t938 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__955, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__956))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__957), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__958), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t938) && (u_is_con(s937, &n_c_46_CsReturn) && u_is_con(u_field(s937, 0), &n_option_46_None))) {
    static struct u_object l_return_59__959 = U_LIT("return;");
    t938 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__959), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t938) && (u_is_con(s937, &n_c_46_CsReturn) && u_is_con(u_field(s937, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s937, 0), 0);
    static struct u_object l_return_32__960 = U_LIT("return ");
    static struct u_object l__59__961 = U_LIT(";");
    t938 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__960, u_concat(f_c_46_c_expr_1(v_e), &l__59__961))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t938) && u_is_con(s937, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s937, 0);
    static struct u_object l__123__962 = U_LIT("{");
    static struct u_object l__32__32__963 = U_LIT("  ");
    static struct u_object l__125__964 = U_LIT("}");
    t938 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__962), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__963), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__964), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t938) {
    u_no_case(s937);
  }
  return t938;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t965 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e966 = &t965;
  u_value l967 = v_ss;
  while (u_is_con(l967, &n_list_46_Cons)) {
    u_value v_s = u_field(l967, 0);
    if (u_truth(u_mk_bool(1))) {
      e966 = u_append(e966, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l967 = u_field(l967, 1);
  }
  return f_list_46_concat_1(t965);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s968 = v_ps;
  u_value t969 = NULL;
  if ((!t969) && u_is_con(s968, &n_list_46_Nil)) {
    static struct u_object l_void_970 = U_LIT("void");
    t969 = &l_void_970;
  }
  if (!t969) {
    static struct u_object l__44__32__971 = U_LIT(", ");
    u_value t972 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e973 = &t972;
    u_value l974 = v_ps;
    while (u_is_con(l974, &n_list_46_Cons)) {
      u_value v_p = u_field(l974, 0);
      if (u_truth(u_mk_bool(1))) {
        e973 = u_append(e973, f_c_46_c_param_1(v_p));
      }
      l974 = u_field(l974, 1);
    }
    t969 = u_str_join(&l__44__32__971, t972);
  }
  if (!t969) {
    u_no_case(s968);
  }
  return t969;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s975 = v_p;
  u_value t976 = NULL;
  if (!t976) {
    u_value v_t = u_field(s975, 0);
    u_value v_x = u_field(s975, 1);
    t976 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t976) {
    u_no_case(s975);
  }
  return t976;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s977 = v_d;
  u_value t978 = NULL;
  if ((!t978) && u_is_con(s977, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s977, 0);
    static struct u_object l__35_include_32__979 = U_LIT("#include ");
    t978 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__979, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t978) && u_is_con(s977, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s977, 0);
    u_value v_f = u_field(s977, 1);
    u_value v_ps = u_field(s977, 2);
    static struct u_object l__40__980 = U_LIT("(");
    static struct u_object l__41__59__981 = U_LIT(");");
    t978 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__980, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__981))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t978) && u_is_con(s977, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s977, 0);
    u_value v_f = u_field(s977, 1);
    u_value v_ps = u_field(s977, 2);
    u_value v_b = u_field(s977, 3);
    static struct u_object l__40__982 = U_LIT("(");
    static struct u_object l__41__32__123__983 = U_LIT(") {");
    static struct u_object l__32__32__984 = U_LIT("  ");
    static struct u_object l__125__985 = U_LIT("}");
    t978 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__982, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__983))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__984, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__985, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t978) && (u_is_con(s977, &n_c_46_CdGlobal) && u_is_con(u_field(s977, 2), &n_option_46_None))) {
    u_value v_t = u_field(s977, 0);
    u_value v_x = u_field(s977, 1);
    static struct u_object l__59__986 = U_LIT(";");
    t978 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__986), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t978) && (u_is_con(s977, &n_c_46_CdGlobal) && u_is_con(u_field(s977, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s977, 0);
    u_value v_x = u_field(s977, 1);
    u_value v_e = u_field(u_field(s977, 2), 0);
    static struct u_object l__32__61__32__987 = U_LIT(" = ");
    static struct u_object l__59__988 = U_LIT(";");
    t978 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__987, u_concat(f_c_46_c_expr_1(v_e), &l__59__988))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t978) {
    u_no_case(s977);
  }
  return t978;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s989 = v_f;
  u_value t990 = NULL;
  if ((!t990) && u_is_con(s989, &n_c_46_CFile)) {
    u_value v_ds = u_field(s989, 0);
    static struct u_object l__10__10__991 = U_LIT("\n\n");
    u_value t992 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e993 = &t992;
    u_value l994 = v_ds;
    while (u_is_con(l994, &n_list_46_Cons)) {
      u_value v_d = u_field(l994, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__995 = U_LIT("\n");
        e993 = u_append(e993, u_str_join(&l__10__995, f_c_46_c_decl_1(v_d)));
      }
      l994 = u_field(l994, 1);
    }
    static struct u_object l__10__996 = U_LIT("\n");
    t990 = u_concat(u_str_join(&l__10__10__991, t992), &l__10__996);
  }
  if (!t990) {
    u_no_case(s989);
  }
  return t990;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__997 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__997, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__123__125__44__59__46__998 = U_LIT("()[]{},;.");
  return u_str_contains(&l__40__41__91__93__123__125__44__59__46__998, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t999 = u_is_letter(v_c);
  if (!u_truth(t999)) {
    t999 = u_is_digit(v_c);
  }
  u_value t1000 = t999;
  if (!u_truth(t1000)) {
    t1000 = u_eq(v_c, u_mk_char(95));
  }
  return t1000;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t1001;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t1001 = u_str_get(v_s, v_i);
  } else {
    t1001 = u_chr(u_mk_int(0));
  }
  return t1001;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s1002 = v_c;
  u_value t1003 = NULL;
  if ((!t1003) && u_equals(s1002, u_mk_char(34))) {
    t1003 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t1003) && u_equals(s1002, u_mk_char(39))) {
    t1003 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t1003) && u_equals(s1002, u_mk_char(92))) {
    t1003 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t1003) && u_equals(s1002, u_mk_char(110))) {
    t1003 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t1003) && u_equals(s1002, u_mk_char(116))) {
    t1003 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t1003) && u_equals(s1002, u_mk_char(114))) {
    t1003 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t1003) {
    t1003 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1003) {
    u_no_case(s1002);
  }
  return t1003;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t1004;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t1004 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t1004 = v_i;
  }
  return t1004;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t1005 = u_mk_int(0);
    u_value t1006 = u_mk_int(1);
    u_value t1007 = u_mk_int(1);
    v_j = t1005;
    *r_line = t1006;
    *r_column = t1007;
    u_value t1012 = u_lt(v_j, v_i);
    while (u_truth(t1012)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t1008 = u_add(*r_line, u_mk_int(1));
            u_value t1009 = u_mk_int(1);
            *r_line = t1008;
            *r_column = t1009;
          }
        } else {
          {
            u_value t1010 = u_add(*r_column, u_mk_int(1));
            *r_column = t1010;
          }
        }
        u_value t1011 = u_add(v_j, u_mk_int(1));
        v_j = t1011;
      }
      t1012 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t1013 = *r_i;
    u_value t1014 = u_add(*r_i, u_mk_int(1));
    u_value t1015 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t1013;
    *r_i = t1014;
    v_parts = t1015;
    u_value t1016 = *r_i;
    v_from = t1016;
    u_value t1027 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t1027)) {
      {
        u_value t1017 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t1017;
        u_value t1018 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t1018)) {
          t1018 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t1018)) {
          {
            static struct u_object l_unterminated_32_lit_1019 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_1019, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s1020 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d1021 = 0;
            if ((!d1021) && u_is_con(s1020, &n_option_46_Some)) {
              u_value v_d = u_field(s1020, 0);
              d1021 = 1;
              {
                u_value t1022 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t1023 = u_add(*r_i, u_mk_int(2));
                v_parts = t1022;
                *r_i = t1023;
                u_value t1024 = *r_i;
                v_from = t1024;
              }
            }
            if ((!d1021) && u_is_con(s1020, &n_option_46_None)) {
              d1021 = 1;
              {
                static struct u_object l_invalid_32_escape_1025 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_1025, *r_i), "lex.LexError");
              }
            }
            if (!d1021) {
              u_no_case(s1020);
            }
          }
        } else {
          {
            u_value t1026 = u_add(*r_i, u_mk_int(1));
            *r_i = t1026;
          }
        }
      }
      t1027 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__1028 = U_LIT("");
    u_value t1029 = u_str_join(&l__1028, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t1030 = u_add(*r_i, u_mk_int(1));
    *r_text = t1029;
    *r_i = t1030;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1031 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t1031;
    u_value t1032 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1033 = v_j;
    *r_t = t1032;
    *r_i = t1033;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1034 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t1034;
    u_value s1035 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d1036 = 0;
    if ((!d1036) && u_is_con(s1035, &n_option_46_Some)) {
      u_value v_n = u_field(s1035, 0);
      d1036 = 1;
      {
        u_value t1037 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t1038 = v_j;
        *r_t = t1037;
        *r_i = t1038;
      }
    }
    if ((!d1036) && u_is_con(s1035, &n_option_46_None)) {
      d1036 = 1;
      {
        static struct u_object l_invalid_32_number_1039 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_1039, *r_i), "lex.LexError");
      }
    }
    if (!d1036) {
      u_no_case(s1035);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1040 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t1040;
    u_value t1041 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1042 = v_j;
    *r_t = t1041;
    *r_i = t1042;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t1043 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t1043;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t1044 = *r_i;
    v_start = t1044;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_1045 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_1045, v_start), "lex.LexError");
      }
    }
    u_value t1046 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t1046;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t1047 = u_str_get(v_s, *r_i);
    v_c = t1047;
    u_value s1048 = v_c;
    int d1049 = 0;
    if (!d1049) {
      u_value t1050 = u_is_letter(v_c);
      if (!u_truth(t1050)) {
        t1050 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t1050)) {
        d1049 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d1049) {
      if (u_truth(u_is_digit(v_c))) {
        d1049 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d1049) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d1049 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d1049) && u_equals(s1048, u_mk_char(34))) {
      d1049 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d1049) && u_equals(s1048, u_mk_char(39))) {
      d1049 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d1049) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d1049 = 1;
        {
          u_value t1051 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t1052 = u_add(*r_i, u_mk_int(1));
          *r_t = t1051;
          *r_i = t1052;
        }
      }
    }
    if (!d1049) {
      d1049 = 1;
      {
        static struct u_object l_unexpected_32_chara_1053 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_1053, *r_i), "lex.LexError");
      }
    }
    if (!d1049) {
      u_no_case(s1048);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t1054 = u_mk_int(0);
    v_i = t1054;
    u_value t1055 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1055;
    u_value t1062 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t1062)) {
      {
        u_value t1056 = v_i;
        v_start = t1056;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t1057 = u_add(v_i, u_mk_int(1));
            v_i = t1057;
          }
        } else {
          u_value t1058 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t1058)) {
            t1058 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t1058)) {
            {
              u_value t1060 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam1059, 0));
              v_i = t1060;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t1061 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t1061;
            }
          }
        }
      }
      t1062 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t1063 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t1063;
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
  u_value s1064 = v_s;
  u_value t1065 = NULL;
  if ((!t1065) && u_is_con(s1064, &n_set_46_SLeaf)) {
    t1065 = u_mk_bool(0);
  }
  if ((!t1065) && u_is_con(s1064, &n_set_46_SNode)) {
    u_value v_l = u_field(s1064, 0);
    u_value v_y = u_field(s1064, 1);
    u_value v_r = u_field(s1064, 2);
    u_value s1066 = u_compare(v_x, v_y);
    u_value t1067 = NULL;
    if ((!t1067) && u_equals(s1066, u_mk_int(-1))) {
      t1067 = f_set_46_member_2(v_x, v_l);
    }
    if ((!t1067) && u_equals(s1066, u_mk_int(1))) {
      t1067 = f_set_46_member_2(v_x, v_r);
    }
    if (!t1067) {
      t1067 = u_mk_bool(1);
    }
    if (!t1067) {
      u_no_case(s1066);
    }
    t1065 = t1067;
  }
  if (!t1065) {
    u_no_case(s1064);
  }
  return t1065;
}

u_value f_set_46_insert_2(u_value v_x, u_value v_s) {
  u_value s1068 = v_s;
  u_value t1069 = NULL;
  if ((!t1069) && u_is_con(s1068, &n_set_46_SLeaf)) {
    t1069 = u_mk_con(&n_set_46_SNode, 4, u_mk_con(&n_set_46_SLeaf, 0), v_x, u_mk_con(&n_set_46_SLeaf, 0), u_mk_int(1));
  }
  if ((!t1069) && u_is_con(s1068, &n_set_46_SNode)) {
    u_value v_l = u_field(s1068, 0);
    u_value v_y = u_field(s1068, 1);
    u_value v_r = u_field(s1068, 2);
    u_value s1070 = u_compare(v_x, v_y);
    u_value t1071 = NULL;
    if ((!t1071) && u_equals(s1070, u_mk_int(-1))) {
      t1071 = f_set_46_rebalance_3(f_set_46_insert_2(v_x, v_l), v_y, v_r);
    }
    if ((!t1071) && u_equals(s1070, u_mk_int(1))) {
      t1071 = f_set_46_rebalance_3(v_l, v_y, f_set_46_insert_2(v_x, v_r));
    }
    if (!t1071) {
      t1071 = v_s;
    }
    if (!t1071) {
      u_no_case(s1070);
    }
    t1069 = t1071;
  }
  if (!t1069) {
    u_no_case(s1068);
  }
  return t1069;
}

u_value f_set_46_from_list_1(u_value v_xs) {
  return f_set_46_insert_all_2(v_xs, u_mk_con(&n_set_46_SLeaf, 0));
}

u_value f_set_46_insert_all_2(u_value v_xs, u_value v_s) {
  u_value s1072 = v_xs;
  u_value t1073 = NULL;
  if ((!t1073) && u_is_con(s1072, &n_list_46_Nil)) {
    t1073 = v_s;
  }
  if ((!t1073) && u_is_con(s1072, &n_list_46_Cons)) {
    u_value v_x = u_field(s1072, 0);
    u_value v_rest = u_field(s1072, 1);
    t1073 = f_set_46_insert_all_2(v_rest, f_set_46_insert_2(v_x, v_s));
  }
  if (!t1073) {
    u_no_case(s1072);
  }
  return t1073;
}

u_value f_set_46_elements_1(u_value v_s) {
  return f_set_46_elements_onto_2(v_s, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_set_46_elements_onto_2(u_value v_s, u_value v_rest) {
  u_value s1074 = v_s;
  u_value t1075 = NULL;
  if ((!t1075) && u_is_con(s1074, &n_set_46_SLeaf)) {
    t1075 = v_rest;
  }
  if ((!t1075) && u_is_con(s1074, &n_set_46_SNode)) {
    u_value v_l = u_field(s1074, 0);
    u_value v_x = u_field(s1074, 1);
    u_value v_r = u_field(s1074, 2);
    t1075 = f_set_46_elements_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, v_x, f_set_46_elements_onto_2(v_r, v_rest)));
  }
  if (!t1075) {
    u_no_case(s1074);
  }
  return t1075;
}

u_value f_set_46_count_1(u_value v_s) {
  u_value s1076 = v_s;
  u_value t1077 = NULL;
  if ((!t1077) && u_is_con(s1076, &n_set_46_SLeaf)) {
    t1077 = u_mk_int(0);
  }
  if ((!t1077) && u_is_con(s1076, &n_set_46_SNode)) {
    u_value v_l = u_field(s1076, 0);
    u_value v_r = u_field(s1076, 2);
    t1077 = u_add(u_add(f_set_46_count_1(v_l), u_mk_int(1)), f_set_46_count_1(v_r));
  }
  if (!t1077) {
    u_no_case(s1076);
  }
  return t1077;
}

u_value f_set_46_depth_1(u_value v_s) {
  u_value s1078 = v_s;
  u_value t1079 = NULL;
  if ((!t1079) && u_is_con(s1078, &n_set_46_SLeaf)) {
    t1079 = u_mk_int(0);
  }
  if ((!t1079) && u_is_con(s1078, &n_set_46_SNode)) {
    u_value v_d = u_field(s1078, 3);
    t1079 = v_d;
  }
  if (!t1079) {
    u_no_case(s1078);
  }
  return t1079;
}

u_value f_set_46_snode_depth_2(u_value v_l, u_value v_r) {
  u_value t1080;
  if (u_truth(u_lt(f_set_46_depth_1(v_l), f_set_46_depth_1(v_r)))) {
    t1080 = f_set_46_depth_1(v_r);
  } else {
    t1080 = f_set_46_depth_1(v_l);
  }
  return u_add(u_mk_int(1), t1080);
}

u_value f_set_46_snode_3(u_value v_l, u_value v_x, u_value v_r) {
  return u_mk_con(&n_set_46_SNode, 4, v_l, v_x, v_r, f_set_46_snode_depth_2(v_l, v_r));
}

u_value f_set_46_turn_left_1(u_value v_s) {
  u_value s1081 = v_s;
  u_value t1082 = NULL;
  if ((!t1082) && (u_is_con(s1081, &n_set_46_SNode) && u_is_con(u_field(s1081, 2), &n_set_46_SNode))) {
    u_value v_a = u_field(s1081, 0);
    u_value v_x = u_field(s1081, 1);
    u_value v_b = u_field(u_field(s1081, 2), 0);
    u_value v_y = u_field(u_field(s1081, 2), 1);
    u_value v_c = u_field(u_field(s1081, 2), 2);
    t1082 = f_set_46_snode_3(f_set_46_snode_3(v_a, v_x, v_b), v_y, v_c);
  }
  if (!t1082) {
    t1082 = v_s;
  }
  if (!t1082) {
    u_no_case(s1081);
  }
  return t1082;
}

u_value f_set_46_turn_right_1(u_value v_s) {
  u_value s1083 = v_s;
  u_value t1084 = NULL;
  if ((!t1084) && (u_is_con(s1083, &n_set_46_SNode) && u_is_con(u_field(s1083, 0), &n_set_46_SNode))) {
    u_value v_a = u_field(u_field(s1083, 0), 0);
    u_value v_x = u_field(u_field(s1083, 0), 1);
    u_value v_b = u_field(u_field(s1083, 0), 2);
    u_value v_y = u_field(s1083, 1);
    u_value v_c = u_field(s1083, 2);
    t1084 = f_set_46_snode_3(v_a, v_x, f_set_46_snode_3(v_b, v_y, v_c));
  }
  if (!t1084) {
    t1084 = v_s;
  }
  if (!t1084) {
    u_no_case(s1083);
  }
  return t1084;
}

u_value f_set_46_smaller_1(u_value v_s) {
  u_value s1085 = v_s;
  u_value t1086 = NULL;
  if ((!t1086) && u_is_con(s1085, &n_set_46_SNode)) {
    u_value v_l = u_field(s1085, 0);
    t1086 = v_l;
  }
  if (!t1086) {
    t1086 = v_s;
  }
  if (!t1086) {
    u_no_case(s1085);
  }
  return t1086;
}

u_value f_set_46_greater_1(u_value v_s) {
  u_value s1087 = v_s;
  u_value t1088 = NULL;
  if ((!t1088) && u_is_con(s1087, &n_set_46_SNode)) {
    u_value v_r = u_field(s1087, 2);
    t1088 = v_r;
  }
  if (!t1088) {
    t1088 = v_s;
  }
  if (!t1088) {
    u_no_case(s1087);
  }
  return t1088;
}

u_value f_set_46_rebalance_3(u_value v_l, u_value v_x, u_value v_r) {
  u_value t1092;
  if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1))))) {
    u_value t1089;
    if (u_truth(u_lt(f_set_46_depth_1(f_set_46_smaller_1(v_l)), f_set_46_depth_1(f_set_46_greater_1(v_l))))) {
      t1089 = f_set_46_turn_left_1(v_l);
    } else {
      t1089 = v_l;
    }
    t1092 = f_set_46_turn_right_1(f_set_46_snode_3(t1089, v_x, v_r));
  } else {
    u_value t1091;
    if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1))))) {
      u_value t1090;
      if (u_truth(u_lt(f_set_46_depth_1(f_set_46_greater_1(v_r)), f_set_46_depth_1(f_set_46_smaller_1(v_r))))) {
        t1090 = f_set_46_turn_right_1(v_r);
      } else {
        t1090 = v_r;
      }
      t1091 = f_set_46_turn_left_1(f_set_46_snode_3(v_l, v_x, t1090));
    } else {
      t1091 = f_set_46_snode_3(v_l, v_x, v_r);
    }
    t1092 = t1091;
  }
  return t1092;
}

u_value f_set_46_well_formed_1(u_value v_s) {
  u_value s1093 = v_s;
  u_value t1094 = NULL;
  if ((!t1094) && u_is_con(s1093, &n_set_46_SLeaf)) {
    t1094 = u_mk_bool(1);
  }
  if ((!t1094) && u_is_con(s1093, &n_set_46_SNode)) {
    u_value v_l = u_field(s1093, 0);
    u_value v_r = u_field(s1093, 2);
    u_value v_d = u_field(s1093, 3);
    u_value t1095 = f_set_46_well_formed_1(v_l);
    if (u_truth(t1095)) {
      t1095 = f_set_46_well_formed_1(v_r);
    }
    u_value t1096 = t1095;
    if (u_truth(t1096)) {
      t1096 = u_eq(v_d, f_set_46_snode_depth_2(v_l, v_r));
    }
    u_value t1097 = t1096;
    if (u_truth(t1097)) {
      t1097 = f_prelude_46__60__61__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1)));
    }
    u_value t1098 = t1097;
    if (u_truth(t1098)) {
      t1098 = f_prelude_46__60__61__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1)));
    }
    t1094 = t1098;
  }
  if (!t1094) {
    u_no_case(s1093);
  }
  return t1094;
}

u_value f_set_46_upto_1(u_value v_n) {
  u_value t1099;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t1099 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1099 = f_list_46__43__43__2(f_set_46_upto_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t1099;
}

u_value f_parse_46_Notations_46_cmds_1(u_value v_x) {
  u_value s1100 = v_x;
  u_value t1101 = NULL;
  if ((!t1101) && u_is_con(s1100, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1100, 0);
    t1101 = v_y;
  }
  if (!t1101) {
    u_no_case(s1100);
  }
  return t1101;
}

u_value f_parse_46_Notations_46_ops_1(u_value v_x) {
  u_value s1102 = v_x;
  u_value t1103 = NULL;
  if ((!t1103) && u_is_con(s1102, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1102, 1);
    t1103 = v_y;
  }
  if (!t1103) {
    u_no_case(s1102);
  }
  return t1103;
}

u_value f_parse_46_Notations_46_cmds_58__61__2(u_value v_x, u_value v_y) {
  u_value s1104 = v_x;
  u_value t1105 = NULL;
  if ((!t1105) && u_is_con(s1104, &n_parse_46_Notations)) {
    u_value v_a1 = u_field(s1104, 1);
    t1105 = u_mk_con(&n_parse_46_Notations, 2, v_y, v_a1);
  }
  if (!t1105) {
    u_no_case(s1104);
  }
  return t1105;
}

u_value f_parse_46_Notations_46_ops_58__61__2(u_value v_x, u_value v_y) {
  u_value s1106 = v_x;
  u_value t1107 = NULL;
  if ((!t1107) && u_is_con(s1106, &n_parse_46_Notations)) {
    u_value v_a0 = u_field(s1106, 0);
    t1107 = u_mk_con(&n_parse_46_Notations, 2, v_a0, v_y);
  }
  if (!t1107) {
    u_no_case(s1106);
  }
  return t1107;
}

u_value f_parse_46_Scope_46_params_1(u_value v_x) {
  u_value s1108 = v_x;
  u_value t1109 = NULL;
  if ((!t1109) && u_is_con(s1108, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1108, 0);
    t1109 = v_y;
  }
  if (!t1109) {
    u_no_case(s1108);
  }
  return t1109;
}

u_value f_parse_46_Scope_46_notations_1(u_value v_x) {
  u_value s1110 = v_x;
  u_value t1111 = NULL;
  if ((!t1111) && u_is_con(s1110, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1110, 1);
    t1111 = v_y;
  }
  if (!t1111) {
    u_no_case(s1110);
  }
  return t1111;
}

u_value f_parse_46_Scope_46_params_58__61__2(u_value v_x, u_value v_y) {
  u_value s1112 = v_x;
  u_value t1113 = NULL;
  if ((!t1113) && u_is_con(s1112, &n_parse_46_Scope)) {
    u_value v_a1 = u_field(s1112, 1);
    t1113 = u_mk_con(&n_parse_46_Scope, 2, v_y, v_a1);
  }
  if (!t1113) {
    u_no_case(s1112);
  }
  return t1113;
}

u_value f_parse_46_Scope_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1114 = v_x;
  u_value t1115 = NULL;
  if ((!t1115) && u_is_con(s1114, &n_parse_46_Scope)) {
    u_value v_a0 = u_field(s1114, 0);
    t1115 = u_mk_con(&n_parse_46_Scope, 2, v_a0, v_y);
  }
  if (!t1115) {
    u_no_case(s1114);
  }
  return t1115;
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
  static struct u_object l___1116 = U_LIT("_");
  static struct u_object l_assert_1117 = U_LIT("assert");
  static struct u_object l_assign_1118 = U_LIT("assign");
  static struct u_object l_call_1119 = U_LIT("call");
  static struct u_object l_case_1120 = U_LIT("case");
  static struct u_object l_catch_1121 = U_LIT("catch");
  static struct u_object l_data_1122 = U_LIT("data");
  static struct u_object l_else_1123 = U_LIT("else");
  static struct u_object l_end_1124 = U_LIT("end");
  static struct u_object l_eval_1125 = U_LIT("eval");
  static struct u_object l_false_1126 = U_LIT("false");
  static struct u_object l_for_1127 = U_LIT("for");
  static struct u_object l_function_1128 = U_LIT("function");
  static struct u_object l_if_1129 = U_LIT("if");
  static struct u_object l_import_1130 = U_LIT("import");
  static struct u_object l_in_1131 = U_LIT("in");
  static struct u_object l_infix_1132 = U_LIT("infix");
  static struct u_object l_lambda_1133 = U_LIT("lambda");
  static struct u_object l_let_1134 = U_LIT("let");
  static struct u_object l_local_1135 = U_LIT("local");
  static struct u_object l_match_1136 = U_LIT("match");
  static struct u_object l_out_1137 = U_LIT("out");
  static struct u_object l_postfix_1138 = U_LIT("postfix");
  static struct u_object l_prefix_1139 = U_LIT("prefix");
  static struct u_object l_procedure_1140 = U_LIT("procedure");
  static struct u_object l_raise_1141 = U_LIT("raise");
  static struct u_object l_ref_1142 = U_LIT("ref");
  static struct u_object l_run_1143 = U_LIT("run");
  static struct u_object l_skip_1144 = U_LIT("skip");
  static struct u_object l_test_1145 = U_LIT("test");
  static struct u_object l_then_1146 = U_LIT("then");
  static struct u_object l_true_1147 = U_LIT("true");
  static struct u_object l_type_1148 = U_LIT("type");
  static struct u_object l_try_1149 = U_LIT("try");
  static struct u_object l_while_1150 = U_LIT("while");
  static struct u_object l_with_1151 = U_LIT("with");
  u_pop(handler);
  cache = f_set_46_from_list_1(u_mk_con(&n_list_46_Cons, 2, &l___1116, u_mk_con(&n_list_46_Cons, 2, &l_assert_1117, u_mk_con(&n_list_46_Cons, 2, &l_assign_1118, u_mk_con(&n_list_46_Cons, 2, &l_call_1119, u_mk_con(&n_list_46_Cons, 2, &l_case_1120, u_mk_con(&n_list_46_Cons, 2, &l_catch_1121, u_mk_con(&n_list_46_Cons, 2, &l_data_1122, u_mk_con(&n_list_46_Cons, 2, &l_else_1123, u_mk_con(&n_list_46_Cons, 2, &l_end_1124, u_mk_con(&n_list_46_Cons, 2, &l_eval_1125, u_mk_con(&n_list_46_Cons, 2, &l_false_1126, u_mk_con(&n_list_46_Cons, 2, &l_for_1127, u_mk_con(&n_list_46_Cons, 2, &l_function_1128, u_mk_con(&n_list_46_Cons, 2, &l_if_1129, u_mk_con(&n_list_46_Cons, 2, &l_import_1130, u_mk_con(&n_list_46_Cons, 2, &l_in_1131, u_mk_con(&n_list_46_Cons, 2, &l_infix_1132, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1133, u_mk_con(&n_list_46_Cons, 2, &l_let_1134, u_mk_con(&n_list_46_Cons, 2, &l_local_1135, u_mk_con(&n_list_46_Cons, 2, &l_match_1136, u_mk_con(&n_list_46_Cons, 2, &l_out_1137, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1138, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1139, u_mk_con(&n_list_46_Cons, 2, &l_procedure_1140, u_mk_con(&n_list_46_Cons, 2, &l_raise_1141, u_mk_con(&n_list_46_Cons, 2, &l_ref_1142, u_mk_con(&n_list_46_Cons, 2, &l_run_1143, u_mk_con(&n_list_46_Cons, 2, &l_skip_1144, u_mk_con(&n_list_46_Cons, 2, &l_test_1145, u_mk_con(&n_list_46_Cons, 2, &l_then_1146, u_mk_con(&n_list_46_Cons, 2, &l_true_1147, u_mk_con(&n_list_46_Cons, 2, &l_type_1148, u_mk_con(&n_list_46_Cons, 2, &l_try_1149, u_mk_con(&n_list_46_Cons, 2, &l_while_1150, u_mk_con(&n_list_46_Cons, 2, &l_with_1151, u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_set_46_member_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1152 = U_LIT("=");
  static struct u_object l__61__62__1153 = U_LIT("=>");
  static struct u_object l__58__61__1154 = U_LIT(":=");
  static struct u_object l__58__1155 = U_LIT(":");
  static struct u_object l__124__1156 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1152, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1153, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1154, u_mk_con(&n_list_46_Cons, 2, &l__58__1155, u_mk_con(&n_list_46_Cons, 2, &l__124__1156, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1157 = v_ns;
  u_value t1158 = NULL;
  if ((!t1158) && u_is_con(s1157, &n_list_46_Nil)) {
    t1158 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1158) && (u_is_con(s1157, &n_list_46_Cons) && (u_is_con(u_field(s1157, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1157, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1157, 0), 1);
    u_value v_ops = u_field(u_field(s1157, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1158 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1158) && u_is_con(s1157, &n_list_46_Cons)) {
    u_value v_more = u_field(s1157, 1);
    t1158 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1158) {
    u_no_case(s1157);
  }
  return t1158;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1159 = v_ns;
  u_value t1160 = NULL;
  if ((!t1160) && u_is_con(s1159, &n_list_46_Nil)) {
    t1160 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1160) && (u_is_con(s1159, &n_list_46_Cons) && (u_is_con(u_field(s1159, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1159, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1159, 0), 1);
    u_value v_ops = u_field(u_field(s1159, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1160 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1160) && u_is_con(s1159, &n_list_46_Cons)) {
    u_value v_more = u_field(s1159, 1);
    t1160 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1160) {
    u_no_case(s1159);
  }
  return t1160;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1161 = v_ns;
  u_value t1162 = NULL;
  if ((!t1162) && u_is_con(s1161, &n_list_46_Nil)) {
    t1162 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1162) && (u_is_con(s1161, &n_list_46_Cons) && u_is_con(u_field(s1161, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1161, 0), 0);
    u_value v_p = u_field(u_field(s1161, 0), 1);
    u_value v_ops = u_field(u_field(s1161, 0), 2);
    u_value t1163 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1163)) {
      t1163 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1164 = t1163;
    if (u_truth(t1164)) {
      t1164 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1164)) {
      t1162 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1162) && u_is_con(s1161, &n_list_46_Cons)) {
    u_value v_more = u_field(s1161, 1);
    t1162 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1162) {
    u_no_case(s1161);
  }
  return t1162;
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
  u_value s1165 = v_cs;
  u_value t1166 = NULL;
  if ((!t1166) && u_is_con(s1165, &n_list_46_Nil)) {
    t1166 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1166) && (u_is_con(s1165, &n_list_46_Cons) && u_is_con(u_field(s1165, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1165, 0), 2);
    u_value v_more = u_field(s1165, 1);
    t1166 = f_list_46__43__43__2(v_ops, f_parse_46_operators_of_1(v_more));
  }
  if ((!t1166) && u_is_con(s1165, &n_list_46_Cons)) {
    u_value v_more = u_field(s1165, 1);
    t1166 = f_parse_46_operators_of_1(v_more);
  }
  if (!t1166) {
    u_no_case(s1165);
  }
  return t1166;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1167 = v_ts;
  u_value t1168 = NULL;
  if ((!t1168) && u_is_con(s1167, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1167, 0), 0);
    t1168 = v_t;
  }
  if (!t1168) {
    t1168 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1168) {
    u_no_case(s1167);
  }
  return t1168;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1169 = v_ts;
  u_value t1170 = NULL;
  if ((!t1170) && (u_is_con(s1169, &n_list_46_Cons) && u_is_con(u_field(s1169, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1169, 1), 0), 0);
    t1170 = v_t;
  }
  if (!t1170) {
    t1170 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1170) {
    u_no_case(s1169);
  }
  return t1170;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1171 = v_ts;
  u_value t1172 = NULL;
  if ((!t1172) && u_is_con(s1171, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1171, 0), 1);
    t1172 = v_i;
  }
  if (!t1172) {
    t1172 = u_mk_int(0);
  }
  if (!t1172) {
    u_no_case(s1171);
  }
  return t1172;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1173 = v_t;
  u_value t1174 = NULL;
  if ((!t1174) && u_is_con(s1173, &n_lex_46_TName)) {
    u_value v_n = u_field(s1173, 0);
    t1174 = v_n;
  }
  if ((!t1174) && u_is_con(s1173, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1173, 0);
    t1174 = v_s;
  }
  if ((!t1174) && u_is_con(s1173, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1173, 0);
    t1174 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1174) && u_is_con(s1173, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1173, 0);
    t1174 = u_str_show(v_n);
  }
  if ((!t1174) && u_is_con(s1173, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1173, 0);
    t1174 = u_str_quote(v_s);
  }
  if ((!t1174) && u_is_con(s1173, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1173, 0);
    static struct u_object l__39__1175 = U_LIT("'");
    static struct u_object l__39__1176 = U_LIT("'");
    t1174 = u_concat(&l__39__1175, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1176));
  }
  if ((!t1174) && u_is_con(s1173, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1177 = U_LIT("end of input");
    t1174 = &l_end_32_of_32_input_1177;
  }
  if (!t1174) {
    u_no_case(s1173);
  }
  return t1174;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1178 = f_parse_46_head_token_1(v_ts);
  u_value t1179 = NULL;
  if ((!t1179) && u_is_con(s1178, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1178, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1179 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1179) && u_is_con(s1178, &n_lex_46_TName)) {
    u_value v_n = u_field(s1178, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1179 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1179) {
    t1179 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1179) {
    u_no_case(s1178);
  }
  return t1179;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1180 = *r_ts;
    int d1181 = 0;
    if ((!d1181) && u_is_con(s1180, &n_list_46_Cons)) {
      u_value v_more = u_field(s1180, 1);
      d1181 = 1;
      {
        u_value t1182 = v_more;
        *r_ts = t1182;
      }
    }
    if (!d1181) {
      d1181 = 1;
      {
      }
    }
    if (!d1181) {
      u_no_case(s1180);
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
        static struct u_object l__32_expected_32_but_32__1183 = U_LIT(" expected but ");
        static struct u_object l__32_found_1184 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1183, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1184))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1185 = f_parse_46_head_token_1(*r_ts);
    int d1186 = 0;
    if ((!d1186) && u_is_con(s1185, &n_lex_46_TName)) {
      u_value v_m = u_field(s1185, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1186 = 1;
        {
          u_value t1187 = v_m;
          *r_n = t1187;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1186) {
      d1186 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1188 = U_LIT("name expected but ");
        static struct u_object l__32_found_1189 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1188, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1189)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1186) {
      u_no_case(s1185);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1190 = f_parse_46_head_token_1(*r_ts);
    int d1191 = 0;
    if ((!d1191) && u_is_con(s1190, &n_lex_46_TName)) {
      u_value v_m = u_field(s1190, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_m))) {
        d1191 = 1;
        {
          static struct u_object l_operator_32__1192 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1193 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1192, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1193)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1191) {
      d1191 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1191) {
      u_no_case(s1190);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1194 = f_parse_46_head_token_1(*r_ts);
    int d1195 = 0;
    if ((!d1195) && u_is_con(s1194, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1194, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1195 = 1;
        {
          u_value t1196 = v_s;
          *r_n = t1196;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1195) {
      d1195 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1195) {
      u_no_case(s1194);
    }
  }
}

u_value f_parse_46_Ops_46_notations_1(u_value v_x) {
  u_value s1197 = v_x;
  u_value t1198 = NULL;
  if ((!t1198) && u_is_con(s1197, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1197, 0);
    t1198 = v_y;
  }
  if (!t1198) {
    u_no_case(s1197);
  }
  return t1198;
}

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x) {
  u_value s1199 = v_x;
  u_value t1200 = NULL;
  if ((!t1200) && u_is_con(s1199, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1199, 1);
    t1200 = v_y;
  }
  if (!t1200) {
    u_no_case(s1199);
  }
  return t1200;
}

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x) {
  u_value s1201 = v_x;
  u_value t1202 = NULL;
  if ((!t1202) && u_is_con(s1201, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1201, 2);
    t1202 = v_y;
  }
  if (!t1202) {
    u_no_case(s1201);
  }
  return t1202;
}

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x) {
  u_value s1203 = v_x;
  u_value t1204 = NULL;
  if ((!t1204) && u_is_con(s1203, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1203, 3);
    t1204 = v_y;
  }
  if (!t1204) {
    u_no_case(s1203);
  }
  return t1204;
}

u_value f_parse_46_Ops_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1205 = v_x;
  u_value t1206 = NULL;
  if ((!t1206) && u_is_con(s1205, &n_parse_46_Ops)) {
    u_value v_a1 = u_field(s1205, 1);
    u_value v_a2 = u_field(s1205, 2);
    u_value v_a3 = u_field(s1205, 3);
    t1206 = u_mk_con(&n_parse_46_Ops, 4, v_y, v_a1, v_a2, v_a3);
  }
  if (!t1206) {
    u_no_case(s1205);
  }
  return t1206;
}

u_value f_parse_46_Ops_46_prefix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1207 = v_x;
  u_value t1208 = NULL;
  if ((!t1208) && u_is_con(s1207, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1207, 0);
    u_value v_a2 = u_field(s1207, 2);
    u_value v_a3 = u_field(s1207, 3);
    t1208 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_y, v_a2, v_a3);
  }
  if (!t1208) {
    u_no_case(s1207);
  }
  return t1208;
}

u_value f_parse_46_Ops_46_postfix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1209 = v_x;
  u_value t1210 = NULL;
  if ((!t1210) && u_is_con(s1209, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1209, 0);
    u_value v_a1 = u_field(s1209, 1);
    u_value v_a3 = u_field(s1209, 3);
    t1210 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_y, v_a3);
  }
  if (!t1210) {
    u_no_case(s1209);
  }
  return t1210;
}

u_value f_parse_46_Ops_46_infix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1211 = v_x;
  u_value t1212 = NULL;
  if ((!t1212) && u_is_con(s1211, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1211, 0);
    u_value v_a1 = u_field(s1211, 1);
    u_value v_a2 = u_field(s1211, 2);
    t1212 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_a2, v_y);
  }
  if (!t1212) {
    u_no_case(s1211);
  }
  return t1212;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1213 = v_rest;
    int d1214 = 0;
    if ((!d1214) && u_is_con(s1213, &n_list_46_Nil)) {
      d1214 = 1;
      {
      }
    }
    if ((!d1214) && (u_is_con(s1213, &n_list_46_Cons) && u_is_con(u_field(s1213, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1213, 0), 0);
      u_value v_i = u_field(u_field(s1213, 0), 1);
      d1214 = 1;
      {
        static struct u_object l_unexpected_32_opera_1215 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1215, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1214) {
      d1214 = 1;
      {
        static struct u_object l_missing_32_operator_1216 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1216, v_at), "parse.ParseError");
      }
    }
    if (!d1214) {
      u_no_case(s1213);
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
    u_value s1217 = v_items;
    int d1218 = 0;
    if ((!d1218) && (u_is_con(s1217, &n_list_46_Cons) && u_is_con(u_field(s1217, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1217, 0), 0);
      u_value v_more = u_field(s1217, 1);
      d1218 = 1;
      {
        u_value t1219 = v_x;
        u_value t1220 = v_more;
        *r_a = t1219;
        *r_rest = t1220;
      }
    }
    if ((!d1218) && (u_is_con(s1217, &n_list_46_Cons) && u_is_con(u_field(s1217, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1217, 0), 0);
      u_value v_i = u_field(u_field(s1217, 0), 1);
      u_value v_more = u_field(s1217, 1);
      d1218 = 1;
      {
        u_value s1221 = f_parse_46_prefix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op);
        int d1222 = 0;
        if ((!d1222) && u_is_con(s1221, &n_option_46_Some)) {
          u_value v_p = u_field(s1221, 0);
          d1222 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1223 = u_apply(f_parse_46_Ops_46_prefix_op_1(v_o), 2, v_op, v_b);
            *r_a = t1223;
          }
        }
        if ((!d1222) && u_is_con(s1221, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op))) {
            d1222 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1224 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1224), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1222) && u_is_con(s1221, &n_option_46_None)) {
          d1222 = 1;
          {
            static struct u_object l_undeclared_32_opera_1225 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1225, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1222) {
          u_no_case(s1221);
        }
      }
    }
    if ((!d1218) && u_is_con(s1217, &n_list_46_Nil)) {
      d1218 = 1;
      {
        static struct u_object l_missing_32_operand_1226 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1226, v_at), "parse.ParseError");
      }
    }
    if (!d1218) {
      u_no_case(s1217);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1227 = v_items;
    int d1228 = 0;
    if ((!d1228) && (u_is_con(s1227, &n_list_46_Cons) && u_is_con(u_field(s1227, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1227, 0), 0);
      u_value v_i = u_field(u_field(s1227, 0), 1);
      u_value v_tail = u_field(s1227, 1);
      d1228 = 1;
      {
        u_value s1229 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op), f_parse_46_infix_of_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op));
        int d1230 = 0;
        if ((!d1230) && u_is_con(u_field(s1229, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1229, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1230 = 1;
            {
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_postfix_op_1(v_o), 2, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1230) && u_is_con(u_field(s1229, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1229, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1229, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1230 = 1;
            {
              u_value t1231;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1231 = v_p;
              } else {
                t1231 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1231, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)));
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_infix_op_1(v_o), 3, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1230) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op)))) {
            d1230 = 1;
            {
              static struct u_object l_undeclared_32_opera_1232 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1232, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1230) {
          d1230 = 1;
          {
            u_value t1233 = v_lhs;
            u_value t1234 = v_items;
            *r_a = t1233;
            *r_rest = t1234;
          }
        }
        if (!d1230) {
          u_no_case(s1229);
        }
      }
    }
    if (!d1228) {
      d1228 = 1;
      {
        u_value t1235 = v_lhs;
        u_value t1236 = v_items;
        *r_a = t1235;
        *r_rest = t1236;
      }
    }
    if (!d1228) {
      u_no_case(s1227);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1237 = v_rest;
    int d1238 = 0;
    if ((!d1238) && (u_is_con(s1237, &n_list_46_Cons) && u_is_con(u_field(s1237, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1237, 0), 0);
      u_value v_i = u_field(u_field(s1237, 0), 1);
      u_value t1241 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1241)) {
        t1241 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1241)) {
        d1238 = 1;
        {
          static struct u_object l_operator_32__1239 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1240 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1239, u_concat(v_op, &l__32_is_32_not_32_associat_1240)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1238) {
      d1238 = 1;
      {
      }
    }
    if (!d1238) {
      u_no_case(s1237);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1242 = v_fp;
  u_value t1243 = NULL;
  if ((!t1243) && u_is_con(s1242, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1242, 0), 1);
    t1243 = u_eq(v_q, v_p);
  }
  if (!t1243) {
    t1243 = u_mk_bool(0);
  }
  if (!t1243) {
    u_no_case(s1242);
  }
  return t1243;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1244 = v_ts;
  u_value t1245 = NULL;
  if ((!t1245) && (u_is_con(s1244, &n_list_46_Cons) && (u_is_con(u_field(s1244, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1244, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1244, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1244, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1244, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1244, 0), 1);
    t1245 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1245) && (u_is_con(s1244, &n_list_46_Cons) && u_is_con(u_field(s1244, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1244, 0);
    t1245 = v_t;
  }
  if ((!t1245) && u_is_con(s1244, &n_list_46_Nil)) {
    t1245 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1245) {
    static struct u_object l__40_invalid_41__1246 = U_LIT("(invalid)");
    t1245 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1246), v_ts);
  }
  if (!t1245) {
    u_no_case(s1244);
  }
  return t1245;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1247 = v_ts;
  u_value t1248 = NULL;
  if ((!t1248) && (u_is_con(s1247, &n_list_46_Cons) && (u_is_con(u_field(s1247, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1247, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1247, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1247, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1247, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1247, 0), 1);
    t1248 = v_cs;
  }
  if (!t1248) {
    t1248 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1248) {
    u_no_case(s1247);
  }
  return t1248;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1249 = U_LIT("->");
  u_value t1256;
  if (u_truth(u_eq(v_op, &l__45__62__1249))) {
    u_value t1250 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1251 = &t1250;
    u_value l1252 = v_l;
    while (u_is_con(l1252, &n_list_46_Cons)) {
      u_value v_t = u_field(l1252, 0);
      if (u_truth(u_mk_bool(1))) {
        e1251 = u_append(e1251, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1252 = u_field(l1252, 1);
    }
    t1256 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1250, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1253 = U_LIT("*");
    u_value t1255;
    if (u_truth(u_eq(v_op, &l__42__1253))) {
      static struct u_object l__40_product_41__1254 = U_LIT("(product)");
      t1255 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1254), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1255 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1256 = t1255;
  }
  return t1256;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1257 = v_t;
  u_value t1258 = NULL;
  if ((!t1258) && (u_is_con(s1257, &n_syntax_46_TSort) && (u_is_con(u_field(s1257, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1257, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1257, 0), 1), u_mk_lit("(invalid)")))))) {
    t1258 = u_mk_bool(1);
  }
  if ((!t1258) && u_is_con(s1257, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1257, 1);
    t1258 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1258) && u_is_con(s1257, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1257, 0);
    u_value v_r = u_field(s1257, 1);
    u_value t1259 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1259)) {
      t1259 = f_parse_46_has_invalid_1(v_r);
    }
    t1258 = t1259;
  }
  if ((!t1258) && u_is_con(s1257, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1257, 0);
    t1258 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1258) {
    t1258 = u_mk_bool(0);
  }
  if (!t1258) {
    u_no_case(s1257);
  }
  return t1258;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1260 = v_ts;
  u_value t1261 = NULL;
  if ((!t1261) && u_is_con(s1260, &n_list_46_Nil)) {
    t1261 = u_mk_bool(0);
  }
  if ((!t1261) && u_is_con(s1260, &n_list_46_Cons)) {
    u_value v_t = u_field(s1260, 0);
    u_value v_more = u_field(s1260, 1);
    u_value t1262 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1262)) {
      t1262 = f_parse_46_any_invalid_1(v_more);
    }
    t1261 = t1262;
  }
  if (!t1261) {
    u_no_case(s1260);
  }
  return t1261;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1263 = v_t;
  u_value t1264 = NULL;
  if ((!t1264) && u_is_con(s1263, &n_lex_46_TName)) {
    u_value v_n = u_field(s1263, 0);
    u_value t1265 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1265)) {
      t1265 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1264 = t1265;
  }
  if ((!t1264) && (u_is_con(s1263, &n_lex_46_TPunct) && u_equals(u_field(s1263, 0), u_mk_char(40)))) {
    t1264 = u_mk_bool(1);
  }
  if (!t1264) {
    t1264 = u_mk_bool(0);
  }
  if (!t1264) {
    u_no_case(s1263);
  }
  return t1264;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1266 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1266;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1267 = f_parse_46_type_one_1(v_r);
    *r_t = t1267;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1268 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1268, v_start), "parse.ParseError");
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
    u_value t1269 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1269;
    u_value t1270 = u_mk_bool(1);
    v_next = t1270;
    u_value t1271 = u_mk_bool(1);
    v_go = t1271;
    u_value t1280 = v_go;
    while (u_truth(t1280)) {
      {
        u_value s1272 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1273 = 0;
        if ((!d1273) && u_is_con(s1272, &n_option_46_Some)) {
          u_value v_op = u_field(s1272, 0);
          d1273 = 1;
          {
            u_value t1274 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1275 = u_mk_bool(1);
            v_rev = t1274;
            v_next = t1275;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1273) && u_is_con(s1272, &n_option_46_None)) {
          u_value t1278 = v_next;
          if (u_truth(t1278)) {
            t1278 = f_parse_46_starts_type_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1278)) {
            d1273 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1276 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1277 = u_mk_bool(0);
              v_rev = t1276;
              v_next = t1277;
            }
          }
        }
        if (!d1273) {
          d1273 = 1;
          {
            u_value t1279 = u_mk_bool(0);
            v_go = t1279;
          }
        }
        if (!d1273) {
          u_no_case(s1272);
        }
      }
      t1280 = v_go;
    }
    u_value s1281 = v_rev;
    int d1282 = 0;
    if ((!d1282) && u_is_con(s1281, &n_list_46_Nil)) {
      d1282 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1283 = U_LIT("type expected but ");
        static struct u_object l__32_found_1284 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1283, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1284)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1282) {
      d1282 = 1;
      {
        u_value t1285 = f_list_46_reverse_1(v_rev);
        *r_items = t1285;
      }
    }
    if (!d1282) {
      u_no_case(s1281);
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
    u_value t1289 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (u_truth(t1289)) {
      static struct u_object l_in_1286 = U_LIT("in");
      static struct u_object l_out_1287 = U_LIT("out");
      static struct u_object l_ref_1288 = U_LIT("ref");
      t1289 = f_list_46_contains_2(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_in_1286), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_out_1287), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_ref_1288), u_mk_con(&n_list_46_Nil, 0)))));
    }
    if (u_truth(t1289)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_sections(u_mk_proc(wp_parse_46_parse_type, 0), u_mk_proc(wp_parse_46_parse_type, 0), v_sc, &v_ins, &v_outs, &v_refs, r_ts);
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
        u_value t1290 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TProc, 3, v_ins, v_outs, v_refs), u_mk_con(&n_list_46_Nil, 0));
        *r_r = t1290;
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
              u_value t1291 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1291;
            }
          } else {
            if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Scope_46_params_1(v_sc)))) {
              {
                u_value t1292 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
                *r_r = t1292;
              }
            } else {
              {
                u_value t1293 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
                *r_r = t1293;
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
    u_value t1294 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1294;
    u_value t1295 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1295)) {
      t1295 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1295)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1296 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1296;
        u_value t1298 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1298)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1297 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1297;
          }
          t1298 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1299 = f_list_46_reverse_1(v_rev);
    *r_r = t1299;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_prefix_2(u_value v_op, u_value v_a) {
  u_value s1300 = v_a;
  u_value t1301 = NULL;
  if ((!t1301) && (u_is_con(s1300, &n_syntax_46_ELit) && u_is_con(u_field(s1300, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1300, 0), 0);
    static struct u_object l__45__1302 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1302))) {
      t1301 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1301) {
    t1301 = f_parse_46_expr_op1_2(v_op, v_a);
  }
  if (!t1301) {
    u_no_case(s1300);
  }
  return t1301;
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1303 = v_es;
  u_value t1304 = NULL;
  if ((!t1304) && u_is_con(s1303, &n_list_46_Nil)) {
    t1304 = v_rest;
  }
  if ((!t1304) && u_is_con(s1303, &n_list_46_Cons)) {
    u_value v_e = u_field(s1303, 0);
    u_value v_more = u_field(s1303, 1);
    static struct u_object l_Cons_1305 = U_LIT("Cons");
    t1304 = f_parse_46_expr_op2_3(&l_Cons_1305, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1304) {
    u_no_case(s1303);
  }
  return t1304;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1306 = v_t;
  u_value t1307 = NULL;
  if ((!t1307) && u_is_con(s1306, &n_lex_46_TName)) {
    u_value v_n = u_field(s1306, 0);
    static struct u_object l_let_1308 = U_LIT("let");
    static struct u_object l_if_1309 = U_LIT("if");
    static struct u_object l_lambda_1310 = U_LIT("lambda");
    static struct u_object l_match_1311 = U_LIT("match");
    static struct u_object l_true_1312 = U_LIT("true");
    static struct u_object l_false_1313 = U_LIT("false");
    u_value t1315 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1308, u_mk_con(&n_list_46_Cons, 2, &l_if_1309, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1310, u_mk_con(&n_list_46_Cons, 2, &l_match_1311, u_mk_con(&n_list_46_Cons, 2, &l_true_1312, u_mk_con(&n_list_46_Cons, 2, &l_false_1313, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1315)) {
      u_value t1314 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1314)) {
        t1314 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1315 = t1314;
    }
    t1307 = t1315;
  }
  if ((!t1307) && u_is_con(s1306, &n_lex_46_TNum)) {
    t1307 = u_mk_bool(1);
  }
  if ((!t1307) && u_is_con(s1306, &n_lex_46_TStr)) {
    t1307 = u_mk_bool(1);
  }
  if ((!t1307) && u_is_con(s1306, &n_lex_46_TChar)) {
    t1307 = u_mk_bool(1);
  }
  if ((!t1307) && u_is_con(s1306, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1306, 0);
    u_value t1316 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1316)) {
      t1316 = u_eq(v_c, u_mk_char(91));
    }
    u_value t1317 = t1316;
    if (!u_truth(t1317)) {
      t1317 = u_eq(v_c, u_mk_char(123));
    }
    t1307 = t1317;
  }
  if (!t1307) {
    t1307 = u_mk_bool(0);
  }
  if (!t1307) {
    u_no_case(s1306);
  }
  return t1307;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    static struct u_object l_raise_1318 = U_LIT("raise");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_raise_1318))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, r_e, r_ts);
        u_value t1319 = u_mk_con(&n_syntax_46_ERaise, 2, *r_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_e = t1319;
      }
    } else {
      {
        p_parse_46_operators(v_sc, r_e, r_ts);
        static struct u_object l_with_1320 = U_LIT("with");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_with_1320))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_update(v_sc, r_e, r_ts);
            u_value t1321 = f_parse_46_at_update_1(*r_ts);
            while (u_truth(t1321)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_update(v_sc, r_e, r_ts);
              }
              t1321 = f_parse_46_at_update_1(*r_ts);
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
    static struct u_object l__58__61__1322 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1322), r_ts);
    p_parse_46_operators(v_sc, &v_a, r_ts);
    static struct u_object l__46__1323 = U_LIT(".");
    static struct u_object l__58__61__1324 = U_LIT(":=");
    u_value t1325 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1323, u_concat(v_f, &l__58__61__1324))), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1325;
  }
}

u_value f_parse_46_at_update_1(u_value v_ts) {
  u_value s1326 = v_ts;
  u_value t1327 = NULL;
  if ((!t1327) && (u_is_con(s1326, &n_list_46_Cons) && (u_is_con(u_field(u_field(s1326, 0), 0), &n_lex_46_TPunct) && (u_equals(u_field(u_field(u_field(s1326, 0), 0), 0), u_mk_char(44)) && (u_is_con(u_field(s1326, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s1326, 1), 0), 0), &n_lex_46_TName) && (u_is_con(u_field(u_field(s1326, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(u_field(s1326, 1), 1), 0), 0), &n_lex_46_TSym) && u_equals(u_field(u_field(u_field(u_field(u_field(s1326, 1), 1), 0), 0), 0), u_mk_lit(":=")))))))))) {
    t1327 = u_mk_bool(1);
  }
  if (!t1327) {
    t1327 = u_mk_bool(0);
  }
  if (!t1327) {
    u_no_case(s1326);
  }
  return t1327;
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
    u_value t1328 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1328;
    u_value t1329 = u_mk_bool(1);
    v_next = t1329;
    u_value t1330 = u_mk_bool(1);
    v_go = t1330;
    u_value t1339 = v_go;
    while (u_truth(t1339)) {
      {
        u_value s1331 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1332 = 0;
        if ((!d1332) && u_is_con(s1331, &n_option_46_Some)) {
          u_value v_op = u_field(s1331, 0);
          d1332 = 1;
          {
            u_value t1333 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1334 = u_mk_bool(1);
            v_rev = t1333;
            v_next = t1334;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1332) && u_is_con(s1331, &n_option_46_None)) {
          u_value t1337 = v_next;
          if (u_truth(t1337)) {
            t1337 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1337)) {
            d1332 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1335 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1336 = u_mk_bool(0);
              v_rev = t1335;
              v_next = t1336;
            }
          }
        }
        if (!d1332) {
          d1332 = 1;
          {
            u_value t1338 = u_mk_bool(0);
            v_go = t1338;
          }
        }
        if (!d1332) {
          u_no_case(s1331);
        }
      }
      t1339 = v_go;
    }
    u_value s1340 = v_rev;
    int d1341 = 0;
    if ((!d1341) && u_is_con(s1340, &n_list_46_Nil)) {
      d1341 = 1;
      {
        static struct u_object l_expression_32_expec_1342 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1343 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1342, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1343)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1341) {
      d1341 = 1;
      {
        u_value t1344 = f_list_46_reverse_1(v_rev);
        *r_items = t1344;
      }
    }
    if (!d1341) {
      u_no_case(s1340);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1345 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1345;
    u_value t1347 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1347)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1346 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1346;
      }
      t1347 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1348 = f_list_46_reverse_1(v_rev);
    *r_es = t1348;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1349 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1349;
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
    u_value t1350 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1350)) {
      t1350 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1354 = t1350;
    while (u_truth(t1354)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1351 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1351;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1352 = U_LIT(".");
            u_value t1353 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1352, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1353;
          }
        }
      }
      u_value t1355 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1355)) {
        t1355 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1354 = t1355;
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
    u_value s1356 = f_parse_46_head_token_1(*r_ts);
    int d1357 = 0;
    if ((!d1357) && (u_is_con(s1356, &n_lex_46_TName) && u_equals(u_field(s1356, 0), u_mk_lit("let")))) {
      d1357 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1357) && (u_is_con(s1356, &n_lex_46_TName) && u_equals(u_field(s1356, 0), u_mk_lit("if")))) {
      d1357 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1358 = U_LIT("then");
        p_parse_46_keyword(&l_then_1358, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1359 = U_LIT("else");
        p_parse_46_keyword(&l_else_1359, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1360 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1360;
      }
    }
    if ((!d1357) && (u_is_con(s1356, &n_lex_46_TName) && u_equals(u_field(s1356, 0), u_mk_lit("lambda")))) {
      d1357 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1357) && (u_is_con(s1356, &n_lex_46_TName) && u_equals(u_field(s1356, 0), u_mk_lit("match")))) {
      d1357 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1361 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1361;
      }
    }
    if ((!d1357) && (u_is_con(s1356, &n_lex_46_TName) && u_equals(u_field(s1356, 0), u_mk_lit("true")))) {
      d1357 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1362 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1362;
      }
    }
    if ((!d1357) && (u_is_con(s1356, &n_lex_46_TName) && u_equals(u_field(s1356, 0), u_mk_lit("false")))) {
      d1357 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1363 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1363;
      }
    }
    if ((!d1357) && u_is_con(s1356, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1356, 0);
      d1357 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1364 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1364;
      }
    }
    if ((!d1357) && u_is_con(s1356, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1356, 0);
      d1357 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1365 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1365;
      }
    }
    if ((!d1357) && u_is_con(s1356, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1356, 0);
      d1357 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1366 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1366;
      }
    }
    if ((!d1357) && (u_is_con(s1356, &n_lex_46_TPunct) && u_equals(u_field(s1356, 0), u_mk_char(40)))) {
      d1357 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1367 = v_es;
        int d1368 = 0;
        if ((!d1368) && (u_is_con(s1367, &n_list_46_Cons) && u_is_con(u_field(s1367, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1367, 0);
          d1368 = 1;
          {
            u_value t1369 = v_x;
            *r_e = t1369;
          }
        }
        if (!d1368) {
          d1368 = 1;
          {
            u_value t1370 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1370;
          }
        }
        if (!d1368) {
          u_no_case(s1367);
        }
      }
    }
    if ((!d1357) && (u_is_con(s1356, &n_lex_46_TPunct) && u_equals(u_field(s1356, 0), u_mk_char(91)))) {
      d1357 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1357) && (u_is_con(s1356, &n_lex_46_TPunct) && u_equals(u_field(s1356, 0), u_mk_char(123)))) {
      d1357 = 1;
      {
        p_parse_46_parse_braces(v_sc, r_e, r_ts);
      }
    }
    if ((!d1357) && u_is_con(s1356, &n_lex_46_TName)) {
      u_value v_n = u_field(s1356, 0);
      u_value t1372 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1372)) {
        t1372 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t1372)) {
        d1357 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1371 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1371;
        }
      }
    }
    if (!d1357) {
      d1357 = 1;
      {
        static struct u_object l_expression_32_expec_1373 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1374 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1373, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1374)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1357) {
      u_no_case(s1356);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1375 = v_ps;
  u_value t1376 = NULL;
  if ((!t1376) && u_is_con(s1375, &n_list_46_Nil)) {
    t1376 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1376) && (u_is_con(s1375, &n_list_46_Cons) && u_is_con(u_field(s1375, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1375, 0), 0);
    u_value v_more = u_field(s1375, 1);
    u_value s1377 = f_parse_46_pattern_vars_1(v_more);
    u_value t1378 = NULL;
    if ((!t1378) && u_is_con(s1377, &n_option_46_Some)) {
      u_value v_xs = u_field(s1377, 0);
      t1378 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1378) && u_is_con(s1377, &n_option_46_None)) {
      t1378 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1378) {
      u_no_case(s1377);
    }
    t1376 = t1378;
  }
  if (!t1376) {
    t1376 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1376) {
    u_no_case(s1375);
  }
  return t1376;
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
    static struct u_object l__58__61__1379 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1379), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1380 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1381 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1380;
    v_es = t1381;
    u_value t1385 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1385)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1382 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1382), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1383 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1384 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1383;
        v_es = t1384;
      }
      t1385 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1386 = U_LIT("in");
    p_parse_46_keyword(&l_in_1386, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1387 = f_list_46_reverse_1(v_ps);
    u_value t1388 = f_list_46_reverse_1(v_es);
    v_ps = t1387;
    v_es = t1388;
    u_value s1389 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1390 = 0;
    if ((!d1390) && u_is_con(u_field(s1389, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1389, 0), 0);
      d1390 = 1;
      {
        u_value t1391 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1391;
      }
    }
    if ((!d1390) && (u_is_con(u_field(s1389, 0), &n_option_46_None) && (u_is_con(u_field(s1389, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1389, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1389, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1389, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1389, 1), 0);
      u_value v_c = u_field(u_field(s1389, 2), 0);
      d1390 = 1;
      {
        u_value t1392 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1392;
      }
    }
    if (!d1390) {
      d1390 = 1;
      {
        u_value t1393 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1393;
      }
    }
    if (!d1390) {
      u_no_case(s1389);
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
        static struct u_object l__61__62__1394 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1394), r_ts);
        p_parse_46_parse_body(v_sc, &v_body, r_ts);
        static struct u_object l_end_1395 = U_LIT("end");
        p_parse_46_keyword(&l_end_1395, r_ts);
        u_value t1396 = u_mk_con(&n_syntax_46_EProc, 4, v_ins, v_outs, v_refs, v_body);
        *r_e = t1396;
      }
    } else {
      {
        p_parse_46_lambda_function(v_sc, r_e, r_ts);
      }
    }
  }
}

u_value f_parse_46_at_section_1(u_value v_ts) {
  static struct u_object l_in_1397 = U_LIT("in");
  u_value t1399 = f_parse_46_at_keyword_2(v_ts, &l_in_1397);
  if (!u_truth(t1399)) {
    static struct u_object l_out_1398 = U_LIT("out");
    t1399 = f_parse_46_at_keyword_2(v_ts, &l_out_1398);
  }
  u_value t1401 = t1399;
  if (!u_truth(t1401)) {
    static struct u_object l_ref_1400 = U_LIT("ref");
    t1401 = f_parse_46_at_keyword_2(v_ts, &l_ref_1400);
  }
  return t1401;
}

void p_parse_46_lambda_function(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    u_value t1402 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1402;
    static struct u_object l__61__62__1403 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1403)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1404 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1404;
        u_value t1406 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1406)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1405 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1405;
          }
          t1406 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1407 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1407), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1408 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1408;
  }
}

void p_parse_46_parse_braces(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_at = NULL;
    u_value v_k = NULL;
    u_value v_v = NULL;
    u_value v_entries = NULL;
    u_value v_rev = NULL;
    u_value t1409 = f_parse_46_head_offset_1(*r_ts);
    v_at = t1409;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_expr(v_sc, &v_k, r_ts);
    static struct u_object l__61__62__1410 = U_LIT("=>");
    u_value t1411 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1410));
    v_entries = t1411;
    if (u_truth(v_entries)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_v, r_ts);
      }
    }
    static struct u_object l_for_1412 = U_LIT("for");
    u_value t1413 = f_parse_46_at_keyword_2(*r_ts, &l_for_1412);
    if (u_truth(t1413)) {
      t1413 = v_entries;
    }
    if (u_truth(t1413)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_comprehension(v_sc, v_k, v_v, r_e, r_ts);
      }
    } else {
      static struct u_object l_for_1414 = U_LIT("for");
      if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1414))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_set_comprehension(v_sc, v_k, r_e, r_ts);
        }
      } else {
        {
          u_value t1415;
          if (u_truth(v_entries)) {
            t1415 = u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))));
          } else {
            t1415 = v_k;
          }
          u_value t1416 = u_mk_con(&n_list_46_Cons, 2, t1415, u_mk_con(&n_list_46_Nil, 0));
          v_rev = t1416;
          u_value t1421 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          while (u_truth(t1421)) {
            {
              p_parse_46_advance(r_ts);
              p_parse_46_parse_expr(v_sc, &v_k, r_ts);
              static struct u_object l__61__62__1417 = U_LIT("=>");
              if (u_truth(f_prelude_46__33__61__2(v_entries, u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1417))))) {
                {
                  static struct u_object l_a_32_literal_32__123__46__46__46__125__32__1418 = U_LIT("a literal {...} has either elements a or entries k => v");
                  u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_a_32_literal_32__123__46__46__46__125__32__1418, v_at), "parse.ParseError");
                }
              }
              if (u_truth(v_entries)) {
                {
                  p_parse_46_advance(r_ts);
                  p_parse_46_parse_expr(v_sc, &v_v, r_ts);
                  u_value t1419 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)))), v_rev);
                  v_rev = t1419;
                }
              } else {
                {
                  u_value t1420 = u_mk_con(&n_list_46_Cons, 2, v_k, v_rev);
                  v_rev = t1420;
                }
              }
            }
            t1421 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          }
          u_value t1428;
          if (u_truth(v_entries)) {
            static struct u_object l_map_1422 = U_LIT("map");
            static struct u_object l_put_all_1423 = U_LIT("put_all");
            static struct u_object l_map_1424 = U_LIT("map");
            static struct u_object l_empty_1425 = U_LIT("empty");
            t1428 = f_parse_46_in_library_3(&l_map_1422, &l_put_all_1423, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1424, &l_empty_1425, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
          } else {
            static struct u_object l_set_1426 = U_LIT("set");
            static struct u_object l_from_list_1427 = U_LIT("from_list");
            t1428 = f_parse_46_in_library_3(&l_set_1426, &l_from_list_1427, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Nil, 0)));
          }
          u_value t1429 = t1428;
          *r_e = t1429;
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
  static struct u_object l_Nil_1430 = U_LIT("Nil");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1430), u_mk_con(&n_syntax_46_THole, 0)));
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
    u_value t1431 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1431;
    u_value t1432 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1432;
    u_value t1433 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1433;
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l_in_1434 = U_LIT("in");
    p_parse_46_keyword(&l_in_1434, r_ts);
    p_parse_46_parse_expr(v_sc, &v_l, r_ts);
    static struct u_object l_set_1435 = U_LIT("set");
    static struct u_object l_elements_1436 = U_LIT("elements");
    u_value t1437 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    u_value t1438 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1435, &l_elements_1436, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
    v_xs = t1437;
    v_ls = t1438;
    u_value t1444 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1444)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l_in_1439 = U_LIT("in");
        p_parse_46_keyword(&l_in_1439, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_set_1440 = U_LIT("set");
        static struct u_object l_elements_1441 = U_LIT("elements");
        u_value t1442 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
        u_value t1443 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1440, &l_elements_1441, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), v_ls);
        v_xs = t1442;
        v_ls = t1443;
      }
      t1444 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1445 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1445))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
      }
    }
    static struct u_object l_set_1446 = U_LIT("set");
    static struct u_object l_from_list_1447 = U_LIT("from_list");
    u_value t1448 = f_parse_46_in_library_3(&l_set_1446, &l_from_list_1447, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, v_r, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Nil, 0)));
    *r_e = t1448;
  }
}

void p_parse_46_map_comprehension(u_value v_sc, u_value v_k, u_value v_v, u_value *r_e, u_value *r_ts) {
  {
    u_value v_gs = NULL;
    u_value v_ls = NULL;
    u_value v_c = NULL;
    u_value t1449 = u_mk_con(&n_list_46_Nil, 0);
    v_gs = t1449;
    u_value t1450 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1450;
    u_value t1451 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1451;
    p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
    u_value t1452 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1452)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
      }
      t1452 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1453 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1453))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
        u_value t1454 = f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), v_c);
        v_c = t1454;
      }
    }
    static struct u_object l_map_1455 = U_LIT("map");
    static struct u_object l_put_all_1456 = U_LIT("put_all");
    static struct u_object l_map_1457 = U_LIT("map");
    static struct u_object l_empty_1458 = U_LIT("empty");
    u_value t1459 = f_parse_46_in_library_3(&l_map_1455, &l_put_all_1456, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))))), f_list_46_reverse_1(f_parse_46_hidden_vars_1(v_gs)), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1457, &l_empty_1458, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1459;
  }
}

void p_parse_46_map_generator(u_value v_sc, u_value *r_gs, u_value *r_ls, u_value *r_ts) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value v_m = NULL;
    p_parse_46_parse_plain(v_sc, &v_a, r_ts);
    static struct u_object l__61__62__1460 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1460), r_ts);
    p_parse_46_parse_plain(v_sc, &v_b, r_ts);
    static struct u_object l_in_1461 = U_LIT("in");
    p_parse_46_keyword(&l_in_1461, r_ts);
    p_parse_46_parse_expr(v_sc, &v_m, r_ts);
    static struct u_object l_map_1462 = U_LIT("map");
    static struct u_object l_entries_1463 = U_LIT("entries");
    u_value t1464 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1462, &l_entries_1463, u_mk_con(&n_list_46_Cons, 2, f_parse_46_unpack_2(f_list_46_reverse_1(*r_gs), v_m), u_mk_con(&n_list_46_Nil, 0))), *r_ls);
    *r_ls = t1464;
    static struct u_object l__40_entry_32__1465 = U_LIT("(entry ");
    static struct u_object l__41__1466 = U_LIT(")");
    u_value t1467 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(3, v_a, v_b, u_concat(&l__40_entry_32__1465, u_concat(u_str_show(u_add(f_list_46_length_1(*r_gs), u_mk_int(1))), &l__41__1466))), *r_gs);
    *r_gs = t1467;
  }
}

u_value f_parse_46_hidden_vars_1(u_value v_gs) {
  u_value s1468 = v_gs;
  u_value t1469 = NULL;
  if ((!t1469) && u_is_con(s1468, &n_list_46_Nil)) {
    t1469 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1469) && u_is_con(s1468, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s1468, 0), 2);
    u_value v_rest = u_field(s1468, 1);
    t1469 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0)), f_parse_46_hidden_vars_1(v_rest));
  }
  if (!t1469) {
    u_no_case(s1468);
  }
  return t1469;
}

u_value f_parse_46_unpack_2(u_value v_gs, u_value v_e) {
  u_value s1470 = v_gs;
  u_value t1471 = NULL;
  if ((!t1471) && u_is_con(s1470, &n_list_46_Nil)) {
    t1471 = v_e;
  }
  if ((!t1471) && u_is_con(s1470, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s1470, 0), 0);
    u_value v_v = u_field(u_field(s1470, 0), 1);
    u_value v_h = u_field(u_field(s1470, 0), 2);
    u_value v_rest = u_field(s1470, 1);
    t1471 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_k), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_v), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_parse_46_unpack_2(v_rest, v_e)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1471) {
    u_no_case(s1470);
  }
  return t1471;
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
        static struct u_object l_Nil_1472 = U_LIT("Nil");
        u_value t1473 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1472), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1473;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1474 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1474))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1475 = U_LIT("in");
            p_parse_46_keyword(&l_in_1475, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1476 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1477 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1476;
            v_lists = t1477;
            u_value t1481 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1481)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1478 = U_LIT("in");
                p_parse_46_keyword(&l_in_1478, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1479 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1480 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1479;
                v_lists = t1480;
              }
              t1481 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1482 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1482;
            static struct u_object l_if_1483 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1483))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1484 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1484;
          }
        } else {
          {
            u_value t1485 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1485;
            u_value t1487 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1487)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1486 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1486;
              }
              t1487 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1488 = U_LIT("Nil");
            u_value t1489 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1488), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1489;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1490 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1490;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1491 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1491))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1492 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1492;
      }
    } else {
      {
        u_value t1493 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1493;
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
    u_value t1494 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1494;
    static struct u_object l_case_1495 = U_LIT("case");
    u_value t1498 = f_parse_46_at_keyword_2(*r_ts, &l_case_1495);
    while (u_truth(t1498)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1496 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1496), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1497 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1497;
      }
      static struct u_object l_case_1499 = U_LIT("case");
      t1498 = f_parse_46_at_keyword_2(*r_ts, &l_case_1499);
    }
    static struct u_object l_else_1500 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1500))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1501 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1501;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1502 = U_LIT("end");
    p_parse_46_keyword(&l_end_1502, r_ts);
    u_value t1503 = f_list_46_reverse_1(v_rev);
    *r_cs = t1503;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1504 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1504))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1505 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1505, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
  }
}

u_value f_parse_46_pat_op1_2(u_value v_op, u_value v_p) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_pat_prefix_2(u_value v_op, u_value v_p) {
  u_value s1506 = v_p;
  u_value t1507 = NULL;
  if ((!t1507) && (u_is_con(s1506, &n_syntax_46_PLit) && u_is_con(u_field(s1506, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1506, 0), 0);
    static struct u_object l__45__1508 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1508))) {
      t1507 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1507) {
    t1507 = f_parse_46_pat_op1_2(v_op, v_p);
  }
  if (!t1507) {
    u_no_case(s1506);
  }
  return t1507;
}

u_value f_parse_46_pat_op2_3(u_value v_op, u_value v_l, u_value v_r) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_pats_2(u_value v_ps, u_value v_rest) {
  u_value s1509 = v_ps;
  u_value t1510 = NULL;
  if ((!t1510) && u_is_con(s1509, &n_list_46_Nil)) {
    t1510 = v_rest;
  }
  if ((!t1510) && u_is_con(s1509, &n_list_46_Cons)) {
    u_value v_p = u_field(s1509, 0);
    u_value v_more = u_field(s1509, 1);
    static struct u_object l_Cons_1511 = U_LIT("Cons");
    t1510 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1511), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1510) {
    u_no_case(s1509);
  }
  return t1510;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1512 = v_t;
  u_value t1513 = NULL;
  if ((!t1513) && u_is_con(s1512, &n_lex_46_TName)) {
    u_value v_n = u_field(s1512, 0);
    static struct u_object l___1514 = U_LIT("_");
    static struct u_object l_true_1515 = U_LIT("true");
    static struct u_object l_false_1516 = U_LIT("false");
    u_value t1518 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1514, u_mk_con(&n_list_46_Cons, 2, &l_true_1515, u_mk_con(&n_list_46_Cons, 2, &l_false_1516, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1518)) {
      u_value t1517 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1517)) {
        t1517 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1518 = t1517;
    }
    t1513 = t1518;
  }
  if ((!t1513) && u_is_con(s1512, &n_lex_46_TNum)) {
    t1513 = u_mk_bool(1);
  }
  if ((!t1513) && u_is_con(s1512, &n_lex_46_TStr)) {
    t1513 = u_mk_bool(1);
  }
  if ((!t1513) && u_is_con(s1512, &n_lex_46_TChar)) {
    t1513 = u_mk_bool(1);
  }
  if ((!t1513) && u_is_con(s1512, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1512, 0);
    u_value t1519 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1519)) {
      t1519 = u_eq(v_c, u_mk_char(91));
    }
    t1513 = t1519;
  }
  if (!t1513) {
    t1513 = u_mk_bool(0);
  }
  if (!t1513) {
    u_no_case(s1512);
  }
  return t1513;
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
    u_value t1520 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1520;
    u_value t1521 = u_mk_bool(1);
    v_next = t1521;
    u_value t1522 = u_mk_bool(1);
    v_go = t1522;
    u_value t1531 = v_go;
    while (u_truth(t1531)) {
      {
        u_value s1523 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1524 = 0;
        if ((!d1524) && u_is_con(s1523, &n_option_46_Some)) {
          u_value v_op = u_field(s1523, 0);
          d1524 = 1;
          {
            u_value t1525 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1526 = u_mk_bool(1);
            v_rev = t1525;
            v_next = t1526;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1524) && u_is_con(s1523, &n_option_46_None)) {
          u_value t1529 = v_next;
          if (u_truth(t1529)) {
            t1529 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1529)) {
            d1524 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1527 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1528 = u_mk_bool(0);
              v_rev = t1527;
              v_next = t1528;
            }
          }
        }
        if (!d1524) {
          d1524 = 1;
          {
            u_value t1530 = u_mk_bool(0);
            v_go = t1530;
          }
        }
        if (!d1524) {
          u_no_case(s1523);
        }
      }
      t1531 = v_go;
    }
    u_value s1532 = v_rev;
    int d1533 = 0;
    if ((!d1533) && u_is_con(s1532, &n_list_46_Nil)) {
      d1533 = 1;
      {
        static struct u_object l_pattern_32_expected_1534 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1535 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1534, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1535)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1533) {
      d1533 = 1;
      {
        u_value t1536 = f_list_46_reverse_1(v_rev);
        *r_items = t1536;
      }
    }
    if (!d1533) {
      u_no_case(s1532);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1537 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1537;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1538 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1538;
        u_value t1540 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1540)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1539 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1539;
          }
          t1540 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1541 = f_list_46_reverse_1(v_rev);
    *r_ps = t1541;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1542 = f_parse_46_head_token_1(*r_ts);
    int d1543 = 0;
    if ((!d1543) && (u_is_con(s1542, &n_lex_46_TName) && u_equals(u_field(s1542, 0), u_mk_lit("_")))) {
      d1543 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1544 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1544;
      }
    }
    if ((!d1543) && (u_is_con(s1542, &n_lex_46_TName) && u_equals(u_field(s1542, 0), u_mk_lit("true")))) {
      d1543 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1545 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1545;
      }
    }
    if ((!d1543) && (u_is_con(s1542, &n_lex_46_TName) && u_equals(u_field(s1542, 0), u_mk_lit("false")))) {
      d1543 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1546 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1546;
      }
    }
    if ((!d1543) && u_is_con(s1542, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1542, 0);
      d1543 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1547 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1547;
      }
    }
    if ((!d1543) && u_is_con(s1542, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1542, 0);
      d1543 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1548 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1548;
      }
    }
    if ((!d1543) && u_is_con(s1542, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1542, 0);
      d1543 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1549 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1549;
      }
    }
    if ((!d1543) && (u_is_con(s1542, &n_lex_46_TPunct) && u_equals(u_field(s1542, 0), u_mk_char(40)))) {
      d1543 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1550 = v_ps;
        int d1551 = 0;
        if ((!d1551) && (u_is_con(s1550, &n_list_46_Cons) && u_is_con(u_field(s1550, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1550, 0);
          d1551 = 1;
          {
            u_value t1552 = v_q;
            *r_p = t1552;
          }
        }
        if (!d1551) {
          d1551 = 1;
          {
            u_value t1553 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1553;
          }
        }
        if (!d1551) {
          u_no_case(s1550);
        }
      }
    }
    if ((!d1543) && (u_is_con(s1542, &n_lex_46_TPunct) && u_equals(u_field(s1542, 0), u_mk_char(91)))) {
      d1543 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1554 = U_LIT("Nil");
            u_value t1555 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1554), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1555;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1556 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1556;
            u_value t1558 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1558)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1557 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1557;
              }
              t1558 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1559 = U_LIT("Nil");
            u_value t1560 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1559), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1560;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1561 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1561;
          }
        }
      }
    }
    if (!d1543) {
      d1543 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1562 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1562;
          }
        } else {
          {
            u_value t1563 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1563;
          }
        }
      }
    }
    if (!d1543) {
      u_no_case(s1542);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1564 = v_t;
  u_value t1565 = NULL;
  if ((!t1565) && u_is_con(s1564, &n_lex_46_TName)) {
    u_value v_n = u_field(s1564, 0);
    static struct u_object l_skip_1566 = U_LIT("skip");
    static struct u_object l_if_1567 = U_LIT("if");
    static struct u_object l_while_1568 = U_LIT("while");
    static struct u_object l_call_1569 = U_LIT("call");
    static struct u_object l_match_1570 = U_LIT("match");
    static struct u_object l_assert_1571 = U_LIT("assert");
    static struct u_object l_assign_1572 = U_LIT("assign");
    static struct u_object l_raise_1573 = U_LIT("raise");
    static struct u_object l_try_1574 = U_LIT("try");
    t1565 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1566, u_mk_con(&n_list_46_Cons, 2, &l_if_1567, u_mk_con(&n_list_46_Cons, 2, &l_while_1568, u_mk_con(&n_list_46_Cons, 2, &l_call_1569, u_mk_con(&n_list_46_Cons, 2, &l_match_1570, u_mk_con(&n_list_46_Cons, 2, &l_assert_1571, u_mk_con(&n_list_46_Cons, 2, &l_assign_1572, u_mk_con(&n_list_46_Cons, 2, &l_raise_1573, u_mk_con(&n_list_46_Cons, 2, &l_try_1574, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1565) {
    t1565 = u_mk_bool(0);
  }
  if (!t1565) {
    u_no_case(s1564);
  }
  return t1565;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1575 = v_cs;
  u_value t1576 = NULL;
  if ((!t1576) && u_is_con(s1575, &n_list_46_Nil)) {
    t1576 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1576) && (u_is_con(s1575, &n_list_46_Cons) && u_is_con(u_field(s1575, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1575, 0), 0);
    u_value v_g = u_field(u_field(s1575, 0), 1);
    u_value v_b = u_field(u_field(s1575, 0), 2);
    u_value v_more = u_field(s1575, 1);
    t1576 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1576) {
    u_no_case(s1575);
  }
  return t1576;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1577 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1577;
    u_value t1578 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1578;
    u_value t1579 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1579;
    static struct u_object l_local_1580 = U_LIT("local");
    u_value t1582 = f_parse_46_at_keyword_2(*r_ts, &l_local_1580);
    while (u_truth(t1582)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1581 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1581)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1581 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1583 = U_LIT("local");
      t1582 = f_parse_46_at_keyword_2(*r_ts, &l_local_1583);
    }
    u_value t1585 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1585)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1584 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1584;
      }
      t1585 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1586 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1586;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1587 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1587;
    static struct u_object l__58__1588 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1588)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__1589 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1589)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1590 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1590;
      }
    }
    u_value t1591 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1591;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1592 = f_parse_46_head_token_1(*r_ts);
    int d1593 = 0;
    if ((!d1593) && (u_is_con(s1592, &n_lex_46_TName) && u_equals(u_field(s1592, 0), u_mk_lit("skip")))) {
      d1593 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1594 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1594;
      }
    }
    if ((!d1593) && (u_is_con(s1592, &n_lex_46_TName) && u_equals(u_field(s1592, 0), u_mk_lit("if")))) {
      d1593 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1593) && (u_is_con(s1592, &n_lex_46_TName) && u_equals(u_field(s1592, 0), u_mk_lit("while")))) {
      d1593 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1595 = U_LIT("end");
        p_parse_46_keyword(&l_end_1595, r_ts);
        u_value t1596 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1596;
      }
    }
    if ((!d1593) && (u_is_con(s1592, &n_lex_46_TName) && u_equals(u_field(s1592, 0), u_mk_lit("call")))) {
      d1593 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1593) && (u_is_con(s1592, &n_lex_46_TName) && u_equals(u_field(s1592, 0), u_mk_lit("match")))) {
      d1593 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1597 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1597;
      }
    }
    if ((!d1593) && (u_is_con(s1592, &n_lex_46_TName) && u_equals(u_field(s1592, 0), u_mk_lit("assert")))) {
      d1593 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1598 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1598;
      }
    }
    if ((!d1593) && (u_is_con(s1592, &n_lex_46_TName) && u_equals(u_field(s1592, 0), u_mk_lit("assign")))) {
      d1593 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1593) && (u_is_con(s1592, &n_lex_46_TName) && u_equals(u_field(s1592, 0), u_mk_lit("raise")))) {
      d1593 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1599 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1599;
      }
    }
    if ((!d1593) && (u_is_con(s1592, &n_lex_46_TName) && u_equals(u_field(s1592, 0), u_mk_lit("try")))) {
      d1593 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1600 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1600, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1601 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1601;
      }
    }
    if (!d1593) {
      d1593 = 1;
      {
        static struct u_object l_statement_32_expect_1602 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1603 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1602, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1603)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1593) {
      u_no_case(s1592);
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
    static struct u_object l_else_1604 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1604)))) {
      {
        static struct u_object l_end_1605 = U_LIT("end");
        p_parse_46_keyword(&l_end_1605, r_ts);
        u_value t1606 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1606;
      }
    } else {
      static struct u_object l_if_1607 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1607)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1608 = U_LIT("end");
          p_parse_46_keyword(&l_end_1608, r_ts);
        }
      }
    }
    u_value t1609 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1609;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1610 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1610), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1611 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1611;
    u_value t1614 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1614)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1612 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1612), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1613 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1613;
      }
      t1614 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1615 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1615;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1616 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1616;
    static struct u_object l_case_1617 = U_LIT("case");
    u_value t1620 = f_parse_46_at_keyword_2(*r_ts, &l_case_1617);
    while (u_truth(t1620)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1618 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1618), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1619 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1619;
      }
      static struct u_object l_case_1621 = U_LIT("case");
      t1620 = f_parse_46_at_keyword_2(*r_ts, &l_case_1621);
    }
    static struct u_object l_else_1622 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1622))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1623 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1623;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1624 = U_LIT("end");
    p_parse_46_keyword(&l_end_1624, r_ts);
    u_value t1625 = f_list_46_reverse_1(v_rev);
    *r_cs = t1625;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1627;
  if (u_truth(v_first)) {
    t1627 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1626 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1626)) {
      t1626 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1627 = t1626;
  }
  return t1627;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1629 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1629)) {
    static struct u_object l_out_1628 = U_LIT("out");
    t1629 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1628));
  }
  u_value t1631 = t1629;
  if (u_truth(t1631)) {
    static struct u_object l_ref_1630 = U_LIT("ref");
    t1631 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1630));
  }
  return t1631;
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
    u_value t1632 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), v_ins, v_outs, v_refs);
    *r_s = t1632;
  }
}

void p_parse_46_sections(u_value v_item_in, u_value v_item, u_value v_sc, u_value *r_ins, u_value *r_outs, u_value *r_refs, u_value *r_ts) {
  {
    u_value v_first = NULL;
    u_value t1633 = u_mk_bool(1);
    v_first = t1633;
    u_value t1634 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1635 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1636 = u_mk_con(&n_list_46_Nil, 0);
    *r_ins = t1634;
    *r_outs = t1635;
    *r_refs = t1636;
    static struct u_object l_in_1637 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1637))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_section_items(v_item_in, v_sc, r_ins, r_ts);
        u_value t1638 = u_mk_bool(0);
        v_first = t1638;
      }
    }
    static struct u_object l_out_1639 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1639, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_section_items(v_item, v_sc, r_outs, r_ts);
        u_value t1640 = u_mk_bool(0);
        v_first = t1640;
      }
    }
    static struct u_object l_ref_1641 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1641, v_first))) {
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
    u_value t1642 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1642;
    u_value t1644 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1644)) {
      {
        p_parse_46_advance(r_ts);
        u_call_proc(v_item, 1, 2, v_sc, &v_x, r_ts);
        u_value t1643 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1643;
      }
      t1644 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1645 = f_list_46_reverse_1(v_rev);
    *r_xs = t1645;
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
    u_value t1646 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0));
    *r_x = t1646;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1647 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1647;
    static struct u_object l__58__1648 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1648)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1649 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1649;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1650 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1650;
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
  u_value s1651 = v_cs;
  u_value t1652 = NULL;
  if ((!t1652) && u_is_con(s1651, &n_list_46_Nil)) {
    t1652 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1652) && (u_is_con(s1651, &n_list_46_Cons) && u_is_con(u_field(s1651, 0), &n_syntax_46_CEval))) {
    u_value v_more = u_field(s1651, 1);
    t1652 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1652) && (u_is_con(s1651, &n_list_46_Cons) && u_is_con(u_field(s1651, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1651, 1);
    t1652 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1652) && (u_is_con(s1651, &n_list_46_Cons) && u_is_con(u_field(s1651, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1651, 1);
    t1652 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1652) && u_is_con(s1651, &n_list_46_Cons)) {
    u_value v_c = u_field(s1651, 0);
    u_value v_more = u_field(s1651, 1);
    t1652 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1652) {
    u_no_case(s1651);
  }
  return t1652;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1653 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1654 = &t1653;
  u_value l1655 = v_cs;
  while (u_is_con(l1655, &n_list_46_Cons)) {
    u_value v_c = u_field(l1655, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1654 = u_append(e1654, v_c);
    }
    l1655 = u_field(l1655, 1);
  }
  return t1653;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1656 = v_c;
  u_value t1657 = NULL;
  if ((!t1657) && u_is_con(s1656, &n_syntax_46_CNotation)) {
    t1657 = u_mk_bool(1);
  }
  if (!t1657) {
    t1657 = u_mk_bool(0);
  }
  if (!t1657) {
    u_no_case(s1656);
  }
  return t1657;
}

u_value f_parse_46_Context_46_notations_1(u_value v_x) {
  u_value s1658 = v_x;
  u_value t1659 = NULL;
  if ((!t1659) && u_is_con(s1658, &n_parse_46_Context)) {
    u_value v_y = u_field(s1658, 0);
    t1659 = v_y;
  }
  if (!t1659) {
    u_no_case(s1658);
  }
  return t1659;
}

u_value f_parse_46_Context_46_modules_1(u_value v_x) {
  u_value s1660 = v_x;
  u_value t1661 = NULL;
  if ((!t1661) && u_is_con(s1660, &n_parse_46_Context)) {
    u_value v_y = u_field(s1660, 1);
    t1661 = v_y;
  }
  if (!t1661) {
    u_no_case(s1660);
  }
  return t1661;
}

u_value f_parse_46_Context_46_loading_1(u_value v_x) {
  u_value s1662 = v_x;
  u_value t1663 = NULL;
  if ((!t1663) && u_is_con(s1662, &n_parse_46_Context)) {
    u_value v_y = u_field(s1662, 2);
    t1663 = v_y;
  }
  if (!t1663) {
    u_no_case(s1662);
  }
  return t1663;
}

u_value f_parse_46_Context_46_base_1(u_value v_x) {
  u_value s1664 = v_x;
  u_value t1665 = NULL;
  if ((!t1665) && u_is_con(s1664, &n_parse_46_Context)) {
    u_value v_y = u_field(s1664, 3);
    t1665 = v_y;
  }
  if (!t1665) {
    u_no_case(s1664);
  }
  return t1665;
}

u_value f_parse_46_Context_46_dir_1(u_value v_x) {
  u_value s1666 = v_x;
  u_value t1667 = NULL;
  if ((!t1667) && u_is_con(s1666, &n_parse_46_Context)) {
    u_value v_y = u_field(s1666, 4);
    t1667 = v_y;
  }
  if (!t1667) {
    u_no_case(s1666);
  }
  return t1667;
}

u_value f_parse_46_Context_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1668 = v_x;
  u_value t1669 = NULL;
  if ((!t1669) && u_is_con(s1668, &n_parse_46_Context)) {
    u_value v_a1 = u_field(s1668, 1);
    u_value v_a2 = u_field(s1668, 2);
    u_value v_a3 = u_field(s1668, 3);
    u_value v_a4 = u_field(s1668, 4);
    t1669 = u_mk_con(&n_parse_46_Context, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1669) {
    u_no_case(s1668);
  }
  return t1669;
}

u_value f_parse_46_Context_46_modules_58__61__2(u_value v_x, u_value v_y) {
  u_value s1670 = v_x;
  u_value t1671 = NULL;
  if ((!t1671) && u_is_con(s1670, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1670, 0);
    u_value v_a2 = u_field(s1670, 2);
    u_value v_a3 = u_field(s1670, 3);
    u_value v_a4 = u_field(s1670, 4);
    t1671 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t1671) {
    u_no_case(s1670);
  }
  return t1671;
}

u_value f_parse_46_Context_46_loading_58__61__2(u_value v_x, u_value v_y) {
  u_value s1672 = v_x;
  u_value t1673 = NULL;
  if ((!t1673) && u_is_con(s1672, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1672, 0);
    u_value v_a1 = u_field(s1672, 1);
    u_value v_a3 = u_field(s1672, 3);
    u_value v_a4 = u_field(s1672, 4);
    t1673 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t1673) {
    u_no_case(s1672);
  }
  return t1673;
}

u_value f_parse_46_Context_46_base_58__61__2(u_value v_x, u_value v_y) {
  u_value s1674 = v_x;
  u_value t1675 = NULL;
  if ((!t1675) && u_is_con(s1674, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1674, 0);
    u_value v_a1 = u_field(s1674, 1);
    u_value v_a2 = u_field(s1674, 2);
    u_value v_a4 = u_field(s1674, 4);
    t1675 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t1675) {
    u_no_case(s1674);
  }
  return t1675;
}

u_value f_parse_46_Context_46_dir_58__61__2(u_value v_x, u_value v_y) {
  u_value s1676 = v_x;
  u_value t1677 = NULL;
  if ((!t1677) && u_is_con(s1676, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1676, 0);
    u_value v_a1 = u_field(s1676, 1);
    u_value v_a2 = u_field(s1676, 2);
    u_value v_a3 = u_field(s1676, 3);
    t1677 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t1677) {
    u_no_case(s1676);
  }
  return t1677;
}

u_value f_parse_46_with_commands_2(u_value v_cx, u_value v_cs) {
  return f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_with_notations_2(f_parse_46_Context_46_notations_1(v_cx), v_cs));
}

u_value f_parse_46_with_module_2(u_value v_cx, u_value v_c) {
  return f_parse_46_Context_46_modules_58__61__2(v_cx, f_list_46__43__43__2(f_parse_46_modules_of_1(v_c), f_parse_46_Context_46_modules_1(v_cx)));
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1678 = v_c;
  u_value t1679 = NULL;
  if ((!t1679) && u_is_con(s1678, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1678, 0);
    u_value v_cs = u_field(s1678, 1);
    u_value t1680 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1681 = &t1680;
    u_value l1682 = v_cs;
    while (u_is_con(l1682, &n_list_46_Cons)) {
      u_value v_d = u_field(l1682, 0);
      if (u_truth(u_mk_bool(1))) {
        e1681 = u_append(e1681, f_parse_46_modules_of_1(v_d));
      }
      l1682 = u_field(l1682, 1);
    }
    t1679 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1680));
  }
  if (!t1679) {
    t1679 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1679) {
    u_no_case(s1678);
  }
  return t1679;
}

u_value f_parse_46_own_notations_2(u_value v_cx, u_value v_n) {
  u_value s1683 = f_list_46_lookup_2(v_n, f_parse_46_Context_46_modules_1(v_cx));
  u_value t1684 = NULL;
  if ((!t1684) && u_is_con(s1683, &n_option_46_Some)) {
    u_value v_ns = u_field(s1683, 0);
    t1684 = v_ns;
  }
  if ((!t1684) && u_is_con(s1683, &n_option_46_None)) {
    t1684 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1684) {
    u_no_case(s1683);
  }
  return t1684;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1685 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1685;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1686 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1686;
        u_value t1688 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1688)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1687 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1687;
          }
          t1688 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1689 = f_list_46_reverse_1(v_rev);
    *r_ps = t1689;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1690 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1690;
    u_value s1691 = f_parse_46_head_token_1(*r_ts);
    int d1692 = 0;
    if ((!d1692) && (u_is_con(s1691, &n_lex_46_TName) && u_equals(u_field(s1691, 0), u_mk_lit("prefix")))) {
      d1692 = 1;
      {
        u_value t1693 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1693;
      }
    }
    if ((!d1692) && (u_is_con(s1691, &n_lex_46_TName) && u_equals(u_field(s1691, 0), u_mk_lit("postfix")))) {
      d1692 = 1;
      {
        u_value t1694 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1694;
      }
    }
    if (!d1692) {
      d1692 = 1;
      {
        u_value s1695 = f_parse_46_second_token_1(*r_ts);
        int d1696 = 0;
        if ((!d1696) && (u_is_con(s1695, &n_lex_46_TName) && u_equals(u_field(s1695, 0), u_mk_lit("left")))) {
          d1696 = 1;
          {
            u_value t1697 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1697;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1696) && (u_is_con(s1695, &n_lex_46_TName) && u_equals(u_field(s1695, 0), u_mk_lit("right")))) {
          d1696 = 1;
          {
            u_value t1698 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1698;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1696) {
          d1696 = 1;
          {
          }
        }
        if (!d1696) {
          u_no_case(s1695);
        }
      }
    }
    if (!d1692) {
      u_no_case(s1691);
    }
    p_parse_46_advance(r_ts);
    u_value s1699 = f_parse_46_head_token_1(*r_ts);
    int d1700 = 0;
    if ((!d1700) && u_is_con(s1699, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1699, 0);
      d1700 = 1;
      {
        u_value t1701 = v_n;
        v_p = t1701;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1700) {
      d1700 = 1;
      {
        static struct u_object l_precedence_32_expec_1702 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1703 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1702, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1703)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1700) {
      u_no_case(s1699);
    }
    u_value t1704 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1705 = u_mk_bool(1);
    v_ops = t1704;
    v_go = t1705;
    u_value t1711 = v_go;
    while (u_truth(t1711)) {
      {
        u_value s1706 = f_parse_46_head_token_1(*r_ts);
        int d1707 = 0;
        if ((!d1707) && u_is_con(s1706, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1706, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1707 = 1;
            {
              u_value t1708 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1708;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1707) && u_is_con(s1706, &n_lex_46_TName)) {
          u_value v_n = u_field(s1706, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1707 = 1;
            {
              u_value t1709 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1709;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1707) {
          d1707 = 1;
          {
            u_value t1710 = u_mk_bool(0);
            v_go = t1710;
          }
        }
        if (!d1707) {
          u_no_case(s1706);
        }
      }
      t1711 = v_go;
    }
    u_value s1712 = v_ops;
    int d1713 = 0;
    if ((!d1713) && u_is_con(s1712, &n_list_46_Nil)) {
      d1713 = 1;
      {
        static struct u_object l_operator_32_expecte_1714 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1715 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1714, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1715)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1713) {
      d1713 = 1;
      {
        u_value t1716 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1716;
      }
    }
    if (!d1713) {
      u_no_case(s1712);
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
    u_value t1717 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1718 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1717;
    *r_fs = t1718;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1719 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
            u_value t1720 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
            v_args = t1719;
            *r_fs = t1720;
            u_value t1723 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1723)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t1721 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t1722 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t1721;
                *r_fs = t1722;
              }
              t1723 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1724 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t1725 = f_list_46_reverse_1(*r_fs);
    *r_k = t1724;
    *r_fs = t1725;
    u_value s1726 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d1727 = 0;
    if ((!d1727) && u_is_con(s1726, &n_option_46_Some)) {
      u_value v_f = u_field(s1726, 0);
      d1727 = 1;
      {
        static struct u_object l_repeated_32_field_32__1728 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__1729 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__1728, u_concat(v_f, u_concat(&l__32_of_32__1729, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1727) && u_is_con(s1726, &n_option_46_None)) {
      d1727 = 1;
      {
      }
    }
    if (!d1727) {
      u_no_case(s1726);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1730 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1731 = 0;
    if ((!d1731) && (u_is_con(u_field(s1730, 0), &n_lex_46_TName) && (u_is_con(u_field(s1730, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1730, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1730, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1731 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1732 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1732;
        }
      }
    }
    if (!d1731) {
      d1731 = 1;
      {
        u_value t1733 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1733;
      }
    }
    if (!d1731) {
      u_no_case(s1730);
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
    static struct u_object l__58__61__1734 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1734), r_ts);
    static struct u_object l__124__1735 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1735)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1736 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1736;
    static struct u_object l__124__1737 = U_LIT("|");
    u_value t1739 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1737));
    while (u_truth(t1739)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1738 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1738;
      }
      static struct u_object l__124__1740 = U_LIT("|");
      t1739 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1740));
    }
    u_value s1741 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d1742 = 0;
    if ((!d1742) && u_is_con(s1741, &n_option_46_Some)) {
      u_value v_f = u_field(s1741, 0);
      d1742 = 1;
      {
        static struct u_object l_field_32__1743 = U_LIT("field ");
        static struct u_object l__32_of_32__1744 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_1745 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__1743, u_concat(v_f, u_concat(&l__32_of_32__1744, u_concat(v_n, &l__32_has_32_different_32_t_1745)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1742) && u_is_con(s1741, &n_option_46_None)) {
      d1742 = 1;
      {
      }
    }
    if (!d1742) {
      u_no_case(s1741);
    }
    u_value t1746 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_list_46__43__43__2(f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)), f_parse_46_updaters_3(v_n, v_ps, f_list_46_reverse_1(v_rev))));
    *r_cs = t1746;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s1747 = v_fs;
  u_value t1748 = NULL;
  if ((!t1748) && u_is_con(s1747, &n_list_46_Nil)) {
    t1748 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1748) && (u_is_con(s1747, &n_list_46_Cons) && u_is_con(u_field(s1747, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s1747, 0), 0);
    u_value v_rest = u_field(s1747, 1);
    t1748 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t1748) && (u_is_con(s1747, &n_list_46_Cons) && u_is_con(u_field(s1747, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1747, 1);
    t1748 = f_parse_46_field_names_1(v_rest);
  }
  if (!t1748) {
    u_no_case(s1747);
  }
  return t1748;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s1749 = v_fs;
  u_value t1750 = NULL;
  if ((!t1750) && u_is_con(s1749, &n_list_46_Nil)) {
    t1750 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1750) && u_is_con(s1749, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1749, 0), 0);
    u_value v_t = u_field(u_field(u_field(s1749, 0), 1), 3);
    u_value v_rest = u_field(s1749, 1);
    u_value t1752;
    if (u_truth(f_list_46_exists_2(f_parse_46_fields_named_2(v_f, v_rest), u_mk_closure(lam1751, 1, v_t)))) {
      t1752 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t1752 = f_parse_46_differing_1(v_rest);
    }
    t1750 = t1752;
  }
  if (!t1750) {
    u_no_case(s1749);
  }
  return t1750;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s1753 = v_e;
  u_value t1754 = NULL;
  if (!t1754) {
    u_value v_t = u_field(u_field(s1753, 1), 3);
    t1754 = v_t;
  }
  if (!t1754) {
    u_no_case(s1753);
  }
  return t1754;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s1755 = v_fs;
  u_value t1756 = NULL;
  if ((!t1756) && u_is_con(s1755, &n_list_46_Nil)) {
    t1756 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1756) && u_is_con(s1755, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s1755, 0), 0);
    u_value v_d = u_field(u_field(s1755, 0), 1);
    u_value v_rest = u_field(s1755, 1);
    u_value t1757;
    if (u_truth(u_eq(v_g, v_f))) {
      t1757 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t1757 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t1756 = t1757;
  }
  if (!t1756) {
    u_no_case(s1755);
  }
  return t1756;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1758 = v_ks;
  u_value t1759 = NULL;
  if ((!t1759) && u_is_con(s1758, &n_list_46_Nil)) {
    t1759 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1759) && (u_is_con(s1758, &n_list_46_Cons) && u_is_con(u_field(u_field(s1758, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1758, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1758, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1758, 0), 1);
    u_value v_rest = u_field(s1758, 1);
    t1759 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1759) {
    u_no_case(s1758);
  }
  return t1759;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1760 = v_fs;
  u_value t1761 = NULL;
  if ((!t1761) && u_is_con(s1760, &n_list_46_Nil)) {
    t1761 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1761) && (u_is_con(s1760, &n_list_46_Cons) && u_is_con(u_field(u_field(s1760, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1760, 0), 0), 0);
    u_value v_t = u_field(u_field(s1760, 0), 1);
    u_value v_rest = u_field(s1760, 1);
    t1761 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1761) && (u_is_con(s1760, &n_list_46_Cons) && u_is_con(u_field(u_field(s1760, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1760, 1);
    t1761 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1761) {
    u_no_case(s1760);
  }
  return t1761;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
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
        e1763 = u_append(e1763, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1764 = u_field(l1764, 1);
    }
    t1765 = t1762;
  }
  return t1765;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
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
  u_value t1778 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1767), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1779 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1768);
  u_value t1777;
  {
    u_value v_x = t1778;
    u_value v_owner = t1779;
    static struct u_object l__46__1771 = U_LIT(".");
    static struct u_object l_x_1772 = U_LIT("x");
    u_value t1773 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1774 = &t1773;
    u_value l1775 = v_es;
    while (u_is_con(l1775, &n_list_46_Cons)) {
      u_value v_e = u_field(l1775, 0);
      if (u_truth(u_mk_bool(1))) {
        e1774 = u_append(e1774, f_parse_46_field_case_1(v_e));
      }
      l1775 = u_field(l1775, 1);
    }
    u_value t1776;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1776 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1776 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1777 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1771, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1772), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1773, t1776)));
  }
  return t1777;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1780 = v_es;
  u_value t1781 = NULL;
  if ((!t1781) && u_is_con(s1780, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1780, 0), 1), 3);
    t1781 = v_t;
  }
  if (!t1781) {
    t1781 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1781) {
    u_no_case(s1780);
  }
  return t1781;
}

u_value f_parse_46_field_case_1(u_value v_e) {
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
          static struct u_object l_y_1787 = U_LIT("y");
          t1788 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1787), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1788 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1785 = u_append(e1785, t1788);
      }
      l1786 = u_field(l1786, 1);
    }
    static struct u_object l_y_1789 = U_LIT("y");
    t1783 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1784), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1789), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1783) {
    u_no_case(s1782);
  }
  return t1783;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_MissingField_1790 = U_LIT("MissingField");
  static struct u_object l__46__1791 = U_LIT(".");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_ERaise, 2, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_MissingField_1790), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_t, u_concat(&l__46__1791, v_f)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_parse_46_updaters_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1796 = f_parse_46_fields_of_1(v_ks);
  u_value t1795;
  {
    u_value v_fs = t1796;
    u_value t1792 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1793 = &t1792;
    u_value l1794 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1794, &n_list_46_Cons)) {
      u_value v_f = u_field(l1794, 0);
      if (u_truth(u_mk_bool(1))) {
        e1793 = u_append(e1793, f_parse_46_updater_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1794 = u_field(l1794, 1);
    }
    t1795 = t1792;
  }
  return t1795;
}

u_value f_parse_46_updater_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1797 = U_LIT("x");
  u_value t1798 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1799 = &t1798;
  u_value l1800 = v_ps;
  while (u_is_con(l1800, &n_list_46_Cons)) {
    u_value v_p = u_field(l1800, 0);
    if (u_truth(u_mk_bool(1))) {
      e1799 = u_append(e1799, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1800 = u_field(l1800, 1);
  }
  u_value t1810 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1797), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1811 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1798);
  u_value t1809;
  {
    u_value v_x = t1810;
    u_value v_owner = t1811;
    static struct u_object l__46__1801 = U_LIT(".");
    static struct u_object l__58__61__1802 = U_LIT(":=");
    static struct u_object l_x_1803 = U_LIT("x");
    static struct u_object l_y_1804 = U_LIT("y");
    u_value t1805 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1806 = &t1805;
    u_value l1807 = v_es;
    while (u_is_con(l1807, &n_list_46_Cons)) {
      u_value v_e = u_field(l1807, 0);
      if (u_truth(u_mk_bool(1))) {
        e1806 = u_append(e1806, f_parse_46_update_case_1(v_e));
      }
      l1807 = u_field(l1807, 1);
    }
    u_value t1808;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1808 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1808 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1809 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1801, u_concat(v_f, &l__58__61__1802)))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1803), v_owner), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1804), f_parse_46_field_type_1(v_es)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), v_owner, u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1805, t1808)));
  }
  return t1809;
}

u_value f_parse_46_update_case_1(u_value v_e) {
  u_value s1812 = v_e;
  u_value t1813 = NULL;
  if (!t1813) {
    u_value v_k = u_field(u_field(s1812, 1), 0);
    u_value v_i = u_field(u_field(s1812, 1), 1);
    u_value v_n = u_field(u_field(s1812, 1), 2);
    u_value t1814 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1815 = &t1814;
    u_value l1816 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1816, &n_list_46_Cons)) {
      u_value v_j = u_field(l1816, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1818;
        if (u_truth(u_eq(v_j, v_i))) {
          t1818 = u_mk_con(&n_syntax_46_PWild, 0);
        } else {
          static struct u_object l_a_1817 = U_LIT("a");
          t1818 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l_a_1817, u_str_show(v_j))), u_mk_con(&n_syntax_46_THole, 0)));
        }
        e1815 = u_append(e1815, t1818);
      }
      l1816 = u_field(l1816, 1);
    }
    u_value t1819 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1820 = &t1819;
    u_value l1821 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1821, &n_list_46_Cons)) {
      u_value v_j = u_field(l1821, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1824;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1822 = U_LIT("y");
          t1824 = &l_y_1822;
        } else {
          static struct u_object l_a_1823 = U_LIT("a");
          t1824 = u_concat(&l_a_1823, u_str_show(v_j));
        }
        e1820 = u_append(e1820, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(t1824), u_mk_con(&n_syntax_46_THole, 0))));
      }
      l1821 = u_field(l1821, 1);
    }
    t1813 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1814), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_k, u_mk_con(&n_syntax_46_THole, 0))), t1819));
  }
  if (!t1813) {
    u_no_case(s1812);
  }
  return t1813;
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t1825;
  if (u_truth(u_eq(v_i, v_n))) {
    t1825 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1825 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t1825;
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
    u_value t1826 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1826;
    u_value t1828 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1828)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1827 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1827;
      }
      t1828 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1829 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1829), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1830 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1830), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1831 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1831;
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
    static struct u_object l__58__61__1832 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1832), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1833 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1833;
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
    u_value t1834 = f_parse_46_Context_46_notations_1(v_cx);
    v_ns = t1834;
    u_value s1835 = f_parse_46_head_token_1(*r_ts);
    int d1836 = 0;
    if ((!d1836) && (u_is_con(s1835, &n_lex_46_TName) && u_equals(u_field(s1835, 0), u_mk_lit("import")))) {
      d1836 = 1;
      {
        u_value t1837 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1837;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Context_46_modules_1(v_cx))))) {
          {
            u_value t1838 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1838;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_cx, v_at, r_cs);
            u_value t1839 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1839;
          }
        }
      }
    }
    if ((!d1836) && u_is_con(s1835, &n_lex_46_TName)) {
      u_value v_w = u_field(s1835, 0);
      static struct u_object l_prefix_1841 = U_LIT("prefix");
      static struct u_object l_postfix_1842 = U_LIT("postfix");
      static struct u_object l_infix_1843 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1841, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1842, u_mk_con(&n_list_46_Cons, 2, &l_infix_1843, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1836 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1840 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1840;
        }
      }
    }
    if ((!d1836) && (u_is_con(s1835, &n_lex_46_TName) && u_equals(u_field(s1835, 0), u_mk_lit("type")))) {
      d1836 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1844 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1844), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1845 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1845;
      }
    }
    if ((!d1836) && (u_is_con(s1835, &n_lex_46_TName) && u_equals(u_field(s1835, 0), u_mk_lit("data")))) {
      d1836 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1836) && (u_is_con(s1835, &n_lex_46_TName) && u_equals(u_field(s1835, 0), u_mk_lit("function")))) {
      d1836 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1846 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1846;
      }
    }
    if ((!d1836) && (u_is_con(s1835, &n_lex_46_TName) && u_equals(u_field(s1835, 0), u_mk_lit("procedure")))) {
      d1836 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1847 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1847;
      }
    }
    if ((!d1836) && (u_is_con(s1835, &n_lex_46_TName) && u_equals(u_field(s1835, 0), u_mk_lit("eval")))) {
      d1836 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_e, r_ts);
        u_value t1848 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CEval, 1, v_e), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1848;
      }
    }
    if ((!d1836) && (u_is_con(s1835, &n_lex_46_TName) && u_equals(u_field(s1835, 0), u_mk_lit("run")))) {
      d1836 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1849 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1849), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1850 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1850;
      }
    }
    if ((!d1836) && (u_is_con(s1835, &n_lex_46_TName) && u_equals(u_field(s1835, 0), u_mk_lit("test")))) {
      d1836 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1851 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1851), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1852 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1852;
      }
    }
    if (!d1836) {
      d1836 = 1;
      {
        static struct u_object l_declaration_32_expe_1853 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1854 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1853, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1854)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1836) {
      u_no_case(s1835);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_cx) {
  {
    u_value s1855 = v_cs;
    int d1856 = 0;
    if ((!d1856) && u_is_con(s1855, &n_list_46_Nil)) {
      d1856 = 1;
      {
      }
    }
    if ((!d1856) && (u_is_con(s1855, &n_list_46_Cons) && u_is_con(u_field(s1855, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1855, 0), 0);
      u_value v_p = u_field(u_field(s1855, 0), 1);
      u_value v_ops = u_field(u_field(s1855, 0), 2);
      u_value v_more = u_field(s1855, 1);
      d1856 = 1;
      {
        u_value t1857 = f_parse_46_with_commands_2(*r_cx, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_cx = t1857;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1856) && (u_is_con(s1855, &n_list_46_Cons) && u_is_con(u_field(s1855, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1855, 0), 0);
      u_value v_ds = u_field(u_field(s1855, 0), 1);
      u_value v_more = u_field(s1855, 1);
      d1856 = 1;
      {
        u_value t1858 = f_parse_46_with_module_2(*r_cx, u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds));
        *r_cx = t1858;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1856) && (u_is_con(s1855, &n_list_46_Cons) && u_is_con(u_field(s1855, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1855, 0), 0);
      u_value v_more = u_field(s1855, 1);
      d1856 = 1;
      {
        u_value t1859 = f_parse_46_with_commands_2(*r_cx, f_parse_46_own_notations_2(*r_cx, v_n));
        *r_cx = t1859;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1856) && u_is_con(s1855, &n_list_46_Cons)) {
      u_value v_more = u_field(s1855, 1);
      d1856 = 1;
      {
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if (!d1856) {
      u_no_case(s1855);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_cx, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1860 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1860;
    u_value t1862 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1862)) {
      {
        p_parse_46_parse_command(*r_cx, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_cx);
        u_value t1861 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1861;
      }
      t1862 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1863 = f_list_46_reverse_1(v_rev);
    *r_cs = t1863;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1864 = v_dir;
    int d1865 = 0;
    if ((!d1865) && u_is_con(s1864, &n_option_46_Some)) {
      u_value v_d = u_field(s1864, 0);
      d1865 = 1;
      {
        {
          struct u_handler *h1868 = u_push();
          if (setjmp(h1868->env) == 0) {
            {
              static struct u_object l__47__1866 = U_LIT("/");
              static struct u_object l__46_u_1867 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1866, u_concat(v_n, &l__46_u_1867))), r_text);
            }
            u_pop(h1868);
          } else {
            u_pop(h1868);
            int d1869 = 0;
            if ((!d1869) && (u_has_type(h1868, "os.OsError") && u_is_con(u_caught(h1868), &n_os_46_OsError))) {
              d1869 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1869) {
              u_reraise(h1868);
            }
          }
        }
      }
    }
    if ((!d1865) && u_is_con(s1864, &n_option_46_None)) {
      d1865 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1865) {
      u_no_case(s1864);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1870 = u_push();
      if (setjmp(h1870->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1870);
      } else {
        u_pop(h1870);
        int d1871 = 0;
        if ((!d1871) && (u_has_type(h1870, "os.OsError") && u_is_con(u_caught(h1870), &n_os_46_OsError))) {
          d1871 = 1;
          {
            static struct u_object l_unknown_32_module_32__1872 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1872, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1871) {
          u_reraise(h1870);
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
        static struct u_object l_cyclic_32_import_32_of_1873 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1873, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Context_46_dir_1(v_cx), v_at, &v_text);
    u_value t1874 = f_parse_46_Context_46_loading_58__61__2(f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_base_notations_1(v_cx)), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Context_46_loading_1(v_cx)));
    v_mcx = t1874;
    {
      struct u_handler *h1875 = u_push();
      if (setjmp(h1875->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mcx, &v_mts);
        }
        u_pop(h1875);
      } else {
        u_pop(h1875);
        int d1876 = 0;
        if ((!d1876) && (u_has_type(h1875, "lex.LexError") && u_is_con(u_caught(h1875), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1875), 0);
          u_value v_i = u_field(u_caught(h1875), 1);
          d1876 = 1;
          {
            static struct u_object l__32_in_32__1877 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1877, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1876) && (u_has_type(h1875, "parse.ParseError") && u_is_con(u_caught(h1875), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1875), 0);
          u_value v_i = u_field(u_caught(h1875), 1);
          d1876 = 1;
          {
            static struct u_object l__32_in_32__1878 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1878, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1876) {
          u_reraise(h1875);
        }
      }
    }
  }
}

u_value f_parse_46_base_notations_1(u_value v_cx) {
  u_value s1879 = f_parse_46_Context_46_base_1(v_cx);
  u_value t1880 = NULL;
  if ((!t1880) && u_is_con(s1879, &n_option_46_Some)) {
    u_value v_b = u_field(s1879, 0);
    t1880 = v_b;
  }
  if ((!t1880) && u_is_con(s1879, &n_option_46_None)) {
    t1880 = f_parse_46_Context_46_notations_1(v_cx);
  }
  if (!t1880) {
    u_no_case(s1879);
  }
  return t1880;
}

void p_parse_46_parse_text(u_value v_text, u_value v_cx, u_value *r_p) {
  {
    u_value v_ts = NULL;
    u_value v_cs = NULL;
    u_value v_after = NULL;
    u_value t1881 = v_cx;
    v_after = t1881;
    {
      struct u_handler *h1882 = u_push();
      if (setjmp(h1882->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1882);
      } else {
        u_pop(h1882);
        int d1883 = 0;
        if ((!d1883) && (u_has_type(h1882, "lex.LexError") && u_is_con(u_caught(h1882), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1882), 0);
          u_value v_i = u_field(u_caught(h1882), 1);
          d1883 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1883) {
          u_reraise(h1882);
        }
      }
    }
    p_parse_46_parse_commands(&v_cs, &v_after, &v_ts);
    u_value t1884 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1884;
  }
}

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p) {
  {
    u_value v_cx = NULL;
    u_value t1885 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    v_cx = t1885;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), &v_cx);
    p_parse_46_parse_text(v_text, f_parse_46_Context_46_dir_58__61__2(f_parse_46_Context_46_base_58__61__2(f_parse_46_Context_46_loading_58__61__2(v_cx, u_mk_con(&n_list_46_Cons, 2, v_name, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_option_46_Some, 1, f_parse_46_Context_46_notations_1(v_cx))), v_dir), r_p);
  }
}

void p_parse_46_parse_library(u_value v_n, u_value *r_p) {
  {
    u_value v_text = NULL;
    u_value v_pre = NULL;
    p_parse_46_library_text(v_n, u_mk_int(0), &v_text);
    static struct u_object l_prelude_1886 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1886))) {
      {
        static struct u_object l_prelude_1887 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1887, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1888 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1888, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1889 = v_xs;
  u_value t1890 = NULL;
  if ((!t1890) && (u_is_con(s1889, &n_list_46_Cons) && u_is_con(u_field(s1889, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1889, 0);
    t1890 = v_x;
  }
  if ((!t1890) && u_is_con(s1889, &n_list_46_Cons)) {
    u_value v_more = u_field(s1889, 1);
    t1890 = f_parse_46_last_part_1(v_more);
  }
  if (!t1890) {
    static struct u_object l__1891 = U_LIT("");
    t1890 = &l__1891;
  }
  if (!t1890) {
    u_no_case(s1889);
  }
  return t1890;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1896 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1895;
  {
    u_value v_f = t1896;
    u_value t1893 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1893)) {
      static struct u_object l__46_u_1892 = U_LIT(".u");
      t1893 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1892);
    }
    u_value t1894;
    if (u_truth(t1893)) {
      t1894 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1894 = v_f;
    }
    t1895 = t1894;
  }
  return t1895;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1901 = u_str_split(v_path, u_mk_char(47));
  u_value t1900;
  {
    u_value v_parts = t1901;
    u_value t1899;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1897 = U_LIT(".");
      t1899 = &l__46__1897;
    } else {
      static struct u_object l__47__1898 = U_LIT("/");
      t1899 = u_str_join(&l__47__1898, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1900 = t1899;
  }
  return t1900;
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
    static struct u_object l_prelude_1902 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1902, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1903 = u_mk_tuple(2, v_cs, v_ds);
    int d1904 = 0;
    if ((!d1904) && (u_is_con(u_field(s1903, 0), &n_list_46_Cons) && u_is_con(u_field(s1903, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1903, 0), 0);
      u_value v_more = u_field(u_field(s1903, 0), 1);
      u_value v_d = u_field(u_field(s1903, 1), 0);
      u_value v_rest = u_field(u_field(s1903, 1), 1);
      d1904 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1904) {
      d1904 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1904) {
      u_no_case(s1903);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1905 = v_paths;
    int d1906 = 0;
    if ((!d1906) && u_is_con(s1905, &n_list_46_Nil)) {
      d1906 = 1;
      {
      }
    }
    if ((!d1906) && u_is_con(s1905, &n_list_46_Cons)) {
      u_value v_path = u_field(s1905, 0);
      u_value v_more = u_field(s1905, 1);
      d1906 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1906) {
      u_no_case(s1905);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1907 = v_paths;
    int d1908 = 0;
    if ((!d1908) && u_is_con(s1907, &n_list_46_Nil)) {
      d1908 = 1;
      {
      }
    }
    if ((!d1908) && u_is_con(s1907, &n_list_46_Cons)) {
      u_value v_path = u_field(s1907, 0);
      u_value v_more = u_field(s1907, 1);
      d1908 = 1;
      {
        u_value t1909 = u_mk_bool(0);
        v_failed = t1909;
        {
          struct u_handler *h1910 = u_push();
          if (setjmp(h1910->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1910);
          } else {
            u_pop(h1910);
            int d1911 = 0;
            if ((!d1911) && (u_has_type(h1910, "parse.ParseError") && u_is_con(u_caught(h1910), &n_parse_46_ParseError))) {
              d1911 = 1;
              {
                u_value t1912 = u_mk_bool(1);
                v_failed = t1912;
              }
            }
            if (!d1911) {
              u_reraise(h1910);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1908) {
      u_no_case(s1907);
    }
  }
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s1913 = v_x;
  u_value t1914 = NULL;
  if ((!t1914) && u_is_con(s1913, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1913, 0);
    t1914 = v_y;
  }
  if (!t1914) {
    u_no_case(s1913);
  }
  return t1914;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s1915 = v_x;
  u_value t1916 = NULL;
  if ((!t1916) && u_is_con(s1915, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1915, 1);
    t1916 = v_y;
  }
  if (!t1916) {
    u_no_case(s1915);
  }
  return t1916;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s1917 = v_x;
  u_value t1918 = NULL;
  if ((!t1918) && u_is_con(s1917, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1917, 2);
    t1918 = v_y;
  }
  if (!t1918) {
    u_no_case(s1917);
  }
  return t1918;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s1919 = v_x;
  u_value t1920 = NULL;
  if ((!t1920) && u_is_con(s1919, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1919, 3);
    t1920 = v_y;
  }
  if (!t1920) {
    u_no_case(s1919);
  }
  return t1920;
}

u_value f_compile_46_Globals_46_procs_1(u_value v_x) {
  u_value s1921 = v_x;
  u_value t1922 = NULL;
  if ((!t1922) && u_is_con(s1921, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1921, 4);
    t1922 = v_y;
  }
  if (!t1922) {
    u_no_case(s1921);
  }
  return t1922;
}

u_value f_compile_46_Globals_46_funs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1923 = v_x;
  u_value t1924 = NULL;
  if ((!t1924) && u_is_con(s1923, &n_compile_46_Globals)) {
    u_value v_a1 = u_field(s1923, 1);
    u_value v_a2 = u_field(s1923, 2);
    u_value v_a3 = u_field(s1923, 3);
    u_value v_a4 = u_field(s1923, 4);
    t1924 = u_mk_con(&n_compile_46_Globals, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1924) {
    u_no_case(s1923);
  }
  return t1924;
}

u_value f_compile_46_Globals_46_constrs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1925 = v_x;
  u_value t1926 = NULL;
  if ((!t1926) && u_is_con(s1925, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1925, 0);
    u_value v_a2 = u_field(s1925, 2);
    u_value v_a3 = u_field(s1925, 3);
    u_value v_a4 = u_field(s1925, 4);
    t1926 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t1926) {
    u_no_case(s1925);
  }
  return t1926;
}

u_value f_compile_46_Globals_46_arities_58__61__2(u_value v_x, u_value v_y) {
  u_value s1927 = v_x;
  u_value t1928 = NULL;
  if ((!t1928) && u_is_con(s1927, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1927, 0);
    u_value v_a1 = u_field(s1927, 1);
    u_value v_a3 = u_field(s1927, 3);
    u_value v_a4 = u_field(s1927, 4);
    t1928 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t1928) {
    u_no_case(s1927);
  }
  return t1928;
}

u_value f_compile_46_Globals_46_fields_58__61__2(u_value v_x, u_value v_y) {
  u_value s1929 = v_x;
  u_value t1930 = NULL;
  if ((!t1930) && u_is_con(s1929, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1929, 0);
    u_value v_a1 = u_field(s1929, 1);
    u_value v_a2 = u_field(s1929, 2);
    u_value v_a4 = u_field(s1929, 4);
    t1930 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t1930) {
    u_no_case(s1929);
  }
  return t1930;
}

u_value f_compile_46_Globals_46_procs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1931 = v_x;
  u_value t1932 = NULL;
  if ((!t1932) && u_is_con(s1931, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1931, 0);
    u_value v_a1 = u_field(s1931, 1);
    u_value v_a2 = u_field(s1931, 2);
    u_value v_a3 = u_field(s1931, 3);
    t1932 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t1932) {
    u_no_case(s1931);
  }
  return t1932;
}

u_value f_compile_46_Gen_46_count_1(u_value v_x) {
  u_value s1933 = v_x;
  u_value t1934 = NULL;
  if ((!t1934) && u_is_con(s1933, &n_compile_46_Gen)) {
    u_value v_y = u_field(s1933, 0);
    t1934 = v_y;
  }
  if (!t1934) {
    u_no_case(s1933);
  }
  return t1934;
}

u_value f_compile_46_Gen_46_lifted_1(u_value v_x) {
  u_value s1935 = v_x;
  u_value t1936 = NULL;
  if ((!t1936) && u_is_con(s1935, &n_compile_46_Gen)) {
    u_value v_y = u_field(s1935, 1);
    t1936 = v_y;
  }
  if (!t1936) {
    u_no_case(s1935);
  }
  return t1936;
}

u_value f_compile_46_Gen_46_count_58__61__2(u_value v_x, u_value v_y) {
  u_value s1937 = v_x;
  u_value t1938 = NULL;
  if ((!t1938) && u_is_con(s1937, &n_compile_46_Gen)) {
    u_value v_a1 = u_field(s1937, 1);
    t1938 = u_mk_con(&n_compile_46_Gen, 2, v_y, v_a1);
  }
  if (!t1938) {
    u_no_case(s1937);
  }
  return t1938;
}

u_value f_compile_46_Gen_46_lifted_58__61__2(u_value v_x, u_value v_y) {
  u_value s1939 = v_x;
  u_value t1940 = NULL;
  if ((!t1940) && u_is_con(s1939, &n_compile_46_Gen)) {
    u_value v_a0 = u_field(s1939, 0);
    t1940 = u_mk_con(&n_compile_46_Gen, 2, v_a0, v_y);
  }
  if (!t1940) {
    u_no_case(s1939);
  }
  return t1940;
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
  static struct u_object l_u_value_1941 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1941);
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
  static struct u_object l_u_value_1942 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1942));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t1943 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1944 = &t1943;
  u_value l1945 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l1945, &n_list_46_Cons)) {
    u_value v_c = u_field(l1945, 0);
    if (u_truth(u_mk_bool(1))) {
      e1944 = u_append(e1944, f_compile_46_mangle_char_1(v_c));
    }
    l1945 = u_field(l1945, 1);
  }
  return u_string(f_list_46_concat_1(t1943));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t1946 = u_is_letter(v_c);
  if (!u_truth(t1946)) {
    t1946 = u_is_digit(v_c);
  }
  u_value t1947 = t1946;
  if (!u_truth(t1947)) {
    t1947 = u_eq(v_c, u_mk_char(95));
  }
  u_value t1950;
  if (u_truth(t1947)) {
    t1950 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___1948 = U_LIT("_");
    static struct u_object l___1949 = U_LIT("_");
    t1950 = u_chars(u_concat(&l___1948, u_concat(u_str_show(u_ord(v_c)), &l___1949)));
  }
  return t1950;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__1951 = U_LIT("f_");
  static struct u_object l___1952 = U_LIT("_");
  return u_concat(&l_f__1951, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___1952, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__1953 = U_LIT("p_");
  return u_concat(&l_p__1953, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__1954 = U_LIT("v_");
  return u_concat(&l_v__1954, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__1955 = U_LIT("r_");
  return u_concat(&l_r__1955, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__1956 = U_LIT("+");
  static struct u_object l_u_add_1957 = U_LIT("u_add");
  static struct u_object l__45__1958 = U_LIT("-");
  static struct u_object l_u_sub_1959 = U_LIT("u_sub");
  static struct u_object l__42__1960 = U_LIT("*");
  static struct u_object l_u_mul_1961 = U_LIT("u_mul");
  static struct u_object l__47__1962 = U_LIT("/");
  static struct u_object l_u_div_1963 = U_LIT("u_div");
  static struct u_object l__37__1964 = U_LIT("%");
  static struct u_object l_u_mod_1965 = U_LIT("u_mod");
  static struct u_object l__60__1966 = U_LIT("<");
  static struct u_object l_u_lt_1967 = U_LIT("u_lt");
  static struct u_object l__61__61__1968 = U_LIT("==");
  static struct u_object l_u_eq_1969 = U_LIT("u_eq");
  static struct u_object l_compare_1970 = U_LIT("compare");
  static struct u_object l_u_compare_1971 = U_LIT("u_compare");
  static struct u_object l_chars_1972 = U_LIT("chars");
  static struct u_object l_u_chars_1973 = U_LIT("u_chars");
  static struct u_object l_string_1974 = U_LIT("string");
  static struct u_object l_u_string_1975 = U_LIT("u_string");
  static struct u_object l_ord_1976 = U_LIT("ord");
  static struct u_object l_u_ord_1977 = U_LIT("u_ord");
  static struct u_object l_chr_1978 = U_LIT("chr");
  static struct u_object l_u_chr_1979 = U_LIT("u_chr");
  static struct u_object l__94__1980 = U_LIT("^");
  static struct u_object l_u_concat_1981 = U_LIT("u_concat");
  static struct u_object l_str_size_1982 = U_LIT("str_size");
  static struct u_object l_u_str_size_1983 = U_LIT("u_str_size");
  static struct u_object l_str_join_1984 = U_LIT("str_join");
  static struct u_object l_u_str_join_1985 = U_LIT("u_str_join");
  static struct u_object l_str_slice_1986 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_1987 = U_LIT("u_str_slice");
  static struct u_object l_str_split_1988 = U_LIT("str_split");
  static struct u_object l_u_str_split_1989 = U_LIT("u_str_split");
  static struct u_object l_str_get_1990 = U_LIT("str_get");
  static struct u_object l_u_str_get_1991 = U_LIT("u_str_get");
  static struct u_object l_str_contains_1992 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_1993 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_1994 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_1995 = U_LIT("u_str_compare");
  static struct u_object l_str_show_1996 = U_LIT("str_show");
  static struct u_object l_u_str_show_1997 = U_LIT("u_str_show");
  static struct u_object l_str_quote_1998 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_1999 = U_LIT("u_str_quote");
  static struct u_object l_str_number_2000 = U_LIT("str_number");
  static struct u_object l_u_str_number_2001 = U_LIT("u_str_number");
  static struct u_object l_is_digit_2002 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_2003 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_2004 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_2005 = U_LIT("u_is_letter");
  static struct u_object l_is_space_2006 = U_LIT("is_space");
  static struct u_object l_u_is_space_2007 = U_LIT("u_is_space");
  u_pop(handler);
  cache = f_map_46_put_all_2(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__1956), u_mk_tuple(2, &l_u_add_1957, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__1958), u_mk_tuple(2, &l_u_sub_1959, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__1960), u_mk_tuple(2, &l_u_mul_1961, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__1962), u_mk_tuple(2, &l_u_div_1963, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__1964), u_mk_tuple(2, &l_u_mod_1965, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__1966), u_mk_tuple(2, &l_u_lt_1967, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__1968), u_mk_tuple(2, &l_u_eq_1969, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_1970), u_mk_tuple(2, &l_u_compare_1971, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_1972), u_mk_tuple(2, &l_u_chars_1973, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_1974), u_mk_tuple(2, &l_u_string_1975, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_1976), u_mk_tuple(2, &l_u_ord_1977, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_1978), u_mk_tuple(2, &l_u_chr_1979, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__1980), u_mk_tuple(2, &l_u_concat_1981, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_1982), u_mk_tuple(2, &l_u_str_size_1983, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_1984), u_mk_tuple(2, &l_u_str_join_1985, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_1986), u_mk_tuple(2, &l_u_str_slice_1987, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_1988), u_mk_tuple(2, &l_u_str_split_1989, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_1990), u_mk_tuple(2, &l_u_str_get_1991, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_1992), u_mk_tuple(2, &l_u_str_contains_1993, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_1994), u_mk_tuple(2, &l_u_str_compare_1995, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_1996), u_mk_tuple(2, &l_u_str_show_1997, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_1998), u_mk_tuple(2, &l_u_str_quote_1999, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_2000), u_mk_tuple(2, &l_u_str_number_2001, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_2002), u_mk_tuple(2, &l_u_is_digit_2003, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_2004), u_mk_tuple(2, &l_u_is_letter_2005, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_2006), u_mk_tuple(2, &l_u_is_space_2007, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))), f_map_46_empty_0());
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__2008 = U_LIT("&");
  static struct u_object l_n__2009 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__2008, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__2009, f_compile_46_mangle_1(v_n))));
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
  u_value s2010 = v_n;
  u_value t2011 = NULL;
  if ((!t2011) && (u_is_con(s2010, &n_syntax_46_Name) && u_is_con(u_field(s2010, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s2010, 0), 0);
    u_value v_b = u_field(s2010, 1);
    static struct u_object l_const_32_struct_32_u_n_2012 = U_LIT("const struct u_name");
    static struct u_object l_n__2013 = U_LIT("n_");
    t2011 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2012), u_concat(&l_n__2013, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t2011) && (u_is_con(s2010, &n_syntax_46_Name) && u_is_con(u_field(s2010, 0), &n_option_46_None))) {
    u_value v_b = u_field(s2010, 1);
    static struct u_object l_const_32_struct_32_u_n_2014 = U_LIT("const struct u_name");
    static struct u_object l_n__2015 = U_LIT("n_");
    static struct u_object l_NULL_2016 = U_LIT("NULL");
    t2011 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2014), u_concat(&l_n__2015, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2016), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t2011) {
    u_no_case(s2010);
  }
  return t2011;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s2017 = v_cs;
  u_value t2018 = NULL;
  if ((!t2018) && u_is_con(s2017, &n_list_46_Nil)) {
    t2018 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2018) && (u_is_con(s2017, &n_list_46_Cons) && u_is_con(u_field(s2017, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s2017, 0), 2);
    u_value v_rest = u_field(s2017, 1);
    u_value t2019 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2020 = &t2019;
    u_value l2021 = v_ks;
    while (u_is_con(l2021, &n_list_46_Cons)) {
      u_value v_k = u_field(l2021, 0);
      if (u_truth(u_mk_bool(1))) {
        e2020 = u_append(e2020, f_syntax_46_Constr_46_name_1(v_k));
      }
      l2021 = u_field(l2021, 1);
    }
    t2018 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t2019, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t2018) && u_is_con(s2017, &n_list_46_Cons)) {
    u_value v_rest = u_field(s2017, 1);
    t2018 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t2018) {
    u_no_case(s2017);
  }
  return t2018;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s2022 = v_ks;
  u_value t2023 = NULL;
  if ((!t2023) && u_is_con(s2022, &n_list_46_Nil)) {
    t2023 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2023) && u_is_con(s2022, &n_list_46_Cons)) {
    u_value v_k = u_field(s2022, 0);
    u_value v_rest = u_field(s2022, 1);
    u_value t2024;
    if (u_truth(f_set_46_member_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t2024 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t2024 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t2023 = t2024;
  }
  if (!t2023) {
    u_no_case(s2022);
  }
  return t2023;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t2025 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2026 = &t2025;
  u_value l2027 = v_xs;
  while (u_is_con(l2027, &n_list_46_Cons)) {
    u_value v_x = u_field(l2027, 0);
    if (u_truth(u_mk_bool(1))) {
      e2026 = u_append(e2026, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l2027 = u_field(l2027, 1);
  }
  return t2025;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__2028 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__2028, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s2029 = f_list_46_lookup_2(v_x, v_env);
  u_value t2030 = NULL;
  if ((!t2030) && u_is_con(s2029, &n_option_46_Some)) {
    u_value v_c = u_field(s2029, 0);
    t2030 = v_c;
  }
  if (!t2030) {
    t2030 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t2030) {
    u_no_case(s2029);
  }
  return t2030;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s2031 = v_c;
  u_value t2032 = NULL;
  if ((!t2032) && (u_is_con(s2031, &n_c_46_CeUnary) && u_equals(u_field(s2031, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s2031, 1);
    t2032 = v_p;
  }
  if (!t2032) {
    static struct u_object l__38__2033 = U_LIT("&");
    t2032 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2033, v_c);
  }
  if (!t2032) {
    u_no_case(s2031);
  }
  return t2032;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s2034 = v_t;
  u_value t2035 = NULL;
  if ((!t2035) && (u_is_con(s2034, &n_syntax_46_TSort) && u_is_con(u_field(s2034, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s2034, 0);
    t2035 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t2035) && u_is_con(s2034, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s2034, 0);
    u_value v_args = u_field(s2034, 1);
    static struct u_object l__91__2036 = U_LIT("[");
    static struct u_object l__44__32__2037 = U_LIT(", ");
    u_value t2038 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2039 = &t2038;
    u_value l2040 = v_args;
    while (u_is_con(l2040, &n_list_46_Cons)) {
      u_value v_a = u_field(l2040, 0);
      if (u_truth(u_mk_bool(1))) {
        e2039 = u_append(e2039, f_compile_46_type_key_1(v_a));
      }
      l2040 = u_field(l2040, 1);
    }
    static struct u_object l__93__2041 = U_LIT("]");
    t2035 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__2036, u_concat(u_str_join(&l__44__32__2037, t2038), &l__93__2041)));
  }
  if ((!t2035) && u_is_con(s2034, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s2034, 0);
    u_value v_r = u_field(s2034, 1);
    static struct u_object l__40__2042 = U_LIT("(");
    static struct u_object l__44__32__2043 = U_LIT(", ");
    u_value t2044 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2045 = &t2044;
    u_value l2046 = v_args;
    while (u_is_con(l2046, &n_list_46_Cons)) {
      u_value v_a = u_field(l2046, 0);
      if (u_truth(u_mk_bool(1))) {
        e2045 = u_append(e2045, f_compile_46_type_key_1(v_a));
      }
      l2046 = u_field(l2046, 1);
    }
    static struct u_object l__41__32__45__62__32__2047 = U_LIT(") -> ");
    t2035 = u_concat(&l__40__2042, u_concat(u_str_join(&l__44__32__2043, t2044), u_concat(&l__41__32__45__62__32__2047, f_compile_46_type_key_1(v_r))));
  }
  if ((!t2035) && u_is_con(s2034, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s2034, 0);
    u_value v_os = u_field(s2034, 1);
    u_value v_rs = u_field(s2034, 2);
    static struct u_object l__40_in_32__2048 = U_LIT("(in ");
    static struct u_object l__44__32__2049 = U_LIT(", ");
    u_value t2050 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2051 = &t2050;
    u_value l2052 = v_is;
    while (u_is_con(l2052, &n_list_46_Cons)) {
      u_value v_a = u_field(l2052, 0);
      if (u_truth(u_mk_bool(1))) {
        e2051 = u_append(e2051, f_compile_46_type_key_1(v_a));
      }
      l2052 = u_field(l2052, 1);
    }
    static struct u_object l__44__32_out_32__2053 = U_LIT(", out ");
    static struct u_object l__44__32__2054 = U_LIT(", ");
    u_value t2055 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2056 = &t2055;
    u_value l2057 = v_os;
    while (u_is_con(l2057, &n_list_46_Cons)) {
      u_value v_a = u_field(l2057, 0);
      if (u_truth(u_mk_bool(1))) {
        e2056 = u_append(e2056, f_compile_46_type_key_1(v_a));
      }
      l2057 = u_field(l2057, 1);
    }
    static struct u_object l__44__32_ref_32__2058 = U_LIT(", ref ");
    static struct u_object l__44__32__2059 = U_LIT(", ");
    u_value t2060 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2061 = &t2060;
    u_value l2062 = v_rs;
    while (u_is_con(l2062, &n_list_46_Cons)) {
      u_value v_a = u_field(l2062, 0);
      if (u_truth(u_mk_bool(1))) {
        e2061 = u_append(e2061, f_compile_46_type_key_1(v_a));
      }
      l2062 = u_field(l2062, 1);
    }
    static struct u_object l__41__2063 = U_LIT(")");
    t2035 = u_concat(&l__40_in_32__2048, u_concat(u_str_join(&l__44__32__2049, t2050), u_concat(&l__44__32_out_32__2053, u_concat(u_str_join(&l__44__32__2054, t2055), u_concat(&l__44__32_ref_32__2058, u_concat(u_str_join(&l__44__32__2059, t2060), &l__41__2063))))));
  }
  if ((!t2035) && u_is_con(s2034, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s2034, 0);
    static struct u_object l__40__2064 = U_LIT("(");
    static struct u_object l__32__42__32__2065 = U_LIT(" * ");
    u_value t2066 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2067 = &t2066;
    u_value l2068 = v_ts;
    while (u_is_con(l2068, &n_list_46_Cons)) {
      u_value v_a = u_field(l2068, 0);
      if (u_truth(u_mk_bool(1))) {
        e2067 = u_append(e2067, f_compile_46_type_key_1(v_a));
      }
      l2068 = u_field(l2068, 1);
    }
    static struct u_object l__41__2069 = U_LIT(")");
    t2035 = u_concat(&l__40__2064, u_concat(u_str_join(&l__32__42__32__2065, t2066), &l__41__2069));
  }
  if (!t2035) {
    static struct u_object l__63__2070 = U_LIT("?");
    t2035 = &l__63__2070;
  }
  if (!t2035) {
    u_no_case(s2034);
  }
  return t2035;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t2071 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2072 = &t2071;
  u_value l2073 = v_cs;
  while (u_is_con(l2073, &n_list_46_Cons)) {
    u_value v_c = u_field(l2073, 0);
    if (u_truth(u_mk_bool(1))) {
      e2072 = u_append(e2072, f_compile_46_fun_arities_1(v_c));
    }
    l2073 = u_field(l2073, 1);
  }
  u_value t2074 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2075 = &t2074;
  u_value l2076 = v_cs;
  while (u_is_con(l2076, &n_list_46_Cons)) {
    u_value v_c = u_field(l2076, 0);
    if (u_truth(u_mk_bool(1))) {
      e2075 = u_append(e2075, f_compile_46_constr_arities_1(v_c));
    }
    l2076 = u_field(l2076, 1);
  }
  u_value t2081 = f_list_46_concat_1(t2071);
  u_value t2082 = f_list_46_concat_1(t2074);
  u_value t2080;
  {
    u_value v_fs = t2081;
    u_value v_ks = t2082;
    u_value t2077 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2078 = &t2077;
    u_value l2079 = v_cs;
    while (u_is_con(l2079, &n_list_46_Cons)) {
      u_value v_c = u_field(l2079, 0);
      if (u_truth(u_mk_bool(1))) {
        e2078 = u_append(e2078, f_compile_46_proc_arities_1(v_c));
      }
      l2079 = u_field(l2079, 1);
    }
    t2080 = u_mk_con(&n_compile_46_Globals, 5, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()), f_map_46_put_all_2(f_list_46_concat_1(t2077), f_map_46_empty_0()));
  }
  return t2080;
}

u_value f_compile_46_proc_arities_1(u_value v_c) {
  u_value s2083 = v_c;
  u_value t2084 = NULL;
  if ((!t2084) && u_is_con(s2083, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s2083, 0);
    u_value v_ins = u_field(s2083, 2);
    u_value v_outs = u_field(s2083, 3);
    u_value v_refs = u_field(s2083, 4);
    t2084 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, f_list_46_length_1(v_ins), u_add(f_list_46_length_1(v_outs), f_list_46_length_1(v_refs)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2084) {
    t2084 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2084) {
    u_no_case(s2083);
  }
  return t2084;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s2085 = v_fs;
  u_value t2086 = NULL;
  if ((!t2086) && u_is_con(s2085, &n_list_46_Nil)) {
    t2086 = v_m;
  }
  if ((!t2086) && u_is_con(s2085, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s2085, 0), 0);
    u_value v_k = u_field(u_field(s2085, 0), 1);
    u_value v_rest = u_field(s2085, 1);
    u_value s2087 = f_map_46_get_2(v_f, v_m);
    u_value t2088 = NULL;
    if ((!t2088) && u_is_con(s2087, &n_option_46_Some)) {
      u_value v_ks = u_field(s2087, 0);
      t2088 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t2088) && u_is_con(s2087, &n_option_46_None)) {
      t2088 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t2088) {
      u_no_case(s2087);
    }
    t2086 = t2088;
  }
  if (!t2086) {
    u_no_case(s2085);
  }
  return t2086;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s2089 = v_gl;
  u_value t2090 = NULL;
  if ((!t2090) && u_is_con(s2089, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s2089, 2);
    u_value s2091 = f_map_46_get_2(v_n, v_fm);
    u_value t2092 = NULL;
    if ((!t2092) && u_is_con(s2091, &n_option_46_Some)) {
      u_value v_ks = u_field(s2091, 0);
      t2092 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t2092) && u_is_con(s2091, &n_option_46_None)) {
      t2092 = u_mk_bool(0);
    }
    if (!t2092) {
      u_no_case(s2091);
    }
    t2090 = t2092;
  }
  if (!t2090) {
    u_no_case(s2089);
  }
  return t2090;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s2093 = v_gl;
  u_value t2094 = NULL;
  if ((!t2094) && u_is_con(s2093, &n_compile_46_Globals)) {
    u_value v_km = u_field(s2093, 3);
    t2094 = f_map_46_get_2(v_n, v_km);
  }
  if (!t2094) {
    u_no_case(s2093);
  }
  return t2094;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s2095 = v_c;
  u_value t2096 = NULL;
  if ((!t2096) && u_is_con(s2095, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s2095, 0);
    u_value v_xss = u_field(s2095, 2);
    t2096 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2096) {
    t2096 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2096) {
    u_no_case(s2095);
  }
  return t2096;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s2097 = v_c;
  u_value t2098 = NULL;
  if ((!t2098) && u_is_con(s2097, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s2097, 2);
    u_value t2099 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2100 = &t2099;
    u_value l2101 = v_ks;
    while (u_is_con(l2101, &n_list_46_Cons)) {
      u_value v_k = u_field(l2101, 0);
      if (u_truth(u_mk_bool(1))) {
        e2100 = u_append(e2100, f_compile_46_constr_arity_1(v_k));
      }
      l2101 = u_field(l2101, 1);
    }
    t2098 = t2099;
  }
  if (!t2098) {
    t2098 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2098) {
    u_no_case(s2097);
  }
  return t2098;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s2102 = v_k;
  u_value t2103 = NULL;
  if ((!t2103) && u_is_con(s2102, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s2102, 0);
    u_value v_ts = u_field(s2102, 1);
    t2103 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t2103) {
    u_no_case(s2102);
  }
  return t2103;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s2104 = v_xs;
  u_value t2105 = NULL;
  if ((!t2105) && u_is_con(s2104, &n_list_46_Nil)) {
    t2105 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2105) && u_is_con(s2104, &n_list_46_Cons)) {
    u_value v_x = u_field(s2104, 0);
    u_value v_rest = u_field(s2104, 1);
    t2105 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t2105) {
    u_no_case(s2104);
  }
  return t2105;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_2106 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_2106, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_2107 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_2107, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s2108 = v_cs;
  u_value t2109 = NULL;
  if ((!t2109) && u_is_con(s2108, &n_list_46_Nil)) {
    t2109 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t2109) && (u_is_con(s2108, &n_list_46_Cons) && u_is_con(u_field(s2108, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s2108, 0);
    t2109 = v_d;
  }
  if ((!t2109) && u_is_con(s2108, &n_list_46_Cons)) {
    u_value v_d = u_field(s2108, 0);
    u_value v_rest = u_field(s2108, 1);
    static struct u_object l__38__38__2110 = U_LIT("&&");
    t2109 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__2110, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t2109) {
    u_no_case(s2108);
  }
  return t2109;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s2111 = v_l;
  u_value t2112 = NULL;
  if ((!t2112) && u_is_con(s2111, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_2113 = U_LIT("u_mk_tuple");
    t2112 = f_c_46_c_call_2(&l_u_mk_tuple_2113, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2112) && u_is_con(s2111, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s2111, 0);
    static struct u_object l_u_mk_bool_2114 = U_LIT("u_mk_bool");
    u_value t2115;
    if (u_truth(v_b)) {
      t2115 = u_mk_int(1);
    } else {
      t2115 = u_mk_int(0);
    }
    t2112 = f_c_46_c_call_2(&l_u_mk_bool_2114, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t2115), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2112) && u_is_con(s2111, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s2111, 0);
    u_value t2116 = u_lt(u_mk_int_str("-1000000000000000000"), v_n);
    if (u_truth(t2116)) {
      t2116 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t2119;
    if (u_truth(t2116)) {
      static struct u_object l_u_mk_int_2117 = U_LIT("u_mk_int");
      t2119 = f_c_46_c_call_2(&l_u_mk_int_2117, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_2118 = U_LIT("u_mk_int_str");
      t2119 = f_c_46_c_call_2(&l_u_mk_int_str_2118, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t2112 = t2119;
  }
  if ((!t2112) && u_is_con(s2111, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s2111, 0);
    static struct u_object l_u_mk_lit_2120 = U_LIT("u_mk_lit");
    t2112 = f_c_46_c_call_2(&l_u_mk_lit_2120, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2112) && u_is_con(s2111, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s2111, 0);
    static struct u_object l_u_mk_char_2121 = U_LIT("u_mk_char");
    t2112 = f_c_46_c_call_2(&l_u_mk_char_2121, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2112) {
    u_no_case(s2111);
  }
  return t2112;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s2122 = v_p;
  u_value t2123 = NULL;
  if ((!t2123) && u_is_con(s2122, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s2122, 0);
    static struct u_object l_u_equals_2124 = U_LIT("u_equals");
    t2123 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_2124, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2123) && u_is_con(s2122, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s2122, 0);
    u_value v_ps = u_field(s2122, 1);
    static struct u_object l_u_is_con_2125 = U_LIT("u_is_con");
    t2123 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_2125, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t2123) && u_is_con(s2122, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2122, 0);
    t2123 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2123) {
    t2123 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2123) {
    u_no_case(s2122);
  }
  return t2123;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s2126 = v_ps;
  u_value t2127 = NULL;
  if ((!t2127) && u_is_con(s2126, &n_list_46_Nil)) {
    t2127 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2127) && u_is_con(s2126, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2126, 0), 0);
    u_value v_i = u_field(u_field(s2126, 0), 1);
    u_value v_rest = u_field(s2126, 1);
    t2127 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t2127) {
    u_no_case(s2126);
  }
  return t2127;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s2128 = v_p;
  u_value t2129 = NULL;
  if ((!t2129) && (u_is_con(s2128, &n_syntax_46_PVar) && u_is_con(u_field(s2128, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2128, 0), 0);
    t2129 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2129) && u_is_con(s2128, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2128, 1);
    t2129 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t2129) && u_is_con(s2128, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2128, 0);
    t2129 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2129) {
    t2129 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2129) {
    u_no_case(s2128);
  }
  return t2129;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s2130 = v_ps;
  u_value t2131 = NULL;
  if ((!t2131) && u_is_con(s2130, &n_list_46_Nil)) {
    t2131 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2131) && u_is_con(s2130, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2130, 0), 0);
    u_value v_i = u_field(u_field(s2130, 0), 1);
    u_value v_rest = u_field(s2130, 1);
    t2131 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t2131) {
    u_no_case(s2130);
  }
  return t2131;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s2132 = v_p;
  u_value t2133 = NULL;
  if ((!t2133) && (u_is_con(s2132, &n_syntax_46_PVar) && u_is_con(u_field(s2132, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2132, 0), 0);
    t2133 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2133) && u_is_con(s2132, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2132, 1);
    u_value t2134 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2135 = &t2134;
    u_value l2136 = v_ps;
    while (u_is_con(l2136, &n_list_46_Cons)) {
      u_value v_q = u_field(l2136, 0);
      if (u_truth(u_mk_bool(1))) {
        e2135 = u_append(e2135, f_compile_46_pattern_names_1(v_q));
      }
      l2136 = u_field(l2136, 1);
    }
    t2133 = f_list_46_concat_1(t2134);
  }
  if ((!t2133) && u_is_con(s2132, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2132, 0);
    u_value t2137 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2138 = &t2137;
    u_value l2139 = v_ps;
    while (u_is_con(l2139, &n_list_46_Cons)) {
      u_value v_q = u_field(l2139, 0);
      if (u_truth(u_mk_bool(1))) {
        e2138 = u_append(e2138, f_compile_46_pattern_names_1(v_q));
      }
      l2139 = u_field(l2139, 1);
    }
    t2133 = f_list_46_concat_1(t2137);
  }
  if (!t2133) {
    t2133 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2133) {
    u_no_case(s2132);
  }
  return t2133;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s2140 = v_e;
  u_value t2141 = NULL;
  if ((!t2141) && u_is_con(s2140, &n_syntax_46_ELit)) {
    t2141 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2141) && (u_is_con(s2140, &n_syntax_46_EVar) && u_is_con(u_field(s2140, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2140, 0), 0);
    u_value t2142;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t2142 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t2142 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t2141 = t2142;
  }
  if ((!t2141) && u_is_con(s2140, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s2140, 0);
    u_value v_args = u_field(s2140, 1);
    u_value t2143 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2144 = &t2143;
    u_value l2145 = v_args;
    while (u_is_con(l2145, &n_list_46_Cons)) {
      u_value v_a = u_field(l2145, 0);
      if (u_truth(u_mk_bool(1))) {
        e2144 = u_append(e2144, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2145 = u_field(l2145, 1);
    }
    t2141 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t2143));
  }
  if ((!t2141) && u_is_con(s2140, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s2140, 0);
    u_value v_b = u_field(s2140, 1);
    t2141 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t2141) && u_is_con(s2140, &n_syntax_46_EProc)) {
    u_value v_is = u_field(s2140, 0);
    u_value v_os = u_field(s2140, 1);
    u_value v_rs = u_field(s2140, 2);
    u_value v_b = u_field(s2140, 3);
    t2141 = f_compile_46_free_in_stmt_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), v_bound));
  }
  if ((!t2141) && u_is_con(s2140, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s2140, 0);
    u_value v_b = u_field(s2140, 1);
    u_value t2146 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2147 = &t2146;
    u_value l2148 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l2148, &n_list_46_Cons)) {
      u_value v_a = u_field(l2148, 0);
      if (u_truth(u_mk_bool(1))) {
        e2147 = u_append(e2147, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2148 = u_field(l2148, 1);
    }
    t2141 = f_list_46__43__43__2(f_list_46_concat_1(t2146), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t2141) && u_is_con(s2140, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s2140, 0);
    u_value v_l = u_field(s2140, 1);
    u_value v_r = u_field(s2140, 2);
    t2141 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t2141) && u_is_con(s2140, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s2140, 0);
    u_value v_cs = u_field(s2140, 1);
    u_value t2149 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2150 = &t2149;
    u_value l2151 = v_cs;
    while (u_is_con(l2151, &n_list_46_Cons)) {
      u_value v_b = u_field(l2151, 0);
      if (u_truth(u_mk_bool(1))) {
        e2150 = u_append(e2150, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l2151 = u_field(l2151, 1);
    }
    t2141 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2149));
  }
  if ((!t2141) && u_is_con(s2140, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s2140, 0);
    u_value v_xs = u_field(s2140, 1);
    u_value v_ls = u_field(s2140, 2);
    u_value v_c = u_field(s2140, 3);
    u_value t2152 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2153 = &t2152;
    u_value l2154 = v_ls;
    while (u_is_con(l2154, &n_list_46_Cons)) {
      u_value v_l = u_field(l2154, 0);
      if (u_truth(u_mk_bool(1))) {
        e2153 = u_append(e2153, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2154 = u_field(l2154, 1);
    }
    t2141 = f_list_46__43__43__2(f_list_46_concat_1(t2152), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t2141) && u_is_con(s2140, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s2140, 0);
    u_value t2155 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2156 = &t2155;
    u_value l2157 = v_es;
    while (u_is_con(l2157, &n_list_46_Cons)) {
      u_value v_a = u_field(l2157, 0);
      if (u_truth(u_mk_bool(1))) {
        e2156 = u_append(e2156, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2157 = u_field(l2157, 1);
    }
    t2141 = f_list_46_concat_1(t2155);
  }
  if ((!t2141) && u_is_con(s2140, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s2140, 0);
    t2141 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if (!t2141) {
    u_no_case(s2140);
  }
  return t2141;
}

u_value f_compile_46_free_in_stmt_2(u_value v_x, u_value v_bound) {
  u_value s2158 = v_x;
  u_value t2159 = NULL;
  if ((!t2159) && u_is_con(s2158, &n_syntax_46_SSkip)) {
    t2159 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2159) && u_is_con(s2158, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s2158, 0);
    u_value t2160 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2161 = &t2160;
    u_value l2162 = f_syntax_46_names_1(f_list_46_firsts_1(v_ps));
    while (u_is_con(l2162, &n_list_46_Cons)) {
      u_value v_y = u_field(l2162, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2161 = u_append(e2161, v_y);
      }
      l2162 = u_field(l2162, 1);
    }
    u_value t2163 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2164 = &t2163;
    u_value l2165 = f_list_46_seconds_1(v_ps);
    while (u_is_con(l2165, &n_list_46_Cons)) {
      u_value v_a = u_field(l2165, 0);
      if (u_truth(u_mk_bool(1))) {
        e2164 = u_append(e2164, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2165 = u_field(l2165, 1);
    }
    t2159 = f_list_46__43__43__2(t2160, f_list_46_concat_1(t2163));
  }
  if ((!t2159) && u_is_con(s2158, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s2158, 0);
    u_value v_ys = u_field(s2158, 1);
    u_value t2166 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2167 = &t2166;
    u_value l2168 = v_ys;
    while (u_is_con(l2168, &n_list_46_Cons)) {
      u_value v_y = u_field(l2168, 0);
      if (u_truth(u_mk_bool(1))) {
        e2167 = u_append(e2167, f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2168 = u_field(l2168, 1);
    }
    t2159 = f_list_46_concat_1(t2166);
  }
  if ((!t2159) && u_is_con(s2158, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s2158, 0);
    u_value v_l = u_field(s2158, 1);
    u_value v_r = u_field(s2158, 2);
    t2159 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_l, v_bound), f_compile_46_free_in_stmt_2(v_r, v_bound)));
  }
  if ((!t2159) && u_is_con(s2158, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s2158, 0);
    u_value v_b = u_field(s2158, 1);
    t2159 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_compile_46_free_in_stmt_2(v_b, v_bound));
  }
  if ((!t2159) && u_is_con(s2158, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s2158, 0);
    u_value v_ins = u_field(s2158, 1);
    u_value v_outs = u_field(s2158, 2);
    u_value v_refs = u_field(s2158, 3);
    u_value t2169 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2170 = &t2169;
    u_value l2171 = u_mk_con(&n_list_46_Cons, 2, v_p, f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
    while (u_is_con(l2171, &n_list_46_Cons)) {
      u_value v_y = u_field(l2171, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2170 = u_append(e2170, v_y);
      }
      l2171 = u_field(l2171, 1);
    }
    u_value t2172 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2173 = &t2172;
    u_value l2174 = v_ins;
    while (u_is_con(l2174, &n_list_46_Cons)) {
      u_value v_a = u_field(l2174, 0);
      if (u_truth(u_mk_bool(1))) {
        e2173 = u_append(e2173, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2174 = u_field(l2174, 1);
    }
    t2159 = f_list_46__43__43__2(t2169, f_list_46_concat_1(t2172));
  }
  if ((!t2159) && u_is_con(s2158, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s2158, 0);
    u_value v_bs = u_field(s2158, 1);
    u_value t2175 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2176 = &t2175;
    u_value l2177 = v_bs;
    while (u_is_con(l2177, &n_list_46_Cons)) {
      u_value v_b = u_field(l2177, 0);
      if (u_truth(u_mk_bool(1))) {
        e2176 = u_append(e2176, f_compile_46_free_in_stmt_branch_2(v_b, v_bound));
      }
      l2177 = u_field(l2177, 1);
    }
    t2159 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2175));
  }
  if ((!t2159) && u_is_con(s2158, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s2158, 0);
    t2159 = f_compile_46_free_vars_2(v_c, v_bound);
  }
  if ((!t2159) && u_is_con(s2158, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s2158, 0);
    t2159 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if ((!t2159) && u_is_con(s2158, &n_syntax_46_STry)) {
    u_value v_b = u_field(s2158, 0);
    u_value v_hs = u_field(s2158, 1);
    u_value t2178 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2179 = &t2178;
    u_value l2180 = v_hs;
    while (u_is_con(l2180, &n_list_46_Cons)) {
      u_value v_h = u_field(l2180, 0);
      if (u_truth(u_mk_bool(1))) {
        e2179 = u_append(e2179, f_compile_46_free_in_handler_2(v_h, v_bound));
      }
      l2180 = u_field(l2180, 1);
    }
    t2159 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_b, v_bound), f_list_46_concat_1(t2178));
  }
  if (!t2159) {
    u_no_case(s2158);
  }
  return t2159;
}

u_value f_compile_46_free_in_stmt_branch_2(u_value v_b, u_value v_bound) {
  u_value s2181 = v_b;
  u_value t2182 = NULL;
  if ((!t2182) && (u_is_con(s2181, &n_syntax_46_Branch) && u_is_con(u_field(s2181, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2181, 0);
    u_value v_g = u_field(u_field(s2181, 1), 0);
    u_value v_y = u_field(s2181, 2);
    t2182 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2182) && (u_is_con(s2181, &n_syntax_46_Branch) && u_is_con(u_field(s2181, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2181, 0);
    u_value v_y = u_field(s2181, 2);
    t2182 = f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2182) {
    u_no_case(s2181);
  }
  return t2182;
}

u_value f_compile_46_free_in_handler_2(u_value v_h, u_value v_bound) {
  u_value t2186 = f_list_46__43__43__2(f_compile_46_pattern_names_1(f_syntax_46_Handler_46_pat_1(v_h)), v_bound);
  u_value t2185;
  {
    u_value v_inner = t2186;
    u_value s2183 = f_syntax_46_Handler_46_guard_1(v_h);
    u_value t2184 = NULL;
    if ((!t2184) && u_is_con(s2183, &n_option_46_Some)) {
      u_value v_g = u_field(s2183, 0);
      t2184 = f_compile_46_free_vars_2(v_g, v_inner);
    }
    if ((!t2184) && u_is_con(s2183, &n_option_46_None)) {
      t2184 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t2184) {
      u_no_case(s2183);
    }
    t2185 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(f_syntax_46_Handler_46_body_1(v_h), v_inner), t2184);
  }
  return t2185;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s2187 = v_b;
  u_value t2188 = NULL;
  if ((!t2188) && (u_is_con(s2187, &n_syntax_46_Branch) && u_is_con(u_field(s2187, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2187, 0);
    u_value v_g = u_field(u_field(s2187, 1), 0);
    u_value v_e = u_field(s2187, 2);
    t2188 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2188) && (u_is_con(s2187, &n_syntax_46_Branch) && u_is_con(u_field(s2187, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2187, 0);
    u_value v_e = u_field(s2187, 2);
    t2188 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2188) {
    u_no_case(s2187);
  }
  return t2188;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s2189 = v_xss;
  u_value t2190 = NULL;
  if ((!t2190) && u_is_con(s2189, &n_list_46_Nil)) {
    t2190 = v_b;
  }
  if ((!t2190) && u_is_con(s2189, &n_list_46_Cons)) {
    u_value v_xs = u_field(s2189, 0);
    u_value v_rest = u_field(s2189, 1);
    t2190 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t2190) {
    u_no_case(s2189);
  }
  return t2190;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value t2191 = u_concat(v_stem, u_str_show(f_compile_46_Gen_46_count_1(*r_g)));
    u_value t2192 = f_compile_46_Gen_46_count_58__61__2(*r_g, u_add(f_compile_46_Gen_46_count_1(*r_g), u_mk_int(1)));
    *r_x = t2191;
    *r_g = t2192;
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value t2193 = f_compile_46_Gen_46_lifted_58__61__2(*r_g, f_list_46__43__43__2(f_compile_46_Gen_46_lifted_1(*r_g), u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
    *r_g = t2193;
  }
}

u_value f_compile_46_global_value_3(u_value v_gl, u_value v_n, u_value v_t) {
  u_value t2208;
  if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
    static struct u_object l_u_mk_closure_2194 = U_LIT("u_mk_closure");
    static struct u_object l_w__2195 = U_LIT("w_");
    t2208 = f_c_46_c_call_2(&l_u_mk_closure_2194, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__2195, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
  } else {
    u_value s2196 = f_compile_46_fields_count_2(v_gl, v_n);
    u_value t2197 = NULL;
    if ((!t2197) && (u_is_con(s2196, &n_option_46_Some) && u_equals(u_field(s2196, 0), u_mk_int(0)))) {
      static struct u_object l_u_mk_con_2198 = U_LIT("u_mk_con");
      t2197 = f_c_46_c_call_2(&l_u_mk_con_2198, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
    }
    if ((!t2197) && u_is_con(s2196, &n_option_46_Some)) {
      static struct u_object l_u_mk_closure_2199 = U_LIT("u_mk_closure");
      static struct u_object l_wc__2200 = U_LIT("wc_");
      t2197 = f_c_46_c_call_2(&l_u_mk_closure_2199, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__2200, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
    }
    if ((!t2197) && u_is_con(s2196, &n_option_46_None)) {
      u_value s2201 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
      u_value t2202 = NULL;
      if ((!t2202) && u_is_con(s2201, &n_option_46_Some)) {
        u_value v_f = u_field(u_field(s2201, 0), 0);
        static struct u_object l_u_mk_closure_2203 = U_LIT("u_mk_closure");
        static struct u_object l_w_2204 = U_LIT("w");
        t2202 = f_c_46_c_call_2(&l_u_mk_closure_2203, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_2204, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
      }
      if ((!t2202) && u_is_con(s2201, &n_option_46_None)) {
        if (u_truth(f_map_46_has_2(v_n, f_compile_46_Globals_46_procs_1(v_gl)))) {
          static struct u_object l_u_mk_proc_2205 = U_LIT("u_mk_proc");
          static struct u_object l_wp__2206 = U_LIT("wp_");
          t2202 = f_c_46_c_call_2(&l_u_mk_proc_2205, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wp__2206, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        }
      }
      if ((!t2202) && u_is_con(s2201, &n_option_46_None)) {
        static struct u_object l_the_32_variable_32__2207 = U_LIT("the variable ");
        u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__2207, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
        t2202 = NULL;
      }
      if (!t2202) {
        u_no_case(s2201);
      }
      t2197 = t2202;
    }
    if (!t2197) {
      u_no_case(s2196);
    }
    t2208 = t2197;
  }
  return t2208;
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
    u_value s2209 = v_e;
    int d2210 = 0;
    if ((!d2210) && (u_is_con(s2209, &n_syntax_46_ELit) && u_is_con(u_field(s2209, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s2209, 0), 0);
      d2210 = 1;
      {
        static struct u_object l_l__2211 = U_LIT("l_");
        u_value t2212;
        if (u_truth(u_lt(u_str_size(v_s), u_mk_int(16)))) {
          t2212 = u_str_size(v_s);
        } else {
          t2212 = u_mk_int(16);
        }
        static struct u_object l___2213 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__2211, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), t2212))), &l___2213)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__2214 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_2215 = U_LIT("U_LIT");
        static struct u_object l__38__2216 = U_LIT("&");
        u_value t2217 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__2214), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_2215, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t2218 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2216, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t2217;
        *r_r = t2218;
      }
    }
    if ((!d2210) && u_is_con(s2209, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s2209, 0);
      d2210 = 1;
      {
        u_value t2219 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2220 = f_compile_46_literal_value_1(v_l);
        *r_ss = t2219;
        *r_r = t2220;
      }
    }
    if ((!d2210) && (u_is_con(s2209, &n_syntax_46_EVar) && u_is_con(u_field(s2209, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s2209, 0), 0);
      u_value v_ty = u_field(u_field(s2209, 0), 1);
      d2210 = 1;
      {
        u_value s2221 = f_list_46_lookup_2(v_n, v_env);
        int d2222 = 0;
        if ((!d2222) && u_is_con(s2221, &n_option_46_Some)) {
          u_value v_c = u_field(s2221, 0);
          d2222 = 1;
          {
            u_value t2223 = u_mk_con(&n_list_46_Nil, 0);
            u_value t2224 = v_c;
            *r_ss = t2223;
            *r_r = t2224;
          }
        }
        if ((!d2222) && u_is_con(s2221, &n_option_46_None)) {
          d2222 = 1;
          {
            u_value t2225 = u_mk_con(&n_list_46_Nil, 0);
            u_value t2226 = f_compile_46_global_value_3(v_gl, v_n, v_ty);
            *r_ss = t2225;
            *r_r = t2226;
          }
        }
        if (!d2222) {
          u_no_case(s2221);
        }
      }
    }
    if ((!d2210) && (u_is_con(s2209, &n_syntax_46_EApp) && (u_is_con(u_field(s2209, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s2209, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s2209, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s2209, 0), 0), 1);
      u_value v_args = u_field(s2209, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d2210 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d2210) && u_is_con(s2209, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s2209, 0);
      u_value v_args = u_field(s2209, 1);
      d2210 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_2227 = U_LIT("u_apply");
        u_value t2228 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2229 = f_c_46_c_call_2(&l_u_apply_2227, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t2228;
        *r_r = t2229;
      }
    }
    if ((!d2210) && u_is_con(s2209, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s2209, 0);
      u_value v_b = u_field(s2209, 1);
      d2210 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2210) && u_is_con(s2209, &n_syntax_46_EProc)) {
      u_value v_is = u_field(s2209, 0);
      u_value v_os = u_field(s2209, 1);
      u_value v_rs = u_field(s2209, 2);
      u_value v_b = u_field(s2209, 3);
      d2210 = 1;
      {
        p_compile_46_compile_proc_lambda(v_gl, v_env, v_is, f_list_46__43__43__2(v_os, v_rs), v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2210) && u_is_con(s2209, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s2209, 0);
      u_value v_b = u_field(s2209, 1);
      d2210 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2230 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2230, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2231 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t2231;
        u_value t2232 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2232;
      }
    }
    if ((!d2210) && u_is_con(s2209, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s2209, 0);
      u_value v_l = u_field(s2209, 1);
      u_value v_rr = u_field(s2209, 2);
      d2210 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2233 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2233, &v_t, r_g);
        u_value t2234 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2234;
        u_value t2235 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2235;
      }
    }
    if ((!d2210) && u_is_con(s2209, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s2209, 0);
      u_value v_cases = u_field(s2209, 1);
      d2210 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2236 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2236, &v_d, r_g);
        static struct u_object l_t_2237 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2237, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_2238 = U_LIT("NULL");
        u_value t2239 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2238))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2239;
        static struct u_object l__33__2240 = U_LIT("!");
        static struct u_object l_u_no_case_2241 = U_LIT("u_no_case");
        u_value t2242 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2240, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2241, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2242;
        u_value t2243 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2243;
      }
    }
    if ((!d2210) && u_is_con(s2209, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s2209, 0);
      u_value v_xs = u_field(s2209, 1);
      u_value v_lists = u_field(s2209, 2);
      u_value v_c = u_field(s2209, 3);
      d2210 = 1;
      {
        static struct u_object l_t_2244 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2244, &v_t, r_g);
        static struct u_object l_e_2245 = U_LIT("e");
        p_compile_46_gen_name(&l_e_2245, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_2246 = U_LIT("u_mk_con");
        static struct u_object l__38__2247 = U_LIT("&");
        u_value t2248 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_2246, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__2247, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t2248;
        u_value t2249 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2249;
      }
    }
    if ((!d2210) && u_is_con(s2209, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s2209, 0);
      d2210 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_2250 = U_LIT("u_mk_tuple");
        u_value t2251 = f_c_46_c_call_2(&l_u_mk_tuple_2250, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t2251;
      }
    }
    if ((!d2210) && u_is_con(s2209, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s2209, 0);
      u_value v_ty = u_field(s2209, 1);
      d2210 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2252 = U_LIT("u_raise");
        static struct u_object l_NULL_2253 = U_LIT("NULL");
        u_value t2254 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2252, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        u_value t2255 = u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2253);
        *r_ss = t2254;
        *r_r = t2255;
      }
    }
    if (!d2210) {
      u_no_case(s2209);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s2256 = v_es;
    int d2257 = 0;
    if ((!d2257) && u_is_con(s2256, &n_list_46_Nil)) {
      d2257 = 1;
      {
        u_value t2258 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2259 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2258;
        *r_rs = t2259;
      }
    }
    if ((!d2257) && u_is_con(s2256, &n_list_46_Cons)) {
      u_value v_e = u_field(s2256, 0);
      u_value v_rest = u_field(s2256, 1);
      d2257 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t2260 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2261 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t2260;
        *r_rs = t2261;
      }
    }
    if (!d2257) {
      u_no_case(s2256);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s2262 = v_rs;
    int d2263 = 0;
    if ((!d2263) && u_is_con(s2262, &n_list_46_Nil)) {
      d2263 = 1;
      {
        u_value t2264 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2264;
      }
    }
    if ((!d2263) && u_is_con(s2262, &n_list_46_Cons)) {
      u_value v_r = u_field(s2262, 0);
      u_value v_rest = u_field(s2262, 1);
      d2263 = 1;
      {
        static struct u_object l_t_2265 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2265, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t2266 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t2266;
      }
    }
    if (!d2263) {
      u_no_case(s2262);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s2267 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2268 = NULL;
  if ((!t2268) && (u_is_con(u_field(s2267, 0), &n_list_46_Cons) && (u_is_con(u_field(s2267, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2267, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2267, 0), 0);
    u_value v_more = u_field(u_field(s2267, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2267, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2267, 1), 1);
    t2268 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t2268) {
    t2268 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2268) {
    u_no_case(s2267);
  }
  return t2268;
}

void p_compile_46_compile_named(u_value v_gl, u_value v_env, u_value v_n, u_value v_ty, u_value v_args, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value v_ra = NULL;
    u_value v_rb = NULL;
    u_value v_rs = NULL;
    u_value v_t = NULL;
    u_value s2269 = v_args;
    int d2270 = 0;
    if ((!d2270) && (u_is_con(s2269, &n_list_46_Cons) && (u_is_con(u_field(s2269, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2269, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s2269, 0);
      u_value v_b = u_field(u_field(s2269, 1), 0);
      static struct u_object l_and_2277 = U_LIT("and");
      u_value t2279 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2277));
      if (!u_truth(t2279)) {
        static struct u_object l_or_2278 = U_LIT("or");
        t2279 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_2278));
      }
      if (u_truth(t2279)) {
        d2270 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_2271 = U_LIT("t");
          p_compile_46_gen_name(&l_t_2271, &v_t, r_g);
          static struct u_object l_and_2272 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2272)))) {
            {
              u_value t2273 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2273;
            }
          } else {
            {
              static struct u_object l__33__2274 = U_LIT("!");
              u_value t2275 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2274, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2275;
            }
          }
          u_value t2276 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t2276;
        }
      }
    }
    if (!d2270) {
      d2270 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t2280 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t2280)) {
          t2280 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t2280)) {
          {
            u_value t2281 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t2281;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_2282 = U_LIT("u_mk_con");
              u_value t2283 = f_c_46_c_call_2(&l_u_mk_con_2282, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t2283;
            }
          } else {
            {
              u_value s2284 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
              int d2285 = 0;
              if ((!d2285) && u_is_con(s2284, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s2284, 0), 0);
                u_value v_k = u_field(u_field(s2284, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d2285 = 1;
                  {
                    u_value t2286 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t2286;
                  }
                }
              }
              if (!d2285) {
                d2285 = 1;
                {
                  static struct u_object l_u_apply_2287 = U_LIT("u_apply");
                  u_value t2288 = f_c_46_c_call_2(&l_u_apply_2287, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_global_value_3(v_gl, v_n, v_ty), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t2288;
                }
              }
              if (!d2285) {
                u_no_case(s2284);
              }
            }
          }
        }
      }
    }
    if (!d2270) {
      u_no_case(s2269);
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
    u_value t2289 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2290 = &t2289;
    u_value l2291 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l2291, &n_list_46_Cons)) {
      u_value v_x = u_field(l2291, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2290 = u_append(e2290, v_x);
      }
      l2291 = u_field(l2291, 1);
    }
    u_value t2292 = t2289;
    v_caps = t2292;
    static struct u_object l_lam_2293 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_2293, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_2294 = U_LIT("env");
    static struct u_object l_args_2295 = U_LIT("args");
    u_value t2296 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2294), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_2295));
    v_decls = t2296;
    static struct u_object l_env_2297 = U_LIT("env");
    static struct u_object l_args_2298 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2297), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2298), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_2299 = U_LIT("u_mk_closure");
    u_value t2300 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2301 = &t2300;
    u_value l2302 = v_caps;
    while (u_is_con(l2302, &n_list_46_Cons)) {
      u_value v_x = u_field(l2302, 0);
      if (u_truth(u_mk_bool(1))) {
        e2301 = u_append(e2301, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2302 = u_field(l2302, 1);
    }
    u_value t2303 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2304 = f_c_46_c_call_2(&l_u_mk_closure_2299, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2300));
    *r_ss = t2303;
    *r_r = t2304;
  }
}

void p_compile_46_compile_proc_lambda(u_value v_gl, u_value v_env, u_value v_ins, u_value v_outs, u_value v_b, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_caps = NULL;
    u_value v_f = NULL;
    u_value v_bs = NULL;
    u_value v_decls = NULL;
    u_value t2305 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2306 = &t2305;
    u_value l2307 = f_list_46_distinct_1(f_compile_46_free_in_stmt_2(v_b, f_syntax_46_names_1(f_list_46__43__43__2(v_ins, v_outs))));
    while (u_is_con(l2307, &n_list_46_Cons)) {
      u_value v_x = u_field(l2307, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2306 = u_append(e2306, v_x);
      }
      l2307 = u_field(l2307, 1);
    }
    u_value t2308 = t2305;
    v_caps = t2308;
    static struct u_object l_proc_2309 = U_LIT("proc");
    p_compile_46_gen_name(&l_proc_2309, &v_f, r_g);
    u_value t2310 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2311 = &t2310;
    u_value l2312 = f_syntax_46_names_1(v_outs);
    while (u_is_con(l2312, &n_list_46_Cons)) {
      u_value v_x = u_field(l2312, 0);
      if (u_truth(u_mk_bool(1))) {
        e2311 = u_append(e2311, f_compile_46_pointer_1(v_x));
      }
      l2312 = u_field(l2312, 1);
    }
    p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_ins))), t2310), v_b, &v_bs, r_g);
    static struct u_object l_env_2313 = U_LIT("env");
    static struct u_object l_ins_2314 = U_LIT("ins");
    u_value t2315 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2313), f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_ins), u_mk_int(0)), &l_ins_2314), f_compile_46_unpack_pointers_1(f_compile_46_numbered_2(f_syntax_46_names_1(v_outs), u_mk_int(0)))));
    v_decls = t2315;
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), v_f, f_compile_46_proc_params_0(), f_list_46__43__43__2(v_decls, v_bs)), r_g);
    static struct u_object l_u_mk_proc_2316 = U_LIT("u_mk_proc");
    u_value t2317 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2318 = &t2317;
    u_value l2319 = v_caps;
    while (u_is_con(l2319, &n_list_46_Cons)) {
      u_value v_x = u_field(l2319, 0);
      if (u_truth(u_mk_bool(1))) {
        e2318 = u_append(e2318, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2319 = u_field(l2319, 1);
    }
    u_value t2320 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2321 = f_c_46_c_call_2(&l_u_mk_proc_2316, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2317));
    *r_ss = t2320;
    *r_r = t2321;
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
  static struct u_object l_env_2322 = U_LIT("env");
  static struct u_object l_ins_2323 = U_LIT("ins");
  static struct u_object l_outs_2324 = U_LIT("outs");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2322), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_ins_2323), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, f_compile_46_value_ptr_0()), &l_outs_2324), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_compile_46_unpack_pointers_1(u_value v_xs) {
  u_value s2325 = v_xs;
  u_value t2326 = NULL;
  if ((!t2326) && u_is_con(s2325, &n_list_46_Nil)) {
    t2326 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2326) && u_is_con(s2325, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2325, 0), 0);
    u_value v_i = u_field(u_field(s2325, 0), 1);
    u_value v_rest = u_field(s2325, 1);
    static struct u_object l_outs_2327 = U_LIT("outs");
    t2326 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(&l_outs_2327, v_i))), f_compile_46_unpack_pointers_1(v_rest));
  }
  if (!t2326) {
    u_no_case(s2325);
  }
  return t2326;
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s2328 = v_xs;
  u_value t2329 = NULL;
  if ((!t2329) && u_is_con(s2328, &n_list_46_Nil)) {
    t2329 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2329) && u_is_con(s2328, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2328, 0), 0);
    u_value v_i = u_field(u_field(s2328, 0), 1);
    u_value v_rest = u_field(s2328, 1);
    t2329 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t2329) {
    u_no_case(s2328);
  }
  return t2329;
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
    u_value s2330 = v_cases;
    int d2331 = 0;
    if ((!d2331) && u_is_con(s2330, &n_list_46_Nil)) {
      d2331 = 1;
      {
        u_value t2332 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2332;
      }
    }
    if ((!d2331) && (u_is_con(s2330, &n_list_46_Cons) && u_is_con(u_field(s2330, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2330, 0), 0);
      u_value v_guard = u_field(u_field(s2330, 0), 1);
      u_value v_e = u_field(u_field(s2330, 0), 2);
      u_value v_more = u_field(s2330, 1);
      d2331 = 1;
      {
        u_value t2333 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2333;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2334 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t2334;
        u_value s2335 = v_guard;
        int d2336 = 0;
        if ((!d2336) && u_is_con(s2335, &n_option_46_Some)) {
          u_value v_c = u_field(s2335, 0);
          d2336 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2337 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2337;
          }
        }
        if ((!d2336) && u_is_con(s2335, &n_option_46_None)) {
          d2336 = 1;
          {
          }
        }
        if (!d2336) {
          u_no_case(s2335);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2338 = U_LIT("!");
        u_value t2339 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2338, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2339;
      }
    }
    if (!d2331) {
      u_no_case(s2330);
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
    u_value s2340 = u_mk_tuple(2, v_xs, v_lists);
    int d2341 = 0;
    if ((!d2341) && (u_is_con(u_field(s2340, 0), &n_list_46_Cons) && u_is_con(u_field(s2340, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2340, 0), 0);
      u_value v_more = u_field(u_field(s2340, 0), 1);
      u_value v_list = u_field(u_field(s2340, 1), 0);
      u_value v_others = u_field(u_field(s2340, 1), 1);
      d2341 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2342 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2342, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2343 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2343;
        static struct u_object l_u_is_con_2344 = U_LIT("u_is_con");
        u_value t2345 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2344, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2345;
      }
    }
    if (!d2341) {
      d2341 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2346 = U_LIT("u_append");
        u_value t2347 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2346, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2347;
      }
    }
    if (!d2341) {
      u_no_case(s2340);
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
    u_value s2348 = v_s;
    int d2349 = 0;
    if ((!d2349) && u_is_con(s2348, &n_syntax_46_SSkip)) {
      d2349 = 1;
      {
        u_value t2350 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2350;
      }
    }
    if ((!d2349) && u_is_con(s2348, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2348, 0);
      d2349 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2351 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2351;
      }
    }
    if ((!d2349) && u_is_con(s2348, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2348, 0);
      u_value v_body = u_field(s2348, 1);
      d2349 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t2352 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2353 = &t2352;
        u_value l2354 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2354, &n_list_46_Cons)) {
          u_value v_x = u_field(l2354, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2355 = U_LIT("NULL");
            e2353 = u_append(e2353, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2355))));
          }
          l2354 = u_field(l2354, 1);
        }
        u_value t2356 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2352, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2356;
      }
    }
    if ((!d2349) && u_is_con(s2348, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2348, 0);
      u_value v_l = u_field(s2348, 1);
      u_value v_r = u_field(s2348, 2);
      d2349 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2357 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2357;
      }
    }
    if ((!d2349) && u_is_con(s2348, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2348, 0);
      u_value v_b = u_field(s2348, 1);
      d2349 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2358 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2358, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2359 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2359;
      }
    }
    if ((!d2349) && u_is_con(s2348, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2348, 0);
      u_value v_ins = u_field(s2348, 1);
      u_value v_outs = u_field(s2348, 2);
      u_value v_refs = u_field(s2348, 3);
      d2349 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        if (u_truth(f_list_46_has_key_2(v_p, v_env))) {
          {
            static struct u_object l_u_call_proc_2360 = U_LIT("u_call_proc");
            u_value t2361 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e2362 = &t2361;
            u_value l2363 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
            while (u_is_con(l2363, &n_list_46_Cons)) {
              u_value v_x = u_field(l2363, 0);
              if (u_truth(u_mk_bool(1))) {
                e2362 = u_append(e2362, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
              }
              l2363 = u_field(l2363, 1);
            }
            u_value t2364 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_call_proc_2360, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_var_expr_2(v_env, v_p), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_ins)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(f_list_46__43__43__2(v_outs, v_refs))), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(v_rs, t2361)))), u_mk_con(&n_list_46_Nil, 0)));
            *r_ss = t2364;
          }
        } else {
          {
            u_value t2365 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e2366 = &t2365;
            u_value l2367 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
            while (u_is_con(l2367, &n_list_46_Cons)) {
              u_value v_x = u_field(l2367, 0);
              if (u_truth(u_mk_bool(1))) {
                e2366 = u_append(e2366, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
              }
              l2367 = u_field(l2367, 1);
            }
            u_value t2368 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2365))), u_mk_con(&n_list_46_Nil, 0)));
            *r_ss = t2368;
          }
        }
      }
    }
    if ((!d2349) && u_is_con(s2348, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2348, 0);
      u_value v_cases = u_field(s2348, 1);
      d2349 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2369 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2369, &v_d, r_g);
        static struct u_object l_d_2370 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2370, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2371 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2371;
        static struct u_object l__33__2372 = U_LIT("!");
        static struct u_object l_u_no_case_2373 = U_LIT("u_no_case");
        u_value t2374 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2372, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2373, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2374;
      }
    }
    if ((!d2349) && (u_is_con(s2348, &n_syntax_46_SAssert) && (u_is_con(u_field(s2348, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2348, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2348, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2348, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2348, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2348, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2348, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2348, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2348, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2348, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2348, 0), 1), 1), 0);
      d2349 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2375 = U_LIT("u_assert_equal");
        u_value t2376 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2375, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2376;
      }
    }
    if ((!d2349) && u_is_con(s2348, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2348, 0);
      d2349 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2377 = U_LIT("u_assert_true");
        u_value t2378 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2377, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2378;
      }
    }
    if ((!d2349) && u_is_con(s2348, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2348, 0);
      u_value v_ty = u_field(s2348, 1);
      d2349 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2379 = U_LIT("u_raise");
        u_value t2380 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2379, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2380;
      }
    }
    if ((!d2349) && u_is_con(s2348, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2348, 0);
      u_value v_hs = u_field(s2348, 1);
      d2349 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2381 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2381, &v_d, r_g);
        static struct u_object l_d_2382 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2382, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2383 = U_LIT("u_pop");
        static struct u_object l__33__2384 = U_LIT("!");
        static struct u_object l_u_reraise_2385 = U_LIT("u_reraise");
        u_value t2386 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2383, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2384, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2385, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2386;
        static struct u_object l_struct_32_u_handler_2387 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2388 = U_LIT("u_push");
        static struct u_object l__61__61__2389 = U_LIT("==");
        static struct u_object l_setjmp_2390 = U_LIT("setjmp");
        static struct u_object l_env_2391 = U_LIT("env");
        static struct u_object l_u_pop_2392 = U_LIT("u_pop");
        u_value t2393 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2387)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2388, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2389, f_c_46_c_call_2(&l_setjmp_2390, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2391), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2392, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2393;
      }
    }
    if (!d2349) {
      u_no_case(s2348);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2394 = v_xs;
    int d2395 = 0;
    if ((!d2395) && u_is_con(s2394, &n_list_46_Nil)) {
      d2395 = 1;
      {
        u_value t2396 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2396;
      }
    }
    if ((!d2395) && u_is_con(s2394, &n_list_46_Cons)) {
      u_value v_x = u_field(s2394, 0);
      u_value v_rest = u_field(s2394, 1);
      d2395 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2397 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2397;
      }
    }
    if (!d2395) {
      u_no_case(s2394);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2398 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2399 = NULL;
  if ((!t2399) && (u_is_con(u_field(s2398, 0), &n_list_46_Cons) && (u_is_con(u_field(s2398, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2398, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2398, 0), 0);
    u_value v_more = u_field(u_field(s2398, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2398, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2398, 1), 1);
    t2399 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2399) {
    t2399 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2399) {
    u_no_case(s2398);
  }
  return t2399;
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
    static struct u_object l_u_caught_2400 = U_LIT("u_caught");
    u_value t2401 = f_c_46_c_call_2(&l_u_caught_2400, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2401;
    u_value s2402 = v_hs;
    int d2403 = 0;
    if ((!d2403) && u_is_con(s2402, &n_list_46_Nil)) {
      d2403 = 1;
      {
        u_value t2404 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2404;
      }
    }
    if ((!d2403) && (u_is_con(s2402, &n_list_46_Cons) && u_is_con(u_field(s2402, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2402, 0), 0);
      u_value v_p = u_field(u_field(s2402, 0), 1);
      u_value v_guard = u_field(u_field(s2402, 0), 2);
      u_value v_s = u_field(u_field(s2402, 0), 3);
      u_value v_more = u_field(s2402, 1);
      d2403 = 1;
      {
        u_value t2405 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2405;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2406 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2406;
        u_value s2407 = v_guard;
        int d2408 = 0;
        if ((!d2408) && u_is_con(s2407, &n_option_46_Some)) {
          u_value v_c = u_field(s2407, 0);
          d2408 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2409 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2409;
          }
        }
        if ((!d2408) && u_is_con(s2407, &n_option_46_None)) {
          d2408 = 1;
          {
          }
        }
        if (!d2408) {
          u_no_case(s2407);
        }
        u_value s2410 = v_p;
        int d2411 = 0;
        if ((!d2411) && u_is_con(s2410, &n_syntax_46_PWild)) {
          d2411 = 1;
          {
            u_value t2412 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2412;
          }
        }
        if (!d2411) {
          d2411 = 1;
          {
            static struct u_object l_u_has_type_2413 = U_LIT("u_has_type");
            u_value t2414 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2413, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2414;
          }
        }
        if (!d2411) {
          u_no_case(s2410);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2415 = U_LIT("!");
        u_value t2416 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2415, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2416;
      }
    }
    if (!d2403) {
      u_no_case(s2402);
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
    u_value s2417 = v_cases;
    int d2418 = 0;
    if ((!d2418) && u_is_con(s2417, &n_list_46_Nil)) {
      d2418 = 1;
      {
        u_value t2419 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2419;
      }
    }
    if ((!d2418) && (u_is_con(s2417, &n_list_46_Cons) && u_is_con(u_field(s2417, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2417, 0), 0);
      u_value v_guard = u_field(u_field(s2417, 0), 1);
      u_value v_s = u_field(u_field(s2417, 0), 2);
      u_value v_more = u_field(s2417, 1);
      d2418 = 1;
      {
        u_value t2420 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2420;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2421 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2421;
        u_value s2422 = v_guard;
        int d2423 = 0;
        if ((!d2423) && u_is_con(s2422, &n_option_46_Some)) {
          u_value v_c = u_field(s2422, 0);
          d2423 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2424 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2424;
          }
        }
        if ((!d2423) && u_is_con(s2422, &n_option_46_None)) {
          d2423 = 1;
          {
          }
        }
        if (!d2423) {
          u_no_case(s2422);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2425 = U_LIT("!");
        u_value t2426 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2425, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2426;
      }
    }
    if (!d2418) {
      u_no_case(s2417);
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
    u_value s2427 = v_c;
    int d2428 = 0;
    if ((!d2428) && u_is_con(s2427, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2427, 0);
      u_value v_xss = u_field(s2427, 2);
      u_value v_b = u_field(s2427, 4);
      d2428 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2429 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2430 = &t2429;
        u_value l2431 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2431, &n_list_46_Cons)) {
          u_value v_x = u_field(l2431, 0);
          if (u_truth(u_mk_bool(1))) {
            e2430 = u_append(e2430, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2431 = u_field(l2431, 1);
        }
        u_value t2432 = t2429;
        v_params = t2432;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2433 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2433;
          }
        } else {
          {
            u_value t2434 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2434;
          }
        }
        u_value t2435 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2436 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2435;
        *r_ms = t2436;
      }
    }
    if ((!d2428) && u_is_con(s2427, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2427, 0);
      u_value v_ins = u_field(s2427, 2);
      u_value v_outs = u_field(s2427, 3);
      u_value v_refs = u_field(s2427, 4);
      u_value v_body = u_field(s2427, 5);
      d2428 = 1;
      {
        u_value t2437 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2438 = &t2437;
        u_value l2439 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2439, &n_list_46_Cons)) {
          u_value v_x = u_field(l2439, 0);
          if (u_truth(u_mk_bool(1))) {
            e2438 = u_append(e2438, f_compile_46_pointer_1(v_x));
          }
          l2439 = u_field(l2439, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2437), v_body, &v_bs, r_g);
        u_value t2440 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2441 = &t2440;
        u_value l2442 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2442, &n_list_46_Cons)) {
          u_value v_x = u_field(l2442, 0);
          if (u_truth(u_mk_bool(1))) {
            e2441 = u_append(e2441, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2442 = u_field(l2442, 1);
        }
        u_value t2443 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2444 = &t2443;
        u_value l2445 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2445, &n_list_46_Cons)) {
          u_value v_x = u_field(l2445, 0);
          if (u_truth(u_mk_bool(1))) {
            e2444 = u_append(e2444, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2445 = u_field(l2445, 1);
        }
        u_value t2446 = f_list_46__43__43__2(t2440, t2443);
        v_params = t2446;
        u_value t2447 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2448 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2447;
        *r_ms = t2448;
      }
    }
    if ((!d2428) && u_is_con(s2427, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s2427, 0);
      d2428 = 1;
      {
        p_compile_46_compile_expr(v_gl, u_mk_con(&n_list_46_Nil, 0), v_e, &v_bs, &v_rb, r_g);
        static struct u_object l_u_print_line_2449 = U_LIT("u_print_line");
        u_value t2450 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2451 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_line_2449, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2450;
        *r_ms = t2451;
      }
    }
    if ((!d2428) && (u_is_con(s2427, &n_syntax_46_CRun) && u_is_con(u_field(s2427, 1), &n_syntax_46_SBlock))) {
      u_value v_xs = u_field(u_field(s2427, 1), 0);
      u_value v_body = u_field(u_field(s2427, 1), 1);
      d2428 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_body, &v_bs, r_g);
        u_value t2452 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2453 = &t2452;
        u_value l2454 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2454, &n_list_46_Cons)) {
          u_value v_x = u_field(l2454, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2455 = U_LIT("NULL");
            e2453 = u_append(e2453, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2455))));
          }
          l2454 = u_field(l2454, 1);
        }
        u_value t2456 = t2452;
        v_copy = t2456;
        u_value t2457 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2458 = &t2457;
        u_value l2459 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2459, &n_list_46_Cons)) {
          u_value v_x = u_field(l2459, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_u_print_binding_2460 = U_LIT("u_print_binding");
            e2458 = u_append(e2458, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_binding_2460, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_syntax_46_Name_46_base_1(v_x)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x)), u_mk_con(&n_list_46_Nil, 0))))));
          }
          l2459 = u_field(l2459, 1);
        }
        u_value t2461 = t2457;
        v_back = t2461;
        u_value t2462 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2463 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_copy, f_list_46__43__43__2(v_bs, v_back))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2462;
        *r_ms = t2463;
      }
    }
    if ((!d2428) && u_is_con(s2427, &n_syntax_46_CRun)) {
      u_value v_body = u_field(s2427, 1);
      d2428 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2464 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2465 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2464;
        *r_ms = t2465;
      }
    }
    if ((!d2428) && u_is_con(s2427, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s2427, 1);
      d2428 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2466 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2467 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2466;
        *r_ms = t2467;
      }
    }
    if (!d2428) {
      d2428 = 1;
      {
        u_value t2468 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2469 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2468;
        *r_ms = t2469;
      }
    }
    if (!d2428) {
      u_no_case(s2427);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2470 = v_cs;
    int d2471 = 0;
    if ((!d2471) && u_is_con(s2470, &n_list_46_Nil)) {
      d2471 = 1;
      {
        u_value t2472 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2473 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2472;
        *r_ms = t2473;
      }
    }
    if ((!d2471) && u_is_con(s2470, &n_list_46_Cons)) {
      u_value v_c = u_field(s2470, 0);
      u_value v_rest = u_field(s2470, 1);
      d2471 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_rest, &v_d2, &v_m2, r_g);
        u_value t2474 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2475 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2474;
        *r_ms = t2475;
      }
    }
    if (!d2471) {
      u_no_case(s2470);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_handler_2476 = U_LIT("handler");
  u_value t2503 = u_mk_con(&n_c_46_CeVar, 1, &l_handler_2476);
  u_value t2502;
  {
    u_value v_h = t2503;
    static struct u_object l_static_32_u_value_2477 = U_LIT("static u_value");
    static struct u_object l_cache_2478 = U_LIT("cache");
    static struct u_object l_NULL_2479 = U_LIT("NULL");
    static struct u_object l__61__61__2480 = U_LIT("==");
    static struct u_object l_cache_2481 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2482 = U_LIT("U_COMPUTING");
    static struct u_object l_u_fail_2483 = U_LIT("u_fail");
    static struct u_object l_recursive_32_initia_2484 = U_LIT("recursive initialization of ");
    static struct u_object l_cache_2485 = U_LIT("cache");
    static struct u_object l_cache_2486 = U_LIT("cache");
    static struct u_object l_cache_2487 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2488 = U_LIT("U_COMPUTING");
    static struct u_object l_struct_32_u_handler_2489 = U_LIT("struct u_handler");
    static struct u_object l_handler_2490 = U_LIT("handler");
    static struct u_object l_u_push_2491 = U_LIT("u_push");
    static struct u_object l__33__61__2492 = U_LIT("!=");
    static struct u_object l_setjmp_2493 = U_LIT("setjmp");
    static struct u_object l_env_2494 = U_LIT("env");
    static struct u_object l_u_pop_2495 = U_LIT("u_pop");
    static struct u_object l_cache_2496 = U_LIT("cache");
    static struct u_object l_NULL_2497 = U_LIT("NULL");
    static struct u_object l_u_reraise_2498 = U_LIT("u_reraise");
    static struct u_object l_u_pop_2499 = U_LIT("u_pop");
    static struct u_object l_cache_2500 = U_LIT("cache");
    static struct u_object l_cache_2501 = U_LIT("cache");
    t2502 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2477), &l_cache_2478, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2479))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2480, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2481), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2482)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2483, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2484, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2485), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2486))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2487), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2488)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2489)), &l_handler_2490, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2491, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__33__61__2492, f_c_46_c_call_2(&l_setjmp_2493, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, v_h, &l_env_2494), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2495, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2496), u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2497)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2498, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2499, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2500), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2501))), u_mk_con(&n_list_46_Nil, 0))))));
  }
  return t2502;
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2504 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2505 = &t2504;
  u_value l2506 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l2506, &n_list_46_Cons)) {
    u_value v_p = u_field(l2506, 0);
    if (u_truth(u_mk_bool(1))) {
      e2505 = u_append(e2505, f_compile_46_fun_wrapper_1(v_p));
    }
    l2506 = u_field(l2506, 1);
  }
  u_value t2507 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2508 = &t2507;
  u_value l2509 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l2509, &n_list_46_Cons)) {
    u_value v_p = u_field(l2509, 0);
    if (u_truth(u_mk_bool(1))) {
      e2508 = u_append(e2508, f_compile_46_constr_wrapper_1(v_p));
    }
    l2509 = u_field(l2509, 1);
  }
  u_value t2510 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2511 = &t2510;
  u_value l2512 = f_list_46_seconds_1(f_map_46_entries_1(f_compile_46_builtin_functions_0()));
  while (u_is_con(l2512, &n_list_46_Cons)) {
    u_value v_p = u_field(l2512, 0);
    if (u_truth(u_mk_bool(1))) {
      e2511 = u_append(e2511, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2512 = u_field(l2512, 1);
  }
  u_value t2513 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2514 = &t2513;
  u_value l2515 = f_map_46_entries_1(f_compile_46_Globals_46_procs_1(v_gl));
  while (u_is_con(l2515, &n_list_46_Cons)) {
    u_value v_p = u_field(l2515, 0);
    if (u_truth(u_mk_bool(1))) {
      e2514 = u_append(e2514, f_compile_46_proc_wrapper_1(v_p));
    }
    l2515 = u_field(l2515, 1);
  }
  return f_list_46__43__43__2(t2504, f_list_46__43__43__2(f_list_46_concat_1(t2507), f_list_46__43__43__2(t2510, t2513)));
}

u_value f_compile_46_proc_wrapper_1(u_value v_p) {
  u_value s2516 = v_p;
  u_value t2517 = NULL;
  if (!t2517) {
    u_value v_n = u_field(s2516, 0);
    u_value v_k = u_field(u_field(s2516, 1), 0);
    u_value v_m = u_field(u_field(s2516, 1), 1);
    static struct u_object l_wp__2518 = U_LIT("wp_");
    u_value t2519 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2520 = &t2519;
    u_value l2521 = f_compile_46_range_to_1(v_k);
    while (u_is_con(l2521, &n_list_46_Cons)) {
      u_value v_i = u_field(l2521, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_ins_2522 = U_LIT("ins");
        e2520 = u_append(e2520, f_compile_46_index_2(&l_ins_2522, v_i));
      }
      l2521 = u_field(l2521, 1);
    }
    u_value t2523 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2524 = &t2523;
    u_value l2525 = f_compile_46_range_to_1(v_m);
    while (u_is_con(l2525, &n_list_46_Cons)) {
      u_value v_i = u_field(l2525, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_outs_2526 = U_LIT("outs");
        e2524 = u_append(e2524, f_compile_46_index_2(&l_outs_2526, v_i));
      }
      l2525 = u_field(l2525, 1);
    }
    t2517 = u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), u_concat(&l_wp__2518, f_compile_46_mangle_1(v_n)), f_compile_46_proc_params_0(), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_n), f_list_46__43__43__2(t2519, t2523))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2517) {
    u_no_case(s2516);
  }
  return t2517;
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2527 = v_p;
  u_value t2528 = NULL;
  if (!t2528) {
    u_value v_n = u_field(s2527, 0);
    u_value v_k = u_field(s2527, 1);
    static struct u_object l_w__2529 = U_LIT("w_");
    t2528 = f_compile_46_wrapper_3(u_concat(&l_w__2529, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2528) {
    u_no_case(s2527);
  }
  return t2528;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2530 = v_p;
  u_value t2531 = NULL;
  if (!t2531) {
    u_value v_f = u_field(s2530, 0);
    u_value v_k = u_field(s2530, 1);
    static struct u_object l_w_2532 = U_LIT("w");
    t2531 = f_compile_46_wrapper_3(u_concat(&l_w_2532, v_f), v_f, v_k);
  }
  if (!t2531) {
    u_no_case(s2530);
  }
  return t2531;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2533 = U_LIT("env");
  static struct u_object l_args_2534 = U_LIT("args");
  u_value t2535 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2536 = &t2535;
  u_value l2537 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2537, &n_list_46_Cons)) {
    u_value v_i = u_field(l2537, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2538 = U_LIT("args");
      e2536 = u_append(e2536, f_compile_46_index_2(&l_args_2538, v_i));
    }
    l2537 = u_field(l2537, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2533), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2534), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2535))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2539 = v_p;
  u_value t2540 = NULL;
  if ((!t2540) && u_equals(u_field(s2539, 1), u_mk_int(0))) {
    t2540 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2540) {
    u_value v_n = u_field(s2539, 0);
    u_value v_k = u_field(s2539, 1);
    static struct u_object l_wc__2541 = U_LIT("wc_");
    static struct u_object l_env_2542 = U_LIT("env");
    static struct u_object l_args_2543 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2544 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2545 = U_LIT("args");
    t2540 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2541, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2542), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2543), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2544, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2545), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2540) {
    u_no_case(s2539);
  }
  return t2540;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2546;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2546 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2546 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2546;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2547 = v_d;
  u_value t2548 = NULL;
  if ((!t2548) && u_is_con(s2547, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2547, 0);
    u_value v_f = u_field(s2547, 1);
    u_value v_ps = u_field(s2547, 2);
    t2548 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2548) {
    t2548 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2548) {
    u_no_case(s2547);
  }
  return t2548;
}

void p_compile_46_compile_program(u_value v_q, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2549 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2549;
    u_value t2550 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t2550;
    p_compile_46_compile_cmds(v_gl, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value t2551 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, f_compile_46_Gen_46_lifted_1(v_g)));
    v_fs = t2551;
    static struct u_object l_ulang_46_h_2552 = U_LIT("ulang.h");
    u_value t2553 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2554 = &t2553;
    u_value l2555 = v_fs;
    while (u_is_con(l2555, &n_list_46_Cons)) {
      u_value v_d = u_field(l2555, 0);
      if (u_truth(u_mk_bool(1))) {
        e2554 = u_append(e2554, f_compile_46_prototype_1(v_d));
      }
      l2555 = u_field(l2555, 1);
    }
    static struct u_object l_main_2556 = U_LIT("main");
    static struct u_object l_argc_2557 = U_LIT("argc");
    static struct u_object l_char_2558 = U_LIT("char");
    static struct u_object l_argv_2559 = U_LIT("argv");
    static struct u_object l_u_args_2560 = U_LIT("u_args");
    static struct u_object l_argc_2561 = U_LIT("argc");
    static struct u_object l_argv_2562 = U_LIT("argv");
    u_value t2563 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2552), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2553), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2556, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2557), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2558))), &l_argv_2559), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2560, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2561), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2562), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2563;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_target) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2564 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2564, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2565 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2565, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q);
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

u_value lam352(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam362(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_f = args[0];
  return f_ctx_46_is_fun_2(f_check_46_State_46_ctx_1(v_s), v_f);
}

u_value lam369(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam373(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_p = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_p);
}

u_value lam603(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_t = args[0];
  return f_ctx_46_is_sort_2(f_check_46_State_46_ctx_1(v_s), v_t);
}

u_value lam714(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam717(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam782(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_q = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_q);
}

u_value lam1059(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

u_value lam1751(u_value *env, u_value *args) {
  u_value v_t = env[0];
  u_value v_e = args[0];
  return f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t);
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  {
    u_value v_args = NULL;
    p_os_args(&v_args);
    u_value s2566 = v_args;
    int d2567 = 0;
    if ((!d2567) && u_is_con(s2566, &n_list_46_Nil)) {
      d2567 = 1;
      {
      }
    }
    if ((!d2567) && (u_is_con(s2566, &n_list_46_Cons) && (u_is_con(u_field(s2566, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2566, 1), 1), &n_list_46_Nil)))) {
      u_value v_source = u_field(s2566, 0);
      u_value v_target = u_field(u_field(s2566, 1), 0);
      d2567 = 1;
      {
        p_compile_46_compile_file(v_source, v_target);
      }
    }
    if (!d2567) {
      d2567 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_sou_2568 = U_LIT("usage: ulang source.u target.c");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_sou_2568), "os.OsError");
      }
    }
    if (!d2567) {
      u_no_case(s2566);
    }
    u_print_binding("args", v_args);
  }
  return 0;
}
