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

const struct u_name n_syntax_46_CRun = {"syntax", "CRun", 7};

const struct u_name n_syntax_46_CTest = {"syntax", "CTest", 8};

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

const struct u_name n_check_46_Checked = {"check", "Checked", 0};

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

u_value w_f_compile_46_is_run_2(u_value *env, u_value *args);

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

u_value wc_check_46_Checked(u_value *env, u_value *args);

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

void p_compile_46_compile_program(u_value v_q, u_value v_entry, u_value *r_f);

void p_compile_46_compile_file(u_value v_source, u_value v_entry, u_value v_target);

u_value f_compile_46_is_run_2(u_value v_c, u_value v_entry);

void p_compile_46_compile_with(u_value v_source, u_value v_entry, u_value v_target, u_value *r_cache);

u_value lam367(u_value *env, u_value *args);

u_value lam368(u_value *env, u_value *args);

u_value lam370(u_value *env, u_value *args);

u_value lam371(u_value *env, u_value *args);

u_value lam381(u_value *env, u_value *args);

u_value lam388(u_value *env, u_value *args);

u_value lam392(u_value *env, u_value *args);

u_value lam622(u_value *env, u_value *args);

u_value lam733(u_value *env, u_value *args);

u_value lam736(u_value *env, u_value *args);

u_value lam804(u_value *env, u_value *args);

u_value lam1135(u_value *env, u_value *args);

u_value lam1826(u_value *env, u_value *args);

u_value lam2613(u_value *env, u_value *args);

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

u_value w_f_compile_46_is_run_2(u_value *env, u_value *args) {
  return f_compile_46_is_run_2(args[0], args[1]);
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

u_value wc_check_46_Checked(u_value *env, u_value *args) {
  return u_mk_con_array(&n_check_46_Checked, 3, args);
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
  p_compile_46_compile_program(ins[0], ins[1], outs[0]);
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

u_value f_ctx_46_ctx_of_2(u_value v_c, u_value v_ms) {
  return u_mk_con(&n_ctx_46_Ctx, 7, f_ctx_46_owned_map_2(f_ctx_46_Ctx_46_aliases_1(v_c), v_ms), f_ctx_46_owned_map_2(f_ctx_46_Ctx_46_funs_1(v_c), v_ms), f_ctx_46_owned_map_2(f_ctx_46_Ctx_46_procs_1(v_c), v_ms), f_ctx_46_owned_map_2(f_ctx_46_Ctx_46_datas_1(v_c), v_ms), f_ctx_46_owned_map_2(f_ctx_46_Ctx_46_members_1(v_c), v_ms), f_ctx_46_owned_map_2(f_ctx_46_Ctx_46_constrs_1(v_c), v_ms), f_map_46_put_all_2(f_ctx_46_owned_named_2(f_map_46_entries_1(f_ctx_46_Ctx_46_named_1(v_c)), v_ms), f_map_46_empty_0()));
}

u_value f_ctx_46_owned_map_2(u_value v_m, u_value v_ms) {
  return f_map_46_put_all_2(f_ctx_46_owned_2(f_map_46_entries_1(v_m), v_ms), f_map_46_empty_0());
}

u_value f_ctx_46_owned_2(u_value v_es, u_value v_ms) {
  u_value s217 = v_es;
  u_value t218 = NULL;
  if ((!t218) && u_is_con(s217, &n_list_46_Nil)) {
    t218 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t218) && u_is_con(s217, &n_list_46_Cons)) {
    u_value v_n = u_field(u_field(s217, 0), 0);
    u_value v_v = u_field(u_field(s217, 0), 1);
    u_value v_rest = u_field(s217, 1);
    u_value t219;
    if (u_truth(f_ctx_46_in_modules_2(v_n, v_ms))) {
      t219 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, v_v), f_ctx_46_owned_2(v_rest, v_ms));
    } else {
      t219 = f_ctx_46_owned_2(v_rest, v_ms);
    }
    t218 = t219;
  }
  if (!t218) {
    u_no_case(s217);
  }
  return t218;
}

u_value f_ctx_46_owned_named_2(u_value v_es, u_value v_ms) {
  u_value s220 = v_es;
  u_value t221 = NULL;
  if ((!t221) && u_is_con(s220, &n_list_46_Nil)) {
    t221 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t221) && u_is_con(s220, &n_list_46_Cons)) {
    u_value v_b = u_field(u_field(s220, 0), 0);
    u_value v_ns = u_field(u_field(s220, 0), 1);
    u_value v_rest = u_field(s220, 1);
    u_value t222 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e223 = &t222;
    u_value l224 = v_ns;
    while (u_is_con(l224, &n_list_46_Cons)) {
      u_value v_n = u_field(l224, 0);
      if (u_truth(f_ctx_46_in_modules_2(v_n, v_ms))) {
        e223 = u_append(e223, v_n);
      }
      l224 = u_field(l224, 1);
    }
    u_value t227 = t222;
    u_value t226;
    {
      u_value v_ks = t227;
      u_value t225;
      if (u_truth(u_eq(v_ks, u_mk_con(&n_list_46_Nil, 0)))) {
        t225 = f_ctx_46_owned_named_2(v_rest, v_ms);
      } else {
        t225 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_b, v_ks), f_ctx_46_owned_named_2(v_rest, v_ms));
      }
      t226 = t225;
    }
    t221 = t226;
  }
  if (!t221) {
    u_no_case(s220);
  }
  return t221;
}

u_value f_ctx_46_in_modules_2(u_value v_n, u_value v_ms) {
  u_value s228 = f_syntax_46_Name_46_module_1(v_n);
  u_value t229 = NULL;
  if ((!t229) && u_is_con(s228, &n_option_46_Some)) {
    u_value v_m = u_field(s228, 0);
    t229 = f_list_46_contains_2(v_m, v_ms);
  }
  if ((!t229) && u_is_con(s228, &n_option_46_None)) {
    t229 = u_mk_bool(0);
  }
  if (!t229) {
    u_no_case(s228);
  }
  return t229;
}

u_value f_ctx_46_ctx_union_2(u_value v_a, u_value v_b) {
  return u_mk_con(&n_ctx_46_Ctx, 7, f_map_46_put_all_2(f_map_46_entries_1(f_ctx_46_Ctx_46_aliases_1(v_b)), f_ctx_46_Ctx_46_aliases_1(v_a)), f_map_46_put_all_2(f_map_46_entries_1(f_ctx_46_Ctx_46_funs_1(v_b)), f_ctx_46_Ctx_46_funs_1(v_a)), f_map_46_put_all_2(f_map_46_entries_1(f_ctx_46_Ctx_46_procs_1(v_b)), f_ctx_46_Ctx_46_procs_1(v_a)), f_map_46_put_all_2(f_map_46_entries_1(f_ctx_46_Ctx_46_datas_1(v_b)), f_ctx_46_Ctx_46_datas_1(v_a)), f_map_46_put_all_2(f_map_46_entries_1(f_ctx_46_Ctx_46_members_1(v_b)), f_ctx_46_Ctx_46_members_1(v_a)), f_map_46_put_all_2(f_map_46_entries_1(f_ctx_46_Ctx_46_constrs_1(v_b)), f_ctx_46_Ctx_46_constrs_1(v_a)), f_ctx_46_union_named_2(f_map_46_entries_1(f_ctx_46_Ctx_46_named_1(v_b)), f_ctx_46_Ctx_46_named_1(v_a)));
}

u_value f_ctx_46_union_named_2(u_value v_es, u_value v_bs) {
  u_value s230 = v_es;
  u_value t231 = NULL;
  if ((!t231) && u_is_con(s230, &n_list_46_Nil)) {
    t231 = v_bs;
  }
  if ((!t231) && u_is_con(s230, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s230, 0), 0);
    u_value v_ns = u_field(u_field(s230, 0), 1);
    u_value v_rest = u_field(s230, 1);
    u_value s232 = f_map_46_get_2(v_k, v_bs);
    u_value t233 = NULL;
    if ((!t233) && u_is_con(s232, &n_option_46_Some)) {
      u_value v_ms = u_field(s232, 0);
      t233 = f_ctx_46_union_named_2(v_rest, f_map_46_put_3(v_k, f_list_46__43__43__2(v_ms, v_ns), v_bs));
    }
    if ((!t233) && u_is_con(s232, &n_option_46_None)) {
      t233 = f_ctx_46_union_named_2(v_rest, f_map_46_put_3(v_k, v_ns, v_bs));
    }
    if (!t233) {
      u_no_case(s232);
    }
    t231 = t233;
  }
  if (!t231) {
    u_no_case(s230);
  }
  return t231;
}

u_value f_ctx_46_alias_of_2(u_value v_c, u_value v_n) {
  return f_map_46_get_2(v_n, f_ctx_46_Ctx_46_aliases_1(v_c));
}

u_value f_ctx_46_schemes_of_2(u_value v_c, u_value v_n) {
  u_value s234 = f_map_46_get_2(v_n, f_ctx_46_Ctx_46_funs_1(v_c));
  u_value t235 = NULL;
  if ((!t235) && u_is_con(s234, &n_option_46_Some)) {
    u_value v_ss = u_field(s234, 0);
    t235 = v_ss;
  }
  if ((!t235) && u_is_con(s234, &n_option_46_None)) {
    t235 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t235) {
    u_no_case(s234);
  }
  return t235;
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
  u_value t236 = f_ctx_46_is_data_2(v_c, v_n);
  if (!u_truth(t236)) {
    t236 = f_ctx_46_is_alias_2(v_c, v_n);
  }
  return t236;
}

u_value f_ctx_46_names_of_2(u_value v_c, u_value v_b) {
  u_value s237 = f_map_46_get_2(v_b, f_ctx_46_Ctx_46_named_1(v_c));
  u_value t238 = NULL;
  if ((!t238) && u_is_con(s237, &n_option_46_Some)) {
    u_value v_ns = u_field(s237, 0);
    t238 = v_ns;
  }
  if ((!t238) && u_is_con(s237, &n_option_46_None)) {
    t238 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t238) {
    u_no_case(s237);
  }
  return t238;
}

u_value f_ctx_46_with_named_2(u_value v_bs, u_value v_n) {
  u_value s239 = f_map_46_get_2(f_syntax_46_Name_46_base_1(v_n), v_bs);
  u_value t240 = NULL;
  if ((!t240) && u_is_con(s239, &n_option_46_Some)) {
    u_value v_ns = u_field(s239, 0);
    u_value t241;
    if (u_truth(f_list_46_contains_2(v_n, v_ns))) {
      t241 = v_bs;
    } else {
      t241 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), f_list_46__43__43__2(v_ns, u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0))), v_bs);
    }
    t240 = t241;
  }
  if ((!t240) && u_is_con(s239, &n_option_46_None)) {
    t240 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_bs);
  }
  if (!t240) {
    u_no_case(s239);
  }
  return t240;
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
  u_value s242 = v_x;
  u_value t243 = NULL;
  if ((!t243) && u_is_con(s242, &n_check_46_State)) {
    u_value v_y = u_field(s242, 0);
    t243 = v_y;
  }
  if (!t243) {
    u_no_case(s242);
  }
  return t243;
}

u_value f_check_46_State_46_sub_1(u_value v_x) {
  u_value s244 = v_x;
  u_value t245 = NULL;
  if ((!t245) && u_is_con(s244, &n_check_46_State)) {
    u_value v_y = u_field(s244, 1);
    t245 = v_y;
  }
  if (!t245) {
    u_no_case(s244);
  }
  return t245;
}

u_value f_check_46_State_46_count_1(u_value v_x) {
  u_value s246 = v_x;
  u_value t247 = NULL;
  if ((!t247) && u_is_con(s246, &n_check_46_State)) {
    u_value v_y = u_field(s246, 2);
    t247 = v_y;
  }
  if (!t247) {
    u_no_case(s246);
  }
  return t247;
}

u_value f_check_46_State_46_module_1(u_value v_x) {
  u_value s248 = v_x;
  u_value t249 = NULL;
  if ((!t249) && u_is_con(s248, &n_check_46_State)) {
    u_value v_y = u_field(s248, 3);
    t249 = v_y;
  }
  if (!t249) {
    u_no_case(s248);
  }
  return t249;
}

u_value f_check_46_State_46_visible_1(u_value v_x) {
  u_value s250 = v_x;
  u_value t251 = NULL;
  if ((!t251) && u_is_con(s250, &n_check_46_State)) {
    u_value v_y = u_field(s250, 4);
    t251 = v_y;
  }
  if (!t251) {
    u_no_case(s250);
  }
  return t251;
}

u_value f_check_46_State_46_table_1(u_value v_x) {
  u_value s252 = v_x;
  u_value t253 = NULL;
  if ((!t253) && u_is_con(s252, &n_check_46_State)) {
    u_value v_y = u_field(s252, 5);
    t253 = v_y;
  }
  if (!t253) {
    u_no_case(s252);
  }
  return t253;
}

u_value f_check_46_State_46_ctx_58__61__2(u_value v_x, u_value v_y) {
  u_value s254 = v_x;
  u_value t255 = NULL;
  if ((!t255) && u_is_con(s254, &n_check_46_State)) {
    u_value v_a1 = u_field(s254, 1);
    u_value v_a2 = u_field(s254, 2);
    u_value v_a3 = u_field(s254, 3);
    u_value v_a4 = u_field(s254, 4);
    u_value v_a5 = u_field(s254, 5);
    t255 = u_mk_con(&n_check_46_State, 6, v_y, v_a1, v_a2, v_a3, v_a4, v_a5);
  }
  if (!t255) {
    u_no_case(s254);
  }
  return t255;
}

u_value f_check_46_State_46_sub_58__61__2(u_value v_x, u_value v_y) {
  u_value s256 = v_x;
  u_value t257 = NULL;
  if ((!t257) && u_is_con(s256, &n_check_46_State)) {
    u_value v_a0 = u_field(s256, 0);
    u_value v_a2 = u_field(s256, 2);
    u_value v_a3 = u_field(s256, 3);
    u_value v_a4 = u_field(s256, 4);
    u_value v_a5 = u_field(s256, 5);
    t257 = u_mk_con(&n_check_46_State, 6, v_a0, v_y, v_a2, v_a3, v_a4, v_a5);
  }
  if (!t257) {
    u_no_case(s256);
  }
  return t257;
}

u_value f_check_46_State_46_count_58__61__2(u_value v_x, u_value v_y) {
  u_value s258 = v_x;
  u_value t259 = NULL;
  if ((!t259) && u_is_con(s258, &n_check_46_State)) {
    u_value v_a0 = u_field(s258, 0);
    u_value v_a1 = u_field(s258, 1);
    u_value v_a3 = u_field(s258, 3);
    u_value v_a4 = u_field(s258, 4);
    u_value v_a5 = u_field(s258, 5);
    t259 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_y, v_a3, v_a4, v_a5);
  }
  if (!t259) {
    u_no_case(s258);
  }
  return t259;
}

u_value f_check_46_State_46_module_58__61__2(u_value v_x, u_value v_y) {
  u_value s260 = v_x;
  u_value t261 = NULL;
  if ((!t261) && u_is_con(s260, &n_check_46_State)) {
    u_value v_a0 = u_field(s260, 0);
    u_value v_a1 = u_field(s260, 1);
    u_value v_a2 = u_field(s260, 2);
    u_value v_a4 = u_field(s260, 4);
    u_value v_a5 = u_field(s260, 5);
    t261 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_a2, v_y, v_a4, v_a5);
  }
  if (!t261) {
    u_no_case(s260);
  }
  return t261;
}

u_value f_check_46_State_46_visible_58__61__2(u_value v_x, u_value v_y) {
  u_value s262 = v_x;
  u_value t263 = NULL;
  if ((!t263) && u_is_con(s262, &n_check_46_State)) {
    u_value v_a0 = u_field(s262, 0);
    u_value v_a1 = u_field(s262, 1);
    u_value v_a2 = u_field(s262, 2);
    u_value v_a3 = u_field(s262, 3);
    u_value v_a5 = u_field(s262, 5);
    t263 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_a2, v_a3, v_y, v_a5);
  }
  if (!t263) {
    u_no_case(s262);
  }
  return t263;
}

u_value f_check_46_State_46_table_58__61__2(u_value v_x, u_value v_y) {
  u_value s264 = v_x;
  u_value t265 = NULL;
  if ((!t265) && u_is_con(s264, &n_check_46_State)) {
    u_value v_a0 = u_field(s264, 0);
    u_value v_a1 = u_field(s264, 1);
    u_value v_a2 = u_field(s264, 2);
    u_value v_a3 = u_field(s264, 3);
    u_value v_a4 = u_field(s264, 4);
    t265 = u_mk_con(&n_check_46_State, 6, v_a0, v_a1, v_a2, v_a3, v_a4, v_y);
  }
  if (!t265) {
    u_no_case(s264);
  }
  return t265;
}

u_value f_check_46_with_ctx_2(u_value v_s, u_value v_c) {
  return f_check_46_State_46_ctx_58__61__2(v_s, v_c);
}

u_value f_check_46_enter_2(u_value v_s, u_value v_m) {
  return f_check_46_State_46_visible_58__61__2(f_check_46_State_46_module_58__61__2(f_check_46_State_46_count_58__61__2(f_check_46_State_46_sub_58__61__2(v_s, f_map_46_empty_0()), u_mk_int(0)), v_m), f_check_46_sees_2(v_m, f_check_46_State_46_table_1(v_s)));
}

u_value f_check_46_sees_2(u_value v_m, u_value v_vs) {
  u_value s266 = f_list_46_lookup_2(v_m, v_vs);
  u_value t267 = NULL;
  if ((!t267) && u_is_con(s266, &n_option_46_Some)) {
    u_value v_ms = u_field(s266, 0);
    t267 = v_ms;
  }
  if ((!t267) && u_is_con(s266, &n_option_46_None)) {
    t267 = u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t267) {
    u_no_case(s266);
  }
  return t267;
}

u_value f_check_46_start_2(u_value v_main, u_value v_vs) {
  return u_mk_con(&n_check_46_State, 6, f_check_46_initial_0(), f_map_46_empty_0(), u_mk_int(0), v_main, f_check_46_sees_2(v_main, v_vs), v_vs);
}

u_value f_check_46_flatten_2(u_value v_m, u_value v_cs) {
  u_value s268 = v_cs;
  u_value t269 = NULL;
  if ((!t269) && u_is_con(s268, &n_list_46_Nil)) {
    t269 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t269) && (u_is_con(s268, &n_list_46_Cons) && u_is_con(u_field(s268, 0), &n_syntax_46_CModule))) {
    u_value v_n = u_field(u_field(s268, 0), 0);
    u_value v_ds = u_field(u_field(s268, 0), 1);
    u_value v_rest = u_field(s268, 1);
    t269 = f_list_46__43__43__2(f_check_46_flatten_2(v_n, v_ds), f_check_46_flatten_2(v_m, v_rest));
  }
  if ((!t269) && u_is_con(s268, &n_list_46_Cons)) {
    u_value v_c = u_field(s268, 0);
    u_value v_rest = u_field(s268, 1);
    t269 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_m, f_check_46_qualify_2(v_m, v_c)), f_check_46_flatten_2(v_m, v_rest));
  }
  if (!t269) {
    u_no_case(s268);
  }
  return t269;
}

u_value f_check_46_in_module_2(u_value v_m, u_value v_n) {
  return u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), f_syntax_46_Name_46_base_1(v_n));
}

u_value f_check_46_qualify_2(u_value v_m, u_value v_c) {
  u_value s270 = v_c;
  u_value t271 = NULL;
  if ((!t271) && u_is_con(s270, &n_syntax_46_CSort)) {
    u_value v_n = u_field(s270, 0);
    u_value v_ps = u_field(s270, 1);
    u_value v_t = u_field(s270, 2);
    t271 = u_mk_con(&n_syntax_46_CSort, 3, f_check_46_in_module_2(v_m, v_n), v_ps, v_t);
  }
  if ((!t271) && u_is_con(s270, &n_syntax_46_CData)) {
    u_value v_n = u_field(s270, 0);
    u_value v_ps = u_field(s270, 1);
    u_value v_ks = u_field(s270, 2);
    u_value t272 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e273 = &t272;
    u_value l274 = v_ks;
    while (u_is_con(l274, &n_list_46_Cons)) {
      u_value v_k = u_field(l274, 0);
      if (u_truth(u_mk_bool(1))) {
        e273 = u_append(e273, f_check_46_qualify_constr_2(v_m, v_k));
      }
      l274 = u_field(l274, 1);
    }
    t271 = u_mk_con(&n_syntax_46_CData, 3, f_check_46_in_module_2(v_m, v_n), v_ps, t272);
  }
  if ((!t271) && u_is_con(s270, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s270, 0);
    u_value v_ps = u_field(s270, 1);
    u_value v_xss = u_field(s270, 2);
    u_value v_r = u_field(s270, 3);
    u_value v_b = u_field(s270, 4);
    t271 = u_mk_con(&n_syntax_46_CFun, 5, f_check_46_in_module_2(v_m, v_n), v_ps, v_xss, v_r, v_b);
  }
  if ((!t271) && u_is_con(s270, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s270, 0);
    u_value v_ps = u_field(s270, 1);
    u_value v_ins = u_field(s270, 2);
    u_value v_outs = u_field(s270, 3);
    u_value v_refs = u_field(s270, 4);
    u_value v_b = u_field(s270, 5);
    t271 = u_mk_con(&n_syntax_46_CProc, 6, f_check_46_in_module_2(v_m, v_n), v_ps, v_ins, v_outs, v_refs, v_b);
  }
  if (!t271) {
    t271 = v_c;
  }
  if (!t271) {
    u_no_case(s270);
  }
  return t271;
}

u_value f_check_46_qualify_constr_2(u_value v_m, u_value v_k) {
  return f_syntax_46_Constr_46_name_58__61__2(v_k, f_check_46_in_module_2(v_m, f_syntax_46_Constr_46_name_1(v_k)));
}

u_value f_check_46_imports_in_2(u_value v_ds, u_value v_m) {
  u_value s275 = v_ds;
  u_value t276 = NULL;
  if ((!t276) && u_is_con(s275, &n_list_46_Nil)) {
    t276 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t276) && (u_is_con(s275, &n_list_46_Cons) && u_is_con(u_field(u_field(s275, 0), 1), &n_syntax_46_CImport))) {
    u_value v_k = u_field(u_field(s275, 0), 0);
    u_value v_n = u_field(u_field(u_field(s275, 0), 1), 0);
    u_value v_rest = u_field(s275, 1);
    if (u_truth(u_eq(v_k, v_m))) {
      t276 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_imports_in_2(v_rest, v_m));
    }
  }
  if ((!t276) && u_is_con(s275, &n_list_46_Cons)) {
    u_value v_rest = u_field(s275, 1);
    t276 = f_check_46_imports_in_2(v_rest, v_m);
  }
  if (!t276) {
    u_no_case(s275);
  }
  return t276;
}

u_value f_check_46_visibility_2(u_value v_imports, u_value v_main) {
  u_value t277 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e278 = &t277;
  u_value l279 = f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_main, u_mk_con(&n_list_46_Nil, 0)), f_list_46_firsts_1(v_imports)));
  while (u_is_con(l279, &n_list_46_Cons)) {
    u_value v_m = u_field(l279, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_prelude_280 = U_LIT("prelude");
      static struct u_object l_prelude_281 = U_LIT("prelude");
      e278 = u_append(e278, u_mk_tuple(2, v_m, f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_check_46_imports_of_2(v_imports, v_m), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, &l_prelude_280, u_mk_con(&n_list_46_Nil, 0)), f_check_46_imports_of_2(v_imports, &l_prelude_281)))))));
    }
    l279 = u_field(l279, 1);
  }
  return t277;
}

u_value f_check_46_imports_of_2(u_value v_imports, u_value v_m) {
  u_value s282 = f_list_46_lookup_2(v_m, v_imports);
  u_value t283 = NULL;
  if ((!t283) && u_is_con(s282, &n_option_46_Some)) {
    u_value v_ns = u_field(s282, 0);
    t283 = v_ns;
  }
  if ((!t283) && u_is_con(s282, &n_option_46_None)) {
    t283 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t283) {
    u_no_case(s282);
  }
  return t283;
}

u_value f_check_46_visible_names_3(u_value v_s, u_value v_n, u_value v_declared) {
  u_value t284 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e285 = &t284;
  u_value l286 = f_ctx_46_names_of_2(f_check_46_State_46_ctx_1(v_s), f_syntax_46_Name_46_base_1(v_n));
  while (u_is_con(l286, &n_list_46_Cons)) {
    u_value v_d = u_field(l286, 0);
    if (u_truth(u_apply(v_declared, 1, v_d))) {
      e285 = u_append(e285, v_d);
    }
    l286 = u_field(l286, 1);
  }
  u_value t295 = t284;
  u_value t294;
  {
    u_value v_ds = t295;
    u_value s287 = v_n;
    u_value t288 = NULL;
    if ((!t288) && (u_is_con(s287, &n_syntax_46_Name) && u_is_con(u_field(s287, 0), &n_option_46_Some))) {
      u_value t289;
      if (u_truth(f_list_46_contains_2(v_n, v_ds))) {
        t289 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
      } else {
        t289 = u_mk_con(&n_list_46_Nil, 0);
      }
      t288 = t289;
    }
    if ((!t288) && (u_is_con(s287, &n_syntax_46_Name) && u_is_con(u_field(s287, 0), &n_option_46_None))) {
      u_value v_b = u_field(s287, 1);
      u_value t293;
      if (u_truth(f_list_46_contains_2(u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), v_ds))) {
        t293 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), u_mk_con(&n_list_46_Nil, 0));
      } else {
        u_value t290 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e291 = &t290;
        u_value l292 = v_ds;
        while (u_is_con(l292, &n_list_46_Cons)) {
          u_value v_d = u_field(l292, 0);
          if (u_truth(f_check_46_is_visible_2(v_s, v_d))) {
            e291 = u_append(e291, v_d);
          }
          l292 = u_field(l292, 1);
        }
        t293 = t290;
      }
      t288 = t293;
    }
    if (!t288) {
      u_no_case(s287);
    }
    t294 = t288;
  }
  return t294;
}

u_value f_check_46_is_visible_2(u_value v_s, u_value v_d) {
  u_value s296 = f_syntax_46_Name_46_module_1(v_d);
  u_value t297 = NULL;
  if ((!t297) && u_is_con(s296, &n_option_46_Some)) {
    u_value v_m = u_field(s296, 0);
    t297 = f_list_46_contains_2(v_m, f_check_46_State_46_visible_1(v_s));
  }
  if ((!t297) && u_is_con(s296, &n_option_46_None)) {
    t297 = u_mk_bool(0);
  }
  if (!t297) {
    u_no_case(s296);
  }
  return t297;
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
  static struct u_object l_Bool_298 = U_LIT("Bool");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Bool_298), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Int_299 = U_LIT("Int");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Int_299), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Str_300 = U_LIT("Str");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Str_300), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Char_301 = U_LIT("Char");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Char_301), u_mk_con(&n_list_46_Nil, 0));
  return cache;
}

u_value f_check_46_tlist_1(u_value v_t) {
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_list_name_0(), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_check_46_fun_2(u_value v_args, u_value v_res) {
  return u_mk_con(&n_syntax_46_TFun, 2, v_args, v_res);
}

u_value f_check_46_params_1(u_value v_ps) {
  u_value t302 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e303 = &t302;
  u_value l304 = v_ps;
  while (u_is_con(l304, &n_list_46_Cons)) {
    u_value v_p = u_field(l304, 0);
    if (u_truth(u_mk_bool(1))) {
      e303 = u_append(e303, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l304 = u_field(l304, 1);
  }
  return t302;
}

u_value f_check_46_basic_1(u_value v_n) {
  u_value s305 = v_n;
  u_value t306 = NULL;
  if ((!t306) && (u_is_con(s305, &n_syntax_46_Name) && u_is_con(u_field(s305, 0), &n_option_46_None))) {
    u_value v_b = u_field(s305, 1);
    static struct u_object l_Bool_307 = U_LIT("Bool");
    static struct u_object l_Int_308 = U_LIT("Int");
    static struct u_object l_Str_309 = U_LIT("Str");
    static struct u_object l_Char_310 = U_LIT("Char");
    t306 = f_list_46_contains_2(v_b, u_mk_con(&n_list_46_Cons, 2, &l_Bool_307, u_mk_con(&n_list_46_Cons, 2, &l_Int_308, u_mk_con(&n_list_46_Cons, 2, &l_Str_309, u_mk_con(&n_list_46_Cons, 2, &l_Char_310, u_mk_con(&n_list_46_Nil, 0))))));
  }
  if (!t306) {
    t306 = u_mk_bool(0);
  }
  if (!t306) {
    u_no_case(s305);
  }
  return t306;
}

u_value f_check_46_literal_1(u_value v_v) {
  u_value s311 = v_v;
  u_value t312 = NULL;
  if ((!t312) && u_is_con(s311, &n_syntax_46_VUnit)) {
    t312 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t312) && u_is_con(s311, &n_syntax_46_VBool)) {
    t312 = f_check_46_tbool_0();
  }
  if ((!t312) && u_is_con(s311, &n_syntax_46_VInt)) {
    t312 = f_check_46_tint_0();
  }
  if ((!t312) && u_is_con(s311, &n_syntax_46_VStr)) {
    t312 = f_check_46_tstr_0();
  }
  if ((!t312) && u_is_con(s311, &n_syntax_46_VChar)) {
    t312 = f_check_46_tchar_0();
  }
  if (!t312) {
    u_no_case(s311);
  }
  return t312;
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
  u_value s313 = v_n;
  u_value t314 = NULL;
  if ((!t314) && (u_is_con(s313, &n_syntax_46_Name) && u_is_con(u_field(s313, 0), &n_option_46_None))) {
    u_value v_b = u_field(s313, 1);
    t314 = f_check_46_builtin_scheme_1(v_b);
  }
  if (!t314) {
    t314 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t314) {
    u_no_case(s313);
  }
  return t314;
}

u_value f_check_46_builtin_scheme_1(u_value v_b) {
  static struct u_object l_A_315 = U_LIT("A");
  u_value t319 = u_mk_con(&n_syntax_46_TParam, 1, &l_A_315);
  u_value t318;
  {
    u_value v_a = t319;
    u_value s316 = v_b;
    u_value t317 = NULL;
    if ((!t317) && u_equals(s316, u_mk_lit("+"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("-"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("*"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("/"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("%"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("<"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("=="))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("compare"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("and"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("or"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("chars"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tlist_1(f_check_46_tchar_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("string"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tchar_0()), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("ord"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("chr"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("^"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("str_size"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("str_join"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("str_slice"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("str_split"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tlist_1(f_check_46_tstr_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("str_get"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("str_contains"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("str_compare"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("str_show"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("str_quote"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("str_number"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_option_name_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("is_digit"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("is_letter"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t317) && u_equals(s316, u_mk_lit("is_space"))) {
      t317 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if (!t317) {
      t317 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t317) {
      u_no_case(s316);
    }
    t318 = t317;
  }
  return t318;
}

u_value f_check_46_is_meta_1(u_value v_t) {
  u_value s320 = v_t;
  u_value t321 = NULL;
  if ((!t321) && u_is_con(s320, &n_syntax_46_TMeta)) {
    t321 = u_mk_bool(1);
  }
  if (!t321) {
    t321 = u_mk_bool(0);
  }
  if (!t321) {
    u_no_case(s320);
  }
  return t321;
}

u_value f_check_46_free_1(u_value v_t) {
  u_value s322 = v_t;
  u_value t323 = NULL;
  if ((!t323) && u_is_con(s322, &n_syntax_46_TParam)) {
    t323 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t323) && u_is_con(s322, &n_syntax_46_TMeta)) {
    t323 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t323) && u_is_con(s322, &n_syntax_46_THole)) {
    t323 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t323) && u_is_con(s322, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s322, 1);
    u_value t324 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e325 = &t324;
    u_value l326 = v_args;
    while (u_is_con(l326, &n_list_46_Cons)) {
      u_value v_a = u_field(l326, 0);
      if (u_truth(u_mk_bool(1))) {
        e325 = u_append(e325, f_check_46_free_1(v_a));
      }
      l326 = u_field(l326, 1);
    }
    t323 = f_list_46_distinct_1(f_list_46_concat_1(t324));
  }
  if ((!t323) && u_is_con(s322, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s322, 0);
    u_value v_r = u_field(s322, 1);
    u_value t327 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e328 = &t327;
    u_value l329 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l329, &n_list_46_Cons)) {
      u_value v_a = u_field(l329, 0);
      if (u_truth(u_mk_bool(1))) {
        e328 = u_append(e328, f_check_46_free_1(v_a));
      }
      l329 = u_field(l329, 1);
    }
    t323 = f_list_46_distinct_1(f_list_46_concat_1(t327));
  }
  if ((!t323) && u_is_con(s322, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s322, 0);
    u_value v_os = u_field(s322, 1);
    u_value v_rs = u_field(s322, 2);
    u_value t330 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e331 = &t330;
    u_value l332 = f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs));
    while (u_is_con(l332, &n_list_46_Cons)) {
      u_value v_a = u_field(l332, 0);
      if (u_truth(u_mk_bool(1))) {
        e331 = u_append(e331, f_check_46_free_1(v_a));
      }
      l332 = u_field(l332, 1);
    }
    t323 = f_list_46_distinct_1(f_list_46_concat_1(t330));
  }
  if ((!t323) && u_is_con(s322, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s322, 0);
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
    t323 = f_list_46_distinct_1(f_list_46_concat_1(t333));
  }
  if (!t323) {
    u_no_case(s322);
  }
  return t323;
}

u_value f_check_46_holes_1(u_value v_t) {
  u_value s336 = v_t;
  u_value t337 = NULL;
  if ((!t337) && u_is_con(s336, &n_syntax_46_THole)) {
    t337 = u_mk_bool(1);
  }
  if ((!t337) && u_is_con(s336, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s336, 1);
    t337 = f_list_46_exists_2(v_args, u_mk_closure(w_f_check_46_holes_1, 0));
  }
  if ((!t337) && u_is_con(s336, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s336, 0);
    u_value v_r = u_field(s336, 1);
    u_value t338 = f_list_46_exists_2(v_args, u_mk_closure(w_f_check_46_holes_1, 0));
    if (!u_truth(t338)) {
      t338 = f_check_46_holes_1(v_r);
    }
    t337 = t338;
  }
  if ((!t337) && u_is_con(s336, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s336, 0);
    u_value v_os = u_field(s336, 1);
    u_value v_rs = u_field(s336, 2);
    t337 = f_list_46_exists_2(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs)), u_mk_closure(w_f_check_46_holes_1, 0));
  }
  if ((!t337) && u_is_con(s336, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s336, 0);
    t337 = f_list_46_exists_2(v_ts, u_mk_closure(w_f_check_46_holes_1, 0));
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
  u_value s339 = v_t;
  u_value t340 = NULL;
  if ((!t340) && u_is_con(s339, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s339, 0);
    u_value v_args = u_field(s339, 1);
    u_value t341 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e342 = &t341;
    u_value l343 = v_args;
    while (u_is_con(l343, &n_list_46_Cons)) {
      u_value v_a = u_field(l343, 0);
      if (u_truth(u_mk_bool(1))) {
        e342 = u_append(e342, f_check_46_subst_2(v_a, v_m));
      }
      l343 = u_field(l343, 1);
    }
    t340 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t341);
  }
  if ((!t340) && u_is_con(s339, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s339, 0);
    u_value v_r = u_field(s339, 1);
    u_value t344 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e345 = &t344;
    u_value l346 = v_args;
    while (u_is_con(l346, &n_list_46_Cons)) {
      u_value v_a = u_field(l346, 0);
      if (u_truth(u_mk_bool(1))) {
        e345 = u_append(e345, f_check_46_subst_2(v_a, v_m));
      }
      l346 = u_field(l346, 1);
    }
    t340 = u_mk_con(&n_syntax_46_TFun, 2, t344, f_check_46_subst_2(v_r, v_m));
  }
  if ((!t340) && u_is_con(s339, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s339, 0);
    u_value v_os = u_field(s339, 1);
    u_value v_rs = u_field(s339, 2);
    u_value t347 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e348 = &t347;
    u_value l349 = v_is;
    while (u_is_con(l349, &n_list_46_Cons)) {
      u_value v_a = u_field(l349, 0);
      if (u_truth(u_mk_bool(1))) {
        e348 = u_append(e348, f_check_46_subst_2(v_a, v_m));
      }
      l349 = u_field(l349, 1);
    }
    u_value t350 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e351 = &t350;
    u_value l352 = v_os;
    while (u_is_con(l352, &n_list_46_Cons)) {
      u_value v_a = u_field(l352, 0);
      if (u_truth(u_mk_bool(1))) {
        e351 = u_append(e351, f_check_46_subst_2(v_a, v_m));
      }
      l352 = u_field(l352, 1);
    }
    u_value t353 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e354 = &t353;
    u_value l355 = v_rs;
    while (u_is_con(l355, &n_list_46_Cons)) {
      u_value v_a = u_field(l355, 0);
      if (u_truth(u_mk_bool(1))) {
        e354 = u_append(e354, f_check_46_subst_2(v_a, v_m));
      }
      l355 = u_field(l355, 1);
    }
    t340 = u_mk_con(&n_syntax_46_TProc, 3, t347, t350, t353);
  }
  if ((!t340) && u_is_con(s339, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s339, 0);
    u_value t356 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e357 = &t356;
    u_value l358 = v_ts;
    while (u_is_con(l358, &n_list_46_Cons)) {
      u_value v_a = u_field(l358, 0);
      if (u_truth(u_mk_bool(1))) {
        e357 = u_append(e357, f_check_46_subst_2(v_a, v_m));
      }
      l358 = u_field(l358, 1);
    }
    t340 = u_mk_con(&n_syntax_46_TTuple, 1, t356);
  }
  if (!t340) {
    u_value s359 = f_list_46_lookup_2(v_t, v_m);
    u_value t360 = NULL;
    if ((!t360) && u_is_con(s359, &n_option_46_Some)) {
      u_value v_u = u_field(s359, 0);
      t360 = v_u;
    }
    if ((!t360) && u_is_con(s359, &n_option_46_None)) {
      t360 = v_t;
    }
    if (!t360) {
      u_no_case(s359);
    }
    t340 = t360;
  }
  if (!t340) {
    u_no_case(s339);
  }
  return t340;
}

u_value f_check_46_resolve_2(u_value v_su, u_value v_t) {
  u_value s361 = v_t;
  u_value t362 = NULL;
  if ((!t362) && u_is_con(s361, &n_syntax_46_TMeta)) {
    u_value v_i = u_field(s361, 0);
    u_value s363 = f_map_46_get_2(v_i, v_su);
    u_value t364 = NULL;
    if ((!t364) && u_is_con(s363, &n_option_46_Some)) {
      u_value v_u = u_field(s363, 0);
      t364 = f_check_46_resolve_2(v_su, v_u);
    }
    if ((!t364) && u_is_con(s363, &n_option_46_None)) {
      t364 = v_t;
    }
    if (!t364) {
      u_no_case(s363);
    }
    t362 = t364;
  }
  if (!t362) {
    t362 = v_t;
  }
  if (!t362) {
    u_no_case(s361);
  }
  return t362;
}

u_value f_check_46_occurs_3(u_value v_su, u_value v_i, u_value v_t) {
  u_value s365 = f_check_46_resolve_2(v_su, v_t);
  u_value t366 = NULL;
  if ((!t366) && u_is_con(s365, &n_syntax_46_TMeta)) {
    u_value v_j = u_field(s365, 0);
    t366 = u_eq(v_i, v_j);
  }
  if ((!t366) && u_is_con(s365, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s365, 1);
    t366 = f_list_46_exists_2(v_args, u_mk_closure(lam367, 2, v_su, v_i));
  }
  if ((!t366) && u_is_con(s365, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s365, 0);
    u_value v_r = u_field(s365, 1);
    u_value t369 = f_list_46_exists_2(v_args, u_mk_closure(lam368, 2, v_su, v_i));
    if (!u_truth(t369)) {
      t369 = f_check_46_occurs_3(v_su, v_i, v_r);
    }
    t366 = t369;
  }
  if ((!t366) && u_is_con(s365, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s365, 0);
    u_value v_os = u_field(s365, 1);
    u_value v_rs = u_field(s365, 2);
    t366 = f_list_46_exists_2(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs)), u_mk_closure(lam370, 2, v_su, v_i));
  }
  if ((!t366) && u_is_con(s365, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s365, 0);
    t366 = f_list_46_exists_2(v_ts, u_mk_closure(lam371, 2, v_su, v_i));
  }
  if (!t366) {
    t366 = u_mk_bool(0);
  }
  if (!t366) {
    u_no_case(s365);
  }
  return t366;
}

u_value f_check_46_constructor_scheme_1(u_value v_k) {
  u_value s372 = v_k;
  u_value t373 = NULL;
  if (!t373) {
    u_value v_ps = u_field(s372, 0);
    u_value v_ks = u_field(s372, 1);
    u_value v_r = u_field(s372, 2);
    u_value t374;
    if (u_truth(u_eq(v_ks, u_mk_con(&n_list_46_Nil, 0)))) {
      t374 = v_r;
    } else {
      t374 = f_check_46_fun_2(v_ks, v_r);
    }
    t373 = u_mk_tuple(2, v_ps, t374);
  }
  if (!t373) {
    u_no_case(s372);
  }
  return t373;
}

u_value f_check_46_constructor_of_2(u_value v_c, u_value v_k) {
  u_value s375 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t376 = NULL;
  if ((!t376) && u_is_con(s375, &n_option_46_Some)) {
    u_value v_kc = u_field(s375, 0);
    t376 = f_check_46_constructor_scheme_1(v_kc);
  }
  if ((!t376) && u_is_con(s375, &n_option_46_None)) {
    t376 = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t376) {
    u_no_case(s375);
  }
  return t376;
}

u_value f_check_46_candidates_3(u_value v_s, u_value v_env, u_value v_n) {
  u_value s377 = f_list_46_lookup_2(v_n, v_env);
  u_value t378 = NULL;
  if ((!t378) && u_is_con(s377, &n_option_46_Some)) {
    u_value v_sc = u_field(s377, 0);
    t378 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, v_sc), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t378) && u_is_con(s377, &n_option_46_None)) {
    u_value t379 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e380 = &t379;
    u_value l382 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam381, 1, v_s));
    while (u_is_con(l382, &n_list_46_Cons)) {
      u_value v_f = u_field(l382, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t383 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e384 = &t383;
        u_value l385 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(v_s), v_f);
        while (u_is_con(l385, &n_list_46_Cons)) {
          u_value v_sc = u_field(l385, 0);
          if (u_truth(u_mk_bool(1))) {
            e384 = u_append(e384, u_mk_tuple(2, v_f, v_sc));
          }
          l385 = u_field(l385, 1);
        }
        e380 = u_append(e380, t383);
      }
      l382 = u_field(l382, 1);
    }
    u_value t386 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e387 = &t386;
    u_value l389 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam388, 1, v_s));
    while (u_is_con(l389, &n_list_46_Cons)) {
      u_value v_k = u_field(l389, 0);
      if (u_truth(u_mk_bool(1))) {
        e387 = u_append(e387, u_mk_tuple(2, v_k, f_check_46_constructor_of_2(f_check_46_State_46_ctx_1(v_s), v_k)));
      }
      l389 = u_field(l389, 1);
    }
    u_value t390 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e391 = &t390;
    u_value l393 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam392, 1, v_s));
    while (u_is_con(l393, &n_list_46_Cons)) {
      u_value v_p = u_field(l393, 0);
      if (u_truth(u_mk_bool(1))) {
        e391 = u_append(e391, u_mk_tuple(2, v_p, f_check_46_proc_scheme_2(f_check_46_State_46_ctx_1(v_s), v_p)));
      }
      l393 = u_field(l393, 1);
    }
    u_value t394 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e395 = &t394;
    u_value l396 = f_check_46_builtin_1(v_n);
    while (u_is_con(l396, &n_list_46_Cons)) {
      u_value v_sc = u_field(l396, 0);
      if (u_truth(u_mk_bool(1))) {
        e395 = u_append(e395, u_mk_tuple(2, v_n, v_sc));
      }
      l396 = u_field(l396, 1);
    }
    t378 = f_list_46__43__43__2(f_list_46_concat_1(t379), f_list_46__43__43__2(t386, f_list_46__43__43__2(t390, t394)));
  }
  if (!t378) {
    u_no_case(s377);
  }
  return t378;
}

u_value f_check_46_proc_scheme_2(u_value v_c, u_value v_p) {
  u_value s397 = f_ctx_46_proc_of_2(v_c, v_p);
  u_value t398 = NULL;
  if ((!t398) && u_is_con(s397, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s397, 0), 0);
    u_value v_is = u_field(u_field(s397, 0), 1);
    u_value v_os = u_field(u_field(s397, 0), 2);
    u_value v_rs = u_field(u_field(s397, 0), 3);
    t398 = u_mk_tuple(2, v_ps, u_mk_con(&n_syntax_46_TProc, 3, v_is, v_os, v_rs));
  }
  if ((!t398) && u_is_con(s397, &n_option_46_None)) {
    t398 = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_syntax_46_THole, 0));
  }
  if (!t398) {
    u_no_case(s397);
  }
  return t398;
}

u_value f_check_46_with_arity_2(u_value v_cs, u_value v_k) {
  u_value s399 = v_cs;
  u_value t400 = NULL;
  if ((!t400) && u_is_con(s399, &n_list_46_Nil)) {
    t400 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t400) && u_is_con(s399, &n_list_46_Cons)) {
    u_value v_n = u_field(u_field(s399, 0), 0);
    u_value v_ps = u_field(u_field(u_field(s399, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s399, 0), 1), 1);
    u_value v_rest = u_field(s399, 1);
    u_value t401;
    if (u_truth(u_eq(f_syntax_46_arity_1(v_t), v_k))) {
      t401 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, v_ps, v_t)), f_check_46_with_arity_2(v_rest, v_k));
    } else {
      t401 = f_check_46_with_arity_2(v_rest, v_k);
    }
    t400 = t401;
  }
  if (!t400) {
    u_no_case(s399);
  }
  return t400;
}

u_value f_check_46_has_arity_2(u_value v_scs, u_value v_k) {
  u_value s402 = v_scs;
  u_value t403 = NULL;
  if ((!t403) && u_is_con(s402, &n_list_46_Nil)) {
    t403 = u_mk_bool(0);
  }
  if ((!t403) && u_is_con(s402, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s402, 0), 1);
    u_value v_rest = u_field(s402, 1);
    u_value t404 = u_eq(f_syntax_46_arity_1(v_t), v_k);
    if (!u_truth(t404)) {
      t404 = f_check_46_has_arity_2(v_rest, v_k);
    }
    t403 = t404;
  }
  if (!t403) {
    u_no_case(s402);
  }
  return t403;
}

u_value f_check_46_bind_1(u_value v_xs) {
  u_value t405 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e406 = &t405;
  u_value l407 = v_xs;
  while (u_is_con(l407, &n_list_46_Cons)) {
    u_value v_x = u_field(l407, 0);
    if (u_truth(u_mk_bool(1))) {
      e406 = u_append(e406, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_syntax_46_Var_46_typ_1(v_x))));
    }
    l407 = u_field(l407, 1);
  }
  return t405;
}

u_value f_check_46_fixed_2(u_value v_su, u_value v_env) {
  u_value s408 = v_env;
  u_value t409 = NULL;
  if ((!t409) && u_is_con(s408, &n_list_46_Nil)) {
    t409 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t409) && u_is_con(s408, &n_list_46_Cons)) {
    u_value v_ps = u_field(u_field(u_field(s408, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s408, 0), 1), 1);
    u_value v_rest = u_field(s408, 1);
    t409 = f_list_46__43__43__2(f_list_46_diff_2(f_check_46_free_1(f_check_46_zonk_type_2(v_su, v_t)), v_ps), f_check_46_fixed_2(v_su, v_rest));
  }
  if (!t409) {
    u_no_case(s408);
  }
  return t409;
}

u_value f_check_46_generalize_3(u_value v_su, u_value v_env, u_value v_t) {
  u_value t415 = f_check_46_fixed_2(v_su, v_env);
  u_value t416 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t414;
  {
    u_value v_fx = t415;
    u_value v_u = t416;
    u_value t410 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e411 = &t410;
    u_value l412 = f_check_46_free_1(v_u);
    while (u_is_con(l412, &n_list_46_Cons)) {
      u_value v_m = u_field(l412, 0);
      u_value t413 = f_check_46_is_meta_1(v_m);
      if (u_truth(t413)) {
        t413 = f_prelude_46_not_1(f_list_46_contains_2(v_m, v_fx));
      }
      if (u_truth(t413)) {
        e411 = u_append(e411, v_m);
      }
      l412 = u_field(l412, 1);
    }
    t414 = u_mk_tuple(2, t410, v_u);
  }
  return t414;
}

u_value f_check_46_generalized_3(u_value v_su, u_value v_env, u_value v_xs) {
  u_value t417 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e418 = &t417;
  u_value l419 = v_xs;
  while (u_is_con(l419, &n_list_46_Cons)) {
    u_value v_x = u_field(l419, 0);
    if (u_truth(u_mk_bool(1))) {
      e418 = u_append(e418, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), f_check_46_generalize_3(v_su, v_env, f_syntax_46_Var_46_typ_1(v_x))));
    }
    l419 = u_field(l419, 1);
  }
  return t417;
}

u_value f_check_46_case_of_1(u_value v_b) {
  u_value s420 = v_b;
  u_value t421 = NULL;
  if ((!t421) && u_is_con(s420, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s420, 0);
    u_value v_g = u_field(s420, 1);
    t421 = u_mk_tuple(2, v_p, u_eq(v_g, u_mk_con(&n_option_46_None, 0)));
  }
  if (!t421) {
    u_no_case(s420);
  }
  return t421;
}

u_value f_check_46_zonk_guard_2(u_value v_su, u_value v_g) {
  u_value s422 = v_g;
  u_value t423 = NULL;
  if ((!t423) && u_is_con(s422, &n_option_46_Some)) {
    u_value v_e = u_field(s422, 0);
    t423 = u_mk_con(&n_option_46_Some, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t423) && u_is_con(s422, &n_option_46_None)) {
    t423 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t423) {
    u_no_case(s422);
  }
  return t423;
}

u_value f_check_46_zonk_type_2(u_value v_su, u_value v_t) {
  u_value s424 = f_check_46_resolve_2(v_su, v_t);
  u_value t425 = NULL;
  if ((!t425) && u_is_con(s424, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s424, 0);
    u_value v_args = u_field(s424, 1);
    u_value t426 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e427 = &t426;
    u_value l428 = v_args;
    while (u_is_con(l428, &n_list_46_Cons)) {
      u_value v_a = u_field(l428, 0);
      if (u_truth(u_mk_bool(1))) {
        e427 = u_append(e427, f_check_46_zonk_type_2(v_su, v_a));
      }
      l428 = u_field(l428, 1);
    }
    t425 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t426);
  }
  if ((!t425) && u_is_con(s424, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s424, 0);
    u_value v_r = u_field(s424, 1);
    u_value t429 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e430 = &t429;
    u_value l431 = v_args;
    while (u_is_con(l431, &n_list_46_Cons)) {
      u_value v_a = u_field(l431, 0);
      if (u_truth(u_mk_bool(1))) {
        e430 = u_append(e430, f_check_46_zonk_type_2(v_su, v_a));
      }
      l431 = u_field(l431, 1);
    }
    t425 = u_mk_con(&n_syntax_46_TFun, 2, t429, f_check_46_zonk_type_2(v_su, v_r));
  }
  if ((!t425) && u_is_con(s424, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s424, 0);
    u_value v_os = u_field(s424, 1);
    u_value v_rs = u_field(s424, 2);
    u_value t432 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e433 = &t432;
    u_value l434 = v_is;
    while (u_is_con(l434, &n_list_46_Cons)) {
      u_value v_a = u_field(l434, 0);
      if (u_truth(u_mk_bool(1))) {
        e433 = u_append(e433, f_check_46_zonk_type_2(v_su, v_a));
      }
      l434 = u_field(l434, 1);
    }
    u_value t435 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e436 = &t435;
    u_value l437 = v_os;
    while (u_is_con(l437, &n_list_46_Cons)) {
      u_value v_a = u_field(l437, 0);
      if (u_truth(u_mk_bool(1))) {
        e436 = u_append(e436, f_check_46_zonk_type_2(v_su, v_a));
      }
      l437 = u_field(l437, 1);
    }
    u_value t438 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e439 = &t438;
    u_value l440 = v_rs;
    while (u_is_con(l440, &n_list_46_Cons)) {
      u_value v_a = u_field(l440, 0);
      if (u_truth(u_mk_bool(1))) {
        e439 = u_append(e439, f_check_46_zonk_type_2(v_su, v_a));
      }
      l440 = u_field(l440, 1);
    }
    t425 = u_mk_con(&n_syntax_46_TProc, 3, t432, t435, t438);
  }
  if ((!t425) && u_is_con(s424, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s424, 0);
    u_value t441 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e442 = &t441;
    u_value l443 = v_ts;
    while (u_is_con(l443, &n_list_46_Cons)) {
      u_value v_a = u_field(l443, 0);
      if (u_truth(u_mk_bool(1))) {
        e442 = u_append(e442, f_check_46_zonk_type_2(v_su, v_a));
      }
      l443 = u_field(l443, 1);
    }
    t425 = u_mk_con(&n_syntax_46_TTuple, 1, t441);
  }
  if (!t425) {
    u_value v_u = s424;
    t425 = v_u;
  }
  if (!t425) {
    u_no_case(s424);
  }
  return t425;
}

u_value f_check_46_zonk_var_2(u_value v_su, u_value v_x) {
  return f_syntax_46_Var_46_typ_58__61__2(v_x, f_check_46_zonk_type_2(v_su, f_syntax_46_Var_46_typ_1(v_x)));
}

u_value f_check_46_zonk_vars_2(u_value v_su, u_value v_xs) {
  u_value t444 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e445 = &t444;
  u_value l446 = v_xs;
  while (u_is_con(l446, &n_list_46_Cons)) {
    u_value v_x = u_field(l446, 0);
    if (u_truth(u_mk_bool(1))) {
      e445 = u_append(e445, f_check_46_zonk_var_2(v_su, v_x));
    }
    l446 = u_field(l446, 1);
  }
  return t444;
}

u_value f_check_46_zonk_expr_2(u_value v_su, u_value v_e) {
  u_value s447 = v_e;
  u_value t448 = NULL;
  if ((!t448) && u_is_con(s447, &n_syntax_46_ELit)) {
    t448 = v_e;
  }
  if ((!t448) && u_is_con(s447, &n_syntax_46_EVar)) {
    u_value v_x = u_field(s447, 0);
    t448 = u_mk_con(&n_syntax_46_EVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t448) && u_is_con(s447, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s447, 0);
    u_value v_args = u_field(s447, 1);
    u_value t449 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e450 = &t449;
    u_value l451 = v_args;
    while (u_is_con(l451, &n_list_46_Cons)) {
      u_value v_a = u_field(l451, 0);
      if (u_truth(u_mk_bool(1))) {
        e450 = u_append(e450, f_check_46_zonk_expr_2(v_su, v_a));
      }
      l451 = u_field(l451, 1);
    }
    t448 = u_mk_con(&n_syntax_46_EApp, 2, f_check_46_zonk_expr_2(v_su, v_f), t449);
  }
  if ((!t448) && u_is_con(s447, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s447, 0);
    u_value v_b = u_field(s447, 1);
    t448 = u_mk_con(&n_syntax_46_ELam, 2, f_check_46_zonk_vars_2(v_su, v_xs), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t448) && u_is_con(s447, &n_syntax_46_EProc)) {
    u_value v_is = u_field(s447, 0);
    u_value v_os = u_field(s447, 1);
    u_value v_rs = u_field(s447, 2);
    u_value v_b = u_field(s447, 3);
    t448 = u_mk_con(&n_syntax_46_EProc, 4, f_check_46_zonk_vars_2(v_su, v_is), f_check_46_zonk_vars_2(v_su, v_os), f_check_46_zonk_vars_2(v_su, v_rs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t448) && u_is_con(s447, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s447, 0);
    u_value v_b = u_field(s447, 1);
    u_value t452 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e453 = &t452;
    u_value l454 = v_ps;
    while (u_is_con(l454, &n_list_46_Cons)) {
      u_value v_p = u_field(l454, 0);
      if (u_truth(u_mk_bool(1))) {
        e453 = u_append(e453, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l454 = u_field(l454, 1);
    }
    t448 = u_mk_con(&n_syntax_46_ELet, 2, t452, f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t448) && u_is_con(s447, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s447, 0);
    u_value v_l = u_field(s447, 1);
    u_value v_r = u_field(s447, 2);
    t448 = u_mk_con(&n_syntax_46_EIte, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_expr_2(v_su, v_l), f_check_46_zonk_expr_2(v_su, v_r));
  }
  if ((!t448) && u_is_con(s447, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s447, 0);
    u_value v_bs = u_field(s447, 1);
    u_value t455 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e456 = &t455;
    u_value l457 = v_bs;
    while (u_is_con(l457, &n_list_46_Cons)) {
      u_value v_b = u_field(l457, 0);
      if (u_truth(u_mk_bool(1))) {
        e456 = u_append(e456, f_check_46_zonk_expr_branch_2(v_su, v_b));
      }
      l457 = u_field(l457, 1);
    }
    t448 = u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t455);
  }
  if ((!t448) && u_is_con(s447, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s447, 0);
    u_value v_xs = u_field(s447, 1);
    u_value v_ls = u_field(s447, 2);
    u_value v_c = u_field(s447, 3);
    u_value t458 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e459 = &t458;
    u_value l460 = v_ls;
    while (u_is_con(l460, &n_list_46_Cons)) {
      u_value v_l = u_field(l460, 0);
      if (u_truth(u_mk_bool(1))) {
        e459 = u_append(e459, f_check_46_zonk_expr_2(v_su, v_l));
      }
      l460 = u_field(l460, 1);
    }
    t448 = u_mk_con(&n_syntax_46_EComp, 4, f_check_46_zonk_expr_2(v_su, v_r), f_check_46_zonk_vars_2(v_su, v_xs), t458, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t448) && u_is_con(s447, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s447, 0);
    u_value t461 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e462 = &t461;
    u_value l463 = v_es;
    while (u_is_con(l463, &n_list_46_Cons)) {
      u_value v_e = u_field(l463, 0);
      if (u_truth(u_mk_bool(1))) {
        e462 = u_append(e462, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l463 = u_field(l463, 1);
    }
    t448 = u_mk_con(&n_syntax_46_ETuple, 1, t461);
  }
  if ((!t448) && u_is_con(s447, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s447, 0);
    u_value v_t = u_field(s447, 1);
    t448 = u_mk_con(&n_syntax_46_ERaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t448) {
    u_no_case(s447);
  }
  return t448;
}

u_value f_check_46_zonk_binding_2(u_value v_su, u_value v_p) {
  u_value s464 = v_p;
  u_value t465 = NULL;
  if (!t465) {
    u_value v_x = u_field(s464, 0);
    u_value v_e = u_field(s464, 1);
    t465 = u_mk_tuple(2, f_check_46_zonk_var_2(v_su, v_x), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t465) {
    u_no_case(s464);
  }
  return t465;
}

u_value f_check_46_zonk_expr_branch_2(u_value v_su, u_value v_b) {
  u_value s466 = v_b;
  u_value t467 = NULL;
  if ((!t467) && u_is_con(s466, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s466, 0);
    u_value v_g = u_field(s466, 1);
    u_value v_e = u_field(s466, 2);
    t467 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t467) {
    u_no_case(s466);
  }
  return t467;
}

u_value f_check_46_zonk_pat_2(u_value v_su, u_value v_p) {
  u_value s468 = v_p;
  u_value t469 = NULL;
  if ((!t469) && u_is_con(s468, &n_syntax_46_PVar)) {
    u_value v_x = u_field(s468, 0);
    t469 = u_mk_con(&n_syntax_46_PVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t469) && u_is_con(s468, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s468, 0);
    u_value v_args = u_field(s468, 1);
    u_value t470 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e471 = &t470;
    u_value l472 = v_args;
    while (u_is_con(l472, &n_list_46_Cons)) {
      u_value v_a = u_field(l472, 0);
      if (u_truth(u_mk_bool(1))) {
        e471 = u_append(e471, f_check_46_zonk_pat_2(v_su, v_a));
      }
      l472 = u_field(l472, 1);
    }
    t469 = u_mk_con(&n_syntax_46_PApp, 2, v_n, t470);
  }
  if ((!t469) && u_is_con(s468, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s468, 0);
    u_value t473 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e474 = &t473;
    u_value l475 = v_ps;
    while (u_is_con(l475, &n_list_46_Cons)) {
      u_value v_q = u_field(l475, 0);
      if (u_truth(u_mk_bool(1))) {
        e474 = u_append(e474, f_check_46_zonk_pat_2(v_su, v_q));
      }
      l475 = u_field(l475, 1);
    }
    t469 = u_mk_con(&n_syntax_46_PTuple, 1, t473);
  }
  if (!t469) {
    t469 = v_p;
  }
  if (!t469) {
    u_no_case(s468);
  }
  return t469;
}

u_value f_check_46_zonk_stmt_2(u_value v_su, u_value v_x) {
  u_value s476 = v_x;
  u_value t477 = NULL;
  if ((!t477) && u_is_con(s476, &n_syntax_46_SSkip)) {
    t477 = v_x;
  }
  if ((!t477) && u_is_con(s476, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s476, 0);
    u_value t478 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e479 = &t478;
    u_value l480 = v_ps;
    while (u_is_con(l480, &n_list_46_Cons)) {
      u_value v_p = u_field(l480, 0);
      if (u_truth(u_mk_bool(1))) {
        e479 = u_append(e479, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l480 = u_field(l480, 1);
    }
    t477 = u_mk_con(&n_syntax_46_SAssign, 1, t478);
  }
  if ((!t477) && u_is_con(s476, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s476, 0);
    u_value v_ys = u_field(s476, 1);
    u_value t481 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e482 = &t481;
    u_value l483 = v_ys;
    while (u_is_con(l483, &n_list_46_Cons)) {
      u_value v_y = u_field(l483, 0);
      if (u_truth(u_mk_bool(1))) {
        e482 = u_append(e482, f_check_46_zonk_stmt_2(v_su, v_y));
      }
      l483 = u_field(l483, 1);
    }
    t477 = u_mk_con(&n_syntax_46_SBlock, 2, f_check_46_zonk_vars_2(v_su, v_xs), t481);
  }
  if ((!t477) && u_is_con(s476, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s476, 0);
    u_value v_l = u_field(s476, 1);
    u_value v_r = u_field(s476, 2);
    t477 = u_mk_con(&n_syntax_46_SIf, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_l), f_check_46_zonk_stmt_2(v_su, v_r));
  }
  if ((!t477) && u_is_con(s476, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s476, 0);
    u_value v_b = u_field(s476, 1);
    t477 = u_mk_con(&n_syntax_46_SWhile, 2, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t477) && u_is_con(s476, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s476, 0);
    u_value v_ins = u_field(s476, 1);
    u_value v_outs = u_field(s476, 2);
    u_value v_refs = u_field(s476, 3);
    u_value t484 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e485 = &t484;
    u_value l486 = v_ins;
    while (u_is_con(l486, &n_list_46_Cons)) {
      u_value v_e = u_field(l486, 0);
      if (u_truth(u_mk_bool(1))) {
        e485 = u_append(e485, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l486 = u_field(l486, 1);
    }
    t477 = u_mk_con(&n_syntax_46_SCall, 4, v_p, t484, f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs));
  }
  if ((!t477) && u_is_con(s476, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s476, 0);
    u_value v_bs = u_field(s476, 1);
    u_value t487 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e488 = &t487;
    u_value l489 = v_bs;
    while (u_is_con(l489, &n_list_46_Cons)) {
      u_value v_b = u_field(l489, 0);
      if (u_truth(u_mk_bool(1))) {
        e488 = u_append(e488, f_check_46_zonk_stmt_branch_2(v_su, v_b));
      }
      l489 = u_field(l489, 1);
    }
    t477 = u_mk_con(&n_syntax_46_SMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t487);
  }
  if ((!t477) && u_is_con(s476, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s476, 0);
    t477 = u_mk_con(&n_syntax_46_SAssert, 1, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t477) && u_is_con(s476, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s476, 0);
    u_value v_t = u_field(s476, 1);
    t477 = u_mk_con(&n_syntax_46_SRaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if ((!t477) && u_is_con(s476, &n_syntax_46_STry)) {
    u_value v_b = u_field(s476, 0);
    u_value v_hs = u_field(s476, 1);
    u_value t490 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e491 = &t490;
    u_value l492 = v_hs;
    while (u_is_con(l492, &n_list_46_Cons)) {
      u_value v_h = u_field(l492, 0);
      if (u_truth(u_mk_bool(1))) {
        e491 = u_append(e491, f_check_46_zonk_handler_2(v_su, v_h));
      }
      l492 = u_field(l492, 1);
    }
    t477 = u_mk_con(&n_syntax_46_STry, 2, f_check_46_zonk_stmt_2(v_su, v_b), t490);
  }
  if (!t477) {
    u_no_case(s476);
  }
  return t477;
}

u_value f_check_46_zonk_stmt_branch_2(u_value v_su, u_value v_b) {
  u_value s493 = v_b;
  u_value t494 = NULL;
  if ((!t494) && u_is_con(s493, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s493, 0);
    u_value v_g = u_field(s493, 1);
    u_value v_y = u_field(s493, 2);
    t494 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_y));
  }
  if (!t494) {
    u_no_case(s493);
  }
  return t494;
}

u_value f_check_46_zonk_handler_2(u_value v_su, u_value v_h) {
  u_value s495 = v_h;
  u_value t496 = NULL;
  if ((!t496) && u_is_con(s495, &n_syntax_46_Handler)) {
    u_value v_t = u_field(s495, 0);
    u_value v_p = u_field(s495, 1);
    u_value v_g = u_field(s495, 2);
    u_value v_b = u_field(s495, 3);
    t496 = u_mk_con(&n_syntax_46_Handler, 4, f_check_46_zonk_type_2(v_su, v_t), f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t496) {
    u_no_case(s495);
  }
  return t496;
}

u_value f_check_46_zonk_cmd_2(u_value v_su, u_value v_c) {
  u_value s497 = v_c;
  u_value t498 = NULL;
  if ((!t498) && u_is_con(s497, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s497, 0);
    u_value v_ps = u_field(s497, 1);
    u_value v_xss = u_field(s497, 2);
    u_value v_r = u_field(s497, 3);
    u_value v_b = u_field(s497, 4);
    u_value t499 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e500 = &t499;
    u_value l501 = v_xss;
    while (u_is_con(l501, &n_list_46_Cons)) {
      u_value v_xs = u_field(l501, 0);
      if (u_truth(u_mk_bool(1))) {
        e500 = u_append(e500, f_check_46_zonk_vars_2(v_su, v_xs));
      }
      l501 = u_field(l501, 1);
    }
    t498 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, t499, f_check_46_zonk_type_2(v_su, v_r), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t498) && u_is_con(s497, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s497, 0);
    u_value v_ps = u_field(s497, 1);
    u_value v_ins = u_field(s497, 2);
    u_value v_outs = u_field(s497, 3);
    u_value v_refs = u_field(s497, 4);
    u_value v_b = u_field(s497, 5);
    t498 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, f_check_46_zonk_vars_2(v_su, v_ins), f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t498) && u_is_con(s497, &n_syntax_46_CRun)) {
    u_value v_n = u_field(s497, 0);
    u_value v_b = u_field(s497, 1);
    t498 = u_mk_con(&n_syntax_46_CRun, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t498) && u_is_con(s497, &n_syntax_46_CTest)) {
    u_value v_n = u_field(s497, 0);
    u_value v_b = u_field(s497, 1);
    t498 = u_mk_con(&n_syntax_46_CTest, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t498) {
    t498 = v_c;
  }
  if (!t498) {
    u_no_case(s497);
  }
  return t498;
}

u_value f_check_46_unknown_type_1(u_value v_t) {
  u_value t502;
  if (u_truth(u_eq(f_check_46_free_1(v_t), u_mk_con(&n_list_46_Nil, 0)))) {
    t502 = u_mk_con(&n_option_46_None, 0);
  } else {
    t502 = u_mk_con(&n_option_46_Some, 1, v_t);
  }
  return t502;
}

u_value f_check_46_unknown_in_1(u_value v_x) {
  u_value s503 = v_x;
  u_value t504 = NULL;
  if ((!t504) && u_is_con(s503, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s503, 0);
    t504 = f_check_46_unknown_in_exprs_1(f_list_46_seconds_1(v_ps));
  }
  if ((!t504) && u_is_con(s503, &n_syntax_46_SBlock)) {
    u_value v_ys = u_field(s503, 1);
    t504 = f_check_46_unknown_in_all_1(v_ys);
  }
  if ((!t504) && u_is_con(s503, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s503, 0);
    u_value v_l = u_field(s503, 1);
    u_value v_r = u_field(s503, 2);
    t504 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_option_46_or_else_2(f_check_46_unknown_in_1(v_l), f_check_46_unknown_in_1(v_r)));
  }
  if ((!t504) && u_is_con(s503, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s503, 0);
    u_value v_b = u_field(s503, 1);
    t504 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_check_46_unknown_in_1(v_b));
  }
  if ((!t504) && u_is_con(s503, &n_syntax_46_SCall)) {
    u_value v_ins = u_field(s503, 1);
    t504 = f_check_46_unknown_in_exprs_1(v_ins);
  }
  if ((!t504) && u_is_con(s503, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s503, 0);
    u_value v_bs = u_field(s503, 1);
    t504 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_a), f_check_46_unknown_in_branches_1(v_bs));
  }
  if ((!t504) && u_is_con(s503, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s503, 0);
    t504 = f_check_46_unknown_in_expr_1(v_c);
  }
  if ((!t504) && u_is_con(s503, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s503, 0);
    u_value v_t = u_field(s503, 1);
    t504 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t504) && u_is_con(s503, &n_syntax_46_STry)) {
    u_value v_b = u_field(s503, 0);
    u_value v_hs = u_field(s503, 1);
    t504 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_hs));
  }
  if (!t504) {
    t504 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t504) {
    u_no_case(s503);
  }
  return t504;
}

u_value f_check_46_unknown_in_expr_1(u_value v_e) {
  u_value s505 = v_e;
  u_value t506 = NULL;
  if ((!t506) && u_is_con(s505, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s505, 0);
    u_value v_t = u_field(s505, 1);
    t506 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t506) && u_is_con(s505, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s505, 0);
    u_value v_args = u_field(s505, 1);
    t506 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_f, v_args));
  }
  if ((!t506) && u_is_con(s505, &n_syntax_46_ELam)) {
    u_value v_b = u_field(s505, 1);
    t506 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t506) && u_is_con(s505, &n_syntax_46_EProc)) {
    u_value v_b = u_field(s505, 3);
    t506 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t506) && u_is_con(s505, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s505, 0);
    u_value v_b = u_field(s505, 1);
    t506 = f_check_46_unknown_in_exprs_1(f_list_46__43__43__2(f_list_46_seconds_1(v_ps), u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t506) && u_is_con(s505, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s505, 0);
    u_value v_l = u_field(s505, 1);
    u_value v_r = u_field(s505, 2);
    t506 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)))));
  }
  if ((!t506) && u_is_con(s505, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s505, 0);
    u_value v_bs = u_field(s505, 1);
    u_value t507 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e508 = &t507;
    u_value l509 = v_bs;
    while (u_is_con(l509, &n_list_46_Cons)) {
      u_value v_b = u_field(l509, 0);
      if (u_truth(u_mk_bool(1))) {
        e508 = u_append(e508, f_check_46_branch_exprs_1(v_b));
      }
      l509 = u_field(l509, 1);
    }
    t506 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_a, f_list_46_concat_1(t507)));
  }
  if ((!t506) && u_is_con(s505, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s505, 0);
    u_value v_ls = u_field(s505, 2);
    u_value v_c = u_field(s505, 3);
    t506 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Cons, 2, v_c, v_ls)));
  }
  if ((!t506) && u_is_con(s505, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s505, 0);
    t506 = f_check_46_unknown_in_exprs_1(v_es);
  }
  if (!t506) {
    t506 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t506) {
    u_no_case(s505);
  }
  return t506;
}

u_value f_check_46_unknown_in_exprs_1(u_value v_es) {
  u_value s510 = v_es;
  u_value t511 = NULL;
  if ((!t511) && u_is_con(s510, &n_list_46_Nil)) {
    t511 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t511) && u_is_con(s510, &n_list_46_Cons)) {
    u_value v_e = u_field(s510, 0);
    u_value v_rest = u_field(s510, 1);
    t511 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_e), f_check_46_unknown_in_exprs_1(v_rest));
  }
  if (!t511) {
    u_no_case(s510);
  }
  return t511;
}

u_value f_check_46_branch_exprs_1(u_value v_b) {
  u_value s512 = v_b;
  u_value t513 = NULL;
  if ((!t513) && (u_is_con(s512, &n_syntax_46_Branch) && u_is_con(u_field(s512, 1), &n_option_46_Some))) {
    u_value v_g = u_field(u_field(s512, 1), 0);
    u_value v_e = u_field(s512, 2);
    t513 = u_mk_con(&n_list_46_Cons, 2, v_g, u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t513) && (u_is_con(s512, &n_syntax_46_Branch) && u_is_con(u_field(s512, 1), &n_option_46_None))) {
    u_value v_e = u_field(s512, 2);
    t513 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t513) {
    u_no_case(s512);
  }
  return t513;
}

u_value f_check_46_unknown_in_all_1(u_value v_xs) {
  u_value s514 = v_xs;
  u_value t515 = NULL;
  if ((!t515) && u_is_con(s514, &n_list_46_Nil)) {
    t515 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t515) && u_is_con(s514, &n_list_46_Cons)) {
    u_value v_y = u_field(s514, 0);
    u_value v_ys = u_field(s514, 1);
    t515 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_all_1(v_ys));
  }
  if (!t515) {
    u_no_case(s514);
  }
  return t515;
}

u_value f_check_46_unknown_in_branches_1(u_value v_bs) {
  u_value s516 = v_bs;
  u_value t517 = NULL;
  if ((!t517) && u_is_con(s516, &n_list_46_Nil)) {
    t517 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t517) && (u_is_con(s516, &n_list_46_Cons) && (u_is_con(u_field(s516, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s516, 0), 1), &n_option_46_Some)))) {
    u_value v_g = u_field(u_field(u_field(s516, 0), 1), 0);
    u_value v_y = u_field(u_field(s516, 0), 2);
    u_value v_rest = u_field(s516, 1);
    t517 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_g), f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest)));
  }
  if ((!t517) && (u_is_con(s516, &n_list_46_Cons) && (u_is_con(u_field(s516, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s516, 0), 1), &n_option_46_None)))) {
    u_value v_y = u_field(u_field(s516, 0), 2);
    u_value v_rest = u_field(s516, 1);
    t517 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest));
  }
  if (!t517) {
    u_no_case(s516);
  }
  return t517;
}

u_value f_check_46_unknown_in_handlers_1(u_value v_hs) {
  u_value s518 = v_hs;
  u_value t519 = NULL;
  if ((!t519) && u_is_con(s518, &n_list_46_Nil)) {
    t519 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t519) && (u_is_con(s518, &n_list_46_Cons) && (u_is_con(u_field(s518, 0), &n_syntax_46_Handler) && u_is_con(u_field(u_field(s518, 0), 1), &n_syntax_46_PWild)))) {
    u_value v_b = u_field(u_field(s518, 0), 3);
    u_value v_rest = u_field(s518, 1);
    t519 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest));
  }
  if ((!t519) && (u_is_con(s518, &n_list_46_Cons) && u_is_con(u_field(s518, 0), &n_syntax_46_Handler))) {
    u_value v_t = u_field(u_field(s518, 0), 0);
    u_value v_b = u_field(u_field(s518, 0), 3);
    u_value v_rest = u_field(s518, 1);
    t519 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest)));
  }
  if (!t519) {
    u_no_case(s518);
  }
  return t519;
}

u_value f_check_46_unknown_in_cmd_1(u_value v_c) {
  u_value s520 = v_c;
  u_value t521 = NULL;
  if ((!t521) && u_is_con(s520, &n_syntax_46_CFun)) {
    u_value v_b = u_field(s520, 4);
    t521 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t521) && u_is_con(s520, &n_syntax_46_CProc)) {
    u_value v_b = u_field(s520, 5);
    t521 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t521) && u_is_con(s520, &n_syntax_46_CRun)) {
    u_value v_b = u_field(s520, 1);
    t521 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t521) && u_is_con(s520, &n_syntax_46_CTest)) {
    u_value v_b = u_field(s520, 1);
    t521 = f_check_46_unknown_in_1(v_b);
  }
  if (!t521) {
    t521 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t521) {
    u_no_case(s520);
  }
  return t521;
}

u_value f_check_46_head_of_1(u_value v_p) {
  u_value s522 = v_p;
  u_value t523 = NULL;
  if ((!t523) && u_is_con(s522, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s522, 0);
    u_value v_args = u_field(s522, 1);
    t523 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_n), v_args));
  }
  if ((!t523) && u_is_con(s522, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s522, 0);
    t523 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ps));
  }
  if ((!t523) && u_is_con(s522, &n_syntax_46_PLit)) {
    u_value v_v = u_field(s522, 0);
    t523 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, v_v), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t523) {
    t523 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t523) {
    u_no_case(s522);
  }
  return t523;
}

u_value f_check_46_heads_1(u_value v_rows) {
  u_value s524 = v_rows;
  u_value t525 = NULL;
  if ((!t525) && u_is_con(s524, &n_list_46_Nil)) {
    t525 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t525) && (u_is_con(s524, &n_list_46_Cons) && u_is_con(u_field(s524, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s524, 0), 0);
    u_value v_rest = u_field(s524, 1);
    u_value s526 = f_check_46_head_of_1(v_p);
    u_value t527 = NULL;
    if ((!t527) && u_is_con(s526, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s526, 0), 0);
      t527 = u_mk_con(&n_list_46_Cons, 2, v_h, f_check_46_heads_1(v_rest));
    }
    if ((!t527) && u_is_con(s526, &n_option_46_None)) {
      t527 = f_check_46_heads_1(v_rest);
    }
    if (!t527) {
      u_no_case(s526);
    }
    t525 = t527;
  }
  if ((!t525) && (u_is_con(s524, &n_list_46_Cons) && u_is_con(u_field(s524, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s524, 1);
    t525 = f_check_46_heads_1(v_rest);
  }
  if (!t525) {
    u_no_case(s524);
  }
  return t525;
}

u_value f_check_46_field_types_3(u_value v_c, u_value v_k, u_value v_args) {
  u_value s528 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t529 = NULL;
  if ((!t529) && u_is_con(s528, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s528, 0), 0);
    u_value v_ks = u_field(u_field(s528, 0), 1);
    u_value t530 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e531 = &t530;
    u_value l532 = v_ks;
    while (u_is_con(l532, &n_list_46_Cons)) {
      u_value v_a = u_field(l532, 0);
      if (u_truth(u_mk_bool(1))) {
        e531 = u_append(e531, f_check_46_subst_2(v_a, f_list_46_zip_2(v_ps, v_args)));
      }
      l532 = u_field(l532, 1);
    }
    t529 = t530;
  }
  if ((!t529) && u_is_con(s528, &n_option_46_None)) {
    t529 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t529) {
    u_no_case(s528);
  }
  return t529;
}

u_value f_check_46_constructors_of_3(u_value v_c, u_value v_su, u_value v_t) {
  u_value s533 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t534 = NULL;
  if ((!t534) && (u_is_con(s533, &n_syntax_46_TSort) && (u_is_con(u_field(s533, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s533, 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(s533, 0), 1), u_mk_lit("Bool")) && u_is_con(u_field(s533, 1), &n_list_46_Nil)))))) {
    t534 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t534) && u_is_con(s533, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s533, 0);
    t534 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ts), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t534) && u_is_con(s533, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s533, 0);
    u_value v_args = u_field(s533, 1);
    u_value s535 = f_ctx_46_members_of_2(v_c, v_n);
    u_value t536 = NULL;
    if ((!t536) && u_is_con(s535, &n_option_46_Some)) {
      u_value v_ks = u_field(s535, 0);
      u_value t537 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e538 = &t537;
      u_value l539 = v_ks;
      while (u_is_con(l539, &n_list_46_Cons)) {
        u_value v_k = u_field(l539, 0);
        if (u_truth(u_mk_bool(1))) {
          e538 = u_append(e538, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_k), f_check_46_field_types_3(v_c, v_k, v_args)));
        }
        l539 = u_field(l539, 1);
      }
      t536 = u_mk_con(&n_option_46_Some, 1, t537);
    }
    if ((!t536) && u_is_con(s535, &n_option_46_None)) {
      t536 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t536) {
      u_no_case(s535);
    }
    t534 = t536;
  }
  if (!t534) {
    t534 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t534) {
    u_no_case(s533);
  }
  return t534;
}

u_value f_check_46_fields_of_4(u_value v_c, u_value v_su, u_value v_t, u_value v_h) {
  u_value s540 = f_check_46_constructors_of_3(v_c, v_su, v_t);
  u_value t541 = NULL;
  if ((!t541) && u_is_con(s540, &n_option_46_Some)) {
    u_value v_ks = u_field(s540, 0);
    u_value s542 = f_list_46_lookup_2(v_h, v_ks);
    u_value t543 = NULL;
    if ((!t543) && u_is_con(s542, &n_option_46_Some)) {
      u_value v_ts = u_field(s542, 0);
      t543 = v_ts;
    }
    if ((!t543) && u_is_con(s542, &n_option_46_None)) {
      t543 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t543) {
      u_no_case(s542);
    }
    t541 = t543;
  }
  if ((!t541) && u_is_con(s540, &n_option_46_None)) {
    t541 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t541) {
    u_no_case(s540);
  }
  return t541;
}

u_value f_check_46_covers_2(u_value v_hs, u_value v_ks) {
  u_value s544 = v_ks;
  u_value t545 = NULL;
  if ((!t545) && u_is_con(s544, &n_list_46_Nil)) {
    t545 = u_mk_bool(1);
  }
  if ((!t545) && u_is_con(s544, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s544, 0), 0);
    u_value v_rest = u_field(s544, 1);
    u_value t546 = f_list_46_contains_2(v_h, v_hs);
    if (u_truth(t546)) {
      t546 = f_check_46_covers_2(v_hs, v_rest);
    }
    t545 = t546;
  }
  if (!t545) {
    u_no_case(s544);
  }
  return t545;
}

u_value f_check_46_defaults_1(u_value v_rows) {
  u_value s547 = v_rows;
  u_value t548 = NULL;
  if ((!t548) && u_is_con(s547, &n_list_46_Nil)) {
    t548 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t548) && (u_is_con(s547, &n_list_46_Cons) && u_is_con(u_field(s547, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s547, 0), 0);
    u_value v_ps = u_field(u_field(s547, 0), 1);
    u_value v_rest = u_field(s547, 1);
    u_value s549 = f_check_46_head_of_1(v_p);
    u_value t550 = NULL;
    if ((!t550) && u_is_con(s549, &n_option_46_Some)) {
      t550 = f_check_46_defaults_1(v_rest);
    }
    if ((!t550) && u_is_con(s549, &n_option_46_None)) {
      t550 = u_mk_con(&n_list_46_Cons, 2, v_ps, f_check_46_defaults_1(v_rest));
    }
    if (!t550) {
      u_no_case(s549);
    }
    t548 = t550;
  }
  if ((!t548) && (u_is_con(s547, &n_list_46_Cons) && u_is_con(u_field(s547, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s547, 1);
    t548 = f_check_46_defaults_1(v_rest);
  }
  if (!t548) {
    u_no_case(s547);
  }
  return t548;
}

u_value f_check_46_specialize_3(u_value v_rows, u_value v_h, u_value v_n) {
  u_value s551 = v_rows;
  u_value t552 = NULL;
  if ((!t552) && u_is_con(s551, &n_list_46_Nil)) {
    t552 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t552) && (u_is_con(s551, &n_list_46_Cons) && u_is_con(u_field(s551, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s551, 0), 0);
    u_value v_ps = u_field(u_field(s551, 0), 1);
    u_value v_rest = u_field(s551, 1);
    u_value s553 = f_check_46_head_of_1(v_p);
    u_value t554 = NULL;
    if ((!t554) && u_is_con(s553, &n_option_46_Some)) {
      u_value v_g = u_field(u_field(s553, 0), 0);
      u_value v_args = u_field(u_field(s553, 0), 1);
      u_value t555;
      if (u_truth(u_eq(v_g, v_h))) {
        t555 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(v_args, v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
      } else {
        t555 = f_check_46_specialize_3(v_rest, v_h, v_n);
      }
      t554 = t555;
    }
    if ((!t554) && u_is_con(s553, &n_option_46_None)) {
      t554 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(f_list_46_replicate_2(v_n, u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
    }
    if (!t554) {
      u_no_case(s553);
    }
    t552 = t554;
  }
  if ((!t552) && (u_is_con(s551, &n_list_46_Cons) && u_is_con(u_field(s551, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s551, 1);
    t552 = f_check_46_specialize_3(v_rest, v_h, v_n);
  }
  if (!t552) {
    u_no_case(s551);
  }
  return t552;
}

u_value f_check_46_useful_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_q, u_value v_ts) {
  u_value s556 = u_mk_tuple(2, v_q, v_ts);
  u_value t557 = NULL;
  if ((!t557) && u_is_con(u_field(s556, 0), &n_list_46_Nil)) {
    t557 = u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t557) && (u_is_con(u_field(s556, 0), &n_list_46_Cons) && u_is_con(u_field(s556, 1), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s556, 0), 0);
    u_value v_ps = u_field(u_field(s556, 0), 1);
    u_value v_t = u_field(u_field(s556, 1), 0);
    u_value v_rest = u_field(u_field(s556, 1), 1);
    u_value s558 = f_check_46_head_of_1(v_p);
    u_value t559 = NULL;
    if ((!t559) && u_is_con(s558, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s558, 0), 0);
      u_value v_args = u_field(u_field(s558, 0), 1);
      t559 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_ps), f_list_46__43__43__2(f_check_46_fields_of_4(v_c, v_su, v_t, v_h), v_rest));
    }
    if ((!t559) && u_is_con(s558, &n_option_46_None)) {
      u_value s560 = f_check_46_constructors_of_3(v_c, v_su, v_t);
      u_value t561 = NULL;
      if ((!t561) && u_is_con(s560, &n_option_46_Some)) {
        u_value v_ks = u_field(s560, 0);
        u_value t562;
        if (u_truth(f_check_46_covers_2(f_check_46_heads_1(v_rows), v_ks))) {
          t562 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_ks, v_ps, v_rest);
        } else {
          t562 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
        }
        t561 = t562;
      }
      if ((!t561) && u_is_con(s560, &n_option_46_None)) {
        t561 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
      }
      if (!t561) {
        u_no_case(s560);
      }
      t559 = t561;
    }
    if (!t559) {
      u_no_case(s558);
    }
    t557 = t559;
  }
  if ((!t557) && (u_is_con(u_field(s556, 0), &n_list_46_Cons) && u_is_con(u_field(s556, 1), &n_list_46_Nil))) {
    t557 = u_mk_bool(0);
  }
  if (!t557) {
    u_no_case(s556);
  }
  return t557;
}

u_value f_check_46_useful_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_ps, u_value v_rest) {
  u_value s563 = v_ks;
  u_value t564 = NULL;
  if ((!t564) && u_is_con(s563, &n_list_46_Nil)) {
    t564 = u_mk_bool(0);
  }
  if ((!t564) && u_is_con(s563, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s563, 0), 0);
    u_value v_args = u_field(u_field(s563, 0), 1);
    u_value v_more = u_field(s563, 1);
    u_value t565 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_list_46__43__43__2(v_args, v_rest));
    if (!u_truth(t565)) {
      t565 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_more, v_ps, v_rest);
    }
    t564 = t565;
  }
  if (!t564) {
    u_no_case(s563);
  }
  return t564;
}

u_value f_check_46_witness_2(u_value v_h, u_value v_args) {
  u_value s566 = v_h;
  u_value t567 = NULL;
  if ((!t567) && u_is_con(s566, &n_check_46_HCon)) {
    u_value v_n = u_field(s566, 0);
    u_value t568;
    if (u_truth(u_eq(v_n, f_syntax_46_tuple_name_0()))) {
      t568 = u_mk_con(&n_syntax_46_PTuple, 1, v_args);
    } else {
      t568 = u_mk_con(&n_syntax_46_PApp, 2, v_n, v_args);
    }
    t567 = t568;
  }
  if ((!t567) && u_is_con(s566, &n_check_46_HLit)) {
    u_value v_v = u_field(s566, 0);
    t567 = u_mk_con(&n_syntax_46_PLit, 1, v_v);
  }
  if (!t567) {
    u_no_case(s566);
  }
  return t567;
}

u_value f_check_46_missing_4(u_value v_c, u_value v_su, u_value v_rows, u_value v_ts) {
  u_value s569 = v_ts;
  u_value t570 = NULL;
  if ((!t570) && u_is_con(s569, &n_list_46_Nil)) {
    u_value t571;
    if (u_truth(u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0)))) {
      t571 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
    } else {
      t571 = u_mk_con(&n_option_46_None, 0);
    }
    t570 = t571;
  }
  if ((!t570) && u_is_con(s569, &n_list_46_Cons)) {
    u_value v_t = u_field(s569, 0);
    u_value v_rest = u_field(s569, 1);
    t570 = f_check_46_missing_in_6(v_c, v_su, v_rows, f_check_46_heads_1(v_rows), f_check_46_constructors_of_3(v_c, v_su, v_t), v_rest);
  }
  if (!t570) {
    u_no_case(s569);
  }
  return t570;
}

u_value f_check_46_missing_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s572 = v_cs;
  u_value t573 = NULL;
  if ((!t573) && u_is_con(s572, &n_option_46_Some)) {
    u_value v_ks = u_field(s572, 0);
    u_value t574;
    if (u_truth(f_check_46_covers_2(v_hs, v_ks))) {
      t574 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_ks, v_rest);
    } else {
      t574 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
    }
    t573 = t574;
  }
  if ((!t573) && u_is_con(s572, &n_option_46_None)) {
    t573 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
  }
  if (!t573) {
    u_no_case(s572);
  }
  return t573;
}

u_value f_check_46_first_missing_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_rest) {
  u_value s575 = v_ks;
  u_value t576 = NULL;
  if ((!t576) && u_is_con(s575, &n_list_46_Nil)) {
    t576 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t576) && u_is_con(s575, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s575, 0), 0);
    u_value v_args = u_field(u_field(s575, 0), 1);
    u_value v_more = u_field(s575, 1);
    u_value s577 = f_check_46_missing_4(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_rest));
    u_value t578 = NULL;
    if ((!t578) && u_is_con(s577, &n_option_46_Some)) {
      u_value v_ws = u_field(s577, 0);
      t578 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_witness_2(v_h, f_list_46_take_2(f_list_46_length_1(v_args), v_ws)), f_list_46_drop_2(f_list_46_length_1(v_args), v_ws)));
    }
    if ((!t578) && u_is_con(s577, &n_option_46_None)) {
      t578 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_more, v_rest);
    }
    if (!t578) {
      u_no_case(s577);
    }
    t576 = t578;
  }
  if (!t576) {
    u_no_case(s575);
  }
  return t576;
}

u_value f_check_46_other_missing_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s579 = f_check_46_missing_4(v_c, v_su, f_check_46_defaults_1(v_rows), v_rest);
  u_value t580 = NULL;
  if ((!t580) && u_is_con(s579, &n_option_46_Some)) {
    u_value v_ws = u_field(s579, 0);
    t580 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_uncovered_2(v_hs, v_cs), v_ws));
  }
  if ((!t580) && u_is_con(s579, &n_option_46_None)) {
    t580 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t580) {
    u_no_case(s579);
  }
  return t580;
}

u_value f_check_46_uncovered_2(u_value v_hs, u_value v_cs) {
  u_value s581 = v_cs;
  u_value t582 = NULL;
  if ((!t582) && u_is_con(s581, &n_option_46_Some)) {
    u_value v_ks = u_field(s581, 0);
    u_value t583;
    if (u_truth(u_eq(v_hs, u_mk_con(&n_list_46_Nil, 0)))) {
      t583 = u_mk_con(&n_syntax_46_PWild, 0);
    } else {
      t583 = f_check_46_first_uncovered_2(v_hs, v_ks);
    }
    t582 = t583;
  }
  if ((!t582) && u_is_con(s581, &n_option_46_None)) {
    t582 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if (!t582) {
    u_no_case(s581);
  }
  return t582;
}

u_value f_check_46_first_uncovered_2(u_value v_hs, u_value v_ks) {
  u_value s584 = v_ks;
  u_value t585 = NULL;
  if ((!t585) && u_is_con(s584, &n_list_46_Nil)) {
    t585 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if ((!t585) && u_is_con(s584, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s584, 0), 0);
    u_value v_args = u_field(u_field(s584, 0), 1);
    u_value v_rest = u_field(s584, 1);
    u_value t586;
    if (u_truth(f_list_46_contains_2(v_h, v_hs))) {
      t586 = f_check_46_first_uncovered_2(v_hs, v_rest);
    } else {
      t586 = f_check_46_witness_2(v_h, f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)));
    }
    t585 = t586;
  }
  if (!t585) {
    u_no_case(s584);
  }
  return t585;
}

void p_check_46_fresh(u_value *r_t, u_value *r_s) {
  {
    u_value t587 = u_mk_con(&n_syntax_46_TMeta, 1, u_add(f_check_46_State_46_count_1(*r_s), u_mk_int(1)));
    u_value t588 = f_check_46_State_46_count_58__61__2(*r_s, u_add(f_check_46_State_46_count_1(*r_s), u_mk_int(1)));
    *r_t = t587;
    *r_s = t588;
  }
}

void p_check_46_bind_meta(u_value v_i, u_value v_t, u_value *r_s) {
  {
    if (u_truth(f_check_46_occurs_3(f_check_46_State_46_sub_1(*r_s), v_i, v_t))) {
      {
        u_raise(u_mk_con(&n_check_46_InfiniteType, 2, u_mk_con(&n_syntax_46_TMeta, 1, v_i), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t)), "check.TypeError");
      }
    }
    u_value t589 = f_check_46_State_46_sub_58__61__2(*r_s, f_map_46_put_3(v_i, v_t, f_check_46_State_46_sub_1(*r_s)));
    *r_s = t589;
  }
}

void p_check_46_unify(u_value v_t1, u_value v_t2, u_value *r_s) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value t590 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t1);
    v_a = t590;
    u_value t591 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t2);
    v_b = t591;
    u_value s592 = u_mk_tuple(2, v_a, v_b);
    int d593 = 0;
    if ((!d593) && (u_is_con(u_field(s592, 0), &n_syntax_46_TMeta) && u_is_con(u_field(s592, 1), &n_syntax_46_TMeta))) {
      u_value v_i = u_field(u_field(s592, 0), 0);
      u_value v_j = u_field(u_field(s592, 1), 0);
      d593 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_i, v_j))) {
          {
            p_check_46_bind_meta(v_i, v_b, r_s);
          }
        }
      }
    }
    if ((!d593) && u_is_con(u_field(s592, 0), &n_syntax_46_TMeta)) {
      u_value v_i = u_field(u_field(s592, 0), 0);
      d593 = 1;
      {
        p_check_46_bind_meta(v_i, v_b, r_s);
      }
    }
    if ((!d593) && u_is_con(u_field(s592, 1), &n_syntax_46_TMeta)) {
      u_value v_j = u_field(u_field(s592, 1), 0);
      d593 = 1;
      {
        p_check_46_bind_meta(v_j, v_a, r_s);
      }
    }
    if ((!d593) && (u_is_con(u_field(s592, 0), &n_syntax_46_TParam) && u_is_con(u_field(s592, 1), &n_syntax_46_TParam))) {
      u_value v_x = u_field(u_field(s592, 0), 0);
      u_value v_y = u_field(u_field(s592, 1), 0);
      d593 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_x, v_y))) {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, v_a, v_b), "check.TypeError");
          }
        }
      }
    }
    if ((!d593) && (u_is_con(u_field(s592, 0), &n_syntax_46_TSort) && u_is_con(u_field(s592, 1), &n_syntax_46_TSort))) {
      u_value v_f = u_field(u_field(s592, 0), 0);
      u_value v_xs = u_field(u_field(s592, 0), 1);
      u_value v_g = u_field(u_field(s592, 1), 0);
      u_value v_ys = u_field(u_field(s592, 1), 1);
      d593 = 1;
      {
        u_value t594 = u_eq(v_f, v_g);
        if (u_truth(t594)) {
          t594 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys));
        }
        if (u_truth(t594)) {
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
    if ((!d593) && (u_is_con(u_field(s592, 0), &n_syntax_46_TFun) && u_is_con(u_field(s592, 1), &n_syntax_46_TFun))) {
      u_value v_xs = u_field(u_field(s592, 0), 0);
      u_value v_r1 = u_field(u_field(s592, 0), 1);
      u_value v_ys = u_field(u_field(s592, 1), 0);
      u_value v_r2 = u_field(u_field(s592, 1), 1);
      d593 = 1;
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
    if ((!d593) && (u_is_con(u_field(s592, 0), &n_syntax_46_TTuple) && u_is_con(u_field(s592, 1), &n_syntax_46_TTuple))) {
      u_value v_xs = u_field(u_field(s592, 0), 0);
      u_value v_ys = u_field(u_field(s592, 1), 0);
      d593 = 1;
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
    if ((!d593) && (u_is_con(u_field(s592, 0), &n_syntax_46_TProc) && u_is_con(u_field(s592, 1), &n_syntax_46_TProc))) {
      u_value v_xs = u_field(u_field(s592, 0), 0);
      u_value v_ys = u_field(u_field(s592, 0), 1);
      u_value v_zs = u_field(u_field(s592, 0), 2);
      u_value v_us = u_field(u_field(s592, 1), 0);
      u_value v_vs = u_field(u_field(s592, 1), 1);
      u_value v_ws = u_field(u_field(s592, 1), 2);
      d593 = 1;
      {
        u_value t595 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_us));
        if (u_truth(t595)) {
          t595 = u_eq(f_list_46_length_1(v_ys), f_list_46_length_1(v_vs));
        }
        u_value t596 = t595;
        if (u_truth(t596)) {
          t596 = u_eq(f_list_46_length_1(v_zs), f_list_46_length_1(v_ws));
        }
        if (u_truth(t596)) {
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
    if (!d593) {
      d593 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t2)), "check.TypeError");
      }
    }
    if (!d593) {
      u_no_case(s592);
    }
  }
}

void p_check_46_unifies(u_value v_xs, u_value v_ys, u_value *r_s) {
  {
    u_value s597 = u_mk_tuple(2, v_xs, v_ys);
    int d598 = 0;
    if ((!d598) && (u_is_con(u_field(s597, 0), &n_list_46_Cons) && u_is_con(u_field(s597, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s597, 0), 0);
      u_value v_xs1 = u_field(u_field(s597, 0), 1);
      u_value v_y = u_field(u_field(s597, 1), 0);
      u_value v_ys1 = u_field(u_field(s597, 1), 1);
      d598 = 1;
      {
        p_check_46_unify(v_x, v_y, r_s);
        p_check_46_unifies(v_xs1, v_ys1, r_s);
      }
    }
    if ((!d598) && (u_is_con(u_field(s597, 0), &n_list_46_Nil) && u_is_con(u_field(s597, 1), &n_list_46_Nil))) {
      d598 = 1;
      {
      }
    }
    if (!d598) {
      d598 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)), "check.TypeError");
      }
    }
    if (!d598) {
      u_no_case(s597);
    }
  }
}

void p_check_46_fresh_list(u_value v_xs, u_value *r_ts, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s599 = v_xs;
    int d600 = 0;
    if ((!d600) && u_is_con(s599, &n_list_46_Nil)) {
      d600 = 1;
      {
        u_value t601 = u_mk_con(&n_list_46_Nil, 0);
        *r_ts = t601;
      }
    }
    if ((!d600) && u_is_con(s599, &n_list_46_Cons)) {
      u_value v_more = u_field(s599, 1);
      d600 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_fresh_list(v_more, &v_rest, r_s);
        u_value t602 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rest);
        *r_ts = t602;
      }
    }
    if (!d600) {
      u_no_case(s599);
    }
  }
}

void p_check_46_instance(u_value v_ps, u_value *r_m, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s603 = v_ps;
    int d604 = 0;
    if ((!d604) && u_is_con(s603, &n_list_46_Nil)) {
      d604 = 1;
      {
        u_value t605 = u_mk_con(&n_list_46_Nil, 0);
        *r_m = t605;
      }
    }
    if ((!d604) && u_is_con(s603, &n_list_46_Cons)) {
      u_value v_p = u_field(s603, 0);
      u_value v_qs = u_field(s603, 1);
      d604 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_instance(v_qs, &v_rest, r_s);
        u_value t606 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_p, v_t), v_rest);
        *r_m = t606;
      }
    }
    if (!d604) {
      u_no_case(s603);
    }
  }
}

void p_check_46_instantiate(u_value v_sc, u_value *r_t, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value s607 = v_sc;
    int d608 = 0;
    if (!d608) {
      u_value v_ps = u_field(s607, 0);
      u_value v_u = u_field(s607, 1);
      d608 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        u_value t609 = f_check_46_subst_2(f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u), v_m);
        *r_t = t609;
      }
    }
    if (!d608) {
      u_no_case(s607);
    }
  }
}

void p_check_46_unique(u_value v_ns) {
  {
    u_value s610 = f_list_46_repeated_1(v_ns);
    int d611 = 0;
    if ((!d611) && u_is_con(s610, &n_option_46_Some)) {
      u_value v_n = u_field(s610, 0);
      d611 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, f_syntax_46_qualified_1(v_n)), "check.TypeError");
      }
    }
    if ((!d611) && u_is_con(s610, &n_option_46_None)) {
      d611 = 1;
      {
      }
    }
    if (!d611) {
      u_no_case(s610);
    }
  }
}

void p_check_46_unique_ids(u_value v_ns) {
  {
    u_value s612 = f_list_46_repeated_1(v_ns);
    int d613 = 0;
    if ((!d613) && u_is_con(s612, &n_option_46_Some)) {
      u_value v_n = u_field(s612, 0);
      d613 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, v_n), "check.TypeError");
      }
    }
    if ((!d613) && u_is_con(s612, &n_option_46_None)) {
      d613 = 1;
      {
      }
    }
    if (!d613) {
      u_no_case(s612);
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
    u_value s614 = v_t;
    int d615 = 0;
    if ((!d615) && u_is_con(s614, &n_syntax_46_THole)) {
      d615 = 1;
      {
        p_check_46_fresh(r_u, r_s);
      }
    }
    if ((!d615) && u_is_con(s614, &n_syntax_46_TParam)) {
      d615 = 1;
      {
        u_value t616 = v_t;
        *r_u = t616;
      }
    }
    if ((!d615) && u_is_con(s614, &n_syntax_46_TMeta)) {
      d615 = 1;
      {
        u_value t617 = v_t;
        *r_u = t617;
      }
    }
    if ((!d615) && u_is_con(s614, &n_syntax_46_TFun)) {
      u_value v_args = u_field(s614, 0);
      u_value v_r = u_field(s614, 1);
      d615 = 1;
      {
        p_check_46_normalizes(v_args, &v_us, r_s);
        p_check_46_normalize(v_r, &v_v, r_s);
        u_value t618 = u_mk_con(&n_syntax_46_TFun, 2, v_us, v_v);
        *r_u = t618;
      }
    }
    if ((!d615) && u_is_con(s614, &n_syntax_46_TTuple)) {
      u_value v_ts = u_field(s614, 0);
      d615 = 1;
      {
        p_check_46_normalizes(v_ts, &v_us, r_s);
        u_value t619 = u_mk_con(&n_syntax_46_TTuple, 1, v_us);
        *r_u = t619;
      }
    }
    if ((!d615) && u_is_con(s614, &n_syntax_46_TProc)) {
      u_value v_is = u_field(s614, 0);
      u_value v_os = u_field(s614, 1);
      u_value v_rs = u_field(s614, 2);
      d615 = 1;
      {
        p_check_46_normalizes(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs)), &v_us, r_s);
        u_value t620 = u_mk_con(&n_syntax_46_TProc, 3, f_list_46_take_2(f_list_46_length_1(v_is), v_us), f_list_46_take_2(f_list_46_length_1(v_os), f_list_46_drop_2(f_list_46_length_1(v_is), v_us)), f_list_46_drop_2(u_add(f_list_46_length_1(v_is), f_list_46_length_1(v_os)), v_us));
        *r_u = t620;
      }
    }
    if ((!d615) && u_is_con(s614, &n_syntax_46_TSort)) {
      u_value v_n = u_field(s614, 0);
      u_value v_args = u_field(s614, 1);
      d615 = 1;
      {
        if (u_truth(f_check_46_basic_1(v_n))) {
          {
            p_check_46_arguments(v_n, u_mk_int(0), v_args);
            u_value t621 = v_t;
            *r_u = t621;
          }
        } else {
          {
            u_value s623 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam622, 1, *r_s));
            int d624 = 0;
            if ((!d624) && (u_is_con(s623, &n_list_46_Cons) && u_is_con(u_field(s623, 1), &n_list_46_Nil))) {
              u_value v_m = u_field(s623, 0);
              d624 = 1;
              {
                p_check_46_normalize_sort(v_m, v_args, r_u, r_s);
              }
            }
            if ((!d624) && u_is_con(s623, &n_list_46_Nil)) {
              d624 = 1;
              {
                u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
              }
            }
            if (!d624) {
              u_value v_ms = s623;
              d624 = 1;
              {
                u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ms), "check.TypeError");
              }
            }
            if (!d624) {
              u_no_case(s623);
            }
          }
        }
      }
    }
    if (!d615) {
      u_no_case(s614);
    }
  }
}

void p_check_46_normalize_sort(u_value v_n, u_value v_args, u_value *r_u, u_value *r_s) {
  {
    u_value v_us = NULL;
    u_value s625 = f_ctx_46_data_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
    int d626 = 0;
    if ((!d626) && u_is_con(s625, &n_option_46_Some)) {
      u_value v_k = u_field(s625, 0);
      d626 = 1;
      {
        p_check_46_arguments(v_n, v_k, v_args);
        p_check_46_normalizes(v_args, &v_us, r_s);
        u_value t627 = u_mk_con(&n_syntax_46_TSort, 2, v_n, v_us);
        *r_u = t627;
      }
    }
    if ((!d626) && u_is_con(s625, &n_option_46_None)) {
      d626 = 1;
      {
        u_value s628 = f_ctx_46_alias_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
        int d629 = 0;
        if ((!d629) && u_is_con(s628, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s628, 0), 0);
          u_value v_rhs = u_field(u_field(s628, 0), 1);
          d629 = 1;
          {
            p_check_46_arguments(v_n, f_list_46_length_1(v_ps), v_args);
            p_check_46_normalizes(v_args, &v_us, r_s);
            u_value t630 = f_check_46_subst_2(v_rhs, f_list_46_zip_2(f_check_46_params_1(v_ps), v_us));
            *r_u = t630;
          }
        }
        if ((!d629) && u_is_con(s628, &n_option_46_None)) {
          d629 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
          }
        }
        if (!d629) {
          u_no_case(s628);
        }
      }
    }
    if (!d626) {
      u_no_case(s625);
    }
  }
}

void p_check_46_normalizes(u_value v_ts, u_value *r_us, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_rest = NULL;
    u_value s631 = v_ts;
    int d632 = 0;
    if ((!d632) && u_is_con(s631, &n_list_46_Nil)) {
      d632 = 1;
      {
        u_value t633 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t633;
      }
    }
    if ((!d632) && u_is_con(s631, &n_list_46_Cons)) {
      u_value v_t = u_field(s631, 0);
      u_value v_more = u_field(s631, 1);
      d632 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalizes(v_more, &v_rest, r_s);
        u_value t634 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t634;
      }
    }
    if (!d632) {
      u_no_case(s631);
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
    u_value s635 = v_ts;
    int d636 = 0;
    if ((!d636) && u_is_con(s635, &n_list_46_Nil)) {
      d636 = 1;
      {
        u_value t637 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t637;
      }
    }
    if ((!d636) && u_is_con(s635, &n_list_46_Cons)) {
      u_value v_t = u_field(s635, 0);
      u_value v_more = u_field(s635, 1);
      d636 = 1;
      {
        p_check_46_known(v_t, v_what, &v_u, r_s);
        p_check_46_knowns(v_more, v_what, &v_rest, r_s);
        u_value t638 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t638;
      }
    }
    if (!d636) {
      u_no_case(s635);
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
    u_value s639 = v_xs;
    int d640 = 0;
    if ((!d640) && u_is_con(s639, &n_list_46_Nil)) {
      d640 = 1;
      {
        u_value t641 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t641;
      }
    }
    if ((!d640) && (u_is_con(s639, &n_list_46_Cons) && u_is_con(u_field(s639, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s639, 0), 0);
      u_value v_t = u_field(u_field(s639, 0), 1);
      u_value v_more = u_field(s639, 1);
      d640 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalize_vars(v_more, &v_rest, r_s);
        u_value t642 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t642;
      }
    }
    if (!d640) {
      u_no_case(s639);
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
    u_value s643 = v_xs;
    int d644 = 0;
    if ((!d644) && u_is_con(s643, &n_list_46_Nil)) {
      d644 = 1;
      {
        u_value t645 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t645;
      }
    }
    if ((!d644) && (u_is_con(s643, &n_list_46_Cons) && u_is_con(u_field(s643, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s643, 0), 0);
      u_value v_t = u_field(u_field(s643, 0), 1);
      u_value v_more = u_field(s643, 1);
      d644 = 1;
      {
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_known_vars(v_more, &v_rest, r_s);
        u_value t646 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t646;
      }
    }
    if (!d644) {
      u_no_case(s643);
    }
  }
}

void p_check_46_resolve_name(u_value v_s, u_value v_env, u_value v_n, u_value v_k, u_value *r_m, u_value *r_sc) {
  {
    u_value s647 = f_check_46_candidates_3(v_s, v_env, v_n);
    int d648 = 0;
    if ((!d648) && u_is_con(s647, &n_list_46_Nil)) {
      d648 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if ((!d648) && (u_is_con(s647, &n_list_46_Cons) && u_is_con(u_field(s647, 1), &n_list_46_Nil))) {
      u_value v_m1 = u_field(u_field(s647, 0), 0);
      u_value v_one = u_field(u_field(s647, 0), 1);
      d648 = 1;
      {
        u_value t649 = v_m1;
        u_value t650 = v_one;
        *r_m = t649;
        *r_sc = t650;
      }
    }
    if (!d648) {
      u_value v_cs = s647;
      d648 = 1;
      {
        if (u_truth(u_lt(v_k, u_mk_int(0)))) {
          {
            u_raise(u_mk_con(&n_check_46_OverloadedUnapplied, 1, v_n), "check.TypeError");
          }
        }
        u_value s651 = f_check_46_with_arity_2(v_cs, v_k);
        int d652 = 0;
        if ((!d652) && (u_is_con(s651, &n_list_46_Cons) && u_is_con(u_field(s651, 1), &n_list_46_Nil))) {
          u_value v_m1 = u_field(u_field(s651, 0), 0);
          u_value v_one = u_field(u_field(s651, 0), 1);
          d652 = 1;
          {
            u_value t653 = v_m1;
            u_value t654 = v_one;
            *r_m = t653;
            *r_sc = t654;
          }
        }
        if ((!d652) && u_is_con(s651, &n_list_46_Nil)) {
          d652 = 1;
          {
            u_raise(u_mk_con(&n_check_46_WrongArity, 2, v_n, v_k), "check.TypeError");
          }
        }
        if (!d652) {
          u_value v_more = s651;
          d652 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, f_list_46_firsts_1(v_more)), "check.TypeError");
          }
        }
        if (!d652) {
          u_no_case(s651);
        }
      }
    }
    if (!d648) {
      u_no_case(s647);
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
    u_value t655 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_u);
    *r_x = t655;
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
    u_value s656 = v_e;
    int d657 = 0;
    if ((!d657) && u_is_con(s656, &n_syntax_46_ELit)) {
      u_value v_v = u_field(s656, 0);
      d657 = 1;
      {
        u_value t658 = v_e;
        u_value t659 = f_check_46_literal_1(v_v);
        *r_e1 = t658;
        *r_t = t659;
      }
    }
    if ((!d657) && (u_is_con(s656, &n_syntax_46_EVar) && u_is_con(u_field(s656, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s656, 0), 0);
      u_value v_a = u_field(u_field(s656, 0), 1);
      d657 = 1;
      {
        p_check_46_resolve_name(*r_s, v_env, v_n, u_mk_int(-1), &v_m, &v_sc);
        p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
        u_value t660 = u_mk_con(&n_syntax_46_EVar, 1, v_x);
        u_value t661 = f_syntax_46_Var_46_typ_1(v_x);
        *r_e1 = t660;
        *r_t = t661;
      }
    }
    if ((!d657) && u_is_con(s656, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s656, 0);
      u_value v_args = u_field(s656, 1);
      d657 = 1;
      {
        p_check_46_application(v_env, v_f, v_args, r_e1, r_t, r_s);
      }
    }
    if ((!d657) && u_is_con(s656, &n_syntax_46_ELam)) {
      u_value v_bound = u_field(s656, 0);
      u_value v_body = u_field(s656, 1);
      d657 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t662 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, v_b);
        u_value t663 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), v_u);
        *r_e1 = t662;
        *r_t = t663;
      }
    }
    if ((!d657) && u_is_con(s656, &n_syntax_46_EProc)) {
      u_value v_is = u_field(s656, 0);
      u_value v_os = u_field(s656, 1);
      u_value v_rs = u_field(s656, 2);
      u_value v_body = u_field(s656, 3);
      d657 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))));
        p_check_46_formals(v_is, &v_xs, r_s);
        p_check_46_formals(v_os, &v_ys, r_s);
        p_check_46_formals(v_rs, &v_zs, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(f_list_46__43__43__2(v_xs, f_list_46__43__43__2(v_ys, v_zs))), v_env), f_syntax_46_names_1(f_list_46__43__43__2(v_ys, v_zs)), v_body, &v_y, r_s);
        u_value t664 = u_mk_con(&n_syntax_46_EProc, 4, v_xs, v_ys, v_zs, v_y);
        u_value t665 = u_mk_con(&n_syntax_46_TProc, 3, f_syntax_46_types_1(v_xs), f_syntax_46_types_1(v_ys), f_syntax_46_types_1(v_zs));
        *r_e1 = t664;
        *r_t = t665;
      }
    }
    if ((!d657) && u_is_con(s656, &n_syntax_46_ELet)) {
      u_value v_bound = u_field(s656, 0);
      u_value v_body = u_field(s656, 1);
      d657 = 1;
      {
        p_check_46_formals(f_list_46_firsts_1(v_bound), &v_xs, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_bound), f_syntax_46_types_1(v_xs), &v_es, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_generalized_3(f_check_46_State_46_sub_1(*r_s), v_env, v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t666 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        u_value t667 = v_u;
        *r_e1 = t666;
        *r_t = t667;
      }
    }
    if ((!d657) && u_is_con(s656, &n_syntax_46_EIte)) {
      u_value v_cond = u_field(s656, 0);
      u_value v_left = u_field(s656, 1);
      u_value v_right = u_field(s656, 2);
      d657 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(v_env, v_left, &v_l, &v_u, r_s);
        p_check_46_check(v_env, v_right, v_u, &v_b, r_s);
        u_value t668 = u_mk_con(&n_syntax_46_EIte, 3, v_c, v_l, v_b);
        u_value t669 = v_u;
        *r_e1 = t668;
        *r_t = t669;
      }
    }
    if ((!d657) && u_is_con(s656, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s656, 0);
      u_value v_bound = u_field(s656, 1);
      u_value v_lists = u_field(s656, 2);
      u_value v_cond = u_field(s656, 3);
      d657 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_same_length(f_list_46_length_1(v_xs), f_list_46_length_1(v_lists));
        p_check_46_generators(v_env, v_xs, v_lists, &v_es, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_result, &v_b, &v_u, r_s);
        u_value t670 = u_mk_con(&n_syntax_46_EComp, 4, v_b, v_xs, v_es, v_c);
        u_value t671 = f_check_46_tlist_1(v_u);
        *r_e1 = t670;
        *r_t = t671;
      }
    }
    if ((!d657) && u_is_con(s656, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s656, 0);
      d657 = 1;
      {
        p_check_46_infers(v_env, v_items, &v_es, &v_ts, r_s);
        u_value t672 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
        u_value t673 = u_mk_con(&n_syntax_46_TTuple, 1, v_ts);
        *r_e1 = t672;
        *r_t = t673;
      }
    }
    if ((!d657) && u_is_con(s656, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s656, 0);
      u_value v_a = u_field(s656, 1);
      d657 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_normalize(v_a, &v_w, r_s);
        p_check_46_unify(v_u, v_w, r_s);
        p_check_46_fresh(r_t, r_s);
        u_value t674 = u_mk_con(&n_syntax_46_ERaise, 2, v_c, v_u);
        *r_e1 = t674;
      }
    }
    if ((!d657) && u_is_con(s656, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s656, 0);
      u_value v_cases = u_field(s656, 1);
      d657 = 1;
      {
        p_check_46_fresh(r_t, r_s);
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_expr_branches(v_env, v_cases, v_u, *r_t, &v_bs, r_s);
        u_value t675 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e676 = &t675;
        u_value l677 = v_bs;
        while (u_is_con(l677, &n_list_46_Cons)) {
          u_value v_br = u_field(l677, 0);
          if (u_truth(u_mk_bool(1))) {
            e676 = u_append(e676, f_check_46_case_of_1(v_br));
          }
          l677 = u_field(l677, 1);
        }
        p_check_46_coverage(*r_s, v_u, t675);
        u_value t678 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, v_bs);
        *r_e1 = t678;
      }
    }
    if (!d657) {
      u_no_case(s656);
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
    u_value s679 = v_f;
    int d680 = 0;
    if ((!d680) && (u_is_con(s679, &n_syntax_46_EVar) && (u_is_con(u_field(s679, 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(s679, 0), 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(u_field(s679, 0), 0), 0), &n_option_46_None))))) {
      u_value v_b = u_field(u_field(u_field(s679, 0), 0), 1);
      u_value v_a = u_field(u_field(s679, 0), 1);
      u_value t681 = f_prelude_46__62__2(u_str_size(v_b), u_mk_int(1));
      if (u_truth(t681)) {
        t681 = u_eq(u_str_get(v_b, u_mk_int(0)), u_mk_char(46));
      }
      if (u_truth(t681)) {
        d680 = 1;
        {
          p_check_46_field_access(v_env, v_b, v_a, v_args, r_e1, r_t, r_s);
        }
      }
    }
    if ((!d680) && (u_is_con(s679, &n_syntax_46_EVar) && u_is_con(u_field(s679, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s679, 0), 0);
      u_value v_a = u_field(u_field(s679, 0), 1);
      d680 = 1;
      {
        if (u_truth(f_prelude_46__62__2(f_list_46_length_1(f_check_46_candidates_3(*r_s, v_env, v_n)), u_mk_int(1)))) {
          {
            p_check_46_resolve_name(*r_s, v_env, v_n, f_list_46_length_1(v_args), &v_m, &v_sc);
            p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
            p_check_46_apply(v_env, f_syntax_46_Var_46_typ_1(v_x), v_args, &v_args1, r_t, r_s);
            u_value t682 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), v_args1);
            *r_e1 = t682;
          }
        } else {
          {
            p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
            p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
            u_value t683 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
            *r_e1 = t683;
          }
        }
      }
    }
    if (!d680) {
      d680 = 1;
      {
        p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
        p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
        u_value t684 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
        *r_e1 = t684;
      }
    }
    if (!d680) {
      u_no_case(s679);
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
    u_value s685 = v_args;
    int d686 = 0;
    if ((!d686) && u_is_con(s685, &n_list_46_Cons)) {
      u_value v_arg = u_field(s685, 0);
      u_value v_rest = u_field(s685, 1);
      d686 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_arg1, &v_u, r_s);
        u_value s687 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u);
        int d688 = 0;
        if ((!d688) && (u_is_con(s687, &n_syntax_46_TSort) && (u_is_con(u_field(s687, 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(s687, 0), 0), &n_option_46_Some)))) {
          u_value v_m = u_field(u_field(u_field(s687, 0), 0), 0);
          u_value v_tn = u_field(u_field(s687, 0), 1);
          d688 = 1;
          {
            u_value t689 = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), u_concat(v_tn, v_f));
            v_acc = t689;
          }
        }
        if (!d688) {
          u_value v_v = s687;
          d688 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownFieldType, 2, v_f, v_v), "check.TypeError");
          }
        }
        if (!d688) {
          u_no_case(s687);
        }
        u_value s690 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(*r_s), v_acc);
        int d691 = 0;
        if ((!d691) && (u_is_con(s690, &n_list_46_Cons) && u_is_con(u_field(s690, 1), &n_list_46_Nil))) {
          u_value v_sc = u_field(s690, 0);
          d691 = 1;
          {
            p_check_46_use(v_acc, v_a, v_sc, &v_x, r_s);
          }
        }
        if (!d691) {
          d691 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownField, 1, v_acc), "check.TypeError");
          }
        }
        if (!d691) {
          u_no_case(s690);
        }
        p_check_46_infers(v_env, v_rest, &v_rest1, &v_us, r_s);
        p_check_46_fresh(r_t, r_s);
        p_check_46_unify(f_syntax_46_Var_46_typ_1(v_x), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_u, v_us), *r_t), r_s);
        u_value t692 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), u_mk_con(&n_list_46_Cons, 2, v_arg1, v_rest1));
        *r_e1 = t692;
      }
    }
    if (!d686) {
      d686 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, u_mk_int(1), f_list_46_length_1(v_args)), "check.TypeError");
      }
    }
    if (!d686) {
      u_no_case(s685);
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
    u_value s693 = u_mk_tuple(2, v_es, v_ts);
    int d694 = 0;
    if ((!d694) && (u_is_con(u_field(s693, 0), &n_list_46_Cons) && u_is_con(u_field(s693, 1), &n_list_46_Cons))) {
      u_value v_e = u_field(u_field(s693, 0), 0);
      u_value v_more = u_field(u_field(s693, 0), 1);
      u_value v_t = u_field(u_field(s693, 1), 0);
      u_value v_ts1 = u_field(u_field(s693, 1), 1);
      d694 = 1;
      {
        p_check_46_check(v_env, v_e, v_t, &v_e1, r_s);
        p_check_46_checks(v_env, v_more, v_ts1, &v_rest, r_s);
        u_value t695 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        *r_es1 = t695;
      }
    }
    if ((!d694) && (u_is_con(u_field(s693, 0), &n_list_46_Nil) && u_is_con(u_field(s693, 1), &n_list_46_Nil))) {
      d694 = 1;
      {
        u_value t696 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t696;
      }
    }
    if (!d694) {
      d694 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_es)), "check.TypeError");
      }
    }
    if (!d694) {
      u_no_case(s693);
    }
  }
}

void p_check_46_infers(u_value v_env, u_value v_es, u_value *r_es1, u_value *r_ts, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value v_us = NULL;
    u_value s697 = v_es;
    int d698 = 0;
    if ((!d698) && u_is_con(s697, &n_list_46_Nil)) {
      d698 = 1;
      {
        u_value t699 = u_mk_con(&n_list_46_Nil, 0);
        u_value t700 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t699;
        *r_ts = t700;
      }
    }
    if ((!d698) && u_is_con(s697, &n_list_46_Cons)) {
      u_value v_e = u_field(s697, 0);
      u_value v_more = u_field(s697, 1);
      d698 = 1;
      {
        p_check_46_infer(v_env, v_e, &v_e1, &v_t, r_s);
        p_check_46_infers(v_env, v_more, &v_rest, &v_us, r_s);
        u_value t701 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        u_value t702 = u_mk_con(&n_list_46_Cons, 2, v_t, v_us);
        *r_es1 = t701;
        *r_ts = t702;
      }
    }
    if (!d698) {
      u_no_case(s697);
    }
  }
}

void p_check_46_generators(u_value v_env, u_value v_xs, u_value v_lists, u_value *r_lists1, u_value *r_s) {
  {
    u_value v_l = NULL;
    u_value v_rest = NULL;
    u_value s703 = u_mk_tuple(2, v_xs, v_lists);
    int d704 = 0;
    if ((!d704) && (u_is_con(u_field(s703, 0), &n_list_46_Cons) && u_is_con(u_field(s703, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s703, 0), 0);
      u_value v_ys = u_field(u_field(s703, 0), 1);
      u_value v_e = u_field(u_field(s703, 1), 0);
      u_value v_es = u_field(u_field(s703, 1), 1);
      d704 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tlist_1(f_syntax_46_Var_46_typ_1(v_x)), &v_l, r_s);
        p_check_46_generators(f_list_46__43__43__2(f_check_46_bind_1(u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0))), v_env), v_ys, v_es, &v_rest, r_s);
        u_value t705 = u_mk_con(&n_list_46_Cons, 2, v_l, v_rest);
        *r_lists1 = t705;
      }
    }
    if (!d704) {
      d704 = 1;
      {
        u_value t706 = u_mk_con(&n_list_46_Nil, 0);
        *r_lists1 = t706;
      }
    }
    if (!d704) {
      u_no_case(s703);
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
    u_value s707 = v_cases;
    int d708 = 0;
    if ((!d708) && u_is_con(s707, &n_list_46_Nil)) {
      d708 = 1;
      {
        u_value t709 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t709;
      }
    }
    if ((!d708) && (u_is_con(s707, &n_list_46_Cons) && u_is_con(u_field(s707, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s707, 0), 0);
      u_value v_g = u_field(u_field(s707, 0), 1);
      u_value v_b = u_field(u_field(s707, 0), 2);
      u_value v_more = u_field(s707, 1);
      d708 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_b, v_r, &v_b1, r_s);
        p_check_46_expr_branches(v_env, v_more, v_t, v_r, &v_rest, r_s);
        u_value t710 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t710;
      }
    }
    if (!d708) {
      u_no_case(s707);
    }
  }
}

void p_check_46_guard(u_value v_env, u_value v_g, u_value *r_g1, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value s711 = v_g;
    int d712 = 0;
    if ((!d712) && u_is_con(s711, &n_option_46_Some)) {
      u_value v_e = u_field(s711, 0);
      d712 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tbool_0(), &v_e1, r_s);
        u_value t713 = u_mk_con(&n_option_46_Some, 1, v_e1);
        *r_g1 = t713;
      }
    }
    if ((!d712) && u_is_con(s711, &n_option_46_None)) {
      d712 = 1;
      {
        u_value t714 = u_mk_con(&n_option_46_None, 0);
        *r_g1 = t714;
      }
    }
    if (!d712) {
      u_no_case(s711);
    }
  }
}

void p_check_46_coverage(u_value v_s, u_value v_t, u_value v_cs) {
  {
    p_check_46_reachable(v_s, v_t, u_mk_con(&n_list_46_Nil, 0), v_cs);
    u_value t715 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e716 = &t715;
    u_value l717 = f_check_46_unguarded_1(v_cs);
    while (u_is_con(l717, &n_list_46_Cons)) {
      u_value v_p = u_field(l717, 0);
      if (u_truth(u_mk_bool(1))) {
        e716 = u_append(e716, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
      }
      l717 = u_field(l717, 1);
    }
    u_value s718 = f_check_46_missing_4(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t715, u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
    int d719 = 0;
    if ((!d719) && (u_is_con(s718, &n_option_46_Some) && u_is_con(u_field(s718, 0), &n_list_46_Cons))) {
      u_value v_w = u_field(u_field(s718, 0), 0);
      d719 = 1;
      {
        u_raise(u_mk_con(&n_check_46_NotExhaustive, 1, v_w), "check.TypeError");
      }
    }
    if (!d719) {
      d719 = 1;
      {
      }
    }
    if (!d719) {
      u_no_case(s718);
    }
  }
}

u_value f_check_46_unguarded_1(u_value v_cs) {
  u_value s720 = v_cs;
  u_value t721 = NULL;
  if ((!t721) && u_is_con(s720, &n_list_46_Nil)) {
    t721 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t721) && (u_is_con(s720, &n_list_46_Cons) && u_equals(u_field(u_field(s720, 0), 1), u_mk_bool(1)))) {
    u_value v_p = u_field(u_field(s720, 0), 0);
    u_value v_rest = u_field(s720, 1);
    t721 = u_mk_con(&n_list_46_Cons, 2, v_p, f_check_46_unguarded_1(v_rest));
  }
  if ((!t721) && (u_is_con(s720, &n_list_46_Cons) && u_equals(u_field(u_field(s720, 0), 1), u_mk_bool(0)))) {
    u_value v_rest = u_field(s720, 1);
    t721 = f_check_46_unguarded_1(v_rest);
  }
  if (!t721) {
    u_no_case(s720);
  }
  return t721;
}

void p_check_46_reachable(u_value v_s, u_value v_t, u_value v_before, u_value v_cs) {
  {
    u_value s722 = v_cs;
    int d723 = 0;
    if ((!d723) && u_is_con(s722, &n_list_46_Nil)) {
      d723 = 1;
      {
      }
    }
    if ((!d723) && u_is_con(s722, &n_list_46_Cons)) {
      u_value v_p = u_field(u_field(s722, 0), 0);
      u_value v_open = u_field(u_field(s722, 0), 1);
      u_value v_rest = u_field(s722, 1);
      d723 = 1;
      {
        u_value t724 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e725 = &t724;
        u_value l726 = v_before;
        while (u_is_con(l726, &n_list_46_Cons)) {
          u_value v_q = u_field(l726, 0);
          if (u_truth(u_mk_bool(1))) {
            e725 = u_append(e725, u_mk_con(&n_list_46_Cons, 2, v_q, u_mk_con(&n_list_46_Nil, 0)));
          }
          l726 = u_field(l726, 1);
        }
        if (u_truth(f_prelude_46_not_1(f_check_46_useful_5(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t724, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)))))) {
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
    if (!d723) {
      u_no_case(s722);
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
    u_value s727 = v_p;
    int d728 = 0;
    if ((!d728) && u_is_con(s727, &n_syntax_46_PWild)) {
      d728 = 1;
      {
        u_value t729 = v_p;
        u_value t730 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t729;
        *r_xs = t730;
      }
    }
    if ((!d728) && u_is_con(s727, &n_syntax_46_PLit)) {
      u_value v_v = u_field(s727, 0);
      d728 = 1;
      {
        p_check_46_unify(f_check_46_literal_1(v_v), v_t, r_s);
        u_value t731 = v_p;
        u_value t732 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t731;
        *r_xs = t732;
      }
    }
    if ((!d728) && (u_is_con(s727, &n_syntax_46_PVar) && u_is_con(u_field(s727, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s727, 0), 0);
      u_value v_a = u_field(u_field(s727, 0), 1);
      d728 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam733, 1, *r_s)), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            p_check_46_binders(u_mk_con(&n_syntax_46_PApp, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_t, r_p1, r_xs, r_s);
          }
        } else {
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_u, v_t, r_s);
            u_value t734 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u));
            u_value t735 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), u_mk_con(&n_list_46_Nil, 0));
            *r_p1 = t734;
            *r_xs = t735;
          }
        }
      }
    }
    if ((!d728) && u_is_con(s727, &n_syntax_46_PApp)) {
      u_value v_n = u_field(s727, 0);
      u_value v_args = u_field(s727, 1);
      d728 = 1;
      {
        u_value s737 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam736, 1, *r_s));
        int d738 = 0;
        if ((!d738) && (u_is_con(s737, &n_list_46_Cons) && u_is_con(u_field(s737, 1), &n_list_46_Nil))) {
          u_value v_k1 = u_field(s737, 0);
          d738 = 1;
          {
            u_value t739 = v_k1;
            v_k = t739;
          }
        }
        if ((!d738) && u_is_con(s737, &n_list_46_Nil)) {
          d738 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d738) {
          u_value v_ks = s737;
          d738 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ks), "check.TypeError");
          }
        }
        if (!d738) {
          u_no_case(s737);
        }
        u_value s740 = f_ctx_46_constr_of_2(f_check_46_State_46_ctx_1(*r_s), v_k);
        int d741 = 0;
        if ((!d741) && u_is_con(s740, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s740, 0), 0);
          u_value v_ks = u_field(u_field(s740, 0), 1);
          u_value v_r = u_field(u_field(s740, 0), 2);
          d741 = 1;
          {
            p_check_46_same_length(f_list_46_length_1(v_ks), f_list_46_length_1(v_args));
            p_check_46_instance(v_ps, &v_m, r_s);
            p_check_46_unify(f_check_46_subst_2(v_r, v_m), v_t, r_s);
            u_value t742 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e743 = &t742;
            u_value l744 = v_ks;
            while (u_is_con(l744, &n_list_46_Cons)) {
              u_value v_kt = u_field(l744, 0);
              if (u_truth(u_mk_bool(1))) {
                e743 = u_append(e743, f_check_46_subst_2(v_kt, v_m));
              }
              l744 = u_field(l744, 1);
            }
            p_check_46_binder_list(v_args, t742, &v_qs, r_xs, r_s);
            u_value t745 = u_mk_con(&n_syntax_46_PApp, 2, v_k, v_qs);
            *r_p1 = t745;
          }
        }
        if ((!d741) && u_is_con(s740, &n_option_46_None)) {
          d741 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d741) {
          u_no_case(s740);
        }
      }
    }
    if ((!d728) && u_is_con(s727, &n_syntax_46_PTuple)) {
      u_value v_items = u_field(s727, 0);
      d728 = 1;
      {
        p_check_46_fresh_list(v_items, &v_ts, r_s);
        p_check_46_unify(u_mk_con(&n_syntax_46_TTuple, 1, v_ts), v_t, r_s);
        p_check_46_binder_list(v_items, v_ts, &v_qs, r_xs, r_s);
        u_value t746 = u_mk_con(&n_syntax_46_PTuple, 1, v_qs);
        *r_p1 = t746;
      }
    }
    if (!d728) {
      u_no_case(s727);
    }
  }
}

void p_check_46_binder_list(u_value v_ps, u_value v_ts, u_value *r_qs, u_value *r_xs, u_value *r_s) {
  {
    u_value v_q = NULL;
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value v_zs = NULL;
    u_value s747 = u_mk_tuple(2, v_ps, v_ts);
    int d748 = 0;
    if ((!d748) && (u_is_con(u_field(s747, 0), &n_list_46_Cons) && u_is_con(u_field(s747, 1), &n_list_46_Cons))) {
      u_value v_p = u_field(u_field(s747, 0), 0);
      u_value v_more = u_field(u_field(s747, 0), 1);
      u_value v_t = u_field(u_field(s747, 1), 0);
      u_value v_ts1 = u_field(u_field(s747, 1), 1);
      d748 = 1;
      {
        p_check_46_binders(v_p, v_t, &v_q, &v_ys, r_s);
        p_check_46_binder_list(v_more, v_ts1, &v_rest, &v_zs, r_s);
        u_value t749 = u_mk_con(&n_list_46_Cons, 2, v_q, v_rest);
        u_value t750 = f_list_46__43__43__2(v_ys, v_zs);
        *r_qs = t749;
        *r_xs = t750;
      }
    }
    if ((!d748) && (u_is_con(u_field(s747, 0), &n_list_46_Nil) && u_is_con(u_field(s747, 1), &n_list_46_Nil))) {
      d748 = 1;
      {
        u_value t751 = u_mk_con(&n_list_46_Nil, 0);
        u_value t752 = u_mk_con(&n_list_46_Nil, 0);
        *r_qs = t751;
        *r_xs = t752;
      }
    }
    if (!d748) {
      d748 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_ps)), "check.TypeError");
      }
    }
    if (!d748) {
      u_no_case(s747);
    }
  }
}

void p_check_46_variable(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s753 = v_x;
    int d754 = 0;
    if ((!d754) && u_is_con(s753, &n_syntax_46_Var)) {
      u_value v_n = u_field(s753, 0);
      u_value v_a = u_field(s753, 1);
      d754 = 1;
      {
        if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, v_assignable)))) {
          {
            u_raise(u_mk_con(&n_check_46_NotAssignable, 1, v_n), "check.TypeError");
          }
        }
        u_value s755 = f_list_46_lookup_2(v_n, v_env);
        int d756 = 0;
        if ((!d756) && u_is_con(s755, &n_option_46_Some)) {
          u_value v_t = u_field(u_field(s755, 0), 1);
          d756 = 1;
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_t, v_u, r_s);
            u_value t757 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_t);
            *r_y = t757;
          }
        }
        if ((!d756) && u_is_con(s755, &n_option_46_None)) {
          d756 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
          }
        }
        if (!d756) {
          u_no_case(s755);
        }
      }
    }
    if (!d754) {
      u_no_case(s753);
    }
  }
}

void p_check_46_variables(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s758 = v_xs;
    int d759 = 0;
    if ((!d759) && u_is_con(s758, &n_list_46_Nil)) {
      d759 = 1;
      {
        u_value t760 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t760;
      }
    }
    if ((!d759) && u_is_con(s758, &n_list_46_Cons)) {
      u_value v_x = u_field(s758, 0);
      u_value v_more = u_field(s758, 1);
      d759 = 1;
      {
        p_check_46_variable(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_variables(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t761 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t761;
      }
    }
    if (!d759) {
      u_no_case(s758);
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
    u_value s762 = v_x;
    int d763 = 0;
    if ((!d763) && u_is_con(s762, &n_syntax_46_SSkip)) {
      d763 = 1;
      {
        u_value t764 = v_x;
        *r_y = t764;
      }
    }
    if ((!d763) && u_is_con(s762, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s762, 0);
      d763 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)));
        p_check_46_variables(v_env, v_assignable, f_list_46_firsts_1(v_pairs), &v_ys, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_pairs), f_syntax_46_types_1(v_ys), &v_es, r_s);
        u_value t765 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_zip_2(v_ys, v_es));
        *r_y = t765;
      }
    }
    if ((!d763) && u_is_con(s762, &n_syntax_46_SBlock)) {
      u_value v_locals = u_field(s762, 0);
      u_value v_body = u_field(s762, 1);
      d763 = 1;
      {
        p_check_46_untyped(v_locals, f_check_46_initialized_1(v_body));
        p_check_46_formals(v_locals, &v_ys, r_s);
        p_check_46_statements(f_list_46__43__43__2(f_check_46_bind_1(v_ys), v_env), f_list_46__43__43__2(f_syntax_46_names_1(v_ys), v_assignable), v_body, &v_xs, r_s);
        u_value t766 = u_mk_con(&n_syntax_46_SBlock, 2, v_ys, v_xs);
        *r_y = t766;
      }
    }
    if ((!d763) && u_is_con(s762, &n_syntax_46_SIf)) {
      u_value v_cond = u_field(s762, 0);
      u_value v_left = u_field(s762, 1);
      u_value v_right = u_field(s762, 2);
      d763 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_left, &v_l, r_s);
        p_check_46_statement(v_env, v_assignable, v_right, &v_r, r_s);
        u_value t767 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
        *r_y = t767;
      }
    }
    if ((!d763) && u_is_con(s762, &n_syntax_46_SWhile)) {
      u_value v_cond = u_field(s762, 0);
      u_value v_body = u_field(s762, 1);
      d763 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        u_value t768 = u_mk_con(&n_syntax_46_SWhile, 2, v_c, v_l);
        *r_y = t768;
      }
    }
    if ((!d763) && u_is_con(s762, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s762, 0);
      u_value v_ins = u_field(s762, 1);
      u_value v_outs = u_field(s762, 2);
      u_value v_refs = u_field(s762, 3);
      d763 = 1;
      {
        p_check_46_call_proc(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d763) && u_is_con(s762, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s762, 0);
      u_value v_cases = u_field(s762, 1);
      d763 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_cases, v_t, &v_bs, r_s);
        u_value t769 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e770 = &t769;
        u_value l771 = v_bs;
        while (u_is_con(l771, &n_list_46_Cons)) {
          u_value v_br = u_field(l771, 0);
          if (u_truth(u_mk_bool(1))) {
            e770 = u_append(e770, f_check_46_case_of_1(v_br));
          }
          l771 = u_field(l771, 1);
        }
        p_check_46_coverage(*r_s, v_t, t769);
        u_value t772 = u_mk_con(&n_syntax_46_SMatch, 2, v_c, v_bs);
        *r_y = t772;
      }
    }
    if ((!d763) && u_is_con(s762, &n_syntax_46_SAssert)) {
      u_value v_cond = u_field(s762, 0);
      d763 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        u_value t773 = u_mk_con(&n_syntax_46_SAssert, 1, v_c);
        *r_y = t773;
      }
    }
    if ((!d763) && u_is_con(s762, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s762, 0);
      u_value v_a = u_field(s762, 1);
      d763 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_unify(v_t, v_u, r_s);
        u_value t774 = u_mk_con(&n_syntax_46_SRaise, 2, v_c, v_t);
        *r_y = t774;
      }
    }
    if ((!d763) && u_is_con(s762, &n_syntax_46_STry)) {
      u_value v_body = u_field(s762, 0);
      u_value v_handlers = u_field(s762, 1);
      d763 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_handlers, &v_hs, r_s);
        u_value t775 = u_mk_con(&n_syntax_46_STry, 2, v_l, v_hs);
        *r_y = t775;
      }
    }
    if (!d763) {
      u_no_case(s762);
    }
  }
}

u_value f_check_46_initialized_1(u_value v_xs) {
  u_value s776 = v_xs;
  u_value t777 = NULL;
  if ((!t777) && (u_is_con(s776, &n_list_46_Cons) && (u_is_con(u_field(s776, 0), &n_syntax_46_SAssign) && (u_is_con(u_field(u_field(s776, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s776, 0), 0), 1), &n_list_46_Nil))))) {
    u_value v_x = u_field(u_field(u_field(u_field(s776, 0), 0), 0), 0);
    u_value v_rest = u_field(s776, 1);
    t777 = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), f_check_46_initialized_1(v_rest));
  }
  if (!t777) {
    t777 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t777) {
    u_no_case(s776);
  }
  return t777;
}

void p_check_46_untyped(u_value v_xs, u_value v_inits) {
  {
    u_value s778 = v_xs;
    int d779 = 0;
    if ((!d779) && u_is_con(s778, &n_list_46_Nil)) {
      d779 = 1;
      {
      }
    }
    if ((!d779) && (u_is_con(s778, &n_list_46_Cons) && u_is_con(u_field(s778, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s778, 0), 0);
      u_value v_t = u_field(u_field(s778, 0), 1);
      u_value v_rest = u_field(s778, 1);
      d779 = 1;
      {
        u_value t780 = f_check_46_holes_1(v_t);
        if (u_truth(t780)) {
          t780 = f_prelude_46_not_1(f_list_46_contains_2(v_n, v_inits));
        }
        if (u_truth(t780)) {
          {
            u_raise(u_mk_con(&n_check_46_MissingType, 1, v_n), "check.TypeError");
          }
        }
        p_check_46_untyped(v_rest, v_inits);
      }
    }
    if (!d779) {
      u_no_case(s778);
    }
  }
}

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s781 = v_xs;
    int d782 = 0;
    if ((!d782) && u_is_con(s781, &n_list_46_Nil)) {
      d782 = 1;
      {
        u_value t783 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t783;
      }
    }
    if ((!d782) && u_is_con(s781, &n_list_46_Cons)) {
      u_value v_x = u_field(s781, 0);
      u_value v_more = u_field(s781, 1);
      d782 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_statements(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t784 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t784;
      }
    }
    if (!d782) {
      u_no_case(s781);
    }
  }
}

u_value f_check_46_os_procs_1(u_value v_c) {
  u_value t792;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_785 = U_LIT("os_args");
    static struct u_object l_os_read_786 = U_LIT("os_read");
    static struct u_object l_os_write_787 = U_LIT("os_write");
    static struct u_object l_os_print_788 = U_LIT("os_print");
    static struct u_object l_A_789 = U_LIT("A");
    static struct u_object l_A_790 = U_LIT("A");
    static struct u_object l_os_library_791 = U_LIT("os_library");
    t792 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_785), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_786), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_787), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_print_788), u_mk_tuple(4, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_789), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_790), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_791), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))));
  } else {
    t792 = u_mk_con(&n_list_46_Nil, 0);
  }
  return t792;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t793 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e794 = &t793;
  static struct u_object l_Program_795 = U_LIT("Program");
  u_value l796 = f_ctx_46_names_of_2(v_c, &l_Program_795);
  while (u_is_con(l796, &n_list_46_Cons)) {
    u_value v_d = u_field(l796, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e794 = u_append(e794, v_d);
    }
    l796 = u_field(l796, 1);
  }
  u_value s797 = t793;
  u_value t798 = NULL;
  if ((!t798) && u_is_con(s797, &n_list_46_Cons)) {
    u_value v_d = u_field(s797, 0);
    static struct u_object l_read_799 = U_LIT("read");
    static struct u_object l_library_800 = U_LIT("library");
    t798 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_799), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_800), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t798) {
    t798 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t798) {
    u_no_case(s797);
  }
  return t798;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value s801 = f_list_46_lookup_2(v_p, v_env);
    int d802 = 0;
    if ((!d802) && u_is_con(s801, &n_option_46_Some)) {
      u_value v_sc = u_field(s801, 0);
      d802 = 1;
      {
        p_check_46_call_variable(v_env, v_assignable, v_p, v_sc, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d802) && u_is_con(s801, &n_option_46_None)) {
      d802 = 1;
      {
        p_check_46_call_declared(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if (!d802) {
      u_no_case(s801);
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
    u_value t803 = u_mk_con(&n_syntax_46_SCall, 4, v_p, v_ins1, v_outs1, v_refs1);
    *r_y = t803;
  }
}

void p_check_46_call_declared(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s805 = f_check_46_visible_names_3(*r_s, v_p, u_mk_closure(lam804, 1, *r_s));
    int d806 = 0;
    if ((!d806) && (u_is_con(s805, &n_list_46_Cons) && u_is_con(u_field(s805, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s805, 0);
      d806 = 1;
      {
        u_value t807 = v_q1;
        v_q = t807;
      }
    }
    if ((!d806) && u_is_con(s805, &n_list_46_Nil)) {
      d806 = 1;
      {
        u_value t808 = v_p;
        v_q = t808;
      }
    }
    if (!d806) {
      u_value v_qs = s805;
      d806 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d806) {
      u_no_case(s805);
    }
    u_value s809 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_State_46_ctx_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_State_46_ctx_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_State_46_ctx_1(*r_s)))));
    int d810 = 0;
    if ((!d810) && u_is_con(s809, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s809, 0), 0);
      u_value v_is = u_field(u_field(s809, 0), 1);
      u_value v_os = u_field(u_field(s809, 0), 2);
      u_value v_rs = u_field(u_field(s809, 0), 3);
      d810 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t811 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e812 = &t811;
        u_value l813 = v_is;
        while (u_is_con(l813, &n_list_46_Cons)) {
          u_value v_t = u_field(l813, 0);
          if (u_truth(u_mk_bool(1))) {
            e812 = u_append(e812, f_check_46_subst_2(v_t, v_m));
          }
          l813 = u_field(l813, 1);
        }
        p_check_46_checks(v_env, v_ins, t811, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t814 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e815 = &t814;
        u_value l816 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l816, &n_list_46_Cons)) {
          u_value v_t = u_field(l816, 0);
          if (u_truth(u_mk_bool(1))) {
            e815 = u_append(e815, f_check_46_subst_2(v_t, v_m));
          }
          l816 = u_field(l816, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t814, r_s);
        u_value t817 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t817;
      }
    }
    if ((!d810) && u_is_con(s809, &n_option_46_None)) {
      d810 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d810) {
      u_no_case(s809);
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
    u_value s818 = v_cases;
    int d819 = 0;
    if ((!d819) && u_is_con(s818, &n_list_46_Nil)) {
      d819 = 1;
      {
        u_value t820 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t820;
      }
    }
    if ((!d819) && (u_is_con(s818, &n_list_46_Cons) && u_is_con(u_field(s818, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s818, 0), 0);
      u_value v_g = u_field(u_field(s818, 0), 1);
      u_value v_b = u_field(u_field(s818, 0), 2);
      u_value v_more = u_field(s818, 1);
      d819 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t821 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t821;
      }
    }
    if (!d819) {
      u_no_case(s818);
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
    u_value s822 = v_hs;
    int d823 = 0;
    if ((!d823) && u_is_con(s822, &n_list_46_Nil)) {
      d823 = 1;
      {
        u_value t824 = u_mk_con(&n_list_46_Nil, 0);
        *r_hs1 = t824;
      }
    }
    if ((!d823) && (u_is_con(s822, &n_list_46_Cons) && u_is_con(u_field(s822, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s822, 0), 0);
      u_value v_p = u_field(u_field(s822, 0), 1);
      u_value v_g = u_field(u_field(s822, 0), 2);
      u_value v_b = u_field(u_field(s822, 0), 3);
      u_value v_more = u_field(s822, 1);
      d823 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t825 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t825;
      }
    }
    if (!d823) {
      u_no_case(s822);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t826 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t826)) {
      t826 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t827 = t826;
    if (!u_truth(t827)) {
      t827 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t828 = t827;
    if (!u_truth(t828)) {
      t828 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n))), u_mk_con(&n_list_46_Nil, 0));
    }
    if (u_truth(t828)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t829 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t829)) {
      t829 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t830 = t829;
    if (!u_truth(t830)) {
      t830 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t830)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_831 = U_LIT("Bool");
    static struct u_object l_Int_832 = U_LIT("Int");
    static struct u_object l_Str_833 = U_LIT("Str");
    static struct u_object l_Char_834 = U_LIT("Char");
    static struct u_object l__45__62__835 = U_LIT("->");
    u_value t836 = f_list_46_contains_2(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_831, u_mk_con(&n_list_46_Cons, 2, &l_Int_832, u_mk_con(&n_list_46_Cons, 2, &l_Str_833, u_mk_con(&n_list_46_Cons, 2, &l_Char_834, u_mk_con(&n_list_46_Cons, 2, &l__45__62__835, u_mk_con(&n_list_46_Nil, 0)))))));
    if (!u_truth(t836)) {
      t836 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t837 = t836;
    if (!u_truth(t837)) {
      t837 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t837)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t838 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e839 = &t838;
    u_value l840 = v_ts;
    while (u_is_con(l840, &n_list_46_Cons)) {
      u_value v_t = u_field(l840, 0);
      if (u_truth(u_mk_bool(1))) {
        e839 = u_append(e839, f_check_46_free_1(v_t));
      }
      l840 = u_field(l840, 1);
    }
    u_value t841 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t838)), f_check_46_params_1(v_ps));
    v_undeclared = t841;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, u_mk_con(&n_list_46_Nil, 0)))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
  {
    u_value s842 = v_cs;
    int d843 = 0;
    if ((!d843) && u_is_con(s842, &n_list_46_Nil)) {
      d843 = 1;
      {
      }
    }
    if ((!d843) && (u_is_con(s842, &n_list_46_Cons) && u_is_con(u_field(u_field(s842, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s842, 0), 0);
      u_value v_n = u_field(u_field(u_field(s842, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s842, 0), 1), 1);
      u_value v_rest = u_field(s842, 1);
      d843 = 1;
      {
        u_value t844 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t844;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        u_value t845 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t845;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d843) && u_is_con(s842, &n_list_46_Cons)) {
      u_value v_rest = u_field(s842, 1);
      d843 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d843) {
      u_no_case(s842);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s846 = v_cs;
    int d847 = 0;
    if ((!d847) && u_is_con(s846, &n_list_46_Nil)) {
      d847 = 1;
      {
      }
    }
    if ((!d847) && (u_is_con(s846, &n_list_46_Cons) && u_is_con(u_field(u_field(s846, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s846, 0), 0);
      u_value v_n = u_field(u_field(u_field(s846, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s846, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s846, 0), 1), 2);
      u_value v_rest = u_field(s846, 1);
      d847 = 1;
      {
        u_value t848 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t848;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)));
        u_value t849 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_State_46_ctx_1(*r_s), v_n, v_ps, v_u));
        *r_s = t849;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d847) && u_is_con(s846, &n_list_46_Cons)) {
      u_value v_rest = u_field(s846, 1);
      d847 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d847) {
      u_no_case(s846);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
  {
    u_value s850 = v_cs;
    int d851 = 0;
    if ((!d851) && u_is_con(s850, &n_list_46_Nil)) {
      d851 = 1;
      {
      }
    }
    if ((!d851) && (u_is_con(s850, &n_list_46_Cons) && u_is_con(u_field(u_field(s850, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s850, 0), 0);
      u_value v_n = u_field(u_field(u_field(s850, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s850, 0), 1), 1);
      u_value v_ks = u_field(u_field(u_field(s850, 0), 1), 2);
      u_value v_rest = u_field(s850, 1);
      d851 = 1;
      {
        u_value t852 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t852;
        p_check_46_unique_ids(v_ps);
        u_value t853 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e854 = &t853;
        u_value l855 = v_ks;
        while (u_is_con(l855, &n_list_46_Cons)) {
          u_value v_k = u_field(l855, 0);
          if (u_truth(u_mk_bool(1))) {
            e854 = u_append(e854, f_syntax_46_Constr_46_name_1(v_k));
          }
          l855 = u_field(l855, 1);
        }
        u_value t856 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_State_46_ctx_1(*r_s), v_n, t853));
        *r_s = t856;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d851) && u_is_con(s850, &n_list_46_Cons)) {
      u_value v_rest = u_field(s850, 1);
      d851 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d851) {
      u_no_case(s850);
    }
  }
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s857 = v_ks;
    int d858 = 0;
    if ((!d858) && u_is_con(s857, &n_list_46_Nil)) {
      d858 = 1;
      {
      }
    }
    if ((!d858) && (u_is_con(s857, &n_list_46_Cons) && u_is_con(u_field(s857, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s857, 0), 0);
      u_value v_args = u_field(u_field(s857, 0), 1);
      u_value v_rest = u_field(s857, 1);
      d858 = 1;
      {
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t859 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_State_46_ctx_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t859;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d858) {
      u_no_case(s857);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s860 = v_xss;
  u_value t861 = NULL;
  if ((!t861) && u_is_con(s860, &n_list_46_Nil)) {
    t861 = v_r;
  }
  if ((!t861) && u_is_con(s860, &n_list_46_Cons)) {
    u_value v_xs = u_field(s860, 0);
    u_value v_rest = u_field(s860, 1);
    t861 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t861) {
    u_no_case(s860);
  }
  return t861;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s862 = v_xss;
  u_value t863 = NULL;
  if ((!t863) && u_is_con(s862, &n_list_46_Nil)) {
    t863 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t863) && u_is_con(s862, &n_list_46_Cons)) {
    u_value v_xs = u_field(s862, 0);
    t863 = v_xs;
  }
  if (!t863) {
    u_no_case(s862);
  }
  return t863;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s864 = v_xss;
    int d865 = 0;
    if ((!d865) && u_is_con(s864, &n_list_46_Nil)) {
      d865 = 1;
      {
        u_value t866 = u_mk_con(&n_list_46_Nil, 0);
        *r_yss = t866;
      }
    }
    if ((!d865) && u_is_con(s864, &n_list_46_Cons)) {
      u_value v_xs = u_field(s864, 0);
      u_value v_more = u_field(s864, 1);
      d865 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t867 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t867;
      }
    }
    if (!d865) {
      u_no_case(s864);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s868 = v_cs;
    int d869 = 0;
    if ((!d869) && u_is_con(s868, &n_list_46_Nil)) {
      d869 = 1;
      {
      }
    }
    if ((!d869) && (u_is_con(s868, &n_list_46_Cons) && u_is_con(u_field(u_field(s868, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s868, 0), 0);
      u_value v_n = u_field(u_field(u_field(s868, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s868, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s868, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s868, 0), 1), 3);
      u_value v_rest = u_field(s868, 1);
      d869 = 1;
      {
        u_value t870 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t870;
        p_check_46_overload(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), u_mk_con(&n_list_46_Nil, 0)));
        u_value t871 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t871;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d869) && u_is_con(s868, &n_list_46_Cons)) {
      u_value v_rest = u_field(s868, 1);
      d869 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d869) {
      u_no_case(s868);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s872 = v_cs;
    int d873 = 0;
    if ((!d873) && u_is_con(s872, &n_list_46_Nil)) {
      d873 = 1;
      {
      }
    }
    if ((!d873) && (u_is_con(s872, &n_list_46_Cons) && u_is_con(u_field(u_field(s872, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s872, 0), 0);
      u_value v_n = u_field(u_field(u_field(s872, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s872, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s872, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s872, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s872, 0), 1), 4);
      u_value v_rest = u_field(s872, 1);
      d873 = 1;
      {
        u_value t874 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t874;
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t875 = f_syntax_46_types_1(v_xs);
        v_ts = t875;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t876 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t876;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d873) && u_is_con(s872, &n_list_46_Cons)) {
      u_value v_rest = u_field(s872, 1);
      d873 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d873) {
      u_no_case(s872);
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
    u_value s877 = v_c;
    int d878 = 0;
    if ((!d878) && u_is_con(s877, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s877, 0);
      u_value v_ps = u_field(s877, 1);
      u_value v_bound = u_field(s877, 2);
      u_value v_res = u_field(s877, 3);
      u_value v_body = u_field(s877, 4);
      d878 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t879 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t879;
      }
    }
    if ((!d878) && u_is_con(s877, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s877, 0);
      u_value v_ps = u_field(s877, 1);
      u_value v_ins = u_field(s877, 2);
      u_value v_outs = u_field(s877, 3);
      u_value v_refs = u_field(s877, 4);
      u_value v_body = u_field(s877, 5);
      d878 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t880 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t880;
      }
    }
    if ((!d878) && u_is_con(s877, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s877, 0);
      u_value v_body = u_field(s877, 1);
      d878 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t881 = u_mk_con(&n_syntax_46_CRun, 2, v_n, v_y);
        *r_c1 = t881;
      }
    }
    if ((!d878) && u_is_con(s877, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s877, 0);
      u_value v_body = u_field(s877, 1);
      d878 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t882 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t882;
      }
    }
    if (!d878) {
      d878 = 1;
      {
        u_value t883 = v_c;
        *r_c1 = t883;
      }
    }
    if (!d878) {
      u_no_case(s877);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s884 = v_cs;
    int d885 = 0;
    if ((!d885) && u_is_con(s884, &n_list_46_Nil)) {
      d885 = 1;
      {
        u_value t886 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t886;
      }
    }
    if ((!d885) && u_is_con(s884, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s884, 0), 0);
      u_value v_c = u_field(u_field(s884, 0), 1);
      u_value v_more = u_field(s884, 1);
      d885 = 1;
      {
        u_value t887 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t887;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t888 = f_check_46_zonk_cmd_2(f_check_46_State_46_sub_1(*r_s), v_c1);
        v_c2 = t888;
        u_value s889 = f_check_46_unknown_in_cmd_1(v_c2);
        int d890 = 0;
        if ((!d890) && u_is_con(s889, &n_option_46_Some)) {
          u_value v_t = u_field(s889, 0);
          d890 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
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
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t891 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t891;
      }
    }
    if (!d885) {
      u_no_case(s884);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s892 = v_cs;
  u_value t893 = NULL;
  if ((!t893) && u_is_con(s892, &n_list_46_Nil)) {
    t893 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t893) && (u_is_con(s892, &n_list_46_Cons) && u_is_con(u_field(u_field(s892, 0), 1), &n_syntax_46_CRun))) {
    u_value v_n = u_field(u_field(u_field(s892, 0), 1), 0);
    u_value v_more = u_field(s892, 1);
    t893 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t893) && (u_is_con(s892, &n_list_46_Cons) && u_is_con(u_field(u_field(s892, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s892, 0), 1), 0);
    u_value v_more = u_field(s892, 1);
    t893 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t893) && u_is_con(s892, &n_list_46_Cons)) {
    u_value v_more = u_field(s892, 1);
    t893 = f_check_46_block_names_1(v_more);
  }
  if (!t893) {
    u_no_case(s892);
  }
  return t893;
}

u_value f_check_46_Checked_46_cmds_1(u_value v_x) {
  u_value s894 = v_x;
  u_value t895 = NULL;
  if ((!t895) && u_is_con(s894, &n_check_46_Checked)) {
    u_value v_y = u_field(s894, 0);
    t895 = v_y;
  }
  if (!t895) {
    u_no_case(s894);
  }
  return t895;
}

u_value f_check_46_Checked_46_ctx_1(u_value v_x) {
  u_value s896 = v_x;
  u_value t897 = NULL;
  if ((!t897) && u_is_con(s896, &n_check_46_Checked)) {
    u_value v_y = u_field(s896, 1);
    t897 = v_y;
  }
  if (!t897) {
    u_no_case(s896);
  }
  return t897;
}

u_value f_check_46_Checked_46_imports_1(u_value v_x) {
  u_value s898 = v_x;
  u_value t899 = NULL;
  if ((!t899) && u_is_con(s898, &n_check_46_Checked)) {
    u_value v_y = u_field(s898, 2);
    t899 = v_y;
  }
  if (!t899) {
    u_no_case(s898);
  }
  return t899;
}

u_value f_check_46_Checked_46_cmds_58__61__2(u_value v_x, u_value v_y) {
  u_value s900 = v_x;
  u_value t901 = NULL;
  if ((!t901) && u_is_con(s900, &n_check_46_Checked)) {
    u_value v_a1 = u_field(s900, 1);
    u_value v_a2 = u_field(s900, 2);
    t901 = u_mk_con(&n_check_46_Checked, 3, v_y, v_a1, v_a2);
  }
  if (!t901) {
    u_no_case(s900);
  }
  return t901;
}

u_value f_check_46_Checked_46_ctx_58__61__2(u_value v_x, u_value v_y) {
  u_value s902 = v_x;
  u_value t903 = NULL;
  if ((!t903) && u_is_con(s902, &n_check_46_Checked)) {
    u_value v_a0 = u_field(s902, 0);
    u_value v_a2 = u_field(s902, 2);
    t903 = u_mk_con(&n_check_46_Checked, 3, v_a0, v_y, v_a2);
  }
  if (!t903) {
    u_no_case(s902);
  }
  return t903;
}

u_value f_check_46_Checked_46_imports_58__61__2(u_value v_x, u_value v_y) {
  u_value s904 = v_x;
  u_value t905 = NULL;
  if ((!t905) && u_is_con(s904, &n_check_46_Checked)) {
    u_value v_a0 = u_field(s904, 0);
    u_value v_a1 = u_field(s904, 1);
    t905 = u_mk_con(&n_check_46_Checked, 3, v_a0, v_a1, v_y);
  }
  if (!t905) {
    u_no_case(s904);
  }
  return t905;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_cache = NULL;
    u_value t906 = u_mk_con(&n_list_46_Nil, 0);
    v_cache = t906;
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
    u_value t907 = f_ctx_46_empty_ctx_0();
    v_c = t907;
    u_value t908 = u_mk_con(&n_list_46_Nil, 0);
    v_imports = t908;
    u_value t909 = u_mk_con(&n_list_46_Nil, 0);
    v_seen = t909;
    u_value t910 = u_mk_con(&n_list_46_Nil, 0);
    v_cs = t910;
    p_check_46_check_units(f_check_46_modules_1(f_syntax_46_Program_46_cmds_1(v_p)), &v_c, &v_imports, &v_seen, &v_cs, r_cache);
    p_check_46_check_unit(f_check_46_own_decls_2(v_main, f_syntax_46_Program_46_cmds_1(v_p)), v_main, v_c, v_imports, &v_own);
    u_value t911 = u_mk_con(&n_syntax_46_Program, 1, f_list_46__43__43__2(v_cs, f_check_46_Checked_46_cmds_1(v_own)));
    *r_q = t911;
  }
}

void p_check_46_check_units(u_value v_us, u_value *r_c, u_value *r_imports, u_value *r_seen, u_value *r_cs, u_value *r_cache) {
  {
    u_value v_key = NULL;
    u_value v_checked = NULL;
    u_value v_i = NULL;
    u_value s912 = v_us;
    int d913 = 0;
    if ((!d913) && u_is_con(s912, &n_list_46_Nil)) {
      d913 = 1;
      {
      }
    }
    if ((!d913) && u_is_con(s912, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s912, 0), 0);
      u_value v_ds = u_field(u_field(s912, 0), 1);
      u_value v_rest = u_field(s912, 1);
      d913 = 1;
      {
        static struct u_object l_prelude_914 = U_LIT("prelude");
        u_value t915 = u_mk_tuple(3, v_m, v_ds, f_check_46_positions_of_2(u_mk_con(&n_list_46_Cons, 2, &l_prelude_914, f_check_46_imports_in_2(v_ds, v_m)), *r_seen));
        v_key = t915;
        u_value s916 = f_check_46_position_in_3(v_key, *r_cache, u_mk_int(0));
        int d917 = 0;
        if ((!d917) && u_is_con(s916, &n_option_46_Some)) {
          u_value v_k = u_field(u_field(s916, 0), 0);
          u_value v_done = u_field(u_field(s916, 0), 1);
          d917 = 1;
          {
            u_value t918 = v_k;
            u_value t919 = v_done;
            v_i = t918;
            v_checked = t919;
          }
        }
        if ((!d917) && u_is_con(s916, &n_option_46_None)) {
          d917 = 1;
          {
            p_check_46_check_unit(v_ds, v_m, *r_c, *r_imports, &v_checked);
            u_value t920 = f_list_46_length_1(*r_cache);
            u_value t921 = f_list_46__43__43__2(*r_cache, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_key, v_checked), u_mk_con(&n_list_46_Nil, 0)));
            v_i = t920;
            *r_cache = t921;
          }
        }
        if (!d917) {
          u_no_case(s916);
        }
        u_value t922 = f_ctx_46_ctx_union_2(*r_c, f_check_46_Checked_46_ctx_1(v_checked));
        u_value t923 = f_list_46__43__43__2(*r_imports, f_check_46_Checked_46_imports_1(v_checked));
        u_value t924 = f_list_46__43__43__2(*r_cs, f_check_46_Checked_46_cmds_1(v_checked));
        *r_c = t922;
        *r_imports = t923;
        *r_cs = t924;
        u_value t925 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e926 = &t925;
        u_value l927 = f_list_46_firsts_1(f_check_46_Checked_46_imports_1(v_checked));
        while (u_is_con(l927, &n_list_46_Cons)) {
          u_value v_n = u_field(l927, 0);
          if (u_truth(u_mk_bool(1))) {
            e926 = u_append(e926, u_mk_tuple(2, v_n, v_i));
          }
          l927 = u_field(l927, 1);
        }
        u_value t928 = f_list_46__43__43__2(t925, *r_seen);
        *r_seen = t928;
        p_check_46_check_units(v_rest, r_c, r_imports, r_seen, r_cs, r_cache);
      }
    }
    if (!d913) {
      u_no_case(s912);
    }
  }
}

u_value f_check_46_positions_of_2(u_value v_ms, u_value v_seen) {
  u_value s929 = v_ms;
  u_value t930 = NULL;
  if ((!t930) && u_is_con(s929, &n_list_46_Nil)) {
    t930 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t930) && u_is_con(s929, &n_list_46_Cons)) {
    u_value v_m = u_field(s929, 0);
    u_value v_rest = u_field(s929, 1);
    u_value s931 = f_list_46_lookup_2(v_m, v_seen);
    u_value t932 = NULL;
    if ((!t932) && u_is_con(s931, &n_option_46_Some)) {
      u_value v_i = u_field(s931, 0);
      t932 = u_mk_con(&n_list_46_Cons, 2, v_i, f_check_46_positions_of_2(v_rest, v_seen));
    }
    if ((!t932) && u_is_con(s931, &n_option_46_None)) {
      t932 = f_check_46_positions_of_2(v_rest, v_seen);
    }
    if (!t932) {
      u_no_case(s931);
    }
    t930 = t932;
  }
  if (!t930) {
    u_no_case(s929);
  }
  return t930;
}

u_value f_check_46_position_in_3(u_value v_key, u_value v_cache, u_value v_i) {
  u_value s933 = v_cache;
  u_value t934 = NULL;
  if ((!t934) && u_is_con(s933, &n_list_46_Nil)) {
    t934 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t934) && u_is_con(s933, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s933, 0), 0);
    u_value v_checked = u_field(u_field(s933, 0), 1);
    u_value v_rest = u_field(s933, 1);
    u_value t935;
    if (u_truth(u_eq(v_k, v_key))) {
      t935 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_i, v_checked));
    } else {
      t935 = f_check_46_position_in_3(v_key, v_rest, u_add(v_i, u_mk_int(1)));
    }
    t934 = t935;
  }
  if (!t934) {
    u_no_case(s933);
  }
  return t934;
}

void p_check_46_check_unit(u_value v_ds, u_value v_main, u_value v_before, u_value v_imports, u_value *r_checked) {
  {
    u_value v_own = NULL;
    u_value v_s = NULL;
    u_value v_cs = NULL;
    u_value t936 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e937 = &t936;
    u_value l938 = f_list_46_distinct_1(f_list_46_firsts_1(v_ds));
    while (u_is_con(l938, &n_list_46_Cons)) {
      u_value v_m = u_field(l938, 0);
      if (u_truth(u_mk_bool(1))) {
        e937 = u_append(e937, u_mk_tuple(2, v_m, f_check_46_imports_in_2(v_ds, v_m)));
      }
      l938 = u_field(l938, 1);
    }
    u_value t939 = f_list_46__43__43__2(v_imports, t936);
    v_own = t939;
    u_value t940 = u_mk_con(&n_check_46_State, 6, v_before, f_map_46_empty_0(), u_mk_int(0), v_main, u_mk_con(&n_list_46_Nil, 0), f_check_46_visibility_2(v_own, v_main));
    v_s = t940;
    p_check_46_declare_datas(v_ds, &v_s);
    p_check_46_declare_sorts(v_ds, &v_s);
    p_check_46_declare_constrs(v_ds, &v_s);
    p_check_46_declare_funs(v_ds, &v_s);
    p_check_46_declare_procs(v_ds, &v_s);
    p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
    p_check_46_commands(v_ds, &v_cs, &v_s);
    u_value t941 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e942 = &t941;
    u_value l943 = f_list_46_distinct_1(f_list_46_firsts_1(v_ds));
    while (u_is_con(l943, &n_list_46_Cons)) {
      u_value v_m = u_field(l943, 0);
      if (u_truth(u_mk_bool(1))) {
        e942 = u_append(e942, u_mk_tuple(2, v_m, f_check_46_imports_in_2(v_ds, v_m)));
      }
      l943 = u_field(l943, 1);
    }
    u_value t944 = u_mk_con(&n_check_46_Checked, 3, v_cs, f_ctx_46_ctx_of_2(f_check_46_State_46_ctx_1(v_s), f_list_46_distinct_1(f_list_46_firsts_1(v_ds))), t941);
    *r_checked = t944;
  }
}

u_value f_check_46_modules_1(u_value v_cs) {
  u_value s945 = v_cs;
  u_value t946 = NULL;
  if ((!t946) && u_is_con(s945, &n_list_46_Nil)) {
    t946 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t946) && (u_is_con(s945, &n_list_46_Cons) && (u_is_con(u_field(s945, 0), &n_syntax_46_CModule) && u_equals(u_field(u_field(s945, 0), 0), u_mk_lit("prelude"))))) {
    u_value v_ds = u_field(u_field(s945, 0), 1);
    u_value v_rest = u_field(s945, 1);
    static struct u_object l_prelude_947 = U_LIT("prelude");
    static struct u_object l_prelude_948 = U_LIT("prelude");
    t946 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &l_prelude_947, f_check_46_flatten_2(&l_prelude_948, v_ds)), f_check_46_modules_1(v_rest));
  }
  if ((!t946) && (u_is_con(s945, &n_list_46_Cons) && u_is_con(u_field(s945, 0), &n_syntax_46_CModule))) {
    u_value v_n = u_field(u_field(s945, 0), 0);
    u_value v_ds = u_field(u_field(s945, 0), 1);
    u_value v_rest = u_field(s945, 1);
    t946 = f_list_46__43__43__2(f_check_46_modules_1(v_ds), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_check_46_own_decls_2(v_n, v_ds)), f_check_46_modules_1(v_rest)));
  }
  if ((!t946) && u_is_con(s945, &n_list_46_Cons)) {
    u_value v_rest = u_field(s945, 1);
    t946 = f_check_46_modules_1(v_rest);
  }
  if (!t946) {
    u_no_case(s945);
  }
  return t946;
}

u_value f_check_46_own_decls_2(u_value v_m, u_value v_cs) {
  u_value t949 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e950 = &t949;
  u_value l951 = v_cs;
  while (u_is_con(l951, &n_list_46_Cons)) {
    u_value v_c = u_field(l951, 0);
    if (u_truth(f_prelude_46_not_1(f_check_46_is_module_1(v_c)))) {
      e950 = u_append(e950, u_mk_tuple(2, v_m, f_check_46_qualify_2(v_m, v_c)));
    }
    l951 = u_field(l951, 1);
  }
  return t949;
}

u_value f_check_46_is_module_1(u_value v_c) {
  u_value s952 = v_c;
  u_value t953 = NULL;
  if ((!t953) && u_is_con(s952, &n_syntax_46_CModule)) {
    t953 = u_mk_bool(1);
  }
  if (!t953) {
    t953 = u_mk_bool(0);
  }
  if (!t953) {
    u_no_case(s952);
  }
  return t953;
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_954 = U_LIT("main");
    u_value t955 = f_check_46_start_2(&l_main_954, u_mk_con(&n_list_46_Nil, 0));
    v_s = t955;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t956 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(v_s), v_u);
    *r_t = t956;
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
  static struct u_object l_List_957 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_957), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_958 = U_LIT("list");
  static struct u_object l_List_959 = U_LIT("List");
  static struct u_object l_A_960 = U_LIT("A");
  static struct u_object l_Nil_961 = U_LIT("Nil");
  static struct u_object l_Cons_962 = U_LIT("Cons");
  static struct u_object l_A_963 = U_LIT("A");
  static struct u_object l_A_964 = U_LIT("A");
  static struct u_object l_list_965 = U_LIT("list");
  static struct u_object l_len_966 = U_LIT("len");
  static struct u_object l_A_967 = U_LIT("A");
  static struct u_object l_xs_968 = U_LIT("xs");
  static struct u_object l_A_969 = U_LIT("A");
  static struct u_object l_xs_970 = U_LIT("xs");
  static struct u_object l_Nil_971 = U_LIT("Nil");
  static struct u_object l_Cons_972 = U_LIT("Cons");
  static struct u_object l_ys_973 = U_LIT("ys");
  static struct u_object l__43__974 = U_LIT("+");
  static struct u_object l_len_975 = U_LIT("len");
  static struct u_object l_ys_976 = U_LIT("ys");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_958, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_959), u_mk_con(&n_list_46_Cons, 2, &l_A_960, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_961), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_962), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_963), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_964)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_965), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_966), u_mk_con(&n_list_46_Cons, 2, &l_A_967, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_968), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_969))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_970), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_971), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_972), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_973), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__974, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_975, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_976), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s977 = v_t;
  u_value t978 = NULL;
  if ((!t978) && u_is_con(s977, &n_c_46_CtVoid)) {
    static struct u_object l_void_979 = U_LIT("void");
    t978 = &l_void_979;
  }
  if ((!t978) && u_is_con(s977, &n_c_46_CtInt)) {
    static struct u_object l_int_980 = U_LIT("int");
    t978 = &l_int_980;
  }
  if ((!t978) && u_is_con(s977, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s977, 0);
    t978 = v_n;
  }
  if ((!t978) && (u_is_con(s977, &n_c_46_CtPtr) && u_is_con(u_field(s977, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s977, 0), 0);
    static struct u_object l__42__981 = U_LIT("*");
    t978 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__981);
  }
  if ((!t978) && u_is_con(s977, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s977, 0);
    static struct u_object l__32__42__982 = U_LIT(" *");
    t978 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__982);
  }
  if (!t978) {
    u_no_case(s977);
  }
  return t978;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s983 = v_t;
  u_value t984 = NULL;
  if ((!t984) && u_is_con(s983, &n_c_46_CtPtr)) {
    t984 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t984) {
    static struct u_object l__32__985 = U_LIT(" ");
    t984 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__985, v_x));
  }
  if (!t984) {
    u_no_case(s983);
  }
  return t984;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s986 = v_e;
  u_value t987 = NULL;
  if ((!t987) && u_is_con(s986, &n_c_46_CeVar)) {
    u_value v_x = u_field(s986, 0);
    t987 = v_x;
  }
  if ((!t987) && u_is_con(s986, &n_c_46_CeNum)) {
    u_value v_n = u_field(s986, 0);
    t987 = u_str_show(v_n);
  }
  if ((!t987) && u_is_con(s986, &n_c_46_CeStr)) {
    u_value v_s = u_field(s986, 0);
    t987 = u_str_quote(v_s);
  }
  if ((!t987) && u_is_con(s986, &n_c_46_CeCall)) {
    u_value v_f = u_field(s986, 0);
    u_value v_args = u_field(s986, 1);
    static struct u_object l__40__988 = U_LIT("(");
    static struct u_object l__44__32__989 = U_LIT(", ");
    u_value t990 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e991 = &t990;
    u_value l992 = v_args;
    while (u_is_con(l992, &n_list_46_Cons)) {
      u_value v_a = u_field(l992, 0);
      if (u_truth(u_mk_bool(1))) {
        e991 = u_append(e991, f_c_46_c_expr_1(v_a));
      }
      l992 = u_field(l992, 1);
    }
    static struct u_object l__41__993 = U_LIT(")");
    t987 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__988, u_concat(u_str_join(&l__44__32__989, t990), &l__41__993)));
  }
  if ((!t987) && u_is_con(s986, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s986, 0);
    u_value v_a = u_field(s986, 1);
    t987 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t987) && u_is_con(s986, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s986, 0);
    u_value v_a = u_field(s986, 1);
    u_value v_b = u_field(s986, 2);
    static struct u_object l__32__994 = U_LIT(" ");
    static struct u_object l__32__995 = U_LIT(" ");
    t987 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__994, u_concat(v_o, u_concat(&l__32__995, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t987) && u_is_con(s986, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s986, 0);
    u_value v_i = u_field(s986, 1);
    static struct u_object l__91__996 = U_LIT("[");
    static struct u_object l__93__997 = U_LIT("]");
    t987 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__996, u_concat(f_c_46_c_expr_1(v_i), &l__93__997)));
  }
  if ((!t987) && u_is_con(s986, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s986, 0);
    u_value v_f = u_field(s986, 1);
    static struct u_object l__45__62__998 = U_LIT("->");
    t987 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__998, v_f));
  }
  if ((!t987) && u_is_con(s986, &n_c_46_CeCast)) {
    u_value v_t = u_field(s986, 0);
    u_value v_a = u_field(s986, 1);
    static struct u_object l__40__999 = U_LIT("(");
    static struct u_object l__41__1000 = U_LIT(")");
    t987 = u_concat(&l__40__999, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__1000, f_c_46_c_operand_1(v_a))));
  }
  if ((!t987) && u_is_con(s986, &n_c_46_CeCond)) {
    u_value v_c = u_field(s986, 0);
    u_value v_a = u_field(s986, 1);
    u_value v_b = u_field(s986, 2);
    static struct u_object l__32__63__32__1001 = U_LIT(" ? ");
    static struct u_object l__32__58__32__1002 = U_LIT(" : ");
    t987 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__1001, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__1002, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t987) && u_is_con(s986, &n_c_46_CeInit)) {
    u_value v_es = u_field(s986, 0);
    static struct u_object l__123__1003 = U_LIT("{");
    static struct u_object l__44__32__1004 = U_LIT(", ");
    u_value t1005 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1006 = &t1005;
    u_value l1007 = v_es;
    while (u_is_con(l1007, &n_list_46_Cons)) {
      u_value v_a = u_field(l1007, 0);
      if (u_truth(u_mk_bool(1))) {
        e1006 = u_append(e1006, f_c_46_c_expr_1(v_a));
      }
      l1007 = u_field(l1007, 1);
    }
    static struct u_object l__125__1008 = U_LIT("}");
    t987 = u_concat(&l__123__1003, u_concat(u_str_join(&l__44__32__1004, t1005), &l__125__1008));
  }
  if (!t987) {
    u_no_case(s986);
  }
  return t987;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s1009 = v_e;
  u_value t1010 = NULL;
  if ((!t1010) && u_is_con(s1009, &n_c_46_CeVar)) {
    t1010 = f_c_46_c_expr_1(v_e);
  }
  if ((!t1010) && u_is_con(s1009, &n_c_46_CeNum)) {
    u_value v_n = u_field(s1009, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t1010 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t1010) && u_is_con(s1009, &n_c_46_CeStr)) {
    t1010 = f_c_46_c_expr_1(v_e);
  }
  if ((!t1010) && u_is_con(s1009, &n_c_46_CeCall)) {
    t1010 = f_c_46_c_expr_1(v_e);
  }
  if ((!t1010) && u_is_con(s1009, &n_c_46_CeIndex)) {
    t1010 = f_c_46_c_expr_1(v_e);
  }
  if ((!t1010) && u_is_con(s1009, &n_c_46_CeArrow)) {
    t1010 = f_c_46_c_expr_1(v_e);
  }
  if (!t1010) {
    static struct u_object l__40__1011 = U_LIT("(");
    static struct u_object l__41__1012 = U_LIT(")");
    t1010 = u_concat(&l__40__1011, u_concat(f_c_46_c_expr_1(v_e), &l__41__1012));
  }
  if (!t1010) {
    u_no_case(s1009);
  }
  return t1010;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s1013 = v_s;
  u_value t1014 = NULL;
  if ((!t1014) && u_is_con(s1013, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s1013, 0);
    static struct u_object l__59__1015 = U_LIT(";");
    t1014 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__1015)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1014) && (u_is_con(s1013, &n_c_46_CsDecl) && u_is_con(u_field(s1013, 2), &n_option_46_None))) {
    u_value v_t = u_field(s1013, 0);
    u_value v_x = u_field(s1013, 1);
    static struct u_object l__59__1016 = U_LIT(";");
    t1014 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__1016)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1014) && (u_is_con(s1013, &n_c_46_CsDecl) && u_is_con(u_field(s1013, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s1013, 0);
    u_value v_x = u_field(s1013, 1);
    u_value v_e = u_field(u_field(s1013, 2), 0);
    static struct u_object l__32__61__32__1017 = U_LIT(" = ");
    static struct u_object l__59__1018 = U_LIT(";");
    t1014 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__1017, u_concat(f_c_46_c_expr_1(v_e), &l__59__1018)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1014) && u_is_con(s1013, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s1013, 0);
    u_value v_e = u_field(s1013, 1);
    static struct u_object l__32__61__32__1019 = U_LIT(" = ");
    static struct u_object l__59__1020 = U_LIT(";");
    t1014 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__1019, u_concat(f_c_46_c_expr_1(v_e), &l__59__1020)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1014) && (u_is_con(s1013, &n_c_46_CsIf) && u_is_con(u_field(s1013, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s1013, 0);
    u_value v_l = u_field(s1013, 1);
    static struct u_object l_if_32__40__1021 = U_LIT("if (");
    static struct u_object l__41__32__123__1022 = U_LIT(") {");
    static struct u_object l__32__32__1023 = U_LIT("  ");
    static struct u_object l__125__1024 = U_LIT("}");
    t1014 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__1021, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__1022))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__1023), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__1024), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t1014) && u_is_con(s1013, &n_c_46_CsIf)) {
    u_value v_c = u_field(s1013, 0);
    u_value v_l = u_field(s1013, 1);
    u_value v_r = u_field(s1013, 2);
    static struct u_object l_if_32__40__1025 = U_LIT("if (");
    static struct u_object l__41__32__123__1026 = U_LIT(") {");
    static struct u_object l__32__32__1027 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__1028 = U_LIT("} else {");
    static struct u_object l__32__32__1029 = U_LIT("  ");
    static struct u_object l__125__1030 = U_LIT("}");
    t1014 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__1025, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__1026))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__1027), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__1028), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__1029), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__1030), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t1014) && u_is_con(s1013, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s1013, 0);
    u_value v_b = u_field(s1013, 1);
    static struct u_object l_while_32__40__1031 = U_LIT("while (");
    static struct u_object l__41__32__123__1032 = U_LIT(") {");
    static struct u_object l__32__32__1033 = U_LIT("  ");
    static struct u_object l__125__1034 = U_LIT("}");
    t1014 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__1031, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__1032))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__1033), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__1034), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t1014) && (u_is_con(s1013, &n_c_46_CsReturn) && u_is_con(u_field(s1013, 0), &n_option_46_None))) {
    static struct u_object l_return_59__1035 = U_LIT("return;");
    t1014 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__1035), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1014) && (u_is_con(s1013, &n_c_46_CsReturn) && u_is_con(u_field(s1013, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s1013, 0), 0);
    static struct u_object l_return_32__1036 = U_LIT("return ");
    static struct u_object l__59__1037 = U_LIT(";");
    t1014 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__1036, u_concat(f_c_46_c_expr_1(v_e), &l__59__1037))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1014) && u_is_con(s1013, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s1013, 0);
    static struct u_object l__123__1038 = U_LIT("{");
    static struct u_object l__32__32__1039 = U_LIT("  ");
    static struct u_object l__125__1040 = U_LIT("}");
    t1014 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__1038), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__1039), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__1040), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1014) {
    u_no_case(s1013);
  }
  return t1014;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t1041 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1042 = &t1041;
  u_value l1043 = v_ss;
  while (u_is_con(l1043, &n_list_46_Cons)) {
    u_value v_s = u_field(l1043, 0);
    if (u_truth(u_mk_bool(1))) {
      e1042 = u_append(e1042, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l1043 = u_field(l1043, 1);
  }
  return f_list_46_concat_1(t1041);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s1044 = v_ps;
  u_value t1045 = NULL;
  if ((!t1045) && u_is_con(s1044, &n_list_46_Nil)) {
    static struct u_object l_void_1046 = U_LIT("void");
    t1045 = &l_void_1046;
  }
  if (!t1045) {
    static struct u_object l__44__32__1047 = U_LIT(", ");
    u_value t1048 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1049 = &t1048;
    u_value l1050 = v_ps;
    while (u_is_con(l1050, &n_list_46_Cons)) {
      u_value v_p = u_field(l1050, 0);
      if (u_truth(u_mk_bool(1))) {
        e1049 = u_append(e1049, f_c_46_c_param_1(v_p));
      }
      l1050 = u_field(l1050, 1);
    }
    t1045 = u_str_join(&l__44__32__1047, t1048);
  }
  if (!t1045) {
    u_no_case(s1044);
  }
  return t1045;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s1051 = v_p;
  u_value t1052 = NULL;
  if (!t1052) {
    u_value v_t = u_field(s1051, 0);
    u_value v_x = u_field(s1051, 1);
    t1052 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t1052) {
    u_no_case(s1051);
  }
  return t1052;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s1053 = v_d;
  u_value t1054 = NULL;
  if ((!t1054) && u_is_con(s1053, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s1053, 0);
    static struct u_object l__35_include_32__1055 = U_LIT("#include ");
    t1054 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__1055, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1054) && u_is_con(s1053, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s1053, 0);
    u_value v_f = u_field(s1053, 1);
    u_value v_ps = u_field(s1053, 2);
    static struct u_object l__40__1056 = U_LIT("(");
    static struct u_object l__41__59__1057 = U_LIT(");");
    t1054 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__1056, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__1057))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1054) && u_is_con(s1053, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s1053, 0);
    u_value v_f = u_field(s1053, 1);
    u_value v_ps = u_field(s1053, 2);
    u_value v_b = u_field(s1053, 3);
    static struct u_object l__40__1058 = U_LIT("(");
    static struct u_object l__41__32__123__1059 = U_LIT(") {");
    static struct u_object l__32__32__1060 = U_LIT("  ");
    static struct u_object l__125__1061 = U_LIT("}");
    t1054 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__1058, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__1059))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__1060, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__1061, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t1054) && (u_is_con(s1053, &n_c_46_CdGlobal) && u_is_con(u_field(s1053, 2), &n_option_46_None))) {
    u_value v_t = u_field(s1053, 0);
    u_value v_x = u_field(s1053, 1);
    static struct u_object l__59__1062 = U_LIT(";");
    t1054 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__1062), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1054) && (u_is_con(s1053, &n_c_46_CdGlobal) && u_is_con(u_field(s1053, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s1053, 0);
    u_value v_x = u_field(s1053, 1);
    u_value v_e = u_field(u_field(s1053, 2), 0);
    static struct u_object l__32__61__32__1063 = U_LIT(" = ");
    static struct u_object l__59__1064 = U_LIT(";");
    t1054 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__1063, u_concat(f_c_46_c_expr_1(v_e), &l__59__1064))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1054) {
    u_no_case(s1053);
  }
  return t1054;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s1065 = v_f;
  u_value t1066 = NULL;
  if ((!t1066) && u_is_con(s1065, &n_c_46_CFile)) {
    u_value v_ds = u_field(s1065, 0);
    static struct u_object l__10__10__1067 = U_LIT("\n\n");
    u_value t1068 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1069 = &t1068;
    u_value l1070 = v_ds;
    while (u_is_con(l1070, &n_list_46_Cons)) {
      u_value v_d = u_field(l1070, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__1071 = U_LIT("\n");
        e1069 = u_append(e1069, u_str_join(&l__10__1071, f_c_46_c_decl_1(v_d)));
      }
      l1070 = u_field(l1070, 1);
    }
    static struct u_object l__10__1072 = U_LIT("\n");
    t1066 = u_concat(u_str_join(&l__10__10__1067, t1068), &l__10__1072);
  }
  if (!t1066) {
    u_no_case(s1065);
  }
  return t1066;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__1073 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__1073, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__123__125__44__59__46__1074 = U_LIT("()[]{},;.");
  return u_str_contains(&l__40__41__91__93__123__125__44__59__46__1074, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t1075 = u_is_letter(v_c);
  if (!u_truth(t1075)) {
    t1075 = u_is_digit(v_c);
  }
  u_value t1076 = t1075;
  if (!u_truth(t1076)) {
    t1076 = u_eq(v_c, u_mk_char(95));
  }
  return t1076;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t1077;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t1077 = u_str_get(v_s, v_i);
  } else {
    t1077 = u_chr(u_mk_int(0));
  }
  return t1077;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s1078 = v_c;
  u_value t1079 = NULL;
  if ((!t1079) && u_equals(s1078, u_mk_char(34))) {
    t1079 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t1079) && u_equals(s1078, u_mk_char(39))) {
    t1079 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t1079) && u_equals(s1078, u_mk_char(92))) {
    t1079 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t1079) && u_equals(s1078, u_mk_char(110))) {
    t1079 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t1079) && u_equals(s1078, u_mk_char(116))) {
    t1079 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t1079) && u_equals(s1078, u_mk_char(114))) {
    t1079 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t1079) {
    t1079 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1079) {
    u_no_case(s1078);
  }
  return t1079;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t1080;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t1080 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t1080 = v_i;
  }
  return t1080;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t1081 = u_mk_int(0);
    u_value t1082 = u_mk_int(1);
    u_value t1083 = u_mk_int(1);
    v_j = t1081;
    *r_line = t1082;
    *r_column = t1083;
    u_value t1088 = u_lt(v_j, v_i);
    while (u_truth(t1088)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t1084 = u_add(*r_line, u_mk_int(1));
            u_value t1085 = u_mk_int(1);
            *r_line = t1084;
            *r_column = t1085;
          }
        } else {
          {
            u_value t1086 = u_add(*r_column, u_mk_int(1));
            *r_column = t1086;
          }
        }
        u_value t1087 = u_add(v_j, u_mk_int(1));
        v_j = t1087;
      }
      t1088 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t1089 = *r_i;
    u_value t1090 = u_add(*r_i, u_mk_int(1));
    u_value t1091 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t1089;
    *r_i = t1090;
    v_parts = t1091;
    u_value t1092 = *r_i;
    v_from = t1092;
    u_value t1103 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t1103)) {
      {
        u_value t1093 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t1093;
        u_value t1094 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t1094)) {
          t1094 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t1094)) {
          {
            static struct u_object l_unterminated_32_lit_1095 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_1095, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s1096 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d1097 = 0;
            if ((!d1097) && u_is_con(s1096, &n_option_46_Some)) {
              u_value v_d = u_field(s1096, 0);
              d1097 = 1;
              {
                u_value t1098 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t1099 = u_add(*r_i, u_mk_int(2));
                v_parts = t1098;
                *r_i = t1099;
                u_value t1100 = *r_i;
                v_from = t1100;
              }
            }
            if ((!d1097) && u_is_con(s1096, &n_option_46_None)) {
              d1097 = 1;
              {
                static struct u_object l_invalid_32_escape_1101 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_1101, *r_i), "lex.LexError");
              }
            }
            if (!d1097) {
              u_no_case(s1096);
            }
          }
        } else {
          {
            u_value t1102 = u_add(*r_i, u_mk_int(1));
            *r_i = t1102;
          }
        }
      }
      t1103 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__1104 = U_LIT("");
    u_value t1105 = u_str_join(&l__1104, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t1106 = u_add(*r_i, u_mk_int(1));
    *r_text = t1105;
    *r_i = t1106;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1107 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t1107;
    u_value t1108 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1109 = v_j;
    *r_t = t1108;
    *r_i = t1109;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1110 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t1110;
    u_value s1111 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d1112 = 0;
    if ((!d1112) && u_is_con(s1111, &n_option_46_Some)) {
      u_value v_n = u_field(s1111, 0);
      d1112 = 1;
      {
        u_value t1113 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t1114 = v_j;
        *r_t = t1113;
        *r_i = t1114;
      }
    }
    if ((!d1112) && u_is_con(s1111, &n_option_46_None)) {
      d1112 = 1;
      {
        static struct u_object l_invalid_32_number_1115 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_1115, *r_i), "lex.LexError");
      }
    }
    if (!d1112) {
      u_no_case(s1111);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1116 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t1116;
    u_value t1117 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1118 = v_j;
    *r_t = t1117;
    *r_i = t1118;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t1119 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t1119;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t1120 = *r_i;
    v_start = t1120;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_1121 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_1121, v_start), "lex.LexError");
      }
    }
    u_value t1122 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t1122;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t1123 = u_str_get(v_s, *r_i);
    v_c = t1123;
    u_value s1124 = v_c;
    int d1125 = 0;
    if (!d1125) {
      u_value t1126 = u_is_letter(v_c);
      if (!u_truth(t1126)) {
        t1126 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t1126)) {
        d1125 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d1125) {
      if (u_truth(u_is_digit(v_c))) {
        d1125 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d1125) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d1125 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d1125) && u_equals(s1124, u_mk_char(34))) {
      d1125 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d1125) && u_equals(s1124, u_mk_char(39))) {
      d1125 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d1125) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d1125 = 1;
        {
          u_value t1127 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t1128 = u_add(*r_i, u_mk_int(1));
          *r_t = t1127;
          *r_i = t1128;
        }
      }
    }
    if (!d1125) {
      d1125 = 1;
      {
        static struct u_object l_unexpected_32_chara_1129 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_1129, *r_i), "lex.LexError");
      }
    }
    if (!d1125) {
      u_no_case(s1124);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t1130 = u_mk_int(0);
    v_i = t1130;
    u_value t1131 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1131;
    u_value t1138 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t1138)) {
      {
        u_value t1132 = v_i;
        v_start = t1132;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t1133 = u_add(v_i, u_mk_int(1));
            v_i = t1133;
          }
        } else {
          u_value t1134 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t1134)) {
            t1134 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t1134)) {
            {
              u_value t1136 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam1135, 0));
              v_i = t1136;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t1137 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t1137;
            }
          }
        }
      }
      t1138 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t1139 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t1139;
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
  u_value s1140 = v_s;
  u_value t1141 = NULL;
  if ((!t1141) && u_is_con(s1140, &n_set_46_SLeaf)) {
    t1141 = u_mk_bool(0);
  }
  if ((!t1141) && u_is_con(s1140, &n_set_46_SNode)) {
    u_value v_l = u_field(s1140, 0);
    u_value v_y = u_field(s1140, 1);
    u_value v_r = u_field(s1140, 2);
    u_value s1142 = u_compare(v_x, v_y);
    u_value t1143 = NULL;
    if ((!t1143) && u_equals(s1142, u_mk_int(-1))) {
      t1143 = f_set_46_member_2(v_x, v_l);
    }
    if ((!t1143) && u_equals(s1142, u_mk_int(1))) {
      t1143 = f_set_46_member_2(v_x, v_r);
    }
    if (!t1143) {
      t1143 = u_mk_bool(1);
    }
    if (!t1143) {
      u_no_case(s1142);
    }
    t1141 = t1143;
  }
  if (!t1141) {
    u_no_case(s1140);
  }
  return t1141;
}

u_value f_set_46_insert_2(u_value v_x, u_value v_s) {
  u_value s1144 = v_s;
  u_value t1145 = NULL;
  if ((!t1145) && u_is_con(s1144, &n_set_46_SLeaf)) {
    t1145 = u_mk_con(&n_set_46_SNode, 4, u_mk_con(&n_set_46_SLeaf, 0), v_x, u_mk_con(&n_set_46_SLeaf, 0), u_mk_int(1));
  }
  if ((!t1145) && u_is_con(s1144, &n_set_46_SNode)) {
    u_value v_l = u_field(s1144, 0);
    u_value v_y = u_field(s1144, 1);
    u_value v_r = u_field(s1144, 2);
    u_value s1146 = u_compare(v_x, v_y);
    u_value t1147 = NULL;
    if ((!t1147) && u_equals(s1146, u_mk_int(-1))) {
      t1147 = f_set_46_rebalance_3(f_set_46_insert_2(v_x, v_l), v_y, v_r);
    }
    if ((!t1147) && u_equals(s1146, u_mk_int(1))) {
      t1147 = f_set_46_rebalance_3(v_l, v_y, f_set_46_insert_2(v_x, v_r));
    }
    if (!t1147) {
      t1147 = v_s;
    }
    if (!t1147) {
      u_no_case(s1146);
    }
    t1145 = t1147;
  }
  if (!t1145) {
    u_no_case(s1144);
  }
  return t1145;
}

u_value f_set_46_from_list_1(u_value v_xs) {
  return f_set_46_insert_all_2(v_xs, u_mk_con(&n_set_46_SLeaf, 0));
}

u_value f_set_46_insert_all_2(u_value v_xs, u_value v_s) {
  u_value s1148 = v_xs;
  u_value t1149 = NULL;
  if ((!t1149) && u_is_con(s1148, &n_list_46_Nil)) {
    t1149 = v_s;
  }
  if ((!t1149) && u_is_con(s1148, &n_list_46_Cons)) {
    u_value v_x = u_field(s1148, 0);
    u_value v_rest = u_field(s1148, 1);
    t1149 = f_set_46_insert_all_2(v_rest, f_set_46_insert_2(v_x, v_s));
  }
  if (!t1149) {
    u_no_case(s1148);
  }
  return t1149;
}

u_value f_set_46_elements_1(u_value v_s) {
  return f_set_46_elements_onto_2(v_s, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_set_46_elements_onto_2(u_value v_s, u_value v_rest) {
  u_value s1150 = v_s;
  u_value t1151 = NULL;
  if ((!t1151) && u_is_con(s1150, &n_set_46_SLeaf)) {
    t1151 = v_rest;
  }
  if ((!t1151) && u_is_con(s1150, &n_set_46_SNode)) {
    u_value v_l = u_field(s1150, 0);
    u_value v_x = u_field(s1150, 1);
    u_value v_r = u_field(s1150, 2);
    t1151 = f_set_46_elements_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, v_x, f_set_46_elements_onto_2(v_r, v_rest)));
  }
  if (!t1151) {
    u_no_case(s1150);
  }
  return t1151;
}

u_value f_set_46_count_1(u_value v_s) {
  u_value s1152 = v_s;
  u_value t1153 = NULL;
  if ((!t1153) && u_is_con(s1152, &n_set_46_SLeaf)) {
    t1153 = u_mk_int(0);
  }
  if ((!t1153) && u_is_con(s1152, &n_set_46_SNode)) {
    u_value v_l = u_field(s1152, 0);
    u_value v_r = u_field(s1152, 2);
    t1153 = u_add(u_add(f_set_46_count_1(v_l), u_mk_int(1)), f_set_46_count_1(v_r));
  }
  if (!t1153) {
    u_no_case(s1152);
  }
  return t1153;
}

u_value f_set_46_depth_1(u_value v_s) {
  u_value s1154 = v_s;
  u_value t1155 = NULL;
  if ((!t1155) && u_is_con(s1154, &n_set_46_SLeaf)) {
    t1155 = u_mk_int(0);
  }
  if ((!t1155) && u_is_con(s1154, &n_set_46_SNode)) {
    u_value v_d = u_field(s1154, 3);
    t1155 = v_d;
  }
  if (!t1155) {
    u_no_case(s1154);
  }
  return t1155;
}

u_value f_set_46_snode_depth_2(u_value v_l, u_value v_r) {
  u_value t1156;
  if (u_truth(u_lt(f_set_46_depth_1(v_l), f_set_46_depth_1(v_r)))) {
    t1156 = f_set_46_depth_1(v_r);
  } else {
    t1156 = f_set_46_depth_1(v_l);
  }
  return u_add(u_mk_int(1), t1156);
}

u_value f_set_46_snode_3(u_value v_l, u_value v_x, u_value v_r) {
  return u_mk_con(&n_set_46_SNode, 4, v_l, v_x, v_r, f_set_46_snode_depth_2(v_l, v_r));
}

u_value f_set_46_turn_left_1(u_value v_s) {
  u_value s1157 = v_s;
  u_value t1158 = NULL;
  if ((!t1158) && (u_is_con(s1157, &n_set_46_SNode) && u_is_con(u_field(s1157, 2), &n_set_46_SNode))) {
    u_value v_a = u_field(s1157, 0);
    u_value v_x = u_field(s1157, 1);
    u_value v_b = u_field(u_field(s1157, 2), 0);
    u_value v_y = u_field(u_field(s1157, 2), 1);
    u_value v_c = u_field(u_field(s1157, 2), 2);
    t1158 = f_set_46_snode_3(f_set_46_snode_3(v_a, v_x, v_b), v_y, v_c);
  }
  if (!t1158) {
    t1158 = v_s;
  }
  if (!t1158) {
    u_no_case(s1157);
  }
  return t1158;
}

u_value f_set_46_turn_right_1(u_value v_s) {
  u_value s1159 = v_s;
  u_value t1160 = NULL;
  if ((!t1160) && (u_is_con(s1159, &n_set_46_SNode) && u_is_con(u_field(s1159, 0), &n_set_46_SNode))) {
    u_value v_a = u_field(u_field(s1159, 0), 0);
    u_value v_x = u_field(u_field(s1159, 0), 1);
    u_value v_b = u_field(u_field(s1159, 0), 2);
    u_value v_y = u_field(s1159, 1);
    u_value v_c = u_field(s1159, 2);
    t1160 = f_set_46_snode_3(v_a, v_x, f_set_46_snode_3(v_b, v_y, v_c));
  }
  if (!t1160) {
    t1160 = v_s;
  }
  if (!t1160) {
    u_no_case(s1159);
  }
  return t1160;
}

u_value f_set_46_smaller_1(u_value v_s) {
  u_value s1161 = v_s;
  u_value t1162 = NULL;
  if ((!t1162) && u_is_con(s1161, &n_set_46_SNode)) {
    u_value v_l = u_field(s1161, 0);
    t1162 = v_l;
  }
  if (!t1162) {
    t1162 = v_s;
  }
  if (!t1162) {
    u_no_case(s1161);
  }
  return t1162;
}

u_value f_set_46_greater_1(u_value v_s) {
  u_value s1163 = v_s;
  u_value t1164 = NULL;
  if ((!t1164) && u_is_con(s1163, &n_set_46_SNode)) {
    u_value v_r = u_field(s1163, 2);
    t1164 = v_r;
  }
  if (!t1164) {
    t1164 = v_s;
  }
  if (!t1164) {
    u_no_case(s1163);
  }
  return t1164;
}

u_value f_set_46_rebalance_3(u_value v_l, u_value v_x, u_value v_r) {
  u_value t1168;
  if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1))))) {
    u_value t1165;
    if (u_truth(u_lt(f_set_46_depth_1(f_set_46_smaller_1(v_l)), f_set_46_depth_1(f_set_46_greater_1(v_l))))) {
      t1165 = f_set_46_turn_left_1(v_l);
    } else {
      t1165 = v_l;
    }
    t1168 = f_set_46_turn_right_1(f_set_46_snode_3(t1165, v_x, v_r));
  } else {
    u_value t1167;
    if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1))))) {
      u_value t1166;
      if (u_truth(u_lt(f_set_46_depth_1(f_set_46_greater_1(v_r)), f_set_46_depth_1(f_set_46_smaller_1(v_r))))) {
        t1166 = f_set_46_turn_right_1(v_r);
      } else {
        t1166 = v_r;
      }
      t1167 = f_set_46_turn_left_1(f_set_46_snode_3(v_l, v_x, t1166));
    } else {
      t1167 = f_set_46_snode_3(v_l, v_x, v_r);
    }
    t1168 = t1167;
  }
  return t1168;
}

u_value f_set_46_well_formed_1(u_value v_s) {
  u_value s1169 = v_s;
  u_value t1170 = NULL;
  if ((!t1170) && u_is_con(s1169, &n_set_46_SLeaf)) {
    t1170 = u_mk_bool(1);
  }
  if ((!t1170) && u_is_con(s1169, &n_set_46_SNode)) {
    u_value v_l = u_field(s1169, 0);
    u_value v_r = u_field(s1169, 2);
    u_value v_d = u_field(s1169, 3);
    u_value t1171 = f_set_46_well_formed_1(v_l);
    if (u_truth(t1171)) {
      t1171 = f_set_46_well_formed_1(v_r);
    }
    u_value t1172 = t1171;
    if (u_truth(t1172)) {
      t1172 = u_eq(v_d, f_set_46_snode_depth_2(v_l, v_r));
    }
    u_value t1173 = t1172;
    if (u_truth(t1173)) {
      t1173 = f_prelude_46__60__61__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1)));
    }
    u_value t1174 = t1173;
    if (u_truth(t1174)) {
      t1174 = f_prelude_46__60__61__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1)));
    }
    t1170 = t1174;
  }
  if (!t1170) {
    u_no_case(s1169);
  }
  return t1170;
}

u_value f_set_46_upto_1(u_value v_n) {
  u_value t1175;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t1175 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1175 = f_list_46__43__43__2(f_set_46_upto_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t1175;
}

u_value f_parse_46_Notations_46_cmds_1(u_value v_x) {
  u_value s1176 = v_x;
  u_value t1177 = NULL;
  if ((!t1177) && u_is_con(s1176, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1176, 0);
    t1177 = v_y;
  }
  if (!t1177) {
    u_no_case(s1176);
  }
  return t1177;
}

u_value f_parse_46_Notations_46_ops_1(u_value v_x) {
  u_value s1178 = v_x;
  u_value t1179 = NULL;
  if ((!t1179) && u_is_con(s1178, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1178, 1);
    t1179 = v_y;
  }
  if (!t1179) {
    u_no_case(s1178);
  }
  return t1179;
}

u_value f_parse_46_Notations_46_cmds_58__61__2(u_value v_x, u_value v_y) {
  u_value s1180 = v_x;
  u_value t1181 = NULL;
  if ((!t1181) && u_is_con(s1180, &n_parse_46_Notations)) {
    u_value v_a1 = u_field(s1180, 1);
    t1181 = u_mk_con(&n_parse_46_Notations, 2, v_y, v_a1);
  }
  if (!t1181) {
    u_no_case(s1180);
  }
  return t1181;
}

u_value f_parse_46_Notations_46_ops_58__61__2(u_value v_x, u_value v_y) {
  u_value s1182 = v_x;
  u_value t1183 = NULL;
  if ((!t1183) && u_is_con(s1182, &n_parse_46_Notations)) {
    u_value v_a0 = u_field(s1182, 0);
    t1183 = u_mk_con(&n_parse_46_Notations, 2, v_a0, v_y);
  }
  if (!t1183) {
    u_no_case(s1182);
  }
  return t1183;
}

u_value f_parse_46_Scope_46_params_1(u_value v_x) {
  u_value s1184 = v_x;
  u_value t1185 = NULL;
  if ((!t1185) && u_is_con(s1184, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1184, 0);
    t1185 = v_y;
  }
  if (!t1185) {
    u_no_case(s1184);
  }
  return t1185;
}

u_value f_parse_46_Scope_46_notations_1(u_value v_x) {
  u_value s1186 = v_x;
  u_value t1187 = NULL;
  if ((!t1187) && u_is_con(s1186, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1186, 1);
    t1187 = v_y;
  }
  if (!t1187) {
    u_no_case(s1186);
  }
  return t1187;
}

u_value f_parse_46_Scope_46_params_58__61__2(u_value v_x, u_value v_y) {
  u_value s1188 = v_x;
  u_value t1189 = NULL;
  if ((!t1189) && u_is_con(s1188, &n_parse_46_Scope)) {
    u_value v_a1 = u_field(s1188, 1);
    t1189 = u_mk_con(&n_parse_46_Scope, 2, v_y, v_a1);
  }
  if (!t1189) {
    u_no_case(s1188);
  }
  return t1189;
}

u_value f_parse_46_Scope_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1190 = v_x;
  u_value t1191 = NULL;
  if ((!t1191) && u_is_con(s1190, &n_parse_46_Scope)) {
    u_value v_a0 = u_field(s1190, 0);
    t1191 = u_mk_con(&n_parse_46_Scope, 2, v_a0, v_y);
  }
  if (!t1191) {
    u_no_case(s1190);
  }
  return t1191;
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
  static struct u_object l___1192 = U_LIT("_");
  static struct u_object l_assert_1193 = U_LIT("assert");
  static struct u_object l_assign_1194 = U_LIT("assign");
  static struct u_object l_call_1195 = U_LIT("call");
  static struct u_object l_case_1196 = U_LIT("case");
  static struct u_object l_catch_1197 = U_LIT("catch");
  static struct u_object l_data_1198 = U_LIT("data");
  static struct u_object l_else_1199 = U_LIT("else");
  static struct u_object l_end_1200 = U_LIT("end");
  static struct u_object l_false_1201 = U_LIT("false");
  static struct u_object l_for_1202 = U_LIT("for");
  static struct u_object l_function_1203 = U_LIT("function");
  static struct u_object l_if_1204 = U_LIT("if");
  static struct u_object l_import_1205 = U_LIT("import");
  static struct u_object l_in_1206 = U_LIT("in");
  static struct u_object l_infix_1207 = U_LIT("infix");
  static struct u_object l_lambda_1208 = U_LIT("lambda");
  static struct u_object l_let_1209 = U_LIT("let");
  static struct u_object l_local_1210 = U_LIT("local");
  static struct u_object l_match_1211 = U_LIT("match");
  static struct u_object l_out_1212 = U_LIT("out");
  static struct u_object l_postfix_1213 = U_LIT("postfix");
  static struct u_object l_prefix_1214 = U_LIT("prefix");
  static struct u_object l_procedure_1215 = U_LIT("procedure");
  static struct u_object l_raise_1216 = U_LIT("raise");
  static struct u_object l_ref_1217 = U_LIT("ref");
  static struct u_object l_run_1218 = U_LIT("run");
  static struct u_object l_skip_1219 = U_LIT("skip");
  static struct u_object l_test_1220 = U_LIT("test");
  static struct u_object l_then_1221 = U_LIT("then");
  static struct u_object l_true_1222 = U_LIT("true");
  static struct u_object l_type_1223 = U_LIT("type");
  static struct u_object l_try_1224 = U_LIT("try");
  static struct u_object l_while_1225 = U_LIT("while");
  static struct u_object l_with_1226 = U_LIT("with");
  u_pop(handler);
  cache = f_set_46_from_list_1(u_mk_con(&n_list_46_Cons, 2, &l___1192, u_mk_con(&n_list_46_Cons, 2, &l_assert_1193, u_mk_con(&n_list_46_Cons, 2, &l_assign_1194, u_mk_con(&n_list_46_Cons, 2, &l_call_1195, u_mk_con(&n_list_46_Cons, 2, &l_case_1196, u_mk_con(&n_list_46_Cons, 2, &l_catch_1197, u_mk_con(&n_list_46_Cons, 2, &l_data_1198, u_mk_con(&n_list_46_Cons, 2, &l_else_1199, u_mk_con(&n_list_46_Cons, 2, &l_end_1200, u_mk_con(&n_list_46_Cons, 2, &l_false_1201, u_mk_con(&n_list_46_Cons, 2, &l_for_1202, u_mk_con(&n_list_46_Cons, 2, &l_function_1203, u_mk_con(&n_list_46_Cons, 2, &l_if_1204, u_mk_con(&n_list_46_Cons, 2, &l_import_1205, u_mk_con(&n_list_46_Cons, 2, &l_in_1206, u_mk_con(&n_list_46_Cons, 2, &l_infix_1207, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1208, u_mk_con(&n_list_46_Cons, 2, &l_let_1209, u_mk_con(&n_list_46_Cons, 2, &l_local_1210, u_mk_con(&n_list_46_Cons, 2, &l_match_1211, u_mk_con(&n_list_46_Cons, 2, &l_out_1212, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1213, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1214, u_mk_con(&n_list_46_Cons, 2, &l_procedure_1215, u_mk_con(&n_list_46_Cons, 2, &l_raise_1216, u_mk_con(&n_list_46_Cons, 2, &l_ref_1217, u_mk_con(&n_list_46_Cons, 2, &l_run_1218, u_mk_con(&n_list_46_Cons, 2, &l_skip_1219, u_mk_con(&n_list_46_Cons, 2, &l_test_1220, u_mk_con(&n_list_46_Cons, 2, &l_then_1221, u_mk_con(&n_list_46_Cons, 2, &l_true_1222, u_mk_con(&n_list_46_Cons, 2, &l_type_1223, u_mk_con(&n_list_46_Cons, 2, &l_try_1224, u_mk_con(&n_list_46_Cons, 2, &l_while_1225, u_mk_con(&n_list_46_Cons, 2, &l_with_1226, u_mk_con(&n_list_46_Nil, 0)))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_set_46_member_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1227 = U_LIT("=");
  static struct u_object l__61__62__1228 = U_LIT("=>");
  static struct u_object l__58__61__1229 = U_LIT(":=");
  static struct u_object l__58__1230 = U_LIT(":");
  static struct u_object l__124__1231 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1227, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1228, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1229, u_mk_con(&n_list_46_Cons, 2, &l__58__1230, u_mk_con(&n_list_46_Cons, 2, &l__124__1231, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1232 = v_ns;
  u_value t1233 = NULL;
  if ((!t1233) && u_is_con(s1232, &n_list_46_Nil)) {
    t1233 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1233) && (u_is_con(s1232, &n_list_46_Cons) && (u_is_con(u_field(s1232, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1232, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1232, 0), 1);
    u_value v_ops = u_field(u_field(s1232, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1233 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1233) && u_is_con(s1232, &n_list_46_Cons)) {
    u_value v_more = u_field(s1232, 1);
    t1233 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1233) {
    u_no_case(s1232);
  }
  return t1233;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1234 = v_ns;
  u_value t1235 = NULL;
  if ((!t1235) && u_is_con(s1234, &n_list_46_Nil)) {
    t1235 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1235) && (u_is_con(s1234, &n_list_46_Cons) && (u_is_con(u_field(s1234, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1234, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1234, 0), 1);
    u_value v_ops = u_field(u_field(s1234, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1235 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1235) && u_is_con(s1234, &n_list_46_Cons)) {
    u_value v_more = u_field(s1234, 1);
    t1235 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1235) {
    u_no_case(s1234);
  }
  return t1235;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1236 = v_ns;
  u_value t1237 = NULL;
  if ((!t1237) && u_is_con(s1236, &n_list_46_Nil)) {
    t1237 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1237) && (u_is_con(s1236, &n_list_46_Cons) && u_is_con(u_field(s1236, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1236, 0), 0);
    u_value v_p = u_field(u_field(s1236, 0), 1);
    u_value v_ops = u_field(u_field(s1236, 0), 2);
    u_value t1238 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1238)) {
      t1238 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1239 = t1238;
    if (u_truth(t1239)) {
      t1239 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1239)) {
      t1237 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1237) && u_is_con(s1236, &n_list_46_Cons)) {
    u_value v_more = u_field(s1236, 1);
    t1237 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1237) {
    u_no_case(s1236);
  }
  return t1237;
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
  u_value s1240 = v_cs;
  u_value t1241 = NULL;
  if ((!t1241) && u_is_con(s1240, &n_list_46_Nil)) {
    t1241 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1241) && (u_is_con(s1240, &n_list_46_Cons) && u_is_con(u_field(s1240, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1240, 0), 2);
    u_value v_more = u_field(s1240, 1);
    t1241 = f_list_46__43__43__2(v_ops, f_parse_46_operators_of_1(v_more));
  }
  if ((!t1241) && u_is_con(s1240, &n_list_46_Cons)) {
    u_value v_more = u_field(s1240, 1);
    t1241 = f_parse_46_operators_of_1(v_more);
  }
  if (!t1241) {
    u_no_case(s1240);
  }
  return t1241;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1242 = v_ts;
  u_value t1243 = NULL;
  if ((!t1243) && u_is_con(s1242, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1242, 0), 0);
    t1243 = v_t;
  }
  if (!t1243) {
    t1243 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1243) {
    u_no_case(s1242);
  }
  return t1243;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1244 = v_ts;
  u_value t1245 = NULL;
  if ((!t1245) && (u_is_con(s1244, &n_list_46_Cons) && u_is_con(u_field(s1244, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1244, 1), 0), 0);
    t1245 = v_t;
  }
  if (!t1245) {
    t1245 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1245) {
    u_no_case(s1244);
  }
  return t1245;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1246 = v_ts;
  u_value t1247 = NULL;
  if ((!t1247) && u_is_con(s1246, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1246, 0), 1);
    t1247 = v_i;
  }
  if (!t1247) {
    t1247 = u_mk_int(0);
  }
  if (!t1247) {
    u_no_case(s1246);
  }
  return t1247;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1248 = v_t;
  u_value t1249 = NULL;
  if ((!t1249) && u_is_con(s1248, &n_lex_46_TName)) {
    u_value v_n = u_field(s1248, 0);
    t1249 = v_n;
  }
  if ((!t1249) && u_is_con(s1248, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1248, 0);
    t1249 = v_s;
  }
  if ((!t1249) && u_is_con(s1248, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1248, 0);
    t1249 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1249) && u_is_con(s1248, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1248, 0);
    t1249 = u_str_show(v_n);
  }
  if ((!t1249) && u_is_con(s1248, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1248, 0);
    t1249 = u_str_quote(v_s);
  }
  if ((!t1249) && u_is_con(s1248, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1248, 0);
    static struct u_object l__39__1250 = U_LIT("'");
    static struct u_object l__39__1251 = U_LIT("'");
    t1249 = u_concat(&l__39__1250, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1251));
  }
  if ((!t1249) && u_is_con(s1248, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1252 = U_LIT("end of input");
    t1249 = &l_end_32_of_32_input_1252;
  }
  if (!t1249) {
    u_no_case(s1248);
  }
  return t1249;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1253 = f_parse_46_head_token_1(v_ts);
  u_value t1254 = NULL;
  if ((!t1254) && u_is_con(s1253, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1253, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1254 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1254) && u_is_con(s1253, &n_lex_46_TName)) {
    u_value v_n = u_field(s1253, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1254 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1254) {
    t1254 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1254) {
    u_no_case(s1253);
  }
  return t1254;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1255 = *r_ts;
    int d1256 = 0;
    if ((!d1256) && u_is_con(s1255, &n_list_46_Cons)) {
      u_value v_more = u_field(s1255, 1);
      d1256 = 1;
      {
        u_value t1257 = v_more;
        *r_ts = t1257;
      }
    }
    if (!d1256) {
      d1256 = 1;
      {
      }
    }
    if (!d1256) {
      u_no_case(s1255);
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
        static struct u_object l__32_expected_32_but_32__1258 = U_LIT(" expected but ");
        static struct u_object l__32_found_1259 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1258, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1259))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1260 = f_parse_46_head_token_1(*r_ts);
    int d1261 = 0;
    if ((!d1261) && u_is_con(s1260, &n_lex_46_TName)) {
      u_value v_m = u_field(s1260, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1261 = 1;
        {
          u_value t1262 = v_m;
          *r_n = t1262;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1261) {
      d1261 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1263 = U_LIT("name expected but ");
        static struct u_object l__32_found_1264 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1263, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1264)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1261) {
      u_no_case(s1260);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1265 = f_parse_46_head_token_1(*r_ts);
    int d1266 = 0;
    if ((!d1266) && u_is_con(s1265, &n_lex_46_TName)) {
      u_value v_m = u_field(s1265, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_m))) {
        d1266 = 1;
        {
          static struct u_object l_operator_32__1267 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1268 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1267, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1268)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1266) {
      d1266 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1266) {
      u_no_case(s1265);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1269 = f_parse_46_head_token_1(*r_ts);
    int d1270 = 0;
    if ((!d1270) && u_is_con(s1269, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1269, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1270 = 1;
        {
          u_value t1271 = v_s;
          *r_n = t1271;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1270) {
      d1270 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1270) {
      u_no_case(s1269);
    }
  }
}

u_value f_parse_46_Ops_46_notations_1(u_value v_x) {
  u_value s1272 = v_x;
  u_value t1273 = NULL;
  if ((!t1273) && u_is_con(s1272, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1272, 0);
    t1273 = v_y;
  }
  if (!t1273) {
    u_no_case(s1272);
  }
  return t1273;
}

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x) {
  u_value s1274 = v_x;
  u_value t1275 = NULL;
  if ((!t1275) && u_is_con(s1274, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1274, 1);
    t1275 = v_y;
  }
  if (!t1275) {
    u_no_case(s1274);
  }
  return t1275;
}

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x) {
  u_value s1276 = v_x;
  u_value t1277 = NULL;
  if ((!t1277) && u_is_con(s1276, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1276, 2);
    t1277 = v_y;
  }
  if (!t1277) {
    u_no_case(s1276);
  }
  return t1277;
}

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x) {
  u_value s1278 = v_x;
  u_value t1279 = NULL;
  if ((!t1279) && u_is_con(s1278, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1278, 3);
    t1279 = v_y;
  }
  if (!t1279) {
    u_no_case(s1278);
  }
  return t1279;
}

u_value f_parse_46_Ops_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1280 = v_x;
  u_value t1281 = NULL;
  if ((!t1281) && u_is_con(s1280, &n_parse_46_Ops)) {
    u_value v_a1 = u_field(s1280, 1);
    u_value v_a2 = u_field(s1280, 2);
    u_value v_a3 = u_field(s1280, 3);
    t1281 = u_mk_con(&n_parse_46_Ops, 4, v_y, v_a1, v_a2, v_a3);
  }
  if (!t1281) {
    u_no_case(s1280);
  }
  return t1281;
}

u_value f_parse_46_Ops_46_prefix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1282 = v_x;
  u_value t1283 = NULL;
  if ((!t1283) && u_is_con(s1282, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1282, 0);
    u_value v_a2 = u_field(s1282, 2);
    u_value v_a3 = u_field(s1282, 3);
    t1283 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_y, v_a2, v_a3);
  }
  if (!t1283) {
    u_no_case(s1282);
  }
  return t1283;
}

u_value f_parse_46_Ops_46_postfix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1284 = v_x;
  u_value t1285 = NULL;
  if ((!t1285) && u_is_con(s1284, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1284, 0);
    u_value v_a1 = u_field(s1284, 1);
    u_value v_a3 = u_field(s1284, 3);
    t1285 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_y, v_a3);
  }
  if (!t1285) {
    u_no_case(s1284);
  }
  return t1285;
}

u_value f_parse_46_Ops_46_infix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1286 = v_x;
  u_value t1287 = NULL;
  if ((!t1287) && u_is_con(s1286, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1286, 0);
    u_value v_a1 = u_field(s1286, 1);
    u_value v_a2 = u_field(s1286, 2);
    t1287 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_a2, v_y);
  }
  if (!t1287) {
    u_no_case(s1286);
  }
  return t1287;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1288 = v_rest;
    int d1289 = 0;
    if ((!d1289) && u_is_con(s1288, &n_list_46_Nil)) {
      d1289 = 1;
      {
      }
    }
    if ((!d1289) && (u_is_con(s1288, &n_list_46_Cons) && u_is_con(u_field(s1288, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1288, 0), 0);
      u_value v_i = u_field(u_field(s1288, 0), 1);
      d1289 = 1;
      {
        static struct u_object l_unexpected_32_opera_1290 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1290, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1289) {
      d1289 = 1;
      {
        static struct u_object l_missing_32_operator_1291 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1291, v_at), "parse.ParseError");
      }
    }
    if (!d1289) {
      u_no_case(s1288);
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
    u_value s1292 = v_items;
    int d1293 = 0;
    if ((!d1293) && (u_is_con(s1292, &n_list_46_Cons) && u_is_con(u_field(s1292, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1292, 0), 0);
      u_value v_more = u_field(s1292, 1);
      d1293 = 1;
      {
        u_value t1294 = v_x;
        u_value t1295 = v_more;
        *r_a = t1294;
        *r_rest = t1295;
      }
    }
    if ((!d1293) && (u_is_con(s1292, &n_list_46_Cons) && u_is_con(u_field(s1292, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1292, 0), 0);
      u_value v_i = u_field(u_field(s1292, 0), 1);
      u_value v_more = u_field(s1292, 1);
      d1293 = 1;
      {
        u_value s1296 = f_parse_46_prefix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op);
        int d1297 = 0;
        if ((!d1297) && u_is_con(s1296, &n_option_46_Some)) {
          u_value v_p = u_field(s1296, 0);
          d1297 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1298 = u_apply(f_parse_46_Ops_46_prefix_op_1(v_o), 2, v_op, v_b);
            *r_a = t1298;
          }
        }
        if ((!d1297) && u_is_con(s1296, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op))) {
            d1297 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1299 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1299), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1297) && u_is_con(s1296, &n_option_46_None)) {
          d1297 = 1;
          {
            static struct u_object l_undeclared_32_opera_1300 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1300, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1297) {
          u_no_case(s1296);
        }
      }
    }
    if ((!d1293) && u_is_con(s1292, &n_list_46_Nil)) {
      d1293 = 1;
      {
        static struct u_object l_missing_32_operand_1301 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1301, v_at), "parse.ParseError");
      }
    }
    if (!d1293) {
      u_no_case(s1292);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1302 = v_items;
    int d1303 = 0;
    if ((!d1303) && (u_is_con(s1302, &n_list_46_Cons) && u_is_con(u_field(s1302, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1302, 0), 0);
      u_value v_i = u_field(u_field(s1302, 0), 1);
      u_value v_tail = u_field(s1302, 1);
      d1303 = 1;
      {
        u_value s1304 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op), f_parse_46_infix_of_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op));
        int d1305 = 0;
        if ((!d1305) && u_is_con(u_field(s1304, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1304, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1305 = 1;
            {
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_postfix_op_1(v_o), 2, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1305) && u_is_con(u_field(s1304, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1304, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1304, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1305 = 1;
            {
              u_value t1306;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1306 = v_p;
              } else {
                t1306 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1306, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)));
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_infix_op_1(v_o), 3, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1305) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op)))) {
            d1305 = 1;
            {
              static struct u_object l_undeclared_32_opera_1307 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1307, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1305) {
          d1305 = 1;
          {
            u_value t1308 = v_lhs;
            u_value t1309 = v_items;
            *r_a = t1308;
            *r_rest = t1309;
          }
        }
        if (!d1305) {
          u_no_case(s1304);
        }
      }
    }
    if (!d1303) {
      d1303 = 1;
      {
        u_value t1310 = v_lhs;
        u_value t1311 = v_items;
        *r_a = t1310;
        *r_rest = t1311;
      }
    }
    if (!d1303) {
      u_no_case(s1302);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1312 = v_rest;
    int d1313 = 0;
    if ((!d1313) && (u_is_con(s1312, &n_list_46_Cons) && u_is_con(u_field(s1312, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1312, 0), 0);
      u_value v_i = u_field(u_field(s1312, 0), 1);
      u_value t1316 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1316)) {
        t1316 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1316)) {
        d1313 = 1;
        {
          static struct u_object l_operator_32__1314 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1315 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1314, u_concat(v_op, &l__32_is_32_not_32_associat_1315)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1313) {
      d1313 = 1;
      {
      }
    }
    if (!d1313) {
      u_no_case(s1312);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1317 = v_fp;
  u_value t1318 = NULL;
  if ((!t1318) && u_is_con(s1317, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1317, 0), 1);
    t1318 = u_eq(v_q, v_p);
  }
  if (!t1318) {
    t1318 = u_mk_bool(0);
  }
  if (!t1318) {
    u_no_case(s1317);
  }
  return t1318;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1319 = v_ts;
  u_value t1320 = NULL;
  if ((!t1320) && (u_is_con(s1319, &n_list_46_Cons) && (u_is_con(u_field(s1319, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1319, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1319, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1319, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1319, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1319, 0), 1);
    t1320 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1320) && (u_is_con(s1319, &n_list_46_Cons) && u_is_con(u_field(s1319, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1319, 0);
    t1320 = v_t;
  }
  if ((!t1320) && u_is_con(s1319, &n_list_46_Nil)) {
    t1320 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1320) {
    static struct u_object l__40_invalid_41__1321 = U_LIT("(invalid)");
    t1320 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1321), v_ts);
  }
  if (!t1320) {
    u_no_case(s1319);
  }
  return t1320;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1322 = v_ts;
  u_value t1323 = NULL;
  if ((!t1323) && (u_is_con(s1322, &n_list_46_Cons) && (u_is_con(u_field(s1322, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1322, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1322, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1322, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1322, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1322, 0), 1);
    t1323 = v_cs;
  }
  if (!t1323) {
    t1323 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1323) {
    u_no_case(s1322);
  }
  return t1323;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1324 = U_LIT("->");
  u_value t1331;
  if (u_truth(u_eq(v_op, &l__45__62__1324))) {
    u_value t1325 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1326 = &t1325;
    u_value l1327 = v_l;
    while (u_is_con(l1327, &n_list_46_Cons)) {
      u_value v_t = u_field(l1327, 0);
      if (u_truth(u_mk_bool(1))) {
        e1326 = u_append(e1326, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1327 = u_field(l1327, 1);
    }
    t1331 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1325, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1328 = U_LIT("*");
    u_value t1330;
    if (u_truth(u_eq(v_op, &l__42__1328))) {
      static struct u_object l__40_product_41__1329 = U_LIT("(product)");
      t1330 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1329), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1330 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1331 = t1330;
  }
  return t1331;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1332 = v_t;
  u_value t1333 = NULL;
  if ((!t1333) && (u_is_con(s1332, &n_syntax_46_TSort) && (u_is_con(u_field(s1332, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1332, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1332, 0), 1), u_mk_lit("(invalid)")))))) {
    t1333 = u_mk_bool(1);
  }
  if ((!t1333) && u_is_con(s1332, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1332, 1);
    t1333 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1333) && u_is_con(s1332, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1332, 0);
    u_value v_r = u_field(s1332, 1);
    u_value t1334 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1334)) {
      t1334 = f_parse_46_has_invalid_1(v_r);
    }
    t1333 = t1334;
  }
  if ((!t1333) && u_is_con(s1332, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1332, 0);
    t1333 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1333) {
    t1333 = u_mk_bool(0);
  }
  if (!t1333) {
    u_no_case(s1332);
  }
  return t1333;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1335 = v_ts;
  u_value t1336 = NULL;
  if ((!t1336) && u_is_con(s1335, &n_list_46_Nil)) {
    t1336 = u_mk_bool(0);
  }
  if ((!t1336) && u_is_con(s1335, &n_list_46_Cons)) {
    u_value v_t = u_field(s1335, 0);
    u_value v_more = u_field(s1335, 1);
    u_value t1337 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1337)) {
      t1337 = f_parse_46_any_invalid_1(v_more);
    }
    t1336 = t1337;
  }
  if (!t1336) {
    u_no_case(s1335);
  }
  return t1336;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1338 = v_t;
  u_value t1339 = NULL;
  if ((!t1339) && u_is_con(s1338, &n_lex_46_TName)) {
    u_value v_n = u_field(s1338, 0);
    u_value t1340 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1340)) {
      t1340 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1339 = t1340;
  }
  if ((!t1339) && (u_is_con(s1338, &n_lex_46_TPunct) && u_equals(u_field(s1338, 0), u_mk_char(40)))) {
    t1339 = u_mk_bool(1);
  }
  if (!t1339) {
    t1339 = u_mk_bool(0);
  }
  if (!t1339) {
    u_no_case(s1338);
  }
  return t1339;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1341 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1341;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1342 = f_parse_46_type_one_1(v_r);
    *r_t = t1342;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1343 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1343, v_start), "parse.ParseError");
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
    u_value t1344 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1344;
    u_value t1345 = u_mk_bool(1);
    v_next = t1345;
    u_value t1346 = u_mk_bool(1);
    v_go = t1346;
    u_value t1355 = v_go;
    while (u_truth(t1355)) {
      {
        u_value s1347 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1348 = 0;
        if ((!d1348) && u_is_con(s1347, &n_option_46_Some)) {
          u_value v_op = u_field(s1347, 0);
          d1348 = 1;
          {
            u_value t1349 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1350 = u_mk_bool(1);
            v_rev = t1349;
            v_next = t1350;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1348) && u_is_con(s1347, &n_option_46_None)) {
          u_value t1353 = v_next;
          if (u_truth(t1353)) {
            t1353 = f_parse_46_starts_type_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1353)) {
            d1348 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1351 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1352 = u_mk_bool(0);
              v_rev = t1351;
              v_next = t1352;
            }
          }
        }
        if (!d1348) {
          d1348 = 1;
          {
            u_value t1354 = u_mk_bool(0);
            v_go = t1354;
          }
        }
        if (!d1348) {
          u_no_case(s1347);
        }
      }
      t1355 = v_go;
    }
    u_value s1356 = v_rev;
    int d1357 = 0;
    if ((!d1357) && u_is_con(s1356, &n_list_46_Nil)) {
      d1357 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1358 = U_LIT("type expected but ");
        static struct u_object l__32_found_1359 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1358, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1359)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1357) {
      d1357 = 1;
      {
        u_value t1360 = f_list_46_reverse_1(v_rev);
        *r_items = t1360;
      }
    }
    if (!d1357) {
      u_no_case(s1356);
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
    u_value t1364 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (u_truth(t1364)) {
      static struct u_object l_in_1361 = U_LIT("in");
      static struct u_object l_out_1362 = U_LIT("out");
      static struct u_object l_ref_1363 = U_LIT("ref");
      t1364 = f_list_46_contains_2(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_in_1361), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_out_1362), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_ref_1363), u_mk_con(&n_list_46_Nil, 0)))));
    }
    if (u_truth(t1364)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_sections(u_mk_proc(wp_parse_46_parse_type, 0), u_mk_proc(wp_parse_46_parse_type, 0), v_sc, &v_ins, &v_outs, &v_refs, r_ts);
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
        u_value t1365 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TProc, 3, v_ins, v_outs, v_refs), u_mk_con(&n_list_46_Nil, 0));
        *r_r = t1365;
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
              u_value t1366 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1366;
            }
          } else {
            if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Scope_46_params_1(v_sc)))) {
              {
                u_value t1367 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
                *r_r = t1367;
              }
            } else {
              {
                u_value t1368 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
                *r_r = t1368;
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
    u_value t1369 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1369;
    u_value t1370 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1370)) {
      t1370 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1370)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1371 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1371;
        u_value t1373 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1373)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1372 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1372;
          }
          t1373 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1374 = f_list_46_reverse_1(v_rev);
    *r_r = t1374;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_prefix_2(u_value v_op, u_value v_a) {
  u_value s1375 = v_a;
  u_value t1376 = NULL;
  if ((!t1376) && (u_is_con(s1375, &n_syntax_46_ELit) && u_is_con(u_field(s1375, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1375, 0), 0);
    static struct u_object l__45__1377 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1377))) {
      t1376 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1376) {
    t1376 = f_parse_46_expr_op1_2(v_op, v_a);
  }
  if (!t1376) {
    u_no_case(s1375);
  }
  return t1376;
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1378 = v_es;
  u_value t1379 = NULL;
  if ((!t1379) && u_is_con(s1378, &n_list_46_Nil)) {
    t1379 = v_rest;
  }
  if ((!t1379) && u_is_con(s1378, &n_list_46_Cons)) {
    u_value v_e = u_field(s1378, 0);
    u_value v_more = u_field(s1378, 1);
    static struct u_object l_Cons_1380 = U_LIT("Cons");
    t1379 = f_parse_46_expr_op2_3(&l_Cons_1380, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1379) {
    u_no_case(s1378);
  }
  return t1379;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1381 = v_t;
  u_value t1382 = NULL;
  if ((!t1382) && u_is_con(s1381, &n_lex_46_TName)) {
    u_value v_n = u_field(s1381, 0);
    static struct u_object l_let_1383 = U_LIT("let");
    static struct u_object l_if_1384 = U_LIT("if");
    static struct u_object l_lambda_1385 = U_LIT("lambda");
    static struct u_object l_match_1386 = U_LIT("match");
    static struct u_object l_true_1387 = U_LIT("true");
    static struct u_object l_false_1388 = U_LIT("false");
    u_value t1390 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1383, u_mk_con(&n_list_46_Cons, 2, &l_if_1384, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1385, u_mk_con(&n_list_46_Cons, 2, &l_match_1386, u_mk_con(&n_list_46_Cons, 2, &l_true_1387, u_mk_con(&n_list_46_Cons, 2, &l_false_1388, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1390)) {
      u_value t1389 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1389)) {
        t1389 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1390 = t1389;
    }
    t1382 = t1390;
  }
  if ((!t1382) && u_is_con(s1381, &n_lex_46_TNum)) {
    t1382 = u_mk_bool(1);
  }
  if ((!t1382) && u_is_con(s1381, &n_lex_46_TStr)) {
    t1382 = u_mk_bool(1);
  }
  if ((!t1382) && u_is_con(s1381, &n_lex_46_TChar)) {
    t1382 = u_mk_bool(1);
  }
  if ((!t1382) && u_is_con(s1381, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1381, 0);
    u_value t1391 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1391)) {
      t1391 = u_eq(v_c, u_mk_char(91));
    }
    u_value t1392 = t1391;
    if (!u_truth(t1392)) {
      t1392 = u_eq(v_c, u_mk_char(123));
    }
    t1382 = t1392;
  }
  if (!t1382) {
    t1382 = u_mk_bool(0);
  }
  if (!t1382) {
    u_no_case(s1381);
  }
  return t1382;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    static struct u_object l_raise_1393 = U_LIT("raise");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_raise_1393))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, r_e, r_ts);
        u_value t1394 = u_mk_con(&n_syntax_46_ERaise, 2, *r_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_e = t1394;
      }
    } else {
      {
        p_parse_46_operators(v_sc, r_e, r_ts);
        static struct u_object l_with_1395 = U_LIT("with");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_with_1395))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_update(v_sc, r_e, r_ts);
            u_value t1396 = f_parse_46_at_update_1(*r_ts);
            while (u_truth(t1396)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_update(v_sc, r_e, r_ts);
              }
              t1396 = f_parse_46_at_update_1(*r_ts);
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
    static struct u_object l__58__61__1397 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1397), r_ts);
    p_parse_46_operators(v_sc, &v_a, r_ts);
    static struct u_object l__46__1398 = U_LIT(".");
    static struct u_object l__58__61__1399 = U_LIT(":=");
    u_value t1400 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1398, u_concat(v_f, &l__58__61__1399))), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1400;
  }
}

u_value f_parse_46_at_update_1(u_value v_ts) {
  u_value s1401 = v_ts;
  u_value t1402 = NULL;
  if ((!t1402) && (u_is_con(s1401, &n_list_46_Cons) && (u_is_con(u_field(u_field(s1401, 0), 0), &n_lex_46_TPunct) && (u_equals(u_field(u_field(u_field(s1401, 0), 0), 0), u_mk_char(44)) && (u_is_con(u_field(s1401, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s1401, 1), 0), 0), &n_lex_46_TName) && (u_is_con(u_field(u_field(s1401, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(u_field(s1401, 1), 1), 0), 0), &n_lex_46_TSym) && u_equals(u_field(u_field(u_field(u_field(u_field(s1401, 1), 1), 0), 0), 0), u_mk_lit(":=")))))))))) {
    t1402 = u_mk_bool(1);
  }
  if (!t1402) {
    t1402 = u_mk_bool(0);
  }
  if (!t1402) {
    u_no_case(s1401);
  }
  return t1402;
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
    u_value t1403 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1403;
    u_value t1404 = u_mk_bool(1);
    v_next = t1404;
    u_value t1405 = u_mk_bool(1);
    v_go = t1405;
    u_value t1414 = v_go;
    while (u_truth(t1414)) {
      {
        u_value s1406 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1407 = 0;
        if ((!d1407) && u_is_con(s1406, &n_option_46_Some)) {
          u_value v_op = u_field(s1406, 0);
          d1407 = 1;
          {
            u_value t1408 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1409 = u_mk_bool(1);
            v_rev = t1408;
            v_next = t1409;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1407) && u_is_con(s1406, &n_option_46_None)) {
          u_value t1412 = v_next;
          if (u_truth(t1412)) {
            t1412 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1412)) {
            d1407 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1410 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1411 = u_mk_bool(0);
              v_rev = t1410;
              v_next = t1411;
            }
          }
        }
        if (!d1407) {
          d1407 = 1;
          {
            u_value t1413 = u_mk_bool(0);
            v_go = t1413;
          }
        }
        if (!d1407) {
          u_no_case(s1406);
        }
      }
      t1414 = v_go;
    }
    u_value s1415 = v_rev;
    int d1416 = 0;
    if ((!d1416) && u_is_con(s1415, &n_list_46_Nil)) {
      d1416 = 1;
      {
        static struct u_object l_expression_32_expec_1417 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1418 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1417, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1418)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1416) {
      d1416 = 1;
      {
        u_value t1419 = f_list_46_reverse_1(v_rev);
        *r_items = t1419;
      }
    }
    if (!d1416) {
      u_no_case(s1415);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1420 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1420;
    u_value t1422 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1422)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1421 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1421;
      }
      t1422 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1423 = f_list_46_reverse_1(v_rev);
    *r_es = t1423;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1424 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1424;
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
    u_value t1425 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1425)) {
      t1425 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1429 = t1425;
    while (u_truth(t1429)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1426 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1426;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1427 = U_LIT(".");
            u_value t1428 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1427, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1428;
          }
        }
      }
      u_value t1430 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1430)) {
        t1430 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1429 = t1430;
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
    u_value s1431 = f_parse_46_head_token_1(*r_ts);
    int d1432 = 0;
    if ((!d1432) && (u_is_con(s1431, &n_lex_46_TName) && u_equals(u_field(s1431, 0), u_mk_lit("let")))) {
      d1432 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1432) && (u_is_con(s1431, &n_lex_46_TName) && u_equals(u_field(s1431, 0), u_mk_lit("if")))) {
      d1432 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1433 = U_LIT("then");
        p_parse_46_keyword(&l_then_1433, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1434 = U_LIT("else");
        p_parse_46_keyword(&l_else_1434, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1435 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1435;
      }
    }
    if ((!d1432) && (u_is_con(s1431, &n_lex_46_TName) && u_equals(u_field(s1431, 0), u_mk_lit("lambda")))) {
      d1432 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1432) && (u_is_con(s1431, &n_lex_46_TName) && u_equals(u_field(s1431, 0), u_mk_lit("match")))) {
      d1432 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1436 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1436;
      }
    }
    if ((!d1432) && (u_is_con(s1431, &n_lex_46_TName) && u_equals(u_field(s1431, 0), u_mk_lit("true")))) {
      d1432 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1437 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1437;
      }
    }
    if ((!d1432) && (u_is_con(s1431, &n_lex_46_TName) && u_equals(u_field(s1431, 0), u_mk_lit("false")))) {
      d1432 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1438 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1438;
      }
    }
    if ((!d1432) && u_is_con(s1431, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1431, 0);
      d1432 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1439 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1439;
      }
    }
    if ((!d1432) && u_is_con(s1431, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1431, 0);
      d1432 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1440 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1440;
      }
    }
    if ((!d1432) && u_is_con(s1431, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1431, 0);
      d1432 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1441 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1441;
      }
    }
    if ((!d1432) && (u_is_con(s1431, &n_lex_46_TPunct) && u_equals(u_field(s1431, 0), u_mk_char(40)))) {
      d1432 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1442 = v_es;
        int d1443 = 0;
        if ((!d1443) && (u_is_con(s1442, &n_list_46_Cons) && u_is_con(u_field(s1442, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1442, 0);
          d1443 = 1;
          {
            u_value t1444 = v_x;
            *r_e = t1444;
          }
        }
        if (!d1443) {
          d1443 = 1;
          {
            u_value t1445 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1445;
          }
        }
        if (!d1443) {
          u_no_case(s1442);
        }
      }
    }
    if ((!d1432) && (u_is_con(s1431, &n_lex_46_TPunct) && u_equals(u_field(s1431, 0), u_mk_char(91)))) {
      d1432 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1432) && (u_is_con(s1431, &n_lex_46_TPunct) && u_equals(u_field(s1431, 0), u_mk_char(123)))) {
      d1432 = 1;
      {
        p_parse_46_parse_braces(v_sc, r_e, r_ts);
      }
    }
    if ((!d1432) && u_is_con(s1431, &n_lex_46_TName)) {
      u_value v_n = u_field(s1431, 0);
      u_value t1447 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1447)) {
        t1447 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t1447)) {
        d1432 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1446 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1446;
        }
      }
    }
    if (!d1432) {
      d1432 = 1;
      {
        static struct u_object l_expression_32_expec_1448 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1449 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1448, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1449)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1432) {
      u_no_case(s1431);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1450 = v_ps;
  u_value t1451 = NULL;
  if ((!t1451) && u_is_con(s1450, &n_list_46_Nil)) {
    t1451 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1451) && (u_is_con(s1450, &n_list_46_Cons) && u_is_con(u_field(s1450, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1450, 0), 0);
    u_value v_more = u_field(s1450, 1);
    u_value s1452 = f_parse_46_pattern_vars_1(v_more);
    u_value t1453 = NULL;
    if ((!t1453) && u_is_con(s1452, &n_option_46_Some)) {
      u_value v_xs = u_field(s1452, 0);
      t1453 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1453) && u_is_con(s1452, &n_option_46_None)) {
      t1453 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1453) {
      u_no_case(s1452);
    }
    t1451 = t1453;
  }
  if (!t1451) {
    t1451 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1451) {
    u_no_case(s1450);
  }
  return t1451;
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
    static struct u_object l__58__61__1454 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1454), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1455 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1456 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1455;
    v_es = t1456;
    u_value t1460 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1460)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1457 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1457), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1458 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1459 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1458;
        v_es = t1459;
      }
      t1460 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1461 = U_LIT("in");
    p_parse_46_keyword(&l_in_1461, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1462 = f_list_46_reverse_1(v_ps);
    u_value t1463 = f_list_46_reverse_1(v_es);
    v_ps = t1462;
    v_es = t1463;
    u_value s1464 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1465 = 0;
    if ((!d1465) && u_is_con(u_field(s1464, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1464, 0), 0);
      d1465 = 1;
      {
        u_value t1466 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1466;
      }
    }
    if ((!d1465) && (u_is_con(u_field(s1464, 0), &n_option_46_None) && (u_is_con(u_field(s1464, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1464, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1464, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1464, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1464, 1), 0);
      u_value v_c = u_field(u_field(s1464, 2), 0);
      d1465 = 1;
      {
        u_value t1467 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1467;
      }
    }
    if (!d1465) {
      d1465 = 1;
      {
        u_value t1468 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1468;
      }
    }
    if (!d1465) {
      u_no_case(s1464);
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
        static struct u_object l__61__62__1469 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1469), r_ts);
        p_parse_46_parse_body(v_sc, &v_body, r_ts);
        static struct u_object l_end_1470 = U_LIT("end");
        p_parse_46_keyword(&l_end_1470, r_ts);
        u_value t1471 = u_mk_con(&n_syntax_46_EProc, 4, v_ins, v_outs, v_refs, v_body);
        *r_e = t1471;
      }
    } else {
      {
        p_parse_46_lambda_function(v_sc, r_e, r_ts);
      }
    }
  }
}

u_value f_parse_46_at_section_1(u_value v_ts) {
  static struct u_object l_in_1472 = U_LIT("in");
  u_value t1474 = f_parse_46_at_keyword_2(v_ts, &l_in_1472);
  if (!u_truth(t1474)) {
    static struct u_object l_out_1473 = U_LIT("out");
    t1474 = f_parse_46_at_keyword_2(v_ts, &l_out_1473);
  }
  u_value t1476 = t1474;
  if (!u_truth(t1476)) {
    static struct u_object l_ref_1475 = U_LIT("ref");
    t1476 = f_parse_46_at_keyword_2(v_ts, &l_ref_1475);
  }
  return t1476;
}

void p_parse_46_lambda_function(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    u_value t1477 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1477;
    static struct u_object l__61__62__1478 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1478)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1479 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1479;
        u_value t1481 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1481)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1480 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1480;
          }
          t1481 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1482 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1482), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1483 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1483;
  }
}

void p_parse_46_parse_braces(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_at = NULL;
    u_value v_k = NULL;
    u_value v_v = NULL;
    u_value v_entries = NULL;
    u_value v_rev = NULL;
    u_value t1484 = f_parse_46_head_offset_1(*r_ts);
    v_at = t1484;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_expr(v_sc, &v_k, r_ts);
    static struct u_object l__61__62__1485 = U_LIT("=>");
    u_value t1486 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1485));
    v_entries = t1486;
    if (u_truth(v_entries)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_v, r_ts);
      }
    }
    static struct u_object l_for_1487 = U_LIT("for");
    u_value t1488 = f_parse_46_at_keyword_2(*r_ts, &l_for_1487);
    if (u_truth(t1488)) {
      t1488 = v_entries;
    }
    if (u_truth(t1488)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_comprehension(v_sc, v_k, v_v, r_e, r_ts);
      }
    } else {
      static struct u_object l_for_1489 = U_LIT("for");
      if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1489))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_set_comprehension(v_sc, v_k, r_e, r_ts);
        }
      } else {
        {
          u_value t1490;
          if (u_truth(v_entries)) {
            t1490 = u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))));
          } else {
            t1490 = v_k;
          }
          u_value t1491 = u_mk_con(&n_list_46_Cons, 2, t1490, u_mk_con(&n_list_46_Nil, 0));
          v_rev = t1491;
          u_value t1496 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          while (u_truth(t1496)) {
            {
              p_parse_46_advance(r_ts);
              p_parse_46_parse_expr(v_sc, &v_k, r_ts);
              static struct u_object l__61__62__1492 = U_LIT("=>");
              if (u_truth(f_prelude_46__33__61__2(v_entries, u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1492))))) {
                {
                  static struct u_object l_a_32_literal_32__123__46__46__46__125__32__1493 = U_LIT("a literal {...} has either elements a or entries k => v");
                  u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_a_32_literal_32__123__46__46__46__125__32__1493, v_at), "parse.ParseError");
                }
              }
              if (u_truth(v_entries)) {
                {
                  p_parse_46_advance(r_ts);
                  p_parse_46_parse_expr(v_sc, &v_v, r_ts);
                  u_value t1494 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)))), v_rev);
                  v_rev = t1494;
                }
              } else {
                {
                  u_value t1495 = u_mk_con(&n_list_46_Cons, 2, v_k, v_rev);
                  v_rev = t1495;
                }
              }
            }
            t1496 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          }
          u_value t1503;
          if (u_truth(v_entries)) {
            static struct u_object l_map_1497 = U_LIT("map");
            static struct u_object l_put_all_1498 = U_LIT("put_all");
            static struct u_object l_map_1499 = U_LIT("map");
            static struct u_object l_empty_1500 = U_LIT("empty");
            t1503 = f_parse_46_in_library_3(&l_map_1497, &l_put_all_1498, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1499, &l_empty_1500, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
          } else {
            static struct u_object l_set_1501 = U_LIT("set");
            static struct u_object l_from_list_1502 = U_LIT("from_list");
            t1503 = f_parse_46_in_library_3(&l_set_1501, &l_from_list_1502, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Nil, 0)));
          }
          u_value t1504 = t1503;
          *r_e = t1504;
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
  static struct u_object l_Nil_1505 = U_LIT("Nil");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1505), u_mk_con(&n_syntax_46_THole, 0)));
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
    u_value t1506 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1506;
    u_value t1507 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1507;
    u_value t1508 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1508;
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l_in_1509 = U_LIT("in");
    p_parse_46_keyword(&l_in_1509, r_ts);
    p_parse_46_parse_expr(v_sc, &v_l, r_ts);
    static struct u_object l_set_1510 = U_LIT("set");
    static struct u_object l_elements_1511 = U_LIT("elements");
    u_value t1512 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    u_value t1513 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1510, &l_elements_1511, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
    v_xs = t1512;
    v_ls = t1513;
    u_value t1519 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1519)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l_in_1514 = U_LIT("in");
        p_parse_46_keyword(&l_in_1514, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_set_1515 = U_LIT("set");
        static struct u_object l_elements_1516 = U_LIT("elements");
        u_value t1517 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
        u_value t1518 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1515, &l_elements_1516, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), v_ls);
        v_xs = t1517;
        v_ls = t1518;
      }
      t1519 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1520 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1520))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
      }
    }
    static struct u_object l_set_1521 = U_LIT("set");
    static struct u_object l_from_list_1522 = U_LIT("from_list");
    u_value t1523 = f_parse_46_in_library_3(&l_set_1521, &l_from_list_1522, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, v_r, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Nil, 0)));
    *r_e = t1523;
  }
}

void p_parse_46_map_comprehension(u_value v_sc, u_value v_k, u_value v_v, u_value *r_e, u_value *r_ts) {
  {
    u_value v_gs = NULL;
    u_value v_ls = NULL;
    u_value v_c = NULL;
    u_value t1524 = u_mk_con(&n_list_46_Nil, 0);
    v_gs = t1524;
    u_value t1525 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1525;
    u_value t1526 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1526;
    p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
    u_value t1527 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1527)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
      }
      t1527 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1528 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1528))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
        u_value t1529 = f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), v_c);
        v_c = t1529;
      }
    }
    static struct u_object l_map_1530 = U_LIT("map");
    static struct u_object l_put_all_1531 = U_LIT("put_all");
    static struct u_object l_map_1532 = U_LIT("map");
    static struct u_object l_empty_1533 = U_LIT("empty");
    u_value t1534 = f_parse_46_in_library_3(&l_map_1530, &l_put_all_1531, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))))), f_list_46_reverse_1(f_parse_46_hidden_vars_1(v_gs)), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1532, &l_empty_1533, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1534;
  }
}

void p_parse_46_map_generator(u_value v_sc, u_value *r_gs, u_value *r_ls, u_value *r_ts) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value v_m = NULL;
    p_parse_46_parse_plain(v_sc, &v_a, r_ts);
    static struct u_object l__61__62__1535 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1535), r_ts);
    p_parse_46_parse_plain(v_sc, &v_b, r_ts);
    static struct u_object l_in_1536 = U_LIT("in");
    p_parse_46_keyword(&l_in_1536, r_ts);
    p_parse_46_parse_expr(v_sc, &v_m, r_ts);
    static struct u_object l_map_1537 = U_LIT("map");
    static struct u_object l_entries_1538 = U_LIT("entries");
    u_value t1539 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1537, &l_entries_1538, u_mk_con(&n_list_46_Cons, 2, f_parse_46_unpack_2(f_list_46_reverse_1(*r_gs), v_m), u_mk_con(&n_list_46_Nil, 0))), *r_ls);
    *r_ls = t1539;
    static struct u_object l__40_entry_32__1540 = U_LIT("(entry ");
    static struct u_object l__41__1541 = U_LIT(")");
    u_value t1542 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(3, v_a, v_b, u_concat(&l__40_entry_32__1540, u_concat(u_str_show(u_add(f_list_46_length_1(*r_gs), u_mk_int(1))), &l__41__1541))), *r_gs);
    *r_gs = t1542;
  }
}

u_value f_parse_46_hidden_vars_1(u_value v_gs) {
  u_value s1543 = v_gs;
  u_value t1544 = NULL;
  if ((!t1544) && u_is_con(s1543, &n_list_46_Nil)) {
    t1544 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1544) && u_is_con(s1543, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s1543, 0), 2);
    u_value v_rest = u_field(s1543, 1);
    t1544 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0)), f_parse_46_hidden_vars_1(v_rest));
  }
  if (!t1544) {
    u_no_case(s1543);
  }
  return t1544;
}

u_value f_parse_46_unpack_2(u_value v_gs, u_value v_e) {
  u_value s1545 = v_gs;
  u_value t1546 = NULL;
  if ((!t1546) && u_is_con(s1545, &n_list_46_Nil)) {
    t1546 = v_e;
  }
  if ((!t1546) && u_is_con(s1545, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s1545, 0), 0);
    u_value v_v = u_field(u_field(s1545, 0), 1);
    u_value v_h = u_field(u_field(s1545, 0), 2);
    u_value v_rest = u_field(s1545, 1);
    t1546 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_k), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_v), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_parse_46_unpack_2(v_rest, v_e)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1546) {
    u_no_case(s1545);
  }
  return t1546;
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
        static struct u_object l_Nil_1547 = U_LIT("Nil");
        u_value t1548 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1547), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1548;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1549 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1549))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1550 = U_LIT("in");
            p_parse_46_keyword(&l_in_1550, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1551 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1552 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1551;
            v_lists = t1552;
            u_value t1556 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1556)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1553 = U_LIT("in");
                p_parse_46_keyword(&l_in_1553, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1554 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1555 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1554;
                v_lists = t1555;
              }
              t1556 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1557 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1557;
            static struct u_object l_if_1558 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1558))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1559 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1559;
          }
        } else {
          {
            u_value t1560 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1560;
            u_value t1562 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1562)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1561 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1561;
              }
              t1562 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1563 = U_LIT("Nil");
            u_value t1564 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1563), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1564;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1565 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1565;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1566 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1566))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1567 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1567;
      }
    } else {
      {
        u_value t1568 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1568;
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
    u_value t1569 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1569;
    static struct u_object l_case_1570 = U_LIT("case");
    u_value t1573 = f_parse_46_at_keyword_2(*r_ts, &l_case_1570);
    while (u_truth(t1573)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1571 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1571), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1572 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1572;
      }
      static struct u_object l_case_1574 = U_LIT("case");
      t1573 = f_parse_46_at_keyword_2(*r_ts, &l_case_1574);
    }
    static struct u_object l_else_1575 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1575))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1576 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1576;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1577 = U_LIT("end");
    p_parse_46_keyword(&l_end_1577, r_ts);
    u_value t1578 = f_list_46_reverse_1(v_rev);
    *r_cs = t1578;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1579 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1579))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1580 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1580, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
  }
}

u_value f_parse_46_pat_op1_2(u_value v_op, u_value v_p) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_pat_prefix_2(u_value v_op, u_value v_p) {
  u_value s1581 = v_p;
  u_value t1582 = NULL;
  if ((!t1582) && (u_is_con(s1581, &n_syntax_46_PLit) && u_is_con(u_field(s1581, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1581, 0), 0);
    static struct u_object l__45__1583 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1583))) {
      t1582 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1582) {
    t1582 = f_parse_46_pat_op1_2(v_op, v_p);
  }
  if (!t1582) {
    u_no_case(s1581);
  }
  return t1582;
}

u_value f_parse_46_pat_op2_3(u_value v_op, u_value v_l, u_value v_r) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_pats_2(u_value v_ps, u_value v_rest) {
  u_value s1584 = v_ps;
  u_value t1585 = NULL;
  if ((!t1585) && u_is_con(s1584, &n_list_46_Nil)) {
    t1585 = v_rest;
  }
  if ((!t1585) && u_is_con(s1584, &n_list_46_Cons)) {
    u_value v_p = u_field(s1584, 0);
    u_value v_more = u_field(s1584, 1);
    static struct u_object l_Cons_1586 = U_LIT("Cons");
    t1585 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1586), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1585) {
    u_no_case(s1584);
  }
  return t1585;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1587 = v_t;
  u_value t1588 = NULL;
  if ((!t1588) && u_is_con(s1587, &n_lex_46_TName)) {
    u_value v_n = u_field(s1587, 0);
    static struct u_object l___1589 = U_LIT("_");
    static struct u_object l_true_1590 = U_LIT("true");
    static struct u_object l_false_1591 = U_LIT("false");
    u_value t1593 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1589, u_mk_con(&n_list_46_Cons, 2, &l_true_1590, u_mk_con(&n_list_46_Cons, 2, &l_false_1591, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1593)) {
      u_value t1592 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1592)) {
        t1592 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1593 = t1592;
    }
    t1588 = t1593;
  }
  if ((!t1588) && u_is_con(s1587, &n_lex_46_TNum)) {
    t1588 = u_mk_bool(1);
  }
  if ((!t1588) && u_is_con(s1587, &n_lex_46_TStr)) {
    t1588 = u_mk_bool(1);
  }
  if ((!t1588) && u_is_con(s1587, &n_lex_46_TChar)) {
    t1588 = u_mk_bool(1);
  }
  if ((!t1588) && u_is_con(s1587, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1587, 0);
    u_value t1594 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1594)) {
      t1594 = u_eq(v_c, u_mk_char(91));
    }
    t1588 = t1594;
  }
  if (!t1588) {
    t1588 = u_mk_bool(0);
  }
  if (!t1588) {
    u_no_case(s1587);
  }
  return t1588;
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
    u_value t1595 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1595;
    u_value t1596 = u_mk_bool(1);
    v_next = t1596;
    u_value t1597 = u_mk_bool(1);
    v_go = t1597;
    u_value t1606 = v_go;
    while (u_truth(t1606)) {
      {
        u_value s1598 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1599 = 0;
        if ((!d1599) && u_is_con(s1598, &n_option_46_Some)) {
          u_value v_op = u_field(s1598, 0);
          d1599 = 1;
          {
            u_value t1600 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1601 = u_mk_bool(1);
            v_rev = t1600;
            v_next = t1601;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1599) && u_is_con(s1598, &n_option_46_None)) {
          u_value t1604 = v_next;
          if (u_truth(t1604)) {
            t1604 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1604)) {
            d1599 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1602 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1603 = u_mk_bool(0);
              v_rev = t1602;
              v_next = t1603;
            }
          }
        }
        if (!d1599) {
          d1599 = 1;
          {
            u_value t1605 = u_mk_bool(0);
            v_go = t1605;
          }
        }
        if (!d1599) {
          u_no_case(s1598);
        }
      }
      t1606 = v_go;
    }
    u_value s1607 = v_rev;
    int d1608 = 0;
    if ((!d1608) && u_is_con(s1607, &n_list_46_Nil)) {
      d1608 = 1;
      {
        static struct u_object l_pattern_32_expected_1609 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1610 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1609, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1610)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1608) {
      d1608 = 1;
      {
        u_value t1611 = f_list_46_reverse_1(v_rev);
        *r_items = t1611;
      }
    }
    if (!d1608) {
      u_no_case(s1607);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1612 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1612;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1613 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1613;
        u_value t1615 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1615)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1614 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1614;
          }
          t1615 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1616 = f_list_46_reverse_1(v_rev);
    *r_ps = t1616;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1617 = f_parse_46_head_token_1(*r_ts);
    int d1618 = 0;
    if ((!d1618) && (u_is_con(s1617, &n_lex_46_TName) && u_equals(u_field(s1617, 0), u_mk_lit("_")))) {
      d1618 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1619 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1619;
      }
    }
    if ((!d1618) && (u_is_con(s1617, &n_lex_46_TName) && u_equals(u_field(s1617, 0), u_mk_lit("true")))) {
      d1618 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1620 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1620;
      }
    }
    if ((!d1618) && (u_is_con(s1617, &n_lex_46_TName) && u_equals(u_field(s1617, 0), u_mk_lit("false")))) {
      d1618 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1621 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1621;
      }
    }
    if ((!d1618) && u_is_con(s1617, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1617, 0);
      d1618 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1622 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1622;
      }
    }
    if ((!d1618) && u_is_con(s1617, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1617, 0);
      d1618 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1623 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1623;
      }
    }
    if ((!d1618) && u_is_con(s1617, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1617, 0);
      d1618 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1624 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1624;
      }
    }
    if ((!d1618) && (u_is_con(s1617, &n_lex_46_TPunct) && u_equals(u_field(s1617, 0), u_mk_char(40)))) {
      d1618 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1625 = v_ps;
        int d1626 = 0;
        if ((!d1626) && (u_is_con(s1625, &n_list_46_Cons) && u_is_con(u_field(s1625, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1625, 0);
          d1626 = 1;
          {
            u_value t1627 = v_q;
            *r_p = t1627;
          }
        }
        if (!d1626) {
          d1626 = 1;
          {
            u_value t1628 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1628;
          }
        }
        if (!d1626) {
          u_no_case(s1625);
        }
      }
    }
    if ((!d1618) && (u_is_con(s1617, &n_lex_46_TPunct) && u_equals(u_field(s1617, 0), u_mk_char(91)))) {
      d1618 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1629 = U_LIT("Nil");
            u_value t1630 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1629), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1630;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1631 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1631;
            u_value t1633 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1633)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1632 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1632;
              }
              t1633 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1634 = U_LIT("Nil");
            u_value t1635 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1634), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1635;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1636 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1636;
          }
        }
      }
    }
    if (!d1618) {
      d1618 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1637 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1637;
          }
        } else {
          {
            u_value t1638 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1638;
          }
        }
      }
    }
    if (!d1618) {
      u_no_case(s1617);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1639 = v_t;
  u_value t1640 = NULL;
  if ((!t1640) && u_is_con(s1639, &n_lex_46_TName)) {
    u_value v_n = u_field(s1639, 0);
    static struct u_object l_skip_1641 = U_LIT("skip");
    static struct u_object l_if_1642 = U_LIT("if");
    static struct u_object l_while_1643 = U_LIT("while");
    static struct u_object l_call_1644 = U_LIT("call");
    static struct u_object l_match_1645 = U_LIT("match");
    static struct u_object l_assert_1646 = U_LIT("assert");
    static struct u_object l_assign_1647 = U_LIT("assign");
    static struct u_object l_raise_1648 = U_LIT("raise");
    static struct u_object l_try_1649 = U_LIT("try");
    t1640 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1641, u_mk_con(&n_list_46_Cons, 2, &l_if_1642, u_mk_con(&n_list_46_Cons, 2, &l_while_1643, u_mk_con(&n_list_46_Cons, 2, &l_call_1644, u_mk_con(&n_list_46_Cons, 2, &l_match_1645, u_mk_con(&n_list_46_Cons, 2, &l_assert_1646, u_mk_con(&n_list_46_Cons, 2, &l_assign_1647, u_mk_con(&n_list_46_Cons, 2, &l_raise_1648, u_mk_con(&n_list_46_Cons, 2, &l_try_1649, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1640) {
    t1640 = u_mk_bool(0);
  }
  if (!t1640) {
    u_no_case(s1639);
  }
  return t1640;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1650 = v_cs;
  u_value t1651 = NULL;
  if ((!t1651) && u_is_con(s1650, &n_list_46_Nil)) {
    t1651 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1651) && (u_is_con(s1650, &n_list_46_Cons) && u_is_con(u_field(s1650, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1650, 0), 0);
    u_value v_g = u_field(u_field(s1650, 0), 1);
    u_value v_b = u_field(u_field(s1650, 0), 2);
    u_value v_more = u_field(s1650, 1);
    t1651 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1651) {
    u_no_case(s1650);
  }
  return t1651;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1652 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1652;
    u_value t1653 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1653;
    u_value t1654 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1654;
    static struct u_object l_local_1655 = U_LIT("local");
    u_value t1657 = f_parse_46_at_keyword_2(*r_ts, &l_local_1655);
    while (u_truth(t1657)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1656 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1656)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1656 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1658 = U_LIT("local");
      t1657 = f_parse_46_at_keyword_2(*r_ts, &l_local_1658);
    }
    u_value t1660 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1660)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1659 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1659;
      }
      t1660 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1661 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1661;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1662 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1662;
    static struct u_object l__58__1663 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1663)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__1664 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1664)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1665 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1665;
      }
    }
    u_value t1666 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1666;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1667 = f_parse_46_head_token_1(*r_ts);
    int d1668 = 0;
    if ((!d1668) && (u_is_con(s1667, &n_lex_46_TName) && u_equals(u_field(s1667, 0), u_mk_lit("skip")))) {
      d1668 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1669 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1669;
      }
    }
    if ((!d1668) && (u_is_con(s1667, &n_lex_46_TName) && u_equals(u_field(s1667, 0), u_mk_lit("if")))) {
      d1668 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1668) && (u_is_con(s1667, &n_lex_46_TName) && u_equals(u_field(s1667, 0), u_mk_lit("while")))) {
      d1668 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1670 = U_LIT("end");
        p_parse_46_keyword(&l_end_1670, r_ts);
        u_value t1671 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1671;
      }
    }
    if ((!d1668) && (u_is_con(s1667, &n_lex_46_TName) && u_equals(u_field(s1667, 0), u_mk_lit("call")))) {
      d1668 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1668) && (u_is_con(s1667, &n_lex_46_TName) && u_equals(u_field(s1667, 0), u_mk_lit("match")))) {
      d1668 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1672 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1672;
      }
    }
    if ((!d1668) && (u_is_con(s1667, &n_lex_46_TName) && u_equals(u_field(s1667, 0), u_mk_lit("assert")))) {
      d1668 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1673 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1673;
      }
    }
    if ((!d1668) && (u_is_con(s1667, &n_lex_46_TName) && u_equals(u_field(s1667, 0), u_mk_lit("assign")))) {
      d1668 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1668) && (u_is_con(s1667, &n_lex_46_TName) && u_equals(u_field(s1667, 0), u_mk_lit("raise")))) {
      d1668 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1674 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1674;
      }
    }
    if ((!d1668) && (u_is_con(s1667, &n_lex_46_TName) && u_equals(u_field(s1667, 0), u_mk_lit("try")))) {
      d1668 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1675 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1675, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1676 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1676;
      }
    }
    if (!d1668) {
      d1668 = 1;
      {
        static struct u_object l_statement_32_expect_1677 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1678 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1677, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1678)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1668) {
      u_no_case(s1667);
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
    static struct u_object l_else_1679 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1679)))) {
      {
        static struct u_object l_end_1680 = U_LIT("end");
        p_parse_46_keyword(&l_end_1680, r_ts);
        u_value t1681 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1681;
      }
    } else {
      static struct u_object l_if_1682 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1682)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1683 = U_LIT("end");
          p_parse_46_keyword(&l_end_1683, r_ts);
        }
      }
    }
    u_value t1684 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1684;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1685 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1685), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1686 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1686;
    u_value t1689 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1689)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1687 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1687), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1688 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1688;
      }
      t1689 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1690 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1690;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1691 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1691;
    static struct u_object l_case_1692 = U_LIT("case");
    u_value t1695 = f_parse_46_at_keyword_2(*r_ts, &l_case_1692);
    while (u_truth(t1695)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1693 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1693), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1694 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1694;
      }
      static struct u_object l_case_1696 = U_LIT("case");
      t1695 = f_parse_46_at_keyword_2(*r_ts, &l_case_1696);
    }
    static struct u_object l_else_1697 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1697))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1698 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1698;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1699 = U_LIT("end");
    p_parse_46_keyword(&l_end_1699, r_ts);
    u_value t1700 = f_list_46_reverse_1(v_rev);
    *r_cs = t1700;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1702;
  if (u_truth(v_first)) {
    t1702 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1701 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1701)) {
      t1701 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1702 = t1701;
  }
  return t1702;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1704 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1704)) {
    static struct u_object l_out_1703 = U_LIT("out");
    t1704 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1703));
  }
  u_value t1706 = t1704;
  if (u_truth(t1706)) {
    static struct u_object l_ref_1705 = U_LIT("ref");
    t1706 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1705));
  }
  return t1706;
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
    u_value t1707 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), v_ins, v_outs, v_refs);
    *r_s = t1707;
  }
}

void p_parse_46_sections(u_value v_item_in, u_value v_item, u_value v_sc, u_value *r_ins, u_value *r_outs, u_value *r_refs, u_value *r_ts) {
  {
    u_value v_first = NULL;
    u_value t1708 = u_mk_bool(1);
    v_first = t1708;
    u_value t1709 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1710 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1711 = u_mk_con(&n_list_46_Nil, 0);
    *r_ins = t1709;
    *r_outs = t1710;
    *r_refs = t1711;
    static struct u_object l_in_1712 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1712))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_section_items(v_item_in, v_sc, r_ins, r_ts);
        u_value t1713 = u_mk_bool(0);
        v_first = t1713;
      }
    }
    static struct u_object l_out_1714 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1714, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_section_items(v_item, v_sc, r_outs, r_ts);
        u_value t1715 = u_mk_bool(0);
        v_first = t1715;
      }
    }
    static struct u_object l_ref_1716 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1716, v_first))) {
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
    u_value t1717 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1717;
    u_value t1719 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1719)) {
      {
        p_parse_46_advance(r_ts);
        u_call_proc(v_item, 1, 2, v_sc, &v_x, r_ts);
        u_value t1718 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1718;
      }
      t1719 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1720 = f_list_46_reverse_1(v_rev);
    *r_xs = t1720;
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
    u_value t1721 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0));
    *r_x = t1721;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1722 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1722;
    static struct u_object l__58__1723 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1723)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1724 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1724;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1725 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1725;
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
  u_value s1726 = v_cs;
  u_value t1727 = NULL;
  if ((!t1727) && u_is_con(s1726, &n_list_46_Nil)) {
    t1727 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1727) && (u_is_con(s1726, &n_list_46_Cons) && u_is_con(u_field(s1726, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1726, 1);
    t1727 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1727) && (u_is_con(s1726, &n_list_46_Cons) && u_is_con(u_field(s1726, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1726, 1);
    t1727 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1727) && u_is_con(s1726, &n_list_46_Cons)) {
    u_value v_c = u_field(s1726, 0);
    u_value v_more = u_field(s1726, 1);
    t1727 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1727) {
    u_no_case(s1726);
  }
  return t1727;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1728 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1729 = &t1728;
  u_value l1730 = v_cs;
  while (u_is_con(l1730, &n_list_46_Cons)) {
    u_value v_c = u_field(l1730, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1729 = u_append(e1729, v_c);
    }
    l1730 = u_field(l1730, 1);
  }
  return t1728;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1731 = v_c;
  u_value t1732 = NULL;
  if ((!t1732) && u_is_con(s1731, &n_syntax_46_CNotation)) {
    t1732 = u_mk_bool(1);
  }
  if (!t1732) {
    t1732 = u_mk_bool(0);
  }
  if (!t1732) {
    u_no_case(s1731);
  }
  return t1732;
}

u_value f_parse_46_Context_46_notations_1(u_value v_x) {
  u_value s1733 = v_x;
  u_value t1734 = NULL;
  if ((!t1734) && u_is_con(s1733, &n_parse_46_Context)) {
    u_value v_y = u_field(s1733, 0);
    t1734 = v_y;
  }
  if (!t1734) {
    u_no_case(s1733);
  }
  return t1734;
}

u_value f_parse_46_Context_46_modules_1(u_value v_x) {
  u_value s1735 = v_x;
  u_value t1736 = NULL;
  if ((!t1736) && u_is_con(s1735, &n_parse_46_Context)) {
    u_value v_y = u_field(s1735, 1);
    t1736 = v_y;
  }
  if (!t1736) {
    u_no_case(s1735);
  }
  return t1736;
}

u_value f_parse_46_Context_46_loading_1(u_value v_x) {
  u_value s1737 = v_x;
  u_value t1738 = NULL;
  if ((!t1738) && u_is_con(s1737, &n_parse_46_Context)) {
    u_value v_y = u_field(s1737, 2);
    t1738 = v_y;
  }
  if (!t1738) {
    u_no_case(s1737);
  }
  return t1738;
}

u_value f_parse_46_Context_46_base_1(u_value v_x) {
  u_value s1739 = v_x;
  u_value t1740 = NULL;
  if ((!t1740) && u_is_con(s1739, &n_parse_46_Context)) {
    u_value v_y = u_field(s1739, 3);
    t1740 = v_y;
  }
  if (!t1740) {
    u_no_case(s1739);
  }
  return t1740;
}

u_value f_parse_46_Context_46_dir_1(u_value v_x) {
  u_value s1741 = v_x;
  u_value t1742 = NULL;
  if ((!t1742) && u_is_con(s1741, &n_parse_46_Context)) {
    u_value v_y = u_field(s1741, 4);
    t1742 = v_y;
  }
  if (!t1742) {
    u_no_case(s1741);
  }
  return t1742;
}

u_value f_parse_46_Context_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1743 = v_x;
  u_value t1744 = NULL;
  if ((!t1744) && u_is_con(s1743, &n_parse_46_Context)) {
    u_value v_a1 = u_field(s1743, 1);
    u_value v_a2 = u_field(s1743, 2);
    u_value v_a3 = u_field(s1743, 3);
    u_value v_a4 = u_field(s1743, 4);
    t1744 = u_mk_con(&n_parse_46_Context, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1744) {
    u_no_case(s1743);
  }
  return t1744;
}

u_value f_parse_46_Context_46_modules_58__61__2(u_value v_x, u_value v_y) {
  u_value s1745 = v_x;
  u_value t1746 = NULL;
  if ((!t1746) && u_is_con(s1745, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1745, 0);
    u_value v_a2 = u_field(s1745, 2);
    u_value v_a3 = u_field(s1745, 3);
    u_value v_a4 = u_field(s1745, 4);
    t1746 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t1746) {
    u_no_case(s1745);
  }
  return t1746;
}

u_value f_parse_46_Context_46_loading_58__61__2(u_value v_x, u_value v_y) {
  u_value s1747 = v_x;
  u_value t1748 = NULL;
  if ((!t1748) && u_is_con(s1747, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1747, 0);
    u_value v_a1 = u_field(s1747, 1);
    u_value v_a3 = u_field(s1747, 3);
    u_value v_a4 = u_field(s1747, 4);
    t1748 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t1748) {
    u_no_case(s1747);
  }
  return t1748;
}

u_value f_parse_46_Context_46_base_58__61__2(u_value v_x, u_value v_y) {
  u_value s1749 = v_x;
  u_value t1750 = NULL;
  if ((!t1750) && u_is_con(s1749, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1749, 0);
    u_value v_a1 = u_field(s1749, 1);
    u_value v_a2 = u_field(s1749, 2);
    u_value v_a4 = u_field(s1749, 4);
    t1750 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t1750) {
    u_no_case(s1749);
  }
  return t1750;
}

u_value f_parse_46_Context_46_dir_58__61__2(u_value v_x, u_value v_y) {
  u_value s1751 = v_x;
  u_value t1752 = NULL;
  if ((!t1752) && u_is_con(s1751, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1751, 0);
    u_value v_a1 = u_field(s1751, 1);
    u_value v_a2 = u_field(s1751, 2);
    u_value v_a3 = u_field(s1751, 3);
    t1752 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t1752) {
    u_no_case(s1751);
  }
  return t1752;
}

u_value f_parse_46_with_commands_2(u_value v_cx, u_value v_cs) {
  return f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_with_notations_2(f_parse_46_Context_46_notations_1(v_cx), v_cs));
}

u_value f_parse_46_with_module_2(u_value v_cx, u_value v_c) {
  return f_parse_46_Context_46_modules_58__61__2(v_cx, f_list_46__43__43__2(f_parse_46_modules_of_1(v_c), f_parse_46_Context_46_modules_1(v_cx)));
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1753 = v_c;
  u_value t1754 = NULL;
  if ((!t1754) && u_is_con(s1753, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1753, 0);
    u_value v_cs = u_field(s1753, 1);
    u_value t1755 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1756 = &t1755;
    u_value l1757 = v_cs;
    while (u_is_con(l1757, &n_list_46_Cons)) {
      u_value v_d = u_field(l1757, 0);
      if (u_truth(u_mk_bool(1))) {
        e1756 = u_append(e1756, f_parse_46_modules_of_1(v_d));
      }
      l1757 = u_field(l1757, 1);
    }
    t1754 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1755));
  }
  if (!t1754) {
    t1754 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1754) {
    u_no_case(s1753);
  }
  return t1754;
}

u_value f_parse_46_own_notations_2(u_value v_cx, u_value v_n) {
  u_value s1758 = f_list_46_lookup_2(v_n, f_parse_46_Context_46_modules_1(v_cx));
  u_value t1759 = NULL;
  if ((!t1759) && u_is_con(s1758, &n_option_46_Some)) {
    u_value v_ns = u_field(s1758, 0);
    t1759 = v_ns;
  }
  if ((!t1759) && u_is_con(s1758, &n_option_46_None)) {
    t1759 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1759) {
    u_no_case(s1758);
  }
  return t1759;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1760 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1760;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1761 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1761;
        u_value t1763 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1763)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1762 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1762;
          }
          t1763 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1764 = f_list_46_reverse_1(v_rev);
    *r_ps = t1764;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1765 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1765;
    u_value s1766 = f_parse_46_head_token_1(*r_ts);
    int d1767 = 0;
    if ((!d1767) && (u_is_con(s1766, &n_lex_46_TName) && u_equals(u_field(s1766, 0), u_mk_lit("prefix")))) {
      d1767 = 1;
      {
        u_value t1768 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1768;
      }
    }
    if ((!d1767) && (u_is_con(s1766, &n_lex_46_TName) && u_equals(u_field(s1766, 0), u_mk_lit("postfix")))) {
      d1767 = 1;
      {
        u_value t1769 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1769;
      }
    }
    if (!d1767) {
      d1767 = 1;
      {
        u_value s1770 = f_parse_46_second_token_1(*r_ts);
        int d1771 = 0;
        if ((!d1771) && (u_is_con(s1770, &n_lex_46_TName) && u_equals(u_field(s1770, 0), u_mk_lit("left")))) {
          d1771 = 1;
          {
            u_value t1772 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1772;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1771) && (u_is_con(s1770, &n_lex_46_TName) && u_equals(u_field(s1770, 0), u_mk_lit("right")))) {
          d1771 = 1;
          {
            u_value t1773 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1773;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1771) {
          d1771 = 1;
          {
          }
        }
        if (!d1771) {
          u_no_case(s1770);
        }
      }
    }
    if (!d1767) {
      u_no_case(s1766);
    }
    p_parse_46_advance(r_ts);
    u_value s1774 = f_parse_46_head_token_1(*r_ts);
    int d1775 = 0;
    if ((!d1775) && u_is_con(s1774, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1774, 0);
      d1775 = 1;
      {
        u_value t1776 = v_n;
        v_p = t1776;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1775) {
      d1775 = 1;
      {
        static struct u_object l_precedence_32_expec_1777 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1778 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1777, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1778)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1775) {
      u_no_case(s1774);
    }
    u_value t1779 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1780 = u_mk_bool(1);
    v_ops = t1779;
    v_go = t1780;
    u_value t1786 = v_go;
    while (u_truth(t1786)) {
      {
        u_value s1781 = f_parse_46_head_token_1(*r_ts);
        int d1782 = 0;
        if ((!d1782) && u_is_con(s1781, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1781, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1782 = 1;
            {
              u_value t1783 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1783;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1782) && u_is_con(s1781, &n_lex_46_TName)) {
          u_value v_n = u_field(s1781, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1782 = 1;
            {
              u_value t1784 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1784;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1782) {
          d1782 = 1;
          {
            u_value t1785 = u_mk_bool(0);
            v_go = t1785;
          }
        }
        if (!d1782) {
          u_no_case(s1781);
        }
      }
      t1786 = v_go;
    }
    u_value s1787 = v_ops;
    int d1788 = 0;
    if ((!d1788) && u_is_con(s1787, &n_list_46_Nil)) {
      d1788 = 1;
      {
        static struct u_object l_operator_32_expecte_1789 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1790 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1789, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1790)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1788) {
      d1788 = 1;
      {
        u_value t1791 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1791;
      }
    }
    if (!d1788) {
      u_no_case(s1787);
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
    u_value t1792 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1793 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1792;
    *r_fs = t1793;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1794 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
            u_value t1795 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
            v_args = t1794;
            *r_fs = t1795;
            u_value t1798 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1798)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t1796 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t1797 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t1796;
                *r_fs = t1797;
              }
              t1798 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1799 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t1800 = f_list_46_reverse_1(*r_fs);
    *r_k = t1799;
    *r_fs = t1800;
    u_value s1801 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d1802 = 0;
    if ((!d1802) && u_is_con(s1801, &n_option_46_Some)) {
      u_value v_f = u_field(s1801, 0);
      d1802 = 1;
      {
        static struct u_object l_repeated_32_field_32__1803 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__1804 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__1803, u_concat(v_f, u_concat(&l__32_of_32__1804, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1802) && u_is_con(s1801, &n_option_46_None)) {
      d1802 = 1;
      {
      }
    }
    if (!d1802) {
      u_no_case(s1801);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1805 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1806 = 0;
    if ((!d1806) && (u_is_con(u_field(s1805, 0), &n_lex_46_TName) && (u_is_con(u_field(s1805, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1805, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1805, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1806 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1807 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1807;
        }
      }
    }
    if (!d1806) {
      d1806 = 1;
      {
        u_value t1808 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1808;
      }
    }
    if (!d1806) {
      u_no_case(s1805);
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
    static struct u_object l__58__61__1809 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1809), r_ts);
    static struct u_object l__124__1810 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1810)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1811 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1811;
    static struct u_object l__124__1812 = U_LIT("|");
    u_value t1814 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1812));
    while (u_truth(t1814)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1813 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1813;
      }
      static struct u_object l__124__1815 = U_LIT("|");
      t1814 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1815));
    }
    u_value s1816 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d1817 = 0;
    if ((!d1817) && u_is_con(s1816, &n_option_46_Some)) {
      u_value v_f = u_field(s1816, 0);
      d1817 = 1;
      {
        static struct u_object l_field_32__1818 = U_LIT("field ");
        static struct u_object l__32_of_32__1819 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_1820 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__1818, u_concat(v_f, u_concat(&l__32_of_32__1819, u_concat(v_n, &l__32_has_32_different_32_t_1820)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1817) && u_is_con(s1816, &n_option_46_None)) {
      d1817 = 1;
      {
      }
    }
    if (!d1817) {
      u_no_case(s1816);
    }
    u_value t1821 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_list_46__43__43__2(f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)), f_parse_46_updaters_3(v_n, v_ps, f_list_46_reverse_1(v_rev))));
    *r_cs = t1821;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s1822 = v_fs;
  u_value t1823 = NULL;
  if ((!t1823) && u_is_con(s1822, &n_list_46_Nil)) {
    t1823 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1823) && (u_is_con(s1822, &n_list_46_Cons) && u_is_con(u_field(s1822, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s1822, 0), 0);
    u_value v_rest = u_field(s1822, 1);
    t1823 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t1823) && (u_is_con(s1822, &n_list_46_Cons) && u_is_con(u_field(s1822, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1822, 1);
    t1823 = f_parse_46_field_names_1(v_rest);
  }
  if (!t1823) {
    u_no_case(s1822);
  }
  return t1823;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s1824 = v_fs;
  u_value t1825 = NULL;
  if ((!t1825) && u_is_con(s1824, &n_list_46_Nil)) {
    t1825 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1825) && u_is_con(s1824, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1824, 0), 0);
    u_value v_t = u_field(u_field(u_field(s1824, 0), 1), 3);
    u_value v_rest = u_field(s1824, 1);
    u_value t1827;
    if (u_truth(f_list_46_exists_2(f_parse_46_fields_named_2(v_f, v_rest), u_mk_closure(lam1826, 1, v_t)))) {
      t1827 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t1827 = f_parse_46_differing_1(v_rest);
    }
    t1825 = t1827;
  }
  if (!t1825) {
    u_no_case(s1824);
  }
  return t1825;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s1828 = v_e;
  u_value t1829 = NULL;
  if (!t1829) {
    u_value v_t = u_field(u_field(s1828, 1), 3);
    t1829 = v_t;
  }
  if (!t1829) {
    u_no_case(s1828);
  }
  return t1829;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s1830 = v_fs;
  u_value t1831 = NULL;
  if ((!t1831) && u_is_con(s1830, &n_list_46_Nil)) {
    t1831 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1831) && u_is_con(s1830, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s1830, 0), 0);
    u_value v_d = u_field(u_field(s1830, 0), 1);
    u_value v_rest = u_field(s1830, 1);
    u_value t1832;
    if (u_truth(u_eq(v_g, v_f))) {
      t1832 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t1832 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t1831 = t1832;
  }
  if (!t1831) {
    u_no_case(s1830);
  }
  return t1831;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1833 = v_ks;
  u_value t1834 = NULL;
  if ((!t1834) && u_is_con(s1833, &n_list_46_Nil)) {
    t1834 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1834) && (u_is_con(s1833, &n_list_46_Cons) && u_is_con(u_field(u_field(s1833, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1833, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1833, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1833, 0), 1);
    u_value v_rest = u_field(s1833, 1);
    t1834 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1834) {
    u_no_case(s1833);
  }
  return t1834;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1835 = v_fs;
  u_value t1836 = NULL;
  if ((!t1836) && u_is_con(s1835, &n_list_46_Nil)) {
    t1836 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1836) && (u_is_con(s1835, &n_list_46_Cons) && u_is_con(u_field(u_field(s1835, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1835, 0), 0), 0);
    u_value v_t = u_field(u_field(s1835, 0), 1);
    u_value v_rest = u_field(s1835, 1);
    t1836 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1836) && (u_is_con(s1835, &n_list_46_Cons) && u_is_con(u_field(u_field(s1835, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1835, 1);
    t1836 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1836) {
    u_no_case(s1835);
  }
  return t1836;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1841 = f_parse_46_fields_of_1(v_ks);
  u_value t1840;
  {
    u_value v_fs = t1841;
    u_value t1837 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1838 = &t1837;
    u_value l1839 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1839, &n_list_46_Cons)) {
      u_value v_f = u_field(l1839, 0);
      if (u_truth(u_mk_bool(1))) {
        e1838 = u_append(e1838, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1839 = u_field(l1839, 1);
    }
    t1840 = t1837;
  }
  return t1840;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1842 = U_LIT("x");
  u_value t1843 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1844 = &t1843;
  u_value l1845 = v_ps;
  while (u_is_con(l1845, &n_list_46_Cons)) {
    u_value v_p = u_field(l1845, 0);
    if (u_truth(u_mk_bool(1))) {
      e1844 = u_append(e1844, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1845 = u_field(l1845, 1);
  }
  u_value t1853 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1842), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1854 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1843);
  u_value t1852;
  {
    u_value v_x = t1853;
    u_value v_owner = t1854;
    static struct u_object l__46__1846 = U_LIT(".");
    static struct u_object l_x_1847 = U_LIT("x");
    u_value t1848 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1849 = &t1848;
    u_value l1850 = v_es;
    while (u_is_con(l1850, &n_list_46_Cons)) {
      u_value v_e = u_field(l1850, 0);
      if (u_truth(u_mk_bool(1))) {
        e1849 = u_append(e1849, f_parse_46_field_case_1(v_e));
      }
      l1850 = u_field(l1850, 1);
    }
    u_value t1851;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1851 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1851 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1852 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1846, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1847), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1848, t1851)));
  }
  return t1852;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1855 = v_es;
  u_value t1856 = NULL;
  if ((!t1856) && u_is_con(s1855, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1855, 0), 1), 3);
    t1856 = v_t;
  }
  if (!t1856) {
    t1856 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1856) {
    u_no_case(s1855);
  }
  return t1856;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s1857 = v_e;
  u_value t1858 = NULL;
  if (!t1858) {
    u_value v_k = u_field(u_field(s1857, 1), 0);
    u_value v_i = u_field(u_field(s1857, 1), 1);
    u_value v_n = u_field(u_field(s1857, 1), 2);
    u_value t1859 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1860 = &t1859;
    u_value l1861 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1861, &n_list_46_Cons)) {
      u_value v_j = u_field(l1861, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1863;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1862 = U_LIT("y");
          t1863 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1862), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1863 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1860 = u_append(e1860, t1863);
      }
      l1861 = u_field(l1861, 1);
    }
    static struct u_object l_y_1864 = U_LIT("y");
    t1858 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1859), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1864), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1858) {
    u_no_case(s1857);
  }
  return t1858;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_MissingField_1865 = U_LIT("MissingField");
  static struct u_object l__46__1866 = U_LIT(".");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_ERaise, 2, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_MissingField_1865), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_t, u_concat(&l__46__1866, v_f)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_parse_46_updaters_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1871 = f_parse_46_fields_of_1(v_ks);
  u_value t1870;
  {
    u_value v_fs = t1871;
    u_value t1867 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1868 = &t1867;
    u_value l1869 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1869, &n_list_46_Cons)) {
      u_value v_f = u_field(l1869, 0);
      if (u_truth(u_mk_bool(1))) {
        e1868 = u_append(e1868, f_parse_46_updater_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1869 = u_field(l1869, 1);
    }
    t1870 = t1867;
  }
  return t1870;
}

u_value f_parse_46_updater_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1872 = U_LIT("x");
  u_value t1873 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1874 = &t1873;
  u_value l1875 = v_ps;
  while (u_is_con(l1875, &n_list_46_Cons)) {
    u_value v_p = u_field(l1875, 0);
    if (u_truth(u_mk_bool(1))) {
      e1874 = u_append(e1874, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1875 = u_field(l1875, 1);
  }
  u_value t1885 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1872), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1886 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1873);
  u_value t1884;
  {
    u_value v_x = t1885;
    u_value v_owner = t1886;
    static struct u_object l__46__1876 = U_LIT(".");
    static struct u_object l__58__61__1877 = U_LIT(":=");
    static struct u_object l_x_1878 = U_LIT("x");
    static struct u_object l_y_1879 = U_LIT("y");
    u_value t1880 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1881 = &t1880;
    u_value l1882 = v_es;
    while (u_is_con(l1882, &n_list_46_Cons)) {
      u_value v_e = u_field(l1882, 0);
      if (u_truth(u_mk_bool(1))) {
        e1881 = u_append(e1881, f_parse_46_update_case_1(v_e));
      }
      l1882 = u_field(l1882, 1);
    }
    u_value t1883;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1883 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1883 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1884 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1876, u_concat(v_f, &l__58__61__1877)))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1878), v_owner), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1879), f_parse_46_field_type_1(v_es)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), v_owner, u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1880, t1883)));
  }
  return t1884;
}

u_value f_parse_46_update_case_1(u_value v_e) {
  u_value s1887 = v_e;
  u_value t1888 = NULL;
  if (!t1888) {
    u_value v_k = u_field(u_field(s1887, 1), 0);
    u_value v_i = u_field(u_field(s1887, 1), 1);
    u_value v_n = u_field(u_field(s1887, 1), 2);
    u_value t1889 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1890 = &t1889;
    u_value l1891 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1891, &n_list_46_Cons)) {
      u_value v_j = u_field(l1891, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1893;
        if (u_truth(u_eq(v_j, v_i))) {
          t1893 = u_mk_con(&n_syntax_46_PWild, 0);
        } else {
          static struct u_object l_a_1892 = U_LIT("a");
          t1893 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l_a_1892, u_str_show(v_j))), u_mk_con(&n_syntax_46_THole, 0)));
        }
        e1890 = u_append(e1890, t1893);
      }
      l1891 = u_field(l1891, 1);
    }
    u_value t1894 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1895 = &t1894;
    u_value l1896 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1896, &n_list_46_Cons)) {
      u_value v_j = u_field(l1896, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1899;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1897 = U_LIT("y");
          t1899 = &l_y_1897;
        } else {
          static struct u_object l_a_1898 = U_LIT("a");
          t1899 = u_concat(&l_a_1898, u_str_show(v_j));
        }
        e1895 = u_append(e1895, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(t1899), u_mk_con(&n_syntax_46_THole, 0))));
      }
      l1896 = u_field(l1896, 1);
    }
    t1888 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1889), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_k, u_mk_con(&n_syntax_46_THole, 0))), t1894));
  }
  if (!t1888) {
    u_no_case(s1887);
  }
  return t1888;
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t1900;
  if (u_truth(u_eq(v_i, v_n))) {
    t1900 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1900 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t1900;
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
    u_value t1901 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1901;
    u_value t1903 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1903)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1902 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1902;
      }
      t1903 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1904 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1904), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1905 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1905), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1906 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1906;
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
    static struct u_object l__58__61__1907 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1907), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1908 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1908;
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
    u_value t1909 = f_parse_46_Context_46_notations_1(v_cx);
    v_ns = t1909;
    u_value s1910 = f_parse_46_head_token_1(*r_ts);
    int d1911 = 0;
    if ((!d1911) && (u_is_con(s1910, &n_lex_46_TName) && u_equals(u_field(s1910, 0), u_mk_lit("import")))) {
      d1911 = 1;
      {
        u_value t1912 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1912;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Context_46_modules_1(v_cx))))) {
          {
            u_value t1913 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1913;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_cx, v_at, r_cs);
            u_value t1914 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1914;
          }
        }
      }
    }
    if ((!d1911) && u_is_con(s1910, &n_lex_46_TName)) {
      u_value v_w = u_field(s1910, 0);
      static struct u_object l_prefix_1916 = U_LIT("prefix");
      static struct u_object l_postfix_1917 = U_LIT("postfix");
      static struct u_object l_infix_1918 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1916, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1917, u_mk_con(&n_list_46_Cons, 2, &l_infix_1918, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1911 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1915 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1915;
        }
      }
    }
    if ((!d1911) && (u_is_con(s1910, &n_lex_46_TName) && u_equals(u_field(s1910, 0), u_mk_lit("type")))) {
      d1911 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1919 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1919), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1920 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1920;
      }
    }
    if ((!d1911) && (u_is_con(s1910, &n_lex_46_TName) && u_equals(u_field(s1910, 0), u_mk_lit("data")))) {
      d1911 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1911) && (u_is_con(s1910, &n_lex_46_TName) && u_equals(u_field(s1910, 0), u_mk_lit("function")))) {
      d1911 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1921 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1921;
      }
    }
    if ((!d1911) && (u_is_con(s1910, &n_lex_46_TName) && u_equals(u_field(s1910, 0), u_mk_lit("procedure")))) {
      d1911 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1922 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1922;
      }
    }
    if ((!d1911) && (u_is_con(s1910, &n_lex_46_TName) && u_equals(u_field(s1910, 0), u_mk_lit("run")))) {
      d1911 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1923 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1923), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1924 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1924;
      }
    }
    if ((!d1911) && (u_is_con(s1910, &n_lex_46_TName) && u_equals(u_field(s1910, 0), u_mk_lit("test")))) {
      d1911 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1925 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1925), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1926 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1926;
      }
    }
    if (!d1911) {
      d1911 = 1;
      {
        static struct u_object l_declaration_32_expe_1927 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1928 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1927, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1928)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1911) {
      u_no_case(s1910);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_cx) {
  {
    u_value s1929 = v_cs;
    int d1930 = 0;
    if ((!d1930) && u_is_con(s1929, &n_list_46_Nil)) {
      d1930 = 1;
      {
      }
    }
    if ((!d1930) && (u_is_con(s1929, &n_list_46_Cons) && u_is_con(u_field(s1929, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1929, 0), 0);
      u_value v_p = u_field(u_field(s1929, 0), 1);
      u_value v_ops = u_field(u_field(s1929, 0), 2);
      u_value v_more = u_field(s1929, 1);
      d1930 = 1;
      {
        u_value t1931 = f_parse_46_with_commands_2(*r_cx, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_cx = t1931;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1930) && (u_is_con(s1929, &n_list_46_Cons) && u_is_con(u_field(s1929, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1929, 0), 0);
      u_value v_ds = u_field(u_field(s1929, 0), 1);
      u_value v_more = u_field(s1929, 1);
      d1930 = 1;
      {
        u_value t1932 = f_parse_46_with_module_2(*r_cx, u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds));
        *r_cx = t1932;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1930) && (u_is_con(s1929, &n_list_46_Cons) && u_is_con(u_field(s1929, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1929, 0), 0);
      u_value v_more = u_field(s1929, 1);
      d1930 = 1;
      {
        u_value t1933 = f_parse_46_with_commands_2(*r_cx, f_parse_46_own_notations_2(*r_cx, v_n));
        *r_cx = t1933;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1930) && u_is_con(s1929, &n_list_46_Cons)) {
      u_value v_more = u_field(s1929, 1);
      d1930 = 1;
      {
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if (!d1930) {
      u_no_case(s1929);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_cx, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1934 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1934;
    u_value t1936 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1936)) {
      {
        p_parse_46_parse_command(*r_cx, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_cx);
        u_value t1935 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1935;
      }
      t1936 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1937 = f_list_46_reverse_1(v_rev);
    *r_cs = t1937;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1938 = v_dir;
    int d1939 = 0;
    if ((!d1939) && u_is_con(s1938, &n_option_46_Some)) {
      u_value v_d = u_field(s1938, 0);
      d1939 = 1;
      {
        {
          struct u_handler *h1942 = u_push();
          if (setjmp(h1942->env) == 0) {
            {
              static struct u_object l__47__1940 = U_LIT("/");
              static struct u_object l__46_u_1941 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1940, u_concat(v_n, &l__46_u_1941))), r_text);
            }
            u_pop(h1942);
          } else {
            u_pop(h1942);
            int d1943 = 0;
            if ((!d1943) && (u_has_type(h1942, "os.OsError") && u_is_con(u_caught(h1942), &n_os_46_OsError))) {
              d1943 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1943) {
              u_reraise(h1942);
            }
          }
        }
      }
    }
    if ((!d1939) && u_is_con(s1938, &n_option_46_None)) {
      d1939 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1939) {
      u_no_case(s1938);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1944 = u_push();
      if (setjmp(h1944->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1944);
      } else {
        u_pop(h1944);
        int d1945 = 0;
        if ((!d1945) && (u_has_type(h1944, "os.OsError") && u_is_con(u_caught(h1944), &n_os_46_OsError))) {
          d1945 = 1;
          {
            static struct u_object l_unknown_32_module_32__1946 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1946, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1945) {
          u_reraise(h1944);
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
        static struct u_object l_cyclic_32_import_32_of_1947 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1947, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Context_46_dir_1(v_cx), v_at, &v_text);
    u_value t1948 = f_parse_46_Context_46_loading_58__61__2(f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_base_notations_1(v_cx)), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Context_46_loading_1(v_cx)));
    v_mcx = t1948;
    {
      struct u_handler *h1949 = u_push();
      if (setjmp(h1949->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mcx, &v_mts);
        }
        u_pop(h1949);
      } else {
        u_pop(h1949);
        int d1950 = 0;
        if ((!d1950) && (u_has_type(h1949, "lex.LexError") && u_is_con(u_caught(h1949), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1949), 0);
          u_value v_i = u_field(u_caught(h1949), 1);
          d1950 = 1;
          {
            static struct u_object l__32_in_32__1951 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1951, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1950) && (u_has_type(h1949, "parse.ParseError") && u_is_con(u_caught(h1949), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1949), 0);
          u_value v_i = u_field(u_caught(h1949), 1);
          d1950 = 1;
          {
            static struct u_object l__32_in_32__1952 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1952, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1950) {
          u_reraise(h1949);
        }
      }
    }
  }
}

u_value f_parse_46_base_notations_1(u_value v_cx) {
  u_value s1953 = f_parse_46_Context_46_base_1(v_cx);
  u_value t1954 = NULL;
  if ((!t1954) && u_is_con(s1953, &n_option_46_Some)) {
    u_value v_b = u_field(s1953, 0);
    t1954 = v_b;
  }
  if ((!t1954) && u_is_con(s1953, &n_option_46_None)) {
    t1954 = f_parse_46_Context_46_notations_1(v_cx);
  }
  if (!t1954) {
    u_no_case(s1953);
  }
  return t1954;
}

void p_parse_46_parse_text(u_value v_text, u_value v_cx, u_value *r_p) {
  {
    u_value v_ts = NULL;
    u_value v_cs = NULL;
    u_value v_after = NULL;
    u_value t1955 = v_cx;
    v_after = t1955;
    {
      struct u_handler *h1956 = u_push();
      if (setjmp(h1956->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1956);
      } else {
        u_pop(h1956);
        int d1957 = 0;
        if ((!d1957) && (u_has_type(h1956, "lex.LexError") && u_is_con(u_caught(h1956), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1956), 0);
          u_value v_i = u_field(u_caught(h1956), 1);
          d1957 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1957) {
          u_reraise(h1956);
        }
      }
    }
    p_parse_46_parse_commands(&v_cs, &v_after, &v_ts);
    u_value t1958 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1958;
  }
}

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p) {
  {
    u_value v_cx = NULL;
    u_value t1959 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    v_cx = t1959;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), &v_cx);
    p_parse_46_parse_text(v_text, f_parse_46_Context_46_dir_58__61__2(f_parse_46_Context_46_base_58__61__2(f_parse_46_Context_46_loading_58__61__2(v_cx, u_mk_con(&n_list_46_Cons, 2, v_name, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_option_46_Some, 1, f_parse_46_Context_46_notations_1(v_cx))), v_dir), r_p);
  }
}

void p_parse_46_parse_library(u_value v_n, u_value *r_p) {
  {
    u_value v_text = NULL;
    u_value v_pre = NULL;
    p_parse_46_library_text(v_n, u_mk_int(0), &v_text);
    static struct u_object l_prelude_1960 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1960))) {
      {
        static struct u_object l_prelude_1961 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1961, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1962 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1962, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1963 = v_xs;
  u_value t1964 = NULL;
  if ((!t1964) && (u_is_con(s1963, &n_list_46_Cons) && u_is_con(u_field(s1963, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1963, 0);
    t1964 = v_x;
  }
  if ((!t1964) && u_is_con(s1963, &n_list_46_Cons)) {
    u_value v_more = u_field(s1963, 1);
    t1964 = f_parse_46_last_part_1(v_more);
  }
  if (!t1964) {
    static struct u_object l__1965 = U_LIT("");
    t1964 = &l__1965;
  }
  if (!t1964) {
    u_no_case(s1963);
  }
  return t1964;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1970 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1969;
  {
    u_value v_f = t1970;
    u_value t1967 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1967)) {
      static struct u_object l__46_u_1966 = U_LIT(".u");
      t1967 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1966);
    }
    u_value t1968;
    if (u_truth(t1967)) {
      t1968 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1968 = v_f;
    }
    t1969 = t1968;
  }
  return t1969;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1975 = u_str_split(v_path, u_mk_char(47));
  u_value t1974;
  {
    u_value v_parts = t1975;
    u_value t1973;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1971 = U_LIT(".");
      t1973 = &l__46__1971;
    } else {
      static struct u_object l__47__1972 = U_LIT("/");
      t1973 = u_str_join(&l__47__1972, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1974 = t1973;
  }
  return t1974;
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
    static struct u_object l_prelude_1976 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1976, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1977 = u_mk_tuple(2, v_cs, v_ds);
    int d1978 = 0;
    if ((!d1978) && (u_is_con(u_field(s1977, 0), &n_list_46_Cons) && u_is_con(u_field(s1977, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1977, 0), 0);
      u_value v_more = u_field(u_field(s1977, 0), 1);
      u_value v_d = u_field(u_field(s1977, 1), 0);
      u_value v_rest = u_field(u_field(s1977, 1), 1);
      d1978 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1978) {
      d1978 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1978) {
      u_no_case(s1977);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1979 = v_paths;
    int d1980 = 0;
    if ((!d1980) && u_is_con(s1979, &n_list_46_Nil)) {
      d1980 = 1;
      {
      }
    }
    if ((!d1980) && u_is_con(s1979, &n_list_46_Cons)) {
      u_value v_path = u_field(s1979, 0);
      u_value v_more = u_field(s1979, 1);
      d1980 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1980) {
      u_no_case(s1979);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1981 = v_paths;
    int d1982 = 0;
    if ((!d1982) && u_is_con(s1981, &n_list_46_Nil)) {
      d1982 = 1;
      {
      }
    }
    if ((!d1982) && u_is_con(s1981, &n_list_46_Cons)) {
      u_value v_path = u_field(s1981, 0);
      u_value v_more = u_field(s1981, 1);
      d1982 = 1;
      {
        u_value t1983 = u_mk_bool(0);
        v_failed = t1983;
        {
          struct u_handler *h1984 = u_push();
          if (setjmp(h1984->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1984);
          } else {
            u_pop(h1984);
            int d1985 = 0;
            if ((!d1985) && (u_has_type(h1984, "parse.ParseError") && u_is_con(u_caught(h1984), &n_parse_46_ParseError))) {
              d1985 = 1;
              {
                u_value t1986 = u_mk_bool(1);
                v_failed = t1986;
              }
            }
            if (!d1985) {
              u_reraise(h1984);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1982) {
      u_no_case(s1981);
    }
  }
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s1987 = v_x;
  u_value t1988 = NULL;
  if ((!t1988) && u_is_con(s1987, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1987, 0);
    t1988 = v_y;
  }
  if (!t1988) {
    u_no_case(s1987);
  }
  return t1988;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s1989 = v_x;
  u_value t1990 = NULL;
  if ((!t1990) && u_is_con(s1989, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1989, 1);
    t1990 = v_y;
  }
  if (!t1990) {
    u_no_case(s1989);
  }
  return t1990;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s1991 = v_x;
  u_value t1992 = NULL;
  if ((!t1992) && u_is_con(s1991, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1991, 2);
    t1992 = v_y;
  }
  if (!t1992) {
    u_no_case(s1991);
  }
  return t1992;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s1993 = v_x;
  u_value t1994 = NULL;
  if ((!t1994) && u_is_con(s1993, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1993, 3);
    t1994 = v_y;
  }
  if (!t1994) {
    u_no_case(s1993);
  }
  return t1994;
}

u_value f_compile_46_Globals_46_procs_1(u_value v_x) {
  u_value s1995 = v_x;
  u_value t1996 = NULL;
  if ((!t1996) && u_is_con(s1995, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1995, 4);
    t1996 = v_y;
  }
  if (!t1996) {
    u_no_case(s1995);
  }
  return t1996;
}

u_value f_compile_46_Globals_46_funs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1997 = v_x;
  u_value t1998 = NULL;
  if ((!t1998) && u_is_con(s1997, &n_compile_46_Globals)) {
    u_value v_a1 = u_field(s1997, 1);
    u_value v_a2 = u_field(s1997, 2);
    u_value v_a3 = u_field(s1997, 3);
    u_value v_a4 = u_field(s1997, 4);
    t1998 = u_mk_con(&n_compile_46_Globals, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1998) {
    u_no_case(s1997);
  }
  return t1998;
}

u_value f_compile_46_Globals_46_constrs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1999 = v_x;
  u_value t2000 = NULL;
  if ((!t2000) && u_is_con(s1999, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1999, 0);
    u_value v_a2 = u_field(s1999, 2);
    u_value v_a3 = u_field(s1999, 3);
    u_value v_a4 = u_field(s1999, 4);
    t2000 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t2000) {
    u_no_case(s1999);
  }
  return t2000;
}

u_value f_compile_46_Globals_46_arities_58__61__2(u_value v_x, u_value v_y) {
  u_value s2001 = v_x;
  u_value t2002 = NULL;
  if ((!t2002) && u_is_con(s2001, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s2001, 0);
    u_value v_a1 = u_field(s2001, 1);
    u_value v_a3 = u_field(s2001, 3);
    u_value v_a4 = u_field(s2001, 4);
    t2002 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t2002) {
    u_no_case(s2001);
  }
  return t2002;
}

u_value f_compile_46_Globals_46_fields_58__61__2(u_value v_x, u_value v_y) {
  u_value s2003 = v_x;
  u_value t2004 = NULL;
  if ((!t2004) && u_is_con(s2003, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s2003, 0);
    u_value v_a1 = u_field(s2003, 1);
    u_value v_a2 = u_field(s2003, 2);
    u_value v_a4 = u_field(s2003, 4);
    t2004 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t2004) {
    u_no_case(s2003);
  }
  return t2004;
}

u_value f_compile_46_Globals_46_procs_58__61__2(u_value v_x, u_value v_y) {
  u_value s2005 = v_x;
  u_value t2006 = NULL;
  if ((!t2006) && u_is_con(s2005, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s2005, 0);
    u_value v_a1 = u_field(s2005, 1);
    u_value v_a2 = u_field(s2005, 2);
    u_value v_a3 = u_field(s2005, 3);
    t2006 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t2006) {
    u_no_case(s2005);
  }
  return t2006;
}

u_value f_compile_46_Gen_46_count_1(u_value v_x) {
  u_value s2007 = v_x;
  u_value t2008 = NULL;
  if ((!t2008) && u_is_con(s2007, &n_compile_46_Gen)) {
    u_value v_y = u_field(s2007, 0);
    t2008 = v_y;
  }
  if (!t2008) {
    u_no_case(s2007);
  }
  return t2008;
}

u_value f_compile_46_Gen_46_lifted_1(u_value v_x) {
  u_value s2009 = v_x;
  u_value t2010 = NULL;
  if ((!t2010) && u_is_con(s2009, &n_compile_46_Gen)) {
    u_value v_y = u_field(s2009, 1);
    t2010 = v_y;
  }
  if (!t2010) {
    u_no_case(s2009);
  }
  return t2010;
}

u_value f_compile_46_Gen_46_count_58__61__2(u_value v_x, u_value v_y) {
  u_value s2011 = v_x;
  u_value t2012 = NULL;
  if ((!t2012) && u_is_con(s2011, &n_compile_46_Gen)) {
    u_value v_a1 = u_field(s2011, 1);
    t2012 = u_mk_con(&n_compile_46_Gen, 2, v_y, v_a1);
  }
  if (!t2012) {
    u_no_case(s2011);
  }
  return t2012;
}

u_value f_compile_46_Gen_46_lifted_58__61__2(u_value v_x, u_value v_y) {
  u_value s2013 = v_x;
  u_value t2014 = NULL;
  if ((!t2014) && u_is_con(s2013, &n_compile_46_Gen)) {
    u_value v_a0 = u_field(s2013, 0);
    t2014 = u_mk_con(&n_compile_46_Gen, 2, v_a0, v_y);
  }
  if (!t2014) {
    u_no_case(s2013);
  }
  return t2014;
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
  static struct u_object l_u_value_2015 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_2015);
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
  static struct u_object l_u_value_2016 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_2016));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t2017 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2018 = &t2017;
  u_value l2019 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l2019, &n_list_46_Cons)) {
    u_value v_c = u_field(l2019, 0);
    if (u_truth(u_mk_bool(1))) {
      e2018 = u_append(e2018, f_compile_46_mangle_char_1(v_c));
    }
    l2019 = u_field(l2019, 1);
  }
  return u_string(f_list_46_concat_1(t2017));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t2020 = u_is_letter(v_c);
  if (!u_truth(t2020)) {
    t2020 = u_is_digit(v_c);
  }
  u_value t2021 = t2020;
  if (!u_truth(t2021)) {
    t2021 = u_eq(v_c, u_mk_char(95));
  }
  u_value t2024;
  if (u_truth(t2021)) {
    t2024 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___2022 = U_LIT("_");
    static struct u_object l___2023 = U_LIT("_");
    t2024 = u_chars(u_concat(&l___2022, u_concat(u_str_show(u_ord(v_c)), &l___2023)));
  }
  return t2024;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__2025 = U_LIT("f_");
  static struct u_object l___2026 = U_LIT("_");
  return u_concat(&l_f__2025, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___2026, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__2027 = U_LIT("p_");
  return u_concat(&l_p__2027, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__2028 = U_LIT("v_");
  return u_concat(&l_v__2028, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__2029 = U_LIT("r_");
  return u_concat(&l_r__2029, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__2030 = U_LIT("+");
  static struct u_object l_u_add_2031 = U_LIT("u_add");
  static struct u_object l__45__2032 = U_LIT("-");
  static struct u_object l_u_sub_2033 = U_LIT("u_sub");
  static struct u_object l__42__2034 = U_LIT("*");
  static struct u_object l_u_mul_2035 = U_LIT("u_mul");
  static struct u_object l__47__2036 = U_LIT("/");
  static struct u_object l_u_div_2037 = U_LIT("u_div");
  static struct u_object l__37__2038 = U_LIT("%");
  static struct u_object l_u_mod_2039 = U_LIT("u_mod");
  static struct u_object l__60__2040 = U_LIT("<");
  static struct u_object l_u_lt_2041 = U_LIT("u_lt");
  static struct u_object l__61__61__2042 = U_LIT("==");
  static struct u_object l_u_eq_2043 = U_LIT("u_eq");
  static struct u_object l_compare_2044 = U_LIT("compare");
  static struct u_object l_u_compare_2045 = U_LIT("u_compare");
  static struct u_object l_chars_2046 = U_LIT("chars");
  static struct u_object l_u_chars_2047 = U_LIT("u_chars");
  static struct u_object l_string_2048 = U_LIT("string");
  static struct u_object l_u_string_2049 = U_LIT("u_string");
  static struct u_object l_ord_2050 = U_LIT("ord");
  static struct u_object l_u_ord_2051 = U_LIT("u_ord");
  static struct u_object l_chr_2052 = U_LIT("chr");
  static struct u_object l_u_chr_2053 = U_LIT("u_chr");
  static struct u_object l__94__2054 = U_LIT("^");
  static struct u_object l_u_concat_2055 = U_LIT("u_concat");
  static struct u_object l_str_size_2056 = U_LIT("str_size");
  static struct u_object l_u_str_size_2057 = U_LIT("u_str_size");
  static struct u_object l_str_join_2058 = U_LIT("str_join");
  static struct u_object l_u_str_join_2059 = U_LIT("u_str_join");
  static struct u_object l_str_slice_2060 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_2061 = U_LIT("u_str_slice");
  static struct u_object l_str_split_2062 = U_LIT("str_split");
  static struct u_object l_u_str_split_2063 = U_LIT("u_str_split");
  static struct u_object l_str_get_2064 = U_LIT("str_get");
  static struct u_object l_u_str_get_2065 = U_LIT("u_str_get");
  static struct u_object l_str_contains_2066 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_2067 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_2068 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_2069 = U_LIT("u_str_compare");
  static struct u_object l_str_show_2070 = U_LIT("str_show");
  static struct u_object l_u_str_show_2071 = U_LIT("u_str_show");
  static struct u_object l_str_quote_2072 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_2073 = U_LIT("u_str_quote");
  static struct u_object l_str_number_2074 = U_LIT("str_number");
  static struct u_object l_u_str_number_2075 = U_LIT("u_str_number");
  static struct u_object l_is_digit_2076 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_2077 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_2078 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_2079 = U_LIT("u_is_letter");
  static struct u_object l_is_space_2080 = U_LIT("is_space");
  static struct u_object l_u_is_space_2081 = U_LIT("u_is_space");
  u_pop(handler);
  cache = f_map_46_put_all_2(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__2030), u_mk_tuple(2, &l_u_add_2031, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__2032), u_mk_tuple(2, &l_u_sub_2033, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__2034), u_mk_tuple(2, &l_u_mul_2035, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__2036), u_mk_tuple(2, &l_u_div_2037, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__2038), u_mk_tuple(2, &l_u_mod_2039, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__2040), u_mk_tuple(2, &l_u_lt_2041, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__2042), u_mk_tuple(2, &l_u_eq_2043, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_2044), u_mk_tuple(2, &l_u_compare_2045, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_2046), u_mk_tuple(2, &l_u_chars_2047, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_2048), u_mk_tuple(2, &l_u_string_2049, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_2050), u_mk_tuple(2, &l_u_ord_2051, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_2052), u_mk_tuple(2, &l_u_chr_2053, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__2054), u_mk_tuple(2, &l_u_concat_2055, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_2056), u_mk_tuple(2, &l_u_str_size_2057, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_2058), u_mk_tuple(2, &l_u_str_join_2059, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_2060), u_mk_tuple(2, &l_u_str_slice_2061, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_2062), u_mk_tuple(2, &l_u_str_split_2063, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_2064), u_mk_tuple(2, &l_u_str_get_2065, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_2066), u_mk_tuple(2, &l_u_str_contains_2067, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_2068), u_mk_tuple(2, &l_u_str_compare_2069, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_2070), u_mk_tuple(2, &l_u_str_show_2071, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_2072), u_mk_tuple(2, &l_u_str_quote_2073, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_2074), u_mk_tuple(2, &l_u_str_number_2075, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_2076), u_mk_tuple(2, &l_u_is_digit_2077, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_2078), u_mk_tuple(2, &l_u_is_letter_2079, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_2080), u_mk_tuple(2, &l_u_is_space_2081, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))), f_map_46_empty_0());
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__2082 = U_LIT("&");
  static struct u_object l_n__2083 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__2082, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__2083, f_compile_46_mangle_1(v_n))));
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
  u_value s2084 = v_n;
  u_value t2085 = NULL;
  if ((!t2085) && (u_is_con(s2084, &n_syntax_46_Name) && u_is_con(u_field(s2084, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s2084, 0), 0);
    u_value v_b = u_field(s2084, 1);
    static struct u_object l_const_32_struct_32_u_n_2086 = U_LIT("const struct u_name");
    static struct u_object l_n__2087 = U_LIT("n_");
    t2085 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2086), u_concat(&l_n__2087, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t2085) && (u_is_con(s2084, &n_syntax_46_Name) && u_is_con(u_field(s2084, 0), &n_option_46_None))) {
    u_value v_b = u_field(s2084, 1);
    static struct u_object l_const_32_struct_32_u_n_2088 = U_LIT("const struct u_name");
    static struct u_object l_n__2089 = U_LIT("n_");
    static struct u_object l_NULL_2090 = U_LIT("NULL");
    t2085 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2088), u_concat(&l_n__2089, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2090), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t2085) {
    u_no_case(s2084);
  }
  return t2085;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s2091 = v_cs;
  u_value t2092 = NULL;
  if ((!t2092) && u_is_con(s2091, &n_list_46_Nil)) {
    t2092 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2092) && (u_is_con(s2091, &n_list_46_Cons) && u_is_con(u_field(s2091, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s2091, 0), 2);
    u_value v_rest = u_field(s2091, 1);
    u_value t2093 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2094 = &t2093;
    u_value l2095 = v_ks;
    while (u_is_con(l2095, &n_list_46_Cons)) {
      u_value v_k = u_field(l2095, 0);
      if (u_truth(u_mk_bool(1))) {
        e2094 = u_append(e2094, f_syntax_46_Constr_46_name_1(v_k));
      }
      l2095 = u_field(l2095, 1);
    }
    t2092 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t2093, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t2092) && u_is_con(s2091, &n_list_46_Cons)) {
    u_value v_rest = u_field(s2091, 1);
    t2092 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t2092) {
    u_no_case(s2091);
  }
  return t2092;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s2096 = v_ks;
  u_value t2097 = NULL;
  if ((!t2097) && u_is_con(s2096, &n_list_46_Nil)) {
    t2097 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2097) && u_is_con(s2096, &n_list_46_Cons)) {
    u_value v_k = u_field(s2096, 0);
    u_value v_rest = u_field(s2096, 1);
    u_value t2098;
    if (u_truth(f_set_46_member_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t2098 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t2098 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t2097 = t2098;
  }
  if (!t2097) {
    u_no_case(s2096);
  }
  return t2097;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t2099 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2100 = &t2099;
  u_value l2101 = v_xs;
  while (u_is_con(l2101, &n_list_46_Cons)) {
    u_value v_x = u_field(l2101, 0);
    if (u_truth(u_mk_bool(1))) {
      e2100 = u_append(e2100, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l2101 = u_field(l2101, 1);
  }
  return t2099;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__2102 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__2102, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s2103 = f_list_46_lookup_2(v_x, v_env);
  u_value t2104 = NULL;
  if ((!t2104) && u_is_con(s2103, &n_option_46_Some)) {
    u_value v_c = u_field(s2103, 0);
    t2104 = v_c;
  }
  if (!t2104) {
    t2104 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t2104) {
    u_no_case(s2103);
  }
  return t2104;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s2105 = v_c;
  u_value t2106 = NULL;
  if ((!t2106) && (u_is_con(s2105, &n_c_46_CeUnary) && u_equals(u_field(s2105, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s2105, 1);
    t2106 = v_p;
  }
  if (!t2106) {
    static struct u_object l__38__2107 = U_LIT("&");
    t2106 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2107, v_c);
  }
  if (!t2106) {
    u_no_case(s2105);
  }
  return t2106;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s2108 = v_t;
  u_value t2109 = NULL;
  if ((!t2109) && (u_is_con(s2108, &n_syntax_46_TSort) && u_is_con(u_field(s2108, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s2108, 0);
    t2109 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t2109) && u_is_con(s2108, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s2108, 0);
    u_value v_args = u_field(s2108, 1);
    static struct u_object l__91__2110 = U_LIT("[");
    static struct u_object l__44__32__2111 = U_LIT(", ");
    u_value t2112 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2113 = &t2112;
    u_value l2114 = v_args;
    while (u_is_con(l2114, &n_list_46_Cons)) {
      u_value v_a = u_field(l2114, 0);
      if (u_truth(u_mk_bool(1))) {
        e2113 = u_append(e2113, f_compile_46_type_key_1(v_a));
      }
      l2114 = u_field(l2114, 1);
    }
    static struct u_object l__93__2115 = U_LIT("]");
    t2109 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__2110, u_concat(u_str_join(&l__44__32__2111, t2112), &l__93__2115)));
  }
  if ((!t2109) && u_is_con(s2108, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s2108, 0);
    u_value v_r = u_field(s2108, 1);
    static struct u_object l__40__2116 = U_LIT("(");
    static struct u_object l__44__32__2117 = U_LIT(", ");
    u_value t2118 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2119 = &t2118;
    u_value l2120 = v_args;
    while (u_is_con(l2120, &n_list_46_Cons)) {
      u_value v_a = u_field(l2120, 0);
      if (u_truth(u_mk_bool(1))) {
        e2119 = u_append(e2119, f_compile_46_type_key_1(v_a));
      }
      l2120 = u_field(l2120, 1);
    }
    static struct u_object l__41__32__45__62__32__2121 = U_LIT(") -> ");
    t2109 = u_concat(&l__40__2116, u_concat(u_str_join(&l__44__32__2117, t2118), u_concat(&l__41__32__45__62__32__2121, f_compile_46_type_key_1(v_r))));
  }
  if ((!t2109) && u_is_con(s2108, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s2108, 0);
    u_value v_os = u_field(s2108, 1);
    u_value v_rs = u_field(s2108, 2);
    static struct u_object l__40_in_32__2122 = U_LIT("(in ");
    static struct u_object l__44__32__2123 = U_LIT(", ");
    u_value t2124 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2125 = &t2124;
    u_value l2126 = v_is;
    while (u_is_con(l2126, &n_list_46_Cons)) {
      u_value v_a = u_field(l2126, 0);
      if (u_truth(u_mk_bool(1))) {
        e2125 = u_append(e2125, f_compile_46_type_key_1(v_a));
      }
      l2126 = u_field(l2126, 1);
    }
    static struct u_object l__44__32_out_32__2127 = U_LIT(", out ");
    static struct u_object l__44__32__2128 = U_LIT(", ");
    u_value t2129 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2130 = &t2129;
    u_value l2131 = v_os;
    while (u_is_con(l2131, &n_list_46_Cons)) {
      u_value v_a = u_field(l2131, 0);
      if (u_truth(u_mk_bool(1))) {
        e2130 = u_append(e2130, f_compile_46_type_key_1(v_a));
      }
      l2131 = u_field(l2131, 1);
    }
    static struct u_object l__44__32_ref_32__2132 = U_LIT(", ref ");
    static struct u_object l__44__32__2133 = U_LIT(", ");
    u_value t2134 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2135 = &t2134;
    u_value l2136 = v_rs;
    while (u_is_con(l2136, &n_list_46_Cons)) {
      u_value v_a = u_field(l2136, 0);
      if (u_truth(u_mk_bool(1))) {
        e2135 = u_append(e2135, f_compile_46_type_key_1(v_a));
      }
      l2136 = u_field(l2136, 1);
    }
    static struct u_object l__41__2137 = U_LIT(")");
    t2109 = u_concat(&l__40_in_32__2122, u_concat(u_str_join(&l__44__32__2123, t2124), u_concat(&l__44__32_out_32__2127, u_concat(u_str_join(&l__44__32__2128, t2129), u_concat(&l__44__32_ref_32__2132, u_concat(u_str_join(&l__44__32__2133, t2134), &l__41__2137))))));
  }
  if ((!t2109) && u_is_con(s2108, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s2108, 0);
    static struct u_object l__40__2138 = U_LIT("(");
    static struct u_object l__32__42__32__2139 = U_LIT(" * ");
    u_value t2140 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2141 = &t2140;
    u_value l2142 = v_ts;
    while (u_is_con(l2142, &n_list_46_Cons)) {
      u_value v_a = u_field(l2142, 0);
      if (u_truth(u_mk_bool(1))) {
        e2141 = u_append(e2141, f_compile_46_type_key_1(v_a));
      }
      l2142 = u_field(l2142, 1);
    }
    static struct u_object l__41__2143 = U_LIT(")");
    t2109 = u_concat(&l__40__2138, u_concat(u_str_join(&l__32__42__32__2139, t2140), &l__41__2143));
  }
  if (!t2109) {
    static struct u_object l__63__2144 = U_LIT("?");
    t2109 = &l__63__2144;
  }
  if (!t2109) {
    u_no_case(s2108);
  }
  return t2109;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t2145 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2146 = &t2145;
  u_value l2147 = v_cs;
  while (u_is_con(l2147, &n_list_46_Cons)) {
    u_value v_c = u_field(l2147, 0);
    if (u_truth(u_mk_bool(1))) {
      e2146 = u_append(e2146, f_compile_46_fun_arities_1(v_c));
    }
    l2147 = u_field(l2147, 1);
  }
  u_value t2148 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2149 = &t2148;
  u_value l2150 = v_cs;
  while (u_is_con(l2150, &n_list_46_Cons)) {
    u_value v_c = u_field(l2150, 0);
    if (u_truth(u_mk_bool(1))) {
      e2149 = u_append(e2149, f_compile_46_constr_arities_1(v_c));
    }
    l2150 = u_field(l2150, 1);
  }
  u_value t2155 = f_list_46_concat_1(t2145);
  u_value t2156 = f_list_46_concat_1(t2148);
  u_value t2154;
  {
    u_value v_fs = t2155;
    u_value v_ks = t2156;
    u_value t2151 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2152 = &t2151;
    u_value l2153 = v_cs;
    while (u_is_con(l2153, &n_list_46_Cons)) {
      u_value v_c = u_field(l2153, 0);
      if (u_truth(u_mk_bool(1))) {
        e2152 = u_append(e2152, f_compile_46_proc_arities_1(v_c));
      }
      l2153 = u_field(l2153, 1);
    }
    t2154 = u_mk_con(&n_compile_46_Globals, 5, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()), f_map_46_put_all_2(f_list_46_concat_1(t2151), f_map_46_empty_0()));
  }
  return t2154;
}

u_value f_compile_46_proc_arities_1(u_value v_c) {
  u_value s2157 = v_c;
  u_value t2158 = NULL;
  if ((!t2158) && u_is_con(s2157, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s2157, 0);
    u_value v_ins = u_field(s2157, 2);
    u_value v_outs = u_field(s2157, 3);
    u_value v_refs = u_field(s2157, 4);
    t2158 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, f_list_46_length_1(v_ins), u_add(f_list_46_length_1(v_outs), f_list_46_length_1(v_refs)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2158) {
    t2158 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2158) {
    u_no_case(s2157);
  }
  return t2158;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s2159 = v_fs;
  u_value t2160 = NULL;
  if ((!t2160) && u_is_con(s2159, &n_list_46_Nil)) {
    t2160 = v_m;
  }
  if ((!t2160) && u_is_con(s2159, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s2159, 0), 0);
    u_value v_k = u_field(u_field(s2159, 0), 1);
    u_value v_rest = u_field(s2159, 1);
    u_value s2161 = f_map_46_get_2(v_f, v_m);
    u_value t2162 = NULL;
    if ((!t2162) && u_is_con(s2161, &n_option_46_Some)) {
      u_value v_ks = u_field(s2161, 0);
      t2162 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t2162) && u_is_con(s2161, &n_option_46_None)) {
      t2162 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t2162) {
      u_no_case(s2161);
    }
    t2160 = t2162;
  }
  if (!t2160) {
    u_no_case(s2159);
  }
  return t2160;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s2163 = v_gl;
  u_value t2164 = NULL;
  if ((!t2164) && u_is_con(s2163, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s2163, 2);
    u_value s2165 = f_map_46_get_2(v_n, v_fm);
    u_value t2166 = NULL;
    if ((!t2166) && u_is_con(s2165, &n_option_46_Some)) {
      u_value v_ks = u_field(s2165, 0);
      t2166 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t2166) && u_is_con(s2165, &n_option_46_None)) {
      t2166 = u_mk_bool(0);
    }
    if (!t2166) {
      u_no_case(s2165);
    }
    t2164 = t2166;
  }
  if (!t2164) {
    u_no_case(s2163);
  }
  return t2164;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s2167 = v_gl;
  u_value t2168 = NULL;
  if ((!t2168) && u_is_con(s2167, &n_compile_46_Globals)) {
    u_value v_km = u_field(s2167, 3);
    t2168 = f_map_46_get_2(v_n, v_km);
  }
  if (!t2168) {
    u_no_case(s2167);
  }
  return t2168;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s2169 = v_c;
  u_value t2170 = NULL;
  if ((!t2170) && u_is_con(s2169, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s2169, 0);
    u_value v_xss = u_field(s2169, 2);
    t2170 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2170) {
    t2170 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2170) {
    u_no_case(s2169);
  }
  return t2170;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s2171 = v_c;
  u_value t2172 = NULL;
  if ((!t2172) && u_is_con(s2171, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s2171, 2);
    u_value t2173 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2174 = &t2173;
    u_value l2175 = v_ks;
    while (u_is_con(l2175, &n_list_46_Cons)) {
      u_value v_k = u_field(l2175, 0);
      if (u_truth(u_mk_bool(1))) {
        e2174 = u_append(e2174, f_compile_46_constr_arity_1(v_k));
      }
      l2175 = u_field(l2175, 1);
    }
    t2172 = t2173;
  }
  if (!t2172) {
    t2172 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2172) {
    u_no_case(s2171);
  }
  return t2172;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s2176 = v_k;
  u_value t2177 = NULL;
  if ((!t2177) && u_is_con(s2176, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s2176, 0);
    u_value v_ts = u_field(s2176, 1);
    t2177 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t2177) {
    u_no_case(s2176);
  }
  return t2177;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s2178 = v_xs;
  u_value t2179 = NULL;
  if ((!t2179) && u_is_con(s2178, &n_list_46_Nil)) {
    t2179 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2179) && u_is_con(s2178, &n_list_46_Cons)) {
    u_value v_x = u_field(s2178, 0);
    u_value v_rest = u_field(s2178, 1);
    t2179 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t2179) {
    u_no_case(s2178);
  }
  return t2179;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_2180 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_2180, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_2181 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_2181, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s2182 = v_cs;
  u_value t2183 = NULL;
  if ((!t2183) && u_is_con(s2182, &n_list_46_Nil)) {
    t2183 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t2183) && (u_is_con(s2182, &n_list_46_Cons) && u_is_con(u_field(s2182, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s2182, 0);
    t2183 = v_d;
  }
  if ((!t2183) && u_is_con(s2182, &n_list_46_Cons)) {
    u_value v_d = u_field(s2182, 0);
    u_value v_rest = u_field(s2182, 1);
    static struct u_object l__38__38__2184 = U_LIT("&&");
    t2183 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__2184, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t2183) {
    u_no_case(s2182);
  }
  return t2183;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s2185 = v_l;
  u_value t2186 = NULL;
  if ((!t2186) && u_is_con(s2185, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_2187 = U_LIT("u_mk_tuple");
    t2186 = f_c_46_c_call_2(&l_u_mk_tuple_2187, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2186) && u_is_con(s2185, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s2185, 0);
    static struct u_object l_u_mk_bool_2188 = U_LIT("u_mk_bool");
    u_value t2189;
    if (u_truth(v_b)) {
      t2189 = u_mk_int(1);
    } else {
      t2189 = u_mk_int(0);
    }
    t2186 = f_c_46_c_call_2(&l_u_mk_bool_2188, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t2189), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2186) && u_is_con(s2185, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s2185, 0);
    u_value t2190 = u_lt(u_mk_int_str("-1000000000000000000"), v_n);
    if (u_truth(t2190)) {
      t2190 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t2193;
    if (u_truth(t2190)) {
      static struct u_object l_u_mk_int_2191 = U_LIT("u_mk_int");
      t2193 = f_c_46_c_call_2(&l_u_mk_int_2191, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_2192 = U_LIT("u_mk_int_str");
      t2193 = f_c_46_c_call_2(&l_u_mk_int_str_2192, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t2186 = t2193;
  }
  if ((!t2186) && u_is_con(s2185, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s2185, 0);
    static struct u_object l_u_mk_lit_2194 = U_LIT("u_mk_lit");
    t2186 = f_c_46_c_call_2(&l_u_mk_lit_2194, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2186) && u_is_con(s2185, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s2185, 0);
    static struct u_object l_u_mk_char_2195 = U_LIT("u_mk_char");
    t2186 = f_c_46_c_call_2(&l_u_mk_char_2195, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2186) {
    u_no_case(s2185);
  }
  return t2186;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s2196 = v_p;
  u_value t2197 = NULL;
  if ((!t2197) && u_is_con(s2196, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s2196, 0);
    static struct u_object l_u_equals_2198 = U_LIT("u_equals");
    t2197 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_2198, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2197) && u_is_con(s2196, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s2196, 0);
    u_value v_ps = u_field(s2196, 1);
    static struct u_object l_u_is_con_2199 = U_LIT("u_is_con");
    t2197 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_2199, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t2197) && u_is_con(s2196, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2196, 0);
    t2197 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2197) {
    t2197 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2197) {
    u_no_case(s2196);
  }
  return t2197;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s2200 = v_ps;
  u_value t2201 = NULL;
  if ((!t2201) && u_is_con(s2200, &n_list_46_Nil)) {
    t2201 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2201) && u_is_con(s2200, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2200, 0), 0);
    u_value v_i = u_field(u_field(s2200, 0), 1);
    u_value v_rest = u_field(s2200, 1);
    t2201 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t2201) {
    u_no_case(s2200);
  }
  return t2201;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s2202 = v_p;
  u_value t2203 = NULL;
  if ((!t2203) && (u_is_con(s2202, &n_syntax_46_PVar) && u_is_con(u_field(s2202, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2202, 0), 0);
    t2203 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2203) && u_is_con(s2202, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2202, 1);
    t2203 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t2203) && u_is_con(s2202, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2202, 0);
    t2203 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2203) {
    t2203 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2203) {
    u_no_case(s2202);
  }
  return t2203;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s2204 = v_ps;
  u_value t2205 = NULL;
  if ((!t2205) && u_is_con(s2204, &n_list_46_Nil)) {
    t2205 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2205) && u_is_con(s2204, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2204, 0), 0);
    u_value v_i = u_field(u_field(s2204, 0), 1);
    u_value v_rest = u_field(s2204, 1);
    t2205 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t2205) {
    u_no_case(s2204);
  }
  return t2205;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s2206 = v_p;
  u_value t2207 = NULL;
  if ((!t2207) && (u_is_con(s2206, &n_syntax_46_PVar) && u_is_con(u_field(s2206, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2206, 0), 0);
    t2207 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2207) && u_is_con(s2206, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2206, 1);
    u_value t2208 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2209 = &t2208;
    u_value l2210 = v_ps;
    while (u_is_con(l2210, &n_list_46_Cons)) {
      u_value v_q = u_field(l2210, 0);
      if (u_truth(u_mk_bool(1))) {
        e2209 = u_append(e2209, f_compile_46_pattern_names_1(v_q));
      }
      l2210 = u_field(l2210, 1);
    }
    t2207 = f_list_46_concat_1(t2208);
  }
  if ((!t2207) && u_is_con(s2206, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2206, 0);
    u_value t2211 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2212 = &t2211;
    u_value l2213 = v_ps;
    while (u_is_con(l2213, &n_list_46_Cons)) {
      u_value v_q = u_field(l2213, 0);
      if (u_truth(u_mk_bool(1))) {
        e2212 = u_append(e2212, f_compile_46_pattern_names_1(v_q));
      }
      l2213 = u_field(l2213, 1);
    }
    t2207 = f_list_46_concat_1(t2211);
  }
  if (!t2207) {
    t2207 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2207) {
    u_no_case(s2206);
  }
  return t2207;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s2214 = v_e;
  u_value t2215 = NULL;
  if ((!t2215) && u_is_con(s2214, &n_syntax_46_ELit)) {
    t2215 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2215) && (u_is_con(s2214, &n_syntax_46_EVar) && u_is_con(u_field(s2214, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2214, 0), 0);
    u_value t2216;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t2216 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t2216 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t2215 = t2216;
  }
  if ((!t2215) && u_is_con(s2214, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s2214, 0);
    u_value v_args = u_field(s2214, 1);
    u_value t2217 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2218 = &t2217;
    u_value l2219 = v_args;
    while (u_is_con(l2219, &n_list_46_Cons)) {
      u_value v_a = u_field(l2219, 0);
      if (u_truth(u_mk_bool(1))) {
        e2218 = u_append(e2218, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2219 = u_field(l2219, 1);
    }
    t2215 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t2217));
  }
  if ((!t2215) && u_is_con(s2214, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s2214, 0);
    u_value v_b = u_field(s2214, 1);
    t2215 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t2215) && u_is_con(s2214, &n_syntax_46_EProc)) {
    u_value v_is = u_field(s2214, 0);
    u_value v_os = u_field(s2214, 1);
    u_value v_rs = u_field(s2214, 2);
    u_value v_b = u_field(s2214, 3);
    t2215 = f_compile_46_free_in_stmt_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), v_bound));
  }
  if ((!t2215) && u_is_con(s2214, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s2214, 0);
    u_value v_b = u_field(s2214, 1);
    u_value t2220 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2221 = &t2220;
    u_value l2222 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l2222, &n_list_46_Cons)) {
      u_value v_a = u_field(l2222, 0);
      if (u_truth(u_mk_bool(1))) {
        e2221 = u_append(e2221, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2222 = u_field(l2222, 1);
    }
    t2215 = f_list_46__43__43__2(f_list_46_concat_1(t2220), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t2215) && u_is_con(s2214, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s2214, 0);
    u_value v_l = u_field(s2214, 1);
    u_value v_r = u_field(s2214, 2);
    t2215 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t2215) && u_is_con(s2214, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s2214, 0);
    u_value v_cs = u_field(s2214, 1);
    u_value t2223 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2224 = &t2223;
    u_value l2225 = v_cs;
    while (u_is_con(l2225, &n_list_46_Cons)) {
      u_value v_b = u_field(l2225, 0);
      if (u_truth(u_mk_bool(1))) {
        e2224 = u_append(e2224, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l2225 = u_field(l2225, 1);
    }
    t2215 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2223));
  }
  if ((!t2215) && u_is_con(s2214, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s2214, 0);
    u_value v_xs = u_field(s2214, 1);
    u_value v_ls = u_field(s2214, 2);
    u_value v_c = u_field(s2214, 3);
    u_value t2226 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2227 = &t2226;
    u_value l2228 = v_ls;
    while (u_is_con(l2228, &n_list_46_Cons)) {
      u_value v_l = u_field(l2228, 0);
      if (u_truth(u_mk_bool(1))) {
        e2227 = u_append(e2227, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2228 = u_field(l2228, 1);
    }
    t2215 = f_list_46__43__43__2(f_list_46_concat_1(t2226), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t2215) && u_is_con(s2214, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s2214, 0);
    u_value t2229 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2230 = &t2229;
    u_value l2231 = v_es;
    while (u_is_con(l2231, &n_list_46_Cons)) {
      u_value v_a = u_field(l2231, 0);
      if (u_truth(u_mk_bool(1))) {
        e2230 = u_append(e2230, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2231 = u_field(l2231, 1);
    }
    t2215 = f_list_46_concat_1(t2229);
  }
  if ((!t2215) && u_is_con(s2214, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s2214, 0);
    t2215 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if (!t2215) {
    u_no_case(s2214);
  }
  return t2215;
}

u_value f_compile_46_free_in_stmt_2(u_value v_x, u_value v_bound) {
  u_value s2232 = v_x;
  u_value t2233 = NULL;
  if ((!t2233) && u_is_con(s2232, &n_syntax_46_SSkip)) {
    t2233 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2233) && u_is_con(s2232, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s2232, 0);
    u_value t2234 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2235 = &t2234;
    u_value l2236 = f_syntax_46_names_1(f_list_46_firsts_1(v_ps));
    while (u_is_con(l2236, &n_list_46_Cons)) {
      u_value v_y = u_field(l2236, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2235 = u_append(e2235, v_y);
      }
      l2236 = u_field(l2236, 1);
    }
    u_value t2237 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2238 = &t2237;
    u_value l2239 = f_list_46_seconds_1(v_ps);
    while (u_is_con(l2239, &n_list_46_Cons)) {
      u_value v_a = u_field(l2239, 0);
      if (u_truth(u_mk_bool(1))) {
        e2238 = u_append(e2238, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2239 = u_field(l2239, 1);
    }
    t2233 = f_list_46__43__43__2(t2234, f_list_46_concat_1(t2237));
  }
  if ((!t2233) && u_is_con(s2232, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s2232, 0);
    u_value v_ys = u_field(s2232, 1);
    u_value t2240 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2241 = &t2240;
    u_value l2242 = v_ys;
    while (u_is_con(l2242, &n_list_46_Cons)) {
      u_value v_y = u_field(l2242, 0);
      if (u_truth(u_mk_bool(1))) {
        e2241 = u_append(e2241, f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2242 = u_field(l2242, 1);
    }
    t2233 = f_list_46_concat_1(t2240);
  }
  if ((!t2233) && u_is_con(s2232, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s2232, 0);
    u_value v_l = u_field(s2232, 1);
    u_value v_r = u_field(s2232, 2);
    t2233 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_l, v_bound), f_compile_46_free_in_stmt_2(v_r, v_bound)));
  }
  if ((!t2233) && u_is_con(s2232, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s2232, 0);
    u_value v_b = u_field(s2232, 1);
    t2233 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_compile_46_free_in_stmt_2(v_b, v_bound));
  }
  if ((!t2233) && u_is_con(s2232, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s2232, 0);
    u_value v_ins = u_field(s2232, 1);
    u_value v_outs = u_field(s2232, 2);
    u_value v_refs = u_field(s2232, 3);
    u_value t2243 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2244 = &t2243;
    u_value l2245 = u_mk_con(&n_list_46_Cons, 2, v_p, f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
    while (u_is_con(l2245, &n_list_46_Cons)) {
      u_value v_y = u_field(l2245, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2244 = u_append(e2244, v_y);
      }
      l2245 = u_field(l2245, 1);
    }
    u_value t2246 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2247 = &t2246;
    u_value l2248 = v_ins;
    while (u_is_con(l2248, &n_list_46_Cons)) {
      u_value v_a = u_field(l2248, 0);
      if (u_truth(u_mk_bool(1))) {
        e2247 = u_append(e2247, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2248 = u_field(l2248, 1);
    }
    t2233 = f_list_46__43__43__2(t2243, f_list_46_concat_1(t2246));
  }
  if ((!t2233) && u_is_con(s2232, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s2232, 0);
    u_value v_bs = u_field(s2232, 1);
    u_value t2249 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2250 = &t2249;
    u_value l2251 = v_bs;
    while (u_is_con(l2251, &n_list_46_Cons)) {
      u_value v_b = u_field(l2251, 0);
      if (u_truth(u_mk_bool(1))) {
        e2250 = u_append(e2250, f_compile_46_free_in_stmt_branch_2(v_b, v_bound));
      }
      l2251 = u_field(l2251, 1);
    }
    t2233 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2249));
  }
  if ((!t2233) && u_is_con(s2232, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s2232, 0);
    t2233 = f_compile_46_free_vars_2(v_c, v_bound);
  }
  if ((!t2233) && u_is_con(s2232, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s2232, 0);
    t2233 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if ((!t2233) && u_is_con(s2232, &n_syntax_46_STry)) {
    u_value v_b = u_field(s2232, 0);
    u_value v_hs = u_field(s2232, 1);
    u_value t2252 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2253 = &t2252;
    u_value l2254 = v_hs;
    while (u_is_con(l2254, &n_list_46_Cons)) {
      u_value v_h = u_field(l2254, 0);
      if (u_truth(u_mk_bool(1))) {
        e2253 = u_append(e2253, f_compile_46_free_in_handler_2(v_h, v_bound));
      }
      l2254 = u_field(l2254, 1);
    }
    t2233 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_b, v_bound), f_list_46_concat_1(t2252));
  }
  if (!t2233) {
    u_no_case(s2232);
  }
  return t2233;
}

u_value f_compile_46_free_in_stmt_branch_2(u_value v_b, u_value v_bound) {
  u_value s2255 = v_b;
  u_value t2256 = NULL;
  if ((!t2256) && (u_is_con(s2255, &n_syntax_46_Branch) && u_is_con(u_field(s2255, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2255, 0);
    u_value v_g = u_field(u_field(s2255, 1), 0);
    u_value v_y = u_field(s2255, 2);
    t2256 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2256) && (u_is_con(s2255, &n_syntax_46_Branch) && u_is_con(u_field(s2255, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2255, 0);
    u_value v_y = u_field(s2255, 2);
    t2256 = f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2256) {
    u_no_case(s2255);
  }
  return t2256;
}

u_value f_compile_46_free_in_handler_2(u_value v_h, u_value v_bound) {
  u_value t2260 = f_list_46__43__43__2(f_compile_46_pattern_names_1(f_syntax_46_Handler_46_pat_1(v_h)), v_bound);
  u_value t2259;
  {
    u_value v_inner = t2260;
    u_value s2257 = f_syntax_46_Handler_46_guard_1(v_h);
    u_value t2258 = NULL;
    if ((!t2258) && u_is_con(s2257, &n_option_46_Some)) {
      u_value v_g = u_field(s2257, 0);
      t2258 = f_compile_46_free_vars_2(v_g, v_inner);
    }
    if ((!t2258) && u_is_con(s2257, &n_option_46_None)) {
      t2258 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t2258) {
      u_no_case(s2257);
    }
    t2259 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(f_syntax_46_Handler_46_body_1(v_h), v_inner), t2258);
  }
  return t2259;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s2261 = v_b;
  u_value t2262 = NULL;
  if ((!t2262) && (u_is_con(s2261, &n_syntax_46_Branch) && u_is_con(u_field(s2261, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2261, 0);
    u_value v_g = u_field(u_field(s2261, 1), 0);
    u_value v_e = u_field(s2261, 2);
    t2262 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2262) && (u_is_con(s2261, &n_syntax_46_Branch) && u_is_con(u_field(s2261, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2261, 0);
    u_value v_e = u_field(s2261, 2);
    t2262 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2262) {
    u_no_case(s2261);
  }
  return t2262;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s2263 = v_xss;
  u_value t2264 = NULL;
  if ((!t2264) && u_is_con(s2263, &n_list_46_Nil)) {
    t2264 = v_b;
  }
  if ((!t2264) && u_is_con(s2263, &n_list_46_Cons)) {
    u_value v_xs = u_field(s2263, 0);
    u_value v_rest = u_field(s2263, 1);
    t2264 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t2264) {
    u_no_case(s2263);
  }
  return t2264;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value t2265 = u_concat(v_stem, u_str_show(f_compile_46_Gen_46_count_1(*r_g)));
    u_value t2266 = f_compile_46_Gen_46_count_58__61__2(*r_g, u_add(f_compile_46_Gen_46_count_1(*r_g), u_mk_int(1)));
    *r_x = t2265;
    *r_g = t2266;
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value t2267 = f_compile_46_Gen_46_lifted_58__61__2(*r_g, f_list_46__43__43__2(f_compile_46_Gen_46_lifted_1(*r_g), u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
    *r_g = t2267;
  }
}

u_value f_compile_46_global_value_3(u_value v_gl, u_value v_n, u_value v_t) {
  u_value t2282;
  if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
    static struct u_object l_u_mk_closure_2268 = U_LIT("u_mk_closure");
    static struct u_object l_w__2269 = U_LIT("w_");
    t2282 = f_c_46_c_call_2(&l_u_mk_closure_2268, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__2269, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
  } else {
    u_value s2270 = f_compile_46_fields_count_2(v_gl, v_n);
    u_value t2271 = NULL;
    if ((!t2271) && (u_is_con(s2270, &n_option_46_Some) && u_equals(u_field(s2270, 0), u_mk_int(0)))) {
      static struct u_object l_u_mk_con_2272 = U_LIT("u_mk_con");
      t2271 = f_c_46_c_call_2(&l_u_mk_con_2272, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
    }
    if ((!t2271) && u_is_con(s2270, &n_option_46_Some)) {
      static struct u_object l_u_mk_closure_2273 = U_LIT("u_mk_closure");
      static struct u_object l_wc__2274 = U_LIT("wc_");
      t2271 = f_c_46_c_call_2(&l_u_mk_closure_2273, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__2274, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
    }
    if ((!t2271) && u_is_con(s2270, &n_option_46_None)) {
      u_value s2275 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
      u_value t2276 = NULL;
      if ((!t2276) && u_is_con(s2275, &n_option_46_Some)) {
        u_value v_f = u_field(u_field(s2275, 0), 0);
        static struct u_object l_u_mk_closure_2277 = U_LIT("u_mk_closure");
        static struct u_object l_w_2278 = U_LIT("w");
        t2276 = f_c_46_c_call_2(&l_u_mk_closure_2277, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_2278, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
      }
      if ((!t2276) && u_is_con(s2275, &n_option_46_None)) {
        if (u_truth(f_map_46_has_2(v_n, f_compile_46_Globals_46_procs_1(v_gl)))) {
          static struct u_object l_u_mk_proc_2279 = U_LIT("u_mk_proc");
          static struct u_object l_wp__2280 = U_LIT("wp_");
          t2276 = f_c_46_c_call_2(&l_u_mk_proc_2279, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wp__2280, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        }
      }
      if ((!t2276) && u_is_con(s2275, &n_option_46_None)) {
        static struct u_object l_the_32_variable_32__2281 = U_LIT("the variable ");
        u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__2281, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
        t2276 = NULL;
      }
      if (!t2276) {
        u_no_case(s2275);
      }
      t2271 = t2276;
    }
    if (!t2271) {
      u_no_case(s2270);
    }
    t2282 = t2271;
  }
  return t2282;
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
    u_value s2283 = v_e;
    int d2284 = 0;
    if ((!d2284) && (u_is_con(s2283, &n_syntax_46_ELit) && u_is_con(u_field(s2283, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s2283, 0), 0);
      d2284 = 1;
      {
        static struct u_object l_l__2285 = U_LIT("l_");
        u_value t2286;
        if (u_truth(u_lt(u_str_size(v_s), u_mk_int(16)))) {
          t2286 = u_str_size(v_s);
        } else {
          t2286 = u_mk_int(16);
        }
        static struct u_object l___2287 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__2285, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), t2286))), &l___2287)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__2288 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_2289 = U_LIT("U_LIT");
        static struct u_object l__38__2290 = U_LIT("&");
        u_value t2291 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__2288), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_2289, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t2292 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2290, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t2291;
        *r_r = t2292;
      }
    }
    if ((!d2284) && u_is_con(s2283, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s2283, 0);
      d2284 = 1;
      {
        u_value t2293 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2294 = f_compile_46_literal_value_1(v_l);
        *r_ss = t2293;
        *r_r = t2294;
      }
    }
    if ((!d2284) && (u_is_con(s2283, &n_syntax_46_EVar) && u_is_con(u_field(s2283, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s2283, 0), 0);
      u_value v_ty = u_field(u_field(s2283, 0), 1);
      d2284 = 1;
      {
        u_value s2295 = f_list_46_lookup_2(v_n, v_env);
        int d2296 = 0;
        if ((!d2296) && u_is_con(s2295, &n_option_46_Some)) {
          u_value v_c = u_field(s2295, 0);
          d2296 = 1;
          {
            u_value t2297 = u_mk_con(&n_list_46_Nil, 0);
            u_value t2298 = v_c;
            *r_ss = t2297;
            *r_r = t2298;
          }
        }
        if ((!d2296) && u_is_con(s2295, &n_option_46_None)) {
          d2296 = 1;
          {
            u_value t2299 = u_mk_con(&n_list_46_Nil, 0);
            u_value t2300 = f_compile_46_global_value_3(v_gl, v_n, v_ty);
            *r_ss = t2299;
            *r_r = t2300;
          }
        }
        if (!d2296) {
          u_no_case(s2295);
        }
      }
    }
    if ((!d2284) && (u_is_con(s2283, &n_syntax_46_EApp) && (u_is_con(u_field(s2283, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s2283, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s2283, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s2283, 0), 0), 1);
      u_value v_args = u_field(s2283, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d2284 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d2284) && u_is_con(s2283, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s2283, 0);
      u_value v_args = u_field(s2283, 1);
      d2284 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_2301 = U_LIT("u_apply");
        u_value t2302 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2303 = f_c_46_c_call_2(&l_u_apply_2301, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t2302;
        *r_r = t2303;
      }
    }
    if ((!d2284) && u_is_con(s2283, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s2283, 0);
      u_value v_b = u_field(s2283, 1);
      d2284 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2284) && u_is_con(s2283, &n_syntax_46_EProc)) {
      u_value v_is = u_field(s2283, 0);
      u_value v_os = u_field(s2283, 1);
      u_value v_rs = u_field(s2283, 2);
      u_value v_b = u_field(s2283, 3);
      d2284 = 1;
      {
        p_compile_46_compile_proc_lambda(v_gl, v_env, v_is, f_list_46__43__43__2(v_os, v_rs), v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2284) && u_is_con(s2283, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s2283, 0);
      u_value v_b = u_field(s2283, 1);
      d2284 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2304 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2304, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2305 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t2305;
        u_value t2306 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2306;
      }
    }
    if ((!d2284) && u_is_con(s2283, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s2283, 0);
      u_value v_l = u_field(s2283, 1);
      u_value v_rr = u_field(s2283, 2);
      d2284 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2307 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2307, &v_t, r_g);
        u_value t2308 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2308;
        u_value t2309 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2309;
      }
    }
    if ((!d2284) && u_is_con(s2283, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s2283, 0);
      u_value v_cases = u_field(s2283, 1);
      d2284 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2310 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2310, &v_d, r_g);
        static struct u_object l_t_2311 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2311, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_2312 = U_LIT("NULL");
        u_value t2313 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2312))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2313;
        static struct u_object l__33__2314 = U_LIT("!");
        static struct u_object l_u_no_case_2315 = U_LIT("u_no_case");
        u_value t2316 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2314, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2315, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2316;
        u_value t2317 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2317;
      }
    }
    if ((!d2284) && u_is_con(s2283, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s2283, 0);
      u_value v_xs = u_field(s2283, 1);
      u_value v_lists = u_field(s2283, 2);
      u_value v_c = u_field(s2283, 3);
      d2284 = 1;
      {
        static struct u_object l_t_2318 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2318, &v_t, r_g);
        static struct u_object l_e_2319 = U_LIT("e");
        p_compile_46_gen_name(&l_e_2319, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_2320 = U_LIT("u_mk_con");
        static struct u_object l__38__2321 = U_LIT("&");
        u_value t2322 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_2320, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__2321, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t2322;
        u_value t2323 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2323;
      }
    }
    if ((!d2284) && u_is_con(s2283, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s2283, 0);
      d2284 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_2324 = U_LIT("u_mk_tuple");
        u_value t2325 = f_c_46_c_call_2(&l_u_mk_tuple_2324, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t2325;
      }
    }
    if ((!d2284) && u_is_con(s2283, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s2283, 0);
      u_value v_ty = u_field(s2283, 1);
      d2284 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2326 = U_LIT("u_raise");
        static struct u_object l_NULL_2327 = U_LIT("NULL");
        u_value t2328 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2326, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        u_value t2329 = u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2327);
        *r_ss = t2328;
        *r_r = t2329;
      }
    }
    if (!d2284) {
      u_no_case(s2283);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s2330 = v_es;
    int d2331 = 0;
    if ((!d2331) && u_is_con(s2330, &n_list_46_Nil)) {
      d2331 = 1;
      {
        u_value t2332 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2333 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2332;
        *r_rs = t2333;
      }
    }
    if ((!d2331) && u_is_con(s2330, &n_list_46_Cons)) {
      u_value v_e = u_field(s2330, 0);
      u_value v_rest = u_field(s2330, 1);
      d2331 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t2334 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2335 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t2334;
        *r_rs = t2335;
      }
    }
    if (!d2331) {
      u_no_case(s2330);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s2336 = v_rs;
    int d2337 = 0;
    if ((!d2337) && u_is_con(s2336, &n_list_46_Nil)) {
      d2337 = 1;
      {
        u_value t2338 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2338;
      }
    }
    if ((!d2337) && u_is_con(s2336, &n_list_46_Cons)) {
      u_value v_r = u_field(s2336, 0);
      u_value v_rest = u_field(s2336, 1);
      d2337 = 1;
      {
        static struct u_object l_t_2339 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2339, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t2340 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t2340;
      }
    }
    if (!d2337) {
      u_no_case(s2336);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s2341 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2342 = NULL;
  if ((!t2342) && (u_is_con(u_field(s2341, 0), &n_list_46_Cons) && (u_is_con(u_field(s2341, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2341, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2341, 0), 0);
    u_value v_more = u_field(u_field(s2341, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2341, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2341, 1), 1);
    t2342 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t2342) {
    t2342 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2342) {
    u_no_case(s2341);
  }
  return t2342;
}

void p_compile_46_compile_named(u_value v_gl, u_value v_env, u_value v_n, u_value v_ty, u_value v_args, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value v_ra = NULL;
    u_value v_rb = NULL;
    u_value v_rs = NULL;
    u_value v_t = NULL;
    u_value s2343 = v_args;
    int d2344 = 0;
    if ((!d2344) && (u_is_con(s2343, &n_list_46_Cons) && (u_is_con(u_field(s2343, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2343, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s2343, 0);
      u_value v_b = u_field(u_field(s2343, 1), 0);
      static struct u_object l_and_2351 = U_LIT("and");
      u_value t2353 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2351));
      if (!u_truth(t2353)) {
        static struct u_object l_or_2352 = U_LIT("or");
        t2353 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_2352));
      }
      if (u_truth(t2353)) {
        d2344 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_2345 = U_LIT("t");
          p_compile_46_gen_name(&l_t_2345, &v_t, r_g);
          static struct u_object l_and_2346 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2346)))) {
            {
              u_value t2347 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2347;
            }
          } else {
            {
              static struct u_object l__33__2348 = U_LIT("!");
              u_value t2349 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2348, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2349;
            }
          }
          u_value t2350 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t2350;
        }
      }
    }
    if (!d2344) {
      d2344 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t2354 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t2354)) {
          t2354 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t2354)) {
          {
            u_value t2355 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t2355;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_2356 = U_LIT("u_mk_con");
              u_value t2357 = f_c_46_c_call_2(&l_u_mk_con_2356, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t2357;
            }
          } else {
            {
              u_value s2358 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
              int d2359 = 0;
              if ((!d2359) && u_is_con(s2358, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s2358, 0), 0);
                u_value v_k = u_field(u_field(s2358, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d2359 = 1;
                  {
                    u_value t2360 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t2360;
                  }
                }
              }
              if (!d2359) {
                d2359 = 1;
                {
                  static struct u_object l_u_apply_2361 = U_LIT("u_apply");
                  u_value t2362 = f_c_46_c_call_2(&l_u_apply_2361, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_global_value_3(v_gl, v_n, v_ty), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t2362;
                }
              }
              if (!d2359) {
                u_no_case(s2358);
              }
            }
          }
        }
      }
    }
    if (!d2344) {
      u_no_case(s2343);
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
    u_value t2363 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2364 = &t2363;
    u_value l2365 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l2365, &n_list_46_Cons)) {
      u_value v_x = u_field(l2365, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2364 = u_append(e2364, v_x);
      }
      l2365 = u_field(l2365, 1);
    }
    u_value t2366 = t2363;
    v_caps = t2366;
    static struct u_object l_lam_2367 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_2367, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_2368 = U_LIT("env");
    static struct u_object l_args_2369 = U_LIT("args");
    u_value t2370 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2368), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_2369));
    v_decls = t2370;
    static struct u_object l_env_2371 = U_LIT("env");
    static struct u_object l_args_2372 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2371), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2372), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_2373 = U_LIT("u_mk_closure");
    u_value t2374 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2375 = &t2374;
    u_value l2376 = v_caps;
    while (u_is_con(l2376, &n_list_46_Cons)) {
      u_value v_x = u_field(l2376, 0);
      if (u_truth(u_mk_bool(1))) {
        e2375 = u_append(e2375, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2376 = u_field(l2376, 1);
    }
    u_value t2377 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2378 = f_c_46_c_call_2(&l_u_mk_closure_2373, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2374));
    *r_ss = t2377;
    *r_r = t2378;
  }
}

void p_compile_46_compile_proc_lambda(u_value v_gl, u_value v_env, u_value v_ins, u_value v_outs, u_value v_b, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_caps = NULL;
    u_value v_f = NULL;
    u_value v_bs = NULL;
    u_value v_decls = NULL;
    u_value t2379 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2380 = &t2379;
    u_value l2381 = f_list_46_distinct_1(f_compile_46_free_in_stmt_2(v_b, f_syntax_46_names_1(f_list_46__43__43__2(v_ins, v_outs))));
    while (u_is_con(l2381, &n_list_46_Cons)) {
      u_value v_x = u_field(l2381, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2380 = u_append(e2380, v_x);
      }
      l2381 = u_field(l2381, 1);
    }
    u_value t2382 = t2379;
    v_caps = t2382;
    static struct u_object l_proc_2383 = U_LIT("proc");
    p_compile_46_gen_name(&l_proc_2383, &v_f, r_g);
    u_value t2384 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2385 = &t2384;
    u_value l2386 = f_syntax_46_names_1(v_outs);
    while (u_is_con(l2386, &n_list_46_Cons)) {
      u_value v_x = u_field(l2386, 0);
      if (u_truth(u_mk_bool(1))) {
        e2385 = u_append(e2385, f_compile_46_pointer_1(v_x));
      }
      l2386 = u_field(l2386, 1);
    }
    p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_ins))), t2384), v_b, &v_bs, r_g);
    static struct u_object l_env_2387 = U_LIT("env");
    static struct u_object l_ins_2388 = U_LIT("ins");
    u_value t2389 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2387), f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_ins), u_mk_int(0)), &l_ins_2388), f_compile_46_unpack_pointers_1(f_compile_46_numbered_2(f_syntax_46_names_1(v_outs), u_mk_int(0)))));
    v_decls = t2389;
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), v_f, f_compile_46_proc_params_0(), f_list_46__43__43__2(v_decls, v_bs)), r_g);
    static struct u_object l_u_mk_proc_2390 = U_LIT("u_mk_proc");
    u_value t2391 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2392 = &t2391;
    u_value l2393 = v_caps;
    while (u_is_con(l2393, &n_list_46_Cons)) {
      u_value v_x = u_field(l2393, 0);
      if (u_truth(u_mk_bool(1))) {
        e2392 = u_append(e2392, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2393 = u_field(l2393, 1);
    }
    u_value t2394 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2395 = f_c_46_c_call_2(&l_u_mk_proc_2390, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2391));
    *r_ss = t2394;
    *r_r = t2395;
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
  static struct u_object l_env_2396 = U_LIT("env");
  static struct u_object l_ins_2397 = U_LIT("ins");
  static struct u_object l_outs_2398 = U_LIT("outs");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2396), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_ins_2397), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, f_compile_46_value_ptr_0()), &l_outs_2398), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_compile_46_unpack_pointers_1(u_value v_xs) {
  u_value s2399 = v_xs;
  u_value t2400 = NULL;
  if ((!t2400) && u_is_con(s2399, &n_list_46_Nil)) {
    t2400 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2400) && u_is_con(s2399, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2399, 0), 0);
    u_value v_i = u_field(u_field(s2399, 0), 1);
    u_value v_rest = u_field(s2399, 1);
    static struct u_object l_outs_2401 = U_LIT("outs");
    t2400 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(&l_outs_2401, v_i))), f_compile_46_unpack_pointers_1(v_rest));
  }
  if (!t2400) {
    u_no_case(s2399);
  }
  return t2400;
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s2402 = v_xs;
  u_value t2403 = NULL;
  if ((!t2403) && u_is_con(s2402, &n_list_46_Nil)) {
    t2403 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2403) && u_is_con(s2402, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2402, 0), 0);
    u_value v_i = u_field(u_field(s2402, 0), 1);
    u_value v_rest = u_field(s2402, 1);
    t2403 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t2403) {
    u_no_case(s2402);
  }
  return t2403;
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
    u_value s2404 = v_cases;
    int d2405 = 0;
    if ((!d2405) && u_is_con(s2404, &n_list_46_Nil)) {
      d2405 = 1;
      {
        u_value t2406 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2406;
      }
    }
    if ((!d2405) && (u_is_con(s2404, &n_list_46_Cons) && u_is_con(u_field(s2404, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2404, 0), 0);
      u_value v_guard = u_field(u_field(s2404, 0), 1);
      u_value v_e = u_field(u_field(s2404, 0), 2);
      u_value v_more = u_field(s2404, 1);
      d2405 = 1;
      {
        u_value t2407 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2407;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2408 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t2408;
        u_value s2409 = v_guard;
        int d2410 = 0;
        if ((!d2410) && u_is_con(s2409, &n_option_46_Some)) {
          u_value v_c = u_field(s2409, 0);
          d2410 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2411 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2411;
          }
        }
        if ((!d2410) && u_is_con(s2409, &n_option_46_None)) {
          d2410 = 1;
          {
          }
        }
        if (!d2410) {
          u_no_case(s2409);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2412 = U_LIT("!");
        u_value t2413 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2412, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2413;
      }
    }
    if (!d2405) {
      u_no_case(s2404);
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
    u_value s2414 = u_mk_tuple(2, v_xs, v_lists);
    int d2415 = 0;
    if ((!d2415) && (u_is_con(u_field(s2414, 0), &n_list_46_Cons) && u_is_con(u_field(s2414, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2414, 0), 0);
      u_value v_more = u_field(u_field(s2414, 0), 1);
      u_value v_list = u_field(u_field(s2414, 1), 0);
      u_value v_others = u_field(u_field(s2414, 1), 1);
      d2415 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2416 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2416, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2417 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2417;
        static struct u_object l_u_is_con_2418 = U_LIT("u_is_con");
        u_value t2419 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2418, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2419;
      }
    }
    if (!d2415) {
      d2415 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2420 = U_LIT("u_append");
        u_value t2421 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2420, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2421;
      }
    }
    if (!d2415) {
      u_no_case(s2414);
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
    u_value s2422 = v_s;
    int d2423 = 0;
    if ((!d2423) && u_is_con(s2422, &n_syntax_46_SSkip)) {
      d2423 = 1;
      {
        u_value t2424 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2424;
      }
    }
    if ((!d2423) && u_is_con(s2422, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2422, 0);
      d2423 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2425 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2425;
      }
    }
    if ((!d2423) && u_is_con(s2422, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2422, 0);
      u_value v_body = u_field(s2422, 1);
      d2423 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t2426 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2427 = &t2426;
        u_value l2428 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2428, &n_list_46_Cons)) {
          u_value v_x = u_field(l2428, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2429 = U_LIT("NULL");
            e2427 = u_append(e2427, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2429))));
          }
          l2428 = u_field(l2428, 1);
        }
        u_value t2430 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2426, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2430;
      }
    }
    if ((!d2423) && u_is_con(s2422, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2422, 0);
      u_value v_l = u_field(s2422, 1);
      u_value v_r = u_field(s2422, 2);
      d2423 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2431 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2431;
      }
    }
    if ((!d2423) && u_is_con(s2422, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2422, 0);
      u_value v_b = u_field(s2422, 1);
      d2423 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2432 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2432, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2433 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2433;
      }
    }
    if ((!d2423) && u_is_con(s2422, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2422, 0);
      u_value v_ins = u_field(s2422, 1);
      u_value v_outs = u_field(s2422, 2);
      u_value v_refs = u_field(s2422, 3);
      d2423 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        if (u_truth(f_list_46_has_key_2(v_p, v_env))) {
          {
            static struct u_object l_u_call_proc_2434 = U_LIT("u_call_proc");
            u_value t2435 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e2436 = &t2435;
            u_value l2437 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
            while (u_is_con(l2437, &n_list_46_Cons)) {
              u_value v_x = u_field(l2437, 0);
              if (u_truth(u_mk_bool(1))) {
                e2436 = u_append(e2436, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
              }
              l2437 = u_field(l2437, 1);
            }
            u_value t2438 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_call_proc_2434, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_var_expr_2(v_env, v_p), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_ins)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(f_list_46__43__43__2(v_outs, v_refs))), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(v_rs, t2435)))), u_mk_con(&n_list_46_Nil, 0)));
            *r_ss = t2438;
          }
        } else {
          {
            u_value t2439 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e2440 = &t2439;
            u_value l2441 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
            while (u_is_con(l2441, &n_list_46_Cons)) {
              u_value v_x = u_field(l2441, 0);
              if (u_truth(u_mk_bool(1))) {
                e2440 = u_append(e2440, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
              }
              l2441 = u_field(l2441, 1);
            }
            u_value t2442 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2439))), u_mk_con(&n_list_46_Nil, 0)));
            *r_ss = t2442;
          }
        }
      }
    }
    if ((!d2423) && u_is_con(s2422, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2422, 0);
      u_value v_cases = u_field(s2422, 1);
      d2423 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2443 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2443, &v_d, r_g);
        static struct u_object l_d_2444 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2444, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2445 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2445;
        static struct u_object l__33__2446 = U_LIT("!");
        static struct u_object l_u_no_case_2447 = U_LIT("u_no_case");
        u_value t2448 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2446, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2447, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2448;
      }
    }
    if ((!d2423) && (u_is_con(s2422, &n_syntax_46_SAssert) && (u_is_con(u_field(s2422, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2422, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2422, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2422, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2422, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2422, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2422, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2422, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2422, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2422, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2422, 0), 1), 1), 0);
      d2423 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2449 = U_LIT("u_assert_equal");
        u_value t2450 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2449, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2450;
      }
    }
    if ((!d2423) && u_is_con(s2422, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2422, 0);
      d2423 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2451 = U_LIT("u_assert_true");
        u_value t2452 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2451, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2452;
      }
    }
    if ((!d2423) && u_is_con(s2422, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2422, 0);
      u_value v_ty = u_field(s2422, 1);
      d2423 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2453 = U_LIT("u_raise");
        u_value t2454 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2453, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2454;
      }
    }
    if ((!d2423) && u_is_con(s2422, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2422, 0);
      u_value v_hs = u_field(s2422, 1);
      d2423 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2455 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2455, &v_d, r_g);
        static struct u_object l_d_2456 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2456, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2457 = U_LIT("u_pop");
        static struct u_object l__33__2458 = U_LIT("!");
        static struct u_object l_u_reraise_2459 = U_LIT("u_reraise");
        u_value t2460 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2457, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2458, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2459, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2460;
        static struct u_object l_struct_32_u_handler_2461 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2462 = U_LIT("u_push");
        static struct u_object l__61__61__2463 = U_LIT("==");
        static struct u_object l_setjmp_2464 = U_LIT("setjmp");
        static struct u_object l_env_2465 = U_LIT("env");
        static struct u_object l_u_pop_2466 = U_LIT("u_pop");
        u_value t2467 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2461)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2462, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2463, f_c_46_c_call_2(&l_setjmp_2464, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2465), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2466, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2467;
      }
    }
    if (!d2423) {
      u_no_case(s2422);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2468 = v_xs;
    int d2469 = 0;
    if ((!d2469) && u_is_con(s2468, &n_list_46_Nil)) {
      d2469 = 1;
      {
        u_value t2470 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2470;
      }
    }
    if ((!d2469) && u_is_con(s2468, &n_list_46_Cons)) {
      u_value v_x = u_field(s2468, 0);
      u_value v_rest = u_field(s2468, 1);
      d2469 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2471 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2471;
      }
    }
    if (!d2469) {
      u_no_case(s2468);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2472 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2473 = NULL;
  if ((!t2473) && (u_is_con(u_field(s2472, 0), &n_list_46_Cons) && (u_is_con(u_field(s2472, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2472, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2472, 0), 0);
    u_value v_more = u_field(u_field(s2472, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2472, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2472, 1), 1);
    t2473 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2473) {
    t2473 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2473) {
    u_no_case(s2472);
  }
  return t2473;
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
    static struct u_object l_u_caught_2474 = U_LIT("u_caught");
    u_value t2475 = f_c_46_c_call_2(&l_u_caught_2474, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2475;
    u_value s2476 = v_hs;
    int d2477 = 0;
    if ((!d2477) && u_is_con(s2476, &n_list_46_Nil)) {
      d2477 = 1;
      {
        u_value t2478 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2478;
      }
    }
    if ((!d2477) && (u_is_con(s2476, &n_list_46_Cons) && u_is_con(u_field(s2476, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2476, 0), 0);
      u_value v_p = u_field(u_field(s2476, 0), 1);
      u_value v_guard = u_field(u_field(s2476, 0), 2);
      u_value v_s = u_field(u_field(s2476, 0), 3);
      u_value v_more = u_field(s2476, 1);
      d2477 = 1;
      {
        u_value t2479 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2479;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2480 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2480;
        u_value s2481 = v_guard;
        int d2482 = 0;
        if ((!d2482) && u_is_con(s2481, &n_option_46_Some)) {
          u_value v_c = u_field(s2481, 0);
          d2482 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2483 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2483;
          }
        }
        if ((!d2482) && u_is_con(s2481, &n_option_46_None)) {
          d2482 = 1;
          {
          }
        }
        if (!d2482) {
          u_no_case(s2481);
        }
        u_value s2484 = v_p;
        int d2485 = 0;
        if ((!d2485) && u_is_con(s2484, &n_syntax_46_PWild)) {
          d2485 = 1;
          {
            u_value t2486 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2486;
          }
        }
        if (!d2485) {
          d2485 = 1;
          {
            static struct u_object l_u_has_type_2487 = U_LIT("u_has_type");
            u_value t2488 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2487, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2488;
          }
        }
        if (!d2485) {
          u_no_case(s2484);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2489 = U_LIT("!");
        u_value t2490 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2489, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2490;
      }
    }
    if (!d2477) {
      u_no_case(s2476);
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
    u_value s2491 = v_cases;
    int d2492 = 0;
    if ((!d2492) && u_is_con(s2491, &n_list_46_Nil)) {
      d2492 = 1;
      {
        u_value t2493 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2493;
      }
    }
    if ((!d2492) && (u_is_con(s2491, &n_list_46_Cons) && u_is_con(u_field(s2491, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2491, 0), 0);
      u_value v_guard = u_field(u_field(s2491, 0), 1);
      u_value v_s = u_field(u_field(s2491, 0), 2);
      u_value v_more = u_field(s2491, 1);
      d2492 = 1;
      {
        u_value t2494 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2494;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2495 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2495;
        u_value s2496 = v_guard;
        int d2497 = 0;
        if ((!d2497) && u_is_con(s2496, &n_option_46_Some)) {
          u_value v_c = u_field(s2496, 0);
          d2497 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2498 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2498;
          }
        }
        if ((!d2497) && u_is_con(s2496, &n_option_46_None)) {
          d2497 = 1;
          {
          }
        }
        if (!d2497) {
          u_no_case(s2496);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2499 = U_LIT("!");
        u_value t2500 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2499, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2500;
      }
    }
    if (!d2492) {
      u_no_case(s2491);
    }
  }
}

void p_compile_46_compile_cmd(u_value v_gl, u_value v_entry, u_value v_c, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_bs = NULL;
    u_value v_rb = NULL;
    u_value v_params = NULL;
    u_value s2501 = v_c;
    int d2502 = 0;
    if ((!d2502) && u_is_con(s2501, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2501, 0);
      u_value v_xss = u_field(s2501, 2);
      u_value v_b = u_field(s2501, 4);
      d2502 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2503 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2504 = &t2503;
        u_value l2505 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2505, &n_list_46_Cons)) {
          u_value v_x = u_field(l2505, 0);
          if (u_truth(u_mk_bool(1))) {
            e2504 = u_append(e2504, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2505 = u_field(l2505, 1);
        }
        u_value t2506 = t2503;
        v_params = t2506;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2507 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2507;
          }
        } else {
          {
            u_value t2508 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2508;
          }
        }
        u_value t2509 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2510 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2509;
        *r_ms = t2510;
      }
    }
    if ((!d2502) && u_is_con(s2501, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2501, 0);
      u_value v_ins = u_field(s2501, 2);
      u_value v_outs = u_field(s2501, 3);
      u_value v_refs = u_field(s2501, 4);
      u_value v_body = u_field(s2501, 5);
      d2502 = 1;
      {
        u_value t2511 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2512 = &t2511;
        u_value l2513 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2513, &n_list_46_Cons)) {
          u_value v_x = u_field(l2513, 0);
          if (u_truth(u_mk_bool(1))) {
            e2512 = u_append(e2512, f_compile_46_pointer_1(v_x));
          }
          l2513 = u_field(l2513, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2511), v_body, &v_bs, r_g);
        u_value t2514 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2515 = &t2514;
        u_value l2516 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2516, &n_list_46_Cons)) {
          u_value v_x = u_field(l2516, 0);
          if (u_truth(u_mk_bool(1))) {
            e2515 = u_append(e2515, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2516 = u_field(l2516, 1);
        }
        u_value t2517 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2518 = &t2517;
        u_value l2519 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2519, &n_list_46_Cons)) {
          u_value v_x = u_field(l2519, 0);
          if (u_truth(u_mk_bool(1))) {
            e2518 = u_append(e2518, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2519 = u_field(l2519, 1);
        }
        u_value t2520 = f_list_46__43__43__2(t2514, t2517);
        v_params = t2520;
        u_value t2521 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2522 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2521;
        *r_ms = t2522;
      }
    }
    if ((!d2502) && u_is_con(s2501, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s2501, 0);
      u_value v_body = u_field(s2501, 1);
      if (u_truth(u_eq(v_n, v_entry))) {
        d2502 = 1;
        {
          p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
          u_value t2523 = u_mk_con(&n_list_46_Nil, 0);
          u_value t2524 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
          *r_ds = t2523;
          *r_ms = t2524;
        }
      }
    }
    if ((!d2502) && u_is_con(s2501, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s2501, 0);
      u_value v_body = u_field(s2501, 1);
      static struct u_object l_test_2528 = U_LIT("test");
      if (u_truth(u_eq(v_entry, &l_test_2528))) {
        d2502 = 1;
        {
          p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
          static struct u_object l_u_passed_2525 = U_LIT("u_passed");
          u_value t2526 = u_mk_con(&n_list_46_Nil, 0);
          u_value t2527 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_passed_2525, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
          *r_ds = t2526;
          *r_ms = t2527;
        }
      }
    }
    if (!d2502) {
      d2502 = 1;
      {
        u_value t2529 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2530 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2529;
        *r_ms = t2530;
      }
    }
    if (!d2502) {
      u_no_case(s2501);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_entry, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2531 = v_cs;
    int d2532 = 0;
    if ((!d2532) && u_is_con(s2531, &n_list_46_Nil)) {
      d2532 = 1;
      {
        u_value t2533 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2534 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2533;
        *r_ms = t2534;
      }
    }
    if ((!d2532) && u_is_con(s2531, &n_list_46_Cons)) {
      u_value v_c = u_field(s2531, 0);
      u_value v_rest = u_field(s2531, 1);
      d2532 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_entry, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_entry, v_rest, &v_d2, &v_m2, r_g);
        u_value t2535 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2536 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2535;
        *r_ms = t2536;
      }
    }
    if (!d2532) {
      u_no_case(s2531);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_handler_2537 = U_LIT("handler");
  u_value t2564 = u_mk_con(&n_c_46_CeVar, 1, &l_handler_2537);
  u_value t2563;
  {
    u_value v_h = t2564;
    static struct u_object l_static_32_u_value_2538 = U_LIT("static u_value");
    static struct u_object l_cache_2539 = U_LIT("cache");
    static struct u_object l_NULL_2540 = U_LIT("NULL");
    static struct u_object l__61__61__2541 = U_LIT("==");
    static struct u_object l_cache_2542 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2543 = U_LIT("U_COMPUTING");
    static struct u_object l_u_fail_2544 = U_LIT("u_fail");
    static struct u_object l_recursive_32_initia_2545 = U_LIT("recursive initialization of ");
    static struct u_object l_cache_2546 = U_LIT("cache");
    static struct u_object l_cache_2547 = U_LIT("cache");
    static struct u_object l_cache_2548 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2549 = U_LIT("U_COMPUTING");
    static struct u_object l_struct_32_u_handler_2550 = U_LIT("struct u_handler");
    static struct u_object l_handler_2551 = U_LIT("handler");
    static struct u_object l_u_push_2552 = U_LIT("u_push");
    static struct u_object l__33__61__2553 = U_LIT("!=");
    static struct u_object l_setjmp_2554 = U_LIT("setjmp");
    static struct u_object l_env_2555 = U_LIT("env");
    static struct u_object l_u_pop_2556 = U_LIT("u_pop");
    static struct u_object l_cache_2557 = U_LIT("cache");
    static struct u_object l_NULL_2558 = U_LIT("NULL");
    static struct u_object l_u_reraise_2559 = U_LIT("u_reraise");
    static struct u_object l_u_pop_2560 = U_LIT("u_pop");
    static struct u_object l_cache_2561 = U_LIT("cache");
    static struct u_object l_cache_2562 = U_LIT("cache");
    t2563 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2538), &l_cache_2539, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2540))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2541, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2542), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2543)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2544, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2545, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2546), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2547))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2548), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2549)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2550)), &l_handler_2551, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2552, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__33__61__2553, f_c_46_c_call_2(&l_setjmp_2554, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, v_h, &l_env_2555), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2556, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2557), u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2558)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2559, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2560, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2561), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2562))), u_mk_con(&n_list_46_Nil, 0))))));
  }
  return t2563;
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2565 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2566 = &t2565;
  u_value l2567 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l2567, &n_list_46_Cons)) {
    u_value v_p = u_field(l2567, 0);
    if (u_truth(u_mk_bool(1))) {
      e2566 = u_append(e2566, f_compile_46_fun_wrapper_1(v_p));
    }
    l2567 = u_field(l2567, 1);
  }
  u_value t2568 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2569 = &t2568;
  u_value l2570 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l2570, &n_list_46_Cons)) {
    u_value v_p = u_field(l2570, 0);
    if (u_truth(u_mk_bool(1))) {
      e2569 = u_append(e2569, f_compile_46_constr_wrapper_1(v_p));
    }
    l2570 = u_field(l2570, 1);
  }
  u_value t2571 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2572 = &t2571;
  u_value l2573 = f_list_46_seconds_1(f_map_46_entries_1(f_compile_46_builtin_functions_0()));
  while (u_is_con(l2573, &n_list_46_Cons)) {
    u_value v_p = u_field(l2573, 0);
    if (u_truth(u_mk_bool(1))) {
      e2572 = u_append(e2572, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2573 = u_field(l2573, 1);
  }
  u_value t2574 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2575 = &t2574;
  u_value l2576 = f_map_46_entries_1(f_compile_46_Globals_46_procs_1(v_gl));
  while (u_is_con(l2576, &n_list_46_Cons)) {
    u_value v_p = u_field(l2576, 0);
    if (u_truth(u_mk_bool(1))) {
      e2575 = u_append(e2575, f_compile_46_proc_wrapper_1(v_p));
    }
    l2576 = u_field(l2576, 1);
  }
  return f_list_46__43__43__2(t2565, f_list_46__43__43__2(f_list_46_concat_1(t2568), f_list_46__43__43__2(t2571, t2574)));
}

u_value f_compile_46_proc_wrapper_1(u_value v_p) {
  u_value s2577 = v_p;
  u_value t2578 = NULL;
  if (!t2578) {
    u_value v_n = u_field(s2577, 0);
    u_value v_k = u_field(u_field(s2577, 1), 0);
    u_value v_m = u_field(u_field(s2577, 1), 1);
    static struct u_object l_wp__2579 = U_LIT("wp_");
    u_value t2580 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2581 = &t2580;
    u_value l2582 = f_compile_46_range_to_1(v_k);
    while (u_is_con(l2582, &n_list_46_Cons)) {
      u_value v_i = u_field(l2582, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_ins_2583 = U_LIT("ins");
        e2581 = u_append(e2581, f_compile_46_index_2(&l_ins_2583, v_i));
      }
      l2582 = u_field(l2582, 1);
    }
    u_value t2584 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2585 = &t2584;
    u_value l2586 = f_compile_46_range_to_1(v_m);
    while (u_is_con(l2586, &n_list_46_Cons)) {
      u_value v_i = u_field(l2586, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_outs_2587 = U_LIT("outs");
        e2585 = u_append(e2585, f_compile_46_index_2(&l_outs_2587, v_i));
      }
      l2586 = u_field(l2586, 1);
    }
    t2578 = u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), u_concat(&l_wp__2579, f_compile_46_mangle_1(v_n)), f_compile_46_proc_params_0(), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_n), f_list_46__43__43__2(t2580, t2584))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2578) {
    u_no_case(s2577);
  }
  return t2578;
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2588 = v_p;
  u_value t2589 = NULL;
  if (!t2589) {
    u_value v_n = u_field(s2588, 0);
    u_value v_k = u_field(s2588, 1);
    static struct u_object l_w__2590 = U_LIT("w_");
    t2589 = f_compile_46_wrapper_3(u_concat(&l_w__2590, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2589) {
    u_no_case(s2588);
  }
  return t2589;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2591 = v_p;
  u_value t2592 = NULL;
  if (!t2592) {
    u_value v_f = u_field(s2591, 0);
    u_value v_k = u_field(s2591, 1);
    static struct u_object l_w_2593 = U_LIT("w");
    t2592 = f_compile_46_wrapper_3(u_concat(&l_w_2593, v_f), v_f, v_k);
  }
  if (!t2592) {
    u_no_case(s2591);
  }
  return t2592;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2594 = U_LIT("env");
  static struct u_object l_args_2595 = U_LIT("args");
  u_value t2596 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2597 = &t2596;
  u_value l2598 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2598, &n_list_46_Cons)) {
    u_value v_i = u_field(l2598, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2599 = U_LIT("args");
      e2597 = u_append(e2597, f_compile_46_index_2(&l_args_2599, v_i));
    }
    l2598 = u_field(l2598, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2594), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2595), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2596))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2600 = v_p;
  u_value t2601 = NULL;
  if ((!t2601) && u_equals(u_field(s2600, 1), u_mk_int(0))) {
    t2601 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2601) {
    u_value v_n = u_field(s2600, 0);
    u_value v_k = u_field(s2600, 1);
    static struct u_object l_wc__2602 = U_LIT("wc_");
    static struct u_object l_env_2603 = U_LIT("env");
    static struct u_object l_args_2604 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2605 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2606 = U_LIT("args");
    t2601 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2602, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2603), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2604), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2605, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2606), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2601) {
    u_no_case(s2600);
  }
  return t2601;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2607;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2607 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2607 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2607;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2608 = v_d;
  u_value t2609 = NULL;
  if ((!t2609) && u_is_con(s2608, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2608, 0);
    u_value v_f = u_field(s2608, 1);
    u_value v_ps = u_field(s2608, 2);
    t2609 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2609) {
    t2609 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2609) {
    u_no_case(s2608);
  }
  return t2609;
}

void p_compile_46_compile_program(u_value v_q, u_value v_entry, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2610 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2610;
    u_value t2611 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t2611;
    static struct u_object l_test_2612 = U_LIT("test");
    u_value t2614 = f_prelude_46__33__61__2(v_entry, &l_test_2612);
    if (u_truth(t2614)) {
      t2614 = f_prelude_46_not_1(f_list_46_exists_2(f_syntax_46_Program_46_cmds_1(v_q), u_mk_closure(lam2613, 1, v_entry)));
    }
    if (u_truth(t2614)) {
      {
        u_raise(u_mk_con(&n_compile_46_UnknownEntry, 1, v_entry), "compile.CompileError");
      }
    }
    p_compile_46_compile_cmds(v_gl, v_entry, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value t2615 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, f_compile_46_Gen_46_lifted_1(v_g)));
    v_fs = t2615;
    static struct u_object l_ulang_46_h_2616 = U_LIT("ulang.h");
    u_value t2617 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2618 = &t2617;
    u_value l2619 = v_fs;
    while (u_is_con(l2619, &n_list_46_Cons)) {
      u_value v_d = u_field(l2619, 0);
      if (u_truth(u_mk_bool(1))) {
        e2618 = u_append(e2618, f_compile_46_prototype_1(v_d));
      }
      l2619 = u_field(l2619, 1);
    }
    static struct u_object l_main_2620 = U_LIT("main");
    static struct u_object l_argc_2621 = U_LIT("argc");
    static struct u_object l_char_2622 = U_LIT("char");
    static struct u_object l_argv_2623 = U_LIT("argv");
    static struct u_object l_u_args_2624 = U_LIT("u_args");
    static struct u_object l_argc_2625 = U_LIT("argc");
    static struct u_object l_argv_2626 = U_LIT("argv");
    u_value t2627 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2616), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2617), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2620, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2621), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2622))), &l_argv_2623), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2624, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2625), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2626), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2627;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_entry, u_value v_target) {
  {
    u_value v_cache = NULL;
    u_value t2628 = u_mk_con(&n_list_46_Nil, 0);
    v_cache = t2628;
    p_compile_46_compile_with(v_source, v_entry, v_target, &v_cache);
  }
}

u_value f_compile_46_is_run_2(u_value v_c, u_value v_entry) {
  u_value s2629 = v_c;
  u_value t2630 = NULL;
  if ((!t2630) && u_is_con(s2629, &n_syntax_46_CRun)) {
    u_value v_n = u_field(s2629, 0);
    t2630 = u_eq(v_n, v_entry);
  }
  if (!t2630) {
    t2630 = u_mk_bool(0);
  }
  if (!t2630) {
    u_no_case(s2629);
  }
  return t2630;
}

void p_compile_46_compile_with(u_value v_source, u_value v_entry, u_value v_target, u_value *r_cache) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2631 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2631, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2632 = U_LIT("prelude");
    p_check_46_check_modules(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2632, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q, r_cache);
    p_compile_46_compile_program(v_q, v_entry, &v_f);
    p_os_write(v_target, f_c_46_c_file_1(v_f));
  }
}

u_value lam367(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam368(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam370(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam371(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam381(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_f = args[0];
  return f_ctx_46_is_fun_2(f_check_46_State_46_ctx_1(v_s), v_f);
}

u_value lam388(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam392(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_p = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_p);
}

u_value lam622(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_t = args[0];
  return f_ctx_46_is_sort_2(f_check_46_State_46_ctx_1(v_s), v_t);
}

u_value lam733(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam736(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam804(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_q = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_q);
}

u_value lam1135(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

u_value lam1826(u_value *env, u_value *args) {
  u_value v_t = env[0];
  u_value v_e = args[0];
  return f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t);
}

u_value lam2613(u_value *env, u_value *args) {
  u_value v_entry = env[0];
  u_value v_c = args[0];
  return f_compile_46_is_run_2(v_c, v_entry);
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  {
    {
      u_value v_args = NULL;
      p_os_args(&v_args);
      u_value s2633 = v_args;
      int d2634 = 0;
      if ((!d2634) && u_is_con(s2633, &n_list_46_Nil)) {
        d2634 = 1;
        {
        }
      }
      if ((!d2634) && (u_is_con(s2633, &n_list_46_Cons) && (u_equals(u_field(s2633, 0), u_mk_lit("compile")) && (u_is_con(u_field(s2633, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2633, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2633, 1), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2633, 1), 1), 1), 1), &n_list_46_Nil))))))) {
        u_value v_source = u_field(u_field(s2633, 1), 0);
        u_value v_entry = u_field(u_field(u_field(s2633, 1), 1), 0);
        u_value v_target = u_field(u_field(u_field(u_field(s2633, 1), 1), 1), 0);
        d2634 = 1;
        {
          p_compile_46_compile_file(v_source, v_entry, v_target);
        }
      }
      if (!d2634) {
        d2634 = 1;
        {
          static struct u_object l_usage_58__32_ulang_32_com_2635 = U_LIT("usage: ulang compile <file> <entry> <target.c>");
          u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_com_2635), "os.OsError");
        }
      }
      if (!d2634) {
        u_no_case(s2633);
      }
    }
  }
  return 0;
}
