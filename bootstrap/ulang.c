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

const struct u_name n_syntax_46_CTest = {"syntax", "CTest", 7};

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

u_value w_f_compile_46_is_entry_2(u_value *env, u_value *args);

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

void wp_ulang_46_main(u_value *env, u_value *ins, u_value **outs);

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

void p_compile_46_compile_program(u_value v_q, u_value v_m, u_value v_entry, u_value *r_f);

void p_compile_46_compile_file(u_value v_source, u_value v_entry, u_value v_target);

u_value f_compile_46_is_entry_2(u_value v_c, u_value v_n);

void p_compile_46_compile_with(u_value v_source, u_value v_entry, u_value v_target, u_value *r_cache);

void p_ulang_46_main(void);

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

u_value lam807(u_value *env, u_value *args);

u_value lam1137(u_value *env, u_value *args);

u_value lam1827(u_value *env, u_value *args);

u_value lam2610(u_value *env, u_value *args);

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

u_value w_f_compile_46_is_entry_2(u_value *env, u_value *args) {
  return f_compile_46_is_entry_2(args[0], args[1]);
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

void wp_ulang_46_main(u_value *env, u_value *ins, u_value **outs) {
  p_ulang_46_main();
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
  u_value t795;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_785 = U_LIT("os_args");
    static struct u_object l_os_read_786 = U_LIT("os_read");
    static struct u_object l_os_write_787 = U_LIT("os_write");
    static struct u_object l_os_show_788 = U_LIT("os_show");
    static struct u_object l_A_789 = U_LIT("A");
    static struct u_object l_A_790 = U_LIT("A");
    static struct u_object l_os_print_791 = U_LIT("os_print");
    static struct u_object l_os_print_error_792 = U_LIT("os_print_error");
    static struct u_object l_os_exit_793 = U_LIT("os_exit");
    static struct u_object l_os_library_794 = U_LIT("os_library");
    t795 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_785), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_786), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_787), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_show_788), u_mk_tuple(4, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_789), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_790), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_print_791), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_print_error_792), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_exit_793), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_794), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)))))))));
  } else {
    t795 = u_mk_con(&n_list_46_Nil, 0);
  }
  return t795;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t796 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e797 = &t796;
  static struct u_object l_Program_798 = U_LIT("Program");
  u_value l799 = f_ctx_46_names_of_2(v_c, &l_Program_798);
  while (u_is_con(l799, &n_list_46_Cons)) {
    u_value v_d = u_field(l799, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e797 = u_append(e797, v_d);
    }
    l799 = u_field(l799, 1);
  }
  u_value s800 = t796;
  u_value t801 = NULL;
  if ((!t801) && u_is_con(s800, &n_list_46_Cons)) {
    u_value v_d = u_field(s800, 0);
    static struct u_object l_read_802 = U_LIT("read");
    static struct u_object l_library_803 = U_LIT("library");
    t801 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_802), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_803), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t801) {
    t801 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t801) {
    u_no_case(s800);
  }
  return t801;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value s804 = f_list_46_lookup_2(v_p, v_env);
    int d805 = 0;
    if ((!d805) && u_is_con(s804, &n_option_46_Some)) {
      u_value v_sc = u_field(s804, 0);
      d805 = 1;
      {
        p_check_46_call_variable(v_env, v_assignable, v_p, v_sc, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d805) && u_is_con(s804, &n_option_46_None)) {
      d805 = 1;
      {
        p_check_46_call_declared(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if (!d805) {
      u_no_case(s804);
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
    u_value t806 = u_mk_con(&n_syntax_46_SCall, 4, v_p, v_ins1, v_outs1, v_refs1);
    *r_y = t806;
  }
}

void p_check_46_call_declared(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s808 = f_check_46_visible_names_3(*r_s, v_p, u_mk_closure(lam807, 1, *r_s));
    int d809 = 0;
    if ((!d809) && (u_is_con(s808, &n_list_46_Cons) && u_is_con(u_field(s808, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s808, 0);
      d809 = 1;
      {
        u_value t810 = v_q1;
        v_q = t810;
      }
    }
    if ((!d809) && u_is_con(s808, &n_list_46_Nil)) {
      d809 = 1;
      {
        u_value t811 = v_p;
        v_q = t811;
      }
    }
    if (!d809) {
      u_value v_qs = s808;
      d809 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d809) {
      u_no_case(s808);
    }
    u_value s812 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_State_46_ctx_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_State_46_ctx_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_State_46_ctx_1(*r_s)))));
    int d813 = 0;
    if ((!d813) && u_is_con(s812, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s812, 0), 0);
      u_value v_is = u_field(u_field(s812, 0), 1);
      u_value v_os = u_field(u_field(s812, 0), 2);
      u_value v_rs = u_field(u_field(s812, 0), 3);
      d813 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t814 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e815 = &t814;
        u_value l816 = v_is;
        while (u_is_con(l816, &n_list_46_Cons)) {
          u_value v_t = u_field(l816, 0);
          if (u_truth(u_mk_bool(1))) {
            e815 = u_append(e815, f_check_46_subst_2(v_t, v_m));
          }
          l816 = u_field(l816, 1);
        }
        p_check_46_checks(v_env, v_ins, t814, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t817 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e818 = &t817;
        u_value l819 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l819, &n_list_46_Cons)) {
          u_value v_t = u_field(l819, 0);
          if (u_truth(u_mk_bool(1))) {
            e818 = u_append(e818, f_check_46_subst_2(v_t, v_m));
          }
          l819 = u_field(l819, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t817, r_s);
        u_value t820 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t820;
      }
    }
    if ((!d813) && u_is_con(s812, &n_option_46_None)) {
      d813 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d813) {
      u_no_case(s812);
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
    u_value s821 = v_cases;
    int d822 = 0;
    if ((!d822) && u_is_con(s821, &n_list_46_Nil)) {
      d822 = 1;
      {
        u_value t823 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t823;
      }
    }
    if ((!d822) && (u_is_con(s821, &n_list_46_Cons) && u_is_con(u_field(s821, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s821, 0), 0);
      u_value v_g = u_field(u_field(s821, 0), 1);
      u_value v_b = u_field(u_field(s821, 0), 2);
      u_value v_more = u_field(s821, 1);
      d822 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t824 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t824;
      }
    }
    if (!d822) {
      u_no_case(s821);
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
    u_value s825 = v_hs;
    int d826 = 0;
    if ((!d826) && u_is_con(s825, &n_list_46_Nil)) {
      d826 = 1;
      {
        u_value t827 = u_mk_con(&n_list_46_Nil, 0);
        *r_hs1 = t827;
      }
    }
    if ((!d826) && (u_is_con(s825, &n_list_46_Cons) && u_is_con(u_field(s825, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s825, 0), 0);
      u_value v_p = u_field(u_field(s825, 0), 1);
      u_value v_g = u_field(u_field(s825, 0), 2);
      u_value v_b = u_field(u_field(s825, 0), 3);
      u_value v_more = u_field(s825, 1);
      d826 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t828 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t828;
      }
    }
    if (!d826) {
      u_no_case(s825);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t829 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t829)) {
      t829 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t830 = t829;
    if (!u_truth(t830)) {
      t830 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t831 = t830;
    if (!u_truth(t831)) {
      t831 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n))), u_mk_con(&n_list_46_Nil, 0));
    }
    if (u_truth(t831)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t832 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t832)) {
      t832 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t833 = t832;
    if (!u_truth(t833)) {
      t833 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t833)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_834 = U_LIT("Bool");
    static struct u_object l_Int_835 = U_LIT("Int");
    static struct u_object l_Str_836 = U_LIT("Str");
    static struct u_object l_Char_837 = U_LIT("Char");
    static struct u_object l__45__62__838 = U_LIT("->");
    u_value t839 = f_list_46_contains_2(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_834, u_mk_con(&n_list_46_Cons, 2, &l_Int_835, u_mk_con(&n_list_46_Cons, 2, &l_Str_836, u_mk_con(&n_list_46_Cons, 2, &l_Char_837, u_mk_con(&n_list_46_Cons, 2, &l__45__62__838, u_mk_con(&n_list_46_Nil, 0)))))));
    if (!u_truth(t839)) {
      t839 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t840 = t839;
    if (!u_truth(t840)) {
      t840 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t840)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t841 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e842 = &t841;
    u_value l843 = v_ts;
    while (u_is_con(l843, &n_list_46_Cons)) {
      u_value v_t = u_field(l843, 0);
      if (u_truth(u_mk_bool(1))) {
        e842 = u_append(e842, f_check_46_free_1(v_t));
      }
      l843 = u_field(l843, 1);
    }
    u_value t844 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t841)), f_check_46_params_1(v_ps));
    v_undeclared = t844;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, u_mk_con(&n_list_46_Nil, 0)))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
  {
    u_value s845 = v_cs;
    int d846 = 0;
    if ((!d846) && u_is_con(s845, &n_list_46_Nil)) {
      d846 = 1;
      {
      }
    }
    if ((!d846) && (u_is_con(s845, &n_list_46_Cons) && u_is_con(u_field(u_field(s845, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s845, 0), 0);
      u_value v_n = u_field(u_field(u_field(s845, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s845, 0), 1), 1);
      u_value v_rest = u_field(s845, 1);
      d846 = 1;
      {
        u_value t847 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t847;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        u_value t848 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t848;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d846) && u_is_con(s845, &n_list_46_Cons)) {
      u_value v_rest = u_field(s845, 1);
      d846 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d846) {
      u_no_case(s845);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s849 = v_cs;
    int d850 = 0;
    if ((!d850) && u_is_con(s849, &n_list_46_Nil)) {
      d850 = 1;
      {
      }
    }
    if ((!d850) && (u_is_con(s849, &n_list_46_Cons) && u_is_con(u_field(u_field(s849, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s849, 0), 0);
      u_value v_n = u_field(u_field(u_field(s849, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s849, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s849, 0), 1), 2);
      u_value v_rest = u_field(s849, 1);
      d850 = 1;
      {
        u_value t851 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t851;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)));
        u_value t852 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_State_46_ctx_1(*r_s), v_n, v_ps, v_u));
        *r_s = t852;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d850) && u_is_con(s849, &n_list_46_Cons)) {
      u_value v_rest = u_field(s849, 1);
      d850 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d850) {
      u_no_case(s849);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
  {
    u_value s853 = v_cs;
    int d854 = 0;
    if ((!d854) && u_is_con(s853, &n_list_46_Nil)) {
      d854 = 1;
      {
      }
    }
    if ((!d854) && (u_is_con(s853, &n_list_46_Cons) && u_is_con(u_field(u_field(s853, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s853, 0), 0);
      u_value v_n = u_field(u_field(u_field(s853, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s853, 0), 1), 1);
      u_value v_ks = u_field(u_field(u_field(s853, 0), 1), 2);
      u_value v_rest = u_field(s853, 1);
      d854 = 1;
      {
        u_value t855 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t855;
        p_check_46_unique_ids(v_ps);
        u_value t856 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e857 = &t856;
        u_value l858 = v_ks;
        while (u_is_con(l858, &n_list_46_Cons)) {
          u_value v_k = u_field(l858, 0);
          if (u_truth(u_mk_bool(1))) {
            e857 = u_append(e857, f_syntax_46_Constr_46_name_1(v_k));
          }
          l858 = u_field(l858, 1);
        }
        u_value t859 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_State_46_ctx_1(*r_s), v_n, t856));
        *r_s = t859;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d854) && u_is_con(s853, &n_list_46_Cons)) {
      u_value v_rest = u_field(s853, 1);
      d854 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d854) {
      u_no_case(s853);
    }
  }
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s860 = v_ks;
    int d861 = 0;
    if ((!d861) && u_is_con(s860, &n_list_46_Nil)) {
      d861 = 1;
      {
      }
    }
    if ((!d861) && (u_is_con(s860, &n_list_46_Cons) && u_is_con(u_field(s860, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s860, 0), 0);
      u_value v_args = u_field(u_field(s860, 0), 1);
      u_value v_rest = u_field(s860, 1);
      d861 = 1;
      {
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t862 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_State_46_ctx_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t862;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d861) {
      u_no_case(s860);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s863 = v_xss;
  u_value t864 = NULL;
  if ((!t864) && u_is_con(s863, &n_list_46_Nil)) {
    t864 = v_r;
  }
  if ((!t864) && u_is_con(s863, &n_list_46_Cons)) {
    u_value v_xs = u_field(s863, 0);
    u_value v_rest = u_field(s863, 1);
    t864 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t864) {
    u_no_case(s863);
  }
  return t864;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s865 = v_xss;
  u_value t866 = NULL;
  if ((!t866) && u_is_con(s865, &n_list_46_Nil)) {
    t866 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t866) && u_is_con(s865, &n_list_46_Cons)) {
    u_value v_xs = u_field(s865, 0);
    t866 = v_xs;
  }
  if (!t866) {
    u_no_case(s865);
  }
  return t866;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s867 = v_xss;
    int d868 = 0;
    if ((!d868) && u_is_con(s867, &n_list_46_Nil)) {
      d868 = 1;
      {
        u_value t869 = u_mk_con(&n_list_46_Nil, 0);
        *r_yss = t869;
      }
    }
    if ((!d868) && u_is_con(s867, &n_list_46_Cons)) {
      u_value v_xs = u_field(s867, 0);
      u_value v_more = u_field(s867, 1);
      d868 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t870 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t870;
      }
    }
    if (!d868) {
      u_no_case(s867);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s871 = v_cs;
    int d872 = 0;
    if ((!d872) && u_is_con(s871, &n_list_46_Nil)) {
      d872 = 1;
      {
      }
    }
    if ((!d872) && (u_is_con(s871, &n_list_46_Cons) && u_is_con(u_field(u_field(s871, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s871, 0), 0);
      u_value v_n = u_field(u_field(u_field(s871, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s871, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s871, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s871, 0), 1), 3);
      u_value v_rest = u_field(s871, 1);
      d872 = 1;
      {
        u_value t873 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t873;
        p_check_46_overload(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), u_mk_con(&n_list_46_Nil, 0)));
        u_value t874 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t874;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d872) && u_is_con(s871, &n_list_46_Cons)) {
      u_value v_rest = u_field(s871, 1);
      d872 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d872) {
      u_no_case(s871);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s875 = v_cs;
    int d876 = 0;
    if ((!d876) && u_is_con(s875, &n_list_46_Nil)) {
      d876 = 1;
      {
      }
    }
    if ((!d876) && (u_is_con(s875, &n_list_46_Cons) && u_is_con(u_field(u_field(s875, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s875, 0), 0);
      u_value v_n = u_field(u_field(u_field(s875, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s875, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s875, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s875, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s875, 0), 1), 4);
      u_value v_rest = u_field(s875, 1);
      d876 = 1;
      {
        u_value t877 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t877;
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t878 = f_syntax_46_types_1(v_xs);
        v_ts = t878;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t879 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t879;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d876) && u_is_con(s875, &n_list_46_Cons)) {
      u_value v_rest = u_field(s875, 1);
      d876 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d876) {
      u_no_case(s875);
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
    u_value s880 = v_c;
    int d881 = 0;
    if ((!d881) && u_is_con(s880, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s880, 0);
      u_value v_ps = u_field(s880, 1);
      u_value v_bound = u_field(s880, 2);
      u_value v_res = u_field(s880, 3);
      u_value v_body = u_field(s880, 4);
      d881 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t882 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t882;
      }
    }
    if ((!d881) && u_is_con(s880, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s880, 0);
      u_value v_ps = u_field(s880, 1);
      u_value v_ins = u_field(s880, 2);
      u_value v_outs = u_field(s880, 3);
      u_value v_refs = u_field(s880, 4);
      u_value v_body = u_field(s880, 5);
      d881 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t883 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t883;
      }
    }
    if ((!d881) && u_is_con(s880, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s880, 0);
      u_value v_body = u_field(s880, 1);
      d881 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t884 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t884;
      }
    }
    if (!d881) {
      d881 = 1;
      {
        u_value t885 = v_c;
        *r_c1 = t885;
      }
    }
    if (!d881) {
      u_no_case(s880);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s886 = v_cs;
    int d887 = 0;
    if ((!d887) && u_is_con(s886, &n_list_46_Nil)) {
      d887 = 1;
      {
        u_value t888 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t888;
      }
    }
    if ((!d887) && u_is_con(s886, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s886, 0), 0);
      u_value v_c = u_field(u_field(s886, 0), 1);
      u_value v_more = u_field(s886, 1);
      d887 = 1;
      {
        u_value t889 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t889;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t890 = f_check_46_zonk_cmd_2(f_check_46_State_46_sub_1(*r_s), v_c1);
        v_c2 = t890;
        u_value s891 = f_check_46_unknown_in_cmd_1(v_c2);
        int d892 = 0;
        if ((!d892) && u_is_con(s891, &n_option_46_Some)) {
          u_value v_t = u_field(s891, 0);
          d892 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d892) && u_is_con(s891, &n_option_46_None)) {
          d892 = 1;
          {
          }
        }
        if (!d892) {
          u_no_case(s891);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t893 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t893;
      }
    }
    if (!d887) {
      u_no_case(s886);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s894 = v_cs;
  u_value t895 = NULL;
  if ((!t895) && u_is_con(s894, &n_list_46_Nil)) {
    t895 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t895) && (u_is_con(s894, &n_list_46_Cons) && u_is_con(u_field(u_field(s894, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s894, 0), 1), 0);
    u_value v_more = u_field(s894, 1);
    t895 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t895) && u_is_con(s894, &n_list_46_Cons)) {
    u_value v_more = u_field(s894, 1);
    t895 = f_check_46_block_names_1(v_more);
  }
  if (!t895) {
    u_no_case(s894);
  }
  return t895;
}

u_value f_check_46_Checked_46_cmds_1(u_value v_x) {
  u_value s896 = v_x;
  u_value t897 = NULL;
  if ((!t897) && u_is_con(s896, &n_check_46_Checked)) {
    u_value v_y = u_field(s896, 0);
    t897 = v_y;
  }
  if (!t897) {
    u_no_case(s896);
  }
  return t897;
}

u_value f_check_46_Checked_46_ctx_1(u_value v_x) {
  u_value s898 = v_x;
  u_value t899 = NULL;
  if ((!t899) && u_is_con(s898, &n_check_46_Checked)) {
    u_value v_y = u_field(s898, 1);
    t899 = v_y;
  }
  if (!t899) {
    u_no_case(s898);
  }
  return t899;
}

u_value f_check_46_Checked_46_imports_1(u_value v_x) {
  u_value s900 = v_x;
  u_value t901 = NULL;
  if ((!t901) && u_is_con(s900, &n_check_46_Checked)) {
    u_value v_y = u_field(s900, 2);
    t901 = v_y;
  }
  if (!t901) {
    u_no_case(s900);
  }
  return t901;
}

u_value f_check_46_Checked_46_cmds_58__61__2(u_value v_x, u_value v_y) {
  u_value s902 = v_x;
  u_value t903 = NULL;
  if ((!t903) && u_is_con(s902, &n_check_46_Checked)) {
    u_value v_a1 = u_field(s902, 1);
    u_value v_a2 = u_field(s902, 2);
    t903 = u_mk_con(&n_check_46_Checked, 3, v_y, v_a1, v_a2);
  }
  if (!t903) {
    u_no_case(s902);
  }
  return t903;
}

u_value f_check_46_Checked_46_ctx_58__61__2(u_value v_x, u_value v_y) {
  u_value s904 = v_x;
  u_value t905 = NULL;
  if ((!t905) && u_is_con(s904, &n_check_46_Checked)) {
    u_value v_a0 = u_field(s904, 0);
    u_value v_a2 = u_field(s904, 2);
    t905 = u_mk_con(&n_check_46_Checked, 3, v_a0, v_y, v_a2);
  }
  if (!t905) {
    u_no_case(s904);
  }
  return t905;
}

u_value f_check_46_Checked_46_imports_58__61__2(u_value v_x, u_value v_y) {
  u_value s906 = v_x;
  u_value t907 = NULL;
  if ((!t907) && u_is_con(s906, &n_check_46_Checked)) {
    u_value v_a0 = u_field(s906, 0);
    u_value v_a1 = u_field(s906, 1);
    t907 = u_mk_con(&n_check_46_Checked, 3, v_a0, v_a1, v_y);
  }
  if (!t907) {
    u_no_case(s906);
  }
  return t907;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_cache = NULL;
    u_value t908 = u_mk_con(&n_list_46_Nil, 0);
    v_cache = t908;
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
    u_value t909 = f_ctx_46_empty_ctx_0();
    v_c = t909;
    u_value t910 = u_mk_con(&n_list_46_Nil, 0);
    v_imports = t910;
    u_value t911 = u_mk_con(&n_list_46_Nil, 0);
    v_seen = t911;
    u_value t912 = u_mk_con(&n_list_46_Nil, 0);
    v_cs = t912;
    p_check_46_check_units(f_check_46_modules_1(f_syntax_46_Program_46_cmds_1(v_p)), &v_c, &v_imports, &v_seen, &v_cs, r_cache);
    p_check_46_check_unit(f_check_46_own_decls_2(v_main, f_syntax_46_Program_46_cmds_1(v_p)), v_main, v_c, v_imports, &v_own);
    u_value t913 = u_mk_con(&n_syntax_46_Program, 1, f_list_46__43__43__2(v_cs, f_check_46_Checked_46_cmds_1(v_own)));
    *r_q = t913;
  }
}

void p_check_46_check_units(u_value v_us, u_value *r_c, u_value *r_imports, u_value *r_seen, u_value *r_cs, u_value *r_cache) {
  {
    u_value v_key = NULL;
    u_value v_checked = NULL;
    u_value v_i = NULL;
    u_value s914 = v_us;
    int d915 = 0;
    if ((!d915) && u_is_con(s914, &n_list_46_Nil)) {
      d915 = 1;
      {
      }
    }
    if ((!d915) && u_is_con(s914, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s914, 0), 0);
      u_value v_ds = u_field(u_field(s914, 0), 1);
      u_value v_rest = u_field(s914, 1);
      d915 = 1;
      {
        static struct u_object l_prelude_916 = U_LIT("prelude");
        u_value t917 = u_mk_tuple(3, v_m, v_ds, f_check_46_positions_of_2(u_mk_con(&n_list_46_Cons, 2, &l_prelude_916, f_check_46_imports_in_2(v_ds, v_m)), *r_seen));
        v_key = t917;
        u_value s918 = f_check_46_position_in_3(v_key, *r_cache, u_mk_int(0));
        int d919 = 0;
        if ((!d919) && u_is_con(s918, &n_option_46_Some)) {
          u_value v_k = u_field(u_field(s918, 0), 0);
          u_value v_done = u_field(u_field(s918, 0), 1);
          d919 = 1;
          {
            u_value t920 = v_k;
            u_value t921 = v_done;
            v_i = t920;
            v_checked = t921;
          }
        }
        if ((!d919) && u_is_con(s918, &n_option_46_None)) {
          d919 = 1;
          {
            p_check_46_check_unit(v_ds, v_m, *r_c, *r_imports, &v_checked);
            u_value t922 = f_list_46_length_1(*r_cache);
            u_value t923 = f_list_46__43__43__2(*r_cache, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_key, v_checked), u_mk_con(&n_list_46_Nil, 0)));
            v_i = t922;
            *r_cache = t923;
          }
        }
        if (!d919) {
          u_no_case(s918);
        }
        u_value t924 = f_ctx_46_ctx_union_2(*r_c, f_check_46_Checked_46_ctx_1(v_checked));
        u_value t925 = f_list_46__43__43__2(*r_imports, f_check_46_Checked_46_imports_1(v_checked));
        u_value t926 = f_list_46__43__43__2(*r_cs, f_check_46_Checked_46_cmds_1(v_checked));
        *r_c = t924;
        *r_imports = t925;
        *r_cs = t926;
        u_value t927 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e928 = &t927;
        u_value l929 = f_list_46_firsts_1(f_check_46_Checked_46_imports_1(v_checked));
        while (u_is_con(l929, &n_list_46_Cons)) {
          u_value v_n = u_field(l929, 0);
          if (u_truth(u_mk_bool(1))) {
            e928 = u_append(e928, u_mk_tuple(2, v_n, v_i));
          }
          l929 = u_field(l929, 1);
        }
        u_value t930 = f_list_46__43__43__2(t927, *r_seen);
        *r_seen = t930;
        p_check_46_check_units(v_rest, r_c, r_imports, r_seen, r_cs, r_cache);
      }
    }
    if (!d915) {
      u_no_case(s914);
    }
  }
}

u_value f_check_46_positions_of_2(u_value v_ms, u_value v_seen) {
  u_value s931 = v_ms;
  u_value t932 = NULL;
  if ((!t932) && u_is_con(s931, &n_list_46_Nil)) {
    t932 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t932) && u_is_con(s931, &n_list_46_Cons)) {
    u_value v_m = u_field(s931, 0);
    u_value v_rest = u_field(s931, 1);
    u_value s933 = f_list_46_lookup_2(v_m, v_seen);
    u_value t934 = NULL;
    if ((!t934) && u_is_con(s933, &n_option_46_Some)) {
      u_value v_i = u_field(s933, 0);
      t934 = u_mk_con(&n_list_46_Cons, 2, v_i, f_check_46_positions_of_2(v_rest, v_seen));
    }
    if ((!t934) && u_is_con(s933, &n_option_46_None)) {
      t934 = f_check_46_positions_of_2(v_rest, v_seen);
    }
    if (!t934) {
      u_no_case(s933);
    }
    t932 = t934;
  }
  if (!t932) {
    u_no_case(s931);
  }
  return t932;
}

u_value f_check_46_position_in_3(u_value v_key, u_value v_cache, u_value v_i) {
  u_value s935 = v_cache;
  u_value t936 = NULL;
  if ((!t936) && u_is_con(s935, &n_list_46_Nil)) {
    t936 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t936) && u_is_con(s935, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s935, 0), 0);
    u_value v_checked = u_field(u_field(s935, 0), 1);
    u_value v_rest = u_field(s935, 1);
    u_value t937;
    if (u_truth(u_eq(v_k, v_key))) {
      t937 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_i, v_checked));
    } else {
      t937 = f_check_46_position_in_3(v_key, v_rest, u_add(v_i, u_mk_int(1)));
    }
    t936 = t937;
  }
  if (!t936) {
    u_no_case(s935);
  }
  return t936;
}

void p_check_46_check_unit(u_value v_ds, u_value v_main, u_value v_before, u_value v_imports, u_value *r_checked) {
  {
    u_value v_own = NULL;
    u_value v_s = NULL;
    u_value v_cs = NULL;
    u_value t938 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e939 = &t938;
    u_value l940 = f_list_46_distinct_1(f_list_46_firsts_1(v_ds));
    while (u_is_con(l940, &n_list_46_Cons)) {
      u_value v_m = u_field(l940, 0);
      if (u_truth(u_mk_bool(1))) {
        e939 = u_append(e939, u_mk_tuple(2, v_m, f_check_46_imports_in_2(v_ds, v_m)));
      }
      l940 = u_field(l940, 1);
    }
    u_value t941 = f_list_46__43__43__2(v_imports, t938);
    v_own = t941;
    u_value t942 = u_mk_con(&n_check_46_State, 6, v_before, f_map_46_empty_0(), u_mk_int(0), v_main, u_mk_con(&n_list_46_Nil, 0), f_check_46_visibility_2(v_own, v_main));
    v_s = t942;
    p_check_46_declare_datas(v_ds, &v_s);
    p_check_46_declare_sorts(v_ds, &v_s);
    p_check_46_declare_constrs(v_ds, &v_s);
    p_check_46_declare_funs(v_ds, &v_s);
    p_check_46_declare_procs(v_ds, &v_s);
    p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
    p_check_46_commands(v_ds, &v_cs, &v_s);
    u_value t943 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e944 = &t943;
    u_value l945 = f_list_46_distinct_1(f_list_46_firsts_1(v_ds));
    while (u_is_con(l945, &n_list_46_Cons)) {
      u_value v_m = u_field(l945, 0);
      if (u_truth(u_mk_bool(1))) {
        e944 = u_append(e944, u_mk_tuple(2, v_m, f_check_46_imports_in_2(v_ds, v_m)));
      }
      l945 = u_field(l945, 1);
    }
    u_value t946 = u_mk_con(&n_check_46_Checked, 3, v_cs, f_ctx_46_ctx_of_2(f_check_46_State_46_ctx_1(v_s), f_list_46_distinct_1(f_list_46_firsts_1(v_ds))), t943);
    *r_checked = t946;
  }
}

u_value f_check_46_modules_1(u_value v_cs) {
  u_value s947 = v_cs;
  u_value t948 = NULL;
  if ((!t948) && u_is_con(s947, &n_list_46_Nil)) {
    t948 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t948) && (u_is_con(s947, &n_list_46_Cons) && (u_is_con(u_field(s947, 0), &n_syntax_46_CModule) && u_equals(u_field(u_field(s947, 0), 0), u_mk_lit("prelude"))))) {
    u_value v_ds = u_field(u_field(s947, 0), 1);
    u_value v_rest = u_field(s947, 1);
    static struct u_object l_prelude_949 = U_LIT("prelude");
    static struct u_object l_prelude_950 = U_LIT("prelude");
    t948 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &l_prelude_949, f_check_46_flatten_2(&l_prelude_950, v_ds)), f_check_46_modules_1(v_rest));
  }
  if ((!t948) && (u_is_con(s947, &n_list_46_Cons) && u_is_con(u_field(s947, 0), &n_syntax_46_CModule))) {
    u_value v_n = u_field(u_field(s947, 0), 0);
    u_value v_ds = u_field(u_field(s947, 0), 1);
    u_value v_rest = u_field(s947, 1);
    t948 = f_list_46__43__43__2(f_check_46_modules_1(v_ds), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_check_46_own_decls_2(v_n, v_ds)), f_check_46_modules_1(v_rest)));
  }
  if ((!t948) && u_is_con(s947, &n_list_46_Cons)) {
    u_value v_rest = u_field(s947, 1);
    t948 = f_check_46_modules_1(v_rest);
  }
  if (!t948) {
    u_no_case(s947);
  }
  return t948;
}

u_value f_check_46_own_decls_2(u_value v_m, u_value v_cs) {
  u_value t951 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e952 = &t951;
  u_value l953 = v_cs;
  while (u_is_con(l953, &n_list_46_Cons)) {
    u_value v_c = u_field(l953, 0);
    if (u_truth(f_prelude_46_not_1(f_check_46_is_module_1(v_c)))) {
      e952 = u_append(e952, u_mk_tuple(2, v_m, f_check_46_qualify_2(v_m, v_c)));
    }
    l953 = u_field(l953, 1);
  }
  return t951;
}

u_value f_check_46_is_module_1(u_value v_c) {
  u_value s954 = v_c;
  u_value t955 = NULL;
  if ((!t955) && u_is_con(s954, &n_syntax_46_CModule)) {
    t955 = u_mk_bool(1);
  }
  if (!t955) {
    t955 = u_mk_bool(0);
  }
  if (!t955) {
    u_no_case(s954);
  }
  return t955;
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_956 = U_LIT("main");
    u_value t957 = f_check_46_start_2(&l_main_956, u_mk_con(&n_list_46_Nil, 0));
    v_s = t957;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t958 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(v_s), v_u);
    *r_t = t958;
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
  static struct u_object l_List_959 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_959), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_960 = U_LIT("list");
  static struct u_object l_List_961 = U_LIT("List");
  static struct u_object l_A_962 = U_LIT("A");
  static struct u_object l_Nil_963 = U_LIT("Nil");
  static struct u_object l_Cons_964 = U_LIT("Cons");
  static struct u_object l_A_965 = U_LIT("A");
  static struct u_object l_A_966 = U_LIT("A");
  static struct u_object l_list_967 = U_LIT("list");
  static struct u_object l_len_968 = U_LIT("len");
  static struct u_object l_A_969 = U_LIT("A");
  static struct u_object l_xs_970 = U_LIT("xs");
  static struct u_object l_A_971 = U_LIT("A");
  static struct u_object l_xs_972 = U_LIT("xs");
  static struct u_object l_Nil_973 = U_LIT("Nil");
  static struct u_object l_Cons_974 = U_LIT("Cons");
  static struct u_object l_ys_975 = U_LIT("ys");
  static struct u_object l__43__976 = U_LIT("+");
  static struct u_object l_len_977 = U_LIT("len");
  static struct u_object l_ys_978 = U_LIT("ys");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_960, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_961), u_mk_con(&n_list_46_Cons, 2, &l_A_962, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_963), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_964), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_965), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_966)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_967), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_968), u_mk_con(&n_list_46_Cons, 2, &l_A_969, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_970), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_971))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_972), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_973), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_974), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_975), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__976, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_977, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_978), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s979 = v_t;
  u_value t980 = NULL;
  if ((!t980) && u_is_con(s979, &n_c_46_CtVoid)) {
    static struct u_object l_void_981 = U_LIT("void");
    t980 = &l_void_981;
  }
  if ((!t980) && u_is_con(s979, &n_c_46_CtInt)) {
    static struct u_object l_int_982 = U_LIT("int");
    t980 = &l_int_982;
  }
  if ((!t980) && u_is_con(s979, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s979, 0);
    t980 = v_n;
  }
  if ((!t980) && (u_is_con(s979, &n_c_46_CtPtr) && u_is_con(u_field(s979, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s979, 0), 0);
    static struct u_object l__42__983 = U_LIT("*");
    t980 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__983);
  }
  if ((!t980) && u_is_con(s979, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s979, 0);
    static struct u_object l__32__42__984 = U_LIT(" *");
    t980 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__984);
  }
  if (!t980) {
    u_no_case(s979);
  }
  return t980;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s985 = v_t;
  u_value t986 = NULL;
  if ((!t986) && u_is_con(s985, &n_c_46_CtPtr)) {
    t986 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t986) {
    static struct u_object l__32__987 = U_LIT(" ");
    t986 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__987, v_x));
  }
  if (!t986) {
    u_no_case(s985);
  }
  return t986;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s988 = v_e;
  u_value t989 = NULL;
  if ((!t989) && u_is_con(s988, &n_c_46_CeVar)) {
    u_value v_x = u_field(s988, 0);
    t989 = v_x;
  }
  if ((!t989) && u_is_con(s988, &n_c_46_CeNum)) {
    u_value v_n = u_field(s988, 0);
    t989 = u_str_show(v_n);
  }
  if ((!t989) && u_is_con(s988, &n_c_46_CeStr)) {
    u_value v_s = u_field(s988, 0);
    t989 = u_str_quote(v_s);
  }
  if ((!t989) && u_is_con(s988, &n_c_46_CeCall)) {
    u_value v_f = u_field(s988, 0);
    u_value v_args = u_field(s988, 1);
    static struct u_object l__40__990 = U_LIT("(");
    static struct u_object l__44__32__991 = U_LIT(", ");
    u_value t992 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e993 = &t992;
    u_value l994 = v_args;
    while (u_is_con(l994, &n_list_46_Cons)) {
      u_value v_a = u_field(l994, 0);
      if (u_truth(u_mk_bool(1))) {
        e993 = u_append(e993, f_c_46_c_expr_1(v_a));
      }
      l994 = u_field(l994, 1);
    }
    static struct u_object l__41__995 = U_LIT(")");
    t989 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__990, u_concat(u_str_join(&l__44__32__991, t992), &l__41__995)));
  }
  if ((!t989) && u_is_con(s988, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s988, 0);
    u_value v_a = u_field(s988, 1);
    t989 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t989) && u_is_con(s988, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s988, 0);
    u_value v_a = u_field(s988, 1);
    u_value v_b = u_field(s988, 2);
    static struct u_object l__32__996 = U_LIT(" ");
    static struct u_object l__32__997 = U_LIT(" ");
    t989 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__996, u_concat(v_o, u_concat(&l__32__997, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t989) && u_is_con(s988, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s988, 0);
    u_value v_i = u_field(s988, 1);
    static struct u_object l__91__998 = U_LIT("[");
    static struct u_object l__93__999 = U_LIT("]");
    t989 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__998, u_concat(f_c_46_c_expr_1(v_i), &l__93__999)));
  }
  if ((!t989) && u_is_con(s988, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s988, 0);
    u_value v_f = u_field(s988, 1);
    static struct u_object l__45__62__1000 = U_LIT("->");
    t989 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__1000, v_f));
  }
  if ((!t989) && u_is_con(s988, &n_c_46_CeCast)) {
    u_value v_t = u_field(s988, 0);
    u_value v_a = u_field(s988, 1);
    static struct u_object l__40__1001 = U_LIT("(");
    static struct u_object l__41__1002 = U_LIT(")");
    t989 = u_concat(&l__40__1001, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__1002, f_c_46_c_operand_1(v_a))));
  }
  if ((!t989) && u_is_con(s988, &n_c_46_CeCond)) {
    u_value v_c = u_field(s988, 0);
    u_value v_a = u_field(s988, 1);
    u_value v_b = u_field(s988, 2);
    static struct u_object l__32__63__32__1003 = U_LIT(" ? ");
    static struct u_object l__32__58__32__1004 = U_LIT(" : ");
    t989 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__1003, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__1004, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t989) && u_is_con(s988, &n_c_46_CeInit)) {
    u_value v_es = u_field(s988, 0);
    static struct u_object l__123__1005 = U_LIT("{");
    static struct u_object l__44__32__1006 = U_LIT(", ");
    u_value t1007 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1008 = &t1007;
    u_value l1009 = v_es;
    while (u_is_con(l1009, &n_list_46_Cons)) {
      u_value v_a = u_field(l1009, 0);
      if (u_truth(u_mk_bool(1))) {
        e1008 = u_append(e1008, f_c_46_c_expr_1(v_a));
      }
      l1009 = u_field(l1009, 1);
    }
    static struct u_object l__125__1010 = U_LIT("}");
    t989 = u_concat(&l__123__1005, u_concat(u_str_join(&l__44__32__1006, t1007), &l__125__1010));
  }
  if (!t989) {
    u_no_case(s988);
  }
  return t989;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s1011 = v_e;
  u_value t1012 = NULL;
  if ((!t1012) && u_is_con(s1011, &n_c_46_CeVar)) {
    t1012 = f_c_46_c_expr_1(v_e);
  }
  if ((!t1012) && u_is_con(s1011, &n_c_46_CeNum)) {
    u_value v_n = u_field(s1011, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t1012 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t1012) && u_is_con(s1011, &n_c_46_CeStr)) {
    t1012 = f_c_46_c_expr_1(v_e);
  }
  if ((!t1012) && u_is_con(s1011, &n_c_46_CeCall)) {
    t1012 = f_c_46_c_expr_1(v_e);
  }
  if ((!t1012) && u_is_con(s1011, &n_c_46_CeIndex)) {
    t1012 = f_c_46_c_expr_1(v_e);
  }
  if ((!t1012) && u_is_con(s1011, &n_c_46_CeArrow)) {
    t1012 = f_c_46_c_expr_1(v_e);
  }
  if (!t1012) {
    static struct u_object l__40__1013 = U_LIT("(");
    static struct u_object l__41__1014 = U_LIT(")");
    t1012 = u_concat(&l__40__1013, u_concat(f_c_46_c_expr_1(v_e), &l__41__1014));
  }
  if (!t1012) {
    u_no_case(s1011);
  }
  return t1012;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s1015 = v_s;
  u_value t1016 = NULL;
  if ((!t1016) && u_is_con(s1015, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s1015, 0);
    static struct u_object l__59__1017 = U_LIT(";");
    t1016 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__1017)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1016) && (u_is_con(s1015, &n_c_46_CsDecl) && u_is_con(u_field(s1015, 2), &n_option_46_None))) {
    u_value v_t = u_field(s1015, 0);
    u_value v_x = u_field(s1015, 1);
    static struct u_object l__59__1018 = U_LIT(";");
    t1016 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__1018)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1016) && (u_is_con(s1015, &n_c_46_CsDecl) && u_is_con(u_field(s1015, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s1015, 0);
    u_value v_x = u_field(s1015, 1);
    u_value v_e = u_field(u_field(s1015, 2), 0);
    static struct u_object l__32__61__32__1019 = U_LIT(" = ");
    static struct u_object l__59__1020 = U_LIT(";");
    t1016 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__1019, u_concat(f_c_46_c_expr_1(v_e), &l__59__1020)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1016) && u_is_con(s1015, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s1015, 0);
    u_value v_e = u_field(s1015, 1);
    static struct u_object l__32__61__32__1021 = U_LIT(" = ");
    static struct u_object l__59__1022 = U_LIT(";");
    t1016 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__1021, u_concat(f_c_46_c_expr_1(v_e), &l__59__1022)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1016) && (u_is_con(s1015, &n_c_46_CsIf) && u_is_con(u_field(s1015, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s1015, 0);
    u_value v_l = u_field(s1015, 1);
    static struct u_object l_if_32__40__1023 = U_LIT("if (");
    static struct u_object l__41__32__123__1024 = U_LIT(") {");
    static struct u_object l__32__32__1025 = U_LIT("  ");
    static struct u_object l__125__1026 = U_LIT("}");
    t1016 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__1023, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__1024))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__1025), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__1026), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t1016) && u_is_con(s1015, &n_c_46_CsIf)) {
    u_value v_c = u_field(s1015, 0);
    u_value v_l = u_field(s1015, 1);
    u_value v_r = u_field(s1015, 2);
    static struct u_object l_if_32__40__1027 = U_LIT("if (");
    static struct u_object l__41__32__123__1028 = U_LIT(") {");
    static struct u_object l__32__32__1029 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__1030 = U_LIT("} else {");
    static struct u_object l__32__32__1031 = U_LIT("  ");
    static struct u_object l__125__1032 = U_LIT("}");
    t1016 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__1027, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__1028))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__1029), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__1030), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__1031), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__1032), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t1016) && u_is_con(s1015, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s1015, 0);
    u_value v_b = u_field(s1015, 1);
    static struct u_object l_while_32__40__1033 = U_LIT("while (");
    static struct u_object l__41__32__123__1034 = U_LIT(") {");
    static struct u_object l__32__32__1035 = U_LIT("  ");
    static struct u_object l__125__1036 = U_LIT("}");
    t1016 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__1033, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__1034))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__1035), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__1036), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t1016) && (u_is_con(s1015, &n_c_46_CsReturn) && u_is_con(u_field(s1015, 0), &n_option_46_None))) {
    static struct u_object l_return_59__1037 = U_LIT("return;");
    t1016 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__1037), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1016) && (u_is_con(s1015, &n_c_46_CsReturn) && u_is_con(u_field(s1015, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s1015, 0), 0);
    static struct u_object l_return_32__1038 = U_LIT("return ");
    static struct u_object l__59__1039 = U_LIT(";");
    t1016 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__1038, u_concat(f_c_46_c_expr_1(v_e), &l__59__1039))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1016) && u_is_con(s1015, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s1015, 0);
    static struct u_object l__123__1040 = U_LIT("{");
    static struct u_object l__32__32__1041 = U_LIT("  ");
    static struct u_object l__125__1042 = U_LIT("}");
    t1016 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__1040), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__1041), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__1042), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1016) {
    u_no_case(s1015);
  }
  return t1016;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t1043 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1044 = &t1043;
  u_value l1045 = v_ss;
  while (u_is_con(l1045, &n_list_46_Cons)) {
    u_value v_s = u_field(l1045, 0);
    if (u_truth(u_mk_bool(1))) {
      e1044 = u_append(e1044, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l1045 = u_field(l1045, 1);
  }
  return f_list_46_concat_1(t1043);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s1046 = v_ps;
  u_value t1047 = NULL;
  if ((!t1047) && u_is_con(s1046, &n_list_46_Nil)) {
    static struct u_object l_void_1048 = U_LIT("void");
    t1047 = &l_void_1048;
  }
  if (!t1047) {
    static struct u_object l__44__32__1049 = U_LIT(", ");
    u_value t1050 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1051 = &t1050;
    u_value l1052 = v_ps;
    while (u_is_con(l1052, &n_list_46_Cons)) {
      u_value v_p = u_field(l1052, 0);
      if (u_truth(u_mk_bool(1))) {
        e1051 = u_append(e1051, f_c_46_c_param_1(v_p));
      }
      l1052 = u_field(l1052, 1);
    }
    t1047 = u_str_join(&l__44__32__1049, t1050);
  }
  if (!t1047) {
    u_no_case(s1046);
  }
  return t1047;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s1053 = v_p;
  u_value t1054 = NULL;
  if (!t1054) {
    u_value v_t = u_field(s1053, 0);
    u_value v_x = u_field(s1053, 1);
    t1054 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t1054) {
    u_no_case(s1053);
  }
  return t1054;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s1055 = v_d;
  u_value t1056 = NULL;
  if ((!t1056) && u_is_con(s1055, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s1055, 0);
    static struct u_object l__35_include_32__1057 = U_LIT("#include ");
    t1056 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__1057, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1056) && u_is_con(s1055, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s1055, 0);
    u_value v_f = u_field(s1055, 1);
    u_value v_ps = u_field(s1055, 2);
    static struct u_object l__40__1058 = U_LIT("(");
    static struct u_object l__41__59__1059 = U_LIT(");");
    t1056 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__1058, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__1059))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1056) && u_is_con(s1055, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s1055, 0);
    u_value v_f = u_field(s1055, 1);
    u_value v_ps = u_field(s1055, 2);
    u_value v_b = u_field(s1055, 3);
    static struct u_object l__40__1060 = U_LIT("(");
    static struct u_object l__41__32__123__1061 = U_LIT(") {");
    static struct u_object l__32__32__1062 = U_LIT("  ");
    static struct u_object l__125__1063 = U_LIT("}");
    t1056 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__1060, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__1061))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__1062, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__1063, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t1056) && (u_is_con(s1055, &n_c_46_CdGlobal) && u_is_con(u_field(s1055, 2), &n_option_46_None))) {
    u_value v_t = u_field(s1055, 0);
    u_value v_x = u_field(s1055, 1);
    static struct u_object l__59__1064 = U_LIT(";");
    t1056 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__1064), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1056) && (u_is_con(s1055, &n_c_46_CdGlobal) && u_is_con(u_field(s1055, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s1055, 0);
    u_value v_x = u_field(s1055, 1);
    u_value v_e = u_field(u_field(s1055, 2), 0);
    static struct u_object l__32__61__32__1065 = U_LIT(" = ");
    static struct u_object l__59__1066 = U_LIT(";");
    t1056 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__1065, u_concat(f_c_46_c_expr_1(v_e), &l__59__1066))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1056) {
    u_no_case(s1055);
  }
  return t1056;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s1067 = v_f;
  u_value t1068 = NULL;
  if ((!t1068) && u_is_con(s1067, &n_c_46_CFile)) {
    u_value v_ds = u_field(s1067, 0);
    static struct u_object l__10__10__1069 = U_LIT("\n\n");
    u_value t1070 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1071 = &t1070;
    u_value l1072 = v_ds;
    while (u_is_con(l1072, &n_list_46_Cons)) {
      u_value v_d = u_field(l1072, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__1073 = U_LIT("\n");
        e1071 = u_append(e1071, u_str_join(&l__10__1073, f_c_46_c_decl_1(v_d)));
      }
      l1072 = u_field(l1072, 1);
    }
    static struct u_object l__10__1074 = U_LIT("\n");
    t1068 = u_concat(u_str_join(&l__10__10__1069, t1070), &l__10__1074);
  }
  if (!t1068) {
    u_no_case(s1067);
  }
  return t1068;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__1075 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__1075, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__123__125__44__59__46__1076 = U_LIT("()[]{},;.");
  return u_str_contains(&l__40__41__91__93__123__125__44__59__46__1076, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t1077 = u_is_letter(v_c);
  if (!u_truth(t1077)) {
    t1077 = u_is_digit(v_c);
  }
  u_value t1078 = t1077;
  if (!u_truth(t1078)) {
    t1078 = u_eq(v_c, u_mk_char(95));
  }
  return t1078;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t1079;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t1079 = u_str_get(v_s, v_i);
  } else {
    t1079 = u_chr(u_mk_int(0));
  }
  return t1079;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s1080 = v_c;
  u_value t1081 = NULL;
  if ((!t1081) && u_equals(s1080, u_mk_char(34))) {
    t1081 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t1081) && u_equals(s1080, u_mk_char(39))) {
    t1081 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t1081) && u_equals(s1080, u_mk_char(92))) {
    t1081 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t1081) && u_equals(s1080, u_mk_char(110))) {
    t1081 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t1081) && u_equals(s1080, u_mk_char(116))) {
    t1081 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t1081) && u_equals(s1080, u_mk_char(114))) {
    t1081 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t1081) {
    t1081 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1081) {
    u_no_case(s1080);
  }
  return t1081;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t1082;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t1082 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t1082 = v_i;
  }
  return t1082;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t1083 = u_mk_int(0);
    u_value t1084 = u_mk_int(1);
    u_value t1085 = u_mk_int(1);
    v_j = t1083;
    *r_line = t1084;
    *r_column = t1085;
    u_value t1090 = u_lt(v_j, v_i);
    while (u_truth(t1090)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t1086 = u_add(*r_line, u_mk_int(1));
            u_value t1087 = u_mk_int(1);
            *r_line = t1086;
            *r_column = t1087;
          }
        } else {
          {
            u_value t1088 = u_add(*r_column, u_mk_int(1));
            *r_column = t1088;
          }
        }
        u_value t1089 = u_add(v_j, u_mk_int(1));
        v_j = t1089;
      }
      t1090 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t1091 = *r_i;
    u_value t1092 = u_add(*r_i, u_mk_int(1));
    u_value t1093 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t1091;
    *r_i = t1092;
    v_parts = t1093;
    u_value t1094 = *r_i;
    v_from = t1094;
    u_value t1105 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t1105)) {
      {
        u_value t1095 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t1095;
        u_value t1096 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t1096)) {
          t1096 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t1096)) {
          {
            static struct u_object l_unterminated_32_lit_1097 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_1097, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s1098 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d1099 = 0;
            if ((!d1099) && u_is_con(s1098, &n_option_46_Some)) {
              u_value v_d = u_field(s1098, 0);
              d1099 = 1;
              {
                u_value t1100 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t1101 = u_add(*r_i, u_mk_int(2));
                v_parts = t1100;
                *r_i = t1101;
                u_value t1102 = *r_i;
                v_from = t1102;
              }
            }
            if ((!d1099) && u_is_con(s1098, &n_option_46_None)) {
              d1099 = 1;
              {
                static struct u_object l_invalid_32_escape_1103 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_1103, *r_i), "lex.LexError");
              }
            }
            if (!d1099) {
              u_no_case(s1098);
            }
          }
        } else {
          {
            u_value t1104 = u_add(*r_i, u_mk_int(1));
            *r_i = t1104;
          }
        }
      }
      t1105 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__1106 = U_LIT("");
    u_value t1107 = u_str_join(&l__1106, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t1108 = u_add(*r_i, u_mk_int(1));
    *r_text = t1107;
    *r_i = t1108;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1109 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t1109;
    u_value t1110 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1111 = v_j;
    *r_t = t1110;
    *r_i = t1111;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1112 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t1112;
    u_value s1113 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d1114 = 0;
    if ((!d1114) && u_is_con(s1113, &n_option_46_Some)) {
      u_value v_n = u_field(s1113, 0);
      d1114 = 1;
      {
        u_value t1115 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t1116 = v_j;
        *r_t = t1115;
        *r_i = t1116;
      }
    }
    if ((!d1114) && u_is_con(s1113, &n_option_46_None)) {
      d1114 = 1;
      {
        static struct u_object l_invalid_32_number_1117 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_1117, *r_i), "lex.LexError");
      }
    }
    if (!d1114) {
      u_no_case(s1113);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1118 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t1118;
    u_value t1119 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1120 = v_j;
    *r_t = t1119;
    *r_i = t1120;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t1121 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t1121;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t1122 = *r_i;
    v_start = t1122;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_1123 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_1123, v_start), "lex.LexError");
      }
    }
    u_value t1124 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t1124;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t1125 = u_str_get(v_s, *r_i);
    v_c = t1125;
    u_value s1126 = v_c;
    int d1127 = 0;
    if (!d1127) {
      u_value t1128 = u_is_letter(v_c);
      if (!u_truth(t1128)) {
        t1128 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t1128)) {
        d1127 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d1127) {
      if (u_truth(u_is_digit(v_c))) {
        d1127 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d1127) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d1127 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d1127) && u_equals(s1126, u_mk_char(34))) {
      d1127 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d1127) && u_equals(s1126, u_mk_char(39))) {
      d1127 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d1127) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d1127 = 1;
        {
          u_value t1129 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t1130 = u_add(*r_i, u_mk_int(1));
          *r_t = t1129;
          *r_i = t1130;
        }
      }
    }
    if (!d1127) {
      d1127 = 1;
      {
        static struct u_object l_unexpected_32_chara_1131 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_1131, *r_i), "lex.LexError");
      }
    }
    if (!d1127) {
      u_no_case(s1126);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t1132 = u_mk_int(0);
    v_i = t1132;
    u_value t1133 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1133;
    u_value t1140 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t1140)) {
      {
        u_value t1134 = v_i;
        v_start = t1134;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t1135 = u_add(v_i, u_mk_int(1));
            v_i = t1135;
          }
        } else {
          u_value t1136 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t1136)) {
            t1136 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t1136)) {
            {
              u_value t1138 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam1137, 0));
              v_i = t1138;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t1139 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t1139;
            }
          }
        }
      }
      t1140 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t1141 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t1141;
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
  u_value s1142 = v_s;
  u_value t1143 = NULL;
  if ((!t1143) && u_is_con(s1142, &n_set_46_SLeaf)) {
    t1143 = u_mk_bool(0);
  }
  if ((!t1143) && u_is_con(s1142, &n_set_46_SNode)) {
    u_value v_l = u_field(s1142, 0);
    u_value v_y = u_field(s1142, 1);
    u_value v_r = u_field(s1142, 2);
    u_value s1144 = u_compare(v_x, v_y);
    u_value t1145 = NULL;
    if ((!t1145) && u_equals(s1144, u_mk_int(-1))) {
      t1145 = f_set_46_member_2(v_x, v_l);
    }
    if ((!t1145) && u_equals(s1144, u_mk_int(1))) {
      t1145 = f_set_46_member_2(v_x, v_r);
    }
    if (!t1145) {
      t1145 = u_mk_bool(1);
    }
    if (!t1145) {
      u_no_case(s1144);
    }
    t1143 = t1145;
  }
  if (!t1143) {
    u_no_case(s1142);
  }
  return t1143;
}

u_value f_set_46_insert_2(u_value v_x, u_value v_s) {
  u_value s1146 = v_s;
  u_value t1147 = NULL;
  if ((!t1147) && u_is_con(s1146, &n_set_46_SLeaf)) {
    t1147 = u_mk_con(&n_set_46_SNode, 4, u_mk_con(&n_set_46_SLeaf, 0), v_x, u_mk_con(&n_set_46_SLeaf, 0), u_mk_int(1));
  }
  if ((!t1147) && u_is_con(s1146, &n_set_46_SNode)) {
    u_value v_l = u_field(s1146, 0);
    u_value v_y = u_field(s1146, 1);
    u_value v_r = u_field(s1146, 2);
    u_value s1148 = u_compare(v_x, v_y);
    u_value t1149 = NULL;
    if ((!t1149) && u_equals(s1148, u_mk_int(-1))) {
      t1149 = f_set_46_rebalance_3(f_set_46_insert_2(v_x, v_l), v_y, v_r);
    }
    if ((!t1149) && u_equals(s1148, u_mk_int(1))) {
      t1149 = f_set_46_rebalance_3(v_l, v_y, f_set_46_insert_2(v_x, v_r));
    }
    if (!t1149) {
      t1149 = v_s;
    }
    if (!t1149) {
      u_no_case(s1148);
    }
    t1147 = t1149;
  }
  if (!t1147) {
    u_no_case(s1146);
  }
  return t1147;
}

u_value f_set_46_from_list_1(u_value v_xs) {
  return f_set_46_insert_all_2(v_xs, u_mk_con(&n_set_46_SLeaf, 0));
}

u_value f_set_46_insert_all_2(u_value v_xs, u_value v_s) {
  u_value s1150 = v_xs;
  u_value t1151 = NULL;
  if ((!t1151) && u_is_con(s1150, &n_list_46_Nil)) {
    t1151 = v_s;
  }
  if ((!t1151) && u_is_con(s1150, &n_list_46_Cons)) {
    u_value v_x = u_field(s1150, 0);
    u_value v_rest = u_field(s1150, 1);
    t1151 = f_set_46_insert_all_2(v_rest, f_set_46_insert_2(v_x, v_s));
  }
  if (!t1151) {
    u_no_case(s1150);
  }
  return t1151;
}

u_value f_set_46_elements_1(u_value v_s) {
  return f_set_46_elements_onto_2(v_s, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_set_46_elements_onto_2(u_value v_s, u_value v_rest) {
  u_value s1152 = v_s;
  u_value t1153 = NULL;
  if ((!t1153) && u_is_con(s1152, &n_set_46_SLeaf)) {
    t1153 = v_rest;
  }
  if ((!t1153) && u_is_con(s1152, &n_set_46_SNode)) {
    u_value v_l = u_field(s1152, 0);
    u_value v_x = u_field(s1152, 1);
    u_value v_r = u_field(s1152, 2);
    t1153 = f_set_46_elements_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, v_x, f_set_46_elements_onto_2(v_r, v_rest)));
  }
  if (!t1153) {
    u_no_case(s1152);
  }
  return t1153;
}

u_value f_set_46_count_1(u_value v_s) {
  u_value s1154 = v_s;
  u_value t1155 = NULL;
  if ((!t1155) && u_is_con(s1154, &n_set_46_SLeaf)) {
    t1155 = u_mk_int(0);
  }
  if ((!t1155) && u_is_con(s1154, &n_set_46_SNode)) {
    u_value v_l = u_field(s1154, 0);
    u_value v_r = u_field(s1154, 2);
    t1155 = u_add(u_add(f_set_46_count_1(v_l), u_mk_int(1)), f_set_46_count_1(v_r));
  }
  if (!t1155) {
    u_no_case(s1154);
  }
  return t1155;
}

u_value f_set_46_depth_1(u_value v_s) {
  u_value s1156 = v_s;
  u_value t1157 = NULL;
  if ((!t1157) && u_is_con(s1156, &n_set_46_SLeaf)) {
    t1157 = u_mk_int(0);
  }
  if ((!t1157) && u_is_con(s1156, &n_set_46_SNode)) {
    u_value v_d = u_field(s1156, 3);
    t1157 = v_d;
  }
  if (!t1157) {
    u_no_case(s1156);
  }
  return t1157;
}

u_value f_set_46_snode_depth_2(u_value v_l, u_value v_r) {
  u_value t1158;
  if (u_truth(u_lt(f_set_46_depth_1(v_l), f_set_46_depth_1(v_r)))) {
    t1158 = f_set_46_depth_1(v_r);
  } else {
    t1158 = f_set_46_depth_1(v_l);
  }
  return u_add(u_mk_int(1), t1158);
}

u_value f_set_46_snode_3(u_value v_l, u_value v_x, u_value v_r) {
  return u_mk_con(&n_set_46_SNode, 4, v_l, v_x, v_r, f_set_46_snode_depth_2(v_l, v_r));
}

u_value f_set_46_turn_left_1(u_value v_s) {
  u_value s1159 = v_s;
  u_value t1160 = NULL;
  if ((!t1160) && (u_is_con(s1159, &n_set_46_SNode) && u_is_con(u_field(s1159, 2), &n_set_46_SNode))) {
    u_value v_a = u_field(s1159, 0);
    u_value v_x = u_field(s1159, 1);
    u_value v_b = u_field(u_field(s1159, 2), 0);
    u_value v_y = u_field(u_field(s1159, 2), 1);
    u_value v_c = u_field(u_field(s1159, 2), 2);
    t1160 = f_set_46_snode_3(f_set_46_snode_3(v_a, v_x, v_b), v_y, v_c);
  }
  if (!t1160) {
    t1160 = v_s;
  }
  if (!t1160) {
    u_no_case(s1159);
  }
  return t1160;
}

u_value f_set_46_turn_right_1(u_value v_s) {
  u_value s1161 = v_s;
  u_value t1162 = NULL;
  if ((!t1162) && (u_is_con(s1161, &n_set_46_SNode) && u_is_con(u_field(s1161, 0), &n_set_46_SNode))) {
    u_value v_a = u_field(u_field(s1161, 0), 0);
    u_value v_x = u_field(u_field(s1161, 0), 1);
    u_value v_b = u_field(u_field(s1161, 0), 2);
    u_value v_y = u_field(s1161, 1);
    u_value v_c = u_field(s1161, 2);
    t1162 = f_set_46_snode_3(v_a, v_x, f_set_46_snode_3(v_b, v_y, v_c));
  }
  if (!t1162) {
    t1162 = v_s;
  }
  if (!t1162) {
    u_no_case(s1161);
  }
  return t1162;
}

u_value f_set_46_smaller_1(u_value v_s) {
  u_value s1163 = v_s;
  u_value t1164 = NULL;
  if ((!t1164) && u_is_con(s1163, &n_set_46_SNode)) {
    u_value v_l = u_field(s1163, 0);
    t1164 = v_l;
  }
  if (!t1164) {
    t1164 = v_s;
  }
  if (!t1164) {
    u_no_case(s1163);
  }
  return t1164;
}

u_value f_set_46_greater_1(u_value v_s) {
  u_value s1165 = v_s;
  u_value t1166 = NULL;
  if ((!t1166) && u_is_con(s1165, &n_set_46_SNode)) {
    u_value v_r = u_field(s1165, 2);
    t1166 = v_r;
  }
  if (!t1166) {
    t1166 = v_s;
  }
  if (!t1166) {
    u_no_case(s1165);
  }
  return t1166;
}

u_value f_set_46_rebalance_3(u_value v_l, u_value v_x, u_value v_r) {
  u_value t1170;
  if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1))))) {
    u_value t1167;
    if (u_truth(u_lt(f_set_46_depth_1(f_set_46_smaller_1(v_l)), f_set_46_depth_1(f_set_46_greater_1(v_l))))) {
      t1167 = f_set_46_turn_left_1(v_l);
    } else {
      t1167 = v_l;
    }
    t1170 = f_set_46_turn_right_1(f_set_46_snode_3(t1167, v_x, v_r));
  } else {
    u_value t1169;
    if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1))))) {
      u_value t1168;
      if (u_truth(u_lt(f_set_46_depth_1(f_set_46_greater_1(v_r)), f_set_46_depth_1(f_set_46_smaller_1(v_r))))) {
        t1168 = f_set_46_turn_right_1(v_r);
      } else {
        t1168 = v_r;
      }
      t1169 = f_set_46_turn_left_1(f_set_46_snode_3(v_l, v_x, t1168));
    } else {
      t1169 = f_set_46_snode_3(v_l, v_x, v_r);
    }
    t1170 = t1169;
  }
  return t1170;
}

u_value f_set_46_well_formed_1(u_value v_s) {
  u_value s1171 = v_s;
  u_value t1172 = NULL;
  if ((!t1172) && u_is_con(s1171, &n_set_46_SLeaf)) {
    t1172 = u_mk_bool(1);
  }
  if ((!t1172) && u_is_con(s1171, &n_set_46_SNode)) {
    u_value v_l = u_field(s1171, 0);
    u_value v_r = u_field(s1171, 2);
    u_value v_d = u_field(s1171, 3);
    u_value t1173 = f_set_46_well_formed_1(v_l);
    if (u_truth(t1173)) {
      t1173 = f_set_46_well_formed_1(v_r);
    }
    u_value t1174 = t1173;
    if (u_truth(t1174)) {
      t1174 = u_eq(v_d, f_set_46_snode_depth_2(v_l, v_r));
    }
    u_value t1175 = t1174;
    if (u_truth(t1175)) {
      t1175 = f_prelude_46__60__61__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1)));
    }
    u_value t1176 = t1175;
    if (u_truth(t1176)) {
      t1176 = f_prelude_46__60__61__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1)));
    }
    t1172 = t1176;
  }
  if (!t1172) {
    u_no_case(s1171);
  }
  return t1172;
}

u_value f_set_46_upto_1(u_value v_n) {
  u_value t1177;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t1177 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1177 = f_list_46__43__43__2(f_set_46_upto_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t1177;
}

u_value f_parse_46_Notations_46_cmds_1(u_value v_x) {
  u_value s1178 = v_x;
  u_value t1179 = NULL;
  if ((!t1179) && u_is_con(s1178, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1178, 0);
    t1179 = v_y;
  }
  if (!t1179) {
    u_no_case(s1178);
  }
  return t1179;
}

u_value f_parse_46_Notations_46_ops_1(u_value v_x) {
  u_value s1180 = v_x;
  u_value t1181 = NULL;
  if ((!t1181) && u_is_con(s1180, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1180, 1);
    t1181 = v_y;
  }
  if (!t1181) {
    u_no_case(s1180);
  }
  return t1181;
}

u_value f_parse_46_Notations_46_cmds_58__61__2(u_value v_x, u_value v_y) {
  u_value s1182 = v_x;
  u_value t1183 = NULL;
  if ((!t1183) && u_is_con(s1182, &n_parse_46_Notations)) {
    u_value v_a1 = u_field(s1182, 1);
    t1183 = u_mk_con(&n_parse_46_Notations, 2, v_y, v_a1);
  }
  if (!t1183) {
    u_no_case(s1182);
  }
  return t1183;
}

u_value f_parse_46_Notations_46_ops_58__61__2(u_value v_x, u_value v_y) {
  u_value s1184 = v_x;
  u_value t1185 = NULL;
  if ((!t1185) && u_is_con(s1184, &n_parse_46_Notations)) {
    u_value v_a0 = u_field(s1184, 0);
    t1185 = u_mk_con(&n_parse_46_Notations, 2, v_a0, v_y);
  }
  if (!t1185) {
    u_no_case(s1184);
  }
  return t1185;
}

u_value f_parse_46_Scope_46_params_1(u_value v_x) {
  u_value s1186 = v_x;
  u_value t1187 = NULL;
  if ((!t1187) && u_is_con(s1186, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1186, 0);
    t1187 = v_y;
  }
  if (!t1187) {
    u_no_case(s1186);
  }
  return t1187;
}

u_value f_parse_46_Scope_46_notations_1(u_value v_x) {
  u_value s1188 = v_x;
  u_value t1189 = NULL;
  if ((!t1189) && u_is_con(s1188, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1188, 1);
    t1189 = v_y;
  }
  if (!t1189) {
    u_no_case(s1188);
  }
  return t1189;
}

u_value f_parse_46_Scope_46_params_58__61__2(u_value v_x, u_value v_y) {
  u_value s1190 = v_x;
  u_value t1191 = NULL;
  if ((!t1191) && u_is_con(s1190, &n_parse_46_Scope)) {
    u_value v_a1 = u_field(s1190, 1);
    t1191 = u_mk_con(&n_parse_46_Scope, 2, v_y, v_a1);
  }
  if (!t1191) {
    u_no_case(s1190);
  }
  return t1191;
}

u_value f_parse_46_Scope_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1192 = v_x;
  u_value t1193 = NULL;
  if ((!t1193) && u_is_con(s1192, &n_parse_46_Scope)) {
    u_value v_a0 = u_field(s1192, 0);
    t1193 = u_mk_con(&n_parse_46_Scope, 2, v_a0, v_y);
  }
  if (!t1193) {
    u_no_case(s1192);
  }
  return t1193;
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
  static struct u_object l___1194 = U_LIT("_");
  static struct u_object l_assert_1195 = U_LIT("assert");
  static struct u_object l_assign_1196 = U_LIT("assign");
  static struct u_object l_call_1197 = U_LIT("call");
  static struct u_object l_case_1198 = U_LIT("case");
  static struct u_object l_catch_1199 = U_LIT("catch");
  static struct u_object l_data_1200 = U_LIT("data");
  static struct u_object l_else_1201 = U_LIT("else");
  static struct u_object l_end_1202 = U_LIT("end");
  static struct u_object l_false_1203 = U_LIT("false");
  static struct u_object l_for_1204 = U_LIT("for");
  static struct u_object l_function_1205 = U_LIT("function");
  static struct u_object l_if_1206 = U_LIT("if");
  static struct u_object l_import_1207 = U_LIT("import");
  static struct u_object l_in_1208 = U_LIT("in");
  static struct u_object l_infix_1209 = U_LIT("infix");
  static struct u_object l_lambda_1210 = U_LIT("lambda");
  static struct u_object l_let_1211 = U_LIT("let");
  static struct u_object l_local_1212 = U_LIT("local");
  static struct u_object l_match_1213 = U_LIT("match");
  static struct u_object l_out_1214 = U_LIT("out");
  static struct u_object l_postfix_1215 = U_LIT("postfix");
  static struct u_object l_prefix_1216 = U_LIT("prefix");
  static struct u_object l_procedure_1217 = U_LIT("procedure");
  static struct u_object l_raise_1218 = U_LIT("raise");
  static struct u_object l_ref_1219 = U_LIT("ref");
  static struct u_object l_skip_1220 = U_LIT("skip");
  static struct u_object l_test_1221 = U_LIT("test");
  static struct u_object l_then_1222 = U_LIT("then");
  static struct u_object l_true_1223 = U_LIT("true");
  static struct u_object l_type_1224 = U_LIT("type");
  static struct u_object l_try_1225 = U_LIT("try");
  static struct u_object l_while_1226 = U_LIT("while");
  static struct u_object l_with_1227 = U_LIT("with");
  u_pop(handler);
  cache = f_set_46_from_list_1(u_mk_con(&n_list_46_Cons, 2, &l___1194, u_mk_con(&n_list_46_Cons, 2, &l_assert_1195, u_mk_con(&n_list_46_Cons, 2, &l_assign_1196, u_mk_con(&n_list_46_Cons, 2, &l_call_1197, u_mk_con(&n_list_46_Cons, 2, &l_case_1198, u_mk_con(&n_list_46_Cons, 2, &l_catch_1199, u_mk_con(&n_list_46_Cons, 2, &l_data_1200, u_mk_con(&n_list_46_Cons, 2, &l_else_1201, u_mk_con(&n_list_46_Cons, 2, &l_end_1202, u_mk_con(&n_list_46_Cons, 2, &l_false_1203, u_mk_con(&n_list_46_Cons, 2, &l_for_1204, u_mk_con(&n_list_46_Cons, 2, &l_function_1205, u_mk_con(&n_list_46_Cons, 2, &l_if_1206, u_mk_con(&n_list_46_Cons, 2, &l_import_1207, u_mk_con(&n_list_46_Cons, 2, &l_in_1208, u_mk_con(&n_list_46_Cons, 2, &l_infix_1209, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1210, u_mk_con(&n_list_46_Cons, 2, &l_let_1211, u_mk_con(&n_list_46_Cons, 2, &l_local_1212, u_mk_con(&n_list_46_Cons, 2, &l_match_1213, u_mk_con(&n_list_46_Cons, 2, &l_out_1214, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1215, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1216, u_mk_con(&n_list_46_Cons, 2, &l_procedure_1217, u_mk_con(&n_list_46_Cons, 2, &l_raise_1218, u_mk_con(&n_list_46_Cons, 2, &l_ref_1219, u_mk_con(&n_list_46_Cons, 2, &l_skip_1220, u_mk_con(&n_list_46_Cons, 2, &l_test_1221, u_mk_con(&n_list_46_Cons, 2, &l_then_1222, u_mk_con(&n_list_46_Cons, 2, &l_true_1223, u_mk_con(&n_list_46_Cons, 2, &l_type_1224, u_mk_con(&n_list_46_Cons, 2, &l_try_1225, u_mk_con(&n_list_46_Cons, 2, &l_while_1226, u_mk_con(&n_list_46_Cons, 2, &l_with_1227, u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_set_46_member_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1228 = U_LIT("=");
  static struct u_object l__61__62__1229 = U_LIT("=>");
  static struct u_object l__58__61__1230 = U_LIT(":=");
  static struct u_object l__58__1231 = U_LIT(":");
  static struct u_object l__124__1232 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1228, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1229, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1230, u_mk_con(&n_list_46_Cons, 2, &l__58__1231, u_mk_con(&n_list_46_Cons, 2, &l__124__1232, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1233 = v_ns;
  u_value t1234 = NULL;
  if ((!t1234) && u_is_con(s1233, &n_list_46_Nil)) {
    t1234 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1234) && (u_is_con(s1233, &n_list_46_Cons) && (u_is_con(u_field(s1233, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1233, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1233, 0), 1);
    u_value v_ops = u_field(u_field(s1233, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1234 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1234) && u_is_con(s1233, &n_list_46_Cons)) {
    u_value v_more = u_field(s1233, 1);
    t1234 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1234) {
    u_no_case(s1233);
  }
  return t1234;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1235 = v_ns;
  u_value t1236 = NULL;
  if ((!t1236) && u_is_con(s1235, &n_list_46_Nil)) {
    t1236 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1236) && (u_is_con(s1235, &n_list_46_Cons) && (u_is_con(u_field(s1235, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1235, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1235, 0), 1);
    u_value v_ops = u_field(u_field(s1235, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1236 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1236) && u_is_con(s1235, &n_list_46_Cons)) {
    u_value v_more = u_field(s1235, 1);
    t1236 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1236) {
    u_no_case(s1235);
  }
  return t1236;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1237 = v_ns;
  u_value t1238 = NULL;
  if ((!t1238) && u_is_con(s1237, &n_list_46_Nil)) {
    t1238 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1238) && (u_is_con(s1237, &n_list_46_Cons) && u_is_con(u_field(s1237, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1237, 0), 0);
    u_value v_p = u_field(u_field(s1237, 0), 1);
    u_value v_ops = u_field(u_field(s1237, 0), 2);
    u_value t1239 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1239)) {
      t1239 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1240 = t1239;
    if (u_truth(t1240)) {
      t1240 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1240)) {
      t1238 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1238) && u_is_con(s1237, &n_list_46_Cons)) {
    u_value v_more = u_field(s1237, 1);
    t1238 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1238) {
    u_no_case(s1237);
  }
  return t1238;
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
  u_value s1241 = v_cs;
  u_value t1242 = NULL;
  if ((!t1242) && u_is_con(s1241, &n_list_46_Nil)) {
    t1242 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1242) && (u_is_con(s1241, &n_list_46_Cons) && u_is_con(u_field(s1241, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1241, 0), 2);
    u_value v_more = u_field(s1241, 1);
    t1242 = f_list_46__43__43__2(v_ops, f_parse_46_operators_of_1(v_more));
  }
  if ((!t1242) && u_is_con(s1241, &n_list_46_Cons)) {
    u_value v_more = u_field(s1241, 1);
    t1242 = f_parse_46_operators_of_1(v_more);
  }
  if (!t1242) {
    u_no_case(s1241);
  }
  return t1242;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1243 = v_ts;
  u_value t1244 = NULL;
  if ((!t1244) && u_is_con(s1243, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1243, 0), 0);
    t1244 = v_t;
  }
  if (!t1244) {
    t1244 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1244) {
    u_no_case(s1243);
  }
  return t1244;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1245 = v_ts;
  u_value t1246 = NULL;
  if ((!t1246) && (u_is_con(s1245, &n_list_46_Cons) && u_is_con(u_field(s1245, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1245, 1), 0), 0);
    t1246 = v_t;
  }
  if (!t1246) {
    t1246 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1246) {
    u_no_case(s1245);
  }
  return t1246;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1247 = v_ts;
  u_value t1248 = NULL;
  if ((!t1248) && u_is_con(s1247, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1247, 0), 1);
    t1248 = v_i;
  }
  if (!t1248) {
    t1248 = u_mk_int(0);
  }
  if (!t1248) {
    u_no_case(s1247);
  }
  return t1248;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1249 = v_t;
  u_value t1250 = NULL;
  if ((!t1250) && u_is_con(s1249, &n_lex_46_TName)) {
    u_value v_n = u_field(s1249, 0);
    t1250 = v_n;
  }
  if ((!t1250) && u_is_con(s1249, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1249, 0);
    t1250 = v_s;
  }
  if ((!t1250) && u_is_con(s1249, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1249, 0);
    t1250 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1250) && u_is_con(s1249, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1249, 0);
    t1250 = u_str_show(v_n);
  }
  if ((!t1250) && u_is_con(s1249, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1249, 0);
    t1250 = u_str_quote(v_s);
  }
  if ((!t1250) && u_is_con(s1249, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1249, 0);
    static struct u_object l__39__1251 = U_LIT("'");
    static struct u_object l__39__1252 = U_LIT("'");
    t1250 = u_concat(&l__39__1251, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1252));
  }
  if ((!t1250) && u_is_con(s1249, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1253 = U_LIT("end of input");
    t1250 = &l_end_32_of_32_input_1253;
  }
  if (!t1250) {
    u_no_case(s1249);
  }
  return t1250;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1254 = f_parse_46_head_token_1(v_ts);
  u_value t1255 = NULL;
  if ((!t1255) && u_is_con(s1254, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1254, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1255 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1255) && u_is_con(s1254, &n_lex_46_TName)) {
    u_value v_n = u_field(s1254, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1255 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1255) {
    t1255 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1255) {
    u_no_case(s1254);
  }
  return t1255;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1256 = *r_ts;
    int d1257 = 0;
    if ((!d1257) && u_is_con(s1256, &n_list_46_Cons)) {
      u_value v_more = u_field(s1256, 1);
      d1257 = 1;
      {
        u_value t1258 = v_more;
        *r_ts = t1258;
      }
    }
    if (!d1257) {
      d1257 = 1;
      {
      }
    }
    if (!d1257) {
      u_no_case(s1256);
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
        static struct u_object l__32_expected_32_but_32__1259 = U_LIT(" expected but ");
        static struct u_object l__32_found_1260 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1259, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1260))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1261 = f_parse_46_head_token_1(*r_ts);
    int d1262 = 0;
    if ((!d1262) && u_is_con(s1261, &n_lex_46_TName)) {
      u_value v_m = u_field(s1261, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1262 = 1;
        {
          u_value t1263 = v_m;
          *r_n = t1263;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1262) {
      d1262 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1264 = U_LIT("name expected but ");
        static struct u_object l__32_found_1265 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1264, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1265)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1262) {
      u_no_case(s1261);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1266 = f_parse_46_head_token_1(*r_ts);
    int d1267 = 0;
    if ((!d1267) && u_is_con(s1266, &n_lex_46_TName)) {
      u_value v_m = u_field(s1266, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_m))) {
        d1267 = 1;
        {
          static struct u_object l_operator_32__1268 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1269 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1268, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1269)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1267) {
      d1267 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1267) {
      u_no_case(s1266);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1270 = f_parse_46_head_token_1(*r_ts);
    int d1271 = 0;
    if ((!d1271) && u_is_con(s1270, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1270, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1271 = 1;
        {
          u_value t1272 = v_s;
          *r_n = t1272;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1271) {
      d1271 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1271) {
      u_no_case(s1270);
    }
  }
}

u_value f_parse_46_Ops_46_notations_1(u_value v_x) {
  u_value s1273 = v_x;
  u_value t1274 = NULL;
  if ((!t1274) && u_is_con(s1273, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1273, 0);
    t1274 = v_y;
  }
  if (!t1274) {
    u_no_case(s1273);
  }
  return t1274;
}

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x) {
  u_value s1275 = v_x;
  u_value t1276 = NULL;
  if ((!t1276) && u_is_con(s1275, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1275, 1);
    t1276 = v_y;
  }
  if (!t1276) {
    u_no_case(s1275);
  }
  return t1276;
}

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x) {
  u_value s1277 = v_x;
  u_value t1278 = NULL;
  if ((!t1278) && u_is_con(s1277, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1277, 2);
    t1278 = v_y;
  }
  if (!t1278) {
    u_no_case(s1277);
  }
  return t1278;
}

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x) {
  u_value s1279 = v_x;
  u_value t1280 = NULL;
  if ((!t1280) && u_is_con(s1279, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1279, 3);
    t1280 = v_y;
  }
  if (!t1280) {
    u_no_case(s1279);
  }
  return t1280;
}

u_value f_parse_46_Ops_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1281 = v_x;
  u_value t1282 = NULL;
  if ((!t1282) && u_is_con(s1281, &n_parse_46_Ops)) {
    u_value v_a1 = u_field(s1281, 1);
    u_value v_a2 = u_field(s1281, 2);
    u_value v_a3 = u_field(s1281, 3);
    t1282 = u_mk_con(&n_parse_46_Ops, 4, v_y, v_a1, v_a2, v_a3);
  }
  if (!t1282) {
    u_no_case(s1281);
  }
  return t1282;
}

u_value f_parse_46_Ops_46_prefix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1283 = v_x;
  u_value t1284 = NULL;
  if ((!t1284) && u_is_con(s1283, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1283, 0);
    u_value v_a2 = u_field(s1283, 2);
    u_value v_a3 = u_field(s1283, 3);
    t1284 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_y, v_a2, v_a3);
  }
  if (!t1284) {
    u_no_case(s1283);
  }
  return t1284;
}

u_value f_parse_46_Ops_46_postfix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1285 = v_x;
  u_value t1286 = NULL;
  if ((!t1286) && u_is_con(s1285, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1285, 0);
    u_value v_a1 = u_field(s1285, 1);
    u_value v_a3 = u_field(s1285, 3);
    t1286 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_y, v_a3);
  }
  if (!t1286) {
    u_no_case(s1285);
  }
  return t1286;
}

u_value f_parse_46_Ops_46_infix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1287 = v_x;
  u_value t1288 = NULL;
  if ((!t1288) && u_is_con(s1287, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1287, 0);
    u_value v_a1 = u_field(s1287, 1);
    u_value v_a2 = u_field(s1287, 2);
    t1288 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_a2, v_y);
  }
  if (!t1288) {
    u_no_case(s1287);
  }
  return t1288;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1289 = v_rest;
    int d1290 = 0;
    if ((!d1290) && u_is_con(s1289, &n_list_46_Nil)) {
      d1290 = 1;
      {
      }
    }
    if ((!d1290) && (u_is_con(s1289, &n_list_46_Cons) && u_is_con(u_field(s1289, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1289, 0), 0);
      u_value v_i = u_field(u_field(s1289, 0), 1);
      d1290 = 1;
      {
        static struct u_object l_unexpected_32_opera_1291 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1291, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1290) {
      d1290 = 1;
      {
        static struct u_object l_missing_32_operator_1292 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1292, v_at), "parse.ParseError");
      }
    }
    if (!d1290) {
      u_no_case(s1289);
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
    u_value s1293 = v_items;
    int d1294 = 0;
    if ((!d1294) && (u_is_con(s1293, &n_list_46_Cons) && u_is_con(u_field(s1293, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1293, 0), 0);
      u_value v_more = u_field(s1293, 1);
      d1294 = 1;
      {
        u_value t1295 = v_x;
        u_value t1296 = v_more;
        *r_a = t1295;
        *r_rest = t1296;
      }
    }
    if ((!d1294) && (u_is_con(s1293, &n_list_46_Cons) && u_is_con(u_field(s1293, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1293, 0), 0);
      u_value v_i = u_field(u_field(s1293, 0), 1);
      u_value v_more = u_field(s1293, 1);
      d1294 = 1;
      {
        u_value s1297 = f_parse_46_prefix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op);
        int d1298 = 0;
        if ((!d1298) && u_is_con(s1297, &n_option_46_Some)) {
          u_value v_p = u_field(s1297, 0);
          d1298 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1299 = u_apply(f_parse_46_Ops_46_prefix_op_1(v_o), 2, v_op, v_b);
            *r_a = t1299;
          }
        }
        if ((!d1298) && u_is_con(s1297, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op))) {
            d1298 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1300 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1300), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1298) && u_is_con(s1297, &n_option_46_None)) {
          d1298 = 1;
          {
            static struct u_object l_undeclared_32_opera_1301 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1301, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1298) {
          u_no_case(s1297);
        }
      }
    }
    if ((!d1294) && u_is_con(s1293, &n_list_46_Nil)) {
      d1294 = 1;
      {
        static struct u_object l_missing_32_operand_1302 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1302, v_at), "parse.ParseError");
      }
    }
    if (!d1294) {
      u_no_case(s1293);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1303 = v_items;
    int d1304 = 0;
    if ((!d1304) && (u_is_con(s1303, &n_list_46_Cons) && u_is_con(u_field(s1303, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1303, 0), 0);
      u_value v_i = u_field(u_field(s1303, 0), 1);
      u_value v_tail = u_field(s1303, 1);
      d1304 = 1;
      {
        u_value s1305 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op), f_parse_46_infix_of_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op));
        int d1306 = 0;
        if ((!d1306) && u_is_con(u_field(s1305, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1305, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1306 = 1;
            {
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_postfix_op_1(v_o), 2, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1306) && u_is_con(u_field(s1305, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1305, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1305, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1306 = 1;
            {
              u_value t1307;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1307 = v_p;
              } else {
                t1307 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1307, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)));
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_infix_op_1(v_o), 3, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1306) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op)))) {
            d1306 = 1;
            {
              static struct u_object l_undeclared_32_opera_1308 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1308, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1306) {
          d1306 = 1;
          {
            u_value t1309 = v_lhs;
            u_value t1310 = v_items;
            *r_a = t1309;
            *r_rest = t1310;
          }
        }
        if (!d1306) {
          u_no_case(s1305);
        }
      }
    }
    if (!d1304) {
      d1304 = 1;
      {
        u_value t1311 = v_lhs;
        u_value t1312 = v_items;
        *r_a = t1311;
        *r_rest = t1312;
      }
    }
    if (!d1304) {
      u_no_case(s1303);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1313 = v_rest;
    int d1314 = 0;
    if ((!d1314) && (u_is_con(s1313, &n_list_46_Cons) && u_is_con(u_field(s1313, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1313, 0), 0);
      u_value v_i = u_field(u_field(s1313, 0), 1);
      u_value t1317 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1317)) {
        t1317 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1317)) {
        d1314 = 1;
        {
          static struct u_object l_operator_32__1315 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1316 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1315, u_concat(v_op, &l__32_is_32_not_32_associat_1316)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1314) {
      d1314 = 1;
      {
      }
    }
    if (!d1314) {
      u_no_case(s1313);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1318 = v_fp;
  u_value t1319 = NULL;
  if ((!t1319) && u_is_con(s1318, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1318, 0), 1);
    t1319 = u_eq(v_q, v_p);
  }
  if (!t1319) {
    t1319 = u_mk_bool(0);
  }
  if (!t1319) {
    u_no_case(s1318);
  }
  return t1319;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1320 = v_ts;
  u_value t1321 = NULL;
  if ((!t1321) && (u_is_con(s1320, &n_list_46_Cons) && (u_is_con(u_field(s1320, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1320, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1320, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1320, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1320, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1320, 0), 1);
    t1321 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1321) && (u_is_con(s1320, &n_list_46_Cons) && u_is_con(u_field(s1320, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1320, 0);
    t1321 = v_t;
  }
  if ((!t1321) && u_is_con(s1320, &n_list_46_Nil)) {
    t1321 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1321) {
    static struct u_object l__40_invalid_41__1322 = U_LIT("(invalid)");
    t1321 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1322), v_ts);
  }
  if (!t1321) {
    u_no_case(s1320);
  }
  return t1321;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1323 = v_ts;
  u_value t1324 = NULL;
  if ((!t1324) && (u_is_con(s1323, &n_list_46_Cons) && (u_is_con(u_field(s1323, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1323, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1323, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1323, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1323, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1323, 0), 1);
    t1324 = v_cs;
  }
  if (!t1324) {
    t1324 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1324) {
    u_no_case(s1323);
  }
  return t1324;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1325 = U_LIT("->");
  u_value t1332;
  if (u_truth(u_eq(v_op, &l__45__62__1325))) {
    u_value t1326 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1327 = &t1326;
    u_value l1328 = v_l;
    while (u_is_con(l1328, &n_list_46_Cons)) {
      u_value v_t = u_field(l1328, 0);
      if (u_truth(u_mk_bool(1))) {
        e1327 = u_append(e1327, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1328 = u_field(l1328, 1);
    }
    t1332 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1326, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1329 = U_LIT("*");
    u_value t1331;
    if (u_truth(u_eq(v_op, &l__42__1329))) {
      static struct u_object l__40_product_41__1330 = U_LIT("(product)");
      t1331 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1330), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1331 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1332 = t1331;
  }
  return t1332;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1333 = v_t;
  u_value t1334 = NULL;
  if ((!t1334) && (u_is_con(s1333, &n_syntax_46_TSort) && (u_is_con(u_field(s1333, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1333, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1333, 0), 1), u_mk_lit("(invalid)")))))) {
    t1334 = u_mk_bool(1);
  }
  if ((!t1334) && u_is_con(s1333, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1333, 1);
    t1334 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1334) && u_is_con(s1333, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1333, 0);
    u_value v_r = u_field(s1333, 1);
    u_value t1335 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1335)) {
      t1335 = f_parse_46_has_invalid_1(v_r);
    }
    t1334 = t1335;
  }
  if ((!t1334) && u_is_con(s1333, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1333, 0);
    t1334 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1334) {
    t1334 = u_mk_bool(0);
  }
  if (!t1334) {
    u_no_case(s1333);
  }
  return t1334;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1336 = v_ts;
  u_value t1337 = NULL;
  if ((!t1337) && u_is_con(s1336, &n_list_46_Nil)) {
    t1337 = u_mk_bool(0);
  }
  if ((!t1337) && u_is_con(s1336, &n_list_46_Cons)) {
    u_value v_t = u_field(s1336, 0);
    u_value v_more = u_field(s1336, 1);
    u_value t1338 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1338)) {
      t1338 = f_parse_46_any_invalid_1(v_more);
    }
    t1337 = t1338;
  }
  if (!t1337) {
    u_no_case(s1336);
  }
  return t1337;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1339 = v_t;
  u_value t1340 = NULL;
  if ((!t1340) && u_is_con(s1339, &n_lex_46_TName)) {
    u_value v_n = u_field(s1339, 0);
    u_value t1341 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1341)) {
      t1341 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1340 = t1341;
  }
  if ((!t1340) && (u_is_con(s1339, &n_lex_46_TPunct) && u_equals(u_field(s1339, 0), u_mk_char(40)))) {
    t1340 = u_mk_bool(1);
  }
  if (!t1340) {
    t1340 = u_mk_bool(0);
  }
  if (!t1340) {
    u_no_case(s1339);
  }
  return t1340;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1342 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1342;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1343 = f_parse_46_type_one_1(v_r);
    *r_t = t1343;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1344 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1344, v_start), "parse.ParseError");
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
    u_value t1345 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1345;
    u_value t1346 = u_mk_bool(1);
    v_next = t1346;
    u_value t1347 = u_mk_bool(1);
    v_go = t1347;
    u_value t1356 = v_go;
    while (u_truth(t1356)) {
      {
        u_value s1348 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1349 = 0;
        if ((!d1349) && u_is_con(s1348, &n_option_46_Some)) {
          u_value v_op = u_field(s1348, 0);
          d1349 = 1;
          {
            u_value t1350 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1351 = u_mk_bool(1);
            v_rev = t1350;
            v_next = t1351;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1349) && u_is_con(s1348, &n_option_46_None)) {
          u_value t1354 = v_next;
          if (u_truth(t1354)) {
            t1354 = f_parse_46_starts_type_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1354)) {
            d1349 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1352 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1353 = u_mk_bool(0);
              v_rev = t1352;
              v_next = t1353;
            }
          }
        }
        if (!d1349) {
          d1349 = 1;
          {
            u_value t1355 = u_mk_bool(0);
            v_go = t1355;
          }
        }
        if (!d1349) {
          u_no_case(s1348);
        }
      }
      t1356 = v_go;
    }
    u_value s1357 = v_rev;
    int d1358 = 0;
    if ((!d1358) && u_is_con(s1357, &n_list_46_Nil)) {
      d1358 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1359 = U_LIT("type expected but ");
        static struct u_object l__32_found_1360 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1359, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1360)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1358) {
      d1358 = 1;
      {
        u_value t1361 = f_list_46_reverse_1(v_rev);
        *r_items = t1361;
      }
    }
    if (!d1358) {
      u_no_case(s1357);
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
    u_value t1365 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (u_truth(t1365)) {
      static struct u_object l_in_1362 = U_LIT("in");
      static struct u_object l_out_1363 = U_LIT("out");
      static struct u_object l_ref_1364 = U_LIT("ref");
      t1365 = f_list_46_contains_2(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_in_1362), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_out_1363), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_ref_1364), u_mk_con(&n_list_46_Nil, 0)))));
    }
    if (u_truth(t1365)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_sections(u_mk_proc(wp_parse_46_parse_type, 0), u_mk_proc(wp_parse_46_parse_type, 0), v_sc, &v_ins, &v_outs, &v_refs, r_ts);
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
        u_value t1366 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TProc, 3, v_ins, v_outs, v_refs), u_mk_con(&n_list_46_Nil, 0));
        *r_r = t1366;
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
              u_value t1367 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1367;
            }
          } else {
            if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Scope_46_params_1(v_sc)))) {
              {
                u_value t1368 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
                *r_r = t1368;
              }
            } else {
              {
                u_value t1369 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
                *r_r = t1369;
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
    u_value t1370 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1370;
    u_value t1371 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1371)) {
      t1371 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1371)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1372 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1372;
        u_value t1374 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1374)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1373 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1373;
          }
          t1374 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1375 = f_list_46_reverse_1(v_rev);
    *r_r = t1375;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_prefix_2(u_value v_op, u_value v_a) {
  u_value s1376 = v_a;
  u_value t1377 = NULL;
  if ((!t1377) && (u_is_con(s1376, &n_syntax_46_ELit) && u_is_con(u_field(s1376, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1376, 0), 0);
    static struct u_object l__45__1378 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1378))) {
      t1377 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1377) {
    t1377 = f_parse_46_expr_op1_2(v_op, v_a);
  }
  if (!t1377) {
    u_no_case(s1376);
  }
  return t1377;
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1379 = v_es;
  u_value t1380 = NULL;
  if ((!t1380) && u_is_con(s1379, &n_list_46_Nil)) {
    t1380 = v_rest;
  }
  if ((!t1380) && u_is_con(s1379, &n_list_46_Cons)) {
    u_value v_e = u_field(s1379, 0);
    u_value v_more = u_field(s1379, 1);
    static struct u_object l_Cons_1381 = U_LIT("Cons");
    t1380 = f_parse_46_expr_op2_3(&l_Cons_1381, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1380) {
    u_no_case(s1379);
  }
  return t1380;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1382 = v_t;
  u_value t1383 = NULL;
  if ((!t1383) && u_is_con(s1382, &n_lex_46_TName)) {
    u_value v_n = u_field(s1382, 0);
    static struct u_object l_let_1384 = U_LIT("let");
    static struct u_object l_if_1385 = U_LIT("if");
    static struct u_object l_lambda_1386 = U_LIT("lambda");
    static struct u_object l_match_1387 = U_LIT("match");
    static struct u_object l_true_1388 = U_LIT("true");
    static struct u_object l_false_1389 = U_LIT("false");
    u_value t1391 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1384, u_mk_con(&n_list_46_Cons, 2, &l_if_1385, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1386, u_mk_con(&n_list_46_Cons, 2, &l_match_1387, u_mk_con(&n_list_46_Cons, 2, &l_true_1388, u_mk_con(&n_list_46_Cons, 2, &l_false_1389, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1391)) {
      u_value t1390 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1390)) {
        t1390 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1391 = t1390;
    }
    t1383 = t1391;
  }
  if ((!t1383) && u_is_con(s1382, &n_lex_46_TNum)) {
    t1383 = u_mk_bool(1);
  }
  if ((!t1383) && u_is_con(s1382, &n_lex_46_TStr)) {
    t1383 = u_mk_bool(1);
  }
  if ((!t1383) && u_is_con(s1382, &n_lex_46_TChar)) {
    t1383 = u_mk_bool(1);
  }
  if ((!t1383) && u_is_con(s1382, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1382, 0);
    u_value t1392 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1392)) {
      t1392 = u_eq(v_c, u_mk_char(91));
    }
    u_value t1393 = t1392;
    if (!u_truth(t1393)) {
      t1393 = u_eq(v_c, u_mk_char(123));
    }
    t1383 = t1393;
  }
  if (!t1383) {
    t1383 = u_mk_bool(0);
  }
  if (!t1383) {
    u_no_case(s1382);
  }
  return t1383;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    static struct u_object l_raise_1394 = U_LIT("raise");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_raise_1394))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, r_e, r_ts);
        u_value t1395 = u_mk_con(&n_syntax_46_ERaise, 2, *r_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_e = t1395;
      }
    } else {
      {
        p_parse_46_operators(v_sc, r_e, r_ts);
        static struct u_object l_with_1396 = U_LIT("with");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_with_1396))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_update(v_sc, r_e, r_ts);
            u_value t1397 = f_parse_46_at_update_1(*r_ts);
            while (u_truth(t1397)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_update(v_sc, r_e, r_ts);
              }
              t1397 = f_parse_46_at_update_1(*r_ts);
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
    static struct u_object l__58__61__1398 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1398), r_ts);
    p_parse_46_operators(v_sc, &v_a, r_ts);
    static struct u_object l__46__1399 = U_LIT(".");
    static struct u_object l__58__61__1400 = U_LIT(":=");
    u_value t1401 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1399, u_concat(v_f, &l__58__61__1400))), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1401;
  }
}

u_value f_parse_46_at_update_1(u_value v_ts) {
  u_value s1402 = v_ts;
  u_value t1403 = NULL;
  if ((!t1403) && (u_is_con(s1402, &n_list_46_Cons) && (u_is_con(u_field(u_field(s1402, 0), 0), &n_lex_46_TPunct) && (u_equals(u_field(u_field(u_field(s1402, 0), 0), 0), u_mk_char(44)) && (u_is_con(u_field(s1402, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s1402, 1), 0), 0), &n_lex_46_TName) && (u_is_con(u_field(u_field(s1402, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(u_field(s1402, 1), 1), 0), 0), &n_lex_46_TSym) && u_equals(u_field(u_field(u_field(u_field(u_field(s1402, 1), 1), 0), 0), 0), u_mk_lit(":=")))))))))) {
    t1403 = u_mk_bool(1);
  }
  if (!t1403) {
    t1403 = u_mk_bool(0);
  }
  if (!t1403) {
    u_no_case(s1402);
  }
  return t1403;
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
    u_value t1404 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1404;
    u_value t1405 = u_mk_bool(1);
    v_next = t1405;
    u_value t1406 = u_mk_bool(1);
    v_go = t1406;
    u_value t1415 = v_go;
    while (u_truth(t1415)) {
      {
        u_value s1407 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1408 = 0;
        if ((!d1408) && u_is_con(s1407, &n_option_46_Some)) {
          u_value v_op = u_field(s1407, 0);
          d1408 = 1;
          {
            u_value t1409 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1410 = u_mk_bool(1);
            v_rev = t1409;
            v_next = t1410;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1408) && u_is_con(s1407, &n_option_46_None)) {
          u_value t1413 = v_next;
          if (u_truth(t1413)) {
            t1413 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1413)) {
            d1408 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1411 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1412 = u_mk_bool(0);
              v_rev = t1411;
              v_next = t1412;
            }
          }
        }
        if (!d1408) {
          d1408 = 1;
          {
            u_value t1414 = u_mk_bool(0);
            v_go = t1414;
          }
        }
        if (!d1408) {
          u_no_case(s1407);
        }
      }
      t1415 = v_go;
    }
    u_value s1416 = v_rev;
    int d1417 = 0;
    if ((!d1417) && u_is_con(s1416, &n_list_46_Nil)) {
      d1417 = 1;
      {
        static struct u_object l_expression_32_expec_1418 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1419 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1418, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1419)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1417) {
      d1417 = 1;
      {
        u_value t1420 = f_list_46_reverse_1(v_rev);
        *r_items = t1420;
      }
    }
    if (!d1417) {
      u_no_case(s1416);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1421 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1421;
    u_value t1423 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1423)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1422 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1422;
      }
      t1423 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1424 = f_list_46_reverse_1(v_rev);
    *r_es = t1424;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1425 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1425;
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
    u_value t1426 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1426)) {
      t1426 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1430 = t1426;
    while (u_truth(t1430)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1427 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1427;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1428 = U_LIT(".");
            u_value t1429 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1428, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1429;
          }
        }
      }
      u_value t1431 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1431)) {
        t1431 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1430 = t1431;
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
    u_value s1432 = f_parse_46_head_token_1(*r_ts);
    int d1433 = 0;
    if ((!d1433) && (u_is_con(s1432, &n_lex_46_TName) && u_equals(u_field(s1432, 0), u_mk_lit("let")))) {
      d1433 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1433) && (u_is_con(s1432, &n_lex_46_TName) && u_equals(u_field(s1432, 0), u_mk_lit("if")))) {
      d1433 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1434 = U_LIT("then");
        p_parse_46_keyword(&l_then_1434, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1435 = U_LIT("else");
        p_parse_46_keyword(&l_else_1435, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1436 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1436;
      }
    }
    if ((!d1433) && (u_is_con(s1432, &n_lex_46_TName) && u_equals(u_field(s1432, 0), u_mk_lit("lambda")))) {
      d1433 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1433) && (u_is_con(s1432, &n_lex_46_TName) && u_equals(u_field(s1432, 0), u_mk_lit("match")))) {
      d1433 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1437 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1437;
      }
    }
    if ((!d1433) && (u_is_con(s1432, &n_lex_46_TName) && u_equals(u_field(s1432, 0), u_mk_lit("true")))) {
      d1433 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1438 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1438;
      }
    }
    if ((!d1433) && (u_is_con(s1432, &n_lex_46_TName) && u_equals(u_field(s1432, 0), u_mk_lit("false")))) {
      d1433 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1439 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1439;
      }
    }
    if ((!d1433) && u_is_con(s1432, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1432, 0);
      d1433 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1440 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1440;
      }
    }
    if ((!d1433) && u_is_con(s1432, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1432, 0);
      d1433 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1441 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1441;
      }
    }
    if ((!d1433) && u_is_con(s1432, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1432, 0);
      d1433 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1442 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1442;
      }
    }
    if ((!d1433) && (u_is_con(s1432, &n_lex_46_TPunct) && u_equals(u_field(s1432, 0), u_mk_char(40)))) {
      d1433 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1443 = v_es;
        int d1444 = 0;
        if ((!d1444) && (u_is_con(s1443, &n_list_46_Cons) && u_is_con(u_field(s1443, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1443, 0);
          d1444 = 1;
          {
            u_value t1445 = v_x;
            *r_e = t1445;
          }
        }
        if (!d1444) {
          d1444 = 1;
          {
            u_value t1446 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1446;
          }
        }
        if (!d1444) {
          u_no_case(s1443);
        }
      }
    }
    if ((!d1433) && (u_is_con(s1432, &n_lex_46_TPunct) && u_equals(u_field(s1432, 0), u_mk_char(91)))) {
      d1433 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1433) && (u_is_con(s1432, &n_lex_46_TPunct) && u_equals(u_field(s1432, 0), u_mk_char(123)))) {
      d1433 = 1;
      {
        p_parse_46_parse_braces(v_sc, r_e, r_ts);
      }
    }
    if ((!d1433) && u_is_con(s1432, &n_lex_46_TName)) {
      u_value v_n = u_field(s1432, 0);
      u_value t1448 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1448)) {
        t1448 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t1448)) {
        d1433 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1447 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1447;
        }
      }
    }
    if (!d1433) {
      d1433 = 1;
      {
        static struct u_object l_expression_32_expec_1449 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1450 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1449, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1450)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1433) {
      u_no_case(s1432);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1451 = v_ps;
  u_value t1452 = NULL;
  if ((!t1452) && u_is_con(s1451, &n_list_46_Nil)) {
    t1452 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1452) && (u_is_con(s1451, &n_list_46_Cons) && u_is_con(u_field(s1451, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1451, 0), 0);
    u_value v_more = u_field(s1451, 1);
    u_value s1453 = f_parse_46_pattern_vars_1(v_more);
    u_value t1454 = NULL;
    if ((!t1454) && u_is_con(s1453, &n_option_46_Some)) {
      u_value v_xs = u_field(s1453, 0);
      t1454 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1454) && u_is_con(s1453, &n_option_46_None)) {
      t1454 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1454) {
      u_no_case(s1453);
    }
    t1452 = t1454;
  }
  if (!t1452) {
    t1452 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1452) {
    u_no_case(s1451);
  }
  return t1452;
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
    static struct u_object l__58__61__1455 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1455), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1456 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1457 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1456;
    v_es = t1457;
    u_value t1461 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1461)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1458 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1458), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1459 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1460 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1459;
        v_es = t1460;
      }
      t1461 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1462 = U_LIT("in");
    p_parse_46_keyword(&l_in_1462, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1463 = f_list_46_reverse_1(v_ps);
    u_value t1464 = f_list_46_reverse_1(v_es);
    v_ps = t1463;
    v_es = t1464;
    u_value s1465 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1466 = 0;
    if ((!d1466) && u_is_con(u_field(s1465, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1465, 0), 0);
      d1466 = 1;
      {
        u_value t1467 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1467;
      }
    }
    if ((!d1466) && (u_is_con(u_field(s1465, 0), &n_option_46_None) && (u_is_con(u_field(s1465, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1465, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1465, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1465, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1465, 1), 0);
      u_value v_c = u_field(u_field(s1465, 2), 0);
      d1466 = 1;
      {
        u_value t1468 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1468;
      }
    }
    if (!d1466) {
      d1466 = 1;
      {
        u_value t1469 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1469;
      }
    }
    if (!d1466) {
      u_no_case(s1465);
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
        static struct u_object l__61__62__1470 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1470), r_ts);
        p_parse_46_parse_body(v_sc, &v_body, r_ts);
        static struct u_object l_end_1471 = U_LIT("end");
        p_parse_46_keyword(&l_end_1471, r_ts);
        u_value t1472 = u_mk_con(&n_syntax_46_EProc, 4, v_ins, v_outs, v_refs, v_body);
        *r_e = t1472;
      }
    } else {
      {
        p_parse_46_lambda_function(v_sc, r_e, r_ts);
      }
    }
  }
}

u_value f_parse_46_at_section_1(u_value v_ts) {
  static struct u_object l_in_1473 = U_LIT("in");
  u_value t1475 = f_parse_46_at_keyword_2(v_ts, &l_in_1473);
  if (!u_truth(t1475)) {
    static struct u_object l_out_1474 = U_LIT("out");
    t1475 = f_parse_46_at_keyword_2(v_ts, &l_out_1474);
  }
  u_value t1477 = t1475;
  if (!u_truth(t1477)) {
    static struct u_object l_ref_1476 = U_LIT("ref");
    t1477 = f_parse_46_at_keyword_2(v_ts, &l_ref_1476);
  }
  return t1477;
}

void p_parse_46_lambda_function(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    u_value t1478 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1478;
    static struct u_object l__61__62__1479 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1479)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1480 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1480;
        u_value t1482 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1482)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1481 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1481;
          }
          t1482 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1483 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1483), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1484 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1484;
  }
}

void p_parse_46_parse_braces(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_at = NULL;
    u_value v_k = NULL;
    u_value v_v = NULL;
    u_value v_entries = NULL;
    u_value v_rev = NULL;
    u_value t1485 = f_parse_46_head_offset_1(*r_ts);
    v_at = t1485;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_expr(v_sc, &v_k, r_ts);
    static struct u_object l__61__62__1486 = U_LIT("=>");
    u_value t1487 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1486));
    v_entries = t1487;
    if (u_truth(v_entries)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_v, r_ts);
      }
    }
    static struct u_object l_for_1488 = U_LIT("for");
    u_value t1489 = f_parse_46_at_keyword_2(*r_ts, &l_for_1488);
    if (u_truth(t1489)) {
      t1489 = v_entries;
    }
    if (u_truth(t1489)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_comprehension(v_sc, v_k, v_v, r_e, r_ts);
      }
    } else {
      static struct u_object l_for_1490 = U_LIT("for");
      if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1490))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_set_comprehension(v_sc, v_k, r_e, r_ts);
        }
      } else {
        {
          u_value t1491;
          if (u_truth(v_entries)) {
            t1491 = u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))));
          } else {
            t1491 = v_k;
          }
          u_value t1492 = u_mk_con(&n_list_46_Cons, 2, t1491, u_mk_con(&n_list_46_Nil, 0));
          v_rev = t1492;
          u_value t1497 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          while (u_truth(t1497)) {
            {
              p_parse_46_advance(r_ts);
              p_parse_46_parse_expr(v_sc, &v_k, r_ts);
              static struct u_object l__61__62__1493 = U_LIT("=>");
              if (u_truth(f_prelude_46__33__61__2(v_entries, u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1493))))) {
                {
                  static struct u_object l_a_32_literal_32__123__46__46__46__125__32__1494 = U_LIT("a literal {...} has either elements a or entries k => v");
                  u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_a_32_literal_32__123__46__46__46__125__32__1494, v_at), "parse.ParseError");
                }
              }
              if (u_truth(v_entries)) {
                {
                  p_parse_46_advance(r_ts);
                  p_parse_46_parse_expr(v_sc, &v_v, r_ts);
                  u_value t1495 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)))), v_rev);
                  v_rev = t1495;
                }
              } else {
                {
                  u_value t1496 = u_mk_con(&n_list_46_Cons, 2, v_k, v_rev);
                  v_rev = t1496;
                }
              }
            }
            t1497 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          }
          u_value t1504;
          if (u_truth(v_entries)) {
            static struct u_object l_map_1498 = U_LIT("map");
            static struct u_object l_put_all_1499 = U_LIT("put_all");
            static struct u_object l_map_1500 = U_LIT("map");
            static struct u_object l_empty_1501 = U_LIT("empty");
            t1504 = f_parse_46_in_library_3(&l_map_1498, &l_put_all_1499, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1500, &l_empty_1501, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
          } else {
            static struct u_object l_set_1502 = U_LIT("set");
            static struct u_object l_from_list_1503 = U_LIT("from_list");
            t1504 = f_parse_46_in_library_3(&l_set_1502, &l_from_list_1503, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Nil, 0)));
          }
          u_value t1505 = t1504;
          *r_e = t1505;
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
  static struct u_object l_Nil_1506 = U_LIT("Nil");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1506), u_mk_con(&n_syntax_46_THole, 0)));
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
    u_value t1507 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1507;
    u_value t1508 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1508;
    u_value t1509 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1509;
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l_in_1510 = U_LIT("in");
    p_parse_46_keyword(&l_in_1510, r_ts);
    p_parse_46_parse_expr(v_sc, &v_l, r_ts);
    static struct u_object l_set_1511 = U_LIT("set");
    static struct u_object l_elements_1512 = U_LIT("elements");
    u_value t1513 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    u_value t1514 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1511, &l_elements_1512, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
    v_xs = t1513;
    v_ls = t1514;
    u_value t1520 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1520)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l_in_1515 = U_LIT("in");
        p_parse_46_keyword(&l_in_1515, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_set_1516 = U_LIT("set");
        static struct u_object l_elements_1517 = U_LIT("elements");
        u_value t1518 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
        u_value t1519 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1516, &l_elements_1517, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), v_ls);
        v_xs = t1518;
        v_ls = t1519;
      }
      t1520 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1521 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1521))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
      }
    }
    static struct u_object l_set_1522 = U_LIT("set");
    static struct u_object l_from_list_1523 = U_LIT("from_list");
    u_value t1524 = f_parse_46_in_library_3(&l_set_1522, &l_from_list_1523, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, v_r, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Nil, 0)));
    *r_e = t1524;
  }
}

void p_parse_46_map_comprehension(u_value v_sc, u_value v_k, u_value v_v, u_value *r_e, u_value *r_ts) {
  {
    u_value v_gs = NULL;
    u_value v_ls = NULL;
    u_value v_c = NULL;
    u_value t1525 = u_mk_con(&n_list_46_Nil, 0);
    v_gs = t1525;
    u_value t1526 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1526;
    u_value t1527 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1527;
    p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
    u_value t1528 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1528)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
      }
      t1528 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1529 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1529))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
        u_value t1530 = f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), v_c);
        v_c = t1530;
      }
    }
    static struct u_object l_map_1531 = U_LIT("map");
    static struct u_object l_put_all_1532 = U_LIT("put_all");
    static struct u_object l_map_1533 = U_LIT("map");
    static struct u_object l_empty_1534 = U_LIT("empty");
    u_value t1535 = f_parse_46_in_library_3(&l_map_1531, &l_put_all_1532, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))))), f_list_46_reverse_1(f_parse_46_hidden_vars_1(v_gs)), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1533, &l_empty_1534, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1535;
  }
}

void p_parse_46_map_generator(u_value v_sc, u_value *r_gs, u_value *r_ls, u_value *r_ts) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value v_m = NULL;
    p_parse_46_parse_plain(v_sc, &v_a, r_ts);
    static struct u_object l__61__62__1536 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1536), r_ts);
    p_parse_46_parse_plain(v_sc, &v_b, r_ts);
    static struct u_object l_in_1537 = U_LIT("in");
    p_parse_46_keyword(&l_in_1537, r_ts);
    p_parse_46_parse_expr(v_sc, &v_m, r_ts);
    static struct u_object l_map_1538 = U_LIT("map");
    static struct u_object l_entries_1539 = U_LIT("entries");
    u_value t1540 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1538, &l_entries_1539, u_mk_con(&n_list_46_Cons, 2, f_parse_46_unpack_2(f_list_46_reverse_1(*r_gs), v_m), u_mk_con(&n_list_46_Nil, 0))), *r_ls);
    *r_ls = t1540;
    static struct u_object l__40_entry_32__1541 = U_LIT("(entry ");
    static struct u_object l__41__1542 = U_LIT(")");
    u_value t1543 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(3, v_a, v_b, u_concat(&l__40_entry_32__1541, u_concat(u_str_show(u_add(f_list_46_length_1(*r_gs), u_mk_int(1))), &l__41__1542))), *r_gs);
    *r_gs = t1543;
  }
}

u_value f_parse_46_hidden_vars_1(u_value v_gs) {
  u_value s1544 = v_gs;
  u_value t1545 = NULL;
  if ((!t1545) && u_is_con(s1544, &n_list_46_Nil)) {
    t1545 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1545) && u_is_con(s1544, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s1544, 0), 2);
    u_value v_rest = u_field(s1544, 1);
    t1545 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0)), f_parse_46_hidden_vars_1(v_rest));
  }
  if (!t1545) {
    u_no_case(s1544);
  }
  return t1545;
}

u_value f_parse_46_unpack_2(u_value v_gs, u_value v_e) {
  u_value s1546 = v_gs;
  u_value t1547 = NULL;
  if ((!t1547) && u_is_con(s1546, &n_list_46_Nil)) {
    t1547 = v_e;
  }
  if ((!t1547) && u_is_con(s1546, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s1546, 0), 0);
    u_value v_v = u_field(u_field(s1546, 0), 1);
    u_value v_h = u_field(u_field(s1546, 0), 2);
    u_value v_rest = u_field(s1546, 1);
    t1547 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_k), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_v), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_parse_46_unpack_2(v_rest, v_e)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1547) {
    u_no_case(s1546);
  }
  return t1547;
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
        static struct u_object l_Nil_1548 = U_LIT("Nil");
        u_value t1549 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1548), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1549;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1550 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1550))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1551 = U_LIT("in");
            p_parse_46_keyword(&l_in_1551, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1552 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1553 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1552;
            v_lists = t1553;
            u_value t1557 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1557)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1554 = U_LIT("in");
                p_parse_46_keyword(&l_in_1554, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1555 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1556 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1555;
                v_lists = t1556;
              }
              t1557 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1558 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1558;
            static struct u_object l_if_1559 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1559))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1560 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1560;
          }
        } else {
          {
            u_value t1561 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1561;
            u_value t1563 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1563)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1562 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1562;
              }
              t1563 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1564 = U_LIT("Nil");
            u_value t1565 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1564), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1565;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1566 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1566;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1567 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1567))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1568 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1568;
      }
    } else {
      {
        u_value t1569 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1569;
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
    u_value t1570 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1570;
    static struct u_object l_case_1571 = U_LIT("case");
    u_value t1574 = f_parse_46_at_keyword_2(*r_ts, &l_case_1571);
    while (u_truth(t1574)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1572 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1572), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1573 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1573;
      }
      static struct u_object l_case_1575 = U_LIT("case");
      t1574 = f_parse_46_at_keyword_2(*r_ts, &l_case_1575);
    }
    static struct u_object l_else_1576 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1576))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1577 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1577;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1578 = U_LIT("end");
    p_parse_46_keyword(&l_end_1578, r_ts);
    u_value t1579 = f_list_46_reverse_1(v_rev);
    *r_cs = t1579;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1580 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1580))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1581 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1581, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
  }
}

u_value f_parse_46_pat_op1_2(u_value v_op, u_value v_p) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_pat_prefix_2(u_value v_op, u_value v_p) {
  u_value s1582 = v_p;
  u_value t1583 = NULL;
  if ((!t1583) && (u_is_con(s1582, &n_syntax_46_PLit) && u_is_con(u_field(s1582, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1582, 0), 0);
    static struct u_object l__45__1584 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1584))) {
      t1583 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1583) {
    t1583 = f_parse_46_pat_op1_2(v_op, v_p);
  }
  if (!t1583) {
    u_no_case(s1582);
  }
  return t1583;
}

u_value f_parse_46_pat_op2_3(u_value v_op, u_value v_l, u_value v_r) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_pats_2(u_value v_ps, u_value v_rest) {
  u_value s1585 = v_ps;
  u_value t1586 = NULL;
  if ((!t1586) && u_is_con(s1585, &n_list_46_Nil)) {
    t1586 = v_rest;
  }
  if ((!t1586) && u_is_con(s1585, &n_list_46_Cons)) {
    u_value v_p = u_field(s1585, 0);
    u_value v_more = u_field(s1585, 1);
    static struct u_object l_Cons_1587 = U_LIT("Cons");
    t1586 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1587), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1586) {
    u_no_case(s1585);
  }
  return t1586;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1588 = v_t;
  u_value t1589 = NULL;
  if ((!t1589) && u_is_con(s1588, &n_lex_46_TName)) {
    u_value v_n = u_field(s1588, 0);
    static struct u_object l___1590 = U_LIT("_");
    static struct u_object l_true_1591 = U_LIT("true");
    static struct u_object l_false_1592 = U_LIT("false");
    u_value t1594 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1590, u_mk_con(&n_list_46_Cons, 2, &l_true_1591, u_mk_con(&n_list_46_Cons, 2, &l_false_1592, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1594)) {
      u_value t1593 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1593)) {
        t1593 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1594 = t1593;
    }
    t1589 = t1594;
  }
  if ((!t1589) && u_is_con(s1588, &n_lex_46_TNum)) {
    t1589 = u_mk_bool(1);
  }
  if ((!t1589) && u_is_con(s1588, &n_lex_46_TStr)) {
    t1589 = u_mk_bool(1);
  }
  if ((!t1589) && u_is_con(s1588, &n_lex_46_TChar)) {
    t1589 = u_mk_bool(1);
  }
  if ((!t1589) && u_is_con(s1588, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1588, 0);
    u_value t1595 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1595)) {
      t1595 = u_eq(v_c, u_mk_char(91));
    }
    t1589 = t1595;
  }
  if (!t1589) {
    t1589 = u_mk_bool(0);
  }
  if (!t1589) {
    u_no_case(s1588);
  }
  return t1589;
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
    u_value t1596 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1596;
    u_value t1597 = u_mk_bool(1);
    v_next = t1597;
    u_value t1598 = u_mk_bool(1);
    v_go = t1598;
    u_value t1607 = v_go;
    while (u_truth(t1607)) {
      {
        u_value s1599 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1600 = 0;
        if ((!d1600) && u_is_con(s1599, &n_option_46_Some)) {
          u_value v_op = u_field(s1599, 0);
          d1600 = 1;
          {
            u_value t1601 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1602 = u_mk_bool(1);
            v_rev = t1601;
            v_next = t1602;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1600) && u_is_con(s1599, &n_option_46_None)) {
          u_value t1605 = v_next;
          if (u_truth(t1605)) {
            t1605 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1605)) {
            d1600 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1603 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1604 = u_mk_bool(0);
              v_rev = t1603;
              v_next = t1604;
            }
          }
        }
        if (!d1600) {
          d1600 = 1;
          {
            u_value t1606 = u_mk_bool(0);
            v_go = t1606;
          }
        }
        if (!d1600) {
          u_no_case(s1599);
        }
      }
      t1607 = v_go;
    }
    u_value s1608 = v_rev;
    int d1609 = 0;
    if ((!d1609) && u_is_con(s1608, &n_list_46_Nil)) {
      d1609 = 1;
      {
        static struct u_object l_pattern_32_expected_1610 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1611 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1610, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1611)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1609) {
      d1609 = 1;
      {
        u_value t1612 = f_list_46_reverse_1(v_rev);
        *r_items = t1612;
      }
    }
    if (!d1609) {
      u_no_case(s1608);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1613 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1613;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1614 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1614;
        u_value t1616 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1616)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1615 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1615;
          }
          t1616 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1617 = f_list_46_reverse_1(v_rev);
    *r_ps = t1617;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1618 = f_parse_46_head_token_1(*r_ts);
    int d1619 = 0;
    if ((!d1619) && (u_is_con(s1618, &n_lex_46_TName) && u_equals(u_field(s1618, 0), u_mk_lit("_")))) {
      d1619 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1620 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1620;
      }
    }
    if ((!d1619) && (u_is_con(s1618, &n_lex_46_TName) && u_equals(u_field(s1618, 0), u_mk_lit("true")))) {
      d1619 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1621 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1621;
      }
    }
    if ((!d1619) && (u_is_con(s1618, &n_lex_46_TName) && u_equals(u_field(s1618, 0), u_mk_lit("false")))) {
      d1619 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1622 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1622;
      }
    }
    if ((!d1619) && u_is_con(s1618, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1618, 0);
      d1619 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1623 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1623;
      }
    }
    if ((!d1619) && u_is_con(s1618, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1618, 0);
      d1619 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1624 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1624;
      }
    }
    if ((!d1619) && u_is_con(s1618, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1618, 0);
      d1619 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1625 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1625;
      }
    }
    if ((!d1619) && (u_is_con(s1618, &n_lex_46_TPunct) && u_equals(u_field(s1618, 0), u_mk_char(40)))) {
      d1619 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1626 = v_ps;
        int d1627 = 0;
        if ((!d1627) && (u_is_con(s1626, &n_list_46_Cons) && u_is_con(u_field(s1626, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1626, 0);
          d1627 = 1;
          {
            u_value t1628 = v_q;
            *r_p = t1628;
          }
        }
        if (!d1627) {
          d1627 = 1;
          {
            u_value t1629 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1629;
          }
        }
        if (!d1627) {
          u_no_case(s1626);
        }
      }
    }
    if ((!d1619) && (u_is_con(s1618, &n_lex_46_TPunct) && u_equals(u_field(s1618, 0), u_mk_char(91)))) {
      d1619 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1630 = U_LIT("Nil");
            u_value t1631 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1630), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1631;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1632 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1632;
            u_value t1634 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1634)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1633 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1633;
              }
              t1634 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1635 = U_LIT("Nil");
            u_value t1636 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1635), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1636;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1637 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1637;
          }
        }
      }
    }
    if (!d1619) {
      d1619 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1638 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1638;
          }
        } else {
          {
            u_value t1639 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1639;
          }
        }
      }
    }
    if (!d1619) {
      u_no_case(s1618);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1640 = v_t;
  u_value t1641 = NULL;
  if ((!t1641) && u_is_con(s1640, &n_lex_46_TName)) {
    u_value v_n = u_field(s1640, 0);
    static struct u_object l_skip_1642 = U_LIT("skip");
    static struct u_object l_if_1643 = U_LIT("if");
    static struct u_object l_while_1644 = U_LIT("while");
    static struct u_object l_call_1645 = U_LIT("call");
    static struct u_object l_match_1646 = U_LIT("match");
    static struct u_object l_assert_1647 = U_LIT("assert");
    static struct u_object l_assign_1648 = U_LIT("assign");
    static struct u_object l_raise_1649 = U_LIT("raise");
    static struct u_object l_try_1650 = U_LIT("try");
    t1641 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1642, u_mk_con(&n_list_46_Cons, 2, &l_if_1643, u_mk_con(&n_list_46_Cons, 2, &l_while_1644, u_mk_con(&n_list_46_Cons, 2, &l_call_1645, u_mk_con(&n_list_46_Cons, 2, &l_match_1646, u_mk_con(&n_list_46_Cons, 2, &l_assert_1647, u_mk_con(&n_list_46_Cons, 2, &l_assign_1648, u_mk_con(&n_list_46_Cons, 2, &l_raise_1649, u_mk_con(&n_list_46_Cons, 2, &l_try_1650, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1641) {
    t1641 = u_mk_bool(0);
  }
  if (!t1641) {
    u_no_case(s1640);
  }
  return t1641;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1651 = v_cs;
  u_value t1652 = NULL;
  if ((!t1652) && u_is_con(s1651, &n_list_46_Nil)) {
    t1652 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1652) && (u_is_con(s1651, &n_list_46_Cons) && u_is_con(u_field(s1651, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1651, 0), 0);
    u_value v_g = u_field(u_field(s1651, 0), 1);
    u_value v_b = u_field(u_field(s1651, 0), 2);
    u_value v_more = u_field(s1651, 1);
    t1652 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1652) {
    u_no_case(s1651);
  }
  return t1652;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1653 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1653;
    u_value t1654 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1654;
    u_value t1655 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1655;
    static struct u_object l_local_1656 = U_LIT("local");
    u_value t1658 = f_parse_46_at_keyword_2(*r_ts, &l_local_1656);
    while (u_truth(t1658)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1657 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1657)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1657 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1659 = U_LIT("local");
      t1658 = f_parse_46_at_keyword_2(*r_ts, &l_local_1659);
    }
    u_value t1661 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1661)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1660 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1660;
      }
      t1661 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1662 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1662;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1663 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1663;
    static struct u_object l__58__1664 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1664)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__1665 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1665)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1666 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1666;
      }
    }
    u_value t1667 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1667;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1668 = f_parse_46_head_token_1(*r_ts);
    int d1669 = 0;
    if ((!d1669) && (u_is_con(s1668, &n_lex_46_TName) && u_equals(u_field(s1668, 0), u_mk_lit("skip")))) {
      d1669 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1670 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1670;
      }
    }
    if ((!d1669) && (u_is_con(s1668, &n_lex_46_TName) && u_equals(u_field(s1668, 0), u_mk_lit("if")))) {
      d1669 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1669) && (u_is_con(s1668, &n_lex_46_TName) && u_equals(u_field(s1668, 0), u_mk_lit("while")))) {
      d1669 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1671 = U_LIT("end");
        p_parse_46_keyword(&l_end_1671, r_ts);
        u_value t1672 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1672;
      }
    }
    if ((!d1669) && (u_is_con(s1668, &n_lex_46_TName) && u_equals(u_field(s1668, 0), u_mk_lit("call")))) {
      d1669 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1669) && (u_is_con(s1668, &n_lex_46_TName) && u_equals(u_field(s1668, 0), u_mk_lit("match")))) {
      d1669 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1673 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1673;
      }
    }
    if ((!d1669) && (u_is_con(s1668, &n_lex_46_TName) && u_equals(u_field(s1668, 0), u_mk_lit("assert")))) {
      d1669 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1674 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1674;
      }
    }
    if ((!d1669) && (u_is_con(s1668, &n_lex_46_TName) && u_equals(u_field(s1668, 0), u_mk_lit("assign")))) {
      d1669 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1669) && (u_is_con(s1668, &n_lex_46_TName) && u_equals(u_field(s1668, 0), u_mk_lit("raise")))) {
      d1669 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1675 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1675;
      }
    }
    if ((!d1669) && (u_is_con(s1668, &n_lex_46_TName) && u_equals(u_field(s1668, 0), u_mk_lit("try")))) {
      d1669 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1676 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1676, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1677 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1677;
      }
    }
    if (!d1669) {
      d1669 = 1;
      {
        static struct u_object l_statement_32_expect_1678 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1679 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1678, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1679)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1669) {
      u_no_case(s1668);
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
    static struct u_object l_else_1680 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1680)))) {
      {
        static struct u_object l_end_1681 = U_LIT("end");
        p_parse_46_keyword(&l_end_1681, r_ts);
        u_value t1682 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1682;
      }
    } else {
      static struct u_object l_if_1683 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1683)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1684 = U_LIT("end");
          p_parse_46_keyword(&l_end_1684, r_ts);
        }
      }
    }
    u_value t1685 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1685;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1686 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1686), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1687 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1687;
    u_value t1690 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1690)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1688 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1688), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1689 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1689;
      }
      t1690 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1691 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1691;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1692 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1692;
    static struct u_object l_case_1693 = U_LIT("case");
    u_value t1696 = f_parse_46_at_keyword_2(*r_ts, &l_case_1693);
    while (u_truth(t1696)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1694 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1694), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1695 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1695;
      }
      static struct u_object l_case_1697 = U_LIT("case");
      t1696 = f_parse_46_at_keyword_2(*r_ts, &l_case_1697);
    }
    static struct u_object l_else_1698 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1698))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1699 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1699;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1700 = U_LIT("end");
    p_parse_46_keyword(&l_end_1700, r_ts);
    u_value t1701 = f_list_46_reverse_1(v_rev);
    *r_cs = t1701;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1703;
  if (u_truth(v_first)) {
    t1703 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1702 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1702)) {
      t1702 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1703 = t1702;
  }
  return t1703;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1705 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1705)) {
    static struct u_object l_out_1704 = U_LIT("out");
    t1705 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1704));
  }
  u_value t1707 = t1705;
  if (u_truth(t1707)) {
    static struct u_object l_ref_1706 = U_LIT("ref");
    t1707 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1706));
  }
  return t1707;
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
    u_value t1708 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), v_ins, v_outs, v_refs);
    *r_s = t1708;
  }
}

void p_parse_46_sections(u_value v_item_in, u_value v_item, u_value v_sc, u_value *r_ins, u_value *r_outs, u_value *r_refs, u_value *r_ts) {
  {
    u_value v_first = NULL;
    u_value t1709 = u_mk_bool(1);
    v_first = t1709;
    u_value t1710 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1711 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1712 = u_mk_con(&n_list_46_Nil, 0);
    *r_ins = t1710;
    *r_outs = t1711;
    *r_refs = t1712;
    static struct u_object l_in_1713 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1713))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_section_items(v_item_in, v_sc, r_ins, r_ts);
        u_value t1714 = u_mk_bool(0);
        v_first = t1714;
      }
    }
    static struct u_object l_out_1715 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1715, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_section_items(v_item, v_sc, r_outs, r_ts);
        u_value t1716 = u_mk_bool(0);
        v_first = t1716;
      }
    }
    static struct u_object l_ref_1717 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1717, v_first))) {
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
    u_value t1718 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1718;
    u_value t1720 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1720)) {
      {
        p_parse_46_advance(r_ts);
        u_call_proc(v_item, 1, 2, v_sc, &v_x, r_ts);
        u_value t1719 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1719;
      }
      t1720 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1721 = f_list_46_reverse_1(v_rev);
    *r_xs = t1721;
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
    u_value t1722 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0));
    *r_x = t1722;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1723 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1723;
    static struct u_object l__58__1724 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1724)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1725 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1725;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1726 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1726;
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
  u_value s1727 = v_cs;
  u_value t1728 = NULL;
  if ((!t1728) && u_is_con(s1727, &n_list_46_Nil)) {
    t1728 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1728) && (u_is_con(s1727, &n_list_46_Cons) && u_is_con(u_field(s1727, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1727, 1);
    t1728 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1728) && u_is_con(s1727, &n_list_46_Cons)) {
    u_value v_c = u_field(s1727, 0);
    u_value v_more = u_field(s1727, 1);
    t1728 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1728) {
    u_no_case(s1727);
  }
  return t1728;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1729 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1730 = &t1729;
  u_value l1731 = v_cs;
  while (u_is_con(l1731, &n_list_46_Cons)) {
    u_value v_c = u_field(l1731, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1730 = u_append(e1730, v_c);
    }
    l1731 = u_field(l1731, 1);
  }
  return t1729;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1732 = v_c;
  u_value t1733 = NULL;
  if ((!t1733) && u_is_con(s1732, &n_syntax_46_CNotation)) {
    t1733 = u_mk_bool(1);
  }
  if (!t1733) {
    t1733 = u_mk_bool(0);
  }
  if (!t1733) {
    u_no_case(s1732);
  }
  return t1733;
}

u_value f_parse_46_Context_46_notations_1(u_value v_x) {
  u_value s1734 = v_x;
  u_value t1735 = NULL;
  if ((!t1735) && u_is_con(s1734, &n_parse_46_Context)) {
    u_value v_y = u_field(s1734, 0);
    t1735 = v_y;
  }
  if (!t1735) {
    u_no_case(s1734);
  }
  return t1735;
}

u_value f_parse_46_Context_46_modules_1(u_value v_x) {
  u_value s1736 = v_x;
  u_value t1737 = NULL;
  if ((!t1737) && u_is_con(s1736, &n_parse_46_Context)) {
    u_value v_y = u_field(s1736, 1);
    t1737 = v_y;
  }
  if (!t1737) {
    u_no_case(s1736);
  }
  return t1737;
}

u_value f_parse_46_Context_46_loading_1(u_value v_x) {
  u_value s1738 = v_x;
  u_value t1739 = NULL;
  if ((!t1739) && u_is_con(s1738, &n_parse_46_Context)) {
    u_value v_y = u_field(s1738, 2);
    t1739 = v_y;
  }
  if (!t1739) {
    u_no_case(s1738);
  }
  return t1739;
}

u_value f_parse_46_Context_46_base_1(u_value v_x) {
  u_value s1740 = v_x;
  u_value t1741 = NULL;
  if ((!t1741) && u_is_con(s1740, &n_parse_46_Context)) {
    u_value v_y = u_field(s1740, 3);
    t1741 = v_y;
  }
  if (!t1741) {
    u_no_case(s1740);
  }
  return t1741;
}

u_value f_parse_46_Context_46_dir_1(u_value v_x) {
  u_value s1742 = v_x;
  u_value t1743 = NULL;
  if ((!t1743) && u_is_con(s1742, &n_parse_46_Context)) {
    u_value v_y = u_field(s1742, 4);
    t1743 = v_y;
  }
  if (!t1743) {
    u_no_case(s1742);
  }
  return t1743;
}

u_value f_parse_46_Context_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1744 = v_x;
  u_value t1745 = NULL;
  if ((!t1745) && u_is_con(s1744, &n_parse_46_Context)) {
    u_value v_a1 = u_field(s1744, 1);
    u_value v_a2 = u_field(s1744, 2);
    u_value v_a3 = u_field(s1744, 3);
    u_value v_a4 = u_field(s1744, 4);
    t1745 = u_mk_con(&n_parse_46_Context, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1745) {
    u_no_case(s1744);
  }
  return t1745;
}

u_value f_parse_46_Context_46_modules_58__61__2(u_value v_x, u_value v_y) {
  u_value s1746 = v_x;
  u_value t1747 = NULL;
  if ((!t1747) && u_is_con(s1746, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1746, 0);
    u_value v_a2 = u_field(s1746, 2);
    u_value v_a3 = u_field(s1746, 3);
    u_value v_a4 = u_field(s1746, 4);
    t1747 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t1747) {
    u_no_case(s1746);
  }
  return t1747;
}

u_value f_parse_46_Context_46_loading_58__61__2(u_value v_x, u_value v_y) {
  u_value s1748 = v_x;
  u_value t1749 = NULL;
  if ((!t1749) && u_is_con(s1748, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1748, 0);
    u_value v_a1 = u_field(s1748, 1);
    u_value v_a3 = u_field(s1748, 3);
    u_value v_a4 = u_field(s1748, 4);
    t1749 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t1749) {
    u_no_case(s1748);
  }
  return t1749;
}

u_value f_parse_46_Context_46_base_58__61__2(u_value v_x, u_value v_y) {
  u_value s1750 = v_x;
  u_value t1751 = NULL;
  if ((!t1751) && u_is_con(s1750, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1750, 0);
    u_value v_a1 = u_field(s1750, 1);
    u_value v_a2 = u_field(s1750, 2);
    u_value v_a4 = u_field(s1750, 4);
    t1751 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t1751) {
    u_no_case(s1750);
  }
  return t1751;
}

u_value f_parse_46_Context_46_dir_58__61__2(u_value v_x, u_value v_y) {
  u_value s1752 = v_x;
  u_value t1753 = NULL;
  if ((!t1753) && u_is_con(s1752, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1752, 0);
    u_value v_a1 = u_field(s1752, 1);
    u_value v_a2 = u_field(s1752, 2);
    u_value v_a3 = u_field(s1752, 3);
    t1753 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t1753) {
    u_no_case(s1752);
  }
  return t1753;
}

u_value f_parse_46_with_commands_2(u_value v_cx, u_value v_cs) {
  return f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_with_notations_2(f_parse_46_Context_46_notations_1(v_cx), v_cs));
}

u_value f_parse_46_with_module_2(u_value v_cx, u_value v_c) {
  return f_parse_46_Context_46_modules_58__61__2(v_cx, f_list_46__43__43__2(f_parse_46_modules_of_1(v_c), f_parse_46_Context_46_modules_1(v_cx)));
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1754 = v_c;
  u_value t1755 = NULL;
  if ((!t1755) && u_is_con(s1754, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1754, 0);
    u_value v_cs = u_field(s1754, 1);
    u_value t1756 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1757 = &t1756;
    u_value l1758 = v_cs;
    while (u_is_con(l1758, &n_list_46_Cons)) {
      u_value v_d = u_field(l1758, 0);
      if (u_truth(u_mk_bool(1))) {
        e1757 = u_append(e1757, f_parse_46_modules_of_1(v_d));
      }
      l1758 = u_field(l1758, 1);
    }
    t1755 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1756));
  }
  if (!t1755) {
    t1755 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1755) {
    u_no_case(s1754);
  }
  return t1755;
}

u_value f_parse_46_own_notations_2(u_value v_cx, u_value v_n) {
  u_value s1759 = f_list_46_lookup_2(v_n, f_parse_46_Context_46_modules_1(v_cx));
  u_value t1760 = NULL;
  if ((!t1760) && u_is_con(s1759, &n_option_46_Some)) {
    u_value v_ns = u_field(s1759, 0);
    t1760 = v_ns;
  }
  if ((!t1760) && u_is_con(s1759, &n_option_46_None)) {
    t1760 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1760) {
    u_no_case(s1759);
  }
  return t1760;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1761 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1761;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1762 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1762;
        u_value t1764 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1764)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1763 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1763;
          }
          t1764 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1765 = f_list_46_reverse_1(v_rev);
    *r_ps = t1765;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1766 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1766;
    u_value s1767 = f_parse_46_head_token_1(*r_ts);
    int d1768 = 0;
    if ((!d1768) && (u_is_con(s1767, &n_lex_46_TName) && u_equals(u_field(s1767, 0), u_mk_lit("prefix")))) {
      d1768 = 1;
      {
        u_value t1769 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1769;
      }
    }
    if ((!d1768) && (u_is_con(s1767, &n_lex_46_TName) && u_equals(u_field(s1767, 0), u_mk_lit("postfix")))) {
      d1768 = 1;
      {
        u_value t1770 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1770;
      }
    }
    if (!d1768) {
      d1768 = 1;
      {
        u_value s1771 = f_parse_46_second_token_1(*r_ts);
        int d1772 = 0;
        if ((!d1772) && (u_is_con(s1771, &n_lex_46_TName) && u_equals(u_field(s1771, 0), u_mk_lit("left")))) {
          d1772 = 1;
          {
            u_value t1773 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1773;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1772) && (u_is_con(s1771, &n_lex_46_TName) && u_equals(u_field(s1771, 0), u_mk_lit("right")))) {
          d1772 = 1;
          {
            u_value t1774 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1774;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1772) {
          d1772 = 1;
          {
          }
        }
        if (!d1772) {
          u_no_case(s1771);
        }
      }
    }
    if (!d1768) {
      u_no_case(s1767);
    }
    p_parse_46_advance(r_ts);
    u_value s1775 = f_parse_46_head_token_1(*r_ts);
    int d1776 = 0;
    if ((!d1776) && u_is_con(s1775, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1775, 0);
      d1776 = 1;
      {
        u_value t1777 = v_n;
        v_p = t1777;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1776) {
      d1776 = 1;
      {
        static struct u_object l_precedence_32_expec_1778 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1779 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1778, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1779)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1776) {
      u_no_case(s1775);
    }
    u_value t1780 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1781 = u_mk_bool(1);
    v_ops = t1780;
    v_go = t1781;
    u_value t1787 = v_go;
    while (u_truth(t1787)) {
      {
        u_value s1782 = f_parse_46_head_token_1(*r_ts);
        int d1783 = 0;
        if ((!d1783) && u_is_con(s1782, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1782, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1783 = 1;
            {
              u_value t1784 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1784;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1783) && u_is_con(s1782, &n_lex_46_TName)) {
          u_value v_n = u_field(s1782, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1783 = 1;
            {
              u_value t1785 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1785;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1783) {
          d1783 = 1;
          {
            u_value t1786 = u_mk_bool(0);
            v_go = t1786;
          }
        }
        if (!d1783) {
          u_no_case(s1782);
        }
      }
      t1787 = v_go;
    }
    u_value s1788 = v_ops;
    int d1789 = 0;
    if ((!d1789) && u_is_con(s1788, &n_list_46_Nil)) {
      d1789 = 1;
      {
        static struct u_object l_operator_32_expecte_1790 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1791 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1790, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1791)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1789) {
      d1789 = 1;
      {
        u_value t1792 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1792;
      }
    }
    if (!d1789) {
      u_no_case(s1788);
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
    u_value t1793 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1794 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1793;
    *r_fs = t1794;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1795 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
            u_value t1796 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
            v_args = t1795;
            *r_fs = t1796;
            u_value t1799 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1799)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t1797 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t1798 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t1797;
                *r_fs = t1798;
              }
              t1799 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1800 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t1801 = f_list_46_reverse_1(*r_fs);
    *r_k = t1800;
    *r_fs = t1801;
    u_value s1802 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d1803 = 0;
    if ((!d1803) && u_is_con(s1802, &n_option_46_Some)) {
      u_value v_f = u_field(s1802, 0);
      d1803 = 1;
      {
        static struct u_object l_repeated_32_field_32__1804 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__1805 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__1804, u_concat(v_f, u_concat(&l__32_of_32__1805, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1803) && u_is_con(s1802, &n_option_46_None)) {
      d1803 = 1;
      {
      }
    }
    if (!d1803) {
      u_no_case(s1802);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1806 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1807 = 0;
    if ((!d1807) && (u_is_con(u_field(s1806, 0), &n_lex_46_TName) && (u_is_con(u_field(s1806, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1806, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1806, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1807 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1808 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1808;
        }
      }
    }
    if (!d1807) {
      d1807 = 1;
      {
        u_value t1809 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1809;
      }
    }
    if (!d1807) {
      u_no_case(s1806);
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
    static struct u_object l__58__61__1810 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1810), r_ts);
    static struct u_object l__124__1811 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1811)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1812 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1812;
    static struct u_object l__124__1813 = U_LIT("|");
    u_value t1815 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1813));
    while (u_truth(t1815)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1814 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1814;
      }
      static struct u_object l__124__1816 = U_LIT("|");
      t1815 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1816));
    }
    u_value s1817 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d1818 = 0;
    if ((!d1818) && u_is_con(s1817, &n_option_46_Some)) {
      u_value v_f = u_field(s1817, 0);
      d1818 = 1;
      {
        static struct u_object l_field_32__1819 = U_LIT("field ");
        static struct u_object l__32_of_32__1820 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_1821 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__1819, u_concat(v_f, u_concat(&l__32_of_32__1820, u_concat(v_n, &l__32_has_32_different_32_t_1821)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1818) && u_is_con(s1817, &n_option_46_None)) {
      d1818 = 1;
      {
      }
    }
    if (!d1818) {
      u_no_case(s1817);
    }
    u_value t1822 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_list_46__43__43__2(f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)), f_parse_46_updaters_3(v_n, v_ps, f_list_46_reverse_1(v_rev))));
    *r_cs = t1822;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s1823 = v_fs;
  u_value t1824 = NULL;
  if ((!t1824) && u_is_con(s1823, &n_list_46_Nil)) {
    t1824 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1824) && (u_is_con(s1823, &n_list_46_Cons) && u_is_con(u_field(s1823, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s1823, 0), 0);
    u_value v_rest = u_field(s1823, 1);
    t1824 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t1824) && (u_is_con(s1823, &n_list_46_Cons) && u_is_con(u_field(s1823, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1823, 1);
    t1824 = f_parse_46_field_names_1(v_rest);
  }
  if (!t1824) {
    u_no_case(s1823);
  }
  return t1824;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s1825 = v_fs;
  u_value t1826 = NULL;
  if ((!t1826) && u_is_con(s1825, &n_list_46_Nil)) {
    t1826 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1826) && u_is_con(s1825, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1825, 0), 0);
    u_value v_t = u_field(u_field(u_field(s1825, 0), 1), 3);
    u_value v_rest = u_field(s1825, 1);
    u_value t1828;
    if (u_truth(f_list_46_exists_2(f_parse_46_fields_named_2(v_f, v_rest), u_mk_closure(lam1827, 1, v_t)))) {
      t1828 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t1828 = f_parse_46_differing_1(v_rest);
    }
    t1826 = t1828;
  }
  if (!t1826) {
    u_no_case(s1825);
  }
  return t1826;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s1829 = v_e;
  u_value t1830 = NULL;
  if (!t1830) {
    u_value v_t = u_field(u_field(s1829, 1), 3);
    t1830 = v_t;
  }
  if (!t1830) {
    u_no_case(s1829);
  }
  return t1830;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s1831 = v_fs;
  u_value t1832 = NULL;
  if ((!t1832) && u_is_con(s1831, &n_list_46_Nil)) {
    t1832 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1832) && u_is_con(s1831, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s1831, 0), 0);
    u_value v_d = u_field(u_field(s1831, 0), 1);
    u_value v_rest = u_field(s1831, 1);
    u_value t1833;
    if (u_truth(u_eq(v_g, v_f))) {
      t1833 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t1833 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t1832 = t1833;
  }
  if (!t1832) {
    u_no_case(s1831);
  }
  return t1832;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1834 = v_ks;
  u_value t1835 = NULL;
  if ((!t1835) && u_is_con(s1834, &n_list_46_Nil)) {
    t1835 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1835) && (u_is_con(s1834, &n_list_46_Cons) && u_is_con(u_field(u_field(s1834, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1834, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1834, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1834, 0), 1);
    u_value v_rest = u_field(s1834, 1);
    t1835 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1835) {
    u_no_case(s1834);
  }
  return t1835;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1836 = v_fs;
  u_value t1837 = NULL;
  if ((!t1837) && u_is_con(s1836, &n_list_46_Nil)) {
    t1837 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1837) && (u_is_con(s1836, &n_list_46_Cons) && u_is_con(u_field(u_field(s1836, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1836, 0), 0), 0);
    u_value v_t = u_field(u_field(s1836, 0), 1);
    u_value v_rest = u_field(s1836, 1);
    t1837 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1837) && (u_is_con(s1836, &n_list_46_Cons) && u_is_con(u_field(u_field(s1836, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1836, 1);
    t1837 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1837) {
    u_no_case(s1836);
  }
  return t1837;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1842 = f_parse_46_fields_of_1(v_ks);
  u_value t1841;
  {
    u_value v_fs = t1842;
    u_value t1838 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1839 = &t1838;
    u_value l1840 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1840, &n_list_46_Cons)) {
      u_value v_f = u_field(l1840, 0);
      if (u_truth(u_mk_bool(1))) {
        e1839 = u_append(e1839, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1840 = u_field(l1840, 1);
    }
    t1841 = t1838;
  }
  return t1841;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1843 = U_LIT("x");
  u_value t1844 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1845 = &t1844;
  u_value l1846 = v_ps;
  while (u_is_con(l1846, &n_list_46_Cons)) {
    u_value v_p = u_field(l1846, 0);
    if (u_truth(u_mk_bool(1))) {
      e1845 = u_append(e1845, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1846 = u_field(l1846, 1);
  }
  u_value t1854 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1843), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1855 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1844);
  u_value t1853;
  {
    u_value v_x = t1854;
    u_value v_owner = t1855;
    static struct u_object l__46__1847 = U_LIT(".");
    static struct u_object l_x_1848 = U_LIT("x");
    u_value t1849 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1850 = &t1849;
    u_value l1851 = v_es;
    while (u_is_con(l1851, &n_list_46_Cons)) {
      u_value v_e = u_field(l1851, 0);
      if (u_truth(u_mk_bool(1))) {
        e1850 = u_append(e1850, f_parse_46_field_case_1(v_e));
      }
      l1851 = u_field(l1851, 1);
    }
    u_value t1852;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1852 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1852 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1853 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1847, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1848), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1849, t1852)));
  }
  return t1853;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1856 = v_es;
  u_value t1857 = NULL;
  if ((!t1857) && u_is_con(s1856, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1856, 0), 1), 3);
    t1857 = v_t;
  }
  if (!t1857) {
    t1857 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1857) {
    u_no_case(s1856);
  }
  return t1857;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s1858 = v_e;
  u_value t1859 = NULL;
  if (!t1859) {
    u_value v_k = u_field(u_field(s1858, 1), 0);
    u_value v_i = u_field(u_field(s1858, 1), 1);
    u_value v_n = u_field(u_field(s1858, 1), 2);
    u_value t1860 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1861 = &t1860;
    u_value l1862 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1862, &n_list_46_Cons)) {
      u_value v_j = u_field(l1862, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1864;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1863 = U_LIT("y");
          t1864 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1863), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1864 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1861 = u_append(e1861, t1864);
      }
      l1862 = u_field(l1862, 1);
    }
    static struct u_object l_y_1865 = U_LIT("y");
    t1859 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1860), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1865), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1859) {
    u_no_case(s1858);
  }
  return t1859;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_MissingField_1866 = U_LIT("MissingField");
  static struct u_object l__46__1867 = U_LIT(".");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_ERaise, 2, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_MissingField_1866), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_t, u_concat(&l__46__1867, v_f)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_parse_46_updaters_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1872 = f_parse_46_fields_of_1(v_ks);
  u_value t1871;
  {
    u_value v_fs = t1872;
    u_value t1868 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1869 = &t1868;
    u_value l1870 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1870, &n_list_46_Cons)) {
      u_value v_f = u_field(l1870, 0);
      if (u_truth(u_mk_bool(1))) {
        e1869 = u_append(e1869, f_parse_46_updater_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1870 = u_field(l1870, 1);
    }
    t1871 = t1868;
  }
  return t1871;
}

u_value f_parse_46_updater_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1873 = U_LIT("x");
  u_value t1874 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1875 = &t1874;
  u_value l1876 = v_ps;
  while (u_is_con(l1876, &n_list_46_Cons)) {
    u_value v_p = u_field(l1876, 0);
    if (u_truth(u_mk_bool(1))) {
      e1875 = u_append(e1875, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1876 = u_field(l1876, 1);
  }
  u_value t1886 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1873), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1887 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1874);
  u_value t1885;
  {
    u_value v_x = t1886;
    u_value v_owner = t1887;
    static struct u_object l__46__1877 = U_LIT(".");
    static struct u_object l__58__61__1878 = U_LIT(":=");
    static struct u_object l_x_1879 = U_LIT("x");
    static struct u_object l_y_1880 = U_LIT("y");
    u_value t1881 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1882 = &t1881;
    u_value l1883 = v_es;
    while (u_is_con(l1883, &n_list_46_Cons)) {
      u_value v_e = u_field(l1883, 0);
      if (u_truth(u_mk_bool(1))) {
        e1882 = u_append(e1882, f_parse_46_update_case_1(v_e));
      }
      l1883 = u_field(l1883, 1);
    }
    u_value t1884;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1884 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1884 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1885 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1877, u_concat(v_f, &l__58__61__1878)))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1879), v_owner), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1880), f_parse_46_field_type_1(v_es)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), v_owner, u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1881, t1884)));
  }
  return t1885;
}

u_value f_parse_46_update_case_1(u_value v_e) {
  u_value s1888 = v_e;
  u_value t1889 = NULL;
  if (!t1889) {
    u_value v_k = u_field(u_field(s1888, 1), 0);
    u_value v_i = u_field(u_field(s1888, 1), 1);
    u_value v_n = u_field(u_field(s1888, 1), 2);
    u_value t1890 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1891 = &t1890;
    u_value l1892 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1892, &n_list_46_Cons)) {
      u_value v_j = u_field(l1892, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1894;
        if (u_truth(u_eq(v_j, v_i))) {
          t1894 = u_mk_con(&n_syntax_46_PWild, 0);
        } else {
          static struct u_object l_a_1893 = U_LIT("a");
          t1894 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l_a_1893, u_str_show(v_j))), u_mk_con(&n_syntax_46_THole, 0)));
        }
        e1891 = u_append(e1891, t1894);
      }
      l1892 = u_field(l1892, 1);
    }
    u_value t1895 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1896 = &t1895;
    u_value l1897 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1897, &n_list_46_Cons)) {
      u_value v_j = u_field(l1897, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1900;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1898 = U_LIT("y");
          t1900 = &l_y_1898;
        } else {
          static struct u_object l_a_1899 = U_LIT("a");
          t1900 = u_concat(&l_a_1899, u_str_show(v_j));
        }
        e1896 = u_append(e1896, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(t1900), u_mk_con(&n_syntax_46_THole, 0))));
      }
      l1897 = u_field(l1897, 1);
    }
    t1889 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1890), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_k, u_mk_con(&n_syntax_46_THole, 0))), t1895));
  }
  if (!t1889) {
    u_no_case(s1888);
  }
  return t1889;
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t1901;
  if (u_truth(u_eq(v_i, v_n))) {
    t1901 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1901 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t1901;
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
    u_value t1902 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1902;
    u_value t1904 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1904)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1903 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1903;
      }
      t1904 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1905 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1905), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1906 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1906), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1907 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1907;
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
    static struct u_object l__58__61__1908 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1908), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1909 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1909;
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
    u_value t1910 = f_parse_46_Context_46_notations_1(v_cx);
    v_ns = t1910;
    u_value s1911 = f_parse_46_head_token_1(*r_ts);
    int d1912 = 0;
    if ((!d1912) && (u_is_con(s1911, &n_lex_46_TName) && u_equals(u_field(s1911, 0), u_mk_lit("import")))) {
      d1912 = 1;
      {
        u_value t1913 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1913;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Context_46_modules_1(v_cx))))) {
          {
            u_value t1914 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1914;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_cx, v_at, r_cs);
            u_value t1915 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1915;
          }
        }
      }
    }
    if ((!d1912) && u_is_con(s1911, &n_lex_46_TName)) {
      u_value v_w = u_field(s1911, 0);
      static struct u_object l_prefix_1917 = U_LIT("prefix");
      static struct u_object l_postfix_1918 = U_LIT("postfix");
      static struct u_object l_infix_1919 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1917, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1918, u_mk_con(&n_list_46_Cons, 2, &l_infix_1919, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1912 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1916 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1916;
        }
      }
    }
    if ((!d1912) && (u_is_con(s1911, &n_lex_46_TName) && u_equals(u_field(s1911, 0), u_mk_lit("type")))) {
      d1912 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1920 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1920), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1921 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1921;
      }
    }
    if ((!d1912) && (u_is_con(s1911, &n_lex_46_TName) && u_equals(u_field(s1911, 0), u_mk_lit("data")))) {
      d1912 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1912) && (u_is_con(s1911, &n_lex_46_TName) && u_equals(u_field(s1911, 0), u_mk_lit("function")))) {
      d1912 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1922 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1922;
      }
    }
    if ((!d1912) && (u_is_con(s1911, &n_lex_46_TName) && u_equals(u_field(s1911, 0), u_mk_lit("procedure")))) {
      d1912 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1923 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1923;
      }
    }
    if ((!d1912) && (u_is_con(s1911, &n_lex_46_TName) && u_equals(u_field(s1911, 0), u_mk_lit("test")))) {
      d1912 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1924 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1924), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1925 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1925;
      }
    }
    if (!d1912) {
      d1912 = 1;
      {
        static struct u_object l_declaration_32_expe_1926 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1927 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1926, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1927)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1912) {
      u_no_case(s1911);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_cx) {
  {
    u_value s1928 = v_cs;
    int d1929 = 0;
    if ((!d1929) && u_is_con(s1928, &n_list_46_Nil)) {
      d1929 = 1;
      {
      }
    }
    if ((!d1929) && (u_is_con(s1928, &n_list_46_Cons) && u_is_con(u_field(s1928, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1928, 0), 0);
      u_value v_p = u_field(u_field(s1928, 0), 1);
      u_value v_ops = u_field(u_field(s1928, 0), 2);
      u_value v_more = u_field(s1928, 1);
      d1929 = 1;
      {
        u_value t1930 = f_parse_46_with_commands_2(*r_cx, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_cx = t1930;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1929) && (u_is_con(s1928, &n_list_46_Cons) && u_is_con(u_field(s1928, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1928, 0), 0);
      u_value v_ds = u_field(u_field(s1928, 0), 1);
      u_value v_more = u_field(s1928, 1);
      d1929 = 1;
      {
        u_value t1931 = f_parse_46_with_module_2(*r_cx, u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds));
        *r_cx = t1931;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1929) && (u_is_con(s1928, &n_list_46_Cons) && u_is_con(u_field(s1928, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1928, 0), 0);
      u_value v_more = u_field(s1928, 1);
      d1929 = 1;
      {
        u_value t1932 = f_parse_46_with_commands_2(*r_cx, f_parse_46_own_notations_2(*r_cx, v_n));
        *r_cx = t1932;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1929) && u_is_con(s1928, &n_list_46_Cons)) {
      u_value v_more = u_field(s1928, 1);
      d1929 = 1;
      {
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if (!d1929) {
      u_no_case(s1928);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_cx, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1933 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1933;
    u_value t1935 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1935)) {
      {
        p_parse_46_parse_command(*r_cx, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_cx);
        u_value t1934 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1934;
      }
      t1935 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1936 = f_list_46_reverse_1(v_rev);
    *r_cs = t1936;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1937 = v_dir;
    int d1938 = 0;
    if ((!d1938) && u_is_con(s1937, &n_option_46_Some)) {
      u_value v_d = u_field(s1937, 0);
      d1938 = 1;
      {
        {
          struct u_handler *h1941 = u_push();
          if (setjmp(h1941->env) == 0) {
            {
              static struct u_object l__47__1939 = U_LIT("/");
              static struct u_object l__46_u_1940 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1939, u_concat(v_n, &l__46_u_1940))), r_text);
            }
            u_pop(h1941);
          } else {
            u_pop(h1941);
            int d1942 = 0;
            if ((!d1942) && (u_has_type(h1941, "os.OsError") && u_is_con(u_caught(h1941), &n_os_46_OsError))) {
              d1942 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1942) {
              u_reraise(h1941);
            }
          }
        }
      }
    }
    if ((!d1938) && u_is_con(s1937, &n_option_46_None)) {
      d1938 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1938) {
      u_no_case(s1937);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1943 = u_push();
      if (setjmp(h1943->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1943);
      } else {
        u_pop(h1943);
        int d1944 = 0;
        if ((!d1944) && (u_has_type(h1943, "os.OsError") && u_is_con(u_caught(h1943), &n_os_46_OsError))) {
          d1944 = 1;
          {
            static struct u_object l_unknown_32_module_32__1945 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1945, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1944) {
          u_reraise(h1943);
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
        static struct u_object l_cyclic_32_import_32_of_1946 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1946, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Context_46_dir_1(v_cx), v_at, &v_text);
    u_value t1947 = f_parse_46_Context_46_loading_58__61__2(f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_base_notations_1(v_cx)), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Context_46_loading_1(v_cx)));
    v_mcx = t1947;
    {
      struct u_handler *h1948 = u_push();
      if (setjmp(h1948->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mcx, &v_mts);
        }
        u_pop(h1948);
      } else {
        u_pop(h1948);
        int d1949 = 0;
        if ((!d1949) && (u_has_type(h1948, "lex.LexError") && u_is_con(u_caught(h1948), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1948), 0);
          u_value v_i = u_field(u_caught(h1948), 1);
          d1949 = 1;
          {
            static struct u_object l__32_in_32__1950 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1950, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1949) && (u_has_type(h1948, "parse.ParseError") && u_is_con(u_caught(h1948), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1948), 0);
          u_value v_i = u_field(u_caught(h1948), 1);
          d1949 = 1;
          {
            static struct u_object l__32_in_32__1951 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1951, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1949) {
          u_reraise(h1948);
        }
      }
    }
  }
}

u_value f_parse_46_base_notations_1(u_value v_cx) {
  u_value s1952 = f_parse_46_Context_46_base_1(v_cx);
  u_value t1953 = NULL;
  if ((!t1953) && u_is_con(s1952, &n_option_46_Some)) {
    u_value v_b = u_field(s1952, 0);
    t1953 = v_b;
  }
  if ((!t1953) && u_is_con(s1952, &n_option_46_None)) {
    t1953 = f_parse_46_Context_46_notations_1(v_cx);
  }
  if (!t1953) {
    u_no_case(s1952);
  }
  return t1953;
}

void p_parse_46_parse_text(u_value v_text, u_value v_cx, u_value *r_p) {
  {
    u_value v_ts = NULL;
    u_value v_cs = NULL;
    u_value v_after = NULL;
    u_value t1954 = v_cx;
    v_after = t1954;
    {
      struct u_handler *h1955 = u_push();
      if (setjmp(h1955->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1955);
      } else {
        u_pop(h1955);
        int d1956 = 0;
        if ((!d1956) && (u_has_type(h1955, "lex.LexError") && u_is_con(u_caught(h1955), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1955), 0);
          u_value v_i = u_field(u_caught(h1955), 1);
          d1956 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1956) {
          u_reraise(h1955);
        }
      }
    }
    p_parse_46_parse_commands(&v_cs, &v_after, &v_ts);
    u_value t1957 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1957;
  }
}

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p) {
  {
    u_value v_cx = NULL;
    u_value t1958 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    v_cx = t1958;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), &v_cx);
    p_parse_46_parse_text(v_text, f_parse_46_Context_46_dir_58__61__2(f_parse_46_Context_46_base_58__61__2(f_parse_46_Context_46_loading_58__61__2(v_cx, u_mk_con(&n_list_46_Cons, 2, v_name, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_option_46_Some, 1, f_parse_46_Context_46_notations_1(v_cx))), v_dir), r_p);
  }
}

void p_parse_46_parse_library(u_value v_n, u_value *r_p) {
  {
    u_value v_text = NULL;
    u_value v_pre = NULL;
    p_parse_46_library_text(v_n, u_mk_int(0), &v_text);
    static struct u_object l_prelude_1959 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1959))) {
      {
        static struct u_object l_prelude_1960 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1960, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1961 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1961, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1962 = v_xs;
  u_value t1963 = NULL;
  if ((!t1963) && (u_is_con(s1962, &n_list_46_Cons) && u_is_con(u_field(s1962, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1962, 0);
    t1963 = v_x;
  }
  if ((!t1963) && u_is_con(s1962, &n_list_46_Cons)) {
    u_value v_more = u_field(s1962, 1);
    t1963 = f_parse_46_last_part_1(v_more);
  }
  if (!t1963) {
    static struct u_object l__1964 = U_LIT("");
    t1963 = &l__1964;
  }
  if (!t1963) {
    u_no_case(s1962);
  }
  return t1963;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1969 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1968;
  {
    u_value v_f = t1969;
    u_value t1966 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1966)) {
      static struct u_object l__46_u_1965 = U_LIT(".u");
      t1966 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1965);
    }
    u_value t1967;
    if (u_truth(t1966)) {
      t1967 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1967 = v_f;
    }
    t1968 = t1967;
  }
  return t1968;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1974 = u_str_split(v_path, u_mk_char(47));
  u_value t1973;
  {
    u_value v_parts = t1974;
    u_value t1972;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1970 = U_LIT(".");
      t1972 = &l__46__1970;
    } else {
      static struct u_object l__47__1971 = U_LIT("/");
      t1972 = u_str_join(&l__47__1971, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1973 = t1972;
  }
  return t1973;
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
    static struct u_object l_prelude_1975 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1975, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1976 = u_mk_tuple(2, v_cs, v_ds);
    int d1977 = 0;
    if ((!d1977) && (u_is_con(u_field(s1976, 0), &n_list_46_Cons) && u_is_con(u_field(s1976, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1976, 0), 0);
      u_value v_more = u_field(u_field(s1976, 0), 1);
      u_value v_d = u_field(u_field(s1976, 1), 0);
      u_value v_rest = u_field(u_field(s1976, 1), 1);
      d1977 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1977) {
      d1977 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1977) {
      u_no_case(s1976);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1978 = v_paths;
    int d1979 = 0;
    if ((!d1979) && u_is_con(s1978, &n_list_46_Nil)) {
      d1979 = 1;
      {
      }
    }
    if ((!d1979) && u_is_con(s1978, &n_list_46_Cons)) {
      u_value v_path = u_field(s1978, 0);
      u_value v_more = u_field(s1978, 1);
      d1979 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1979) {
      u_no_case(s1978);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1980 = v_paths;
    int d1981 = 0;
    if ((!d1981) && u_is_con(s1980, &n_list_46_Nil)) {
      d1981 = 1;
      {
      }
    }
    if ((!d1981) && u_is_con(s1980, &n_list_46_Cons)) {
      u_value v_path = u_field(s1980, 0);
      u_value v_more = u_field(s1980, 1);
      d1981 = 1;
      {
        u_value t1982 = u_mk_bool(0);
        v_failed = t1982;
        {
          struct u_handler *h1983 = u_push();
          if (setjmp(h1983->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1983);
          } else {
            u_pop(h1983);
            int d1984 = 0;
            if ((!d1984) && (u_has_type(h1983, "parse.ParseError") && u_is_con(u_caught(h1983), &n_parse_46_ParseError))) {
              d1984 = 1;
              {
                u_value t1985 = u_mk_bool(1);
                v_failed = t1985;
              }
            }
            if (!d1984) {
              u_reraise(h1983);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1981) {
      u_no_case(s1980);
    }
  }
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s1986 = v_x;
  u_value t1987 = NULL;
  if ((!t1987) && u_is_con(s1986, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1986, 0);
    t1987 = v_y;
  }
  if (!t1987) {
    u_no_case(s1986);
  }
  return t1987;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s1988 = v_x;
  u_value t1989 = NULL;
  if ((!t1989) && u_is_con(s1988, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1988, 1);
    t1989 = v_y;
  }
  if (!t1989) {
    u_no_case(s1988);
  }
  return t1989;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s1990 = v_x;
  u_value t1991 = NULL;
  if ((!t1991) && u_is_con(s1990, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1990, 2);
    t1991 = v_y;
  }
  if (!t1991) {
    u_no_case(s1990);
  }
  return t1991;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s1992 = v_x;
  u_value t1993 = NULL;
  if ((!t1993) && u_is_con(s1992, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1992, 3);
    t1993 = v_y;
  }
  if (!t1993) {
    u_no_case(s1992);
  }
  return t1993;
}

u_value f_compile_46_Globals_46_procs_1(u_value v_x) {
  u_value s1994 = v_x;
  u_value t1995 = NULL;
  if ((!t1995) && u_is_con(s1994, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1994, 4);
    t1995 = v_y;
  }
  if (!t1995) {
    u_no_case(s1994);
  }
  return t1995;
}

u_value f_compile_46_Globals_46_funs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1996 = v_x;
  u_value t1997 = NULL;
  if ((!t1997) && u_is_con(s1996, &n_compile_46_Globals)) {
    u_value v_a1 = u_field(s1996, 1);
    u_value v_a2 = u_field(s1996, 2);
    u_value v_a3 = u_field(s1996, 3);
    u_value v_a4 = u_field(s1996, 4);
    t1997 = u_mk_con(&n_compile_46_Globals, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1997) {
    u_no_case(s1996);
  }
  return t1997;
}

u_value f_compile_46_Globals_46_constrs_58__61__2(u_value v_x, u_value v_y) {
  u_value s1998 = v_x;
  u_value t1999 = NULL;
  if ((!t1999) && u_is_con(s1998, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s1998, 0);
    u_value v_a2 = u_field(s1998, 2);
    u_value v_a3 = u_field(s1998, 3);
    u_value v_a4 = u_field(s1998, 4);
    t1999 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t1999) {
    u_no_case(s1998);
  }
  return t1999;
}

u_value f_compile_46_Globals_46_arities_58__61__2(u_value v_x, u_value v_y) {
  u_value s2000 = v_x;
  u_value t2001 = NULL;
  if ((!t2001) && u_is_con(s2000, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s2000, 0);
    u_value v_a1 = u_field(s2000, 1);
    u_value v_a3 = u_field(s2000, 3);
    u_value v_a4 = u_field(s2000, 4);
    t2001 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t2001) {
    u_no_case(s2000);
  }
  return t2001;
}

u_value f_compile_46_Globals_46_fields_58__61__2(u_value v_x, u_value v_y) {
  u_value s2002 = v_x;
  u_value t2003 = NULL;
  if ((!t2003) && u_is_con(s2002, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s2002, 0);
    u_value v_a1 = u_field(s2002, 1);
    u_value v_a2 = u_field(s2002, 2);
    u_value v_a4 = u_field(s2002, 4);
    t2003 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t2003) {
    u_no_case(s2002);
  }
  return t2003;
}

u_value f_compile_46_Globals_46_procs_58__61__2(u_value v_x, u_value v_y) {
  u_value s2004 = v_x;
  u_value t2005 = NULL;
  if ((!t2005) && u_is_con(s2004, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s2004, 0);
    u_value v_a1 = u_field(s2004, 1);
    u_value v_a2 = u_field(s2004, 2);
    u_value v_a3 = u_field(s2004, 3);
    t2005 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t2005) {
    u_no_case(s2004);
  }
  return t2005;
}

u_value f_compile_46_Gen_46_count_1(u_value v_x) {
  u_value s2006 = v_x;
  u_value t2007 = NULL;
  if ((!t2007) && u_is_con(s2006, &n_compile_46_Gen)) {
    u_value v_y = u_field(s2006, 0);
    t2007 = v_y;
  }
  if (!t2007) {
    u_no_case(s2006);
  }
  return t2007;
}

u_value f_compile_46_Gen_46_lifted_1(u_value v_x) {
  u_value s2008 = v_x;
  u_value t2009 = NULL;
  if ((!t2009) && u_is_con(s2008, &n_compile_46_Gen)) {
    u_value v_y = u_field(s2008, 1);
    t2009 = v_y;
  }
  if (!t2009) {
    u_no_case(s2008);
  }
  return t2009;
}

u_value f_compile_46_Gen_46_count_58__61__2(u_value v_x, u_value v_y) {
  u_value s2010 = v_x;
  u_value t2011 = NULL;
  if ((!t2011) && u_is_con(s2010, &n_compile_46_Gen)) {
    u_value v_a1 = u_field(s2010, 1);
    t2011 = u_mk_con(&n_compile_46_Gen, 2, v_y, v_a1);
  }
  if (!t2011) {
    u_no_case(s2010);
  }
  return t2011;
}

u_value f_compile_46_Gen_46_lifted_58__61__2(u_value v_x, u_value v_y) {
  u_value s2012 = v_x;
  u_value t2013 = NULL;
  if ((!t2013) && u_is_con(s2012, &n_compile_46_Gen)) {
    u_value v_a0 = u_field(s2012, 0);
    t2013 = u_mk_con(&n_compile_46_Gen, 2, v_a0, v_y);
  }
  if (!t2013) {
    u_no_case(s2012);
  }
  return t2013;
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
  static struct u_object l_u_value_2014 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_2014);
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
  static struct u_object l_u_value_2015 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_2015));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t2016 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2017 = &t2016;
  u_value l2018 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l2018, &n_list_46_Cons)) {
    u_value v_c = u_field(l2018, 0);
    if (u_truth(u_mk_bool(1))) {
      e2017 = u_append(e2017, f_compile_46_mangle_char_1(v_c));
    }
    l2018 = u_field(l2018, 1);
  }
  return u_string(f_list_46_concat_1(t2016));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t2019 = u_is_letter(v_c);
  if (!u_truth(t2019)) {
    t2019 = u_is_digit(v_c);
  }
  u_value t2020 = t2019;
  if (!u_truth(t2020)) {
    t2020 = u_eq(v_c, u_mk_char(95));
  }
  u_value t2023;
  if (u_truth(t2020)) {
    t2023 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___2021 = U_LIT("_");
    static struct u_object l___2022 = U_LIT("_");
    t2023 = u_chars(u_concat(&l___2021, u_concat(u_str_show(u_ord(v_c)), &l___2022)));
  }
  return t2023;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__2024 = U_LIT("f_");
  static struct u_object l___2025 = U_LIT("_");
  return u_concat(&l_f__2024, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___2025, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__2026 = U_LIT("p_");
  return u_concat(&l_p__2026, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__2027 = U_LIT("v_");
  return u_concat(&l_v__2027, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__2028 = U_LIT("r_");
  return u_concat(&l_r__2028, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__2029 = U_LIT("+");
  static struct u_object l_u_add_2030 = U_LIT("u_add");
  static struct u_object l__45__2031 = U_LIT("-");
  static struct u_object l_u_sub_2032 = U_LIT("u_sub");
  static struct u_object l__42__2033 = U_LIT("*");
  static struct u_object l_u_mul_2034 = U_LIT("u_mul");
  static struct u_object l__47__2035 = U_LIT("/");
  static struct u_object l_u_div_2036 = U_LIT("u_div");
  static struct u_object l__37__2037 = U_LIT("%");
  static struct u_object l_u_mod_2038 = U_LIT("u_mod");
  static struct u_object l__60__2039 = U_LIT("<");
  static struct u_object l_u_lt_2040 = U_LIT("u_lt");
  static struct u_object l__61__61__2041 = U_LIT("==");
  static struct u_object l_u_eq_2042 = U_LIT("u_eq");
  static struct u_object l_compare_2043 = U_LIT("compare");
  static struct u_object l_u_compare_2044 = U_LIT("u_compare");
  static struct u_object l_chars_2045 = U_LIT("chars");
  static struct u_object l_u_chars_2046 = U_LIT("u_chars");
  static struct u_object l_string_2047 = U_LIT("string");
  static struct u_object l_u_string_2048 = U_LIT("u_string");
  static struct u_object l_ord_2049 = U_LIT("ord");
  static struct u_object l_u_ord_2050 = U_LIT("u_ord");
  static struct u_object l_chr_2051 = U_LIT("chr");
  static struct u_object l_u_chr_2052 = U_LIT("u_chr");
  static struct u_object l__94__2053 = U_LIT("^");
  static struct u_object l_u_concat_2054 = U_LIT("u_concat");
  static struct u_object l_str_size_2055 = U_LIT("str_size");
  static struct u_object l_u_str_size_2056 = U_LIT("u_str_size");
  static struct u_object l_str_join_2057 = U_LIT("str_join");
  static struct u_object l_u_str_join_2058 = U_LIT("u_str_join");
  static struct u_object l_str_slice_2059 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_2060 = U_LIT("u_str_slice");
  static struct u_object l_str_split_2061 = U_LIT("str_split");
  static struct u_object l_u_str_split_2062 = U_LIT("u_str_split");
  static struct u_object l_str_get_2063 = U_LIT("str_get");
  static struct u_object l_u_str_get_2064 = U_LIT("u_str_get");
  static struct u_object l_str_contains_2065 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_2066 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_2067 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_2068 = U_LIT("u_str_compare");
  static struct u_object l_str_show_2069 = U_LIT("str_show");
  static struct u_object l_u_str_show_2070 = U_LIT("u_str_show");
  static struct u_object l_str_quote_2071 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_2072 = U_LIT("u_str_quote");
  static struct u_object l_str_number_2073 = U_LIT("str_number");
  static struct u_object l_u_str_number_2074 = U_LIT("u_str_number");
  static struct u_object l_is_digit_2075 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_2076 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_2077 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_2078 = U_LIT("u_is_letter");
  static struct u_object l_is_space_2079 = U_LIT("is_space");
  static struct u_object l_u_is_space_2080 = U_LIT("u_is_space");
  u_pop(handler);
  cache = f_map_46_put_all_2(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__2029), u_mk_tuple(2, &l_u_add_2030, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__2031), u_mk_tuple(2, &l_u_sub_2032, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__2033), u_mk_tuple(2, &l_u_mul_2034, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__2035), u_mk_tuple(2, &l_u_div_2036, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__2037), u_mk_tuple(2, &l_u_mod_2038, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__2039), u_mk_tuple(2, &l_u_lt_2040, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__2041), u_mk_tuple(2, &l_u_eq_2042, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_2043), u_mk_tuple(2, &l_u_compare_2044, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_2045), u_mk_tuple(2, &l_u_chars_2046, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_2047), u_mk_tuple(2, &l_u_string_2048, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_2049), u_mk_tuple(2, &l_u_ord_2050, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_2051), u_mk_tuple(2, &l_u_chr_2052, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__2053), u_mk_tuple(2, &l_u_concat_2054, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_2055), u_mk_tuple(2, &l_u_str_size_2056, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_2057), u_mk_tuple(2, &l_u_str_join_2058, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_2059), u_mk_tuple(2, &l_u_str_slice_2060, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_2061), u_mk_tuple(2, &l_u_str_split_2062, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_2063), u_mk_tuple(2, &l_u_str_get_2064, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_2065), u_mk_tuple(2, &l_u_str_contains_2066, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_2067), u_mk_tuple(2, &l_u_str_compare_2068, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_2069), u_mk_tuple(2, &l_u_str_show_2070, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_2071), u_mk_tuple(2, &l_u_str_quote_2072, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_2073), u_mk_tuple(2, &l_u_str_number_2074, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_2075), u_mk_tuple(2, &l_u_is_digit_2076, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_2077), u_mk_tuple(2, &l_u_is_letter_2078, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_2079), u_mk_tuple(2, &l_u_is_space_2080, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))), f_map_46_empty_0());
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__2081 = U_LIT("&");
  static struct u_object l_n__2082 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__2081, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__2082, f_compile_46_mangle_1(v_n))));
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
  u_value s2083 = v_n;
  u_value t2084 = NULL;
  if ((!t2084) && (u_is_con(s2083, &n_syntax_46_Name) && u_is_con(u_field(s2083, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s2083, 0), 0);
    u_value v_b = u_field(s2083, 1);
    static struct u_object l_const_32_struct_32_u_n_2085 = U_LIT("const struct u_name");
    static struct u_object l_n__2086 = U_LIT("n_");
    t2084 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2085), u_concat(&l_n__2086, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t2084) && (u_is_con(s2083, &n_syntax_46_Name) && u_is_con(u_field(s2083, 0), &n_option_46_None))) {
    u_value v_b = u_field(s2083, 1);
    static struct u_object l_const_32_struct_32_u_n_2087 = U_LIT("const struct u_name");
    static struct u_object l_n__2088 = U_LIT("n_");
    static struct u_object l_NULL_2089 = U_LIT("NULL");
    t2084 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2087), u_concat(&l_n__2088, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2089), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t2084) {
    u_no_case(s2083);
  }
  return t2084;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s2090 = v_cs;
  u_value t2091 = NULL;
  if ((!t2091) && u_is_con(s2090, &n_list_46_Nil)) {
    t2091 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2091) && (u_is_con(s2090, &n_list_46_Cons) && u_is_con(u_field(s2090, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s2090, 0), 2);
    u_value v_rest = u_field(s2090, 1);
    u_value t2092 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2093 = &t2092;
    u_value l2094 = v_ks;
    while (u_is_con(l2094, &n_list_46_Cons)) {
      u_value v_k = u_field(l2094, 0);
      if (u_truth(u_mk_bool(1))) {
        e2093 = u_append(e2093, f_syntax_46_Constr_46_name_1(v_k));
      }
      l2094 = u_field(l2094, 1);
    }
    t2091 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t2092, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t2091) && u_is_con(s2090, &n_list_46_Cons)) {
    u_value v_rest = u_field(s2090, 1);
    t2091 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t2091) {
    u_no_case(s2090);
  }
  return t2091;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s2095 = v_ks;
  u_value t2096 = NULL;
  if ((!t2096) && u_is_con(s2095, &n_list_46_Nil)) {
    t2096 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2096) && u_is_con(s2095, &n_list_46_Cons)) {
    u_value v_k = u_field(s2095, 0);
    u_value v_rest = u_field(s2095, 1);
    u_value t2097;
    if (u_truth(f_set_46_member_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t2097 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t2097 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t2096 = t2097;
  }
  if (!t2096) {
    u_no_case(s2095);
  }
  return t2096;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t2098 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2099 = &t2098;
  u_value l2100 = v_xs;
  while (u_is_con(l2100, &n_list_46_Cons)) {
    u_value v_x = u_field(l2100, 0);
    if (u_truth(u_mk_bool(1))) {
      e2099 = u_append(e2099, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l2100 = u_field(l2100, 1);
  }
  return t2098;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__2101 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__2101, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s2102 = f_list_46_lookup_2(v_x, v_env);
  u_value t2103 = NULL;
  if ((!t2103) && u_is_con(s2102, &n_option_46_Some)) {
    u_value v_c = u_field(s2102, 0);
    t2103 = v_c;
  }
  if (!t2103) {
    t2103 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t2103) {
    u_no_case(s2102);
  }
  return t2103;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s2104 = v_c;
  u_value t2105 = NULL;
  if ((!t2105) && (u_is_con(s2104, &n_c_46_CeUnary) && u_equals(u_field(s2104, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s2104, 1);
    t2105 = v_p;
  }
  if (!t2105) {
    static struct u_object l__38__2106 = U_LIT("&");
    t2105 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2106, v_c);
  }
  if (!t2105) {
    u_no_case(s2104);
  }
  return t2105;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s2107 = v_t;
  u_value t2108 = NULL;
  if ((!t2108) && (u_is_con(s2107, &n_syntax_46_TSort) && u_is_con(u_field(s2107, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s2107, 0);
    t2108 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t2108) && u_is_con(s2107, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s2107, 0);
    u_value v_args = u_field(s2107, 1);
    static struct u_object l__91__2109 = U_LIT("[");
    static struct u_object l__44__32__2110 = U_LIT(", ");
    u_value t2111 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2112 = &t2111;
    u_value l2113 = v_args;
    while (u_is_con(l2113, &n_list_46_Cons)) {
      u_value v_a = u_field(l2113, 0);
      if (u_truth(u_mk_bool(1))) {
        e2112 = u_append(e2112, f_compile_46_type_key_1(v_a));
      }
      l2113 = u_field(l2113, 1);
    }
    static struct u_object l__93__2114 = U_LIT("]");
    t2108 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__2109, u_concat(u_str_join(&l__44__32__2110, t2111), &l__93__2114)));
  }
  if ((!t2108) && u_is_con(s2107, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s2107, 0);
    u_value v_r = u_field(s2107, 1);
    static struct u_object l__40__2115 = U_LIT("(");
    static struct u_object l__44__32__2116 = U_LIT(", ");
    u_value t2117 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2118 = &t2117;
    u_value l2119 = v_args;
    while (u_is_con(l2119, &n_list_46_Cons)) {
      u_value v_a = u_field(l2119, 0);
      if (u_truth(u_mk_bool(1))) {
        e2118 = u_append(e2118, f_compile_46_type_key_1(v_a));
      }
      l2119 = u_field(l2119, 1);
    }
    static struct u_object l__41__32__45__62__32__2120 = U_LIT(") -> ");
    t2108 = u_concat(&l__40__2115, u_concat(u_str_join(&l__44__32__2116, t2117), u_concat(&l__41__32__45__62__32__2120, f_compile_46_type_key_1(v_r))));
  }
  if ((!t2108) && u_is_con(s2107, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s2107, 0);
    u_value v_os = u_field(s2107, 1);
    u_value v_rs = u_field(s2107, 2);
    static struct u_object l__40_in_32__2121 = U_LIT("(in ");
    static struct u_object l__44__32__2122 = U_LIT(", ");
    u_value t2123 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2124 = &t2123;
    u_value l2125 = v_is;
    while (u_is_con(l2125, &n_list_46_Cons)) {
      u_value v_a = u_field(l2125, 0);
      if (u_truth(u_mk_bool(1))) {
        e2124 = u_append(e2124, f_compile_46_type_key_1(v_a));
      }
      l2125 = u_field(l2125, 1);
    }
    static struct u_object l__44__32_out_32__2126 = U_LIT(", out ");
    static struct u_object l__44__32__2127 = U_LIT(", ");
    u_value t2128 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2129 = &t2128;
    u_value l2130 = v_os;
    while (u_is_con(l2130, &n_list_46_Cons)) {
      u_value v_a = u_field(l2130, 0);
      if (u_truth(u_mk_bool(1))) {
        e2129 = u_append(e2129, f_compile_46_type_key_1(v_a));
      }
      l2130 = u_field(l2130, 1);
    }
    static struct u_object l__44__32_ref_32__2131 = U_LIT(", ref ");
    static struct u_object l__44__32__2132 = U_LIT(", ");
    u_value t2133 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2134 = &t2133;
    u_value l2135 = v_rs;
    while (u_is_con(l2135, &n_list_46_Cons)) {
      u_value v_a = u_field(l2135, 0);
      if (u_truth(u_mk_bool(1))) {
        e2134 = u_append(e2134, f_compile_46_type_key_1(v_a));
      }
      l2135 = u_field(l2135, 1);
    }
    static struct u_object l__41__2136 = U_LIT(")");
    t2108 = u_concat(&l__40_in_32__2121, u_concat(u_str_join(&l__44__32__2122, t2123), u_concat(&l__44__32_out_32__2126, u_concat(u_str_join(&l__44__32__2127, t2128), u_concat(&l__44__32_ref_32__2131, u_concat(u_str_join(&l__44__32__2132, t2133), &l__41__2136))))));
  }
  if ((!t2108) && u_is_con(s2107, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s2107, 0);
    static struct u_object l__40__2137 = U_LIT("(");
    static struct u_object l__32__42__32__2138 = U_LIT(" * ");
    u_value t2139 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2140 = &t2139;
    u_value l2141 = v_ts;
    while (u_is_con(l2141, &n_list_46_Cons)) {
      u_value v_a = u_field(l2141, 0);
      if (u_truth(u_mk_bool(1))) {
        e2140 = u_append(e2140, f_compile_46_type_key_1(v_a));
      }
      l2141 = u_field(l2141, 1);
    }
    static struct u_object l__41__2142 = U_LIT(")");
    t2108 = u_concat(&l__40__2137, u_concat(u_str_join(&l__32__42__32__2138, t2139), &l__41__2142));
  }
  if (!t2108) {
    static struct u_object l__63__2143 = U_LIT("?");
    t2108 = &l__63__2143;
  }
  if (!t2108) {
    u_no_case(s2107);
  }
  return t2108;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t2144 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2145 = &t2144;
  u_value l2146 = v_cs;
  while (u_is_con(l2146, &n_list_46_Cons)) {
    u_value v_c = u_field(l2146, 0);
    if (u_truth(u_mk_bool(1))) {
      e2145 = u_append(e2145, f_compile_46_fun_arities_1(v_c));
    }
    l2146 = u_field(l2146, 1);
  }
  u_value t2147 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2148 = &t2147;
  u_value l2149 = v_cs;
  while (u_is_con(l2149, &n_list_46_Cons)) {
    u_value v_c = u_field(l2149, 0);
    if (u_truth(u_mk_bool(1))) {
      e2148 = u_append(e2148, f_compile_46_constr_arities_1(v_c));
    }
    l2149 = u_field(l2149, 1);
  }
  u_value t2154 = f_list_46_concat_1(t2144);
  u_value t2155 = f_list_46_concat_1(t2147);
  u_value t2153;
  {
    u_value v_fs = t2154;
    u_value v_ks = t2155;
    u_value t2150 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2151 = &t2150;
    u_value l2152 = v_cs;
    while (u_is_con(l2152, &n_list_46_Cons)) {
      u_value v_c = u_field(l2152, 0);
      if (u_truth(u_mk_bool(1))) {
        e2151 = u_append(e2151, f_compile_46_proc_arities_1(v_c));
      }
      l2152 = u_field(l2152, 1);
    }
    t2153 = u_mk_con(&n_compile_46_Globals, 5, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()), f_map_46_put_all_2(f_list_46_concat_1(t2150), f_map_46_empty_0()));
  }
  return t2153;
}

u_value f_compile_46_proc_arities_1(u_value v_c) {
  u_value s2156 = v_c;
  u_value t2157 = NULL;
  if ((!t2157) && u_is_con(s2156, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s2156, 0);
    u_value v_ins = u_field(s2156, 2);
    u_value v_outs = u_field(s2156, 3);
    u_value v_refs = u_field(s2156, 4);
    t2157 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, f_list_46_length_1(v_ins), u_add(f_list_46_length_1(v_outs), f_list_46_length_1(v_refs)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2157) {
    t2157 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2157) {
    u_no_case(s2156);
  }
  return t2157;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s2158 = v_fs;
  u_value t2159 = NULL;
  if ((!t2159) && u_is_con(s2158, &n_list_46_Nil)) {
    t2159 = v_m;
  }
  if ((!t2159) && u_is_con(s2158, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s2158, 0), 0);
    u_value v_k = u_field(u_field(s2158, 0), 1);
    u_value v_rest = u_field(s2158, 1);
    u_value s2160 = f_map_46_get_2(v_f, v_m);
    u_value t2161 = NULL;
    if ((!t2161) && u_is_con(s2160, &n_option_46_Some)) {
      u_value v_ks = u_field(s2160, 0);
      t2161 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t2161) && u_is_con(s2160, &n_option_46_None)) {
      t2161 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t2161) {
      u_no_case(s2160);
    }
    t2159 = t2161;
  }
  if (!t2159) {
    u_no_case(s2158);
  }
  return t2159;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s2162 = v_gl;
  u_value t2163 = NULL;
  if ((!t2163) && u_is_con(s2162, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s2162, 2);
    u_value s2164 = f_map_46_get_2(v_n, v_fm);
    u_value t2165 = NULL;
    if ((!t2165) && u_is_con(s2164, &n_option_46_Some)) {
      u_value v_ks = u_field(s2164, 0);
      t2165 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t2165) && u_is_con(s2164, &n_option_46_None)) {
      t2165 = u_mk_bool(0);
    }
    if (!t2165) {
      u_no_case(s2164);
    }
    t2163 = t2165;
  }
  if (!t2163) {
    u_no_case(s2162);
  }
  return t2163;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s2166 = v_gl;
  u_value t2167 = NULL;
  if ((!t2167) && u_is_con(s2166, &n_compile_46_Globals)) {
    u_value v_km = u_field(s2166, 3);
    t2167 = f_map_46_get_2(v_n, v_km);
  }
  if (!t2167) {
    u_no_case(s2166);
  }
  return t2167;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s2168 = v_c;
  u_value t2169 = NULL;
  if ((!t2169) && u_is_con(s2168, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s2168, 0);
    u_value v_xss = u_field(s2168, 2);
    t2169 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2169) {
    t2169 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2169) {
    u_no_case(s2168);
  }
  return t2169;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s2170 = v_c;
  u_value t2171 = NULL;
  if ((!t2171) && u_is_con(s2170, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s2170, 2);
    u_value t2172 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2173 = &t2172;
    u_value l2174 = v_ks;
    while (u_is_con(l2174, &n_list_46_Cons)) {
      u_value v_k = u_field(l2174, 0);
      if (u_truth(u_mk_bool(1))) {
        e2173 = u_append(e2173, f_compile_46_constr_arity_1(v_k));
      }
      l2174 = u_field(l2174, 1);
    }
    t2171 = t2172;
  }
  if (!t2171) {
    t2171 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2171) {
    u_no_case(s2170);
  }
  return t2171;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s2175 = v_k;
  u_value t2176 = NULL;
  if ((!t2176) && u_is_con(s2175, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s2175, 0);
    u_value v_ts = u_field(s2175, 1);
    t2176 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t2176) {
    u_no_case(s2175);
  }
  return t2176;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s2177 = v_xs;
  u_value t2178 = NULL;
  if ((!t2178) && u_is_con(s2177, &n_list_46_Nil)) {
    t2178 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2178) && u_is_con(s2177, &n_list_46_Cons)) {
    u_value v_x = u_field(s2177, 0);
    u_value v_rest = u_field(s2177, 1);
    t2178 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t2178) {
    u_no_case(s2177);
  }
  return t2178;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_2179 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_2179, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_2180 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_2180, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s2181 = v_cs;
  u_value t2182 = NULL;
  if ((!t2182) && u_is_con(s2181, &n_list_46_Nil)) {
    t2182 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t2182) && (u_is_con(s2181, &n_list_46_Cons) && u_is_con(u_field(s2181, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s2181, 0);
    t2182 = v_d;
  }
  if ((!t2182) && u_is_con(s2181, &n_list_46_Cons)) {
    u_value v_d = u_field(s2181, 0);
    u_value v_rest = u_field(s2181, 1);
    static struct u_object l__38__38__2183 = U_LIT("&&");
    t2182 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__2183, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t2182) {
    u_no_case(s2181);
  }
  return t2182;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s2184 = v_l;
  u_value t2185 = NULL;
  if ((!t2185) && u_is_con(s2184, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_2186 = U_LIT("u_mk_tuple");
    t2185 = f_c_46_c_call_2(&l_u_mk_tuple_2186, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2185) && u_is_con(s2184, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s2184, 0);
    static struct u_object l_u_mk_bool_2187 = U_LIT("u_mk_bool");
    u_value t2188;
    if (u_truth(v_b)) {
      t2188 = u_mk_int(1);
    } else {
      t2188 = u_mk_int(0);
    }
    t2185 = f_c_46_c_call_2(&l_u_mk_bool_2187, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t2188), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2185) && u_is_con(s2184, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s2184, 0);
    u_value t2189 = u_lt(u_mk_int_str("-1000000000000000000"), v_n);
    if (u_truth(t2189)) {
      t2189 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t2192;
    if (u_truth(t2189)) {
      static struct u_object l_u_mk_int_2190 = U_LIT("u_mk_int");
      t2192 = f_c_46_c_call_2(&l_u_mk_int_2190, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_2191 = U_LIT("u_mk_int_str");
      t2192 = f_c_46_c_call_2(&l_u_mk_int_str_2191, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t2185 = t2192;
  }
  if ((!t2185) && u_is_con(s2184, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s2184, 0);
    static struct u_object l_u_mk_lit_2193 = U_LIT("u_mk_lit");
    t2185 = f_c_46_c_call_2(&l_u_mk_lit_2193, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2185) && u_is_con(s2184, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s2184, 0);
    static struct u_object l_u_mk_char_2194 = U_LIT("u_mk_char");
    t2185 = f_c_46_c_call_2(&l_u_mk_char_2194, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2185) {
    u_no_case(s2184);
  }
  return t2185;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s2195 = v_p;
  u_value t2196 = NULL;
  if ((!t2196) && u_is_con(s2195, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s2195, 0);
    static struct u_object l_u_equals_2197 = U_LIT("u_equals");
    t2196 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_2197, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2196) && u_is_con(s2195, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s2195, 0);
    u_value v_ps = u_field(s2195, 1);
    static struct u_object l_u_is_con_2198 = U_LIT("u_is_con");
    t2196 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_2198, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t2196) && u_is_con(s2195, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2195, 0);
    t2196 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2196) {
    t2196 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2196) {
    u_no_case(s2195);
  }
  return t2196;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s2199 = v_ps;
  u_value t2200 = NULL;
  if ((!t2200) && u_is_con(s2199, &n_list_46_Nil)) {
    t2200 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2200) && u_is_con(s2199, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2199, 0), 0);
    u_value v_i = u_field(u_field(s2199, 0), 1);
    u_value v_rest = u_field(s2199, 1);
    t2200 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t2200) {
    u_no_case(s2199);
  }
  return t2200;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s2201 = v_p;
  u_value t2202 = NULL;
  if ((!t2202) && (u_is_con(s2201, &n_syntax_46_PVar) && u_is_con(u_field(s2201, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2201, 0), 0);
    t2202 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2202) && u_is_con(s2201, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2201, 1);
    t2202 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t2202) && u_is_con(s2201, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2201, 0);
    t2202 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2202) {
    t2202 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2202) {
    u_no_case(s2201);
  }
  return t2202;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s2203 = v_ps;
  u_value t2204 = NULL;
  if ((!t2204) && u_is_con(s2203, &n_list_46_Nil)) {
    t2204 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2204) && u_is_con(s2203, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2203, 0), 0);
    u_value v_i = u_field(u_field(s2203, 0), 1);
    u_value v_rest = u_field(s2203, 1);
    t2204 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t2204) {
    u_no_case(s2203);
  }
  return t2204;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s2205 = v_p;
  u_value t2206 = NULL;
  if ((!t2206) && (u_is_con(s2205, &n_syntax_46_PVar) && u_is_con(u_field(s2205, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2205, 0), 0);
    t2206 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2206) && u_is_con(s2205, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2205, 1);
    u_value t2207 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2208 = &t2207;
    u_value l2209 = v_ps;
    while (u_is_con(l2209, &n_list_46_Cons)) {
      u_value v_q = u_field(l2209, 0);
      if (u_truth(u_mk_bool(1))) {
        e2208 = u_append(e2208, f_compile_46_pattern_names_1(v_q));
      }
      l2209 = u_field(l2209, 1);
    }
    t2206 = f_list_46_concat_1(t2207);
  }
  if ((!t2206) && u_is_con(s2205, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2205, 0);
    u_value t2210 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2211 = &t2210;
    u_value l2212 = v_ps;
    while (u_is_con(l2212, &n_list_46_Cons)) {
      u_value v_q = u_field(l2212, 0);
      if (u_truth(u_mk_bool(1))) {
        e2211 = u_append(e2211, f_compile_46_pattern_names_1(v_q));
      }
      l2212 = u_field(l2212, 1);
    }
    t2206 = f_list_46_concat_1(t2210);
  }
  if (!t2206) {
    t2206 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2206) {
    u_no_case(s2205);
  }
  return t2206;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s2213 = v_e;
  u_value t2214 = NULL;
  if ((!t2214) && u_is_con(s2213, &n_syntax_46_ELit)) {
    t2214 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2214) && (u_is_con(s2213, &n_syntax_46_EVar) && u_is_con(u_field(s2213, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2213, 0), 0);
    u_value t2215;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t2215 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t2215 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t2214 = t2215;
  }
  if ((!t2214) && u_is_con(s2213, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s2213, 0);
    u_value v_args = u_field(s2213, 1);
    u_value t2216 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2217 = &t2216;
    u_value l2218 = v_args;
    while (u_is_con(l2218, &n_list_46_Cons)) {
      u_value v_a = u_field(l2218, 0);
      if (u_truth(u_mk_bool(1))) {
        e2217 = u_append(e2217, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2218 = u_field(l2218, 1);
    }
    t2214 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t2216));
  }
  if ((!t2214) && u_is_con(s2213, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s2213, 0);
    u_value v_b = u_field(s2213, 1);
    t2214 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t2214) && u_is_con(s2213, &n_syntax_46_EProc)) {
    u_value v_is = u_field(s2213, 0);
    u_value v_os = u_field(s2213, 1);
    u_value v_rs = u_field(s2213, 2);
    u_value v_b = u_field(s2213, 3);
    t2214 = f_compile_46_free_in_stmt_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), v_bound));
  }
  if ((!t2214) && u_is_con(s2213, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s2213, 0);
    u_value v_b = u_field(s2213, 1);
    u_value t2219 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2220 = &t2219;
    u_value l2221 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l2221, &n_list_46_Cons)) {
      u_value v_a = u_field(l2221, 0);
      if (u_truth(u_mk_bool(1))) {
        e2220 = u_append(e2220, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2221 = u_field(l2221, 1);
    }
    t2214 = f_list_46__43__43__2(f_list_46_concat_1(t2219), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t2214) && u_is_con(s2213, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s2213, 0);
    u_value v_l = u_field(s2213, 1);
    u_value v_r = u_field(s2213, 2);
    t2214 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t2214) && u_is_con(s2213, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s2213, 0);
    u_value v_cs = u_field(s2213, 1);
    u_value t2222 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2223 = &t2222;
    u_value l2224 = v_cs;
    while (u_is_con(l2224, &n_list_46_Cons)) {
      u_value v_b = u_field(l2224, 0);
      if (u_truth(u_mk_bool(1))) {
        e2223 = u_append(e2223, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l2224 = u_field(l2224, 1);
    }
    t2214 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2222));
  }
  if ((!t2214) && u_is_con(s2213, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s2213, 0);
    u_value v_xs = u_field(s2213, 1);
    u_value v_ls = u_field(s2213, 2);
    u_value v_c = u_field(s2213, 3);
    u_value t2225 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2226 = &t2225;
    u_value l2227 = v_ls;
    while (u_is_con(l2227, &n_list_46_Cons)) {
      u_value v_l = u_field(l2227, 0);
      if (u_truth(u_mk_bool(1))) {
        e2226 = u_append(e2226, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2227 = u_field(l2227, 1);
    }
    t2214 = f_list_46__43__43__2(f_list_46_concat_1(t2225), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t2214) && u_is_con(s2213, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s2213, 0);
    u_value t2228 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2229 = &t2228;
    u_value l2230 = v_es;
    while (u_is_con(l2230, &n_list_46_Cons)) {
      u_value v_a = u_field(l2230, 0);
      if (u_truth(u_mk_bool(1))) {
        e2229 = u_append(e2229, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2230 = u_field(l2230, 1);
    }
    t2214 = f_list_46_concat_1(t2228);
  }
  if ((!t2214) && u_is_con(s2213, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s2213, 0);
    t2214 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if (!t2214) {
    u_no_case(s2213);
  }
  return t2214;
}

u_value f_compile_46_free_in_stmt_2(u_value v_x, u_value v_bound) {
  u_value s2231 = v_x;
  u_value t2232 = NULL;
  if ((!t2232) && u_is_con(s2231, &n_syntax_46_SSkip)) {
    t2232 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2232) && u_is_con(s2231, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s2231, 0);
    u_value t2233 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2234 = &t2233;
    u_value l2235 = f_syntax_46_names_1(f_list_46_firsts_1(v_ps));
    while (u_is_con(l2235, &n_list_46_Cons)) {
      u_value v_y = u_field(l2235, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2234 = u_append(e2234, v_y);
      }
      l2235 = u_field(l2235, 1);
    }
    u_value t2236 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2237 = &t2236;
    u_value l2238 = f_list_46_seconds_1(v_ps);
    while (u_is_con(l2238, &n_list_46_Cons)) {
      u_value v_a = u_field(l2238, 0);
      if (u_truth(u_mk_bool(1))) {
        e2237 = u_append(e2237, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2238 = u_field(l2238, 1);
    }
    t2232 = f_list_46__43__43__2(t2233, f_list_46_concat_1(t2236));
  }
  if ((!t2232) && u_is_con(s2231, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s2231, 0);
    u_value v_ys = u_field(s2231, 1);
    u_value t2239 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2240 = &t2239;
    u_value l2241 = v_ys;
    while (u_is_con(l2241, &n_list_46_Cons)) {
      u_value v_y = u_field(l2241, 0);
      if (u_truth(u_mk_bool(1))) {
        e2240 = u_append(e2240, f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2241 = u_field(l2241, 1);
    }
    t2232 = f_list_46_concat_1(t2239);
  }
  if ((!t2232) && u_is_con(s2231, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s2231, 0);
    u_value v_l = u_field(s2231, 1);
    u_value v_r = u_field(s2231, 2);
    t2232 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_l, v_bound), f_compile_46_free_in_stmt_2(v_r, v_bound)));
  }
  if ((!t2232) && u_is_con(s2231, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s2231, 0);
    u_value v_b = u_field(s2231, 1);
    t2232 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_compile_46_free_in_stmt_2(v_b, v_bound));
  }
  if ((!t2232) && u_is_con(s2231, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s2231, 0);
    u_value v_ins = u_field(s2231, 1);
    u_value v_outs = u_field(s2231, 2);
    u_value v_refs = u_field(s2231, 3);
    u_value t2242 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2243 = &t2242;
    u_value l2244 = u_mk_con(&n_list_46_Cons, 2, v_p, f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
    while (u_is_con(l2244, &n_list_46_Cons)) {
      u_value v_y = u_field(l2244, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2243 = u_append(e2243, v_y);
      }
      l2244 = u_field(l2244, 1);
    }
    u_value t2245 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2246 = &t2245;
    u_value l2247 = v_ins;
    while (u_is_con(l2247, &n_list_46_Cons)) {
      u_value v_a = u_field(l2247, 0);
      if (u_truth(u_mk_bool(1))) {
        e2246 = u_append(e2246, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2247 = u_field(l2247, 1);
    }
    t2232 = f_list_46__43__43__2(t2242, f_list_46_concat_1(t2245));
  }
  if ((!t2232) && u_is_con(s2231, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s2231, 0);
    u_value v_bs = u_field(s2231, 1);
    u_value t2248 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2249 = &t2248;
    u_value l2250 = v_bs;
    while (u_is_con(l2250, &n_list_46_Cons)) {
      u_value v_b = u_field(l2250, 0);
      if (u_truth(u_mk_bool(1))) {
        e2249 = u_append(e2249, f_compile_46_free_in_stmt_branch_2(v_b, v_bound));
      }
      l2250 = u_field(l2250, 1);
    }
    t2232 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2248));
  }
  if ((!t2232) && u_is_con(s2231, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s2231, 0);
    t2232 = f_compile_46_free_vars_2(v_c, v_bound);
  }
  if ((!t2232) && u_is_con(s2231, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s2231, 0);
    t2232 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if ((!t2232) && u_is_con(s2231, &n_syntax_46_STry)) {
    u_value v_b = u_field(s2231, 0);
    u_value v_hs = u_field(s2231, 1);
    u_value t2251 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2252 = &t2251;
    u_value l2253 = v_hs;
    while (u_is_con(l2253, &n_list_46_Cons)) {
      u_value v_h = u_field(l2253, 0);
      if (u_truth(u_mk_bool(1))) {
        e2252 = u_append(e2252, f_compile_46_free_in_handler_2(v_h, v_bound));
      }
      l2253 = u_field(l2253, 1);
    }
    t2232 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_b, v_bound), f_list_46_concat_1(t2251));
  }
  if (!t2232) {
    u_no_case(s2231);
  }
  return t2232;
}

u_value f_compile_46_free_in_stmt_branch_2(u_value v_b, u_value v_bound) {
  u_value s2254 = v_b;
  u_value t2255 = NULL;
  if ((!t2255) && (u_is_con(s2254, &n_syntax_46_Branch) && u_is_con(u_field(s2254, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2254, 0);
    u_value v_g = u_field(u_field(s2254, 1), 0);
    u_value v_y = u_field(s2254, 2);
    t2255 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2255) && (u_is_con(s2254, &n_syntax_46_Branch) && u_is_con(u_field(s2254, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2254, 0);
    u_value v_y = u_field(s2254, 2);
    t2255 = f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2255) {
    u_no_case(s2254);
  }
  return t2255;
}

u_value f_compile_46_free_in_handler_2(u_value v_h, u_value v_bound) {
  u_value t2259 = f_list_46__43__43__2(f_compile_46_pattern_names_1(f_syntax_46_Handler_46_pat_1(v_h)), v_bound);
  u_value t2258;
  {
    u_value v_inner = t2259;
    u_value s2256 = f_syntax_46_Handler_46_guard_1(v_h);
    u_value t2257 = NULL;
    if ((!t2257) && u_is_con(s2256, &n_option_46_Some)) {
      u_value v_g = u_field(s2256, 0);
      t2257 = f_compile_46_free_vars_2(v_g, v_inner);
    }
    if ((!t2257) && u_is_con(s2256, &n_option_46_None)) {
      t2257 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t2257) {
      u_no_case(s2256);
    }
    t2258 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(f_syntax_46_Handler_46_body_1(v_h), v_inner), t2257);
  }
  return t2258;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s2260 = v_b;
  u_value t2261 = NULL;
  if ((!t2261) && (u_is_con(s2260, &n_syntax_46_Branch) && u_is_con(u_field(s2260, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2260, 0);
    u_value v_g = u_field(u_field(s2260, 1), 0);
    u_value v_e = u_field(s2260, 2);
    t2261 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2261) && (u_is_con(s2260, &n_syntax_46_Branch) && u_is_con(u_field(s2260, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2260, 0);
    u_value v_e = u_field(s2260, 2);
    t2261 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2261) {
    u_no_case(s2260);
  }
  return t2261;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s2262 = v_xss;
  u_value t2263 = NULL;
  if ((!t2263) && u_is_con(s2262, &n_list_46_Nil)) {
    t2263 = v_b;
  }
  if ((!t2263) && u_is_con(s2262, &n_list_46_Cons)) {
    u_value v_xs = u_field(s2262, 0);
    u_value v_rest = u_field(s2262, 1);
    t2263 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t2263) {
    u_no_case(s2262);
  }
  return t2263;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value t2264 = u_concat(v_stem, u_str_show(f_compile_46_Gen_46_count_1(*r_g)));
    u_value t2265 = f_compile_46_Gen_46_count_58__61__2(*r_g, u_add(f_compile_46_Gen_46_count_1(*r_g), u_mk_int(1)));
    *r_x = t2264;
    *r_g = t2265;
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value t2266 = f_compile_46_Gen_46_lifted_58__61__2(*r_g, f_list_46__43__43__2(f_compile_46_Gen_46_lifted_1(*r_g), u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
    *r_g = t2266;
  }
}

u_value f_compile_46_global_value_3(u_value v_gl, u_value v_n, u_value v_t) {
  u_value t2281;
  if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
    static struct u_object l_u_mk_closure_2267 = U_LIT("u_mk_closure");
    static struct u_object l_w__2268 = U_LIT("w_");
    t2281 = f_c_46_c_call_2(&l_u_mk_closure_2267, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__2268, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
  } else {
    u_value s2269 = f_compile_46_fields_count_2(v_gl, v_n);
    u_value t2270 = NULL;
    if ((!t2270) && (u_is_con(s2269, &n_option_46_Some) && u_equals(u_field(s2269, 0), u_mk_int(0)))) {
      static struct u_object l_u_mk_con_2271 = U_LIT("u_mk_con");
      t2270 = f_c_46_c_call_2(&l_u_mk_con_2271, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
    }
    if ((!t2270) && u_is_con(s2269, &n_option_46_Some)) {
      static struct u_object l_u_mk_closure_2272 = U_LIT("u_mk_closure");
      static struct u_object l_wc__2273 = U_LIT("wc_");
      t2270 = f_c_46_c_call_2(&l_u_mk_closure_2272, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__2273, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
    }
    if ((!t2270) && u_is_con(s2269, &n_option_46_None)) {
      u_value s2274 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
      u_value t2275 = NULL;
      if ((!t2275) && u_is_con(s2274, &n_option_46_Some)) {
        u_value v_f = u_field(u_field(s2274, 0), 0);
        static struct u_object l_u_mk_closure_2276 = U_LIT("u_mk_closure");
        static struct u_object l_w_2277 = U_LIT("w");
        t2275 = f_c_46_c_call_2(&l_u_mk_closure_2276, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_2277, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
      }
      if ((!t2275) && u_is_con(s2274, &n_option_46_None)) {
        if (u_truth(f_map_46_has_2(v_n, f_compile_46_Globals_46_procs_1(v_gl)))) {
          static struct u_object l_u_mk_proc_2278 = U_LIT("u_mk_proc");
          static struct u_object l_wp__2279 = U_LIT("wp_");
          t2275 = f_c_46_c_call_2(&l_u_mk_proc_2278, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wp__2279, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        }
      }
      if ((!t2275) && u_is_con(s2274, &n_option_46_None)) {
        static struct u_object l_the_32_variable_32__2280 = U_LIT("the variable ");
        u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__2280, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
        t2275 = NULL;
      }
      if (!t2275) {
        u_no_case(s2274);
      }
      t2270 = t2275;
    }
    if (!t2270) {
      u_no_case(s2269);
    }
    t2281 = t2270;
  }
  return t2281;
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
    u_value s2282 = v_e;
    int d2283 = 0;
    if ((!d2283) && (u_is_con(s2282, &n_syntax_46_ELit) && u_is_con(u_field(s2282, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s2282, 0), 0);
      d2283 = 1;
      {
        static struct u_object l_l__2284 = U_LIT("l_");
        u_value t2285;
        if (u_truth(u_lt(u_str_size(v_s), u_mk_int(16)))) {
          t2285 = u_str_size(v_s);
        } else {
          t2285 = u_mk_int(16);
        }
        static struct u_object l___2286 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__2284, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), t2285))), &l___2286)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__2287 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_2288 = U_LIT("U_LIT");
        static struct u_object l__38__2289 = U_LIT("&");
        u_value t2290 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__2287), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_2288, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t2291 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2289, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t2290;
        *r_r = t2291;
      }
    }
    if ((!d2283) && u_is_con(s2282, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s2282, 0);
      d2283 = 1;
      {
        u_value t2292 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2293 = f_compile_46_literal_value_1(v_l);
        *r_ss = t2292;
        *r_r = t2293;
      }
    }
    if ((!d2283) && (u_is_con(s2282, &n_syntax_46_EVar) && u_is_con(u_field(s2282, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s2282, 0), 0);
      u_value v_ty = u_field(u_field(s2282, 0), 1);
      d2283 = 1;
      {
        u_value s2294 = f_list_46_lookup_2(v_n, v_env);
        int d2295 = 0;
        if ((!d2295) && u_is_con(s2294, &n_option_46_Some)) {
          u_value v_c = u_field(s2294, 0);
          d2295 = 1;
          {
            u_value t2296 = u_mk_con(&n_list_46_Nil, 0);
            u_value t2297 = v_c;
            *r_ss = t2296;
            *r_r = t2297;
          }
        }
        if ((!d2295) && u_is_con(s2294, &n_option_46_None)) {
          d2295 = 1;
          {
            u_value t2298 = u_mk_con(&n_list_46_Nil, 0);
            u_value t2299 = f_compile_46_global_value_3(v_gl, v_n, v_ty);
            *r_ss = t2298;
            *r_r = t2299;
          }
        }
        if (!d2295) {
          u_no_case(s2294);
        }
      }
    }
    if ((!d2283) && (u_is_con(s2282, &n_syntax_46_EApp) && (u_is_con(u_field(s2282, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s2282, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s2282, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s2282, 0), 0), 1);
      u_value v_args = u_field(s2282, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d2283 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d2283) && u_is_con(s2282, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s2282, 0);
      u_value v_args = u_field(s2282, 1);
      d2283 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_2300 = U_LIT("u_apply");
        u_value t2301 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2302 = f_c_46_c_call_2(&l_u_apply_2300, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t2301;
        *r_r = t2302;
      }
    }
    if ((!d2283) && u_is_con(s2282, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s2282, 0);
      u_value v_b = u_field(s2282, 1);
      d2283 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2283) && u_is_con(s2282, &n_syntax_46_EProc)) {
      u_value v_is = u_field(s2282, 0);
      u_value v_os = u_field(s2282, 1);
      u_value v_rs = u_field(s2282, 2);
      u_value v_b = u_field(s2282, 3);
      d2283 = 1;
      {
        p_compile_46_compile_proc_lambda(v_gl, v_env, v_is, f_list_46__43__43__2(v_os, v_rs), v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2283) && u_is_con(s2282, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s2282, 0);
      u_value v_b = u_field(s2282, 1);
      d2283 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2303 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2303, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2304 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t2304;
        u_value t2305 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2305;
      }
    }
    if ((!d2283) && u_is_con(s2282, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s2282, 0);
      u_value v_l = u_field(s2282, 1);
      u_value v_rr = u_field(s2282, 2);
      d2283 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2306 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2306, &v_t, r_g);
        u_value t2307 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2307;
        u_value t2308 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2308;
      }
    }
    if ((!d2283) && u_is_con(s2282, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s2282, 0);
      u_value v_cases = u_field(s2282, 1);
      d2283 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2309 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2309, &v_d, r_g);
        static struct u_object l_t_2310 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2310, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_2311 = U_LIT("NULL");
        u_value t2312 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2311))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2312;
        static struct u_object l__33__2313 = U_LIT("!");
        static struct u_object l_u_no_case_2314 = U_LIT("u_no_case");
        u_value t2315 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2313, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2314, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2315;
        u_value t2316 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2316;
      }
    }
    if ((!d2283) && u_is_con(s2282, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s2282, 0);
      u_value v_xs = u_field(s2282, 1);
      u_value v_lists = u_field(s2282, 2);
      u_value v_c = u_field(s2282, 3);
      d2283 = 1;
      {
        static struct u_object l_t_2317 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2317, &v_t, r_g);
        static struct u_object l_e_2318 = U_LIT("e");
        p_compile_46_gen_name(&l_e_2318, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_2319 = U_LIT("u_mk_con");
        static struct u_object l__38__2320 = U_LIT("&");
        u_value t2321 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_2319, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__2320, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t2321;
        u_value t2322 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2322;
      }
    }
    if ((!d2283) && u_is_con(s2282, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s2282, 0);
      d2283 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_2323 = U_LIT("u_mk_tuple");
        u_value t2324 = f_c_46_c_call_2(&l_u_mk_tuple_2323, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t2324;
      }
    }
    if ((!d2283) && u_is_con(s2282, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s2282, 0);
      u_value v_ty = u_field(s2282, 1);
      d2283 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2325 = U_LIT("u_raise");
        static struct u_object l_NULL_2326 = U_LIT("NULL");
        u_value t2327 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2325, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        u_value t2328 = u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2326);
        *r_ss = t2327;
        *r_r = t2328;
      }
    }
    if (!d2283) {
      u_no_case(s2282);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s2329 = v_es;
    int d2330 = 0;
    if ((!d2330) && u_is_con(s2329, &n_list_46_Nil)) {
      d2330 = 1;
      {
        u_value t2331 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2332 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2331;
        *r_rs = t2332;
      }
    }
    if ((!d2330) && u_is_con(s2329, &n_list_46_Cons)) {
      u_value v_e = u_field(s2329, 0);
      u_value v_rest = u_field(s2329, 1);
      d2330 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t2333 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2334 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t2333;
        *r_rs = t2334;
      }
    }
    if (!d2330) {
      u_no_case(s2329);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s2335 = v_rs;
    int d2336 = 0;
    if ((!d2336) && u_is_con(s2335, &n_list_46_Nil)) {
      d2336 = 1;
      {
        u_value t2337 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2337;
      }
    }
    if ((!d2336) && u_is_con(s2335, &n_list_46_Cons)) {
      u_value v_r = u_field(s2335, 0);
      u_value v_rest = u_field(s2335, 1);
      d2336 = 1;
      {
        static struct u_object l_t_2338 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2338, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t2339 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t2339;
      }
    }
    if (!d2336) {
      u_no_case(s2335);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s2340 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2341 = NULL;
  if ((!t2341) && (u_is_con(u_field(s2340, 0), &n_list_46_Cons) && (u_is_con(u_field(s2340, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2340, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2340, 0), 0);
    u_value v_more = u_field(u_field(s2340, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2340, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2340, 1), 1);
    t2341 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t2341) {
    t2341 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2341) {
    u_no_case(s2340);
  }
  return t2341;
}

void p_compile_46_compile_named(u_value v_gl, u_value v_env, u_value v_n, u_value v_ty, u_value v_args, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value v_ra = NULL;
    u_value v_rb = NULL;
    u_value v_rs = NULL;
    u_value v_t = NULL;
    u_value s2342 = v_args;
    int d2343 = 0;
    if ((!d2343) && (u_is_con(s2342, &n_list_46_Cons) && (u_is_con(u_field(s2342, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2342, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s2342, 0);
      u_value v_b = u_field(u_field(s2342, 1), 0);
      static struct u_object l_and_2350 = U_LIT("and");
      u_value t2352 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2350));
      if (!u_truth(t2352)) {
        static struct u_object l_or_2351 = U_LIT("or");
        t2352 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_2351));
      }
      if (u_truth(t2352)) {
        d2343 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_2344 = U_LIT("t");
          p_compile_46_gen_name(&l_t_2344, &v_t, r_g);
          static struct u_object l_and_2345 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2345)))) {
            {
              u_value t2346 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2346;
            }
          } else {
            {
              static struct u_object l__33__2347 = U_LIT("!");
              u_value t2348 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2347, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2348;
            }
          }
          u_value t2349 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t2349;
        }
      }
    }
    if (!d2343) {
      d2343 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t2353 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t2353)) {
          t2353 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t2353)) {
          {
            u_value t2354 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t2354;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_2355 = U_LIT("u_mk_con");
              u_value t2356 = f_c_46_c_call_2(&l_u_mk_con_2355, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t2356;
            }
          } else {
            {
              u_value s2357 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
              int d2358 = 0;
              if ((!d2358) && u_is_con(s2357, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s2357, 0), 0);
                u_value v_k = u_field(u_field(s2357, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d2358 = 1;
                  {
                    u_value t2359 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t2359;
                  }
                }
              }
              if (!d2358) {
                d2358 = 1;
                {
                  static struct u_object l_u_apply_2360 = U_LIT("u_apply");
                  u_value t2361 = f_c_46_c_call_2(&l_u_apply_2360, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_global_value_3(v_gl, v_n, v_ty), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t2361;
                }
              }
              if (!d2358) {
                u_no_case(s2357);
              }
            }
          }
        }
      }
    }
    if (!d2343) {
      u_no_case(s2342);
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
    u_value t2362 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2363 = &t2362;
    u_value l2364 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l2364, &n_list_46_Cons)) {
      u_value v_x = u_field(l2364, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2363 = u_append(e2363, v_x);
      }
      l2364 = u_field(l2364, 1);
    }
    u_value t2365 = t2362;
    v_caps = t2365;
    static struct u_object l_lam_2366 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_2366, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_2367 = U_LIT("env");
    static struct u_object l_args_2368 = U_LIT("args");
    u_value t2369 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2367), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_2368));
    v_decls = t2369;
    static struct u_object l_env_2370 = U_LIT("env");
    static struct u_object l_args_2371 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2370), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2371), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_2372 = U_LIT("u_mk_closure");
    u_value t2373 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2374 = &t2373;
    u_value l2375 = v_caps;
    while (u_is_con(l2375, &n_list_46_Cons)) {
      u_value v_x = u_field(l2375, 0);
      if (u_truth(u_mk_bool(1))) {
        e2374 = u_append(e2374, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2375 = u_field(l2375, 1);
    }
    u_value t2376 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2377 = f_c_46_c_call_2(&l_u_mk_closure_2372, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2373));
    *r_ss = t2376;
    *r_r = t2377;
  }
}

void p_compile_46_compile_proc_lambda(u_value v_gl, u_value v_env, u_value v_ins, u_value v_outs, u_value v_b, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_caps = NULL;
    u_value v_f = NULL;
    u_value v_bs = NULL;
    u_value v_decls = NULL;
    u_value t2378 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2379 = &t2378;
    u_value l2380 = f_list_46_distinct_1(f_compile_46_free_in_stmt_2(v_b, f_syntax_46_names_1(f_list_46__43__43__2(v_ins, v_outs))));
    while (u_is_con(l2380, &n_list_46_Cons)) {
      u_value v_x = u_field(l2380, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2379 = u_append(e2379, v_x);
      }
      l2380 = u_field(l2380, 1);
    }
    u_value t2381 = t2378;
    v_caps = t2381;
    static struct u_object l_proc_2382 = U_LIT("proc");
    p_compile_46_gen_name(&l_proc_2382, &v_f, r_g);
    u_value t2383 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2384 = &t2383;
    u_value l2385 = f_syntax_46_names_1(v_outs);
    while (u_is_con(l2385, &n_list_46_Cons)) {
      u_value v_x = u_field(l2385, 0);
      if (u_truth(u_mk_bool(1))) {
        e2384 = u_append(e2384, f_compile_46_pointer_1(v_x));
      }
      l2385 = u_field(l2385, 1);
    }
    p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_ins))), t2383), v_b, &v_bs, r_g);
    static struct u_object l_env_2386 = U_LIT("env");
    static struct u_object l_ins_2387 = U_LIT("ins");
    u_value t2388 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2386), f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_ins), u_mk_int(0)), &l_ins_2387), f_compile_46_unpack_pointers_1(f_compile_46_numbered_2(f_syntax_46_names_1(v_outs), u_mk_int(0)))));
    v_decls = t2388;
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), v_f, f_compile_46_proc_params_0(), f_list_46__43__43__2(v_decls, v_bs)), r_g);
    static struct u_object l_u_mk_proc_2389 = U_LIT("u_mk_proc");
    u_value t2390 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2391 = &t2390;
    u_value l2392 = v_caps;
    while (u_is_con(l2392, &n_list_46_Cons)) {
      u_value v_x = u_field(l2392, 0);
      if (u_truth(u_mk_bool(1))) {
        e2391 = u_append(e2391, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2392 = u_field(l2392, 1);
    }
    u_value t2393 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2394 = f_c_46_c_call_2(&l_u_mk_proc_2389, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2390));
    *r_ss = t2393;
    *r_r = t2394;
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
  static struct u_object l_env_2395 = U_LIT("env");
  static struct u_object l_ins_2396 = U_LIT("ins");
  static struct u_object l_outs_2397 = U_LIT("outs");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2395), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_ins_2396), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, f_compile_46_value_ptr_0()), &l_outs_2397), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_compile_46_unpack_pointers_1(u_value v_xs) {
  u_value s2398 = v_xs;
  u_value t2399 = NULL;
  if ((!t2399) && u_is_con(s2398, &n_list_46_Nil)) {
    t2399 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2399) && u_is_con(s2398, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2398, 0), 0);
    u_value v_i = u_field(u_field(s2398, 0), 1);
    u_value v_rest = u_field(s2398, 1);
    static struct u_object l_outs_2400 = U_LIT("outs");
    t2399 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(&l_outs_2400, v_i))), f_compile_46_unpack_pointers_1(v_rest));
  }
  if (!t2399) {
    u_no_case(s2398);
  }
  return t2399;
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s2401 = v_xs;
  u_value t2402 = NULL;
  if ((!t2402) && u_is_con(s2401, &n_list_46_Nil)) {
    t2402 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2402) && u_is_con(s2401, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2401, 0), 0);
    u_value v_i = u_field(u_field(s2401, 0), 1);
    u_value v_rest = u_field(s2401, 1);
    t2402 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t2402) {
    u_no_case(s2401);
  }
  return t2402;
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
    u_value s2403 = v_cases;
    int d2404 = 0;
    if ((!d2404) && u_is_con(s2403, &n_list_46_Nil)) {
      d2404 = 1;
      {
        u_value t2405 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2405;
      }
    }
    if ((!d2404) && (u_is_con(s2403, &n_list_46_Cons) && u_is_con(u_field(s2403, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2403, 0), 0);
      u_value v_guard = u_field(u_field(s2403, 0), 1);
      u_value v_e = u_field(u_field(s2403, 0), 2);
      u_value v_more = u_field(s2403, 1);
      d2404 = 1;
      {
        u_value t2406 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2406;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2407 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t2407;
        u_value s2408 = v_guard;
        int d2409 = 0;
        if ((!d2409) && u_is_con(s2408, &n_option_46_Some)) {
          u_value v_c = u_field(s2408, 0);
          d2409 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2410 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2410;
          }
        }
        if ((!d2409) && u_is_con(s2408, &n_option_46_None)) {
          d2409 = 1;
          {
          }
        }
        if (!d2409) {
          u_no_case(s2408);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2411 = U_LIT("!");
        u_value t2412 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2411, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2412;
      }
    }
    if (!d2404) {
      u_no_case(s2403);
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
    u_value s2413 = u_mk_tuple(2, v_xs, v_lists);
    int d2414 = 0;
    if ((!d2414) && (u_is_con(u_field(s2413, 0), &n_list_46_Cons) && u_is_con(u_field(s2413, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2413, 0), 0);
      u_value v_more = u_field(u_field(s2413, 0), 1);
      u_value v_list = u_field(u_field(s2413, 1), 0);
      u_value v_others = u_field(u_field(s2413, 1), 1);
      d2414 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2415 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2415, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2416 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2416;
        static struct u_object l_u_is_con_2417 = U_LIT("u_is_con");
        u_value t2418 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2417, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2418;
      }
    }
    if (!d2414) {
      d2414 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2419 = U_LIT("u_append");
        u_value t2420 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2419, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2420;
      }
    }
    if (!d2414) {
      u_no_case(s2413);
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
    u_value s2421 = v_s;
    int d2422 = 0;
    if ((!d2422) && u_is_con(s2421, &n_syntax_46_SSkip)) {
      d2422 = 1;
      {
        u_value t2423 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2423;
      }
    }
    if ((!d2422) && u_is_con(s2421, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2421, 0);
      d2422 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2424 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2424;
      }
    }
    if ((!d2422) && u_is_con(s2421, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2421, 0);
      u_value v_body = u_field(s2421, 1);
      d2422 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t2425 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2426 = &t2425;
        u_value l2427 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2427, &n_list_46_Cons)) {
          u_value v_x = u_field(l2427, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2428 = U_LIT("NULL");
            e2426 = u_append(e2426, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2428))));
          }
          l2427 = u_field(l2427, 1);
        }
        u_value t2429 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2425, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2429;
      }
    }
    if ((!d2422) && u_is_con(s2421, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2421, 0);
      u_value v_l = u_field(s2421, 1);
      u_value v_r = u_field(s2421, 2);
      d2422 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2430 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2430;
      }
    }
    if ((!d2422) && u_is_con(s2421, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2421, 0);
      u_value v_b = u_field(s2421, 1);
      d2422 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2431 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2431, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2432 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2432;
      }
    }
    if ((!d2422) && u_is_con(s2421, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2421, 0);
      u_value v_ins = u_field(s2421, 1);
      u_value v_outs = u_field(s2421, 2);
      u_value v_refs = u_field(s2421, 3);
      d2422 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        if (u_truth(f_list_46_has_key_2(v_p, v_env))) {
          {
            static struct u_object l_u_call_proc_2433 = U_LIT("u_call_proc");
            u_value t2434 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e2435 = &t2434;
            u_value l2436 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
            while (u_is_con(l2436, &n_list_46_Cons)) {
              u_value v_x = u_field(l2436, 0);
              if (u_truth(u_mk_bool(1))) {
                e2435 = u_append(e2435, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
              }
              l2436 = u_field(l2436, 1);
            }
            u_value t2437 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_call_proc_2433, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_var_expr_2(v_env, v_p), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_ins)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(f_list_46__43__43__2(v_outs, v_refs))), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(v_rs, t2434)))), u_mk_con(&n_list_46_Nil, 0)));
            *r_ss = t2437;
          }
        } else {
          {
            u_value t2438 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e2439 = &t2438;
            u_value l2440 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
            while (u_is_con(l2440, &n_list_46_Cons)) {
              u_value v_x = u_field(l2440, 0);
              if (u_truth(u_mk_bool(1))) {
                e2439 = u_append(e2439, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
              }
              l2440 = u_field(l2440, 1);
            }
            u_value t2441 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2438))), u_mk_con(&n_list_46_Nil, 0)));
            *r_ss = t2441;
          }
        }
      }
    }
    if ((!d2422) && u_is_con(s2421, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2421, 0);
      u_value v_cases = u_field(s2421, 1);
      d2422 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2442 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2442, &v_d, r_g);
        static struct u_object l_d_2443 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2443, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2444 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2444;
        static struct u_object l__33__2445 = U_LIT("!");
        static struct u_object l_u_no_case_2446 = U_LIT("u_no_case");
        u_value t2447 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2445, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2446, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2447;
      }
    }
    if ((!d2422) && (u_is_con(s2421, &n_syntax_46_SAssert) && (u_is_con(u_field(s2421, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2421, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2421, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2421, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2421, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2421, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2421, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2421, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2421, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2421, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2421, 0), 1), 1), 0);
      d2422 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2448 = U_LIT("u_assert_equal");
        u_value t2449 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2448, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2449;
      }
    }
    if ((!d2422) && u_is_con(s2421, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2421, 0);
      d2422 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2450 = U_LIT("u_assert_true");
        u_value t2451 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2450, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2451;
      }
    }
    if ((!d2422) && u_is_con(s2421, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2421, 0);
      u_value v_ty = u_field(s2421, 1);
      d2422 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2452 = U_LIT("u_raise");
        u_value t2453 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2452, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2453;
      }
    }
    if ((!d2422) && u_is_con(s2421, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2421, 0);
      u_value v_hs = u_field(s2421, 1);
      d2422 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2454 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2454, &v_d, r_g);
        static struct u_object l_d_2455 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2455, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2456 = U_LIT("u_pop");
        static struct u_object l__33__2457 = U_LIT("!");
        static struct u_object l_u_reraise_2458 = U_LIT("u_reraise");
        u_value t2459 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2456, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2457, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2458, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2459;
        static struct u_object l_struct_32_u_handler_2460 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2461 = U_LIT("u_push");
        static struct u_object l__61__61__2462 = U_LIT("==");
        static struct u_object l_setjmp_2463 = U_LIT("setjmp");
        static struct u_object l_env_2464 = U_LIT("env");
        static struct u_object l_u_pop_2465 = U_LIT("u_pop");
        u_value t2466 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2460)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2461, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2462, f_c_46_c_call_2(&l_setjmp_2463, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2464), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2465, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2466;
      }
    }
    if (!d2422) {
      u_no_case(s2421);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2467 = v_xs;
    int d2468 = 0;
    if ((!d2468) && u_is_con(s2467, &n_list_46_Nil)) {
      d2468 = 1;
      {
        u_value t2469 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2469;
      }
    }
    if ((!d2468) && u_is_con(s2467, &n_list_46_Cons)) {
      u_value v_x = u_field(s2467, 0);
      u_value v_rest = u_field(s2467, 1);
      d2468 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2470 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2470;
      }
    }
    if (!d2468) {
      u_no_case(s2467);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2471 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2472 = NULL;
  if ((!t2472) && (u_is_con(u_field(s2471, 0), &n_list_46_Cons) && (u_is_con(u_field(s2471, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2471, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2471, 0), 0);
    u_value v_more = u_field(u_field(s2471, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2471, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2471, 1), 1);
    t2472 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2472) {
    t2472 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2472) {
    u_no_case(s2471);
  }
  return t2472;
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
    static struct u_object l_u_caught_2473 = U_LIT("u_caught");
    u_value t2474 = f_c_46_c_call_2(&l_u_caught_2473, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2474;
    u_value s2475 = v_hs;
    int d2476 = 0;
    if ((!d2476) && u_is_con(s2475, &n_list_46_Nil)) {
      d2476 = 1;
      {
        u_value t2477 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2477;
      }
    }
    if ((!d2476) && (u_is_con(s2475, &n_list_46_Cons) && u_is_con(u_field(s2475, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2475, 0), 0);
      u_value v_p = u_field(u_field(s2475, 0), 1);
      u_value v_guard = u_field(u_field(s2475, 0), 2);
      u_value v_s = u_field(u_field(s2475, 0), 3);
      u_value v_more = u_field(s2475, 1);
      d2476 = 1;
      {
        u_value t2478 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2478;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2479 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2479;
        u_value s2480 = v_guard;
        int d2481 = 0;
        if ((!d2481) && u_is_con(s2480, &n_option_46_Some)) {
          u_value v_c = u_field(s2480, 0);
          d2481 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2482 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2482;
          }
        }
        if ((!d2481) && u_is_con(s2480, &n_option_46_None)) {
          d2481 = 1;
          {
          }
        }
        if (!d2481) {
          u_no_case(s2480);
        }
        u_value s2483 = v_p;
        int d2484 = 0;
        if ((!d2484) && u_is_con(s2483, &n_syntax_46_PWild)) {
          d2484 = 1;
          {
            u_value t2485 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2485;
          }
        }
        if (!d2484) {
          d2484 = 1;
          {
            static struct u_object l_u_has_type_2486 = U_LIT("u_has_type");
            u_value t2487 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2486, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2487;
          }
        }
        if (!d2484) {
          u_no_case(s2483);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2488 = U_LIT("!");
        u_value t2489 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2488, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2489;
      }
    }
    if (!d2476) {
      u_no_case(s2475);
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
    u_value s2490 = v_cases;
    int d2491 = 0;
    if ((!d2491) && u_is_con(s2490, &n_list_46_Nil)) {
      d2491 = 1;
      {
        u_value t2492 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2492;
      }
    }
    if ((!d2491) && (u_is_con(s2490, &n_list_46_Cons) && u_is_con(u_field(s2490, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2490, 0), 0);
      u_value v_guard = u_field(u_field(s2490, 0), 1);
      u_value v_s = u_field(u_field(s2490, 0), 2);
      u_value v_more = u_field(s2490, 1);
      d2491 = 1;
      {
        u_value t2493 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2493;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2494 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2494;
        u_value s2495 = v_guard;
        int d2496 = 0;
        if ((!d2496) && u_is_con(s2495, &n_option_46_Some)) {
          u_value v_c = u_field(s2495, 0);
          d2496 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2497 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2497;
          }
        }
        if ((!d2496) && u_is_con(s2495, &n_option_46_None)) {
          d2496 = 1;
          {
          }
        }
        if (!d2496) {
          u_no_case(s2495);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2498 = U_LIT("!");
        u_value t2499 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2498, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2499;
      }
    }
    if (!d2491) {
      u_no_case(s2490);
    }
  }
}

void p_compile_46_compile_cmd(u_value v_gl, u_value v_entry, u_value v_c, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_bs = NULL;
    u_value v_rb = NULL;
    u_value v_params = NULL;
    u_value s2500 = v_c;
    int d2501 = 0;
    if ((!d2501) && u_is_con(s2500, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2500, 0);
      u_value v_xss = u_field(s2500, 2);
      u_value v_b = u_field(s2500, 4);
      d2501 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2502 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2503 = &t2502;
        u_value l2504 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2504, &n_list_46_Cons)) {
          u_value v_x = u_field(l2504, 0);
          if (u_truth(u_mk_bool(1))) {
            e2503 = u_append(e2503, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2504 = u_field(l2504, 1);
        }
        u_value t2505 = t2502;
        v_params = t2505;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2506 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2506;
          }
        } else {
          {
            u_value t2507 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2507;
          }
        }
        u_value t2508 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2509 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2508;
        *r_ms = t2509;
      }
    }
    if ((!d2501) && u_is_con(s2500, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2500, 0);
      u_value v_ins = u_field(s2500, 2);
      u_value v_outs = u_field(s2500, 3);
      u_value v_refs = u_field(s2500, 4);
      u_value v_body = u_field(s2500, 5);
      d2501 = 1;
      {
        u_value t2510 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2511 = &t2510;
        u_value l2512 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2512, &n_list_46_Cons)) {
          u_value v_x = u_field(l2512, 0);
          if (u_truth(u_mk_bool(1))) {
            e2511 = u_append(e2511, f_compile_46_pointer_1(v_x));
          }
          l2512 = u_field(l2512, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2510), v_body, &v_bs, r_g);
        u_value t2513 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2514 = &t2513;
        u_value l2515 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2515, &n_list_46_Cons)) {
          u_value v_x = u_field(l2515, 0);
          if (u_truth(u_mk_bool(1))) {
            e2514 = u_append(e2514, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2515 = u_field(l2515, 1);
        }
        u_value t2516 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2517 = &t2516;
        u_value l2518 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2518, &n_list_46_Cons)) {
          u_value v_x = u_field(l2518, 0);
          if (u_truth(u_mk_bool(1))) {
            e2517 = u_append(e2517, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2518 = u_field(l2518, 1);
        }
        u_value t2519 = f_list_46__43__43__2(t2513, t2516);
        v_params = t2519;
        u_value t2520 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2521 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2520;
        *r_ms = t2521;
      }
    }
    if ((!d2501) && u_is_con(s2500, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s2500, 0);
      u_value v_body = u_field(s2500, 1);
      static struct u_object l_test_2525 = U_LIT("test");
      if (u_truth(u_eq(v_entry, &l_test_2525))) {
        d2501 = 1;
        {
          p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
          static struct u_object l_u_passed_2522 = U_LIT("u_passed");
          u_value t2523 = u_mk_con(&n_list_46_Nil, 0);
          u_value t2524 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_passed_2522, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
          *r_ds = t2523;
          *r_ms = t2524;
        }
      }
    }
    if (!d2501) {
      d2501 = 1;
      {
        u_value t2526 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2527 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2526;
        *r_ms = t2527;
      }
    }
    if (!d2501) {
      u_no_case(s2500);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_entry, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2528 = v_cs;
    int d2529 = 0;
    if ((!d2529) && u_is_con(s2528, &n_list_46_Nil)) {
      d2529 = 1;
      {
        u_value t2530 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2531 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2530;
        *r_ms = t2531;
      }
    }
    if ((!d2529) && u_is_con(s2528, &n_list_46_Cons)) {
      u_value v_c = u_field(s2528, 0);
      u_value v_rest = u_field(s2528, 1);
      d2529 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_entry, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_entry, v_rest, &v_d2, &v_m2, r_g);
        u_value t2532 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2533 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2532;
        *r_ms = t2533;
      }
    }
    if (!d2529) {
      u_no_case(s2528);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_handler_2534 = U_LIT("handler");
  u_value t2561 = u_mk_con(&n_c_46_CeVar, 1, &l_handler_2534);
  u_value t2560;
  {
    u_value v_h = t2561;
    static struct u_object l_static_32_u_value_2535 = U_LIT("static u_value");
    static struct u_object l_cache_2536 = U_LIT("cache");
    static struct u_object l_NULL_2537 = U_LIT("NULL");
    static struct u_object l__61__61__2538 = U_LIT("==");
    static struct u_object l_cache_2539 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2540 = U_LIT("U_COMPUTING");
    static struct u_object l_u_fail_2541 = U_LIT("u_fail");
    static struct u_object l_recursive_32_initia_2542 = U_LIT("recursive initialization of ");
    static struct u_object l_cache_2543 = U_LIT("cache");
    static struct u_object l_cache_2544 = U_LIT("cache");
    static struct u_object l_cache_2545 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2546 = U_LIT("U_COMPUTING");
    static struct u_object l_struct_32_u_handler_2547 = U_LIT("struct u_handler");
    static struct u_object l_handler_2548 = U_LIT("handler");
    static struct u_object l_u_push_2549 = U_LIT("u_push");
    static struct u_object l__33__61__2550 = U_LIT("!=");
    static struct u_object l_setjmp_2551 = U_LIT("setjmp");
    static struct u_object l_env_2552 = U_LIT("env");
    static struct u_object l_u_pop_2553 = U_LIT("u_pop");
    static struct u_object l_cache_2554 = U_LIT("cache");
    static struct u_object l_NULL_2555 = U_LIT("NULL");
    static struct u_object l_u_reraise_2556 = U_LIT("u_reraise");
    static struct u_object l_u_pop_2557 = U_LIT("u_pop");
    static struct u_object l_cache_2558 = U_LIT("cache");
    static struct u_object l_cache_2559 = U_LIT("cache");
    t2560 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2535), &l_cache_2536, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2537))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2538, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2539), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2540)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2541, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2542, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2543), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2544))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2545), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2546)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2547)), &l_handler_2548, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2549, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__33__61__2550, f_c_46_c_call_2(&l_setjmp_2551, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, v_h, &l_env_2552), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2553, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2554), u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2555)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2556, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2557, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2558), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2559))), u_mk_con(&n_list_46_Nil, 0))))));
  }
  return t2560;
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2562 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2563 = &t2562;
  u_value l2564 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l2564, &n_list_46_Cons)) {
    u_value v_p = u_field(l2564, 0);
    if (u_truth(u_mk_bool(1))) {
      e2563 = u_append(e2563, f_compile_46_fun_wrapper_1(v_p));
    }
    l2564 = u_field(l2564, 1);
  }
  u_value t2565 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2566 = &t2565;
  u_value l2567 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l2567, &n_list_46_Cons)) {
    u_value v_p = u_field(l2567, 0);
    if (u_truth(u_mk_bool(1))) {
      e2566 = u_append(e2566, f_compile_46_constr_wrapper_1(v_p));
    }
    l2567 = u_field(l2567, 1);
  }
  u_value t2568 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2569 = &t2568;
  u_value l2570 = f_list_46_seconds_1(f_map_46_entries_1(f_compile_46_builtin_functions_0()));
  while (u_is_con(l2570, &n_list_46_Cons)) {
    u_value v_p = u_field(l2570, 0);
    if (u_truth(u_mk_bool(1))) {
      e2569 = u_append(e2569, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2570 = u_field(l2570, 1);
  }
  u_value t2571 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2572 = &t2571;
  u_value l2573 = f_map_46_entries_1(f_compile_46_Globals_46_procs_1(v_gl));
  while (u_is_con(l2573, &n_list_46_Cons)) {
    u_value v_p = u_field(l2573, 0);
    if (u_truth(u_mk_bool(1))) {
      e2572 = u_append(e2572, f_compile_46_proc_wrapper_1(v_p));
    }
    l2573 = u_field(l2573, 1);
  }
  return f_list_46__43__43__2(t2562, f_list_46__43__43__2(f_list_46_concat_1(t2565), f_list_46__43__43__2(t2568, t2571)));
}

u_value f_compile_46_proc_wrapper_1(u_value v_p) {
  u_value s2574 = v_p;
  u_value t2575 = NULL;
  if (!t2575) {
    u_value v_n = u_field(s2574, 0);
    u_value v_k = u_field(u_field(s2574, 1), 0);
    u_value v_m = u_field(u_field(s2574, 1), 1);
    static struct u_object l_wp__2576 = U_LIT("wp_");
    u_value t2577 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2578 = &t2577;
    u_value l2579 = f_compile_46_range_to_1(v_k);
    while (u_is_con(l2579, &n_list_46_Cons)) {
      u_value v_i = u_field(l2579, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_ins_2580 = U_LIT("ins");
        e2578 = u_append(e2578, f_compile_46_index_2(&l_ins_2580, v_i));
      }
      l2579 = u_field(l2579, 1);
    }
    u_value t2581 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2582 = &t2581;
    u_value l2583 = f_compile_46_range_to_1(v_m);
    while (u_is_con(l2583, &n_list_46_Cons)) {
      u_value v_i = u_field(l2583, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_outs_2584 = U_LIT("outs");
        e2582 = u_append(e2582, f_compile_46_index_2(&l_outs_2584, v_i));
      }
      l2583 = u_field(l2583, 1);
    }
    t2575 = u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), u_concat(&l_wp__2576, f_compile_46_mangle_1(v_n)), f_compile_46_proc_params_0(), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_n), f_list_46__43__43__2(t2577, t2581))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2575) {
    u_no_case(s2574);
  }
  return t2575;
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2585 = v_p;
  u_value t2586 = NULL;
  if (!t2586) {
    u_value v_n = u_field(s2585, 0);
    u_value v_k = u_field(s2585, 1);
    static struct u_object l_w__2587 = U_LIT("w_");
    t2586 = f_compile_46_wrapper_3(u_concat(&l_w__2587, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2586) {
    u_no_case(s2585);
  }
  return t2586;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2588 = v_p;
  u_value t2589 = NULL;
  if (!t2589) {
    u_value v_f = u_field(s2588, 0);
    u_value v_k = u_field(s2588, 1);
    static struct u_object l_w_2590 = U_LIT("w");
    t2589 = f_compile_46_wrapper_3(u_concat(&l_w_2590, v_f), v_f, v_k);
  }
  if (!t2589) {
    u_no_case(s2588);
  }
  return t2589;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2591 = U_LIT("env");
  static struct u_object l_args_2592 = U_LIT("args");
  u_value t2593 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2594 = &t2593;
  u_value l2595 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2595, &n_list_46_Cons)) {
    u_value v_i = u_field(l2595, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2596 = U_LIT("args");
      e2594 = u_append(e2594, f_compile_46_index_2(&l_args_2596, v_i));
    }
    l2595 = u_field(l2595, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2591), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2592), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2593))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2597 = v_p;
  u_value t2598 = NULL;
  if ((!t2598) && u_equals(u_field(s2597, 1), u_mk_int(0))) {
    t2598 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2598) {
    u_value v_n = u_field(s2597, 0);
    u_value v_k = u_field(s2597, 1);
    static struct u_object l_wc__2599 = U_LIT("wc_");
    static struct u_object l_env_2600 = U_LIT("env");
    static struct u_object l_args_2601 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2602 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2603 = U_LIT("args");
    t2598 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2599, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2600), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2601), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2602, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2603), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2598) {
    u_no_case(s2597);
  }
  return t2598;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2604;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2604 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2604 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2604;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2605 = v_d;
  u_value t2606 = NULL;
  if ((!t2606) && u_is_con(s2605, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2605, 0);
    u_value v_f = u_field(s2605, 1);
    u_value v_ps = u_field(s2605, 2);
    t2606 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2606) {
    t2606 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2606) {
    u_no_case(s2605);
  }
  return t2606;
}

void p_compile_46_compile_program(u_value v_q, u_value v_m, u_value v_entry, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2607 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2607;
    u_value t2608 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t2608;
    static struct u_object l_test_2609 = U_LIT("test");
    u_value t2611 = f_prelude_46__33__61__2(v_entry, &l_test_2609);
    if (u_truth(t2611)) {
      t2611 = f_prelude_46_not_1(f_list_46_exists_2(f_syntax_46_Program_46_cmds_1(v_q), u_mk_closure(lam2610, 2, v_m, v_entry)));
    }
    if (u_truth(t2611)) {
      {
        u_raise(u_mk_con(&n_compile_46_UnknownEntry, 1, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), v_entry)), "compile.CompileError");
      }
    }
    p_compile_46_compile_cmds(v_gl, v_entry, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    static struct u_object l_test_2612 = U_LIT("test");
    if (u_truth(f_prelude_46__33__61__2(v_entry, &l_test_2612))) {
      {
        u_value t2613 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), v_entry)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
        v_ms = t2613;
      }
    }
    u_value t2614 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, f_compile_46_Gen_46_lifted_1(v_g)));
    v_fs = t2614;
    static struct u_object l_ulang_46_h_2615 = U_LIT("ulang.h");
    u_value t2616 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2617 = &t2616;
    u_value l2618 = v_fs;
    while (u_is_con(l2618, &n_list_46_Cons)) {
      u_value v_d = u_field(l2618, 0);
      if (u_truth(u_mk_bool(1))) {
        e2617 = u_append(e2617, f_compile_46_prototype_1(v_d));
      }
      l2618 = u_field(l2618, 1);
    }
    static struct u_object l_main_2619 = U_LIT("main");
    static struct u_object l_argc_2620 = U_LIT("argc");
    static struct u_object l_char_2621 = U_LIT("char");
    static struct u_object l_argv_2622 = U_LIT("argv");
    static struct u_object l_u_args_2623 = U_LIT("u_args");
    static struct u_object l_argc_2624 = U_LIT("argc");
    static struct u_object l_argv_2625 = U_LIT("argv");
    u_value t2626 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2615), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2616), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2619, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2620), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2621))), &l_argv_2622), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2623, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2624), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2625), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2626;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_entry, u_value v_target) {
  {
    u_value v_cache = NULL;
    u_value t2627 = u_mk_con(&n_list_46_Nil, 0);
    v_cache = t2627;
    p_compile_46_compile_with(v_source, v_entry, v_target, &v_cache);
  }
}

u_value f_compile_46_is_entry_2(u_value v_c, u_value v_n) {
  u_value s2628 = v_c;
  u_value t2629 = NULL;
  if ((!t2629) && (u_is_con(s2628, &n_syntax_46_CProc) && (u_is_con(u_field(s2628, 2), &n_list_46_Nil) && (u_is_con(u_field(s2628, 3), &n_list_46_Nil) && u_is_con(u_field(s2628, 4), &n_list_46_Nil))))) {
    u_value v_k = u_field(s2628, 0);
    t2629 = u_eq(v_k, v_n);
  }
  if (!t2629) {
    t2629 = u_mk_bool(0);
  }
  if (!t2629) {
    u_no_case(s2628);
  }
  return t2629;
}

void p_compile_46_compile_with(u_value v_source, u_value v_entry, u_value v_target, u_value *r_cache) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2630 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2630, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2631 = U_LIT("prelude");
    p_check_46_check_modules(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2631, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q, r_cache);
    p_compile_46_compile_program(v_q, f_parse_46_base_name_1(v_source), v_entry, &v_f);
    p_os_write(v_target, f_c_46_c_file_1(v_f));
  }
}

void p_ulang_46_main(void) {
  {
    u_value v_args = NULL;
    p_os_args(&v_args);
    u_value s2632 = v_args;
    int d2633 = 0;
    if ((!d2633) && u_is_con(s2632, &n_list_46_Nil)) {
      d2633 = 1;
      {
      }
    }
    if ((!d2633) && (u_is_con(s2632, &n_list_46_Cons) && (u_equals(u_field(s2632, 0), u_mk_lit("compile")) && (u_is_con(u_field(s2632, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2632, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2632, 1), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2632, 1), 1), 1), 1), &n_list_46_Nil))))))) {
      u_value v_source = u_field(u_field(s2632, 1), 0);
      u_value v_entry = u_field(u_field(u_field(s2632, 1), 1), 0);
      u_value v_target = u_field(u_field(u_field(u_field(s2632, 1), 1), 1), 0);
      d2633 = 1;
      {
        p_compile_46_compile_file(v_source, v_entry, v_target);
      }
    }
    if (!d2633) {
      d2633 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_com_2634 = U_LIT("usage: ulang compile <file> <entry> <target.c>");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_com_2634), "os.OsError");
      }
    }
    if (!d2633) {
      u_no_case(s2632);
    }
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

u_value lam807(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_q = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_q);
}

u_value lam1137(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

u_value lam1827(u_value *env, u_value *args) {
  u_value v_t = env[0];
  u_value v_e = args[0];
  return f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t);
}

u_value lam2610(u_value *env, u_value *args) {
  u_value v_m = env[0];
  u_value v_entry = env[1];
  u_value v_c = args[0];
  return f_compile_46_is_entry_2(v_c, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), v_entry));
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  p_ulang_46_main();
  return 0;
}
