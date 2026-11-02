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

u_value lam807(u_value *env, u_value *args);

u_value lam1138(u_value *env, u_value *args);

u_value lam1829(u_value *env, u_value *args);

u_value lam2616(u_value *env, u_value *args);

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
    if ((!d881) && u_is_con(s880, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s880, 0);
      u_value v_body = u_field(s880, 1);
      d881 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t884 = u_mk_con(&n_syntax_46_CRun, 2, v_n, v_y);
        *r_c1 = t884;
      }
    }
    if ((!d881) && u_is_con(s880, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s880, 0);
      u_value v_body = u_field(s880, 1);
      d881 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t885 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t885;
      }
    }
    if (!d881) {
      d881 = 1;
      {
        u_value t886 = v_c;
        *r_c1 = t886;
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
    u_value s887 = v_cs;
    int d888 = 0;
    if ((!d888) && u_is_con(s887, &n_list_46_Nil)) {
      d888 = 1;
      {
        u_value t889 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t889;
      }
    }
    if ((!d888) && u_is_con(s887, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s887, 0), 0);
      u_value v_c = u_field(u_field(s887, 0), 1);
      u_value v_more = u_field(s887, 1);
      d888 = 1;
      {
        u_value t890 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t890;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t891 = f_check_46_zonk_cmd_2(f_check_46_State_46_sub_1(*r_s), v_c1);
        v_c2 = t891;
        u_value s892 = f_check_46_unknown_in_cmd_1(v_c2);
        int d893 = 0;
        if ((!d893) && u_is_con(s892, &n_option_46_Some)) {
          u_value v_t = u_field(s892, 0);
          d893 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d893) && u_is_con(s892, &n_option_46_None)) {
          d893 = 1;
          {
          }
        }
        if (!d893) {
          u_no_case(s892);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t894 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t894;
      }
    }
    if (!d888) {
      u_no_case(s887);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s895 = v_cs;
  u_value t896 = NULL;
  if ((!t896) && u_is_con(s895, &n_list_46_Nil)) {
    t896 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t896) && (u_is_con(s895, &n_list_46_Cons) && u_is_con(u_field(u_field(s895, 0), 1), &n_syntax_46_CRun))) {
    u_value v_n = u_field(u_field(u_field(s895, 0), 1), 0);
    u_value v_more = u_field(s895, 1);
    t896 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t896) && (u_is_con(s895, &n_list_46_Cons) && u_is_con(u_field(u_field(s895, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s895, 0), 1), 0);
    u_value v_more = u_field(s895, 1);
    t896 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t896) && u_is_con(s895, &n_list_46_Cons)) {
    u_value v_more = u_field(s895, 1);
    t896 = f_check_46_block_names_1(v_more);
  }
  if (!t896) {
    u_no_case(s895);
  }
  return t896;
}

u_value f_check_46_Checked_46_cmds_1(u_value v_x) {
  u_value s897 = v_x;
  u_value t898 = NULL;
  if ((!t898) && u_is_con(s897, &n_check_46_Checked)) {
    u_value v_y = u_field(s897, 0);
    t898 = v_y;
  }
  if (!t898) {
    u_no_case(s897);
  }
  return t898;
}

u_value f_check_46_Checked_46_ctx_1(u_value v_x) {
  u_value s899 = v_x;
  u_value t900 = NULL;
  if ((!t900) && u_is_con(s899, &n_check_46_Checked)) {
    u_value v_y = u_field(s899, 1);
    t900 = v_y;
  }
  if (!t900) {
    u_no_case(s899);
  }
  return t900;
}

u_value f_check_46_Checked_46_imports_1(u_value v_x) {
  u_value s901 = v_x;
  u_value t902 = NULL;
  if ((!t902) && u_is_con(s901, &n_check_46_Checked)) {
    u_value v_y = u_field(s901, 2);
    t902 = v_y;
  }
  if (!t902) {
    u_no_case(s901);
  }
  return t902;
}

u_value f_check_46_Checked_46_cmds_58__61__2(u_value v_x, u_value v_y) {
  u_value s903 = v_x;
  u_value t904 = NULL;
  if ((!t904) && u_is_con(s903, &n_check_46_Checked)) {
    u_value v_a1 = u_field(s903, 1);
    u_value v_a2 = u_field(s903, 2);
    t904 = u_mk_con(&n_check_46_Checked, 3, v_y, v_a1, v_a2);
  }
  if (!t904) {
    u_no_case(s903);
  }
  return t904;
}

u_value f_check_46_Checked_46_ctx_58__61__2(u_value v_x, u_value v_y) {
  u_value s905 = v_x;
  u_value t906 = NULL;
  if ((!t906) && u_is_con(s905, &n_check_46_Checked)) {
    u_value v_a0 = u_field(s905, 0);
    u_value v_a2 = u_field(s905, 2);
    t906 = u_mk_con(&n_check_46_Checked, 3, v_a0, v_y, v_a2);
  }
  if (!t906) {
    u_no_case(s905);
  }
  return t906;
}

u_value f_check_46_Checked_46_imports_58__61__2(u_value v_x, u_value v_y) {
  u_value s907 = v_x;
  u_value t908 = NULL;
  if ((!t908) && u_is_con(s907, &n_check_46_Checked)) {
    u_value v_a0 = u_field(s907, 0);
    u_value v_a1 = u_field(s907, 1);
    t908 = u_mk_con(&n_check_46_Checked, 3, v_a0, v_a1, v_y);
  }
  if (!t908) {
    u_no_case(s907);
  }
  return t908;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_cache = NULL;
    u_value t909 = u_mk_con(&n_list_46_Nil, 0);
    v_cache = t909;
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
    u_value t910 = f_ctx_46_empty_ctx_0();
    v_c = t910;
    u_value t911 = u_mk_con(&n_list_46_Nil, 0);
    v_imports = t911;
    u_value t912 = u_mk_con(&n_list_46_Nil, 0);
    v_seen = t912;
    u_value t913 = u_mk_con(&n_list_46_Nil, 0);
    v_cs = t913;
    p_check_46_check_units(f_check_46_modules_1(f_syntax_46_Program_46_cmds_1(v_p)), &v_c, &v_imports, &v_seen, &v_cs, r_cache);
    p_check_46_check_unit(f_check_46_own_decls_2(v_main, f_syntax_46_Program_46_cmds_1(v_p)), v_main, v_c, v_imports, &v_own);
    u_value t914 = u_mk_con(&n_syntax_46_Program, 1, f_list_46__43__43__2(v_cs, f_check_46_Checked_46_cmds_1(v_own)));
    *r_q = t914;
  }
}

void p_check_46_check_units(u_value v_us, u_value *r_c, u_value *r_imports, u_value *r_seen, u_value *r_cs, u_value *r_cache) {
  {
    u_value v_key = NULL;
    u_value v_checked = NULL;
    u_value v_i = NULL;
    u_value s915 = v_us;
    int d916 = 0;
    if ((!d916) && u_is_con(s915, &n_list_46_Nil)) {
      d916 = 1;
      {
      }
    }
    if ((!d916) && u_is_con(s915, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s915, 0), 0);
      u_value v_ds = u_field(u_field(s915, 0), 1);
      u_value v_rest = u_field(s915, 1);
      d916 = 1;
      {
        static struct u_object l_prelude_917 = U_LIT("prelude");
        u_value t918 = u_mk_tuple(3, v_m, v_ds, f_check_46_positions_of_2(u_mk_con(&n_list_46_Cons, 2, &l_prelude_917, f_check_46_imports_in_2(v_ds, v_m)), *r_seen));
        v_key = t918;
        u_value s919 = f_check_46_position_in_3(v_key, *r_cache, u_mk_int(0));
        int d920 = 0;
        if ((!d920) && u_is_con(s919, &n_option_46_Some)) {
          u_value v_k = u_field(u_field(s919, 0), 0);
          u_value v_done = u_field(u_field(s919, 0), 1);
          d920 = 1;
          {
            u_value t921 = v_k;
            u_value t922 = v_done;
            v_i = t921;
            v_checked = t922;
          }
        }
        if ((!d920) && u_is_con(s919, &n_option_46_None)) {
          d920 = 1;
          {
            p_check_46_check_unit(v_ds, v_m, *r_c, *r_imports, &v_checked);
            u_value t923 = f_list_46_length_1(*r_cache);
            u_value t924 = f_list_46__43__43__2(*r_cache, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_key, v_checked), u_mk_con(&n_list_46_Nil, 0)));
            v_i = t923;
            *r_cache = t924;
          }
        }
        if (!d920) {
          u_no_case(s919);
        }
        u_value t925 = f_ctx_46_ctx_union_2(*r_c, f_check_46_Checked_46_ctx_1(v_checked));
        u_value t926 = f_list_46__43__43__2(*r_imports, f_check_46_Checked_46_imports_1(v_checked));
        u_value t927 = f_list_46__43__43__2(*r_cs, f_check_46_Checked_46_cmds_1(v_checked));
        *r_c = t925;
        *r_imports = t926;
        *r_cs = t927;
        u_value t928 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e929 = &t928;
        u_value l930 = f_list_46_firsts_1(f_check_46_Checked_46_imports_1(v_checked));
        while (u_is_con(l930, &n_list_46_Cons)) {
          u_value v_n = u_field(l930, 0);
          if (u_truth(u_mk_bool(1))) {
            e929 = u_append(e929, u_mk_tuple(2, v_n, v_i));
          }
          l930 = u_field(l930, 1);
        }
        u_value t931 = f_list_46__43__43__2(t928, *r_seen);
        *r_seen = t931;
        p_check_46_check_units(v_rest, r_c, r_imports, r_seen, r_cs, r_cache);
      }
    }
    if (!d916) {
      u_no_case(s915);
    }
  }
}

u_value f_check_46_positions_of_2(u_value v_ms, u_value v_seen) {
  u_value s932 = v_ms;
  u_value t933 = NULL;
  if ((!t933) && u_is_con(s932, &n_list_46_Nil)) {
    t933 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t933) && u_is_con(s932, &n_list_46_Cons)) {
    u_value v_m = u_field(s932, 0);
    u_value v_rest = u_field(s932, 1);
    u_value s934 = f_list_46_lookup_2(v_m, v_seen);
    u_value t935 = NULL;
    if ((!t935) && u_is_con(s934, &n_option_46_Some)) {
      u_value v_i = u_field(s934, 0);
      t935 = u_mk_con(&n_list_46_Cons, 2, v_i, f_check_46_positions_of_2(v_rest, v_seen));
    }
    if ((!t935) && u_is_con(s934, &n_option_46_None)) {
      t935 = f_check_46_positions_of_2(v_rest, v_seen);
    }
    if (!t935) {
      u_no_case(s934);
    }
    t933 = t935;
  }
  if (!t933) {
    u_no_case(s932);
  }
  return t933;
}

u_value f_check_46_position_in_3(u_value v_key, u_value v_cache, u_value v_i) {
  u_value s936 = v_cache;
  u_value t937 = NULL;
  if ((!t937) && u_is_con(s936, &n_list_46_Nil)) {
    t937 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t937) && u_is_con(s936, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s936, 0), 0);
    u_value v_checked = u_field(u_field(s936, 0), 1);
    u_value v_rest = u_field(s936, 1);
    u_value t938;
    if (u_truth(u_eq(v_k, v_key))) {
      t938 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_i, v_checked));
    } else {
      t938 = f_check_46_position_in_3(v_key, v_rest, u_add(v_i, u_mk_int(1)));
    }
    t937 = t938;
  }
  if (!t937) {
    u_no_case(s936);
  }
  return t937;
}

void p_check_46_check_unit(u_value v_ds, u_value v_main, u_value v_before, u_value v_imports, u_value *r_checked) {
  {
    u_value v_own = NULL;
    u_value v_s = NULL;
    u_value v_cs = NULL;
    u_value t939 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e940 = &t939;
    u_value l941 = f_list_46_distinct_1(f_list_46_firsts_1(v_ds));
    while (u_is_con(l941, &n_list_46_Cons)) {
      u_value v_m = u_field(l941, 0);
      if (u_truth(u_mk_bool(1))) {
        e940 = u_append(e940, u_mk_tuple(2, v_m, f_check_46_imports_in_2(v_ds, v_m)));
      }
      l941 = u_field(l941, 1);
    }
    u_value t942 = f_list_46__43__43__2(v_imports, t939);
    v_own = t942;
    u_value t943 = u_mk_con(&n_check_46_State, 6, v_before, f_map_46_empty_0(), u_mk_int(0), v_main, u_mk_con(&n_list_46_Nil, 0), f_check_46_visibility_2(v_own, v_main));
    v_s = t943;
    p_check_46_declare_datas(v_ds, &v_s);
    p_check_46_declare_sorts(v_ds, &v_s);
    p_check_46_declare_constrs(v_ds, &v_s);
    p_check_46_declare_funs(v_ds, &v_s);
    p_check_46_declare_procs(v_ds, &v_s);
    p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
    p_check_46_commands(v_ds, &v_cs, &v_s);
    u_value t944 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e945 = &t944;
    u_value l946 = f_list_46_distinct_1(f_list_46_firsts_1(v_ds));
    while (u_is_con(l946, &n_list_46_Cons)) {
      u_value v_m = u_field(l946, 0);
      if (u_truth(u_mk_bool(1))) {
        e945 = u_append(e945, u_mk_tuple(2, v_m, f_check_46_imports_in_2(v_ds, v_m)));
      }
      l946 = u_field(l946, 1);
    }
    u_value t947 = u_mk_con(&n_check_46_Checked, 3, v_cs, f_ctx_46_ctx_of_2(f_check_46_State_46_ctx_1(v_s), f_list_46_distinct_1(f_list_46_firsts_1(v_ds))), t944);
    *r_checked = t947;
  }
}

u_value f_check_46_modules_1(u_value v_cs) {
  u_value s948 = v_cs;
  u_value t949 = NULL;
  if ((!t949) && u_is_con(s948, &n_list_46_Nil)) {
    t949 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t949) && (u_is_con(s948, &n_list_46_Cons) && (u_is_con(u_field(s948, 0), &n_syntax_46_CModule) && u_equals(u_field(u_field(s948, 0), 0), u_mk_lit("prelude"))))) {
    u_value v_ds = u_field(u_field(s948, 0), 1);
    u_value v_rest = u_field(s948, 1);
    static struct u_object l_prelude_950 = U_LIT("prelude");
    static struct u_object l_prelude_951 = U_LIT("prelude");
    t949 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, &l_prelude_950, f_check_46_flatten_2(&l_prelude_951, v_ds)), f_check_46_modules_1(v_rest));
  }
  if ((!t949) && (u_is_con(s948, &n_list_46_Cons) && u_is_con(u_field(s948, 0), &n_syntax_46_CModule))) {
    u_value v_n = u_field(u_field(s948, 0), 0);
    u_value v_ds = u_field(u_field(s948, 0), 1);
    u_value v_rest = u_field(s948, 1);
    t949 = f_list_46__43__43__2(f_check_46_modules_1(v_ds), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_check_46_own_decls_2(v_n, v_ds)), f_check_46_modules_1(v_rest)));
  }
  if ((!t949) && u_is_con(s948, &n_list_46_Cons)) {
    u_value v_rest = u_field(s948, 1);
    t949 = f_check_46_modules_1(v_rest);
  }
  if (!t949) {
    u_no_case(s948);
  }
  return t949;
}

u_value f_check_46_own_decls_2(u_value v_m, u_value v_cs) {
  u_value t952 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e953 = &t952;
  u_value l954 = v_cs;
  while (u_is_con(l954, &n_list_46_Cons)) {
    u_value v_c = u_field(l954, 0);
    if (u_truth(f_prelude_46_not_1(f_check_46_is_module_1(v_c)))) {
      e953 = u_append(e953, u_mk_tuple(2, v_m, f_check_46_qualify_2(v_m, v_c)));
    }
    l954 = u_field(l954, 1);
  }
  return t952;
}

u_value f_check_46_is_module_1(u_value v_c) {
  u_value s955 = v_c;
  u_value t956 = NULL;
  if ((!t956) && u_is_con(s955, &n_syntax_46_CModule)) {
    t956 = u_mk_bool(1);
  }
  if (!t956) {
    t956 = u_mk_bool(0);
  }
  if (!t956) {
    u_no_case(s955);
  }
  return t956;
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_957 = U_LIT("main");
    u_value t958 = f_check_46_start_2(&l_main_957, u_mk_con(&n_list_46_Nil, 0));
    v_s = t958;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t959 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(v_s), v_u);
    *r_t = t959;
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
  static struct u_object l_List_960 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_960), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_961 = U_LIT("list");
  static struct u_object l_List_962 = U_LIT("List");
  static struct u_object l_A_963 = U_LIT("A");
  static struct u_object l_Nil_964 = U_LIT("Nil");
  static struct u_object l_Cons_965 = U_LIT("Cons");
  static struct u_object l_A_966 = U_LIT("A");
  static struct u_object l_A_967 = U_LIT("A");
  static struct u_object l_list_968 = U_LIT("list");
  static struct u_object l_len_969 = U_LIT("len");
  static struct u_object l_A_970 = U_LIT("A");
  static struct u_object l_xs_971 = U_LIT("xs");
  static struct u_object l_A_972 = U_LIT("A");
  static struct u_object l_xs_973 = U_LIT("xs");
  static struct u_object l_Nil_974 = U_LIT("Nil");
  static struct u_object l_Cons_975 = U_LIT("Cons");
  static struct u_object l_ys_976 = U_LIT("ys");
  static struct u_object l__43__977 = U_LIT("+");
  static struct u_object l_len_978 = U_LIT("len");
  static struct u_object l_ys_979 = U_LIT("ys");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_961, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_962), u_mk_con(&n_list_46_Cons, 2, &l_A_963, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_964), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_965), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_966), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_967)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_968), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_969), u_mk_con(&n_list_46_Cons, 2, &l_A_970, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_971), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_972))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_973), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_974), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_975), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_976), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__977, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_978, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_979), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s980 = v_t;
  u_value t981 = NULL;
  if ((!t981) && u_is_con(s980, &n_c_46_CtVoid)) {
    static struct u_object l_void_982 = U_LIT("void");
    t981 = &l_void_982;
  }
  if ((!t981) && u_is_con(s980, &n_c_46_CtInt)) {
    static struct u_object l_int_983 = U_LIT("int");
    t981 = &l_int_983;
  }
  if ((!t981) && u_is_con(s980, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s980, 0);
    t981 = v_n;
  }
  if ((!t981) && (u_is_con(s980, &n_c_46_CtPtr) && u_is_con(u_field(s980, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s980, 0), 0);
    static struct u_object l__42__984 = U_LIT("*");
    t981 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__984);
  }
  if ((!t981) && u_is_con(s980, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s980, 0);
    static struct u_object l__32__42__985 = U_LIT(" *");
    t981 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__985);
  }
  if (!t981) {
    u_no_case(s980);
  }
  return t981;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s986 = v_t;
  u_value t987 = NULL;
  if ((!t987) && u_is_con(s986, &n_c_46_CtPtr)) {
    t987 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t987) {
    static struct u_object l__32__988 = U_LIT(" ");
    t987 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__988, v_x));
  }
  if (!t987) {
    u_no_case(s986);
  }
  return t987;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s989 = v_e;
  u_value t990 = NULL;
  if ((!t990) && u_is_con(s989, &n_c_46_CeVar)) {
    u_value v_x = u_field(s989, 0);
    t990 = v_x;
  }
  if ((!t990) && u_is_con(s989, &n_c_46_CeNum)) {
    u_value v_n = u_field(s989, 0);
    t990 = u_str_show(v_n);
  }
  if ((!t990) && u_is_con(s989, &n_c_46_CeStr)) {
    u_value v_s = u_field(s989, 0);
    t990 = u_str_quote(v_s);
  }
  if ((!t990) && u_is_con(s989, &n_c_46_CeCall)) {
    u_value v_f = u_field(s989, 0);
    u_value v_args = u_field(s989, 1);
    static struct u_object l__40__991 = U_LIT("(");
    static struct u_object l__44__32__992 = U_LIT(", ");
    u_value t993 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e994 = &t993;
    u_value l995 = v_args;
    while (u_is_con(l995, &n_list_46_Cons)) {
      u_value v_a = u_field(l995, 0);
      if (u_truth(u_mk_bool(1))) {
        e994 = u_append(e994, f_c_46_c_expr_1(v_a));
      }
      l995 = u_field(l995, 1);
    }
    static struct u_object l__41__996 = U_LIT(")");
    t990 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__991, u_concat(u_str_join(&l__44__32__992, t993), &l__41__996)));
  }
  if ((!t990) && u_is_con(s989, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s989, 0);
    u_value v_a = u_field(s989, 1);
    t990 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t990) && u_is_con(s989, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s989, 0);
    u_value v_a = u_field(s989, 1);
    u_value v_b = u_field(s989, 2);
    static struct u_object l__32__997 = U_LIT(" ");
    static struct u_object l__32__998 = U_LIT(" ");
    t990 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__997, u_concat(v_o, u_concat(&l__32__998, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t990) && u_is_con(s989, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s989, 0);
    u_value v_i = u_field(s989, 1);
    static struct u_object l__91__999 = U_LIT("[");
    static struct u_object l__93__1000 = U_LIT("]");
    t990 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__999, u_concat(f_c_46_c_expr_1(v_i), &l__93__1000)));
  }
  if ((!t990) && u_is_con(s989, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s989, 0);
    u_value v_f = u_field(s989, 1);
    static struct u_object l__45__62__1001 = U_LIT("->");
    t990 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__1001, v_f));
  }
  if ((!t990) && u_is_con(s989, &n_c_46_CeCast)) {
    u_value v_t = u_field(s989, 0);
    u_value v_a = u_field(s989, 1);
    static struct u_object l__40__1002 = U_LIT("(");
    static struct u_object l__41__1003 = U_LIT(")");
    t990 = u_concat(&l__40__1002, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__1003, f_c_46_c_operand_1(v_a))));
  }
  if ((!t990) && u_is_con(s989, &n_c_46_CeCond)) {
    u_value v_c = u_field(s989, 0);
    u_value v_a = u_field(s989, 1);
    u_value v_b = u_field(s989, 2);
    static struct u_object l__32__63__32__1004 = U_LIT(" ? ");
    static struct u_object l__32__58__32__1005 = U_LIT(" : ");
    t990 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__1004, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__1005, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t990) && u_is_con(s989, &n_c_46_CeInit)) {
    u_value v_es = u_field(s989, 0);
    static struct u_object l__123__1006 = U_LIT("{");
    static struct u_object l__44__32__1007 = U_LIT(", ");
    u_value t1008 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1009 = &t1008;
    u_value l1010 = v_es;
    while (u_is_con(l1010, &n_list_46_Cons)) {
      u_value v_a = u_field(l1010, 0);
      if (u_truth(u_mk_bool(1))) {
        e1009 = u_append(e1009, f_c_46_c_expr_1(v_a));
      }
      l1010 = u_field(l1010, 1);
    }
    static struct u_object l__125__1011 = U_LIT("}");
    t990 = u_concat(&l__123__1006, u_concat(u_str_join(&l__44__32__1007, t1008), &l__125__1011));
  }
  if (!t990) {
    u_no_case(s989);
  }
  return t990;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s1012 = v_e;
  u_value t1013 = NULL;
  if ((!t1013) && u_is_con(s1012, &n_c_46_CeVar)) {
    t1013 = f_c_46_c_expr_1(v_e);
  }
  if ((!t1013) && u_is_con(s1012, &n_c_46_CeNum)) {
    u_value v_n = u_field(s1012, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t1013 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t1013) && u_is_con(s1012, &n_c_46_CeStr)) {
    t1013 = f_c_46_c_expr_1(v_e);
  }
  if ((!t1013) && u_is_con(s1012, &n_c_46_CeCall)) {
    t1013 = f_c_46_c_expr_1(v_e);
  }
  if ((!t1013) && u_is_con(s1012, &n_c_46_CeIndex)) {
    t1013 = f_c_46_c_expr_1(v_e);
  }
  if ((!t1013) && u_is_con(s1012, &n_c_46_CeArrow)) {
    t1013 = f_c_46_c_expr_1(v_e);
  }
  if (!t1013) {
    static struct u_object l__40__1014 = U_LIT("(");
    static struct u_object l__41__1015 = U_LIT(")");
    t1013 = u_concat(&l__40__1014, u_concat(f_c_46_c_expr_1(v_e), &l__41__1015));
  }
  if (!t1013) {
    u_no_case(s1012);
  }
  return t1013;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s1016 = v_s;
  u_value t1017 = NULL;
  if ((!t1017) && u_is_con(s1016, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s1016, 0);
    static struct u_object l__59__1018 = U_LIT(";");
    t1017 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__1018)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1017) && (u_is_con(s1016, &n_c_46_CsDecl) && u_is_con(u_field(s1016, 2), &n_option_46_None))) {
    u_value v_t = u_field(s1016, 0);
    u_value v_x = u_field(s1016, 1);
    static struct u_object l__59__1019 = U_LIT(";");
    t1017 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__1019)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1017) && (u_is_con(s1016, &n_c_46_CsDecl) && u_is_con(u_field(s1016, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s1016, 0);
    u_value v_x = u_field(s1016, 1);
    u_value v_e = u_field(u_field(s1016, 2), 0);
    static struct u_object l__32__61__32__1020 = U_LIT(" = ");
    static struct u_object l__59__1021 = U_LIT(";");
    t1017 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__1020, u_concat(f_c_46_c_expr_1(v_e), &l__59__1021)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1017) && u_is_con(s1016, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s1016, 0);
    u_value v_e = u_field(s1016, 1);
    static struct u_object l__32__61__32__1022 = U_LIT(" = ");
    static struct u_object l__59__1023 = U_LIT(";");
    t1017 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__1022, u_concat(f_c_46_c_expr_1(v_e), &l__59__1023)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1017) && (u_is_con(s1016, &n_c_46_CsIf) && u_is_con(u_field(s1016, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s1016, 0);
    u_value v_l = u_field(s1016, 1);
    static struct u_object l_if_32__40__1024 = U_LIT("if (");
    static struct u_object l__41__32__123__1025 = U_LIT(") {");
    static struct u_object l__32__32__1026 = U_LIT("  ");
    static struct u_object l__125__1027 = U_LIT("}");
    t1017 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__1024, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__1025))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__1026), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__1027), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t1017) && u_is_con(s1016, &n_c_46_CsIf)) {
    u_value v_c = u_field(s1016, 0);
    u_value v_l = u_field(s1016, 1);
    u_value v_r = u_field(s1016, 2);
    static struct u_object l_if_32__40__1028 = U_LIT("if (");
    static struct u_object l__41__32__123__1029 = U_LIT(") {");
    static struct u_object l__32__32__1030 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__1031 = U_LIT("} else {");
    static struct u_object l__32__32__1032 = U_LIT("  ");
    static struct u_object l__125__1033 = U_LIT("}");
    t1017 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__1028, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__1029))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__1030), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__1031), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__1032), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__1033), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t1017) && u_is_con(s1016, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s1016, 0);
    u_value v_b = u_field(s1016, 1);
    static struct u_object l_while_32__40__1034 = U_LIT("while (");
    static struct u_object l__41__32__123__1035 = U_LIT(") {");
    static struct u_object l__32__32__1036 = U_LIT("  ");
    static struct u_object l__125__1037 = U_LIT("}");
    t1017 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__1034, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__1035))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__1036), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__1037), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t1017) && (u_is_con(s1016, &n_c_46_CsReturn) && u_is_con(u_field(s1016, 0), &n_option_46_None))) {
    static struct u_object l_return_59__1038 = U_LIT("return;");
    t1017 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__1038), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1017) && (u_is_con(s1016, &n_c_46_CsReturn) && u_is_con(u_field(s1016, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s1016, 0), 0);
    static struct u_object l_return_32__1039 = U_LIT("return ");
    static struct u_object l__59__1040 = U_LIT(";");
    t1017 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__1039, u_concat(f_c_46_c_expr_1(v_e), &l__59__1040))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1017) && u_is_con(s1016, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s1016, 0);
    static struct u_object l__123__1041 = U_LIT("{");
    static struct u_object l__32__32__1042 = U_LIT("  ");
    static struct u_object l__125__1043 = U_LIT("}");
    t1017 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__1041), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__1042), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__1043), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1017) {
    u_no_case(s1016);
  }
  return t1017;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t1044 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1045 = &t1044;
  u_value l1046 = v_ss;
  while (u_is_con(l1046, &n_list_46_Cons)) {
    u_value v_s = u_field(l1046, 0);
    if (u_truth(u_mk_bool(1))) {
      e1045 = u_append(e1045, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l1046 = u_field(l1046, 1);
  }
  return f_list_46_concat_1(t1044);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s1047 = v_ps;
  u_value t1048 = NULL;
  if ((!t1048) && u_is_con(s1047, &n_list_46_Nil)) {
    static struct u_object l_void_1049 = U_LIT("void");
    t1048 = &l_void_1049;
  }
  if (!t1048) {
    static struct u_object l__44__32__1050 = U_LIT(", ");
    u_value t1051 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1052 = &t1051;
    u_value l1053 = v_ps;
    while (u_is_con(l1053, &n_list_46_Cons)) {
      u_value v_p = u_field(l1053, 0);
      if (u_truth(u_mk_bool(1))) {
        e1052 = u_append(e1052, f_c_46_c_param_1(v_p));
      }
      l1053 = u_field(l1053, 1);
    }
    t1048 = u_str_join(&l__44__32__1050, t1051);
  }
  if (!t1048) {
    u_no_case(s1047);
  }
  return t1048;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s1054 = v_p;
  u_value t1055 = NULL;
  if (!t1055) {
    u_value v_t = u_field(s1054, 0);
    u_value v_x = u_field(s1054, 1);
    t1055 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t1055) {
    u_no_case(s1054);
  }
  return t1055;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s1056 = v_d;
  u_value t1057 = NULL;
  if ((!t1057) && u_is_con(s1056, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s1056, 0);
    static struct u_object l__35_include_32__1058 = U_LIT("#include ");
    t1057 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__1058, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1057) && u_is_con(s1056, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s1056, 0);
    u_value v_f = u_field(s1056, 1);
    u_value v_ps = u_field(s1056, 2);
    static struct u_object l__40__1059 = U_LIT("(");
    static struct u_object l__41__59__1060 = U_LIT(");");
    t1057 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__1059, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__1060))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1057) && u_is_con(s1056, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s1056, 0);
    u_value v_f = u_field(s1056, 1);
    u_value v_ps = u_field(s1056, 2);
    u_value v_b = u_field(s1056, 3);
    static struct u_object l__40__1061 = U_LIT("(");
    static struct u_object l__41__32__123__1062 = U_LIT(") {");
    static struct u_object l__32__32__1063 = U_LIT("  ");
    static struct u_object l__125__1064 = U_LIT("}");
    t1057 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__1061, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__1062))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__1063, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__1064, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t1057) && (u_is_con(s1056, &n_c_46_CdGlobal) && u_is_con(u_field(s1056, 2), &n_option_46_None))) {
    u_value v_t = u_field(s1056, 0);
    u_value v_x = u_field(s1056, 1);
    static struct u_object l__59__1065 = U_LIT(";");
    t1057 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__1065), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1057) && (u_is_con(s1056, &n_c_46_CdGlobal) && u_is_con(u_field(s1056, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s1056, 0);
    u_value v_x = u_field(s1056, 1);
    u_value v_e = u_field(u_field(s1056, 2), 0);
    static struct u_object l__32__61__32__1066 = U_LIT(" = ");
    static struct u_object l__59__1067 = U_LIT(";");
    t1057 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__1066, u_concat(f_c_46_c_expr_1(v_e), &l__59__1067))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1057) {
    u_no_case(s1056);
  }
  return t1057;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s1068 = v_f;
  u_value t1069 = NULL;
  if ((!t1069) && u_is_con(s1068, &n_c_46_CFile)) {
    u_value v_ds = u_field(s1068, 0);
    static struct u_object l__10__10__1070 = U_LIT("\n\n");
    u_value t1071 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1072 = &t1071;
    u_value l1073 = v_ds;
    while (u_is_con(l1073, &n_list_46_Cons)) {
      u_value v_d = u_field(l1073, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__1074 = U_LIT("\n");
        e1072 = u_append(e1072, u_str_join(&l__10__1074, f_c_46_c_decl_1(v_d)));
      }
      l1073 = u_field(l1073, 1);
    }
    static struct u_object l__10__1075 = U_LIT("\n");
    t1069 = u_concat(u_str_join(&l__10__10__1070, t1071), &l__10__1075);
  }
  if (!t1069) {
    u_no_case(s1068);
  }
  return t1069;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__1076 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__1076, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__123__125__44__59__46__1077 = U_LIT("()[]{},;.");
  return u_str_contains(&l__40__41__91__93__123__125__44__59__46__1077, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t1078 = u_is_letter(v_c);
  if (!u_truth(t1078)) {
    t1078 = u_is_digit(v_c);
  }
  u_value t1079 = t1078;
  if (!u_truth(t1079)) {
    t1079 = u_eq(v_c, u_mk_char(95));
  }
  return t1079;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t1080;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t1080 = u_str_get(v_s, v_i);
  } else {
    t1080 = u_chr(u_mk_int(0));
  }
  return t1080;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s1081 = v_c;
  u_value t1082 = NULL;
  if ((!t1082) && u_equals(s1081, u_mk_char(34))) {
    t1082 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t1082) && u_equals(s1081, u_mk_char(39))) {
    t1082 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t1082) && u_equals(s1081, u_mk_char(92))) {
    t1082 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t1082) && u_equals(s1081, u_mk_char(110))) {
    t1082 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t1082) && u_equals(s1081, u_mk_char(116))) {
    t1082 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t1082) && u_equals(s1081, u_mk_char(114))) {
    t1082 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t1082) {
    t1082 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1082) {
    u_no_case(s1081);
  }
  return t1082;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t1083;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t1083 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t1083 = v_i;
  }
  return t1083;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t1084 = u_mk_int(0);
    u_value t1085 = u_mk_int(1);
    u_value t1086 = u_mk_int(1);
    v_j = t1084;
    *r_line = t1085;
    *r_column = t1086;
    u_value t1091 = u_lt(v_j, v_i);
    while (u_truth(t1091)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t1087 = u_add(*r_line, u_mk_int(1));
            u_value t1088 = u_mk_int(1);
            *r_line = t1087;
            *r_column = t1088;
          }
        } else {
          {
            u_value t1089 = u_add(*r_column, u_mk_int(1));
            *r_column = t1089;
          }
        }
        u_value t1090 = u_add(v_j, u_mk_int(1));
        v_j = t1090;
      }
      t1091 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t1092 = *r_i;
    u_value t1093 = u_add(*r_i, u_mk_int(1));
    u_value t1094 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t1092;
    *r_i = t1093;
    v_parts = t1094;
    u_value t1095 = *r_i;
    v_from = t1095;
    u_value t1106 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t1106)) {
      {
        u_value t1096 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t1096;
        u_value t1097 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t1097)) {
          t1097 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t1097)) {
          {
            static struct u_object l_unterminated_32_lit_1098 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_1098, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s1099 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d1100 = 0;
            if ((!d1100) && u_is_con(s1099, &n_option_46_Some)) {
              u_value v_d = u_field(s1099, 0);
              d1100 = 1;
              {
                u_value t1101 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t1102 = u_add(*r_i, u_mk_int(2));
                v_parts = t1101;
                *r_i = t1102;
                u_value t1103 = *r_i;
                v_from = t1103;
              }
            }
            if ((!d1100) && u_is_con(s1099, &n_option_46_None)) {
              d1100 = 1;
              {
                static struct u_object l_invalid_32_escape_1104 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_1104, *r_i), "lex.LexError");
              }
            }
            if (!d1100) {
              u_no_case(s1099);
            }
          }
        } else {
          {
            u_value t1105 = u_add(*r_i, u_mk_int(1));
            *r_i = t1105;
          }
        }
      }
      t1106 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__1107 = U_LIT("");
    u_value t1108 = u_str_join(&l__1107, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t1109 = u_add(*r_i, u_mk_int(1));
    *r_text = t1108;
    *r_i = t1109;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1110 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t1110;
    u_value t1111 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1112 = v_j;
    *r_t = t1111;
    *r_i = t1112;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1113 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t1113;
    u_value s1114 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d1115 = 0;
    if ((!d1115) && u_is_con(s1114, &n_option_46_Some)) {
      u_value v_n = u_field(s1114, 0);
      d1115 = 1;
      {
        u_value t1116 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t1117 = v_j;
        *r_t = t1116;
        *r_i = t1117;
      }
    }
    if ((!d1115) && u_is_con(s1114, &n_option_46_None)) {
      d1115 = 1;
      {
        static struct u_object l_invalid_32_number_1118 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_1118, *r_i), "lex.LexError");
      }
    }
    if (!d1115) {
      u_no_case(s1114);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t1119 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t1119;
    u_value t1120 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t1121 = v_j;
    *r_t = t1120;
    *r_i = t1121;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t1122 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t1122;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t1123 = *r_i;
    v_start = t1123;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_1124 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_1124, v_start), "lex.LexError");
      }
    }
    u_value t1125 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t1125;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t1126 = u_str_get(v_s, *r_i);
    v_c = t1126;
    u_value s1127 = v_c;
    int d1128 = 0;
    if (!d1128) {
      u_value t1129 = u_is_letter(v_c);
      if (!u_truth(t1129)) {
        t1129 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t1129)) {
        d1128 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d1128) {
      if (u_truth(u_is_digit(v_c))) {
        d1128 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d1128) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d1128 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d1128) && u_equals(s1127, u_mk_char(34))) {
      d1128 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d1128) && u_equals(s1127, u_mk_char(39))) {
      d1128 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d1128) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d1128 = 1;
        {
          u_value t1130 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t1131 = u_add(*r_i, u_mk_int(1));
          *r_t = t1130;
          *r_i = t1131;
        }
      }
    }
    if (!d1128) {
      d1128 = 1;
      {
        static struct u_object l_unexpected_32_chara_1132 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_1132, *r_i), "lex.LexError");
      }
    }
    if (!d1128) {
      u_no_case(s1127);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t1133 = u_mk_int(0);
    v_i = t1133;
    u_value t1134 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1134;
    u_value t1141 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t1141)) {
      {
        u_value t1135 = v_i;
        v_start = t1135;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t1136 = u_add(v_i, u_mk_int(1));
            v_i = t1136;
          }
        } else {
          u_value t1137 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t1137)) {
            t1137 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t1137)) {
            {
              u_value t1139 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam1138, 0));
              v_i = t1139;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t1140 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t1140;
            }
          }
        }
      }
      t1141 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t1142 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t1142;
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
  u_value s1143 = v_s;
  u_value t1144 = NULL;
  if ((!t1144) && u_is_con(s1143, &n_set_46_SLeaf)) {
    t1144 = u_mk_bool(0);
  }
  if ((!t1144) && u_is_con(s1143, &n_set_46_SNode)) {
    u_value v_l = u_field(s1143, 0);
    u_value v_y = u_field(s1143, 1);
    u_value v_r = u_field(s1143, 2);
    u_value s1145 = u_compare(v_x, v_y);
    u_value t1146 = NULL;
    if ((!t1146) && u_equals(s1145, u_mk_int(-1))) {
      t1146 = f_set_46_member_2(v_x, v_l);
    }
    if ((!t1146) && u_equals(s1145, u_mk_int(1))) {
      t1146 = f_set_46_member_2(v_x, v_r);
    }
    if (!t1146) {
      t1146 = u_mk_bool(1);
    }
    if (!t1146) {
      u_no_case(s1145);
    }
    t1144 = t1146;
  }
  if (!t1144) {
    u_no_case(s1143);
  }
  return t1144;
}

u_value f_set_46_insert_2(u_value v_x, u_value v_s) {
  u_value s1147 = v_s;
  u_value t1148 = NULL;
  if ((!t1148) && u_is_con(s1147, &n_set_46_SLeaf)) {
    t1148 = u_mk_con(&n_set_46_SNode, 4, u_mk_con(&n_set_46_SLeaf, 0), v_x, u_mk_con(&n_set_46_SLeaf, 0), u_mk_int(1));
  }
  if ((!t1148) && u_is_con(s1147, &n_set_46_SNode)) {
    u_value v_l = u_field(s1147, 0);
    u_value v_y = u_field(s1147, 1);
    u_value v_r = u_field(s1147, 2);
    u_value s1149 = u_compare(v_x, v_y);
    u_value t1150 = NULL;
    if ((!t1150) && u_equals(s1149, u_mk_int(-1))) {
      t1150 = f_set_46_rebalance_3(f_set_46_insert_2(v_x, v_l), v_y, v_r);
    }
    if ((!t1150) && u_equals(s1149, u_mk_int(1))) {
      t1150 = f_set_46_rebalance_3(v_l, v_y, f_set_46_insert_2(v_x, v_r));
    }
    if (!t1150) {
      t1150 = v_s;
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

u_value f_set_46_from_list_1(u_value v_xs) {
  return f_set_46_insert_all_2(v_xs, u_mk_con(&n_set_46_SLeaf, 0));
}

u_value f_set_46_insert_all_2(u_value v_xs, u_value v_s) {
  u_value s1151 = v_xs;
  u_value t1152 = NULL;
  if ((!t1152) && u_is_con(s1151, &n_list_46_Nil)) {
    t1152 = v_s;
  }
  if ((!t1152) && u_is_con(s1151, &n_list_46_Cons)) {
    u_value v_x = u_field(s1151, 0);
    u_value v_rest = u_field(s1151, 1);
    t1152 = f_set_46_insert_all_2(v_rest, f_set_46_insert_2(v_x, v_s));
  }
  if (!t1152) {
    u_no_case(s1151);
  }
  return t1152;
}

u_value f_set_46_elements_1(u_value v_s) {
  return f_set_46_elements_onto_2(v_s, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_set_46_elements_onto_2(u_value v_s, u_value v_rest) {
  u_value s1153 = v_s;
  u_value t1154 = NULL;
  if ((!t1154) && u_is_con(s1153, &n_set_46_SLeaf)) {
    t1154 = v_rest;
  }
  if ((!t1154) && u_is_con(s1153, &n_set_46_SNode)) {
    u_value v_l = u_field(s1153, 0);
    u_value v_x = u_field(s1153, 1);
    u_value v_r = u_field(s1153, 2);
    t1154 = f_set_46_elements_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, v_x, f_set_46_elements_onto_2(v_r, v_rest)));
  }
  if (!t1154) {
    u_no_case(s1153);
  }
  return t1154;
}

u_value f_set_46_count_1(u_value v_s) {
  u_value s1155 = v_s;
  u_value t1156 = NULL;
  if ((!t1156) && u_is_con(s1155, &n_set_46_SLeaf)) {
    t1156 = u_mk_int(0);
  }
  if ((!t1156) && u_is_con(s1155, &n_set_46_SNode)) {
    u_value v_l = u_field(s1155, 0);
    u_value v_r = u_field(s1155, 2);
    t1156 = u_add(u_add(f_set_46_count_1(v_l), u_mk_int(1)), f_set_46_count_1(v_r));
  }
  if (!t1156) {
    u_no_case(s1155);
  }
  return t1156;
}

u_value f_set_46_depth_1(u_value v_s) {
  u_value s1157 = v_s;
  u_value t1158 = NULL;
  if ((!t1158) && u_is_con(s1157, &n_set_46_SLeaf)) {
    t1158 = u_mk_int(0);
  }
  if ((!t1158) && u_is_con(s1157, &n_set_46_SNode)) {
    u_value v_d = u_field(s1157, 3);
    t1158 = v_d;
  }
  if (!t1158) {
    u_no_case(s1157);
  }
  return t1158;
}

u_value f_set_46_snode_depth_2(u_value v_l, u_value v_r) {
  u_value t1159;
  if (u_truth(u_lt(f_set_46_depth_1(v_l), f_set_46_depth_1(v_r)))) {
    t1159 = f_set_46_depth_1(v_r);
  } else {
    t1159 = f_set_46_depth_1(v_l);
  }
  return u_add(u_mk_int(1), t1159);
}

u_value f_set_46_snode_3(u_value v_l, u_value v_x, u_value v_r) {
  return u_mk_con(&n_set_46_SNode, 4, v_l, v_x, v_r, f_set_46_snode_depth_2(v_l, v_r));
}

u_value f_set_46_turn_left_1(u_value v_s) {
  u_value s1160 = v_s;
  u_value t1161 = NULL;
  if ((!t1161) && (u_is_con(s1160, &n_set_46_SNode) && u_is_con(u_field(s1160, 2), &n_set_46_SNode))) {
    u_value v_a = u_field(s1160, 0);
    u_value v_x = u_field(s1160, 1);
    u_value v_b = u_field(u_field(s1160, 2), 0);
    u_value v_y = u_field(u_field(s1160, 2), 1);
    u_value v_c = u_field(u_field(s1160, 2), 2);
    t1161 = f_set_46_snode_3(f_set_46_snode_3(v_a, v_x, v_b), v_y, v_c);
  }
  if (!t1161) {
    t1161 = v_s;
  }
  if (!t1161) {
    u_no_case(s1160);
  }
  return t1161;
}

u_value f_set_46_turn_right_1(u_value v_s) {
  u_value s1162 = v_s;
  u_value t1163 = NULL;
  if ((!t1163) && (u_is_con(s1162, &n_set_46_SNode) && u_is_con(u_field(s1162, 0), &n_set_46_SNode))) {
    u_value v_a = u_field(u_field(s1162, 0), 0);
    u_value v_x = u_field(u_field(s1162, 0), 1);
    u_value v_b = u_field(u_field(s1162, 0), 2);
    u_value v_y = u_field(s1162, 1);
    u_value v_c = u_field(s1162, 2);
    t1163 = f_set_46_snode_3(v_a, v_x, f_set_46_snode_3(v_b, v_y, v_c));
  }
  if (!t1163) {
    t1163 = v_s;
  }
  if (!t1163) {
    u_no_case(s1162);
  }
  return t1163;
}

u_value f_set_46_smaller_1(u_value v_s) {
  u_value s1164 = v_s;
  u_value t1165 = NULL;
  if ((!t1165) && u_is_con(s1164, &n_set_46_SNode)) {
    u_value v_l = u_field(s1164, 0);
    t1165 = v_l;
  }
  if (!t1165) {
    t1165 = v_s;
  }
  if (!t1165) {
    u_no_case(s1164);
  }
  return t1165;
}

u_value f_set_46_greater_1(u_value v_s) {
  u_value s1166 = v_s;
  u_value t1167 = NULL;
  if ((!t1167) && u_is_con(s1166, &n_set_46_SNode)) {
    u_value v_r = u_field(s1166, 2);
    t1167 = v_r;
  }
  if (!t1167) {
    t1167 = v_s;
  }
  if (!t1167) {
    u_no_case(s1166);
  }
  return t1167;
}

u_value f_set_46_rebalance_3(u_value v_l, u_value v_x, u_value v_r) {
  u_value t1171;
  if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1))))) {
    u_value t1168;
    if (u_truth(u_lt(f_set_46_depth_1(f_set_46_smaller_1(v_l)), f_set_46_depth_1(f_set_46_greater_1(v_l))))) {
      t1168 = f_set_46_turn_left_1(v_l);
    } else {
      t1168 = v_l;
    }
    t1171 = f_set_46_turn_right_1(f_set_46_snode_3(t1168, v_x, v_r));
  } else {
    u_value t1170;
    if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1))))) {
      u_value t1169;
      if (u_truth(u_lt(f_set_46_depth_1(f_set_46_greater_1(v_r)), f_set_46_depth_1(f_set_46_smaller_1(v_r))))) {
        t1169 = f_set_46_turn_right_1(v_r);
      } else {
        t1169 = v_r;
      }
      t1170 = f_set_46_turn_left_1(f_set_46_snode_3(v_l, v_x, t1169));
    } else {
      t1170 = f_set_46_snode_3(v_l, v_x, v_r);
    }
    t1171 = t1170;
  }
  return t1171;
}

u_value f_set_46_well_formed_1(u_value v_s) {
  u_value s1172 = v_s;
  u_value t1173 = NULL;
  if ((!t1173) && u_is_con(s1172, &n_set_46_SLeaf)) {
    t1173 = u_mk_bool(1);
  }
  if ((!t1173) && u_is_con(s1172, &n_set_46_SNode)) {
    u_value v_l = u_field(s1172, 0);
    u_value v_r = u_field(s1172, 2);
    u_value v_d = u_field(s1172, 3);
    u_value t1174 = f_set_46_well_formed_1(v_l);
    if (u_truth(t1174)) {
      t1174 = f_set_46_well_formed_1(v_r);
    }
    u_value t1175 = t1174;
    if (u_truth(t1175)) {
      t1175 = u_eq(v_d, f_set_46_snode_depth_2(v_l, v_r));
    }
    u_value t1176 = t1175;
    if (u_truth(t1176)) {
      t1176 = f_prelude_46__60__61__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1)));
    }
    u_value t1177 = t1176;
    if (u_truth(t1177)) {
      t1177 = f_prelude_46__60__61__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1)));
    }
    t1173 = t1177;
  }
  if (!t1173) {
    u_no_case(s1172);
  }
  return t1173;
}

u_value f_set_46_upto_1(u_value v_n) {
  u_value t1178;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t1178 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1178 = f_list_46__43__43__2(f_set_46_upto_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t1178;
}

u_value f_parse_46_Notations_46_cmds_1(u_value v_x) {
  u_value s1179 = v_x;
  u_value t1180 = NULL;
  if ((!t1180) && u_is_con(s1179, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1179, 0);
    t1180 = v_y;
  }
  if (!t1180) {
    u_no_case(s1179);
  }
  return t1180;
}

u_value f_parse_46_Notations_46_ops_1(u_value v_x) {
  u_value s1181 = v_x;
  u_value t1182 = NULL;
  if ((!t1182) && u_is_con(s1181, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1181, 1);
    t1182 = v_y;
  }
  if (!t1182) {
    u_no_case(s1181);
  }
  return t1182;
}

u_value f_parse_46_Notations_46_cmds_58__61__2(u_value v_x, u_value v_y) {
  u_value s1183 = v_x;
  u_value t1184 = NULL;
  if ((!t1184) && u_is_con(s1183, &n_parse_46_Notations)) {
    u_value v_a1 = u_field(s1183, 1);
    t1184 = u_mk_con(&n_parse_46_Notations, 2, v_y, v_a1);
  }
  if (!t1184) {
    u_no_case(s1183);
  }
  return t1184;
}

u_value f_parse_46_Notations_46_ops_58__61__2(u_value v_x, u_value v_y) {
  u_value s1185 = v_x;
  u_value t1186 = NULL;
  if ((!t1186) && u_is_con(s1185, &n_parse_46_Notations)) {
    u_value v_a0 = u_field(s1185, 0);
    t1186 = u_mk_con(&n_parse_46_Notations, 2, v_a0, v_y);
  }
  if (!t1186) {
    u_no_case(s1185);
  }
  return t1186;
}

u_value f_parse_46_Scope_46_params_1(u_value v_x) {
  u_value s1187 = v_x;
  u_value t1188 = NULL;
  if ((!t1188) && u_is_con(s1187, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1187, 0);
    t1188 = v_y;
  }
  if (!t1188) {
    u_no_case(s1187);
  }
  return t1188;
}

u_value f_parse_46_Scope_46_notations_1(u_value v_x) {
  u_value s1189 = v_x;
  u_value t1190 = NULL;
  if ((!t1190) && u_is_con(s1189, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1189, 1);
    t1190 = v_y;
  }
  if (!t1190) {
    u_no_case(s1189);
  }
  return t1190;
}

u_value f_parse_46_Scope_46_params_58__61__2(u_value v_x, u_value v_y) {
  u_value s1191 = v_x;
  u_value t1192 = NULL;
  if ((!t1192) && u_is_con(s1191, &n_parse_46_Scope)) {
    u_value v_a1 = u_field(s1191, 1);
    t1192 = u_mk_con(&n_parse_46_Scope, 2, v_y, v_a1);
  }
  if (!t1192) {
    u_no_case(s1191);
  }
  return t1192;
}

u_value f_parse_46_Scope_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1193 = v_x;
  u_value t1194 = NULL;
  if ((!t1194) && u_is_con(s1193, &n_parse_46_Scope)) {
    u_value v_a0 = u_field(s1193, 0);
    t1194 = u_mk_con(&n_parse_46_Scope, 2, v_a0, v_y);
  }
  if (!t1194) {
    u_no_case(s1193);
  }
  return t1194;
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
  static struct u_object l___1195 = U_LIT("_");
  static struct u_object l_assert_1196 = U_LIT("assert");
  static struct u_object l_assign_1197 = U_LIT("assign");
  static struct u_object l_call_1198 = U_LIT("call");
  static struct u_object l_case_1199 = U_LIT("case");
  static struct u_object l_catch_1200 = U_LIT("catch");
  static struct u_object l_data_1201 = U_LIT("data");
  static struct u_object l_else_1202 = U_LIT("else");
  static struct u_object l_end_1203 = U_LIT("end");
  static struct u_object l_false_1204 = U_LIT("false");
  static struct u_object l_for_1205 = U_LIT("for");
  static struct u_object l_function_1206 = U_LIT("function");
  static struct u_object l_if_1207 = U_LIT("if");
  static struct u_object l_import_1208 = U_LIT("import");
  static struct u_object l_in_1209 = U_LIT("in");
  static struct u_object l_infix_1210 = U_LIT("infix");
  static struct u_object l_lambda_1211 = U_LIT("lambda");
  static struct u_object l_let_1212 = U_LIT("let");
  static struct u_object l_local_1213 = U_LIT("local");
  static struct u_object l_match_1214 = U_LIT("match");
  static struct u_object l_out_1215 = U_LIT("out");
  static struct u_object l_postfix_1216 = U_LIT("postfix");
  static struct u_object l_prefix_1217 = U_LIT("prefix");
  static struct u_object l_procedure_1218 = U_LIT("procedure");
  static struct u_object l_raise_1219 = U_LIT("raise");
  static struct u_object l_ref_1220 = U_LIT("ref");
  static struct u_object l_run_1221 = U_LIT("run");
  static struct u_object l_skip_1222 = U_LIT("skip");
  static struct u_object l_test_1223 = U_LIT("test");
  static struct u_object l_then_1224 = U_LIT("then");
  static struct u_object l_true_1225 = U_LIT("true");
  static struct u_object l_type_1226 = U_LIT("type");
  static struct u_object l_try_1227 = U_LIT("try");
  static struct u_object l_while_1228 = U_LIT("while");
  static struct u_object l_with_1229 = U_LIT("with");
  u_pop(handler);
  cache = f_set_46_from_list_1(u_mk_con(&n_list_46_Cons, 2, &l___1195, u_mk_con(&n_list_46_Cons, 2, &l_assert_1196, u_mk_con(&n_list_46_Cons, 2, &l_assign_1197, u_mk_con(&n_list_46_Cons, 2, &l_call_1198, u_mk_con(&n_list_46_Cons, 2, &l_case_1199, u_mk_con(&n_list_46_Cons, 2, &l_catch_1200, u_mk_con(&n_list_46_Cons, 2, &l_data_1201, u_mk_con(&n_list_46_Cons, 2, &l_else_1202, u_mk_con(&n_list_46_Cons, 2, &l_end_1203, u_mk_con(&n_list_46_Cons, 2, &l_false_1204, u_mk_con(&n_list_46_Cons, 2, &l_for_1205, u_mk_con(&n_list_46_Cons, 2, &l_function_1206, u_mk_con(&n_list_46_Cons, 2, &l_if_1207, u_mk_con(&n_list_46_Cons, 2, &l_import_1208, u_mk_con(&n_list_46_Cons, 2, &l_in_1209, u_mk_con(&n_list_46_Cons, 2, &l_infix_1210, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1211, u_mk_con(&n_list_46_Cons, 2, &l_let_1212, u_mk_con(&n_list_46_Cons, 2, &l_local_1213, u_mk_con(&n_list_46_Cons, 2, &l_match_1214, u_mk_con(&n_list_46_Cons, 2, &l_out_1215, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1216, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1217, u_mk_con(&n_list_46_Cons, 2, &l_procedure_1218, u_mk_con(&n_list_46_Cons, 2, &l_raise_1219, u_mk_con(&n_list_46_Cons, 2, &l_ref_1220, u_mk_con(&n_list_46_Cons, 2, &l_run_1221, u_mk_con(&n_list_46_Cons, 2, &l_skip_1222, u_mk_con(&n_list_46_Cons, 2, &l_test_1223, u_mk_con(&n_list_46_Cons, 2, &l_then_1224, u_mk_con(&n_list_46_Cons, 2, &l_true_1225, u_mk_con(&n_list_46_Cons, 2, &l_type_1226, u_mk_con(&n_list_46_Cons, 2, &l_try_1227, u_mk_con(&n_list_46_Cons, 2, &l_while_1228, u_mk_con(&n_list_46_Cons, 2, &l_with_1229, u_mk_con(&n_list_46_Nil, 0)))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_set_46_member_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1230 = U_LIT("=");
  static struct u_object l__61__62__1231 = U_LIT("=>");
  static struct u_object l__58__61__1232 = U_LIT(":=");
  static struct u_object l__58__1233 = U_LIT(":");
  static struct u_object l__124__1234 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1230, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1231, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1232, u_mk_con(&n_list_46_Cons, 2, &l__58__1233, u_mk_con(&n_list_46_Cons, 2, &l__124__1234, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1235 = v_ns;
  u_value t1236 = NULL;
  if ((!t1236) && u_is_con(s1235, &n_list_46_Nil)) {
    t1236 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1236) && (u_is_con(s1235, &n_list_46_Cons) && (u_is_con(u_field(s1235, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1235, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1235, 0), 1);
    u_value v_ops = u_field(u_field(s1235, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1236 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1236) && u_is_con(s1235, &n_list_46_Cons)) {
    u_value v_more = u_field(s1235, 1);
    t1236 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1236) {
    u_no_case(s1235);
  }
  return t1236;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1237 = v_ns;
  u_value t1238 = NULL;
  if ((!t1238) && u_is_con(s1237, &n_list_46_Nil)) {
    t1238 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1238) && (u_is_con(s1237, &n_list_46_Cons) && (u_is_con(u_field(s1237, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1237, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1237, 0), 1);
    u_value v_ops = u_field(u_field(s1237, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1238 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1238) && u_is_con(s1237, &n_list_46_Cons)) {
    u_value v_more = u_field(s1237, 1);
    t1238 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1238) {
    u_no_case(s1237);
  }
  return t1238;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1239 = v_ns;
  u_value t1240 = NULL;
  if ((!t1240) && u_is_con(s1239, &n_list_46_Nil)) {
    t1240 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1240) && (u_is_con(s1239, &n_list_46_Cons) && u_is_con(u_field(s1239, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1239, 0), 0);
    u_value v_p = u_field(u_field(s1239, 0), 1);
    u_value v_ops = u_field(u_field(s1239, 0), 2);
    u_value t1241 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1241)) {
      t1241 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1242 = t1241;
    if (u_truth(t1242)) {
      t1242 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1242)) {
      t1240 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1240) && u_is_con(s1239, &n_list_46_Cons)) {
    u_value v_more = u_field(s1239, 1);
    t1240 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1240) {
    u_no_case(s1239);
  }
  return t1240;
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
  u_value s1243 = v_cs;
  u_value t1244 = NULL;
  if ((!t1244) && u_is_con(s1243, &n_list_46_Nil)) {
    t1244 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1244) && (u_is_con(s1243, &n_list_46_Cons) && u_is_con(u_field(s1243, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1243, 0), 2);
    u_value v_more = u_field(s1243, 1);
    t1244 = f_list_46__43__43__2(v_ops, f_parse_46_operators_of_1(v_more));
  }
  if ((!t1244) && u_is_con(s1243, &n_list_46_Cons)) {
    u_value v_more = u_field(s1243, 1);
    t1244 = f_parse_46_operators_of_1(v_more);
  }
  if (!t1244) {
    u_no_case(s1243);
  }
  return t1244;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1245 = v_ts;
  u_value t1246 = NULL;
  if ((!t1246) && u_is_con(s1245, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1245, 0), 0);
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

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1247 = v_ts;
  u_value t1248 = NULL;
  if ((!t1248) && (u_is_con(s1247, &n_list_46_Cons) && u_is_con(u_field(s1247, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1247, 1), 0), 0);
    t1248 = v_t;
  }
  if (!t1248) {
    t1248 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1248) {
    u_no_case(s1247);
  }
  return t1248;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1249 = v_ts;
  u_value t1250 = NULL;
  if ((!t1250) && u_is_con(s1249, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1249, 0), 1);
    t1250 = v_i;
  }
  if (!t1250) {
    t1250 = u_mk_int(0);
  }
  if (!t1250) {
    u_no_case(s1249);
  }
  return t1250;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1251 = v_t;
  u_value t1252 = NULL;
  if ((!t1252) && u_is_con(s1251, &n_lex_46_TName)) {
    u_value v_n = u_field(s1251, 0);
    t1252 = v_n;
  }
  if ((!t1252) && u_is_con(s1251, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1251, 0);
    t1252 = v_s;
  }
  if ((!t1252) && u_is_con(s1251, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1251, 0);
    t1252 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1252) && u_is_con(s1251, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1251, 0);
    t1252 = u_str_show(v_n);
  }
  if ((!t1252) && u_is_con(s1251, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1251, 0);
    t1252 = u_str_quote(v_s);
  }
  if ((!t1252) && u_is_con(s1251, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1251, 0);
    static struct u_object l__39__1253 = U_LIT("'");
    static struct u_object l__39__1254 = U_LIT("'");
    t1252 = u_concat(&l__39__1253, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1254));
  }
  if ((!t1252) && u_is_con(s1251, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1255 = U_LIT("end of input");
    t1252 = &l_end_32_of_32_input_1255;
  }
  if (!t1252) {
    u_no_case(s1251);
  }
  return t1252;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1256 = f_parse_46_head_token_1(v_ts);
  u_value t1257 = NULL;
  if ((!t1257) && u_is_con(s1256, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1256, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1257 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1257) && u_is_con(s1256, &n_lex_46_TName)) {
    u_value v_n = u_field(s1256, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1257 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1257) {
    t1257 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1257) {
    u_no_case(s1256);
  }
  return t1257;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1258 = *r_ts;
    int d1259 = 0;
    if ((!d1259) && u_is_con(s1258, &n_list_46_Cons)) {
      u_value v_more = u_field(s1258, 1);
      d1259 = 1;
      {
        u_value t1260 = v_more;
        *r_ts = t1260;
      }
    }
    if (!d1259) {
      d1259 = 1;
      {
      }
    }
    if (!d1259) {
      u_no_case(s1258);
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
        static struct u_object l__32_expected_32_but_32__1261 = U_LIT(" expected but ");
        static struct u_object l__32_found_1262 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1261, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1262))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1263 = f_parse_46_head_token_1(*r_ts);
    int d1264 = 0;
    if ((!d1264) && u_is_con(s1263, &n_lex_46_TName)) {
      u_value v_m = u_field(s1263, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1264 = 1;
        {
          u_value t1265 = v_m;
          *r_n = t1265;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1264) {
      d1264 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1266 = U_LIT("name expected but ");
        static struct u_object l__32_found_1267 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1266, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1267)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1264) {
      u_no_case(s1263);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1268 = f_parse_46_head_token_1(*r_ts);
    int d1269 = 0;
    if ((!d1269) && u_is_con(s1268, &n_lex_46_TName)) {
      u_value v_m = u_field(s1268, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_m))) {
        d1269 = 1;
        {
          static struct u_object l_operator_32__1270 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1271 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1270, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1271)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1269) {
      d1269 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1269) {
      u_no_case(s1268);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1272 = f_parse_46_head_token_1(*r_ts);
    int d1273 = 0;
    if ((!d1273) && u_is_con(s1272, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1272, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1273 = 1;
        {
          u_value t1274 = v_s;
          *r_n = t1274;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1273) {
      d1273 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1273) {
      u_no_case(s1272);
    }
  }
}

u_value f_parse_46_Ops_46_notations_1(u_value v_x) {
  u_value s1275 = v_x;
  u_value t1276 = NULL;
  if ((!t1276) && u_is_con(s1275, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1275, 0);
    t1276 = v_y;
  }
  if (!t1276) {
    u_no_case(s1275);
  }
  return t1276;
}

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x) {
  u_value s1277 = v_x;
  u_value t1278 = NULL;
  if ((!t1278) && u_is_con(s1277, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1277, 1);
    t1278 = v_y;
  }
  if (!t1278) {
    u_no_case(s1277);
  }
  return t1278;
}

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x) {
  u_value s1279 = v_x;
  u_value t1280 = NULL;
  if ((!t1280) && u_is_con(s1279, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1279, 2);
    t1280 = v_y;
  }
  if (!t1280) {
    u_no_case(s1279);
  }
  return t1280;
}

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x) {
  u_value s1281 = v_x;
  u_value t1282 = NULL;
  if ((!t1282) && u_is_con(s1281, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1281, 3);
    t1282 = v_y;
  }
  if (!t1282) {
    u_no_case(s1281);
  }
  return t1282;
}

u_value f_parse_46_Ops_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1283 = v_x;
  u_value t1284 = NULL;
  if ((!t1284) && u_is_con(s1283, &n_parse_46_Ops)) {
    u_value v_a1 = u_field(s1283, 1);
    u_value v_a2 = u_field(s1283, 2);
    u_value v_a3 = u_field(s1283, 3);
    t1284 = u_mk_con(&n_parse_46_Ops, 4, v_y, v_a1, v_a2, v_a3);
  }
  if (!t1284) {
    u_no_case(s1283);
  }
  return t1284;
}

u_value f_parse_46_Ops_46_prefix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1285 = v_x;
  u_value t1286 = NULL;
  if ((!t1286) && u_is_con(s1285, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1285, 0);
    u_value v_a2 = u_field(s1285, 2);
    u_value v_a3 = u_field(s1285, 3);
    t1286 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_y, v_a2, v_a3);
  }
  if (!t1286) {
    u_no_case(s1285);
  }
  return t1286;
}

u_value f_parse_46_Ops_46_postfix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1287 = v_x;
  u_value t1288 = NULL;
  if ((!t1288) && u_is_con(s1287, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1287, 0);
    u_value v_a1 = u_field(s1287, 1);
    u_value v_a3 = u_field(s1287, 3);
    t1288 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_y, v_a3);
  }
  if (!t1288) {
    u_no_case(s1287);
  }
  return t1288;
}

u_value f_parse_46_Ops_46_infix_op_58__61__2(u_value v_x, u_value v_y) {
  u_value s1289 = v_x;
  u_value t1290 = NULL;
  if ((!t1290) && u_is_con(s1289, &n_parse_46_Ops)) {
    u_value v_a0 = u_field(s1289, 0);
    u_value v_a1 = u_field(s1289, 1);
    u_value v_a2 = u_field(s1289, 2);
    t1290 = u_mk_con(&n_parse_46_Ops, 4, v_a0, v_a1, v_a2, v_y);
  }
  if (!t1290) {
    u_no_case(s1289);
  }
  return t1290;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1291 = v_rest;
    int d1292 = 0;
    if ((!d1292) && u_is_con(s1291, &n_list_46_Nil)) {
      d1292 = 1;
      {
      }
    }
    if ((!d1292) && (u_is_con(s1291, &n_list_46_Cons) && u_is_con(u_field(s1291, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1291, 0), 0);
      u_value v_i = u_field(u_field(s1291, 0), 1);
      d1292 = 1;
      {
        static struct u_object l_unexpected_32_opera_1293 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1293, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1292) {
      d1292 = 1;
      {
        static struct u_object l_missing_32_operator_1294 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1294, v_at), "parse.ParseError");
      }
    }
    if (!d1292) {
      u_no_case(s1291);
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
    u_value s1295 = v_items;
    int d1296 = 0;
    if ((!d1296) && (u_is_con(s1295, &n_list_46_Cons) && u_is_con(u_field(s1295, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1295, 0), 0);
      u_value v_more = u_field(s1295, 1);
      d1296 = 1;
      {
        u_value t1297 = v_x;
        u_value t1298 = v_more;
        *r_a = t1297;
        *r_rest = t1298;
      }
    }
    if ((!d1296) && (u_is_con(s1295, &n_list_46_Cons) && u_is_con(u_field(s1295, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1295, 0), 0);
      u_value v_i = u_field(u_field(s1295, 0), 1);
      u_value v_more = u_field(s1295, 1);
      d1296 = 1;
      {
        u_value s1299 = f_parse_46_prefix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op);
        int d1300 = 0;
        if ((!d1300) && u_is_con(s1299, &n_option_46_Some)) {
          u_value v_p = u_field(s1299, 0);
          d1300 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1301 = u_apply(f_parse_46_Ops_46_prefix_op_1(v_o), 2, v_op, v_b);
            *r_a = t1301;
          }
        }
        if ((!d1300) && u_is_con(s1299, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op))) {
            d1300 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1302 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1302), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1300) && u_is_con(s1299, &n_option_46_None)) {
          d1300 = 1;
          {
            static struct u_object l_undeclared_32_opera_1303 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1303, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1300) {
          u_no_case(s1299);
        }
      }
    }
    if ((!d1296) && u_is_con(s1295, &n_list_46_Nil)) {
      d1296 = 1;
      {
        static struct u_object l_missing_32_operand_1304 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1304, v_at), "parse.ParseError");
      }
    }
    if (!d1296) {
      u_no_case(s1295);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1305 = v_items;
    int d1306 = 0;
    if ((!d1306) && (u_is_con(s1305, &n_list_46_Cons) && u_is_con(u_field(s1305, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1305, 0), 0);
      u_value v_i = u_field(u_field(s1305, 0), 1);
      u_value v_tail = u_field(s1305, 1);
      d1306 = 1;
      {
        u_value s1307 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op), f_parse_46_infix_of_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op));
        int d1308 = 0;
        if ((!d1308) && u_is_con(u_field(s1307, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1307, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1308 = 1;
            {
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_postfix_op_1(v_o), 2, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1308) && u_is_con(u_field(s1307, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1307, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1307, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1308 = 1;
            {
              u_value t1309;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1309 = v_p;
              } else {
                t1309 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1309, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)));
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_infix_op_1(v_o), 3, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1308) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op)))) {
            d1308 = 1;
            {
              static struct u_object l_undeclared_32_opera_1310 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1310, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1308) {
          d1308 = 1;
          {
            u_value t1311 = v_lhs;
            u_value t1312 = v_items;
            *r_a = t1311;
            *r_rest = t1312;
          }
        }
        if (!d1308) {
          u_no_case(s1307);
        }
      }
    }
    if (!d1306) {
      d1306 = 1;
      {
        u_value t1313 = v_lhs;
        u_value t1314 = v_items;
        *r_a = t1313;
        *r_rest = t1314;
      }
    }
    if (!d1306) {
      u_no_case(s1305);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1315 = v_rest;
    int d1316 = 0;
    if ((!d1316) && (u_is_con(s1315, &n_list_46_Cons) && u_is_con(u_field(s1315, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1315, 0), 0);
      u_value v_i = u_field(u_field(s1315, 0), 1);
      u_value t1319 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1319)) {
        t1319 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1319)) {
        d1316 = 1;
        {
          static struct u_object l_operator_32__1317 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1318 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1317, u_concat(v_op, &l__32_is_32_not_32_associat_1318)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1316) {
      d1316 = 1;
      {
      }
    }
    if (!d1316) {
      u_no_case(s1315);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1320 = v_fp;
  u_value t1321 = NULL;
  if ((!t1321) && u_is_con(s1320, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1320, 0), 1);
    t1321 = u_eq(v_q, v_p);
  }
  if (!t1321) {
    t1321 = u_mk_bool(0);
  }
  if (!t1321) {
    u_no_case(s1320);
  }
  return t1321;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1322 = v_ts;
  u_value t1323 = NULL;
  if ((!t1323) && (u_is_con(s1322, &n_list_46_Cons) && (u_is_con(u_field(s1322, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1322, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1322, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1322, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1322, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1322, 0), 1);
    t1323 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1323) && (u_is_con(s1322, &n_list_46_Cons) && u_is_con(u_field(s1322, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1322, 0);
    t1323 = v_t;
  }
  if ((!t1323) && u_is_con(s1322, &n_list_46_Nil)) {
    t1323 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1323) {
    static struct u_object l__40_invalid_41__1324 = U_LIT("(invalid)");
    t1323 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1324), v_ts);
  }
  if (!t1323) {
    u_no_case(s1322);
  }
  return t1323;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1325 = v_ts;
  u_value t1326 = NULL;
  if ((!t1326) && (u_is_con(s1325, &n_list_46_Cons) && (u_is_con(u_field(s1325, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1325, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1325, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1325, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1325, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1325, 0), 1);
    t1326 = v_cs;
  }
  if (!t1326) {
    t1326 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1326) {
    u_no_case(s1325);
  }
  return t1326;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1327 = U_LIT("->");
  u_value t1334;
  if (u_truth(u_eq(v_op, &l__45__62__1327))) {
    u_value t1328 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1329 = &t1328;
    u_value l1330 = v_l;
    while (u_is_con(l1330, &n_list_46_Cons)) {
      u_value v_t = u_field(l1330, 0);
      if (u_truth(u_mk_bool(1))) {
        e1329 = u_append(e1329, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1330 = u_field(l1330, 1);
    }
    t1334 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1328, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1331 = U_LIT("*");
    u_value t1333;
    if (u_truth(u_eq(v_op, &l__42__1331))) {
      static struct u_object l__40_product_41__1332 = U_LIT("(product)");
      t1333 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1332), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1333 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1334 = t1333;
  }
  return t1334;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1335 = v_t;
  u_value t1336 = NULL;
  if ((!t1336) && (u_is_con(s1335, &n_syntax_46_TSort) && (u_is_con(u_field(s1335, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1335, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1335, 0), 1), u_mk_lit("(invalid)")))))) {
    t1336 = u_mk_bool(1);
  }
  if ((!t1336) && u_is_con(s1335, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1335, 1);
    t1336 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1336) && u_is_con(s1335, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1335, 0);
    u_value v_r = u_field(s1335, 1);
    u_value t1337 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1337)) {
      t1337 = f_parse_46_has_invalid_1(v_r);
    }
    t1336 = t1337;
  }
  if ((!t1336) && u_is_con(s1335, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1335, 0);
    t1336 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1336) {
    t1336 = u_mk_bool(0);
  }
  if (!t1336) {
    u_no_case(s1335);
  }
  return t1336;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1338 = v_ts;
  u_value t1339 = NULL;
  if ((!t1339) && u_is_con(s1338, &n_list_46_Nil)) {
    t1339 = u_mk_bool(0);
  }
  if ((!t1339) && u_is_con(s1338, &n_list_46_Cons)) {
    u_value v_t = u_field(s1338, 0);
    u_value v_more = u_field(s1338, 1);
    u_value t1340 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1340)) {
      t1340 = f_parse_46_any_invalid_1(v_more);
    }
    t1339 = t1340;
  }
  if (!t1339) {
    u_no_case(s1338);
  }
  return t1339;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1341 = v_t;
  u_value t1342 = NULL;
  if ((!t1342) && u_is_con(s1341, &n_lex_46_TName)) {
    u_value v_n = u_field(s1341, 0);
    u_value t1343 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1343)) {
      t1343 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1342 = t1343;
  }
  if ((!t1342) && (u_is_con(s1341, &n_lex_46_TPunct) && u_equals(u_field(s1341, 0), u_mk_char(40)))) {
    t1342 = u_mk_bool(1);
  }
  if (!t1342) {
    t1342 = u_mk_bool(0);
  }
  if (!t1342) {
    u_no_case(s1341);
  }
  return t1342;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1344 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1344;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1345 = f_parse_46_type_one_1(v_r);
    *r_t = t1345;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1346 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1346, v_start), "parse.ParseError");
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
    u_value t1347 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1347;
    u_value t1348 = u_mk_bool(1);
    v_next = t1348;
    u_value t1349 = u_mk_bool(1);
    v_go = t1349;
    u_value t1358 = v_go;
    while (u_truth(t1358)) {
      {
        u_value s1350 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1351 = 0;
        if ((!d1351) && u_is_con(s1350, &n_option_46_Some)) {
          u_value v_op = u_field(s1350, 0);
          d1351 = 1;
          {
            u_value t1352 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1353 = u_mk_bool(1);
            v_rev = t1352;
            v_next = t1353;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1351) && u_is_con(s1350, &n_option_46_None)) {
          u_value t1356 = v_next;
          if (u_truth(t1356)) {
            t1356 = f_parse_46_starts_type_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1356)) {
            d1351 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1354 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1355 = u_mk_bool(0);
              v_rev = t1354;
              v_next = t1355;
            }
          }
        }
        if (!d1351) {
          d1351 = 1;
          {
            u_value t1357 = u_mk_bool(0);
            v_go = t1357;
          }
        }
        if (!d1351) {
          u_no_case(s1350);
        }
      }
      t1358 = v_go;
    }
    u_value s1359 = v_rev;
    int d1360 = 0;
    if ((!d1360) && u_is_con(s1359, &n_list_46_Nil)) {
      d1360 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1361 = U_LIT("type expected but ");
        static struct u_object l__32_found_1362 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1361, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1362)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1360) {
      d1360 = 1;
      {
        u_value t1363 = f_list_46_reverse_1(v_rev);
        *r_items = t1363;
      }
    }
    if (!d1360) {
      u_no_case(s1359);
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
    u_value t1367 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (u_truth(t1367)) {
      static struct u_object l_in_1364 = U_LIT("in");
      static struct u_object l_out_1365 = U_LIT("out");
      static struct u_object l_ref_1366 = U_LIT("ref");
      t1367 = f_list_46_contains_2(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_in_1364), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_out_1365), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_lex_46_TName, 1, &l_ref_1366), u_mk_con(&n_list_46_Nil, 0)))));
    }
    if (u_truth(t1367)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_sections(u_mk_proc(wp_parse_46_parse_type, 0), u_mk_proc(wp_parse_46_parse_type, 0), v_sc, &v_ins, &v_outs, &v_refs, r_ts);
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
        u_value t1368 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TProc, 3, v_ins, v_outs, v_refs), u_mk_con(&n_list_46_Nil, 0));
        *r_r = t1368;
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
              u_value t1369 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1369;
            }
          } else {
            if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Scope_46_params_1(v_sc)))) {
              {
                u_value t1370 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
                *r_r = t1370;
              }
            } else {
              {
                u_value t1371 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
                *r_r = t1371;
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
    u_value t1372 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1372;
    u_value t1373 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1373)) {
      t1373 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1373)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1374 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1374;
        u_value t1376 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1376)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1375 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1375;
          }
          t1376 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1377 = f_list_46_reverse_1(v_rev);
    *r_r = t1377;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_prefix_2(u_value v_op, u_value v_a) {
  u_value s1378 = v_a;
  u_value t1379 = NULL;
  if ((!t1379) && (u_is_con(s1378, &n_syntax_46_ELit) && u_is_con(u_field(s1378, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1378, 0), 0);
    static struct u_object l__45__1380 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1380))) {
      t1379 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1379) {
    t1379 = f_parse_46_expr_op1_2(v_op, v_a);
  }
  if (!t1379) {
    u_no_case(s1378);
  }
  return t1379;
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1381 = v_es;
  u_value t1382 = NULL;
  if ((!t1382) && u_is_con(s1381, &n_list_46_Nil)) {
    t1382 = v_rest;
  }
  if ((!t1382) && u_is_con(s1381, &n_list_46_Cons)) {
    u_value v_e = u_field(s1381, 0);
    u_value v_more = u_field(s1381, 1);
    static struct u_object l_Cons_1383 = U_LIT("Cons");
    t1382 = f_parse_46_expr_op2_3(&l_Cons_1383, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1382) {
    u_no_case(s1381);
  }
  return t1382;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1384 = v_t;
  u_value t1385 = NULL;
  if ((!t1385) && u_is_con(s1384, &n_lex_46_TName)) {
    u_value v_n = u_field(s1384, 0);
    static struct u_object l_let_1386 = U_LIT("let");
    static struct u_object l_if_1387 = U_LIT("if");
    static struct u_object l_lambda_1388 = U_LIT("lambda");
    static struct u_object l_match_1389 = U_LIT("match");
    static struct u_object l_true_1390 = U_LIT("true");
    static struct u_object l_false_1391 = U_LIT("false");
    u_value t1393 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1386, u_mk_con(&n_list_46_Cons, 2, &l_if_1387, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1388, u_mk_con(&n_list_46_Cons, 2, &l_match_1389, u_mk_con(&n_list_46_Cons, 2, &l_true_1390, u_mk_con(&n_list_46_Cons, 2, &l_false_1391, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1393)) {
      u_value t1392 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1392)) {
        t1392 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1393 = t1392;
    }
    t1385 = t1393;
  }
  if ((!t1385) && u_is_con(s1384, &n_lex_46_TNum)) {
    t1385 = u_mk_bool(1);
  }
  if ((!t1385) && u_is_con(s1384, &n_lex_46_TStr)) {
    t1385 = u_mk_bool(1);
  }
  if ((!t1385) && u_is_con(s1384, &n_lex_46_TChar)) {
    t1385 = u_mk_bool(1);
  }
  if ((!t1385) && u_is_con(s1384, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1384, 0);
    u_value t1394 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1394)) {
      t1394 = u_eq(v_c, u_mk_char(91));
    }
    u_value t1395 = t1394;
    if (!u_truth(t1395)) {
      t1395 = u_eq(v_c, u_mk_char(123));
    }
    t1385 = t1395;
  }
  if (!t1385) {
    t1385 = u_mk_bool(0);
  }
  if (!t1385) {
    u_no_case(s1384);
  }
  return t1385;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    static struct u_object l_raise_1396 = U_LIT("raise");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_raise_1396))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, r_e, r_ts);
        u_value t1397 = u_mk_con(&n_syntax_46_ERaise, 2, *r_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_e = t1397;
      }
    } else {
      {
        p_parse_46_operators(v_sc, r_e, r_ts);
        static struct u_object l_with_1398 = U_LIT("with");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_with_1398))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_update(v_sc, r_e, r_ts);
            u_value t1399 = f_parse_46_at_update_1(*r_ts);
            while (u_truth(t1399)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_update(v_sc, r_e, r_ts);
              }
              t1399 = f_parse_46_at_update_1(*r_ts);
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
    static struct u_object l__58__61__1400 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1400), r_ts);
    p_parse_46_operators(v_sc, &v_a, r_ts);
    static struct u_object l__46__1401 = U_LIT(".");
    static struct u_object l__58__61__1402 = U_LIT(":=");
    u_value t1403 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1401, u_concat(v_f, &l__58__61__1402))), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1403;
  }
}

u_value f_parse_46_at_update_1(u_value v_ts) {
  u_value s1404 = v_ts;
  u_value t1405 = NULL;
  if ((!t1405) && (u_is_con(s1404, &n_list_46_Cons) && (u_is_con(u_field(u_field(s1404, 0), 0), &n_lex_46_TPunct) && (u_equals(u_field(u_field(u_field(s1404, 0), 0), 0), u_mk_char(44)) && (u_is_con(u_field(s1404, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s1404, 1), 0), 0), &n_lex_46_TName) && (u_is_con(u_field(u_field(s1404, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(u_field(s1404, 1), 1), 0), 0), &n_lex_46_TSym) && u_equals(u_field(u_field(u_field(u_field(u_field(s1404, 1), 1), 0), 0), 0), u_mk_lit(":=")))))))))) {
    t1405 = u_mk_bool(1);
  }
  if (!t1405) {
    t1405 = u_mk_bool(0);
  }
  if (!t1405) {
    u_no_case(s1404);
  }
  return t1405;
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
    u_value t1406 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1406;
    u_value t1407 = u_mk_bool(1);
    v_next = t1407;
    u_value t1408 = u_mk_bool(1);
    v_go = t1408;
    u_value t1417 = v_go;
    while (u_truth(t1417)) {
      {
        u_value s1409 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1410 = 0;
        if ((!d1410) && u_is_con(s1409, &n_option_46_Some)) {
          u_value v_op = u_field(s1409, 0);
          d1410 = 1;
          {
            u_value t1411 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1412 = u_mk_bool(1);
            v_rev = t1411;
            v_next = t1412;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1410) && u_is_con(s1409, &n_option_46_None)) {
          u_value t1415 = v_next;
          if (u_truth(t1415)) {
            t1415 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1415)) {
            d1410 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1413 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1414 = u_mk_bool(0);
              v_rev = t1413;
              v_next = t1414;
            }
          }
        }
        if (!d1410) {
          d1410 = 1;
          {
            u_value t1416 = u_mk_bool(0);
            v_go = t1416;
          }
        }
        if (!d1410) {
          u_no_case(s1409);
        }
      }
      t1417 = v_go;
    }
    u_value s1418 = v_rev;
    int d1419 = 0;
    if ((!d1419) && u_is_con(s1418, &n_list_46_Nil)) {
      d1419 = 1;
      {
        static struct u_object l_expression_32_expec_1420 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1421 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1420, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1421)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1419) {
      d1419 = 1;
      {
        u_value t1422 = f_list_46_reverse_1(v_rev);
        *r_items = t1422;
      }
    }
    if (!d1419) {
      u_no_case(s1418);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1423 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1423;
    u_value t1425 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1425)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1424 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1424;
      }
      t1425 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1426 = f_list_46_reverse_1(v_rev);
    *r_es = t1426;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1427 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1427;
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
    u_value t1428 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1428)) {
      t1428 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1432 = t1428;
    while (u_truth(t1432)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1429 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1429;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1430 = U_LIT(".");
            u_value t1431 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1430, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1431;
          }
        }
      }
      u_value t1433 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1433)) {
        t1433 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1432 = t1433;
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
    u_value s1434 = f_parse_46_head_token_1(*r_ts);
    int d1435 = 0;
    if ((!d1435) && (u_is_con(s1434, &n_lex_46_TName) && u_equals(u_field(s1434, 0), u_mk_lit("let")))) {
      d1435 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1435) && (u_is_con(s1434, &n_lex_46_TName) && u_equals(u_field(s1434, 0), u_mk_lit("if")))) {
      d1435 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1436 = U_LIT("then");
        p_parse_46_keyword(&l_then_1436, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1437 = U_LIT("else");
        p_parse_46_keyword(&l_else_1437, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1438 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1438;
      }
    }
    if ((!d1435) && (u_is_con(s1434, &n_lex_46_TName) && u_equals(u_field(s1434, 0), u_mk_lit("lambda")))) {
      d1435 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1435) && (u_is_con(s1434, &n_lex_46_TName) && u_equals(u_field(s1434, 0), u_mk_lit("match")))) {
      d1435 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1439 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1439;
      }
    }
    if ((!d1435) && (u_is_con(s1434, &n_lex_46_TName) && u_equals(u_field(s1434, 0), u_mk_lit("true")))) {
      d1435 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1440 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1440;
      }
    }
    if ((!d1435) && (u_is_con(s1434, &n_lex_46_TName) && u_equals(u_field(s1434, 0), u_mk_lit("false")))) {
      d1435 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1441 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1441;
      }
    }
    if ((!d1435) && u_is_con(s1434, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1434, 0);
      d1435 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1442 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1442;
      }
    }
    if ((!d1435) && u_is_con(s1434, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1434, 0);
      d1435 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1443 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1443;
      }
    }
    if ((!d1435) && u_is_con(s1434, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1434, 0);
      d1435 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1444 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1444;
      }
    }
    if ((!d1435) && (u_is_con(s1434, &n_lex_46_TPunct) && u_equals(u_field(s1434, 0), u_mk_char(40)))) {
      d1435 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1445 = v_es;
        int d1446 = 0;
        if ((!d1446) && (u_is_con(s1445, &n_list_46_Cons) && u_is_con(u_field(s1445, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1445, 0);
          d1446 = 1;
          {
            u_value t1447 = v_x;
            *r_e = t1447;
          }
        }
        if (!d1446) {
          d1446 = 1;
          {
            u_value t1448 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1448;
          }
        }
        if (!d1446) {
          u_no_case(s1445);
        }
      }
    }
    if ((!d1435) && (u_is_con(s1434, &n_lex_46_TPunct) && u_equals(u_field(s1434, 0), u_mk_char(91)))) {
      d1435 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1435) && (u_is_con(s1434, &n_lex_46_TPunct) && u_equals(u_field(s1434, 0), u_mk_char(123)))) {
      d1435 = 1;
      {
        p_parse_46_parse_braces(v_sc, r_e, r_ts);
      }
    }
    if ((!d1435) && u_is_con(s1434, &n_lex_46_TName)) {
      u_value v_n = u_field(s1434, 0);
      u_value t1450 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1450)) {
        t1450 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t1450)) {
        d1435 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1449 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1449;
        }
      }
    }
    if (!d1435) {
      d1435 = 1;
      {
        static struct u_object l_expression_32_expec_1451 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1452 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1451, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1452)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1435) {
      u_no_case(s1434);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1453 = v_ps;
  u_value t1454 = NULL;
  if ((!t1454) && u_is_con(s1453, &n_list_46_Nil)) {
    t1454 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1454) && (u_is_con(s1453, &n_list_46_Cons) && u_is_con(u_field(s1453, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1453, 0), 0);
    u_value v_more = u_field(s1453, 1);
    u_value s1455 = f_parse_46_pattern_vars_1(v_more);
    u_value t1456 = NULL;
    if ((!t1456) && u_is_con(s1455, &n_option_46_Some)) {
      u_value v_xs = u_field(s1455, 0);
      t1456 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1456) && u_is_con(s1455, &n_option_46_None)) {
      t1456 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1456) {
      u_no_case(s1455);
    }
    t1454 = t1456;
  }
  if (!t1454) {
    t1454 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1454) {
    u_no_case(s1453);
  }
  return t1454;
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
    static struct u_object l__58__61__1457 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1457), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1458 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1459 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1458;
    v_es = t1459;
    u_value t1463 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1463)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1460 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1460), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1461 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1462 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1461;
        v_es = t1462;
      }
      t1463 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1464 = U_LIT("in");
    p_parse_46_keyword(&l_in_1464, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1465 = f_list_46_reverse_1(v_ps);
    u_value t1466 = f_list_46_reverse_1(v_es);
    v_ps = t1465;
    v_es = t1466;
    u_value s1467 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1468 = 0;
    if ((!d1468) && u_is_con(u_field(s1467, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1467, 0), 0);
      d1468 = 1;
      {
        u_value t1469 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1469;
      }
    }
    if ((!d1468) && (u_is_con(u_field(s1467, 0), &n_option_46_None) && (u_is_con(u_field(s1467, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1467, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1467, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1467, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1467, 1), 0);
      u_value v_c = u_field(u_field(s1467, 2), 0);
      d1468 = 1;
      {
        u_value t1470 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1470;
      }
    }
    if (!d1468) {
      d1468 = 1;
      {
        u_value t1471 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1471;
      }
    }
    if (!d1468) {
      u_no_case(s1467);
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
        static struct u_object l__61__62__1472 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1472), r_ts);
        p_parse_46_parse_body(v_sc, &v_body, r_ts);
        static struct u_object l_end_1473 = U_LIT("end");
        p_parse_46_keyword(&l_end_1473, r_ts);
        u_value t1474 = u_mk_con(&n_syntax_46_EProc, 4, v_ins, v_outs, v_refs, v_body);
        *r_e = t1474;
      }
    } else {
      {
        p_parse_46_lambda_function(v_sc, r_e, r_ts);
      }
    }
  }
}

u_value f_parse_46_at_section_1(u_value v_ts) {
  static struct u_object l_in_1475 = U_LIT("in");
  u_value t1477 = f_parse_46_at_keyword_2(v_ts, &l_in_1475);
  if (!u_truth(t1477)) {
    static struct u_object l_out_1476 = U_LIT("out");
    t1477 = f_parse_46_at_keyword_2(v_ts, &l_out_1476);
  }
  u_value t1479 = t1477;
  if (!u_truth(t1479)) {
    static struct u_object l_ref_1478 = U_LIT("ref");
    t1479 = f_parse_46_at_keyword_2(v_ts, &l_ref_1478);
  }
  return t1479;
}

void p_parse_46_lambda_function(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    u_value t1480 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1480;
    static struct u_object l__61__62__1481 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1481)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1482 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1482;
        u_value t1484 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1484)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1483 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1483;
          }
          t1484 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1485 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1485), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1486 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1486;
  }
}

void p_parse_46_parse_braces(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_at = NULL;
    u_value v_k = NULL;
    u_value v_v = NULL;
    u_value v_entries = NULL;
    u_value v_rev = NULL;
    u_value t1487 = f_parse_46_head_offset_1(*r_ts);
    v_at = t1487;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_expr(v_sc, &v_k, r_ts);
    static struct u_object l__61__62__1488 = U_LIT("=>");
    u_value t1489 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1488));
    v_entries = t1489;
    if (u_truth(v_entries)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_v, r_ts);
      }
    }
    static struct u_object l_for_1490 = U_LIT("for");
    u_value t1491 = f_parse_46_at_keyword_2(*r_ts, &l_for_1490);
    if (u_truth(t1491)) {
      t1491 = v_entries;
    }
    if (u_truth(t1491)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_comprehension(v_sc, v_k, v_v, r_e, r_ts);
      }
    } else {
      static struct u_object l_for_1492 = U_LIT("for");
      if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1492))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_set_comprehension(v_sc, v_k, r_e, r_ts);
        }
      } else {
        {
          u_value t1493;
          if (u_truth(v_entries)) {
            t1493 = u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))));
          } else {
            t1493 = v_k;
          }
          u_value t1494 = u_mk_con(&n_list_46_Cons, 2, t1493, u_mk_con(&n_list_46_Nil, 0));
          v_rev = t1494;
          u_value t1499 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          while (u_truth(t1499)) {
            {
              p_parse_46_advance(r_ts);
              p_parse_46_parse_expr(v_sc, &v_k, r_ts);
              static struct u_object l__61__62__1495 = U_LIT("=>");
              if (u_truth(f_prelude_46__33__61__2(v_entries, u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1495))))) {
                {
                  static struct u_object l_a_32_literal_32__123__46__46__46__125__32__1496 = U_LIT("a literal {...} has either elements a or entries k => v");
                  u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_a_32_literal_32__123__46__46__46__125__32__1496, v_at), "parse.ParseError");
                }
              }
              if (u_truth(v_entries)) {
                {
                  p_parse_46_advance(r_ts);
                  p_parse_46_parse_expr(v_sc, &v_v, r_ts);
                  u_value t1497 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)))), v_rev);
                  v_rev = t1497;
                }
              } else {
                {
                  u_value t1498 = u_mk_con(&n_list_46_Cons, 2, v_k, v_rev);
                  v_rev = t1498;
                }
              }
            }
            t1499 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
          }
          u_value t1506;
          if (u_truth(v_entries)) {
            static struct u_object l_map_1500 = U_LIT("map");
            static struct u_object l_put_all_1501 = U_LIT("put_all");
            static struct u_object l_map_1502 = U_LIT("map");
            static struct u_object l_empty_1503 = U_LIT("empty");
            t1506 = f_parse_46_in_library_3(&l_map_1500, &l_put_all_1501, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1502, &l_empty_1503, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
          } else {
            static struct u_object l_set_1504 = U_LIT("set");
            static struct u_object l_from_list_1505 = U_LIT("from_list");
            t1506 = f_parse_46_in_library_3(&l_set_1504, &l_from_list_1505, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_rev), f_parse_46_nil_expr_0()), u_mk_con(&n_list_46_Nil, 0)));
          }
          u_value t1507 = t1506;
          *r_e = t1507;
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
  static struct u_object l_Nil_1508 = U_LIT("Nil");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1508), u_mk_con(&n_syntax_46_THole, 0)));
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
    u_value t1509 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1509;
    u_value t1510 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1510;
    u_value t1511 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1511;
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l_in_1512 = U_LIT("in");
    p_parse_46_keyword(&l_in_1512, r_ts);
    p_parse_46_parse_expr(v_sc, &v_l, r_ts);
    static struct u_object l_set_1513 = U_LIT("set");
    static struct u_object l_elements_1514 = U_LIT("elements");
    u_value t1515 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    u_value t1516 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1513, &l_elements_1514, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
    v_xs = t1515;
    v_ls = t1516;
    u_value t1522 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1522)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l_in_1517 = U_LIT("in");
        p_parse_46_keyword(&l_in_1517, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_set_1518 = U_LIT("set");
        static struct u_object l_elements_1519 = U_LIT("elements");
        u_value t1520 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
        u_value t1521 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_set_1518, &l_elements_1519, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0))), v_ls);
        v_xs = t1520;
        v_ls = t1521;
      }
      t1522 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1523 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1523))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
      }
    }
    static struct u_object l_set_1524 = U_LIT("set");
    static struct u_object l_from_list_1525 = U_LIT("from_list");
    u_value t1526 = f_parse_46_in_library_3(&l_set_1524, &l_from_list_1525, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, v_r, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Nil, 0)));
    *r_e = t1526;
  }
}

void p_parse_46_map_comprehension(u_value v_sc, u_value v_k, u_value v_v, u_value *r_e, u_value *r_ts) {
  {
    u_value v_gs = NULL;
    u_value v_ls = NULL;
    u_value v_c = NULL;
    u_value t1527 = u_mk_con(&n_list_46_Nil, 0);
    v_gs = t1527;
    u_value t1528 = u_mk_con(&n_list_46_Nil, 0);
    v_ls = t1528;
    u_value t1529 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
    v_c = t1529;
    p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
    u_value t1530 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1530)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_map_generator(v_sc, &v_gs, &v_ls, r_ts);
      }
      t1530 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_if_1531 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1531))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_c, r_ts);
        u_value t1532 = f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), v_c);
        v_c = t1532;
      }
    }
    static struct u_object l_map_1533 = U_LIT("map");
    static struct u_object l_put_all_1534 = U_LIT("put_all");
    static struct u_object l_map_1535 = U_LIT("map");
    static struct u_object l_empty_1536 = U_LIT("empty");
    u_value t1537 = f_parse_46_in_library_3(&l_map_1533, &l_put_all_1534, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_EComp, 4, f_parse_46_unpack_2(f_list_46_reverse_1(v_gs), u_mk_con(&n_syntax_46_ETuple, 1, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0))))), f_list_46_reverse_1(f_parse_46_hidden_vars_1(v_gs)), f_list_46_reverse_1(v_ls), v_c), u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1535, &l_empty_1536, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
    *r_e = t1537;
  }
}

void p_parse_46_map_generator(u_value v_sc, u_value *r_gs, u_value *r_ls, u_value *r_ts) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value v_m = NULL;
    p_parse_46_parse_plain(v_sc, &v_a, r_ts);
    static struct u_object l__61__62__1538 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1538), r_ts);
    p_parse_46_parse_plain(v_sc, &v_b, r_ts);
    static struct u_object l_in_1539 = U_LIT("in");
    p_parse_46_keyword(&l_in_1539, r_ts);
    p_parse_46_parse_expr(v_sc, &v_m, r_ts);
    static struct u_object l_map_1540 = U_LIT("map");
    static struct u_object l_entries_1541 = U_LIT("entries");
    u_value t1542 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_in_library_3(&l_map_1540, &l_entries_1541, u_mk_con(&n_list_46_Cons, 2, f_parse_46_unpack_2(f_list_46_reverse_1(*r_gs), v_m), u_mk_con(&n_list_46_Nil, 0))), *r_ls);
    *r_ls = t1542;
    static struct u_object l__40_entry_32__1543 = U_LIT("(entry ");
    static struct u_object l__41__1544 = U_LIT(")");
    u_value t1545 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(3, v_a, v_b, u_concat(&l__40_entry_32__1543, u_concat(u_str_show(u_add(f_list_46_length_1(*r_gs), u_mk_int(1))), &l__41__1544))), *r_gs);
    *r_gs = t1545;
  }
}

u_value f_parse_46_hidden_vars_1(u_value v_gs) {
  u_value s1546 = v_gs;
  u_value t1547 = NULL;
  if ((!t1547) && u_is_con(s1546, &n_list_46_Nil)) {
    t1547 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1547) && u_is_con(s1546, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s1546, 0), 2);
    u_value v_rest = u_field(s1546, 1);
    t1547 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0)), f_parse_46_hidden_vars_1(v_rest));
  }
  if (!t1547) {
    u_no_case(s1546);
  }
  return t1547;
}

u_value f_parse_46_unpack_2(u_value v_gs, u_value v_e) {
  u_value s1548 = v_gs;
  u_value t1549 = NULL;
  if ((!t1549) && u_is_con(s1548, &n_list_46_Nil)) {
    t1549 = v_e;
  }
  if ((!t1549) && u_is_con(s1548, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s1548, 0), 0);
    u_value v_v = u_field(u_field(s1548, 0), 1);
    u_value v_h = u_field(u_field(s1548, 0), 2);
    u_value v_rest = u_field(s1548, 1);
    t1549 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_h), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_k), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_v), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_parse_46_unpack_2(v_rest, v_e)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1549) {
    u_no_case(s1548);
  }
  return t1549;
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
        static struct u_object l_Nil_1550 = U_LIT("Nil");
        u_value t1551 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1550), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1551;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1552 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1552))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1553 = U_LIT("in");
            p_parse_46_keyword(&l_in_1553, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1554 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1555 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1554;
            v_lists = t1555;
            u_value t1559 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1559)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1556 = U_LIT("in");
                p_parse_46_keyword(&l_in_1556, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1557 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1558 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1557;
                v_lists = t1558;
              }
              t1559 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1560 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1560;
            static struct u_object l_if_1561 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1561))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1562 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1562;
          }
        } else {
          {
            u_value t1563 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1563;
            u_value t1565 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1565)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1564 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1564;
              }
              t1565 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1566 = U_LIT("Nil");
            u_value t1567 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1566), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1567;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1568 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1568;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1569 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1569))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1570 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1570;
      }
    } else {
      {
        u_value t1571 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1571;
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
    u_value t1572 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1572;
    static struct u_object l_case_1573 = U_LIT("case");
    u_value t1576 = f_parse_46_at_keyword_2(*r_ts, &l_case_1573);
    while (u_truth(t1576)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1574 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1574), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1575 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1575;
      }
      static struct u_object l_case_1577 = U_LIT("case");
      t1576 = f_parse_46_at_keyword_2(*r_ts, &l_case_1577);
    }
    static struct u_object l_else_1578 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1578))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1579 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1579;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1580 = U_LIT("end");
    p_parse_46_keyword(&l_end_1580, r_ts);
    u_value t1581 = f_list_46_reverse_1(v_rev);
    *r_cs = t1581;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1582 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1582))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1583 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1583, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
  }
}

u_value f_parse_46_pat_op1_2(u_value v_op, u_value v_p) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_pat_prefix_2(u_value v_op, u_value v_p) {
  u_value s1584 = v_p;
  u_value t1585 = NULL;
  if ((!t1585) && (u_is_con(s1584, &n_syntax_46_PLit) && u_is_con(u_field(s1584, 0), &n_syntax_46_VInt))) {
    u_value v_n = u_field(u_field(s1584, 0), 0);
    static struct u_object l__45__1586 = U_LIT("-");
    if (u_truth(u_eq(v_op, &l__45__1586))) {
      t1585 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, u_sub(u_mk_int(0), v_n)));
    }
  }
  if (!t1585) {
    t1585 = f_parse_46_pat_op1_2(v_op, v_p);
  }
  if (!t1585) {
    u_no_case(s1584);
  }
  return t1585;
}

u_value f_parse_46_pat_op2_3(u_value v_op, u_value v_l, u_value v_r) {
  return u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_pats_2(u_value v_ps, u_value v_rest) {
  u_value s1587 = v_ps;
  u_value t1588 = NULL;
  if ((!t1588) && u_is_con(s1587, &n_list_46_Nil)) {
    t1588 = v_rest;
  }
  if ((!t1588) && u_is_con(s1587, &n_list_46_Cons)) {
    u_value v_p = u_field(s1587, 0);
    u_value v_more = u_field(s1587, 1);
    static struct u_object l_Cons_1589 = U_LIT("Cons");
    t1588 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1589), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1588) {
    u_no_case(s1587);
  }
  return t1588;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1590 = v_t;
  u_value t1591 = NULL;
  if ((!t1591) && u_is_con(s1590, &n_lex_46_TName)) {
    u_value v_n = u_field(s1590, 0);
    static struct u_object l___1592 = U_LIT("_");
    static struct u_object l_true_1593 = U_LIT("true");
    static struct u_object l_false_1594 = U_LIT("false");
    u_value t1596 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1592, u_mk_con(&n_list_46_Cons, 2, &l_true_1593, u_mk_con(&n_list_46_Cons, 2, &l_false_1594, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1596)) {
      u_value t1595 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1595)) {
        t1595 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1596 = t1595;
    }
    t1591 = t1596;
  }
  if ((!t1591) && u_is_con(s1590, &n_lex_46_TNum)) {
    t1591 = u_mk_bool(1);
  }
  if ((!t1591) && u_is_con(s1590, &n_lex_46_TStr)) {
    t1591 = u_mk_bool(1);
  }
  if ((!t1591) && u_is_con(s1590, &n_lex_46_TChar)) {
    t1591 = u_mk_bool(1);
  }
  if ((!t1591) && u_is_con(s1590, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1590, 0);
    u_value t1597 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1597)) {
      t1597 = u_eq(v_c, u_mk_char(91));
    }
    t1591 = t1597;
  }
  if (!t1591) {
    t1591 = u_mk_bool(0);
  }
  if (!t1591) {
    u_no_case(s1590);
  }
  return t1591;
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
    u_value t1598 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1598;
    u_value t1599 = u_mk_bool(1);
    v_next = t1599;
    u_value t1600 = u_mk_bool(1);
    v_go = t1600;
    u_value t1609 = v_go;
    while (u_truth(t1609)) {
      {
        u_value s1601 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1602 = 0;
        if ((!d1602) && u_is_con(s1601, &n_option_46_Some)) {
          u_value v_op = u_field(s1601, 0);
          d1602 = 1;
          {
            u_value t1603 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1604 = u_mk_bool(1);
            v_rev = t1603;
            v_next = t1604;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1602) && u_is_con(s1601, &n_option_46_None)) {
          u_value t1607 = v_next;
          if (u_truth(t1607)) {
            t1607 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1607)) {
            d1602 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1605 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1606 = u_mk_bool(0);
              v_rev = t1605;
              v_next = t1606;
            }
          }
        }
        if (!d1602) {
          d1602 = 1;
          {
            u_value t1608 = u_mk_bool(0);
            v_go = t1608;
          }
        }
        if (!d1602) {
          u_no_case(s1601);
        }
      }
      t1609 = v_go;
    }
    u_value s1610 = v_rev;
    int d1611 = 0;
    if ((!d1611) && u_is_con(s1610, &n_list_46_Nil)) {
      d1611 = 1;
      {
        static struct u_object l_pattern_32_expected_1612 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1613 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1612, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1613)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1611) {
      d1611 = 1;
      {
        u_value t1614 = f_list_46_reverse_1(v_rev);
        *r_items = t1614;
      }
    }
    if (!d1611) {
      u_no_case(s1610);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1615 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1615;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1616 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1616;
        u_value t1618 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1618)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1617 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1617;
          }
          t1618 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1619 = f_list_46_reverse_1(v_rev);
    *r_ps = t1619;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1620 = f_parse_46_head_token_1(*r_ts);
    int d1621 = 0;
    if ((!d1621) && (u_is_con(s1620, &n_lex_46_TName) && u_equals(u_field(s1620, 0), u_mk_lit("_")))) {
      d1621 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1622 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1622;
      }
    }
    if ((!d1621) && (u_is_con(s1620, &n_lex_46_TName) && u_equals(u_field(s1620, 0), u_mk_lit("true")))) {
      d1621 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1623 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1623;
      }
    }
    if ((!d1621) && (u_is_con(s1620, &n_lex_46_TName) && u_equals(u_field(s1620, 0), u_mk_lit("false")))) {
      d1621 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1624 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1624;
      }
    }
    if ((!d1621) && u_is_con(s1620, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1620, 0);
      d1621 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1625 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1625;
      }
    }
    if ((!d1621) && u_is_con(s1620, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1620, 0);
      d1621 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1626 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1626;
      }
    }
    if ((!d1621) && u_is_con(s1620, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1620, 0);
      d1621 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1627 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1627;
      }
    }
    if ((!d1621) && (u_is_con(s1620, &n_lex_46_TPunct) && u_equals(u_field(s1620, 0), u_mk_char(40)))) {
      d1621 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1628 = v_ps;
        int d1629 = 0;
        if ((!d1629) && (u_is_con(s1628, &n_list_46_Cons) && u_is_con(u_field(s1628, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1628, 0);
          d1629 = 1;
          {
            u_value t1630 = v_q;
            *r_p = t1630;
          }
        }
        if (!d1629) {
          d1629 = 1;
          {
            u_value t1631 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1631;
          }
        }
        if (!d1629) {
          u_no_case(s1628);
        }
      }
    }
    if ((!d1621) && (u_is_con(s1620, &n_lex_46_TPunct) && u_equals(u_field(s1620, 0), u_mk_char(91)))) {
      d1621 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1632 = U_LIT("Nil");
            u_value t1633 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1632), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1633;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1634 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1634;
            u_value t1636 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1636)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1635 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1635;
              }
              t1636 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1637 = U_LIT("Nil");
            u_value t1638 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1637), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1638;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1639 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1639;
          }
        }
      }
    }
    if (!d1621) {
      d1621 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1640 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1640;
          }
        } else {
          {
            u_value t1641 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1641;
          }
        }
      }
    }
    if (!d1621) {
      u_no_case(s1620);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1642 = v_t;
  u_value t1643 = NULL;
  if ((!t1643) && u_is_con(s1642, &n_lex_46_TName)) {
    u_value v_n = u_field(s1642, 0);
    static struct u_object l_skip_1644 = U_LIT("skip");
    static struct u_object l_if_1645 = U_LIT("if");
    static struct u_object l_while_1646 = U_LIT("while");
    static struct u_object l_call_1647 = U_LIT("call");
    static struct u_object l_match_1648 = U_LIT("match");
    static struct u_object l_assert_1649 = U_LIT("assert");
    static struct u_object l_assign_1650 = U_LIT("assign");
    static struct u_object l_raise_1651 = U_LIT("raise");
    static struct u_object l_try_1652 = U_LIT("try");
    t1643 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1644, u_mk_con(&n_list_46_Cons, 2, &l_if_1645, u_mk_con(&n_list_46_Cons, 2, &l_while_1646, u_mk_con(&n_list_46_Cons, 2, &l_call_1647, u_mk_con(&n_list_46_Cons, 2, &l_match_1648, u_mk_con(&n_list_46_Cons, 2, &l_assert_1649, u_mk_con(&n_list_46_Cons, 2, &l_assign_1650, u_mk_con(&n_list_46_Cons, 2, &l_raise_1651, u_mk_con(&n_list_46_Cons, 2, &l_try_1652, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1643) {
    t1643 = u_mk_bool(0);
  }
  if (!t1643) {
    u_no_case(s1642);
  }
  return t1643;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1653 = v_cs;
  u_value t1654 = NULL;
  if ((!t1654) && u_is_con(s1653, &n_list_46_Nil)) {
    t1654 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1654) && (u_is_con(s1653, &n_list_46_Cons) && u_is_con(u_field(s1653, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1653, 0), 0);
    u_value v_g = u_field(u_field(s1653, 0), 1);
    u_value v_b = u_field(u_field(s1653, 0), 2);
    u_value v_more = u_field(s1653, 1);
    t1654 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1654) {
    u_no_case(s1653);
  }
  return t1654;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1655 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1655;
    u_value t1656 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1656;
    u_value t1657 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1657;
    static struct u_object l_local_1658 = U_LIT("local");
    u_value t1660 = f_parse_46_at_keyword_2(*r_ts, &l_local_1658);
    while (u_truth(t1660)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1659 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1659)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1659 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1661 = U_LIT("local");
      t1660 = f_parse_46_at_keyword_2(*r_ts, &l_local_1661);
    }
    u_value t1663 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1663)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1662 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1662;
      }
      t1663 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1664 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1664;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1665 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1665;
    static struct u_object l__58__1666 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1666)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__1667 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1667)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1668 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1668;
      }
    }
    u_value t1669 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1669;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1670 = f_parse_46_head_token_1(*r_ts);
    int d1671 = 0;
    if ((!d1671) && (u_is_con(s1670, &n_lex_46_TName) && u_equals(u_field(s1670, 0), u_mk_lit("skip")))) {
      d1671 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1672 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1672;
      }
    }
    if ((!d1671) && (u_is_con(s1670, &n_lex_46_TName) && u_equals(u_field(s1670, 0), u_mk_lit("if")))) {
      d1671 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1671) && (u_is_con(s1670, &n_lex_46_TName) && u_equals(u_field(s1670, 0), u_mk_lit("while")))) {
      d1671 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1673 = U_LIT("end");
        p_parse_46_keyword(&l_end_1673, r_ts);
        u_value t1674 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1674;
      }
    }
    if ((!d1671) && (u_is_con(s1670, &n_lex_46_TName) && u_equals(u_field(s1670, 0), u_mk_lit("call")))) {
      d1671 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1671) && (u_is_con(s1670, &n_lex_46_TName) && u_equals(u_field(s1670, 0), u_mk_lit("match")))) {
      d1671 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1675 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1675;
      }
    }
    if ((!d1671) && (u_is_con(s1670, &n_lex_46_TName) && u_equals(u_field(s1670, 0), u_mk_lit("assert")))) {
      d1671 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1676 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1676;
      }
    }
    if ((!d1671) && (u_is_con(s1670, &n_lex_46_TName) && u_equals(u_field(s1670, 0), u_mk_lit("assign")))) {
      d1671 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1671) && (u_is_con(s1670, &n_lex_46_TName) && u_equals(u_field(s1670, 0), u_mk_lit("raise")))) {
      d1671 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1677 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1677;
      }
    }
    if ((!d1671) && (u_is_con(s1670, &n_lex_46_TName) && u_equals(u_field(s1670, 0), u_mk_lit("try")))) {
      d1671 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1678 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1678, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1679 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1679;
      }
    }
    if (!d1671) {
      d1671 = 1;
      {
        static struct u_object l_statement_32_expect_1680 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1681 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1680, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1681)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1671) {
      u_no_case(s1670);
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
    static struct u_object l_else_1682 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1682)))) {
      {
        static struct u_object l_end_1683 = U_LIT("end");
        p_parse_46_keyword(&l_end_1683, r_ts);
        u_value t1684 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1684;
      }
    } else {
      static struct u_object l_if_1685 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1685)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1686 = U_LIT("end");
          p_parse_46_keyword(&l_end_1686, r_ts);
        }
      }
    }
    u_value t1687 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1687;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1688 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1688), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1689 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1689;
    u_value t1692 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1692)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1690 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1690), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1691 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1691;
      }
      t1692 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1693 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1693;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1694 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1694;
    static struct u_object l_case_1695 = U_LIT("case");
    u_value t1698 = f_parse_46_at_keyword_2(*r_ts, &l_case_1695);
    while (u_truth(t1698)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1696 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1696), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1697 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1697;
      }
      static struct u_object l_case_1699 = U_LIT("case");
      t1698 = f_parse_46_at_keyword_2(*r_ts, &l_case_1699);
    }
    static struct u_object l_else_1700 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1700))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1701 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1701;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1702 = U_LIT("end");
    p_parse_46_keyword(&l_end_1702, r_ts);
    u_value t1703 = f_list_46_reverse_1(v_rev);
    *r_cs = t1703;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1705;
  if (u_truth(v_first)) {
    t1705 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1704 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1704)) {
      t1704 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1705 = t1704;
  }
  return t1705;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1707 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1707)) {
    static struct u_object l_out_1706 = U_LIT("out");
    t1707 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1706));
  }
  u_value t1709 = t1707;
  if (u_truth(t1709)) {
    static struct u_object l_ref_1708 = U_LIT("ref");
    t1709 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1708));
  }
  return t1709;
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
    u_value t1710 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), v_ins, v_outs, v_refs);
    *r_s = t1710;
  }
}

void p_parse_46_sections(u_value v_item_in, u_value v_item, u_value v_sc, u_value *r_ins, u_value *r_outs, u_value *r_refs, u_value *r_ts) {
  {
    u_value v_first = NULL;
    u_value t1711 = u_mk_bool(1);
    v_first = t1711;
    u_value t1712 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1713 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1714 = u_mk_con(&n_list_46_Nil, 0);
    *r_ins = t1712;
    *r_outs = t1713;
    *r_refs = t1714;
    static struct u_object l_in_1715 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1715))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_section_items(v_item_in, v_sc, r_ins, r_ts);
        u_value t1716 = u_mk_bool(0);
        v_first = t1716;
      }
    }
    static struct u_object l_out_1717 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1717, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_section_items(v_item, v_sc, r_outs, r_ts);
        u_value t1718 = u_mk_bool(0);
        v_first = t1718;
      }
    }
    static struct u_object l_ref_1719 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1719, v_first))) {
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
    u_value t1720 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1720;
    u_value t1722 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1722)) {
      {
        p_parse_46_advance(r_ts);
        u_call_proc(v_item, 1, 2, v_sc, &v_x, r_ts);
        u_value t1721 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1721;
      }
      t1722 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1723 = f_list_46_reverse_1(v_rev);
    *r_xs = t1723;
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
    u_value t1724 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0));
    *r_x = t1724;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1725 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1725;
    static struct u_object l__58__1726 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1726)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1727 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1727;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1728 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1728;
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
  u_value s1729 = v_cs;
  u_value t1730 = NULL;
  if ((!t1730) && u_is_con(s1729, &n_list_46_Nil)) {
    t1730 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1730) && (u_is_con(s1729, &n_list_46_Cons) && u_is_con(u_field(s1729, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1729, 1);
    t1730 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1730) && (u_is_con(s1729, &n_list_46_Cons) && u_is_con(u_field(s1729, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1729, 1);
    t1730 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1730) && u_is_con(s1729, &n_list_46_Cons)) {
    u_value v_c = u_field(s1729, 0);
    u_value v_more = u_field(s1729, 1);
    t1730 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1730) {
    u_no_case(s1729);
  }
  return t1730;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1731 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1732 = &t1731;
  u_value l1733 = v_cs;
  while (u_is_con(l1733, &n_list_46_Cons)) {
    u_value v_c = u_field(l1733, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1732 = u_append(e1732, v_c);
    }
    l1733 = u_field(l1733, 1);
  }
  return t1731;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1734 = v_c;
  u_value t1735 = NULL;
  if ((!t1735) && u_is_con(s1734, &n_syntax_46_CNotation)) {
    t1735 = u_mk_bool(1);
  }
  if (!t1735) {
    t1735 = u_mk_bool(0);
  }
  if (!t1735) {
    u_no_case(s1734);
  }
  return t1735;
}

u_value f_parse_46_Context_46_notations_1(u_value v_x) {
  u_value s1736 = v_x;
  u_value t1737 = NULL;
  if ((!t1737) && u_is_con(s1736, &n_parse_46_Context)) {
    u_value v_y = u_field(s1736, 0);
    t1737 = v_y;
  }
  if (!t1737) {
    u_no_case(s1736);
  }
  return t1737;
}

u_value f_parse_46_Context_46_modules_1(u_value v_x) {
  u_value s1738 = v_x;
  u_value t1739 = NULL;
  if ((!t1739) && u_is_con(s1738, &n_parse_46_Context)) {
    u_value v_y = u_field(s1738, 1);
    t1739 = v_y;
  }
  if (!t1739) {
    u_no_case(s1738);
  }
  return t1739;
}

u_value f_parse_46_Context_46_loading_1(u_value v_x) {
  u_value s1740 = v_x;
  u_value t1741 = NULL;
  if ((!t1741) && u_is_con(s1740, &n_parse_46_Context)) {
    u_value v_y = u_field(s1740, 2);
    t1741 = v_y;
  }
  if (!t1741) {
    u_no_case(s1740);
  }
  return t1741;
}

u_value f_parse_46_Context_46_base_1(u_value v_x) {
  u_value s1742 = v_x;
  u_value t1743 = NULL;
  if ((!t1743) && u_is_con(s1742, &n_parse_46_Context)) {
    u_value v_y = u_field(s1742, 3);
    t1743 = v_y;
  }
  if (!t1743) {
    u_no_case(s1742);
  }
  return t1743;
}

u_value f_parse_46_Context_46_dir_1(u_value v_x) {
  u_value s1744 = v_x;
  u_value t1745 = NULL;
  if ((!t1745) && u_is_con(s1744, &n_parse_46_Context)) {
    u_value v_y = u_field(s1744, 4);
    t1745 = v_y;
  }
  if (!t1745) {
    u_no_case(s1744);
  }
  return t1745;
}

u_value f_parse_46_Context_46_notations_58__61__2(u_value v_x, u_value v_y) {
  u_value s1746 = v_x;
  u_value t1747 = NULL;
  if ((!t1747) && u_is_con(s1746, &n_parse_46_Context)) {
    u_value v_a1 = u_field(s1746, 1);
    u_value v_a2 = u_field(s1746, 2);
    u_value v_a3 = u_field(s1746, 3);
    u_value v_a4 = u_field(s1746, 4);
    t1747 = u_mk_con(&n_parse_46_Context, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t1747) {
    u_no_case(s1746);
  }
  return t1747;
}

u_value f_parse_46_Context_46_modules_58__61__2(u_value v_x, u_value v_y) {
  u_value s1748 = v_x;
  u_value t1749 = NULL;
  if ((!t1749) && u_is_con(s1748, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1748, 0);
    u_value v_a2 = u_field(s1748, 2);
    u_value v_a3 = u_field(s1748, 3);
    u_value v_a4 = u_field(s1748, 4);
    t1749 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t1749) {
    u_no_case(s1748);
  }
  return t1749;
}

u_value f_parse_46_Context_46_loading_58__61__2(u_value v_x, u_value v_y) {
  u_value s1750 = v_x;
  u_value t1751 = NULL;
  if ((!t1751) && u_is_con(s1750, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1750, 0);
    u_value v_a1 = u_field(s1750, 1);
    u_value v_a3 = u_field(s1750, 3);
    u_value v_a4 = u_field(s1750, 4);
    t1751 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t1751) {
    u_no_case(s1750);
  }
  return t1751;
}

u_value f_parse_46_Context_46_base_58__61__2(u_value v_x, u_value v_y) {
  u_value s1752 = v_x;
  u_value t1753 = NULL;
  if ((!t1753) && u_is_con(s1752, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1752, 0);
    u_value v_a1 = u_field(s1752, 1);
    u_value v_a2 = u_field(s1752, 2);
    u_value v_a4 = u_field(s1752, 4);
    t1753 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t1753) {
    u_no_case(s1752);
  }
  return t1753;
}

u_value f_parse_46_Context_46_dir_58__61__2(u_value v_x, u_value v_y) {
  u_value s1754 = v_x;
  u_value t1755 = NULL;
  if ((!t1755) && u_is_con(s1754, &n_parse_46_Context)) {
    u_value v_a0 = u_field(s1754, 0);
    u_value v_a1 = u_field(s1754, 1);
    u_value v_a2 = u_field(s1754, 2);
    u_value v_a3 = u_field(s1754, 3);
    t1755 = u_mk_con(&n_parse_46_Context, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t1755) {
    u_no_case(s1754);
  }
  return t1755;
}

u_value f_parse_46_with_commands_2(u_value v_cx, u_value v_cs) {
  return f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_with_notations_2(f_parse_46_Context_46_notations_1(v_cx), v_cs));
}

u_value f_parse_46_with_module_2(u_value v_cx, u_value v_c) {
  return f_parse_46_Context_46_modules_58__61__2(v_cx, f_list_46__43__43__2(f_parse_46_modules_of_1(v_c), f_parse_46_Context_46_modules_1(v_cx)));
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1756 = v_c;
  u_value t1757 = NULL;
  if ((!t1757) && u_is_con(s1756, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1756, 0);
    u_value v_cs = u_field(s1756, 1);
    u_value t1758 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1759 = &t1758;
    u_value l1760 = v_cs;
    while (u_is_con(l1760, &n_list_46_Cons)) {
      u_value v_d = u_field(l1760, 0);
      if (u_truth(u_mk_bool(1))) {
        e1759 = u_append(e1759, f_parse_46_modules_of_1(v_d));
      }
      l1760 = u_field(l1760, 1);
    }
    t1757 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1758));
  }
  if (!t1757) {
    t1757 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1757) {
    u_no_case(s1756);
  }
  return t1757;
}

u_value f_parse_46_own_notations_2(u_value v_cx, u_value v_n) {
  u_value s1761 = f_list_46_lookup_2(v_n, f_parse_46_Context_46_modules_1(v_cx));
  u_value t1762 = NULL;
  if ((!t1762) && u_is_con(s1761, &n_option_46_Some)) {
    u_value v_ns = u_field(s1761, 0);
    t1762 = v_ns;
  }
  if ((!t1762) && u_is_con(s1761, &n_option_46_None)) {
    t1762 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1762) {
    u_no_case(s1761);
  }
  return t1762;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1763 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1763;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1764 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1764;
        u_value t1766 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1766)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1765 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1765;
          }
          t1766 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1767 = f_list_46_reverse_1(v_rev);
    *r_ps = t1767;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1768 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1768;
    u_value s1769 = f_parse_46_head_token_1(*r_ts);
    int d1770 = 0;
    if ((!d1770) && (u_is_con(s1769, &n_lex_46_TName) && u_equals(u_field(s1769, 0), u_mk_lit("prefix")))) {
      d1770 = 1;
      {
        u_value t1771 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1771;
      }
    }
    if ((!d1770) && (u_is_con(s1769, &n_lex_46_TName) && u_equals(u_field(s1769, 0), u_mk_lit("postfix")))) {
      d1770 = 1;
      {
        u_value t1772 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1772;
      }
    }
    if (!d1770) {
      d1770 = 1;
      {
        u_value s1773 = f_parse_46_second_token_1(*r_ts);
        int d1774 = 0;
        if ((!d1774) && (u_is_con(s1773, &n_lex_46_TName) && u_equals(u_field(s1773, 0), u_mk_lit("left")))) {
          d1774 = 1;
          {
            u_value t1775 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1775;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1774) && (u_is_con(s1773, &n_lex_46_TName) && u_equals(u_field(s1773, 0), u_mk_lit("right")))) {
          d1774 = 1;
          {
            u_value t1776 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1776;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1774) {
          d1774 = 1;
          {
          }
        }
        if (!d1774) {
          u_no_case(s1773);
        }
      }
    }
    if (!d1770) {
      u_no_case(s1769);
    }
    p_parse_46_advance(r_ts);
    u_value s1777 = f_parse_46_head_token_1(*r_ts);
    int d1778 = 0;
    if ((!d1778) && u_is_con(s1777, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1777, 0);
      d1778 = 1;
      {
        u_value t1779 = v_n;
        v_p = t1779;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1778) {
      d1778 = 1;
      {
        static struct u_object l_precedence_32_expec_1780 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1781 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1780, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1781)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1778) {
      u_no_case(s1777);
    }
    u_value t1782 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1783 = u_mk_bool(1);
    v_ops = t1782;
    v_go = t1783;
    u_value t1789 = v_go;
    while (u_truth(t1789)) {
      {
        u_value s1784 = f_parse_46_head_token_1(*r_ts);
        int d1785 = 0;
        if ((!d1785) && u_is_con(s1784, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1784, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1785 = 1;
            {
              u_value t1786 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1786;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1785) && u_is_con(s1784, &n_lex_46_TName)) {
          u_value v_n = u_field(s1784, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1785 = 1;
            {
              u_value t1787 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1787;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1785) {
          d1785 = 1;
          {
            u_value t1788 = u_mk_bool(0);
            v_go = t1788;
          }
        }
        if (!d1785) {
          u_no_case(s1784);
        }
      }
      t1789 = v_go;
    }
    u_value s1790 = v_ops;
    int d1791 = 0;
    if ((!d1791) && u_is_con(s1790, &n_list_46_Nil)) {
      d1791 = 1;
      {
        static struct u_object l_operator_32_expecte_1792 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1793 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1792, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1793)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1791) {
      d1791 = 1;
      {
        u_value t1794 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1794;
      }
    }
    if (!d1791) {
      u_no_case(s1790);
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
    u_value t1795 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1796 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1795;
    *r_fs = t1796;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1797 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
            u_value t1798 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
            v_args = t1797;
            *r_fs = t1798;
            u_value t1801 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1801)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t1799 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t1800 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t1799;
                *r_fs = t1800;
              }
              t1801 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1802 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t1803 = f_list_46_reverse_1(*r_fs);
    *r_k = t1802;
    *r_fs = t1803;
    u_value s1804 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d1805 = 0;
    if ((!d1805) && u_is_con(s1804, &n_option_46_Some)) {
      u_value v_f = u_field(s1804, 0);
      d1805 = 1;
      {
        static struct u_object l_repeated_32_field_32__1806 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__1807 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__1806, u_concat(v_f, u_concat(&l__32_of_32__1807, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1805) && u_is_con(s1804, &n_option_46_None)) {
      d1805 = 1;
      {
      }
    }
    if (!d1805) {
      u_no_case(s1804);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1808 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1809 = 0;
    if ((!d1809) && (u_is_con(u_field(s1808, 0), &n_lex_46_TName) && (u_is_con(u_field(s1808, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1808, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1808, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1809 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1810 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1810;
        }
      }
    }
    if (!d1809) {
      d1809 = 1;
      {
        u_value t1811 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1811;
      }
    }
    if (!d1809) {
      u_no_case(s1808);
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
    static struct u_object l__58__61__1812 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1812), r_ts);
    static struct u_object l__124__1813 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1813)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1814 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1814;
    static struct u_object l__124__1815 = U_LIT("|");
    u_value t1817 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1815));
    while (u_truth(t1817)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1816 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1816;
      }
      static struct u_object l__124__1818 = U_LIT("|");
      t1817 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1818));
    }
    u_value s1819 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d1820 = 0;
    if ((!d1820) && u_is_con(s1819, &n_option_46_Some)) {
      u_value v_f = u_field(s1819, 0);
      d1820 = 1;
      {
        static struct u_object l_field_32__1821 = U_LIT("field ");
        static struct u_object l__32_of_32__1822 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_1823 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__1821, u_concat(v_f, u_concat(&l__32_of_32__1822, u_concat(v_n, &l__32_has_32_different_32_t_1823)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1820) && u_is_con(s1819, &n_option_46_None)) {
      d1820 = 1;
      {
      }
    }
    if (!d1820) {
      u_no_case(s1819);
    }
    u_value t1824 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_list_46__43__43__2(f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)), f_parse_46_updaters_3(v_n, v_ps, f_list_46_reverse_1(v_rev))));
    *r_cs = t1824;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s1825 = v_fs;
  u_value t1826 = NULL;
  if ((!t1826) && u_is_con(s1825, &n_list_46_Nil)) {
    t1826 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1826) && (u_is_con(s1825, &n_list_46_Cons) && u_is_con(u_field(s1825, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s1825, 0), 0);
    u_value v_rest = u_field(s1825, 1);
    t1826 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t1826) && (u_is_con(s1825, &n_list_46_Cons) && u_is_con(u_field(s1825, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1825, 1);
    t1826 = f_parse_46_field_names_1(v_rest);
  }
  if (!t1826) {
    u_no_case(s1825);
  }
  return t1826;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s1827 = v_fs;
  u_value t1828 = NULL;
  if ((!t1828) && u_is_con(s1827, &n_list_46_Nil)) {
    t1828 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1828) && u_is_con(s1827, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1827, 0), 0);
    u_value v_t = u_field(u_field(u_field(s1827, 0), 1), 3);
    u_value v_rest = u_field(s1827, 1);
    u_value t1830;
    if (u_truth(f_list_46_exists_2(f_parse_46_fields_named_2(v_f, v_rest), u_mk_closure(lam1829, 1, v_t)))) {
      t1830 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t1830 = f_parse_46_differing_1(v_rest);
    }
    t1828 = t1830;
  }
  if (!t1828) {
    u_no_case(s1827);
  }
  return t1828;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s1831 = v_e;
  u_value t1832 = NULL;
  if (!t1832) {
    u_value v_t = u_field(u_field(s1831, 1), 3);
    t1832 = v_t;
  }
  if (!t1832) {
    u_no_case(s1831);
  }
  return t1832;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s1833 = v_fs;
  u_value t1834 = NULL;
  if ((!t1834) && u_is_con(s1833, &n_list_46_Nil)) {
    t1834 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1834) && u_is_con(s1833, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s1833, 0), 0);
    u_value v_d = u_field(u_field(s1833, 0), 1);
    u_value v_rest = u_field(s1833, 1);
    u_value t1835;
    if (u_truth(u_eq(v_g, v_f))) {
      t1835 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t1835 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t1834 = t1835;
  }
  if (!t1834) {
    u_no_case(s1833);
  }
  return t1834;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1836 = v_ks;
  u_value t1837 = NULL;
  if ((!t1837) && u_is_con(s1836, &n_list_46_Nil)) {
    t1837 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1837) && (u_is_con(s1836, &n_list_46_Cons) && u_is_con(u_field(u_field(s1836, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1836, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1836, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1836, 0), 1);
    u_value v_rest = u_field(s1836, 1);
    t1837 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1837) {
    u_no_case(s1836);
  }
  return t1837;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1838 = v_fs;
  u_value t1839 = NULL;
  if ((!t1839) && u_is_con(s1838, &n_list_46_Nil)) {
    t1839 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1839) && (u_is_con(s1838, &n_list_46_Cons) && u_is_con(u_field(u_field(s1838, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1838, 0), 0), 0);
    u_value v_t = u_field(u_field(s1838, 0), 1);
    u_value v_rest = u_field(s1838, 1);
    t1839 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1839) && (u_is_con(s1838, &n_list_46_Cons) && u_is_con(u_field(u_field(s1838, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1838, 1);
    t1839 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1839) {
    u_no_case(s1838);
  }
  return t1839;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1844 = f_parse_46_fields_of_1(v_ks);
  u_value t1843;
  {
    u_value v_fs = t1844;
    u_value t1840 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1841 = &t1840;
    u_value l1842 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1842, &n_list_46_Cons)) {
      u_value v_f = u_field(l1842, 0);
      if (u_truth(u_mk_bool(1))) {
        e1841 = u_append(e1841, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1842 = u_field(l1842, 1);
    }
    t1843 = t1840;
  }
  return t1843;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1845 = U_LIT("x");
  u_value t1846 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1847 = &t1846;
  u_value l1848 = v_ps;
  while (u_is_con(l1848, &n_list_46_Cons)) {
    u_value v_p = u_field(l1848, 0);
    if (u_truth(u_mk_bool(1))) {
      e1847 = u_append(e1847, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1848 = u_field(l1848, 1);
  }
  u_value t1856 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1845), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1857 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1846);
  u_value t1855;
  {
    u_value v_x = t1856;
    u_value v_owner = t1857;
    static struct u_object l__46__1849 = U_LIT(".");
    static struct u_object l_x_1850 = U_LIT("x");
    u_value t1851 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1852 = &t1851;
    u_value l1853 = v_es;
    while (u_is_con(l1853, &n_list_46_Cons)) {
      u_value v_e = u_field(l1853, 0);
      if (u_truth(u_mk_bool(1))) {
        e1852 = u_append(e1852, f_parse_46_field_case_1(v_e));
      }
      l1853 = u_field(l1853, 1);
    }
    u_value t1854;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1854 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1854 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1855 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1849, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1850), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1851, t1854)));
  }
  return t1855;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1858 = v_es;
  u_value t1859 = NULL;
  if ((!t1859) && u_is_con(s1858, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1858, 0), 1), 3);
    t1859 = v_t;
  }
  if (!t1859) {
    t1859 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1859) {
    u_no_case(s1858);
  }
  return t1859;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s1860 = v_e;
  u_value t1861 = NULL;
  if (!t1861) {
    u_value v_k = u_field(u_field(s1860, 1), 0);
    u_value v_i = u_field(u_field(s1860, 1), 1);
    u_value v_n = u_field(u_field(s1860, 1), 2);
    u_value t1862 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1863 = &t1862;
    u_value l1864 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1864, &n_list_46_Cons)) {
      u_value v_j = u_field(l1864, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1866;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1865 = U_LIT("y");
          t1866 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1865), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1866 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1863 = u_append(e1863, t1866);
      }
      l1864 = u_field(l1864, 1);
    }
    static struct u_object l_y_1867 = U_LIT("y");
    t1861 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1862), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1867), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1861) {
    u_no_case(s1860);
  }
  return t1861;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_MissingField_1868 = U_LIT("MissingField");
  static struct u_object l__46__1869 = U_LIT(".");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_ERaise, 2, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_MissingField_1868), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_t, u_concat(&l__46__1869, v_f)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_parse_46_updaters_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1874 = f_parse_46_fields_of_1(v_ks);
  u_value t1873;
  {
    u_value v_fs = t1874;
    u_value t1870 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1871 = &t1870;
    u_value l1872 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1872, &n_list_46_Cons)) {
      u_value v_f = u_field(l1872, 0);
      if (u_truth(u_mk_bool(1))) {
        e1871 = u_append(e1871, f_parse_46_updater_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1872 = u_field(l1872, 1);
    }
    t1873 = t1870;
  }
  return t1873;
}

u_value f_parse_46_updater_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1875 = U_LIT("x");
  u_value t1876 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1877 = &t1876;
  u_value l1878 = v_ps;
  while (u_is_con(l1878, &n_list_46_Cons)) {
    u_value v_p = u_field(l1878, 0);
    if (u_truth(u_mk_bool(1))) {
      e1877 = u_append(e1877, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1878 = u_field(l1878, 1);
  }
  u_value t1888 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1875), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1889 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1876);
  u_value t1887;
  {
    u_value v_x = t1888;
    u_value v_owner = t1889;
    static struct u_object l__46__1879 = U_LIT(".");
    static struct u_object l__58__61__1880 = U_LIT(":=");
    static struct u_object l_x_1881 = U_LIT("x");
    static struct u_object l_y_1882 = U_LIT("y");
    u_value t1883 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1884 = &t1883;
    u_value l1885 = v_es;
    while (u_is_con(l1885, &n_list_46_Cons)) {
      u_value v_e = u_field(l1885, 0);
      if (u_truth(u_mk_bool(1))) {
        e1884 = u_append(e1884, f_parse_46_update_case_1(v_e));
      }
      l1885 = u_field(l1885, 1);
    }
    u_value t1886;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1886 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1886 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1887 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1879, u_concat(v_f, &l__58__61__1880)))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1881), v_owner), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1882), f_parse_46_field_type_1(v_es)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), v_owner, u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1883, t1886)));
  }
  return t1887;
}

u_value f_parse_46_update_case_1(u_value v_e) {
  u_value s1890 = v_e;
  u_value t1891 = NULL;
  if (!t1891) {
    u_value v_k = u_field(u_field(s1890, 1), 0);
    u_value v_i = u_field(u_field(s1890, 1), 1);
    u_value v_n = u_field(u_field(s1890, 1), 2);
    u_value t1892 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1893 = &t1892;
    u_value l1894 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1894, &n_list_46_Cons)) {
      u_value v_j = u_field(l1894, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1896;
        if (u_truth(u_eq(v_j, v_i))) {
          t1896 = u_mk_con(&n_syntax_46_PWild, 0);
        } else {
          static struct u_object l_a_1895 = U_LIT("a");
          t1896 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l_a_1895, u_str_show(v_j))), u_mk_con(&n_syntax_46_THole, 0)));
        }
        e1893 = u_append(e1893, t1896);
      }
      l1894 = u_field(l1894, 1);
    }
    u_value t1897 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1898 = &t1897;
    u_value l1899 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1899, &n_list_46_Cons)) {
      u_value v_j = u_field(l1899, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1902;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1900 = U_LIT("y");
          t1902 = &l_y_1900;
        } else {
          static struct u_object l_a_1901 = U_LIT("a");
          t1902 = u_concat(&l_a_1901, u_str_show(v_j));
        }
        e1898 = u_append(e1898, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(t1902), u_mk_con(&n_syntax_46_THole, 0))));
      }
      l1899 = u_field(l1899, 1);
    }
    t1891 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1892), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_k, u_mk_con(&n_syntax_46_THole, 0))), t1897));
  }
  if (!t1891) {
    u_no_case(s1890);
  }
  return t1891;
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t1903;
  if (u_truth(u_eq(v_i, v_n))) {
    t1903 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1903 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t1903;
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
    u_value t1904 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1904;
    u_value t1906 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1906)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1905 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1905;
      }
      t1906 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1907 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1907), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1908 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1908), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1909 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1909;
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
    static struct u_object l__58__61__1910 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1910), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1911 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1911;
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
    u_value t1912 = f_parse_46_Context_46_notations_1(v_cx);
    v_ns = t1912;
    u_value s1913 = f_parse_46_head_token_1(*r_ts);
    int d1914 = 0;
    if ((!d1914) && (u_is_con(s1913, &n_lex_46_TName) && u_equals(u_field(s1913, 0), u_mk_lit("import")))) {
      d1914 = 1;
      {
        u_value t1915 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1915;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Context_46_modules_1(v_cx))))) {
          {
            u_value t1916 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1916;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_cx, v_at, r_cs);
            u_value t1917 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1917;
          }
        }
      }
    }
    if ((!d1914) && u_is_con(s1913, &n_lex_46_TName)) {
      u_value v_w = u_field(s1913, 0);
      static struct u_object l_prefix_1919 = U_LIT("prefix");
      static struct u_object l_postfix_1920 = U_LIT("postfix");
      static struct u_object l_infix_1921 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1919, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1920, u_mk_con(&n_list_46_Cons, 2, &l_infix_1921, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1914 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1918 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1918;
        }
      }
    }
    if ((!d1914) && (u_is_con(s1913, &n_lex_46_TName) && u_equals(u_field(s1913, 0), u_mk_lit("type")))) {
      d1914 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1922 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1922), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1923 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1923;
      }
    }
    if ((!d1914) && (u_is_con(s1913, &n_lex_46_TName) && u_equals(u_field(s1913, 0), u_mk_lit("data")))) {
      d1914 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1914) && (u_is_con(s1913, &n_lex_46_TName) && u_equals(u_field(s1913, 0), u_mk_lit("function")))) {
      d1914 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1924 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1924;
      }
    }
    if ((!d1914) && (u_is_con(s1913, &n_lex_46_TName) && u_equals(u_field(s1913, 0), u_mk_lit("procedure")))) {
      d1914 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1925 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1925;
      }
    }
    if ((!d1914) && (u_is_con(s1913, &n_lex_46_TName) && u_equals(u_field(s1913, 0), u_mk_lit("run")))) {
      d1914 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1926 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1926), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1927 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1927;
      }
    }
    if ((!d1914) && (u_is_con(s1913, &n_lex_46_TName) && u_equals(u_field(s1913, 0), u_mk_lit("test")))) {
      d1914 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1928 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1928), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1929 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1929;
      }
    }
    if (!d1914) {
      d1914 = 1;
      {
        static struct u_object l_declaration_32_expe_1930 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1931 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1930, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1931)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1914) {
      u_no_case(s1913);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_cx) {
  {
    u_value s1932 = v_cs;
    int d1933 = 0;
    if ((!d1933) && u_is_con(s1932, &n_list_46_Nil)) {
      d1933 = 1;
      {
      }
    }
    if ((!d1933) && (u_is_con(s1932, &n_list_46_Cons) && u_is_con(u_field(s1932, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1932, 0), 0);
      u_value v_p = u_field(u_field(s1932, 0), 1);
      u_value v_ops = u_field(u_field(s1932, 0), 2);
      u_value v_more = u_field(s1932, 1);
      d1933 = 1;
      {
        u_value t1934 = f_parse_46_with_commands_2(*r_cx, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_cx = t1934;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1933) && (u_is_con(s1932, &n_list_46_Cons) && u_is_con(u_field(s1932, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1932, 0), 0);
      u_value v_ds = u_field(u_field(s1932, 0), 1);
      u_value v_more = u_field(s1932, 1);
      d1933 = 1;
      {
        u_value t1935 = f_parse_46_with_module_2(*r_cx, u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds));
        *r_cx = t1935;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1933) && (u_is_con(s1932, &n_list_46_Cons) && u_is_con(u_field(s1932, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1932, 0), 0);
      u_value v_more = u_field(s1932, 1);
      d1933 = 1;
      {
        u_value t1936 = f_parse_46_with_commands_2(*r_cx, f_parse_46_own_notations_2(*r_cx, v_n));
        *r_cx = t1936;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1933) && u_is_con(s1932, &n_list_46_Cons)) {
      u_value v_more = u_field(s1932, 1);
      d1933 = 1;
      {
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if (!d1933) {
      u_no_case(s1932);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_cx, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1937 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1937;
    u_value t1939 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1939)) {
      {
        p_parse_46_parse_command(*r_cx, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_cx);
        u_value t1938 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1938;
      }
      t1939 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1940 = f_list_46_reverse_1(v_rev);
    *r_cs = t1940;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1941 = v_dir;
    int d1942 = 0;
    if ((!d1942) && u_is_con(s1941, &n_option_46_Some)) {
      u_value v_d = u_field(s1941, 0);
      d1942 = 1;
      {
        {
          struct u_handler *h1945 = u_push();
          if (setjmp(h1945->env) == 0) {
            {
              static struct u_object l__47__1943 = U_LIT("/");
              static struct u_object l__46_u_1944 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1943, u_concat(v_n, &l__46_u_1944))), r_text);
            }
            u_pop(h1945);
          } else {
            u_pop(h1945);
            int d1946 = 0;
            if ((!d1946) && (u_has_type(h1945, "os.OsError") && u_is_con(u_caught(h1945), &n_os_46_OsError))) {
              d1946 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1946) {
              u_reraise(h1945);
            }
          }
        }
      }
    }
    if ((!d1942) && u_is_con(s1941, &n_option_46_None)) {
      d1942 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1942) {
      u_no_case(s1941);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1947 = u_push();
      if (setjmp(h1947->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1947);
      } else {
        u_pop(h1947);
        int d1948 = 0;
        if ((!d1948) && (u_has_type(h1947, "os.OsError") && u_is_con(u_caught(h1947), &n_os_46_OsError))) {
          d1948 = 1;
          {
            static struct u_object l_unknown_32_module_32__1949 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1949, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1948) {
          u_reraise(h1947);
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
        static struct u_object l_cyclic_32_import_32_of_1950 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1950, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Context_46_dir_1(v_cx), v_at, &v_text);
    u_value t1951 = f_parse_46_Context_46_loading_58__61__2(f_parse_46_Context_46_notations_58__61__2(v_cx, f_parse_46_base_notations_1(v_cx)), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Context_46_loading_1(v_cx)));
    v_mcx = t1951;
    {
      struct u_handler *h1952 = u_push();
      if (setjmp(h1952->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mcx, &v_mts);
        }
        u_pop(h1952);
      } else {
        u_pop(h1952);
        int d1953 = 0;
        if ((!d1953) && (u_has_type(h1952, "lex.LexError") && u_is_con(u_caught(h1952), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1952), 0);
          u_value v_i = u_field(u_caught(h1952), 1);
          d1953 = 1;
          {
            static struct u_object l__32_in_32__1954 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1954, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1953) && (u_has_type(h1952, "parse.ParseError") && u_is_con(u_caught(h1952), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1952), 0);
          u_value v_i = u_field(u_caught(h1952), 1);
          d1953 = 1;
          {
            static struct u_object l__32_in_32__1955 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1955, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1953) {
          u_reraise(h1952);
        }
      }
    }
  }
}

u_value f_parse_46_base_notations_1(u_value v_cx) {
  u_value s1956 = f_parse_46_Context_46_base_1(v_cx);
  u_value t1957 = NULL;
  if ((!t1957) && u_is_con(s1956, &n_option_46_Some)) {
    u_value v_b = u_field(s1956, 0);
    t1957 = v_b;
  }
  if ((!t1957) && u_is_con(s1956, &n_option_46_None)) {
    t1957 = f_parse_46_Context_46_notations_1(v_cx);
  }
  if (!t1957) {
    u_no_case(s1956);
  }
  return t1957;
}

void p_parse_46_parse_text(u_value v_text, u_value v_cx, u_value *r_p) {
  {
    u_value v_ts = NULL;
    u_value v_cs = NULL;
    u_value v_after = NULL;
    u_value t1958 = v_cx;
    v_after = t1958;
    {
      struct u_handler *h1959 = u_push();
      if (setjmp(h1959->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1959);
      } else {
        u_pop(h1959);
        int d1960 = 0;
        if ((!d1960) && (u_has_type(h1959, "lex.LexError") && u_is_con(u_caught(h1959), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1959), 0);
          u_value v_i = u_field(u_caught(h1959), 1);
          d1960 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1960) {
          u_reraise(h1959);
        }
      }
    }
    p_parse_46_parse_commands(&v_cs, &v_after, &v_ts);
    u_value t1961 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1961;
  }
}

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p) {
  {
    u_value v_cx = NULL;
    u_value t1962 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    v_cx = t1962;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), &v_cx);
    p_parse_46_parse_text(v_text, f_parse_46_Context_46_dir_58__61__2(f_parse_46_Context_46_base_58__61__2(f_parse_46_Context_46_loading_58__61__2(v_cx, u_mk_con(&n_list_46_Cons, 2, v_name, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_option_46_Some, 1, f_parse_46_Context_46_notations_1(v_cx))), v_dir), r_p);
  }
}

void p_parse_46_parse_library(u_value v_n, u_value *r_p) {
  {
    u_value v_text = NULL;
    u_value v_pre = NULL;
    p_parse_46_library_text(v_n, u_mk_int(0), &v_text);
    static struct u_object l_prelude_1963 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1963))) {
      {
        static struct u_object l_prelude_1964 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1964, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1965 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1965, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1966 = v_xs;
  u_value t1967 = NULL;
  if ((!t1967) && (u_is_con(s1966, &n_list_46_Cons) && u_is_con(u_field(s1966, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1966, 0);
    t1967 = v_x;
  }
  if ((!t1967) && u_is_con(s1966, &n_list_46_Cons)) {
    u_value v_more = u_field(s1966, 1);
    t1967 = f_parse_46_last_part_1(v_more);
  }
  if (!t1967) {
    static struct u_object l__1968 = U_LIT("");
    t1967 = &l__1968;
  }
  if (!t1967) {
    u_no_case(s1966);
  }
  return t1967;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1973 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1972;
  {
    u_value v_f = t1973;
    u_value t1970 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1970)) {
      static struct u_object l__46_u_1969 = U_LIT(".u");
      t1970 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1969);
    }
    u_value t1971;
    if (u_truth(t1970)) {
      t1971 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1971 = v_f;
    }
    t1972 = t1971;
  }
  return t1972;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1978 = u_str_split(v_path, u_mk_char(47));
  u_value t1977;
  {
    u_value v_parts = t1978;
    u_value t1976;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1974 = U_LIT(".");
      t1976 = &l__46__1974;
    } else {
      static struct u_object l__47__1975 = U_LIT("/");
      t1976 = u_str_join(&l__47__1975, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1977 = t1976;
  }
  return t1977;
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
    static struct u_object l_prelude_1979 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1979, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1980 = u_mk_tuple(2, v_cs, v_ds);
    int d1981 = 0;
    if ((!d1981) && (u_is_con(u_field(s1980, 0), &n_list_46_Cons) && u_is_con(u_field(s1980, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1980, 0), 0);
      u_value v_more = u_field(u_field(s1980, 0), 1);
      u_value v_d = u_field(u_field(s1980, 1), 0);
      u_value v_rest = u_field(u_field(s1980, 1), 1);
      d1981 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1981) {
      d1981 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1981) {
      u_no_case(s1980);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1982 = v_paths;
    int d1983 = 0;
    if ((!d1983) && u_is_con(s1982, &n_list_46_Nil)) {
      d1983 = 1;
      {
      }
    }
    if ((!d1983) && u_is_con(s1982, &n_list_46_Cons)) {
      u_value v_path = u_field(s1982, 0);
      u_value v_more = u_field(s1982, 1);
      d1983 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1983) {
      u_no_case(s1982);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1984 = v_paths;
    int d1985 = 0;
    if ((!d1985) && u_is_con(s1984, &n_list_46_Nil)) {
      d1985 = 1;
      {
      }
    }
    if ((!d1985) && u_is_con(s1984, &n_list_46_Cons)) {
      u_value v_path = u_field(s1984, 0);
      u_value v_more = u_field(s1984, 1);
      d1985 = 1;
      {
        u_value t1986 = u_mk_bool(0);
        v_failed = t1986;
        {
          struct u_handler *h1987 = u_push();
          if (setjmp(h1987->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1987);
          } else {
            u_pop(h1987);
            int d1988 = 0;
            if ((!d1988) && (u_has_type(h1987, "parse.ParseError") && u_is_con(u_caught(h1987), &n_parse_46_ParseError))) {
              d1988 = 1;
              {
                u_value t1989 = u_mk_bool(1);
                v_failed = t1989;
              }
            }
            if (!d1988) {
              u_reraise(h1987);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1985) {
      u_no_case(s1984);
    }
  }
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s1990 = v_x;
  u_value t1991 = NULL;
  if ((!t1991) && u_is_con(s1990, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1990, 0);
    t1991 = v_y;
  }
  if (!t1991) {
    u_no_case(s1990);
  }
  return t1991;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s1992 = v_x;
  u_value t1993 = NULL;
  if ((!t1993) && u_is_con(s1992, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1992, 1);
    t1993 = v_y;
  }
  if (!t1993) {
    u_no_case(s1992);
  }
  return t1993;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s1994 = v_x;
  u_value t1995 = NULL;
  if ((!t1995) && u_is_con(s1994, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1994, 2);
    t1995 = v_y;
  }
  if (!t1995) {
    u_no_case(s1994);
  }
  return t1995;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s1996 = v_x;
  u_value t1997 = NULL;
  if ((!t1997) && u_is_con(s1996, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1996, 3);
    t1997 = v_y;
  }
  if (!t1997) {
    u_no_case(s1996);
  }
  return t1997;
}

u_value f_compile_46_Globals_46_procs_1(u_value v_x) {
  u_value s1998 = v_x;
  u_value t1999 = NULL;
  if ((!t1999) && u_is_con(s1998, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1998, 4);
    t1999 = v_y;
  }
  if (!t1999) {
    u_no_case(s1998);
  }
  return t1999;
}

u_value f_compile_46_Globals_46_funs_58__61__2(u_value v_x, u_value v_y) {
  u_value s2000 = v_x;
  u_value t2001 = NULL;
  if ((!t2001) && u_is_con(s2000, &n_compile_46_Globals)) {
    u_value v_a1 = u_field(s2000, 1);
    u_value v_a2 = u_field(s2000, 2);
    u_value v_a3 = u_field(s2000, 3);
    u_value v_a4 = u_field(s2000, 4);
    t2001 = u_mk_con(&n_compile_46_Globals, 5, v_y, v_a1, v_a2, v_a3, v_a4);
  }
  if (!t2001) {
    u_no_case(s2000);
  }
  return t2001;
}

u_value f_compile_46_Globals_46_constrs_58__61__2(u_value v_x, u_value v_y) {
  u_value s2002 = v_x;
  u_value t2003 = NULL;
  if ((!t2003) && u_is_con(s2002, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s2002, 0);
    u_value v_a2 = u_field(s2002, 2);
    u_value v_a3 = u_field(s2002, 3);
    u_value v_a4 = u_field(s2002, 4);
    t2003 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_y, v_a2, v_a3, v_a4);
  }
  if (!t2003) {
    u_no_case(s2002);
  }
  return t2003;
}

u_value f_compile_46_Globals_46_arities_58__61__2(u_value v_x, u_value v_y) {
  u_value s2004 = v_x;
  u_value t2005 = NULL;
  if ((!t2005) && u_is_con(s2004, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s2004, 0);
    u_value v_a1 = u_field(s2004, 1);
    u_value v_a3 = u_field(s2004, 3);
    u_value v_a4 = u_field(s2004, 4);
    t2005 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_y, v_a3, v_a4);
  }
  if (!t2005) {
    u_no_case(s2004);
  }
  return t2005;
}

u_value f_compile_46_Globals_46_fields_58__61__2(u_value v_x, u_value v_y) {
  u_value s2006 = v_x;
  u_value t2007 = NULL;
  if ((!t2007) && u_is_con(s2006, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s2006, 0);
    u_value v_a1 = u_field(s2006, 1);
    u_value v_a2 = u_field(s2006, 2);
    u_value v_a4 = u_field(s2006, 4);
    t2007 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_y, v_a4);
  }
  if (!t2007) {
    u_no_case(s2006);
  }
  return t2007;
}

u_value f_compile_46_Globals_46_procs_58__61__2(u_value v_x, u_value v_y) {
  u_value s2008 = v_x;
  u_value t2009 = NULL;
  if ((!t2009) && u_is_con(s2008, &n_compile_46_Globals)) {
    u_value v_a0 = u_field(s2008, 0);
    u_value v_a1 = u_field(s2008, 1);
    u_value v_a2 = u_field(s2008, 2);
    u_value v_a3 = u_field(s2008, 3);
    t2009 = u_mk_con(&n_compile_46_Globals, 5, v_a0, v_a1, v_a2, v_a3, v_y);
  }
  if (!t2009) {
    u_no_case(s2008);
  }
  return t2009;
}

u_value f_compile_46_Gen_46_count_1(u_value v_x) {
  u_value s2010 = v_x;
  u_value t2011 = NULL;
  if ((!t2011) && u_is_con(s2010, &n_compile_46_Gen)) {
    u_value v_y = u_field(s2010, 0);
    t2011 = v_y;
  }
  if (!t2011) {
    u_no_case(s2010);
  }
  return t2011;
}

u_value f_compile_46_Gen_46_lifted_1(u_value v_x) {
  u_value s2012 = v_x;
  u_value t2013 = NULL;
  if ((!t2013) && u_is_con(s2012, &n_compile_46_Gen)) {
    u_value v_y = u_field(s2012, 1);
    t2013 = v_y;
  }
  if (!t2013) {
    u_no_case(s2012);
  }
  return t2013;
}

u_value f_compile_46_Gen_46_count_58__61__2(u_value v_x, u_value v_y) {
  u_value s2014 = v_x;
  u_value t2015 = NULL;
  if ((!t2015) && u_is_con(s2014, &n_compile_46_Gen)) {
    u_value v_a1 = u_field(s2014, 1);
    t2015 = u_mk_con(&n_compile_46_Gen, 2, v_y, v_a1);
  }
  if (!t2015) {
    u_no_case(s2014);
  }
  return t2015;
}

u_value f_compile_46_Gen_46_lifted_58__61__2(u_value v_x, u_value v_y) {
  u_value s2016 = v_x;
  u_value t2017 = NULL;
  if ((!t2017) && u_is_con(s2016, &n_compile_46_Gen)) {
    u_value v_a0 = u_field(s2016, 0);
    t2017 = u_mk_con(&n_compile_46_Gen, 2, v_a0, v_y);
  }
  if (!t2017) {
    u_no_case(s2016);
  }
  return t2017;
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
  static struct u_object l_u_value_2018 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_2018);
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
  static struct u_object l_u_value_2019 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_2019));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t2020 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2021 = &t2020;
  u_value l2022 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l2022, &n_list_46_Cons)) {
    u_value v_c = u_field(l2022, 0);
    if (u_truth(u_mk_bool(1))) {
      e2021 = u_append(e2021, f_compile_46_mangle_char_1(v_c));
    }
    l2022 = u_field(l2022, 1);
  }
  return u_string(f_list_46_concat_1(t2020));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t2023 = u_is_letter(v_c);
  if (!u_truth(t2023)) {
    t2023 = u_is_digit(v_c);
  }
  u_value t2024 = t2023;
  if (!u_truth(t2024)) {
    t2024 = u_eq(v_c, u_mk_char(95));
  }
  u_value t2027;
  if (u_truth(t2024)) {
    t2027 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___2025 = U_LIT("_");
    static struct u_object l___2026 = U_LIT("_");
    t2027 = u_chars(u_concat(&l___2025, u_concat(u_str_show(u_ord(v_c)), &l___2026)));
  }
  return t2027;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__2028 = U_LIT("f_");
  static struct u_object l___2029 = U_LIT("_");
  return u_concat(&l_f__2028, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___2029, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__2030 = U_LIT("p_");
  return u_concat(&l_p__2030, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__2031 = U_LIT("v_");
  return u_concat(&l_v__2031, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__2032 = U_LIT("r_");
  return u_concat(&l_r__2032, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__2033 = U_LIT("+");
  static struct u_object l_u_add_2034 = U_LIT("u_add");
  static struct u_object l__45__2035 = U_LIT("-");
  static struct u_object l_u_sub_2036 = U_LIT("u_sub");
  static struct u_object l__42__2037 = U_LIT("*");
  static struct u_object l_u_mul_2038 = U_LIT("u_mul");
  static struct u_object l__47__2039 = U_LIT("/");
  static struct u_object l_u_div_2040 = U_LIT("u_div");
  static struct u_object l__37__2041 = U_LIT("%");
  static struct u_object l_u_mod_2042 = U_LIT("u_mod");
  static struct u_object l__60__2043 = U_LIT("<");
  static struct u_object l_u_lt_2044 = U_LIT("u_lt");
  static struct u_object l__61__61__2045 = U_LIT("==");
  static struct u_object l_u_eq_2046 = U_LIT("u_eq");
  static struct u_object l_compare_2047 = U_LIT("compare");
  static struct u_object l_u_compare_2048 = U_LIT("u_compare");
  static struct u_object l_chars_2049 = U_LIT("chars");
  static struct u_object l_u_chars_2050 = U_LIT("u_chars");
  static struct u_object l_string_2051 = U_LIT("string");
  static struct u_object l_u_string_2052 = U_LIT("u_string");
  static struct u_object l_ord_2053 = U_LIT("ord");
  static struct u_object l_u_ord_2054 = U_LIT("u_ord");
  static struct u_object l_chr_2055 = U_LIT("chr");
  static struct u_object l_u_chr_2056 = U_LIT("u_chr");
  static struct u_object l__94__2057 = U_LIT("^");
  static struct u_object l_u_concat_2058 = U_LIT("u_concat");
  static struct u_object l_str_size_2059 = U_LIT("str_size");
  static struct u_object l_u_str_size_2060 = U_LIT("u_str_size");
  static struct u_object l_str_join_2061 = U_LIT("str_join");
  static struct u_object l_u_str_join_2062 = U_LIT("u_str_join");
  static struct u_object l_str_slice_2063 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_2064 = U_LIT("u_str_slice");
  static struct u_object l_str_split_2065 = U_LIT("str_split");
  static struct u_object l_u_str_split_2066 = U_LIT("u_str_split");
  static struct u_object l_str_get_2067 = U_LIT("str_get");
  static struct u_object l_u_str_get_2068 = U_LIT("u_str_get");
  static struct u_object l_str_contains_2069 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_2070 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_2071 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_2072 = U_LIT("u_str_compare");
  static struct u_object l_str_show_2073 = U_LIT("str_show");
  static struct u_object l_u_str_show_2074 = U_LIT("u_str_show");
  static struct u_object l_str_quote_2075 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_2076 = U_LIT("u_str_quote");
  static struct u_object l_str_number_2077 = U_LIT("str_number");
  static struct u_object l_u_str_number_2078 = U_LIT("u_str_number");
  static struct u_object l_is_digit_2079 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_2080 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_2081 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_2082 = U_LIT("u_is_letter");
  static struct u_object l_is_space_2083 = U_LIT("is_space");
  static struct u_object l_u_is_space_2084 = U_LIT("u_is_space");
  u_pop(handler);
  cache = f_map_46_put_all_2(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__2033), u_mk_tuple(2, &l_u_add_2034, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__2035), u_mk_tuple(2, &l_u_sub_2036, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__2037), u_mk_tuple(2, &l_u_mul_2038, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__2039), u_mk_tuple(2, &l_u_div_2040, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__2041), u_mk_tuple(2, &l_u_mod_2042, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__2043), u_mk_tuple(2, &l_u_lt_2044, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__2045), u_mk_tuple(2, &l_u_eq_2046, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_2047), u_mk_tuple(2, &l_u_compare_2048, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_2049), u_mk_tuple(2, &l_u_chars_2050, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_2051), u_mk_tuple(2, &l_u_string_2052, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_2053), u_mk_tuple(2, &l_u_ord_2054, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_2055), u_mk_tuple(2, &l_u_chr_2056, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__2057), u_mk_tuple(2, &l_u_concat_2058, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_2059), u_mk_tuple(2, &l_u_str_size_2060, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_2061), u_mk_tuple(2, &l_u_str_join_2062, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_2063), u_mk_tuple(2, &l_u_str_slice_2064, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_2065), u_mk_tuple(2, &l_u_str_split_2066, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_2067), u_mk_tuple(2, &l_u_str_get_2068, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_2069), u_mk_tuple(2, &l_u_str_contains_2070, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_2071), u_mk_tuple(2, &l_u_str_compare_2072, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_2073), u_mk_tuple(2, &l_u_str_show_2074, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_2075), u_mk_tuple(2, &l_u_str_quote_2076, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_2077), u_mk_tuple(2, &l_u_str_number_2078, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_2079), u_mk_tuple(2, &l_u_is_digit_2080, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_2081), u_mk_tuple(2, &l_u_is_letter_2082, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_2083), u_mk_tuple(2, &l_u_is_space_2084, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))), f_map_46_empty_0());
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__2085 = U_LIT("&");
  static struct u_object l_n__2086 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__2085, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__2086, f_compile_46_mangle_1(v_n))));
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
  u_value s2087 = v_n;
  u_value t2088 = NULL;
  if ((!t2088) && (u_is_con(s2087, &n_syntax_46_Name) && u_is_con(u_field(s2087, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s2087, 0), 0);
    u_value v_b = u_field(s2087, 1);
    static struct u_object l_const_32_struct_32_u_n_2089 = U_LIT("const struct u_name");
    static struct u_object l_n__2090 = U_LIT("n_");
    t2088 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2089), u_concat(&l_n__2090, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t2088) && (u_is_con(s2087, &n_syntax_46_Name) && u_is_con(u_field(s2087, 0), &n_option_46_None))) {
    u_value v_b = u_field(s2087, 1);
    static struct u_object l_const_32_struct_32_u_n_2091 = U_LIT("const struct u_name");
    static struct u_object l_n__2092 = U_LIT("n_");
    static struct u_object l_NULL_2093 = U_LIT("NULL");
    t2088 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_2091), u_concat(&l_n__2092, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2093), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t2088) {
    u_no_case(s2087);
  }
  return t2088;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s2094 = v_cs;
  u_value t2095 = NULL;
  if ((!t2095) && u_is_con(s2094, &n_list_46_Nil)) {
    t2095 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2095) && (u_is_con(s2094, &n_list_46_Cons) && u_is_con(u_field(s2094, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s2094, 0), 2);
    u_value v_rest = u_field(s2094, 1);
    u_value t2096 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2097 = &t2096;
    u_value l2098 = v_ks;
    while (u_is_con(l2098, &n_list_46_Cons)) {
      u_value v_k = u_field(l2098, 0);
      if (u_truth(u_mk_bool(1))) {
        e2097 = u_append(e2097, f_syntax_46_Constr_46_name_1(v_k));
      }
      l2098 = u_field(l2098, 1);
    }
    t2095 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t2096, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t2095) && u_is_con(s2094, &n_list_46_Cons)) {
    u_value v_rest = u_field(s2094, 1);
    t2095 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t2095) {
    u_no_case(s2094);
  }
  return t2095;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s2099 = v_ks;
  u_value t2100 = NULL;
  if ((!t2100) && u_is_con(s2099, &n_list_46_Nil)) {
    t2100 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2100) && u_is_con(s2099, &n_list_46_Cons)) {
    u_value v_k = u_field(s2099, 0);
    u_value v_rest = u_field(s2099, 1);
    u_value t2101;
    if (u_truth(f_set_46_member_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t2101 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t2101 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t2100 = t2101;
  }
  if (!t2100) {
    u_no_case(s2099);
  }
  return t2100;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t2102 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2103 = &t2102;
  u_value l2104 = v_xs;
  while (u_is_con(l2104, &n_list_46_Cons)) {
    u_value v_x = u_field(l2104, 0);
    if (u_truth(u_mk_bool(1))) {
      e2103 = u_append(e2103, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l2104 = u_field(l2104, 1);
  }
  return t2102;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__2105 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__2105, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s2106 = f_list_46_lookup_2(v_x, v_env);
  u_value t2107 = NULL;
  if ((!t2107) && u_is_con(s2106, &n_option_46_Some)) {
    u_value v_c = u_field(s2106, 0);
    t2107 = v_c;
  }
  if (!t2107) {
    t2107 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t2107) {
    u_no_case(s2106);
  }
  return t2107;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s2108 = v_c;
  u_value t2109 = NULL;
  if ((!t2109) && (u_is_con(s2108, &n_c_46_CeUnary) && u_equals(u_field(s2108, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s2108, 1);
    t2109 = v_p;
  }
  if (!t2109) {
    static struct u_object l__38__2110 = U_LIT("&");
    t2109 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2110, v_c);
  }
  if (!t2109) {
    u_no_case(s2108);
  }
  return t2109;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s2111 = v_t;
  u_value t2112 = NULL;
  if ((!t2112) && (u_is_con(s2111, &n_syntax_46_TSort) && u_is_con(u_field(s2111, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s2111, 0);
    t2112 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t2112) && u_is_con(s2111, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s2111, 0);
    u_value v_args = u_field(s2111, 1);
    static struct u_object l__91__2113 = U_LIT("[");
    static struct u_object l__44__32__2114 = U_LIT(", ");
    u_value t2115 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2116 = &t2115;
    u_value l2117 = v_args;
    while (u_is_con(l2117, &n_list_46_Cons)) {
      u_value v_a = u_field(l2117, 0);
      if (u_truth(u_mk_bool(1))) {
        e2116 = u_append(e2116, f_compile_46_type_key_1(v_a));
      }
      l2117 = u_field(l2117, 1);
    }
    static struct u_object l__93__2118 = U_LIT("]");
    t2112 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__2113, u_concat(u_str_join(&l__44__32__2114, t2115), &l__93__2118)));
  }
  if ((!t2112) && u_is_con(s2111, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s2111, 0);
    u_value v_r = u_field(s2111, 1);
    static struct u_object l__40__2119 = U_LIT("(");
    static struct u_object l__44__32__2120 = U_LIT(", ");
    u_value t2121 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2122 = &t2121;
    u_value l2123 = v_args;
    while (u_is_con(l2123, &n_list_46_Cons)) {
      u_value v_a = u_field(l2123, 0);
      if (u_truth(u_mk_bool(1))) {
        e2122 = u_append(e2122, f_compile_46_type_key_1(v_a));
      }
      l2123 = u_field(l2123, 1);
    }
    static struct u_object l__41__32__45__62__32__2124 = U_LIT(") -> ");
    t2112 = u_concat(&l__40__2119, u_concat(u_str_join(&l__44__32__2120, t2121), u_concat(&l__41__32__45__62__32__2124, f_compile_46_type_key_1(v_r))));
  }
  if ((!t2112) && u_is_con(s2111, &n_syntax_46_TProc)) {
    u_value v_is = u_field(s2111, 0);
    u_value v_os = u_field(s2111, 1);
    u_value v_rs = u_field(s2111, 2);
    static struct u_object l__40_in_32__2125 = U_LIT("(in ");
    static struct u_object l__44__32__2126 = U_LIT(", ");
    u_value t2127 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2128 = &t2127;
    u_value l2129 = v_is;
    while (u_is_con(l2129, &n_list_46_Cons)) {
      u_value v_a = u_field(l2129, 0);
      if (u_truth(u_mk_bool(1))) {
        e2128 = u_append(e2128, f_compile_46_type_key_1(v_a));
      }
      l2129 = u_field(l2129, 1);
    }
    static struct u_object l__44__32_out_32__2130 = U_LIT(", out ");
    static struct u_object l__44__32__2131 = U_LIT(", ");
    u_value t2132 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2133 = &t2132;
    u_value l2134 = v_os;
    while (u_is_con(l2134, &n_list_46_Cons)) {
      u_value v_a = u_field(l2134, 0);
      if (u_truth(u_mk_bool(1))) {
        e2133 = u_append(e2133, f_compile_46_type_key_1(v_a));
      }
      l2134 = u_field(l2134, 1);
    }
    static struct u_object l__44__32_ref_32__2135 = U_LIT(", ref ");
    static struct u_object l__44__32__2136 = U_LIT(", ");
    u_value t2137 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2138 = &t2137;
    u_value l2139 = v_rs;
    while (u_is_con(l2139, &n_list_46_Cons)) {
      u_value v_a = u_field(l2139, 0);
      if (u_truth(u_mk_bool(1))) {
        e2138 = u_append(e2138, f_compile_46_type_key_1(v_a));
      }
      l2139 = u_field(l2139, 1);
    }
    static struct u_object l__41__2140 = U_LIT(")");
    t2112 = u_concat(&l__40_in_32__2125, u_concat(u_str_join(&l__44__32__2126, t2127), u_concat(&l__44__32_out_32__2130, u_concat(u_str_join(&l__44__32__2131, t2132), u_concat(&l__44__32_ref_32__2135, u_concat(u_str_join(&l__44__32__2136, t2137), &l__41__2140))))));
  }
  if ((!t2112) && u_is_con(s2111, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s2111, 0);
    static struct u_object l__40__2141 = U_LIT("(");
    static struct u_object l__32__42__32__2142 = U_LIT(" * ");
    u_value t2143 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2144 = &t2143;
    u_value l2145 = v_ts;
    while (u_is_con(l2145, &n_list_46_Cons)) {
      u_value v_a = u_field(l2145, 0);
      if (u_truth(u_mk_bool(1))) {
        e2144 = u_append(e2144, f_compile_46_type_key_1(v_a));
      }
      l2145 = u_field(l2145, 1);
    }
    static struct u_object l__41__2146 = U_LIT(")");
    t2112 = u_concat(&l__40__2141, u_concat(u_str_join(&l__32__42__32__2142, t2143), &l__41__2146));
  }
  if (!t2112) {
    static struct u_object l__63__2147 = U_LIT("?");
    t2112 = &l__63__2147;
  }
  if (!t2112) {
    u_no_case(s2111);
  }
  return t2112;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t2148 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2149 = &t2148;
  u_value l2150 = v_cs;
  while (u_is_con(l2150, &n_list_46_Cons)) {
    u_value v_c = u_field(l2150, 0);
    if (u_truth(u_mk_bool(1))) {
      e2149 = u_append(e2149, f_compile_46_fun_arities_1(v_c));
    }
    l2150 = u_field(l2150, 1);
  }
  u_value t2151 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2152 = &t2151;
  u_value l2153 = v_cs;
  while (u_is_con(l2153, &n_list_46_Cons)) {
    u_value v_c = u_field(l2153, 0);
    if (u_truth(u_mk_bool(1))) {
      e2152 = u_append(e2152, f_compile_46_constr_arities_1(v_c));
    }
    l2153 = u_field(l2153, 1);
  }
  u_value t2158 = f_list_46_concat_1(t2148);
  u_value t2159 = f_list_46_concat_1(t2151);
  u_value t2157;
  {
    u_value v_fs = t2158;
    u_value v_ks = t2159;
    u_value t2154 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2155 = &t2154;
    u_value l2156 = v_cs;
    while (u_is_con(l2156, &n_list_46_Cons)) {
      u_value v_c = u_field(l2156, 0);
      if (u_truth(u_mk_bool(1))) {
        e2155 = u_append(e2155, f_compile_46_proc_arities_1(v_c));
      }
      l2156 = u_field(l2156, 1);
    }
    t2157 = u_mk_con(&n_compile_46_Globals, 5, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()), f_map_46_put_all_2(f_list_46_concat_1(t2154), f_map_46_empty_0()));
  }
  return t2157;
}

u_value f_compile_46_proc_arities_1(u_value v_c) {
  u_value s2160 = v_c;
  u_value t2161 = NULL;
  if ((!t2161) && u_is_con(s2160, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s2160, 0);
    u_value v_ins = u_field(s2160, 2);
    u_value v_outs = u_field(s2160, 3);
    u_value v_refs = u_field(s2160, 4);
    t2161 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, f_list_46_length_1(v_ins), u_add(f_list_46_length_1(v_outs), f_list_46_length_1(v_refs)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2161) {
    t2161 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2161) {
    u_no_case(s2160);
  }
  return t2161;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s2162 = v_fs;
  u_value t2163 = NULL;
  if ((!t2163) && u_is_con(s2162, &n_list_46_Nil)) {
    t2163 = v_m;
  }
  if ((!t2163) && u_is_con(s2162, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s2162, 0), 0);
    u_value v_k = u_field(u_field(s2162, 0), 1);
    u_value v_rest = u_field(s2162, 1);
    u_value s2164 = f_map_46_get_2(v_f, v_m);
    u_value t2165 = NULL;
    if ((!t2165) && u_is_con(s2164, &n_option_46_Some)) {
      u_value v_ks = u_field(s2164, 0);
      t2165 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t2165) && u_is_con(s2164, &n_option_46_None)) {
      t2165 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
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

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s2166 = v_gl;
  u_value t2167 = NULL;
  if ((!t2167) && u_is_con(s2166, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s2166, 2);
    u_value s2168 = f_map_46_get_2(v_n, v_fm);
    u_value t2169 = NULL;
    if ((!t2169) && u_is_con(s2168, &n_option_46_Some)) {
      u_value v_ks = u_field(s2168, 0);
      t2169 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t2169) && u_is_con(s2168, &n_option_46_None)) {
      t2169 = u_mk_bool(0);
    }
    if (!t2169) {
      u_no_case(s2168);
    }
    t2167 = t2169;
  }
  if (!t2167) {
    u_no_case(s2166);
  }
  return t2167;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s2170 = v_gl;
  u_value t2171 = NULL;
  if ((!t2171) && u_is_con(s2170, &n_compile_46_Globals)) {
    u_value v_km = u_field(s2170, 3);
    t2171 = f_map_46_get_2(v_n, v_km);
  }
  if (!t2171) {
    u_no_case(s2170);
  }
  return t2171;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s2172 = v_c;
  u_value t2173 = NULL;
  if ((!t2173) && u_is_con(s2172, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s2172, 0);
    u_value v_xss = u_field(s2172, 2);
    t2173 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2173) {
    t2173 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2173) {
    u_no_case(s2172);
  }
  return t2173;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s2174 = v_c;
  u_value t2175 = NULL;
  if ((!t2175) && u_is_con(s2174, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s2174, 2);
    u_value t2176 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2177 = &t2176;
    u_value l2178 = v_ks;
    while (u_is_con(l2178, &n_list_46_Cons)) {
      u_value v_k = u_field(l2178, 0);
      if (u_truth(u_mk_bool(1))) {
        e2177 = u_append(e2177, f_compile_46_constr_arity_1(v_k));
      }
      l2178 = u_field(l2178, 1);
    }
    t2175 = t2176;
  }
  if (!t2175) {
    t2175 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2175) {
    u_no_case(s2174);
  }
  return t2175;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s2179 = v_k;
  u_value t2180 = NULL;
  if ((!t2180) && u_is_con(s2179, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s2179, 0);
    u_value v_ts = u_field(s2179, 1);
    t2180 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t2180) {
    u_no_case(s2179);
  }
  return t2180;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s2181 = v_xs;
  u_value t2182 = NULL;
  if ((!t2182) && u_is_con(s2181, &n_list_46_Nil)) {
    t2182 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2182) && u_is_con(s2181, &n_list_46_Cons)) {
    u_value v_x = u_field(s2181, 0);
    u_value v_rest = u_field(s2181, 1);
    t2182 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t2182) {
    u_no_case(s2181);
  }
  return t2182;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_2183 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_2183, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_2184 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_2184, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s2185 = v_cs;
  u_value t2186 = NULL;
  if ((!t2186) && u_is_con(s2185, &n_list_46_Nil)) {
    t2186 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t2186) && (u_is_con(s2185, &n_list_46_Cons) && u_is_con(u_field(s2185, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s2185, 0);
    t2186 = v_d;
  }
  if ((!t2186) && u_is_con(s2185, &n_list_46_Cons)) {
    u_value v_d = u_field(s2185, 0);
    u_value v_rest = u_field(s2185, 1);
    static struct u_object l__38__38__2187 = U_LIT("&&");
    t2186 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__2187, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t2186) {
    u_no_case(s2185);
  }
  return t2186;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s2188 = v_l;
  u_value t2189 = NULL;
  if ((!t2189) && u_is_con(s2188, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_2190 = U_LIT("u_mk_tuple");
    t2189 = f_c_46_c_call_2(&l_u_mk_tuple_2190, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2189) && u_is_con(s2188, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s2188, 0);
    static struct u_object l_u_mk_bool_2191 = U_LIT("u_mk_bool");
    u_value t2192;
    if (u_truth(v_b)) {
      t2192 = u_mk_int(1);
    } else {
      t2192 = u_mk_int(0);
    }
    t2189 = f_c_46_c_call_2(&l_u_mk_bool_2191, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t2192), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2189) && u_is_con(s2188, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s2188, 0);
    u_value t2193 = u_lt(u_mk_int_str("-1000000000000000000"), v_n);
    if (u_truth(t2193)) {
      t2193 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t2196;
    if (u_truth(t2193)) {
      static struct u_object l_u_mk_int_2194 = U_LIT("u_mk_int");
      t2196 = f_c_46_c_call_2(&l_u_mk_int_2194, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_2195 = U_LIT("u_mk_int_str");
      t2196 = f_c_46_c_call_2(&l_u_mk_int_str_2195, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t2189 = t2196;
  }
  if ((!t2189) && u_is_con(s2188, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s2188, 0);
    static struct u_object l_u_mk_lit_2197 = U_LIT("u_mk_lit");
    t2189 = f_c_46_c_call_2(&l_u_mk_lit_2197, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t2189) && u_is_con(s2188, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s2188, 0);
    static struct u_object l_u_mk_char_2198 = U_LIT("u_mk_char");
    t2189 = f_c_46_c_call_2(&l_u_mk_char_2198, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2189) {
    u_no_case(s2188);
  }
  return t2189;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s2199 = v_p;
  u_value t2200 = NULL;
  if ((!t2200) && u_is_con(s2199, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s2199, 0);
    static struct u_object l_u_equals_2201 = U_LIT("u_equals");
    t2200 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_2201, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2200) && u_is_con(s2199, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s2199, 0);
    u_value v_ps = u_field(s2199, 1);
    static struct u_object l_u_is_con_2202 = U_LIT("u_is_con");
    t2200 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_2202, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t2200) && u_is_con(s2199, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2199, 0);
    t2200 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2200) {
    t2200 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2200) {
    u_no_case(s2199);
  }
  return t2200;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s2203 = v_ps;
  u_value t2204 = NULL;
  if ((!t2204) && u_is_con(s2203, &n_list_46_Nil)) {
    t2204 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2204) && u_is_con(s2203, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2203, 0), 0);
    u_value v_i = u_field(u_field(s2203, 0), 1);
    u_value v_rest = u_field(s2203, 1);
    t2204 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t2204) {
    u_no_case(s2203);
  }
  return t2204;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s2205 = v_p;
  u_value t2206 = NULL;
  if ((!t2206) && (u_is_con(s2205, &n_syntax_46_PVar) && u_is_con(u_field(s2205, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2205, 0), 0);
    t2206 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2206) && u_is_con(s2205, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2205, 1);
    t2206 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t2206) && u_is_con(s2205, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2205, 0);
    t2206 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t2206) {
    t2206 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2206) {
    u_no_case(s2205);
  }
  return t2206;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s2207 = v_ps;
  u_value t2208 = NULL;
  if ((!t2208) && u_is_con(s2207, &n_list_46_Nil)) {
    t2208 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2208) && u_is_con(s2207, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s2207, 0), 0);
    u_value v_i = u_field(u_field(s2207, 0), 1);
    u_value v_rest = u_field(s2207, 1);
    t2208 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t2208) {
    u_no_case(s2207);
  }
  return t2208;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s2209 = v_p;
  u_value t2210 = NULL;
  if ((!t2210) && (u_is_con(s2209, &n_syntax_46_PVar) && u_is_con(u_field(s2209, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2209, 0), 0);
    t2210 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t2210) && u_is_con(s2209, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s2209, 1);
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
    t2210 = f_list_46_concat_1(t2211);
  }
  if ((!t2210) && u_is_con(s2209, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s2209, 0);
    u_value t2214 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2215 = &t2214;
    u_value l2216 = v_ps;
    while (u_is_con(l2216, &n_list_46_Cons)) {
      u_value v_q = u_field(l2216, 0);
      if (u_truth(u_mk_bool(1))) {
        e2215 = u_append(e2215, f_compile_46_pattern_names_1(v_q));
      }
      l2216 = u_field(l2216, 1);
    }
    t2210 = f_list_46_concat_1(t2214);
  }
  if (!t2210) {
    t2210 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2210) {
    u_no_case(s2209);
  }
  return t2210;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s2217 = v_e;
  u_value t2218 = NULL;
  if ((!t2218) && u_is_con(s2217, &n_syntax_46_ELit)) {
    t2218 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2218) && (u_is_con(s2217, &n_syntax_46_EVar) && u_is_con(u_field(s2217, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s2217, 0), 0);
    u_value t2219;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t2219 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t2219 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t2218 = t2219;
  }
  if ((!t2218) && u_is_con(s2217, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s2217, 0);
    u_value v_args = u_field(s2217, 1);
    u_value t2220 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2221 = &t2220;
    u_value l2222 = v_args;
    while (u_is_con(l2222, &n_list_46_Cons)) {
      u_value v_a = u_field(l2222, 0);
      if (u_truth(u_mk_bool(1))) {
        e2221 = u_append(e2221, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2222 = u_field(l2222, 1);
    }
    t2218 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t2220));
  }
  if ((!t2218) && u_is_con(s2217, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s2217, 0);
    u_value v_b = u_field(s2217, 1);
    t2218 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t2218) && u_is_con(s2217, &n_syntax_46_EProc)) {
    u_value v_is = u_field(s2217, 0);
    u_value v_os = u_field(s2217, 1);
    u_value v_rs = u_field(s2217, 2);
    u_value v_b = u_field(s2217, 3);
    t2218 = f_compile_46_free_in_stmt_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), v_bound));
  }
  if ((!t2218) && u_is_con(s2217, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s2217, 0);
    u_value v_b = u_field(s2217, 1);
    u_value t2223 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2224 = &t2223;
    u_value l2225 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l2225, &n_list_46_Cons)) {
      u_value v_a = u_field(l2225, 0);
      if (u_truth(u_mk_bool(1))) {
        e2224 = u_append(e2224, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2225 = u_field(l2225, 1);
    }
    t2218 = f_list_46__43__43__2(f_list_46_concat_1(t2223), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t2218) && u_is_con(s2217, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s2217, 0);
    u_value v_l = u_field(s2217, 1);
    u_value v_r = u_field(s2217, 2);
    t2218 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t2218) && u_is_con(s2217, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s2217, 0);
    u_value v_cs = u_field(s2217, 1);
    u_value t2226 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2227 = &t2226;
    u_value l2228 = v_cs;
    while (u_is_con(l2228, &n_list_46_Cons)) {
      u_value v_b = u_field(l2228, 0);
      if (u_truth(u_mk_bool(1))) {
        e2227 = u_append(e2227, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l2228 = u_field(l2228, 1);
    }
    t2218 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2226));
  }
  if ((!t2218) && u_is_con(s2217, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s2217, 0);
    u_value v_xs = u_field(s2217, 1);
    u_value v_ls = u_field(s2217, 2);
    u_value v_c = u_field(s2217, 3);
    u_value t2229 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2230 = &t2229;
    u_value l2231 = v_ls;
    while (u_is_con(l2231, &n_list_46_Cons)) {
      u_value v_l = u_field(l2231, 0);
      if (u_truth(u_mk_bool(1))) {
        e2230 = u_append(e2230, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2231 = u_field(l2231, 1);
    }
    t2218 = f_list_46__43__43__2(f_list_46_concat_1(t2229), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t2218) && u_is_con(s2217, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s2217, 0);
    u_value t2232 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2233 = &t2232;
    u_value l2234 = v_es;
    while (u_is_con(l2234, &n_list_46_Cons)) {
      u_value v_a = u_field(l2234, 0);
      if (u_truth(u_mk_bool(1))) {
        e2233 = u_append(e2233, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2234 = u_field(l2234, 1);
    }
    t2218 = f_list_46_concat_1(t2232);
  }
  if ((!t2218) && u_is_con(s2217, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s2217, 0);
    t2218 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if (!t2218) {
    u_no_case(s2217);
  }
  return t2218;
}

u_value f_compile_46_free_in_stmt_2(u_value v_x, u_value v_bound) {
  u_value s2235 = v_x;
  u_value t2236 = NULL;
  if ((!t2236) && u_is_con(s2235, &n_syntax_46_SSkip)) {
    t2236 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2236) && u_is_con(s2235, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s2235, 0);
    u_value t2237 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2238 = &t2237;
    u_value l2239 = f_syntax_46_names_1(f_list_46_firsts_1(v_ps));
    while (u_is_con(l2239, &n_list_46_Cons)) {
      u_value v_y = u_field(l2239, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2238 = u_append(e2238, v_y);
      }
      l2239 = u_field(l2239, 1);
    }
    u_value t2240 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2241 = &t2240;
    u_value l2242 = f_list_46_seconds_1(v_ps);
    while (u_is_con(l2242, &n_list_46_Cons)) {
      u_value v_a = u_field(l2242, 0);
      if (u_truth(u_mk_bool(1))) {
        e2241 = u_append(e2241, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2242 = u_field(l2242, 1);
    }
    t2236 = f_list_46__43__43__2(t2237, f_list_46_concat_1(t2240));
  }
  if ((!t2236) && u_is_con(s2235, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s2235, 0);
    u_value v_ys = u_field(s2235, 1);
    u_value t2243 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2244 = &t2243;
    u_value l2245 = v_ys;
    while (u_is_con(l2245, &n_list_46_Cons)) {
      u_value v_y = u_field(l2245, 0);
      if (u_truth(u_mk_bool(1))) {
        e2244 = u_append(e2244, f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l2245 = u_field(l2245, 1);
    }
    t2236 = f_list_46_concat_1(t2243);
  }
  if ((!t2236) && u_is_con(s2235, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s2235, 0);
    u_value v_l = u_field(s2235, 1);
    u_value v_r = u_field(s2235, 2);
    t2236 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_l, v_bound), f_compile_46_free_in_stmt_2(v_r, v_bound)));
  }
  if ((!t2236) && u_is_con(s2235, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s2235, 0);
    u_value v_b = u_field(s2235, 1);
    t2236 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_compile_46_free_in_stmt_2(v_b, v_bound));
  }
  if ((!t2236) && u_is_con(s2235, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s2235, 0);
    u_value v_ins = u_field(s2235, 1);
    u_value v_outs = u_field(s2235, 2);
    u_value v_refs = u_field(s2235, 3);
    u_value t2246 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2247 = &t2246;
    u_value l2248 = u_mk_con(&n_list_46_Cons, 2, v_p, f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
    while (u_is_con(l2248, &n_list_46_Cons)) {
      u_value v_y = u_field(l2248, 0);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_y, v_bound)))) {
        e2247 = u_append(e2247, v_y);
      }
      l2248 = u_field(l2248, 1);
    }
    u_value t2249 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2250 = &t2249;
    u_value l2251 = v_ins;
    while (u_is_con(l2251, &n_list_46_Cons)) {
      u_value v_a = u_field(l2251, 0);
      if (u_truth(u_mk_bool(1))) {
        e2250 = u_append(e2250, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l2251 = u_field(l2251, 1);
    }
    t2236 = f_list_46__43__43__2(t2246, f_list_46_concat_1(t2249));
  }
  if ((!t2236) && u_is_con(s2235, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s2235, 0);
    u_value v_bs = u_field(s2235, 1);
    u_value t2252 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2253 = &t2252;
    u_value l2254 = v_bs;
    while (u_is_con(l2254, &n_list_46_Cons)) {
      u_value v_b = u_field(l2254, 0);
      if (u_truth(u_mk_bool(1))) {
        e2253 = u_append(e2253, f_compile_46_free_in_stmt_branch_2(v_b, v_bound));
      }
      l2254 = u_field(l2254, 1);
    }
    t2236 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t2252));
  }
  if ((!t2236) && u_is_con(s2235, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s2235, 0);
    t2236 = f_compile_46_free_vars_2(v_c, v_bound);
  }
  if ((!t2236) && u_is_con(s2235, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s2235, 0);
    t2236 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if ((!t2236) && u_is_con(s2235, &n_syntax_46_STry)) {
    u_value v_b = u_field(s2235, 0);
    u_value v_hs = u_field(s2235, 1);
    u_value t2255 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2256 = &t2255;
    u_value l2257 = v_hs;
    while (u_is_con(l2257, &n_list_46_Cons)) {
      u_value v_h = u_field(l2257, 0);
      if (u_truth(u_mk_bool(1))) {
        e2256 = u_append(e2256, f_compile_46_free_in_handler_2(v_h, v_bound));
      }
      l2257 = u_field(l2257, 1);
    }
    t2236 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(v_b, v_bound), f_list_46_concat_1(t2255));
  }
  if (!t2236) {
    u_no_case(s2235);
  }
  return t2236;
}

u_value f_compile_46_free_in_stmt_branch_2(u_value v_b, u_value v_bound) {
  u_value s2258 = v_b;
  u_value t2259 = NULL;
  if ((!t2259) && (u_is_con(s2258, &n_syntax_46_Branch) && u_is_con(u_field(s2258, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2258, 0);
    u_value v_g = u_field(u_field(s2258, 1), 0);
    u_value v_y = u_field(s2258, 2);
    t2259 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2259) && (u_is_con(s2258, &n_syntax_46_Branch) && u_is_con(u_field(s2258, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2258, 0);
    u_value v_y = u_field(s2258, 2);
    t2259 = f_compile_46_free_in_stmt_2(v_y, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2259) {
    u_no_case(s2258);
  }
  return t2259;
}

u_value f_compile_46_free_in_handler_2(u_value v_h, u_value v_bound) {
  u_value t2263 = f_list_46__43__43__2(f_compile_46_pattern_names_1(f_syntax_46_Handler_46_pat_1(v_h)), v_bound);
  u_value t2262;
  {
    u_value v_inner = t2263;
    u_value s2260 = f_syntax_46_Handler_46_guard_1(v_h);
    u_value t2261 = NULL;
    if ((!t2261) && u_is_con(s2260, &n_option_46_Some)) {
      u_value v_g = u_field(s2260, 0);
      t2261 = f_compile_46_free_vars_2(v_g, v_inner);
    }
    if ((!t2261) && u_is_con(s2260, &n_option_46_None)) {
      t2261 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t2261) {
      u_no_case(s2260);
    }
    t2262 = f_list_46__43__43__2(f_compile_46_free_in_stmt_2(f_syntax_46_Handler_46_body_1(v_h), v_inner), t2261);
  }
  return t2262;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s2264 = v_b;
  u_value t2265 = NULL;
  if ((!t2265) && (u_is_con(s2264, &n_syntax_46_Branch) && u_is_con(u_field(s2264, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s2264, 0);
    u_value v_g = u_field(u_field(s2264, 1), 0);
    u_value v_e = u_field(s2264, 2);
    t2265 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t2265) && (u_is_con(s2264, &n_syntax_46_Branch) && u_is_con(u_field(s2264, 1), &n_option_46_None))) {
    u_value v_p = u_field(s2264, 0);
    u_value v_e = u_field(s2264, 2);
    t2265 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t2265) {
    u_no_case(s2264);
  }
  return t2265;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s2266 = v_xss;
  u_value t2267 = NULL;
  if ((!t2267) && u_is_con(s2266, &n_list_46_Nil)) {
    t2267 = v_b;
  }
  if ((!t2267) && u_is_con(s2266, &n_list_46_Cons)) {
    u_value v_xs = u_field(s2266, 0);
    u_value v_rest = u_field(s2266, 1);
    t2267 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t2267) {
    u_no_case(s2266);
  }
  return t2267;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value t2268 = u_concat(v_stem, u_str_show(f_compile_46_Gen_46_count_1(*r_g)));
    u_value t2269 = f_compile_46_Gen_46_count_58__61__2(*r_g, u_add(f_compile_46_Gen_46_count_1(*r_g), u_mk_int(1)));
    *r_x = t2268;
    *r_g = t2269;
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value t2270 = f_compile_46_Gen_46_lifted_58__61__2(*r_g, f_list_46__43__43__2(f_compile_46_Gen_46_lifted_1(*r_g), u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
    *r_g = t2270;
  }
}

u_value f_compile_46_global_value_3(u_value v_gl, u_value v_n, u_value v_t) {
  u_value t2285;
  if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
    static struct u_object l_u_mk_closure_2271 = U_LIT("u_mk_closure");
    static struct u_object l_w__2272 = U_LIT("w_");
    t2285 = f_c_46_c_call_2(&l_u_mk_closure_2271, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__2272, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
  } else {
    u_value s2273 = f_compile_46_fields_count_2(v_gl, v_n);
    u_value t2274 = NULL;
    if ((!t2274) && (u_is_con(s2273, &n_option_46_Some) && u_equals(u_field(s2273, 0), u_mk_int(0)))) {
      static struct u_object l_u_mk_con_2275 = U_LIT("u_mk_con");
      t2274 = f_c_46_c_call_2(&l_u_mk_con_2275, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
    }
    if ((!t2274) && u_is_con(s2273, &n_option_46_Some)) {
      static struct u_object l_u_mk_closure_2276 = U_LIT("u_mk_closure");
      static struct u_object l_wc__2277 = U_LIT("wc_");
      t2274 = f_c_46_c_call_2(&l_u_mk_closure_2276, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__2277, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
    }
    if ((!t2274) && u_is_con(s2273, &n_option_46_None)) {
      u_value s2278 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
      u_value t2279 = NULL;
      if ((!t2279) && u_is_con(s2278, &n_option_46_Some)) {
        u_value v_f = u_field(u_field(s2278, 0), 0);
        static struct u_object l_u_mk_closure_2280 = U_LIT("u_mk_closure");
        static struct u_object l_w_2281 = U_LIT("w");
        t2279 = f_c_46_c_call_2(&l_u_mk_closure_2280, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_2281, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
      }
      if ((!t2279) && u_is_con(s2278, &n_option_46_None)) {
        if (u_truth(f_map_46_has_2(v_n, f_compile_46_Globals_46_procs_1(v_gl)))) {
          static struct u_object l_u_mk_proc_2282 = U_LIT("u_mk_proc");
          static struct u_object l_wp__2283 = U_LIT("wp_");
          t2279 = f_c_46_c_call_2(&l_u_mk_proc_2282, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wp__2283, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        }
      }
      if ((!t2279) && u_is_con(s2278, &n_option_46_None)) {
        static struct u_object l_the_32_variable_32__2284 = U_LIT("the variable ");
        u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__2284, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
        t2279 = NULL;
      }
      if (!t2279) {
        u_no_case(s2278);
      }
      t2274 = t2279;
    }
    if (!t2274) {
      u_no_case(s2273);
    }
    t2285 = t2274;
  }
  return t2285;
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
    u_value s2286 = v_e;
    int d2287 = 0;
    if ((!d2287) && (u_is_con(s2286, &n_syntax_46_ELit) && u_is_con(u_field(s2286, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s2286, 0), 0);
      d2287 = 1;
      {
        static struct u_object l_l__2288 = U_LIT("l_");
        u_value t2289;
        if (u_truth(u_lt(u_str_size(v_s), u_mk_int(16)))) {
          t2289 = u_str_size(v_s);
        } else {
          t2289 = u_mk_int(16);
        }
        static struct u_object l___2290 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__2288, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), t2289))), &l___2290)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__2291 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_2292 = U_LIT("U_LIT");
        static struct u_object l__38__2293 = U_LIT("&");
        u_value t2294 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__2291), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_2292, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t2295 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__2293, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t2294;
        *r_r = t2295;
      }
    }
    if ((!d2287) && u_is_con(s2286, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s2286, 0);
      d2287 = 1;
      {
        u_value t2296 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2297 = f_compile_46_literal_value_1(v_l);
        *r_ss = t2296;
        *r_r = t2297;
      }
    }
    if ((!d2287) && (u_is_con(s2286, &n_syntax_46_EVar) && u_is_con(u_field(s2286, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s2286, 0), 0);
      u_value v_ty = u_field(u_field(s2286, 0), 1);
      d2287 = 1;
      {
        u_value s2298 = f_list_46_lookup_2(v_n, v_env);
        int d2299 = 0;
        if ((!d2299) && u_is_con(s2298, &n_option_46_Some)) {
          u_value v_c = u_field(s2298, 0);
          d2299 = 1;
          {
            u_value t2300 = u_mk_con(&n_list_46_Nil, 0);
            u_value t2301 = v_c;
            *r_ss = t2300;
            *r_r = t2301;
          }
        }
        if ((!d2299) && u_is_con(s2298, &n_option_46_None)) {
          d2299 = 1;
          {
            u_value t2302 = u_mk_con(&n_list_46_Nil, 0);
            u_value t2303 = f_compile_46_global_value_3(v_gl, v_n, v_ty);
            *r_ss = t2302;
            *r_r = t2303;
          }
        }
        if (!d2299) {
          u_no_case(s2298);
        }
      }
    }
    if ((!d2287) && (u_is_con(s2286, &n_syntax_46_EApp) && (u_is_con(u_field(s2286, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s2286, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s2286, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s2286, 0), 0), 1);
      u_value v_args = u_field(s2286, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d2287 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d2287) && u_is_con(s2286, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s2286, 0);
      u_value v_args = u_field(s2286, 1);
      d2287 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_2304 = U_LIT("u_apply");
        u_value t2305 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2306 = f_c_46_c_call_2(&l_u_apply_2304, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t2305;
        *r_r = t2306;
      }
    }
    if ((!d2287) && u_is_con(s2286, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s2286, 0);
      u_value v_b = u_field(s2286, 1);
      d2287 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2287) && u_is_con(s2286, &n_syntax_46_EProc)) {
      u_value v_is = u_field(s2286, 0);
      u_value v_os = u_field(s2286, 1);
      u_value v_rs = u_field(s2286, 2);
      u_value v_b = u_field(s2286, 3);
      d2287 = 1;
      {
        p_compile_46_compile_proc_lambda(v_gl, v_env, v_is, f_list_46__43__43__2(v_os, v_rs), v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d2287) && u_is_con(s2286, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s2286, 0);
      u_value v_b = u_field(s2286, 1);
      d2287 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2307 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2307, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2308 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t2308;
        u_value t2309 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2309;
      }
    }
    if ((!d2287) && u_is_con(s2286, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s2286, 0);
      u_value v_l = u_field(s2286, 1);
      u_value v_rr = u_field(s2286, 2);
      d2287 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_2310 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2310, &v_t, r_g);
        u_value t2311 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2311;
        u_value t2312 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2312;
      }
    }
    if ((!d2287) && u_is_con(s2286, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s2286, 0);
      u_value v_cases = u_field(s2286, 1);
      d2287 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2313 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2313, &v_d, r_g);
        static struct u_object l_t_2314 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2314, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_2315 = U_LIT("NULL");
        u_value t2316 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2315))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2316;
        static struct u_object l__33__2317 = U_LIT("!");
        static struct u_object l_u_no_case_2318 = U_LIT("u_no_case");
        u_value t2319 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2317, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2318, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2319;
        u_value t2320 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2320;
      }
    }
    if ((!d2287) && u_is_con(s2286, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s2286, 0);
      u_value v_xs = u_field(s2286, 1);
      u_value v_lists = u_field(s2286, 2);
      u_value v_c = u_field(s2286, 3);
      d2287 = 1;
      {
        static struct u_object l_t_2321 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2321, &v_t, r_g);
        static struct u_object l_e_2322 = U_LIT("e");
        p_compile_46_gen_name(&l_e_2322, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_2323 = U_LIT("u_mk_con");
        static struct u_object l__38__2324 = U_LIT("&");
        u_value t2325 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_2323, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__2324, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t2325;
        u_value t2326 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2326;
      }
    }
    if ((!d2287) && u_is_con(s2286, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s2286, 0);
      d2287 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_2327 = U_LIT("u_mk_tuple");
        u_value t2328 = f_c_46_c_call_2(&l_u_mk_tuple_2327, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t2328;
      }
    }
    if ((!d2287) && u_is_con(s2286, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s2286, 0);
      u_value v_ty = u_field(s2286, 1);
      d2287 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2329 = U_LIT("u_raise");
        static struct u_object l_NULL_2330 = U_LIT("NULL");
        u_value t2331 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2329, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        u_value t2332 = u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2330);
        *r_ss = t2331;
        *r_r = t2332;
      }
    }
    if (!d2287) {
      u_no_case(s2286);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s2333 = v_es;
    int d2334 = 0;
    if ((!d2334) && u_is_con(s2333, &n_list_46_Nil)) {
      d2334 = 1;
      {
        u_value t2335 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2336 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2335;
        *r_rs = t2336;
      }
    }
    if ((!d2334) && u_is_con(s2333, &n_list_46_Cons)) {
      u_value v_e = u_field(s2333, 0);
      u_value v_rest = u_field(s2333, 1);
      d2334 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t2337 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2338 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t2337;
        *r_rs = t2338;
      }
    }
    if (!d2334) {
      u_no_case(s2333);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s2339 = v_rs;
    int d2340 = 0;
    if ((!d2340) && u_is_con(s2339, &n_list_46_Nil)) {
      d2340 = 1;
      {
        u_value t2341 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2341;
      }
    }
    if ((!d2340) && u_is_con(s2339, &n_list_46_Cons)) {
      u_value v_r = u_field(s2339, 0);
      u_value v_rest = u_field(s2339, 1);
      d2340 = 1;
      {
        static struct u_object l_t_2342 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2342, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t2343 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t2343;
      }
    }
    if (!d2340) {
      u_no_case(s2339);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s2344 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2345 = NULL;
  if ((!t2345) && (u_is_con(u_field(s2344, 0), &n_list_46_Cons) && (u_is_con(u_field(s2344, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2344, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2344, 0), 0);
    u_value v_more = u_field(u_field(s2344, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2344, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2344, 1), 1);
    t2345 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t2345) {
    t2345 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2345) {
    u_no_case(s2344);
  }
  return t2345;
}

void p_compile_46_compile_named(u_value v_gl, u_value v_env, u_value v_n, u_value v_ty, u_value v_args, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value v_ra = NULL;
    u_value v_rb = NULL;
    u_value v_rs = NULL;
    u_value v_t = NULL;
    u_value s2346 = v_args;
    int d2347 = 0;
    if ((!d2347) && (u_is_con(s2346, &n_list_46_Cons) && (u_is_con(u_field(s2346, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2346, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s2346, 0);
      u_value v_b = u_field(u_field(s2346, 1), 0);
      static struct u_object l_and_2354 = U_LIT("and");
      u_value t2356 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2354));
      if (!u_truth(t2356)) {
        static struct u_object l_or_2355 = U_LIT("or");
        t2356 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_2355));
      }
      if (u_truth(t2356)) {
        d2347 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_2348 = U_LIT("t");
          p_compile_46_gen_name(&l_t_2348, &v_t, r_g);
          static struct u_object l_and_2349 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2349)))) {
            {
              u_value t2350 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2350;
            }
          } else {
            {
              static struct u_object l__33__2351 = U_LIT("!");
              u_value t2352 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2351, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2352;
            }
          }
          u_value t2353 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t2353;
        }
      }
    }
    if (!d2347) {
      d2347 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t2357 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t2357)) {
          t2357 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t2357)) {
          {
            u_value t2358 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t2358;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_2359 = U_LIT("u_mk_con");
              u_value t2360 = f_c_46_c_call_2(&l_u_mk_con_2359, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t2360;
            }
          } else {
            {
              u_value s2361 = f_map_46_get_2(v_n, f_compile_46_builtin_functions_0());
              int d2362 = 0;
              if ((!d2362) && u_is_con(s2361, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s2361, 0), 0);
                u_value v_k = u_field(u_field(s2361, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d2362 = 1;
                  {
                    u_value t2363 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t2363;
                  }
                }
              }
              if (!d2362) {
                d2362 = 1;
                {
                  static struct u_object l_u_apply_2364 = U_LIT("u_apply");
                  u_value t2365 = f_c_46_c_call_2(&l_u_apply_2364, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_global_value_3(v_gl, v_n, v_ty), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t2365;
                }
              }
              if (!d2362) {
                u_no_case(s2361);
              }
            }
          }
        }
      }
    }
    if (!d2347) {
      u_no_case(s2346);
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
    u_value t2366 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2367 = &t2366;
    u_value l2368 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l2368, &n_list_46_Cons)) {
      u_value v_x = u_field(l2368, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2367 = u_append(e2367, v_x);
      }
      l2368 = u_field(l2368, 1);
    }
    u_value t2369 = t2366;
    v_caps = t2369;
    static struct u_object l_lam_2370 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_2370, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_2371 = U_LIT("env");
    static struct u_object l_args_2372 = U_LIT("args");
    u_value t2373 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2371), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_2372));
    v_decls = t2373;
    static struct u_object l_env_2374 = U_LIT("env");
    static struct u_object l_args_2375 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2374), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2375), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_2376 = U_LIT("u_mk_closure");
    u_value t2377 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2378 = &t2377;
    u_value l2379 = v_caps;
    while (u_is_con(l2379, &n_list_46_Cons)) {
      u_value v_x = u_field(l2379, 0);
      if (u_truth(u_mk_bool(1))) {
        e2378 = u_append(e2378, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2379 = u_field(l2379, 1);
    }
    u_value t2380 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2381 = f_c_46_c_call_2(&l_u_mk_closure_2376, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2377));
    *r_ss = t2380;
    *r_r = t2381;
  }
}

void p_compile_46_compile_proc_lambda(u_value v_gl, u_value v_env, u_value v_ins, u_value v_outs, u_value v_b, u_value *r_ss, u_value *r_r, u_value *r_g) {
  {
    u_value v_caps = NULL;
    u_value v_f = NULL;
    u_value v_bs = NULL;
    u_value v_decls = NULL;
    u_value t2382 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2383 = &t2382;
    u_value l2384 = f_list_46_distinct_1(f_compile_46_free_in_stmt_2(v_b, f_syntax_46_names_1(f_list_46__43__43__2(v_ins, v_outs))));
    while (u_is_con(l2384, &n_list_46_Cons)) {
      u_value v_x = u_field(l2384, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2383 = u_append(e2383, v_x);
      }
      l2384 = u_field(l2384, 1);
    }
    u_value t2385 = t2382;
    v_caps = t2385;
    static struct u_object l_proc_2386 = U_LIT("proc");
    p_compile_46_gen_name(&l_proc_2386, &v_f, r_g);
    u_value t2387 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2388 = &t2387;
    u_value l2389 = f_syntax_46_names_1(v_outs);
    while (u_is_con(l2389, &n_list_46_Cons)) {
      u_value v_x = u_field(l2389, 0);
      if (u_truth(u_mk_bool(1))) {
        e2388 = u_append(e2388, f_compile_46_pointer_1(v_x));
      }
      l2389 = u_field(l2389, 1);
    }
    p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_ins))), t2387), v_b, &v_bs, r_g);
    static struct u_object l_env_2390 = U_LIT("env");
    static struct u_object l_ins_2391 = U_LIT("ins");
    u_value t2392 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2390), f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_ins), u_mk_int(0)), &l_ins_2391), f_compile_46_unpack_pointers_1(f_compile_46_numbered_2(f_syntax_46_names_1(v_outs), u_mk_int(0)))));
    v_decls = t2392;
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), v_f, f_compile_46_proc_params_0(), f_list_46__43__43__2(v_decls, v_bs)), r_g);
    static struct u_object l_u_mk_proc_2393 = U_LIT("u_mk_proc");
    u_value t2394 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2395 = &t2394;
    u_value l2396 = v_caps;
    while (u_is_con(l2396, &n_list_46_Cons)) {
      u_value v_x = u_field(l2396, 0);
      if (u_truth(u_mk_bool(1))) {
        e2395 = u_append(e2395, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2396 = u_field(l2396, 1);
    }
    u_value t2397 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2398 = f_c_46_c_call_2(&l_u_mk_proc_2393, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2394));
    *r_ss = t2397;
    *r_r = t2398;
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
  static struct u_object l_env_2399 = U_LIT("env");
  static struct u_object l_ins_2400 = U_LIT("ins");
  static struct u_object l_outs_2401 = U_LIT("outs");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2399), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_ins_2400), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, f_compile_46_value_ptr_0()), &l_outs_2401), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_compile_46_unpack_pointers_1(u_value v_xs) {
  u_value s2402 = v_xs;
  u_value t2403 = NULL;
  if ((!t2403) && u_is_con(s2402, &n_list_46_Nil)) {
    t2403 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2403) && u_is_con(s2402, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2402, 0), 0);
    u_value v_i = u_field(u_field(s2402, 0), 1);
    u_value v_rest = u_field(s2402, 1);
    static struct u_object l_outs_2404 = U_LIT("outs");
    t2403 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(&l_outs_2404, v_i))), f_compile_46_unpack_pointers_1(v_rest));
  }
  if (!t2403) {
    u_no_case(s2402);
  }
  return t2403;
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s2405 = v_xs;
  u_value t2406 = NULL;
  if ((!t2406) && u_is_con(s2405, &n_list_46_Nil)) {
    t2406 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2406) && u_is_con(s2405, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2405, 0), 0);
    u_value v_i = u_field(u_field(s2405, 0), 1);
    u_value v_rest = u_field(s2405, 1);
    t2406 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t2406) {
    u_no_case(s2405);
  }
  return t2406;
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
    u_value s2407 = v_cases;
    int d2408 = 0;
    if ((!d2408) && u_is_con(s2407, &n_list_46_Nil)) {
      d2408 = 1;
      {
        u_value t2409 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2409;
      }
    }
    if ((!d2408) && (u_is_con(s2407, &n_list_46_Cons) && u_is_con(u_field(s2407, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2407, 0), 0);
      u_value v_guard = u_field(u_field(s2407, 0), 1);
      u_value v_e = u_field(u_field(s2407, 0), 2);
      u_value v_more = u_field(s2407, 1);
      d2408 = 1;
      {
        u_value t2410 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2410;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2411 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t2411;
        u_value s2412 = v_guard;
        int d2413 = 0;
        if ((!d2413) && u_is_con(s2412, &n_option_46_Some)) {
          u_value v_c = u_field(s2412, 0);
          d2413 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2414 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2414;
          }
        }
        if ((!d2413) && u_is_con(s2412, &n_option_46_None)) {
          d2413 = 1;
          {
          }
        }
        if (!d2413) {
          u_no_case(s2412);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2415 = U_LIT("!");
        u_value t2416 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2415, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2416;
      }
    }
    if (!d2408) {
      u_no_case(s2407);
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
    u_value s2417 = u_mk_tuple(2, v_xs, v_lists);
    int d2418 = 0;
    if ((!d2418) && (u_is_con(u_field(s2417, 0), &n_list_46_Cons) && u_is_con(u_field(s2417, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2417, 0), 0);
      u_value v_more = u_field(u_field(s2417, 0), 1);
      u_value v_list = u_field(u_field(s2417, 1), 0);
      u_value v_others = u_field(u_field(s2417, 1), 1);
      d2418 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2419 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2419, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2420 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2420;
        static struct u_object l_u_is_con_2421 = U_LIT("u_is_con");
        u_value t2422 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2421, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2422;
      }
    }
    if (!d2418) {
      d2418 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2423 = U_LIT("u_append");
        u_value t2424 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2423, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2424;
      }
    }
    if (!d2418) {
      u_no_case(s2417);
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
    u_value s2425 = v_s;
    int d2426 = 0;
    if ((!d2426) && u_is_con(s2425, &n_syntax_46_SSkip)) {
      d2426 = 1;
      {
        u_value t2427 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2427;
      }
    }
    if ((!d2426) && u_is_con(s2425, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2425, 0);
      d2426 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2428 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2428;
      }
    }
    if ((!d2426) && u_is_con(s2425, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2425, 0);
      u_value v_body = u_field(s2425, 1);
      d2426 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t2429 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2430 = &t2429;
        u_value l2431 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2431, &n_list_46_Cons)) {
          u_value v_x = u_field(l2431, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2432 = U_LIT("NULL");
            e2430 = u_append(e2430, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2432))));
          }
          l2431 = u_field(l2431, 1);
        }
        u_value t2433 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2429, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2433;
      }
    }
    if ((!d2426) && u_is_con(s2425, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2425, 0);
      u_value v_l = u_field(s2425, 1);
      u_value v_r = u_field(s2425, 2);
      d2426 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2434 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2434;
      }
    }
    if ((!d2426) && u_is_con(s2425, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2425, 0);
      u_value v_b = u_field(s2425, 1);
      d2426 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2435 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2435, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2436 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2436;
      }
    }
    if ((!d2426) && u_is_con(s2425, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2425, 0);
      u_value v_ins = u_field(s2425, 1);
      u_value v_outs = u_field(s2425, 2);
      u_value v_refs = u_field(s2425, 3);
      d2426 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        if (u_truth(f_list_46_has_key_2(v_p, v_env))) {
          {
            static struct u_object l_u_call_proc_2437 = U_LIT("u_call_proc");
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
            u_value t2441 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_call_proc_2437, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_var_expr_2(v_env, v_p), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_ins)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(f_list_46__43__43__2(v_outs, v_refs))), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(v_rs, t2438)))), u_mk_con(&n_list_46_Nil, 0)));
            *r_ss = t2441;
          }
        } else {
          {
            u_value t2442 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e2443 = &t2442;
            u_value l2444 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
            while (u_is_con(l2444, &n_list_46_Cons)) {
              u_value v_x = u_field(l2444, 0);
              if (u_truth(u_mk_bool(1))) {
                e2443 = u_append(e2443, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
              }
              l2444 = u_field(l2444, 1);
            }
            u_value t2445 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2442))), u_mk_con(&n_list_46_Nil, 0)));
            *r_ss = t2445;
          }
        }
      }
    }
    if ((!d2426) && u_is_con(s2425, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2425, 0);
      u_value v_cases = u_field(s2425, 1);
      d2426 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2446 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2446, &v_d, r_g);
        static struct u_object l_d_2447 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2447, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2448 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2448;
        static struct u_object l__33__2449 = U_LIT("!");
        static struct u_object l_u_no_case_2450 = U_LIT("u_no_case");
        u_value t2451 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2449, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2450, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2451;
      }
    }
    if ((!d2426) && (u_is_con(s2425, &n_syntax_46_SAssert) && (u_is_con(u_field(s2425, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2425, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2425, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2425, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2425, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2425, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2425, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2425, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2425, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2425, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2425, 0), 1), 1), 0);
      d2426 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2452 = U_LIT("u_assert_equal");
        u_value t2453 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2452, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2453;
      }
    }
    if ((!d2426) && u_is_con(s2425, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2425, 0);
      d2426 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2454 = U_LIT("u_assert_true");
        u_value t2455 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2454, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2455;
      }
    }
    if ((!d2426) && u_is_con(s2425, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2425, 0);
      u_value v_ty = u_field(s2425, 1);
      d2426 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2456 = U_LIT("u_raise");
        u_value t2457 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2456, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2457;
      }
    }
    if ((!d2426) && u_is_con(s2425, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2425, 0);
      u_value v_hs = u_field(s2425, 1);
      d2426 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2458 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2458, &v_d, r_g);
        static struct u_object l_d_2459 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2459, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2460 = U_LIT("u_pop");
        static struct u_object l__33__2461 = U_LIT("!");
        static struct u_object l_u_reraise_2462 = U_LIT("u_reraise");
        u_value t2463 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2460, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2461, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2462, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2463;
        static struct u_object l_struct_32_u_handler_2464 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2465 = U_LIT("u_push");
        static struct u_object l__61__61__2466 = U_LIT("==");
        static struct u_object l_setjmp_2467 = U_LIT("setjmp");
        static struct u_object l_env_2468 = U_LIT("env");
        static struct u_object l_u_pop_2469 = U_LIT("u_pop");
        u_value t2470 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2464)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2465, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2466, f_c_46_c_call_2(&l_setjmp_2467, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2468), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2469, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2470;
      }
    }
    if (!d2426) {
      u_no_case(s2425);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2471 = v_xs;
    int d2472 = 0;
    if ((!d2472) && u_is_con(s2471, &n_list_46_Nil)) {
      d2472 = 1;
      {
        u_value t2473 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2473;
      }
    }
    if ((!d2472) && u_is_con(s2471, &n_list_46_Cons)) {
      u_value v_x = u_field(s2471, 0);
      u_value v_rest = u_field(s2471, 1);
      d2472 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2474 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2474;
      }
    }
    if (!d2472) {
      u_no_case(s2471);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2475 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2476 = NULL;
  if ((!t2476) && (u_is_con(u_field(s2475, 0), &n_list_46_Cons) && (u_is_con(u_field(s2475, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2475, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2475, 0), 0);
    u_value v_more = u_field(u_field(s2475, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2475, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2475, 1), 1);
    t2476 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2476) {
    t2476 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2476) {
    u_no_case(s2475);
  }
  return t2476;
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
    static struct u_object l_u_caught_2477 = U_LIT("u_caught");
    u_value t2478 = f_c_46_c_call_2(&l_u_caught_2477, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2478;
    u_value s2479 = v_hs;
    int d2480 = 0;
    if ((!d2480) && u_is_con(s2479, &n_list_46_Nil)) {
      d2480 = 1;
      {
        u_value t2481 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2481;
      }
    }
    if ((!d2480) && (u_is_con(s2479, &n_list_46_Cons) && u_is_con(u_field(s2479, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2479, 0), 0);
      u_value v_p = u_field(u_field(s2479, 0), 1);
      u_value v_guard = u_field(u_field(s2479, 0), 2);
      u_value v_s = u_field(u_field(s2479, 0), 3);
      u_value v_more = u_field(s2479, 1);
      d2480 = 1;
      {
        u_value t2482 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2482;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2483 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2483;
        u_value s2484 = v_guard;
        int d2485 = 0;
        if ((!d2485) && u_is_con(s2484, &n_option_46_Some)) {
          u_value v_c = u_field(s2484, 0);
          d2485 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2486 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2486;
          }
        }
        if ((!d2485) && u_is_con(s2484, &n_option_46_None)) {
          d2485 = 1;
          {
          }
        }
        if (!d2485) {
          u_no_case(s2484);
        }
        u_value s2487 = v_p;
        int d2488 = 0;
        if ((!d2488) && u_is_con(s2487, &n_syntax_46_PWild)) {
          d2488 = 1;
          {
            u_value t2489 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2489;
          }
        }
        if (!d2488) {
          d2488 = 1;
          {
            static struct u_object l_u_has_type_2490 = U_LIT("u_has_type");
            u_value t2491 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2490, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2491;
          }
        }
        if (!d2488) {
          u_no_case(s2487);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2492 = U_LIT("!");
        u_value t2493 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2492, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2493;
      }
    }
    if (!d2480) {
      u_no_case(s2479);
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
    u_value s2494 = v_cases;
    int d2495 = 0;
    if ((!d2495) && u_is_con(s2494, &n_list_46_Nil)) {
      d2495 = 1;
      {
        u_value t2496 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2496;
      }
    }
    if ((!d2495) && (u_is_con(s2494, &n_list_46_Cons) && u_is_con(u_field(s2494, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2494, 0), 0);
      u_value v_guard = u_field(u_field(s2494, 0), 1);
      u_value v_s = u_field(u_field(s2494, 0), 2);
      u_value v_more = u_field(s2494, 1);
      d2495 = 1;
      {
        u_value t2497 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2497;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2498 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2498;
        u_value s2499 = v_guard;
        int d2500 = 0;
        if ((!d2500) && u_is_con(s2499, &n_option_46_Some)) {
          u_value v_c = u_field(s2499, 0);
          d2500 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2501 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2501;
          }
        }
        if ((!d2500) && u_is_con(s2499, &n_option_46_None)) {
          d2500 = 1;
          {
          }
        }
        if (!d2500) {
          u_no_case(s2499);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2502 = U_LIT("!");
        u_value t2503 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2502, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2503;
      }
    }
    if (!d2495) {
      u_no_case(s2494);
    }
  }
}

void p_compile_46_compile_cmd(u_value v_gl, u_value v_entry, u_value v_c, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_bs = NULL;
    u_value v_rb = NULL;
    u_value v_params = NULL;
    u_value s2504 = v_c;
    int d2505 = 0;
    if ((!d2505) && u_is_con(s2504, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2504, 0);
      u_value v_xss = u_field(s2504, 2);
      u_value v_b = u_field(s2504, 4);
      d2505 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2506 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2507 = &t2506;
        u_value l2508 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2508, &n_list_46_Cons)) {
          u_value v_x = u_field(l2508, 0);
          if (u_truth(u_mk_bool(1))) {
            e2507 = u_append(e2507, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2508 = u_field(l2508, 1);
        }
        u_value t2509 = t2506;
        v_params = t2509;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2510 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2510;
          }
        } else {
          {
            u_value t2511 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2511;
          }
        }
        u_value t2512 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2513 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2512;
        *r_ms = t2513;
      }
    }
    if ((!d2505) && u_is_con(s2504, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2504, 0);
      u_value v_ins = u_field(s2504, 2);
      u_value v_outs = u_field(s2504, 3);
      u_value v_refs = u_field(s2504, 4);
      u_value v_body = u_field(s2504, 5);
      d2505 = 1;
      {
        u_value t2514 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2515 = &t2514;
        u_value l2516 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2516, &n_list_46_Cons)) {
          u_value v_x = u_field(l2516, 0);
          if (u_truth(u_mk_bool(1))) {
            e2515 = u_append(e2515, f_compile_46_pointer_1(v_x));
          }
          l2516 = u_field(l2516, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2514), v_body, &v_bs, r_g);
        u_value t2517 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2518 = &t2517;
        u_value l2519 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2519, &n_list_46_Cons)) {
          u_value v_x = u_field(l2519, 0);
          if (u_truth(u_mk_bool(1))) {
            e2518 = u_append(e2518, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2519 = u_field(l2519, 1);
        }
        u_value t2520 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2521 = &t2520;
        u_value l2522 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2522, &n_list_46_Cons)) {
          u_value v_x = u_field(l2522, 0);
          if (u_truth(u_mk_bool(1))) {
            e2521 = u_append(e2521, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2522 = u_field(l2522, 1);
        }
        u_value t2523 = f_list_46__43__43__2(t2517, t2520);
        v_params = t2523;
        u_value t2524 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2525 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2524;
        *r_ms = t2525;
      }
    }
    if ((!d2505) && u_is_con(s2504, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s2504, 0);
      u_value v_body = u_field(s2504, 1);
      if (u_truth(u_eq(v_n, v_entry))) {
        d2505 = 1;
        {
          p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
          u_value t2526 = u_mk_con(&n_list_46_Nil, 0);
          u_value t2527 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
          *r_ds = t2526;
          *r_ms = t2527;
        }
      }
    }
    if ((!d2505) && u_is_con(s2504, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s2504, 0);
      u_value v_body = u_field(s2504, 1);
      static struct u_object l_test_2531 = U_LIT("test");
      if (u_truth(u_eq(v_entry, &l_test_2531))) {
        d2505 = 1;
        {
          p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
          static struct u_object l_u_passed_2528 = U_LIT("u_passed");
          u_value t2529 = u_mk_con(&n_list_46_Nil, 0);
          u_value t2530 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_passed_2528, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
          *r_ds = t2529;
          *r_ms = t2530;
        }
      }
    }
    if (!d2505) {
      d2505 = 1;
      {
        u_value t2532 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2533 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2532;
        *r_ms = t2533;
      }
    }
    if (!d2505) {
      u_no_case(s2504);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_entry, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2534 = v_cs;
    int d2535 = 0;
    if ((!d2535) && u_is_con(s2534, &n_list_46_Nil)) {
      d2535 = 1;
      {
        u_value t2536 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2537 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2536;
        *r_ms = t2537;
      }
    }
    if ((!d2535) && u_is_con(s2534, &n_list_46_Cons)) {
      u_value v_c = u_field(s2534, 0);
      u_value v_rest = u_field(s2534, 1);
      d2535 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_entry, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_entry, v_rest, &v_d2, &v_m2, r_g);
        u_value t2538 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2539 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2538;
        *r_ms = t2539;
      }
    }
    if (!d2535) {
      u_no_case(s2534);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_handler_2540 = U_LIT("handler");
  u_value t2567 = u_mk_con(&n_c_46_CeVar, 1, &l_handler_2540);
  u_value t2566;
  {
    u_value v_h = t2567;
    static struct u_object l_static_32_u_value_2541 = U_LIT("static u_value");
    static struct u_object l_cache_2542 = U_LIT("cache");
    static struct u_object l_NULL_2543 = U_LIT("NULL");
    static struct u_object l__61__61__2544 = U_LIT("==");
    static struct u_object l_cache_2545 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2546 = U_LIT("U_COMPUTING");
    static struct u_object l_u_fail_2547 = U_LIT("u_fail");
    static struct u_object l_recursive_32_initia_2548 = U_LIT("recursive initialization of ");
    static struct u_object l_cache_2549 = U_LIT("cache");
    static struct u_object l_cache_2550 = U_LIT("cache");
    static struct u_object l_cache_2551 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2552 = U_LIT("U_COMPUTING");
    static struct u_object l_struct_32_u_handler_2553 = U_LIT("struct u_handler");
    static struct u_object l_handler_2554 = U_LIT("handler");
    static struct u_object l_u_push_2555 = U_LIT("u_push");
    static struct u_object l__33__61__2556 = U_LIT("!=");
    static struct u_object l_setjmp_2557 = U_LIT("setjmp");
    static struct u_object l_env_2558 = U_LIT("env");
    static struct u_object l_u_pop_2559 = U_LIT("u_pop");
    static struct u_object l_cache_2560 = U_LIT("cache");
    static struct u_object l_NULL_2561 = U_LIT("NULL");
    static struct u_object l_u_reraise_2562 = U_LIT("u_reraise");
    static struct u_object l_u_pop_2563 = U_LIT("u_pop");
    static struct u_object l_cache_2564 = U_LIT("cache");
    static struct u_object l_cache_2565 = U_LIT("cache");
    t2566 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2541), &l_cache_2542, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2543))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2544, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2545), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2546)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2547, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2548, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2549), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2550))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2551), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2552)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2553)), &l_handler_2554, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2555, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__33__61__2556, f_c_46_c_call_2(&l_setjmp_2557, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, v_h, &l_env_2558), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2559, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2560), u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2561)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2562, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2563, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2564), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2565))), u_mk_con(&n_list_46_Nil, 0))))));
  }
  return t2566;
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2568 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2569 = &t2568;
  u_value l2570 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l2570, &n_list_46_Cons)) {
    u_value v_p = u_field(l2570, 0);
    if (u_truth(u_mk_bool(1))) {
      e2569 = u_append(e2569, f_compile_46_fun_wrapper_1(v_p));
    }
    l2570 = u_field(l2570, 1);
  }
  u_value t2571 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2572 = &t2571;
  u_value l2573 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l2573, &n_list_46_Cons)) {
    u_value v_p = u_field(l2573, 0);
    if (u_truth(u_mk_bool(1))) {
      e2572 = u_append(e2572, f_compile_46_constr_wrapper_1(v_p));
    }
    l2573 = u_field(l2573, 1);
  }
  u_value t2574 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2575 = &t2574;
  u_value l2576 = f_list_46_seconds_1(f_map_46_entries_1(f_compile_46_builtin_functions_0()));
  while (u_is_con(l2576, &n_list_46_Cons)) {
    u_value v_p = u_field(l2576, 0);
    if (u_truth(u_mk_bool(1))) {
      e2575 = u_append(e2575, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2576 = u_field(l2576, 1);
  }
  u_value t2577 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2578 = &t2577;
  u_value l2579 = f_map_46_entries_1(f_compile_46_Globals_46_procs_1(v_gl));
  while (u_is_con(l2579, &n_list_46_Cons)) {
    u_value v_p = u_field(l2579, 0);
    if (u_truth(u_mk_bool(1))) {
      e2578 = u_append(e2578, f_compile_46_proc_wrapper_1(v_p));
    }
    l2579 = u_field(l2579, 1);
  }
  return f_list_46__43__43__2(t2568, f_list_46__43__43__2(f_list_46_concat_1(t2571), f_list_46__43__43__2(t2574, t2577)));
}

u_value f_compile_46_proc_wrapper_1(u_value v_p) {
  u_value s2580 = v_p;
  u_value t2581 = NULL;
  if (!t2581) {
    u_value v_n = u_field(s2580, 0);
    u_value v_k = u_field(u_field(s2580, 1), 0);
    u_value v_m = u_field(u_field(s2580, 1), 1);
    static struct u_object l_wp__2582 = U_LIT("wp_");
    u_value t2583 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2584 = &t2583;
    u_value l2585 = f_compile_46_range_to_1(v_k);
    while (u_is_con(l2585, &n_list_46_Cons)) {
      u_value v_i = u_field(l2585, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_ins_2586 = U_LIT("ins");
        e2584 = u_append(e2584, f_compile_46_index_2(&l_ins_2586, v_i));
      }
      l2585 = u_field(l2585, 1);
    }
    u_value t2587 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2588 = &t2587;
    u_value l2589 = f_compile_46_range_to_1(v_m);
    while (u_is_con(l2589, &n_list_46_Cons)) {
      u_value v_i = u_field(l2589, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l_outs_2590 = U_LIT("outs");
        e2588 = u_append(e2588, f_compile_46_index_2(&l_outs_2590, v_i));
      }
      l2589 = u_field(l2589, 1);
    }
    t2581 = u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), u_concat(&l_wp__2582, f_compile_46_mangle_1(v_n)), f_compile_46_proc_params_0(), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_n), f_list_46__43__43__2(t2583, t2587))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t2581) {
    u_no_case(s2580);
  }
  return t2581;
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2591 = v_p;
  u_value t2592 = NULL;
  if (!t2592) {
    u_value v_n = u_field(s2591, 0);
    u_value v_k = u_field(s2591, 1);
    static struct u_object l_w__2593 = U_LIT("w_");
    t2592 = f_compile_46_wrapper_3(u_concat(&l_w__2593, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2592) {
    u_no_case(s2591);
  }
  return t2592;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2594 = v_p;
  u_value t2595 = NULL;
  if (!t2595) {
    u_value v_f = u_field(s2594, 0);
    u_value v_k = u_field(s2594, 1);
    static struct u_object l_w_2596 = U_LIT("w");
    t2595 = f_compile_46_wrapper_3(u_concat(&l_w_2596, v_f), v_f, v_k);
  }
  if (!t2595) {
    u_no_case(s2594);
  }
  return t2595;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2597 = U_LIT("env");
  static struct u_object l_args_2598 = U_LIT("args");
  u_value t2599 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2600 = &t2599;
  u_value l2601 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2601, &n_list_46_Cons)) {
    u_value v_i = u_field(l2601, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2602 = U_LIT("args");
      e2600 = u_append(e2600, f_compile_46_index_2(&l_args_2602, v_i));
    }
    l2601 = u_field(l2601, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2597), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2598), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2599))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2603 = v_p;
  u_value t2604 = NULL;
  if ((!t2604) && u_equals(u_field(s2603, 1), u_mk_int(0))) {
    t2604 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2604) {
    u_value v_n = u_field(s2603, 0);
    u_value v_k = u_field(s2603, 1);
    static struct u_object l_wc__2605 = U_LIT("wc_");
    static struct u_object l_env_2606 = U_LIT("env");
    static struct u_object l_args_2607 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2608 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2609 = U_LIT("args");
    t2604 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2605, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2606), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2607), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2608, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2609), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2604) {
    u_no_case(s2603);
  }
  return t2604;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2610;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2610 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2610 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2610;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2611 = v_d;
  u_value t2612 = NULL;
  if ((!t2612) && u_is_con(s2611, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2611, 0);
    u_value v_f = u_field(s2611, 1);
    u_value v_ps = u_field(s2611, 2);
    t2612 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2612) {
    t2612 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2612) {
    u_no_case(s2611);
  }
  return t2612;
}

void p_compile_46_compile_program(u_value v_q, u_value v_entry, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2613 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2613;
    u_value t2614 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t2614;
    static struct u_object l_test_2615 = U_LIT("test");
    u_value t2617 = f_prelude_46__33__61__2(v_entry, &l_test_2615);
    if (u_truth(t2617)) {
      t2617 = f_prelude_46_not_1(f_list_46_exists_2(f_syntax_46_Program_46_cmds_1(v_q), u_mk_closure(lam2616, 1, v_entry)));
    }
    if (u_truth(t2617)) {
      {
        u_raise(u_mk_con(&n_compile_46_UnknownEntry, 1, v_entry), "compile.CompileError");
      }
    }
    p_compile_46_compile_cmds(v_gl, v_entry, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value t2618 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, f_compile_46_Gen_46_lifted_1(v_g)));
    v_fs = t2618;
    static struct u_object l_ulang_46_h_2619 = U_LIT("ulang.h");
    u_value t2620 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2621 = &t2620;
    u_value l2622 = v_fs;
    while (u_is_con(l2622, &n_list_46_Cons)) {
      u_value v_d = u_field(l2622, 0);
      if (u_truth(u_mk_bool(1))) {
        e2621 = u_append(e2621, f_compile_46_prototype_1(v_d));
      }
      l2622 = u_field(l2622, 1);
    }
    static struct u_object l_main_2623 = U_LIT("main");
    static struct u_object l_argc_2624 = U_LIT("argc");
    static struct u_object l_char_2625 = U_LIT("char");
    static struct u_object l_argv_2626 = U_LIT("argv");
    static struct u_object l_u_args_2627 = U_LIT("u_args");
    static struct u_object l_argc_2628 = U_LIT("argc");
    static struct u_object l_argv_2629 = U_LIT("argv");
    u_value t2630 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2619), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2620), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2623, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2624), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2625))), &l_argv_2626), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2627, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2628), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2629), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2630;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_entry, u_value v_target) {
  {
    u_value v_cache = NULL;
    u_value t2631 = u_mk_con(&n_list_46_Nil, 0);
    v_cache = t2631;
    p_compile_46_compile_with(v_source, v_entry, v_target, &v_cache);
  }
}

u_value f_compile_46_is_run_2(u_value v_c, u_value v_entry) {
  u_value s2632 = v_c;
  u_value t2633 = NULL;
  if ((!t2633) && u_is_con(s2632, &n_syntax_46_CRun)) {
    u_value v_n = u_field(s2632, 0);
    t2633 = u_eq(v_n, v_entry);
  }
  if (!t2633) {
    t2633 = u_mk_bool(0);
  }
  if (!t2633) {
    u_no_case(s2632);
  }
  return t2633;
}

void p_compile_46_compile_with(u_value v_source, u_value v_entry, u_value v_target, u_value *r_cache) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2634 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2634, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2635 = U_LIT("prelude");
    p_check_46_check_modules(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2635, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q, r_cache);
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

u_value lam807(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_q = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_q);
}

u_value lam1138(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

u_value lam1829(u_value *env, u_value *args) {
  u_value v_t = env[0];
  u_value v_e = args[0];
  return f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t);
}

u_value lam2616(u_value *env, u_value *args) {
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
      u_value s2636 = v_args;
      int d2637 = 0;
      if ((!d2637) && u_is_con(s2636, &n_list_46_Nil)) {
        d2637 = 1;
        {
        }
      }
      if ((!d2637) && (u_is_con(s2636, &n_list_46_Cons) && (u_equals(u_field(s2636, 0), u_mk_lit("compile")) && (u_is_con(u_field(s2636, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s2636, 1), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2636, 1), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2636, 1), 1), 1), 1), &n_list_46_Nil))))))) {
        u_value v_source = u_field(u_field(s2636, 1), 0);
        u_value v_entry = u_field(u_field(u_field(s2636, 1), 1), 0);
        u_value v_target = u_field(u_field(u_field(u_field(s2636, 1), 1), 1), 0);
        d2637 = 1;
        {
          p_compile_46_compile_file(v_source, v_entry, v_target);
        }
      }
      if (!d2637) {
        d2637 = 1;
        {
          static struct u_object l_usage_58__32_ulang_32_com_2638 = U_LIT("usage: ulang compile <file> <entry> <target.c>");
          u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_com_2638), "os.OsError");
        }
      }
      if (!d2637) {
        u_no_case(s2636);
      }
    }
  }
  return 0;
}
