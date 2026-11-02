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

u_value w_f_syntax_46_Var_46_name_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Var_46_typ_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Branch_46_pat_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Branch_46_guard_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Branch_46_body_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Handler_46_typ_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Handler_46_pat_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Handler_46_guard_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Handler_46_body_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Constr_46_name_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Constr_46_args_1(u_value *env, u_value *args);

u_value w_f_syntax_46_Program_46_cmds_1(u_value *env, u_value *args);

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

u_value w_f_parse_46_Scope_46_params_1(u_value *env, u_value *args);

u_value w_f_parse_46_Scope_46_notations_1(u_value *env, u_value *args);

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

u_value f_syntax_46_Var_46_name_1(u_value v_x);

u_value f_syntax_46_Var_46_typ_1(u_value v_x);

u_value f_syntax_46_Branch_46_pat_1(u_value v_x);

u_value f_syntax_46_Branch_46_guard_1(u_value v_x);

u_value f_syntax_46_Branch_46_body_1(u_value v_x);

u_value f_syntax_46_Handler_46_typ_1(u_value v_x);

u_value f_syntax_46_Handler_46_pat_1(u_value v_x);

u_value f_syntax_46_Handler_46_guard_1(u_value v_x);

u_value f_syntax_46_Handler_46_body_1(u_value v_x);

u_value f_syntax_46_Constr_46_name_1(u_value v_x);

u_value f_syntax_46_Constr_46_args_1(u_value v_x);

u_value f_syntax_46_Program_46_cmds_1(u_value v_x);

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

u_value f_parse_46_Scope_46_params_1(u_value v_x);

u_value f_parse_46_Scope_46_notations_1(u_value v_x);

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

u_value lam312(u_value *env, u_value *args);

u_value lam313(u_value *env, u_value *args);

u_value lam315(u_value *env, u_value *args);

u_value lam325(u_value *env, u_value *args);

u_value lam332(u_value *env, u_value *args);

u_value lam557(u_value *env, u_value *args);

u_value lam666(u_value *env, u_value *args);

u_value lam669(u_value *env, u_value *args);

u_value lam731(u_value *env, u_value *args);

u_value lam1008(u_value *env, u_value *args);

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

u_value w_f_syntax_46_Var_46_name_1(u_value *env, u_value *args) {
  return f_syntax_46_Var_46_name_1(args[0]);
}

u_value w_f_syntax_46_Var_46_typ_1(u_value *env, u_value *args) {
  return f_syntax_46_Var_46_typ_1(args[0]);
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

u_value w_f_syntax_46_Constr_46_name_1(u_value *env, u_value *args) {
  return f_syntax_46_Constr_46_name_1(args[0]);
}

u_value w_f_syntax_46_Constr_46_args_1(u_value *env, u_value *args) {
  return f_syntax_46_Constr_46_args_1(args[0]);
}

u_value w_f_syntax_46_Program_46_cmds_1(u_value *env, u_value *args) {
  return f_syntax_46_Program_46_cmds_1(args[0]);
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

u_value w_f_parse_46_Scope_46_params_1(u_value *env, u_value *args) {
  return f_parse_46_Scope_46_params_1(args[0]);
}

u_value w_f_parse_46_Scope_46_notations_1(u_value *env, u_value *args) {
  return f_parse_46_Scope_46_notations_1(args[0]);
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

u_value f_syntax_46_Var_46_name_1(u_value v_x) {
  u_value s67 = v_x;
  u_value t68 = NULL;
  if ((!t68) && u_is_con(s67, &n_syntax_46_Var)) {
    u_value v_y = u_field(s67, 0);
    t68 = v_y;
  }
  if (!t68) {
    u_no_case(s67);
  }
  return t68;
}

u_value f_syntax_46_Var_46_typ_1(u_value v_x) {
  u_value s69 = v_x;
  u_value t70 = NULL;
  if ((!t70) && u_is_con(s69, &n_syntax_46_Var)) {
    u_value v_y = u_field(s69, 1);
    t70 = v_y;
  }
  if (!t70) {
    u_no_case(s69);
  }
  return t70;
}

u_value f_syntax_46_Branch_46_pat_1(u_value v_x) {
  u_value s71 = v_x;
  u_value t72 = NULL;
  if ((!t72) && u_is_con(s71, &n_syntax_46_Branch)) {
    u_value v_y = u_field(s71, 0);
    t72 = v_y;
  }
  if (!t72) {
    u_no_case(s71);
  }
  return t72;
}

u_value f_syntax_46_Branch_46_guard_1(u_value v_x) {
  u_value s73 = v_x;
  u_value t74 = NULL;
  if ((!t74) && u_is_con(s73, &n_syntax_46_Branch)) {
    u_value v_y = u_field(s73, 1);
    t74 = v_y;
  }
  if (!t74) {
    u_no_case(s73);
  }
  return t74;
}

u_value f_syntax_46_Branch_46_body_1(u_value v_x) {
  u_value s75 = v_x;
  u_value t76 = NULL;
  if ((!t76) && u_is_con(s75, &n_syntax_46_Branch)) {
    u_value v_y = u_field(s75, 2);
    t76 = v_y;
  }
  if (!t76) {
    u_no_case(s75);
  }
  return t76;
}

u_value f_syntax_46_Handler_46_typ_1(u_value v_x) {
  u_value s77 = v_x;
  u_value t78 = NULL;
  if ((!t78) && u_is_con(s77, &n_syntax_46_Handler)) {
    u_value v_y = u_field(s77, 0);
    t78 = v_y;
  }
  if (!t78) {
    u_no_case(s77);
  }
  return t78;
}

u_value f_syntax_46_Handler_46_pat_1(u_value v_x) {
  u_value s79 = v_x;
  u_value t80 = NULL;
  if ((!t80) && u_is_con(s79, &n_syntax_46_Handler)) {
    u_value v_y = u_field(s79, 1);
    t80 = v_y;
  }
  if (!t80) {
    u_no_case(s79);
  }
  return t80;
}

u_value f_syntax_46_Handler_46_guard_1(u_value v_x) {
  u_value s81 = v_x;
  u_value t82 = NULL;
  if ((!t82) && u_is_con(s81, &n_syntax_46_Handler)) {
    u_value v_y = u_field(s81, 2);
    t82 = v_y;
  }
  if (!t82) {
    u_no_case(s81);
  }
  return t82;
}

u_value f_syntax_46_Handler_46_body_1(u_value v_x) {
  u_value s83 = v_x;
  u_value t84 = NULL;
  if ((!t84) && u_is_con(s83, &n_syntax_46_Handler)) {
    u_value v_y = u_field(s83, 3);
    t84 = v_y;
  }
  if (!t84) {
    u_no_case(s83);
  }
  return t84;
}

u_value f_syntax_46_Constr_46_name_1(u_value v_x) {
  u_value s85 = v_x;
  u_value t86 = NULL;
  if ((!t86) && u_is_con(s85, &n_syntax_46_Constr)) {
    u_value v_y = u_field(s85, 0);
    t86 = v_y;
  }
  if (!t86) {
    u_no_case(s85);
  }
  return t86;
}

u_value f_syntax_46_Constr_46_args_1(u_value v_x) {
  u_value s87 = v_x;
  u_value t88 = NULL;
  if ((!t88) && u_is_con(s87, &n_syntax_46_Constr)) {
    u_value v_y = u_field(s87, 1);
    t88 = v_y;
  }
  if (!t88) {
    u_no_case(s87);
  }
  return t88;
}

u_value f_syntax_46_Program_46_cmds_1(u_value v_x) {
  u_value s89 = v_x;
  u_value t90 = NULL;
  if ((!t90) && u_is_con(s89, &n_syntax_46_Program)) {
    u_value v_y = u_field(s89, 0);
    t90 = v_y;
  }
  if (!t90) {
    u_no_case(s89);
  }
  return t90;
}

u_value f_syntax_46_unqualified_1(u_value v_x) {
  return u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_None, 0), v_x);
}

u_value f_syntax_46_qualified_1(u_value v_n) {
  u_value s91 = v_n;
  u_value t92 = NULL;
  if ((!t92) && (u_is_con(s91, &n_syntax_46_Name) && u_is_con(u_field(s91, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s91, 0), 0);
    u_value v_b = u_field(s91, 1);
    static struct u_object l__46__93 = U_LIT(".");
    t92 = u_concat(v_m, u_concat(&l__46__93, v_b));
  }
  if ((!t92) && (u_is_con(s91, &n_syntax_46_Name) && u_is_con(u_field(s91, 0), &n_option_46_None))) {
    u_value v_b = u_field(s91, 1);
    t92 = v_b;
  }
  if (!t92) {
    u_no_case(s91);
  }
  return t92;
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
  static struct u_object l_list_94 = U_LIT("list");
  static struct u_object l_List_95 = U_LIT("List");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_list_94), &l_List_95);
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
  static struct u_object l_list_96 = U_LIT("list");
  static struct u_object l_Nil_97 = U_LIT("Nil");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_list_96), &l_Nil_97);
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
  static struct u_object l_list_98 = U_LIT("list");
  static struct u_object l_Cons_99 = U_LIT("Cons");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_list_98), &l_Cons_99);
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
  static struct u_object l_option_100 = U_LIT("option");
  static struct u_object l_Option_101 = U_LIT("Option");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_option_100), &l_Option_101);
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
  static struct u_object l_option_102 = U_LIT("option");
  static struct u_object l_None_103 = U_LIT("None");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_option_102), &l_None_103);
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
  static struct u_object l_option_104 = U_LIT("option");
  static struct u_object l_Some_105 = U_LIT("Some");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_option_104), &l_Some_105);
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
  static struct u_object l_os_106 = U_LIT("os");
  static struct u_object l_OsError_107 = U_LIT("OsError");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_os_106), &l_OsError_107);
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
  static struct u_object l_prelude_108 = U_LIT("prelude");
  static struct u_object l_DivisionByZero_109 = U_LIT("DivisionByZero");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_prelude_108), &l_DivisionByZero_109);
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
  static struct u_object l_prelude_110 = U_LIT("prelude");
  static struct u_object l_NoIndex_111 = U_LIT("NoIndex");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_prelude_110), &l_NoIndex_111);
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
  static struct u_object l_prelude_112 = U_LIT("prelude");
  static struct u_object l_NoCharacter_113 = U_LIT("NoCharacter");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_prelude_112), &l_NoCharacter_113);
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
  static struct u_object l_prelude_114 = U_LIT("prelude");
  static struct u_object l_MissingField_115 = U_LIT("MissingField");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_prelude_114), &l_MissingField_115);
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
  static struct u_object l__44__116 = U_LIT(",");
  u_pop(handler);
  cache = f_syntax_46_unqualified_1(&l__44__116);
  return cache;
}

u_value f_syntax_46_names_1(u_value v_xs) {
  u_value t117 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e118 = &t117;
  u_value l119 = v_xs;
  while (u_is_con(l119, &n_list_46_Cons)) {
    u_value v_x = u_field(l119, 0);
    if (u_truth(u_mk_bool(1))) {
      e118 = u_append(e118, f_syntax_46_Var_46_name_1(v_x));
    }
    l119 = u_field(l119, 1);
  }
  return t117;
}

u_value f_syntax_46_types_1(u_value v_xs) {
  u_value t120 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e121 = &t120;
  u_value l122 = v_xs;
  while (u_is_con(l122, &n_list_46_Cons)) {
    u_value v_x = u_field(l122, 0);
    if (u_truth(u_mk_bool(1))) {
      e121 = u_append(e121, f_syntax_46_Var_46_typ_1(v_x));
    }
    l122 = u_field(l122, 1);
  }
  return t120;
}

u_value f_syntax_46_arity_1(u_value v_t) {
  u_value s123 = v_t;
  u_value t124 = NULL;
  if ((!t124) && u_is_con(s123, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s123, 0);
    t124 = f_list_46_length_1(v_args);
  }
  if (!t124) {
    t124 = f_prelude_46__45__1(u_mk_int(1));
  }
  if (!t124) {
    u_no_case(s123);
  }
  return t124;
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
  u_value s125 = v_m;
  u_value t126 = NULL;
  if ((!t126) && u_is_con(s125, &n_map_46_MLeaf)) {
    t126 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t126) && u_is_con(s125, &n_map_46_MNode)) {
    u_value v_l = u_field(s125, 0);
    u_value v_j = u_field(s125, 1);
    u_value v_v = u_field(s125, 2);
    u_value v_r = u_field(s125, 3);
    u_value t130 = u_compare(v_k, v_j);
    u_value t129;
    {
      u_value v_c = t130;
      u_value t128;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t128 = f_map_46_get_2(v_k, v_l);
      } else {
        u_value t127;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t127 = f_map_46_get_2(v_k, v_r);
        } else {
          t127 = u_mk_con(&n_option_46_Some, 1, v_v);
        }
        t128 = t127;
      }
      t129 = t128;
    }
    t126 = t129;
  }
  if (!t126) {
    u_no_case(s125);
  }
  return t126;
}

u_value f_map_46_has_2(u_value v_k, u_value v_m) {
  return f_prelude_46__33__61__2(f_map_46_get_2(v_k, v_m), u_mk_con(&n_option_46_None, 0));
}

u_value f_map_46_put_3(u_value v_k, u_value v_v, u_value v_m) {
  u_value s131 = v_m;
  u_value t132 = NULL;
  if ((!t132) && u_is_con(s131, &n_map_46_MLeaf)) {
    t132 = u_mk_con(&n_map_46_MNode, 5, u_mk_con(&n_map_46_MLeaf, 0), v_k, v_v, u_mk_con(&n_map_46_MLeaf, 0), u_mk_int(1));
  }
  if ((!t132) && u_is_con(s131, &n_map_46_MNode)) {
    u_value v_l = u_field(s131, 0);
    u_value v_j = u_field(s131, 1);
    u_value v_w = u_field(s131, 2);
    u_value v_r = u_field(s131, 3);
    u_value v_h = u_field(s131, 4);
    u_value t136 = u_compare(v_k, v_j);
    u_value t135;
    {
      u_value v_c = t136;
      u_value t134;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t134 = f_map_46_balance_4(f_map_46_put_3(v_k, v_v, v_l), v_j, v_w, v_r);
      } else {
        u_value t133;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t133 = f_map_46_balance_4(v_l, v_j, v_w, f_map_46_put_3(v_k, v_v, v_r));
        } else {
          t133 = u_mk_con(&n_map_46_MNode, 5, v_l, v_k, v_v, v_r, v_h);
        }
        t134 = t133;
      }
      t135 = t134;
    }
    t132 = t135;
  }
  if (!t132) {
    u_no_case(s131);
  }
  return t132;
}

u_value f_map_46_entries_1(u_value v_m) {
  return f_map_46_entries_onto_2(v_m, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_map_46_entries_onto_2(u_value v_m, u_value v_rest) {
  u_value s137 = v_m;
  u_value t138 = NULL;
  if ((!t138) && u_is_con(s137, &n_map_46_MLeaf)) {
    t138 = v_rest;
  }
  if ((!t138) && u_is_con(s137, &n_map_46_MNode)) {
    u_value v_l = u_field(s137, 0);
    u_value v_k = u_field(s137, 1);
    u_value v_v = u_field(s137, 2);
    u_value v_r = u_field(s137, 3);
    t138 = f_map_46_entries_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_v), f_map_46_entries_onto_2(v_r, v_rest)));
  }
  if (!t138) {
    u_no_case(s137);
  }
  return t138;
}

u_value f_map_46_size_1(u_value v_m) {
  u_value s139 = v_m;
  u_value t140 = NULL;
  if ((!t140) && u_is_con(s139, &n_map_46_MLeaf)) {
    t140 = u_mk_int(0);
  }
  if ((!t140) && u_is_con(s139, &n_map_46_MNode)) {
    u_value v_l = u_field(s139, 0);
    u_value v_r = u_field(s139, 3);
    t140 = u_add(u_add(f_map_46_size_1(v_l), u_mk_int(1)), f_map_46_size_1(v_r));
  }
  if (!t140) {
    u_no_case(s139);
  }
  return t140;
}

u_value f_map_46_height_1(u_value v_m) {
  u_value s141 = v_m;
  u_value t142 = NULL;
  if ((!t142) && u_is_con(s141, &n_map_46_MLeaf)) {
    t142 = u_mk_int(0);
  }
  if ((!t142) && u_is_con(s141, &n_map_46_MNode)) {
    u_value v_h = u_field(s141, 4);
    t142 = v_h;
  }
  if (!t142) {
    u_no_case(s141);
  }
  return t142;
}

u_value f_map_46_mnode_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r) {
  return u_mk_con(&n_map_46_MNode, 5, v_l, v_k, v_v, v_r, f_map_46_mnode_height_2(v_l, v_r));
}

u_value f_map_46_rotate_left_1(u_value v_m) {
  u_value s143 = v_m;
  u_value t144 = NULL;
  if ((!t144) && (u_is_con(s143, &n_map_46_MNode) && u_is_con(u_field(s143, 3), &n_map_46_MNode))) {
    u_value v_a = u_field(s143, 0);
    u_value v_j = u_field(s143, 1);
    u_value v_v = u_field(s143, 2);
    u_value v_b = u_field(u_field(s143, 3), 0);
    u_value v_k = u_field(u_field(s143, 3), 1);
    u_value v_w = u_field(u_field(s143, 3), 2);
    u_value v_c = u_field(u_field(s143, 3), 3);
    t144 = f_map_46_mnode_4(f_map_46_mnode_4(v_a, v_j, v_v, v_b), v_k, v_w, v_c);
  }
  if (!t144) {
    t144 = v_m;
  }
  if (!t144) {
    u_no_case(s143);
  }
  return t144;
}

u_value f_map_46_rotate_right_1(u_value v_m) {
  u_value s145 = v_m;
  u_value t146 = NULL;
  if ((!t146) && (u_is_con(s145, &n_map_46_MNode) && u_is_con(u_field(s145, 0), &n_map_46_MNode))) {
    u_value v_a = u_field(u_field(s145, 0), 0);
    u_value v_j = u_field(u_field(s145, 0), 1);
    u_value v_v = u_field(u_field(s145, 0), 2);
    u_value v_b = u_field(u_field(s145, 0), 3);
    u_value v_k = u_field(s145, 1);
    u_value v_w = u_field(s145, 2);
    u_value v_c = u_field(s145, 3);
    t146 = f_map_46_mnode_4(v_a, v_j, v_v, f_map_46_mnode_4(v_b, v_k, v_w, v_c));
  }
  if (!t146) {
    t146 = v_m;
  }
  if (!t146) {
    u_no_case(s145);
  }
  return t146;
}

u_value f_map_46_left_of_1(u_value v_m) {
  u_value s147 = v_m;
  u_value t148 = NULL;
  if ((!t148) && u_is_con(s147, &n_map_46_MNode)) {
    u_value v_l = u_field(s147, 0);
    t148 = v_l;
  }
  if (!t148) {
    t148 = v_m;
  }
  if (!t148) {
    u_no_case(s147);
  }
  return t148;
}

u_value f_map_46_right_of_1(u_value v_m) {
  u_value s149 = v_m;
  u_value t150 = NULL;
  if ((!t150) && u_is_con(s149, &n_map_46_MNode)) {
    u_value v_r = u_field(s149, 3);
    t150 = v_r;
  }
  if (!t150) {
    t150 = v_m;
  }
  if (!t150) {
    u_no_case(s149);
  }
  return t150;
}

u_value f_map_46_balance_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r) {
  u_value t154;
  if (u_truth(f_prelude_46__62__2(f_map_46_height_1(v_l), u_add(f_map_46_height_1(v_r), u_mk_int(1))))) {
    u_value t151;
    if (u_truth(u_lt(f_map_46_height_1(f_map_46_left_of_1(v_l)), f_map_46_height_1(f_map_46_right_of_1(v_l))))) {
      t151 = f_map_46_rotate_left_1(v_l);
    } else {
      t151 = v_l;
    }
    t154 = f_map_46_rotate_right_1(f_map_46_mnode_4(t151, v_k, v_v, v_r));
  } else {
    u_value t153;
    if (u_truth(f_prelude_46__62__2(f_map_46_height_1(v_r), u_add(f_map_46_height_1(v_l), u_mk_int(1))))) {
      u_value t152;
      if (u_truth(u_lt(f_map_46_height_1(f_map_46_right_of_1(v_r)), f_map_46_height_1(f_map_46_left_of_1(v_r))))) {
        t152 = f_map_46_rotate_right_1(v_r);
      } else {
        t152 = v_r;
      }
      t153 = f_map_46_rotate_left_1(f_map_46_mnode_4(v_l, v_k, v_v, t152));
    } else {
      t153 = f_map_46_mnode_4(v_l, v_k, v_v, v_r);
    }
    t154 = t153;
  }
  return t154;
}

u_value f_map_46_balanced_1(u_value v_m) {
  u_value s155 = v_m;
  u_value t156 = NULL;
  if ((!t156) && u_is_con(s155, &n_map_46_MLeaf)) {
    t156 = u_mk_bool(1);
  }
  if ((!t156) && u_is_con(s155, &n_map_46_MNode)) {
    u_value v_l = u_field(s155, 0);
    u_value v_r = u_field(s155, 3);
    u_value v_h = u_field(s155, 4);
    u_value t157 = f_map_46_balanced_1(v_l);
    if (u_truth(t157)) {
      t157 = f_map_46_balanced_1(v_r);
    }
    u_value t158 = t157;
    if (u_truth(t158)) {
      t158 = u_eq(v_h, f_map_46_mnode_height_2(v_l, v_r));
    }
    u_value t159 = t158;
    if (u_truth(t159)) {
      t159 = f_prelude_46__60__61__2(f_map_46_height_1(v_l), u_add(f_map_46_height_1(v_r), u_mk_int(1)));
    }
    u_value t160 = t159;
    if (u_truth(t160)) {
      t160 = f_prelude_46__60__61__2(f_map_46_height_1(v_r), u_add(f_map_46_height_1(v_l), u_mk_int(1)));
    }
    t156 = t160;
  }
  if (!t156) {
    u_no_case(s155);
  }
  return t156;
}

u_value f_map_46_mnode_height_2(u_value v_l, u_value v_r) {
  u_value t161;
  if (u_truth(u_lt(f_map_46_height_1(v_l), f_map_46_height_1(v_r)))) {
    t161 = f_map_46_height_1(v_r);
  } else {
    t161 = f_map_46_height_1(v_l);
  }
  return u_add(u_mk_int(1), t161);
}

u_value f_map_46_put_all_2(u_value v_ps, u_value v_m) {
  u_value s162 = v_ps;
  u_value t163 = NULL;
  if ((!t163) && u_is_con(s162, &n_list_46_Nil)) {
    t163 = v_m;
  }
  if ((!t163) && u_is_con(s162, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s162, 0), 0);
    u_value v_v = u_field(u_field(s162, 0), 1);
    u_value v_rest = u_field(s162, 1);
    t163 = f_map_46_put_all_2(v_rest, f_map_46_put_3(v_k, v_v, v_m));
  }
  if (!t163) {
    u_no_case(s162);
  }
  return t163;
}

u_value f_map_46_numbers_1(u_value v_n) {
  u_value t164;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t164 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t164 = f_list_46__43__43__2(f_map_46_numbers_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t164;
}

u_value f_ctx_46_Ctx_46_aliases_1(u_value v_x) {
  u_value s165 = v_x;
  u_value t166 = NULL;
  if ((!t166) && u_is_con(s165, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s165, 0);
    t166 = v_y;
  }
  if (!t166) {
    u_no_case(s165);
  }
  return t166;
}

u_value f_ctx_46_Ctx_46_funs_1(u_value v_x) {
  u_value s167 = v_x;
  u_value t168 = NULL;
  if ((!t168) && u_is_con(s167, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s167, 1);
    t168 = v_y;
  }
  if (!t168) {
    u_no_case(s167);
  }
  return t168;
}

u_value f_ctx_46_Ctx_46_procs_1(u_value v_x) {
  u_value s169 = v_x;
  u_value t170 = NULL;
  if ((!t170) && u_is_con(s169, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s169, 2);
    t170 = v_y;
  }
  if (!t170) {
    u_no_case(s169);
  }
  return t170;
}

u_value f_ctx_46_Ctx_46_datas_1(u_value v_x) {
  u_value s171 = v_x;
  u_value t172 = NULL;
  if ((!t172) && u_is_con(s171, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s171, 3);
    t172 = v_y;
  }
  if (!t172) {
    u_no_case(s171);
  }
  return t172;
}

u_value f_ctx_46_Ctx_46_members_1(u_value v_x) {
  u_value s173 = v_x;
  u_value t174 = NULL;
  if ((!t174) && u_is_con(s173, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s173, 4);
    t174 = v_y;
  }
  if (!t174) {
    u_no_case(s173);
  }
  return t174;
}

u_value f_ctx_46_Ctx_46_constrs_1(u_value v_x) {
  u_value s175 = v_x;
  u_value t176 = NULL;
  if ((!t176) && u_is_con(s175, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s175, 5);
    t176 = v_y;
  }
  if (!t176) {
    u_no_case(s175);
  }
  return t176;
}

u_value f_ctx_46_Ctx_46_named_1(u_value v_x) {
  u_value s177 = v_x;
  u_value t178 = NULL;
  if ((!t178) && u_is_con(s177, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s177, 6);
    t178 = v_y;
  }
  if (!t178) {
    u_no_case(s177);
  }
  return t178;
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
  u_value s179 = f_map_46_get_2(v_n, f_ctx_46_Ctx_46_funs_1(v_c));
  u_value t180 = NULL;
  if ((!t180) && u_is_con(s179, &n_option_46_Some)) {
    u_value v_ss = u_field(s179, 0);
    t180 = v_ss;
  }
  if ((!t180) && u_is_con(s179, &n_option_46_None)) {
    t180 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t180) {
    u_no_case(s179);
  }
  return t180;
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
  u_value t181 = f_ctx_46_is_data_2(v_c, v_n);
  if (!u_truth(t181)) {
    t181 = f_ctx_46_is_alias_2(v_c, v_n);
  }
  return t181;
}

u_value f_ctx_46_names_of_2(u_value v_c, u_value v_b) {
  u_value s182 = f_map_46_get_2(v_b, f_ctx_46_Ctx_46_named_1(v_c));
  u_value t183 = NULL;
  if ((!t183) && u_is_con(s182, &n_option_46_Some)) {
    u_value v_ns = u_field(s182, 0);
    t183 = v_ns;
  }
  if ((!t183) && u_is_con(s182, &n_option_46_None)) {
    t183 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t183) {
    u_no_case(s182);
  }
  return t183;
}

u_value f_ctx_46_with_named_2(u_value v_bs, u_value v_n) {
  u_value s184 = f_map_46_get_2(f_syntax_46_Name_46_base_1(v_n), v_bs);
  u_value t185 = NULL;
  if ((!t185) && u_is_con(s184, &n_option_46_Some)) {
    u_value v_ns = u_field(s184, 0);
    u_value t186;
    if (u_truth(f_list_46_contains_2(v_n, v_ns))) {
      t186 = v_bs;
    } else {
      t186 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), f_list_46__43__43__2(v_ns, u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0))), v_bs);
    }
    t185 = t186;
  }
  if ((!t185) && u_is_con(s184, &n_option_46_None)) {
    t185 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_bs);
  }
  if (!t185) {
    u_no_case(s184);
  }
  return t185;
}

u_value f_ctx_46_with_alias_4(u_value v_c, u_value v_n, u_value v_ps, u_value v_t) {
  u_value s187 = v_c;
  u_value t188 = NULL;
  if ((!t188) && u_is_con(s187, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s187, 0);
    u_value v_f = u_field(s187, 1);
    u_value v_p = u_field(s187, 2);
    u_value v_d = u_field(s187, 3);
    u_value v_m = u_field(s187, 4);
    u_value v_k = u_field(s187, 5);
    u_value v_bs = u_field(s187, 6);
    t188 = u_mk_con(&n_ctx_46_Ctx, 7, f_map_46_put_3(v_n, u_mk_tuple(2, v_ps, v_t), v_a), v_f, v_p, v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t188) {
    u_no_case(s187);
  }
  return t188;
}

u_value f_ctx_46_with_fun_3(u_value v_c, u_value v_n, u_value v_sc) {
  u_value s189 = v_c;
  u_value t190 = NULL;
  if ((!t190) && u_is_con(s189, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s189, 0);
    u_value v_f = u_field(s189, 1);
    u_value v_p = u_field(s189, 2);
    u_value v_d = u_field(s189, 3);
    u_value v_m = u_field(s189, 4);
    u_value v_k = u_field(s189, 5);
    u_value v_bs = u_field(s189, 6);
    t190 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, f_map_46_put_3(v_n, f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), u_mk_con(&n_list_46_Cons, 2, v_sc, u_mk_con(&n_list_46_Nil, 0))), v_f), v_p, v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t190) {
    u_no_case(s189);
  }
  return t190;
}

u_value f_ctx_46_with_proc_3(u_value v_c, u_value v_n, u_value v_g) {
  u_value s191 = v_c;
  u_value t192 = NULL;
  if ((!t192) && u_is_con(s191, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s191, 0);
    u_value v_f = u_field(s191, 1);
    u_value v_p = u_field(s191, 2);
    u_value v_d = u_field(s191, 3);
    u_value v_m = u_field(s191, 4);
    u_value v_k = u_field(s191, 5);
    u_value v_bs = u_field(s191, 6);
    t192 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, f_map_46_put_3(v_n, v_g, v_p), v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t192) {
    u_no_case(s191);
  }
  return t192;
}

u_value f_ctx_46_with_data_3(u_value v_c, u_value v_n, u_value v_arity) {
  u_value s193 = v_c;
  u_value t194 = NULL;
  if ((!t194) && u_is_con(s193, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s193, 0);
    u_value v_f = u_field(s193, 1);
    u_value v_p = u_field(s193, 2);
    u_value v_d = u_field(s193, 3);
    u_value v_m = u_field(s193, 4);
    u_value v_k = u_field(s193, 5);
    u_value v_bs = u_field(s193, 6);
    t194 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, f_map_46_put_3(v_n, v_arity, v_d), v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t194) {
    u_no_case(s193);
  }
  return t194;
}

u_value f_ctx_46_with_members_3(u_value v_c, u_value v_n, u_value v_cs) {
  u_value s195 = v_c;
  u_value t196 = NULL;
  if ((!t196) && u_is_con(s195, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s195, 0);
    u_value v_f = u_field(s195, 1);
    u_value v_p = u_field(s195, 2);
    u_value v_d = u_field(s195, 3);
    u_value v_m = u_field(s195, 4);
    u_value v_k = u_field(s195, 5);
    u_value v_bs = u_field(s195, 6);
    t196 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, v_d, f_map_46_put_3(v_n, v_cs, v_m), v_k, v_bs);
  }
  if (!t196) {
    u_no_case(s195);
  }
  return t196;
}

u_value f_ctx_46_with_constr_3(u_value v_c, u_value v_n, u_value v_kc) {
  u_value s197 = v_c;
  u_value t198 = NULL;
  if ((!t198) && u_is_con(s197, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s197, 0);
    u_value v_f = u_field(s197, 1);
    u_value v_p = u_field(s197, 2);
    u_value v_d = u_field(s197, 3);
    u_value v_m = u_field(s197, 4);
    u_value v_k = u_field(s197, 5);
    u_value v_bs = u_field(s197, 6);
    t198 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, v_d, v_m, f_map_46_put_3(v_n, v_kc, v_k), f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t198) {
    u_no_case(s197);
  }
  return t198;
}

u_value f_check_46_State_46_ctx_1(u_value v_x) {
  u_value s199 = v_x;
  u_value t200 = NULL;
  if ((!t200) && u_is_con(s199, &n_check_46_State)) {
    u_value v_y = u_field(s199, 0);
    t200 = v_y;
  }
  if (!t200) {
    u_no_case(s199);
  }
  return t200;
}

u_value f_check_46_State_46_sub_1(u_value v_x) {
  u_value s201 = v_x;
  u_value t202 = NULL;
  if ((!t202) && u_is_con(s201, &n_check_46_State)) {
    u_value v_y = u_field(s201, 1);
    t202 = v_y;
  }
  if (!t202) {
    u_no_case(s201);
  }
  return t202;
}

u_value f_check_46_State_46_count_1(u_value v_x) {
  u_value s203 = v_x;
  u_value t204 = NULL;
  if ((!t204) && u_is_con(s203, &n_check_46_State)) {
    u_value v_y = u_field(s203, 2);
    t204 = v_y;
  }
  if (!t204) {
    u_no_case(s203);
  }
  return t204;
}

u_value f_check_46_State_46_module_1(u_value v_x) {
  u_value s205 = v_x;
  u_value t206 = NULL;
  if ((!t206) && u_is_con(s205, &n_check_46_State)) {
    u_value v_y = u_field(s205, 3);
    t206 = v_y;
  }
  if (!t206) {
    u_no_case(s205);
  }
  return t206;
}

u_value f_check_46_State_46_visible_1(u_value v_x) {
  u_value s207 = v_x;
  u_value t208 = NULL;
  if ((!t208) && u_is_con(s207, &n_check_46_State)) {
    u_value v_y = u_field(s207, 4);
    t208 = v_y;
  }
  if (!t208) {
    u_no_case(s207);
  }
  return t208;
}

u_value f_check_46_State_46_table_1(u_value v_x) {
  u_value s209 = v_x;
  u_value t210 = NULL;
  if ((!t210) && u_is_con(s209, &n_check_46_State)) {
    u_value v_y = u_field(s209, 5);
    t210 = v_y;
  }
  if (!t210) {
    u_no_case(s209);
  }
  return t210;
}

u_value f_check_46_with_ctx_2(u_value v_s, u_value v_c) {
  u_value s211 = v_s;
  u_value t212 = NULL;
  if ((!t212) && u_is_con(s211, &n_check_46_State)) {
    u_value v_su = u_field(s211, 1);
    u_value v_n = u_field(s211, 2);
    u_value v_m = u_field(s211, 3);
    u_value v_ms = u_field(s211, 4);
    u_value v_vs = u_field(s211, 5);
    t212 = u_mk_con(&n_check_46_State, 6, v_c, v_su, v_n, v_m, v_ms, v_vs);
  }
  if (!t212) {
    u_no_case(s211);
  }
  return t212;
}

u_value f_check_46_enter_2(u_value v_s, u_value v_m) {
  u_value s213 = v_s;
  u_value t214 = NULL;
  if ((!t214) && u_is_con(s213, &n_check_46_State)) {
    u_value v_c = u_field(s213, 0);
    u_value v_vs = u_field(s213, 5);
    t214 = u_mk_con(&n_check_46_State, 6, v_c, f_map_46_empty_0(), u_mk_int(0), v_m, f_check_46_sees_2(v_m, v_vs), v_vs);
  }
  if (!t214) {
    u_no_case(s213);
  }
  return t214;
}

u_value f_check_46_sees_2(u_value v_m, u_value v_vs) {
  u_value s215 = f_list_46_lookup_2(v_m, v_vs);
  u_value t216 = NULL;
  if ((!t216) && u_is_con(s215, &n_option_46_Some)) {
    u_value v_ms = u_field(s215, 0);
    t216 = v_ms;
  }
  if ((!t216) && u_is_con(s215, &n_option_46_None)) {
    t216 = u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t216) {
    u_no_case(s215);
  }
  return t216;
}

u_value f_check_46_start_2(u_value v_main, u_value v_vs) {
  return u_mk_con(&n_check_46_State, 6, f_check_46_initial_0(), f_map_46_empty_0(), u_mk_int(0), v_main, f_check_46_sees_2(v_main, v_vs), v_vs);
}

u_value f_check_46_flatten_2(u_value v_m, u_value v_cs) {
  u_value s217 = v_cs;
  u_value t218 = NULL;
  if ((!t218) && u_is_con(s217, &n_list_46_Nil)) {
    t218 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t218) && (u_is_con(s217, &n_list_46_Cons) && u_is_con(u_field(s217, 0), &n_syntax_46_CModule))) {
    u_value v_n = u_field(u_field(s217, 0), 0);
    u_value v_ds = u_field(u_field(s217, 0), 1);
    u_value v_rest = u_field(s217, 1);
    t218 = f_list_46__43__43__2(f_check_46_flatten_2(v_n, v_ds), f_check_46_flatten_2(v_m, v_rest));
  }
  if ((!t218) && u_is_con(s217, &n_list_46_Cons)) {
    u_value v_c = u_field(s217, 0);
    u_value v_rest = u_field(s217, 1);
    t218 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_m, f_check_46_qualify_2(v_m, v_c)), f_check_46_flatten_2(v_m, v_rest));
  }
  if (!t218) {
    u_no_case(s217);
  }
  return t218;
}

u_value f_check_46_in_module_2(u_value v_m, u_value v_n) {
  return u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), f_syntax_46_Name_46_base_1(v_n));
}

u_value f_check_46_qualify_2(u_value v_m, u_value v_c) {
  u_value s219 = v_c;
  u_value t220 = NULL;
  if ((!t220) && u_is_con(s219, &n_syntax_46_CSort)) {
    u_value v_n = u_field(s219, 0);
    u_value v_ps = u_field(s219, 1);
    u_value v_t = u_field(s219, 2);
    t220 = u_mk_con(&n_syntax_46_CSort, 3, f_check_46_in_module_2(v_m, v_n), v_ps, v_t);
  }
  if ((!t220) && u_is_con(s219, &n_syntax_46_CData)) {
    u_value v_n = u_field(s219, 0);
    u_value v_ps = u_field(s219, 1);
    u_value v_ks = u_field(s219, 2);
    u_value t221 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e222 = &t221;
    u_value l223 = v_ks;
    while (u_is_con(l223, &n_list_46_Cons)) {
      u_value v_k = u_field(l223, 0);
      if (u_truth(u_mk_bool(1))) {
        e222 = u_append(e222, f_check_46_qualify_constr_2(v_m, v_k));
      }
      l223 = u_field(l223, 1);
    }
    t220 = u_mk_con(&n_syntax_46_CData, 3, f_check_46_in_module_2(v_m, v_n), v_ps, t221);
  }
  if ((!t220) && u_is_con(s219, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s219, 0);
    u_value v_ps = u_field(s219, 1);
    u_value v_xss = u_field(s219, 2);
    u_value v_r = u_field(s219, 3);
    u_value v_b = u_field(s219, 4);
    t220 = u_mk_con(&n_syntax_46_CFun, 5, f_check_46_in_module_2(v_m, v_n), v_ps, v_xss, v_r, v_b);
  }
  if ((!t220) && u_is_con(s219, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s219, 0);
    u_value v_ps = u_field(s219, 1);
    u_value v_ins = u_field(s219, 2);
    u_value v_outs = u_field(s219, 3);
    u_value v_refs = u_field(s219, 4);
    u_value v_b = u_field(s219, 5);
    t220 = u_mk_con(&n_syntax_46_CProc, 6, f_check_46_in_module_2(v_m, v_n), v_ps, v_ins, v_outs, v_refs, v_b);
  }
  if (!t220) {
    t220 = v_c;
  }
  if (!t220) {
    u_no_case(s219);
  }
  return t220;
}

u_value f_check_46_qualify_constr_2(u_value v_m, u_value v_k) {
  u_value s224 = v_k;
  u_value t225 = NULL;
  if ((!t225) && u_is_con(s224, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s224, 0);
    u_value v_ts = u_field(s224, 1);
    t225 = u_mk_con(&n_syntax_46_Constr, 2, f_check_46_in_module_2(v_m, v_n), v_ts);
  }
  if (!t225) {
    u_no_case(s224);
  }
  return t225;
}

u_value f_check_46_imports_in_2(u_value v_ds, u_value v_m) {
  u_value s226 = v_ds;
  u_value t227 = NULL;
  if ((!t227) && u_is_con(s226, &n_list_46_Nil)) {
    t227 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t227) && (u_is_con(s226, &n_list_46_Cons) && u_is_con(u_field(u_field(s226, 0), 1), &n_syntax_46_CImport))) {
    u_value v_k = u_field(u_field(s226, 0), 0);
    u_value v_n = u_field(u_field(u_field(s226, 0), 1), 0);
    u_value v_rest = u_field(s226, 1);
    if (u_truth(u_eq(v_k, v_m))) {
      t227 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_imports_in_2(v_rest, v_m));
    }
  }
  if ((!t227) && u_is_con(s226, &n_list_46_Cons)) {
    u_value v_rest = u_field(s226, 1);
    t227 = f_check_46_imports_in_2(v_rest, v_m);
  }
  if (!t227) {
    u_no_case(s226);
  }
  return t227;
}

u_value f_check_46_visibility_2(u_value v_ds, u_value v_main) {
  u_value t228 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e229 = &t228;
  u_value l230 = f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_main, u_mk_con(&n_list_46_Nil, 0)), f_list_46_firsts_1(v_ds)));
  while (u_is_con(l230, &n_list_46_Cons)) {
    u_value v_m = u_field(l230, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_prelude_231 = U_LIT("prelude");
      static struct u_object l_prelude_232 = U_LIT("prelude");
      e229 = u_append(e229, u_mk_tuple(2, v_m, f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_check_46_imports_in_2(v_ds, v_m), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, &l_prelude_231, u_mk_con(&n_list_46_Nil, 0)), f_check_46_imports_in_2(v_ds, &l_prelude_232)))))));
    }
    l230 = u_field(l230, 1);
  }
  return t228;
}

u_value f_check_46_visible_names_3(u_value v_s, u_value v_n, u_value v_declared) {
  u_value t233 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e234 = &t233;
  u_value l235 = f_ctx_46_names_of_2(f_check_46_State_46_ctx_1(v_s), f_syntax_46_Name_46_base_1(v_n));
  while (u_is_con(l235, &n_list_46_Cons)) {
    u_value v_d = u_field(l235, 0);
    if (u_truth(u_apply(v_declared, 1, v_d))) {
      e234 = u_append(e234, v_d);
    }
    l235 = u_field(l235, 1);
  }
  u_value t244 = t233;
  u_value t243;
  {
    u_value v_ds = t244;
    u_value s236 = v_n;
    u_value t237 = NULL;
    if ((!t237) && (u_is_con(s236, &n_syntax_46_Name) && u_is_con(u_field(s236, 0), &n_option_46_Some))) {
      u_value t238;
      if (u_truth(f_list_46_contains_2(v_n, v_ds))) {
        t238 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
      } else {
        t238 = u_mk_con(&n_list_46_Nil, 0);
      }
      t237 = t238;
    }
    if ((!t237) && (u_is_con(s236, &n_syntax_46_Name) && u_is_con(u_field(s236, 0), &n_option_46_None))) {
      u_value v_b = u_field(s236, 1);
      u_value t242;
      if (u_truth(f_list_46_contains_2(u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), v_ds))) {
        t242 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), u_mk_con(&n_list_46_Nil, 0));
      } else {
        u_value t239 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e240 = &t239;
        u_value l241 = v_ds;
        while (u_is_con(l241, &n_list_46_Cons)) {
          u_value v_d = u_field(l241, 0);
          if (u_truth(f_check_46_is_visible_2(v_s, v_d))) {
            e240 = u_append(e240, v_d);
          }
          l241 = u_field(l241, 1);
        }
        t242 = t239;
      }
      t237 = t242;
    }
    if (!t237) {
      u_no_case(s236);
    }
    t243 = t237;
  }
  return t243;
}

u_value f_check_46_is_visible_2(u_value v_s, u_value v_d) {
  u_value s245 = f_syntax_46_Name_46_module_1(v_d);
  u_value t246 = NULL;
  if ((!t246) && u_is_con(s245, &n_option_46_Some)) {
    u_value v_m = u_field(s245, 0);
    t246 = f_list_46_contains_2(v_m, f_check_46_State_46_visible_1(v_s));
  }
  if ((!t246) && u_is_con(s245, &n_option_46_None)) {
    t246 = u_mk_bool(0);
  }
  if (!t246) {
    u_no_case(s245);
  }
  return t246;
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
  static struct u_object l_Bool_247 = U_LIT("Bool");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Bool_247), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Int_248 = U_LIT("Int");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Int_248), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Str_249 = U_LIT("Str");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Str_249), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Char_250 = U_LIT("Char");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Char_250), u_mk_con(&n_list_46_Nil, 0));
  return cache;
}

u_value f_check_46_tlist_1(u_value v_t) {
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_list_name_0(), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_check_46_fun_2(u_value v_args, u_value v_res) {
  return u_mk_con(&n_syntax_46_TFun, 2, v_args, v_res);
}

u_value f_check_46_params_1(u_value v_ps) {
  u_value t251 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e252 = &t251;
  u_value l253 = v_ps;
  while (u_is_con(l253, &n_list_46_Cons)) {
    u_value v_p = u_field(l253, 0);
    if (u_truth(u_mk_bool(1))) {
      e252 = u_append(e252, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l253 = u_field(l253, 1);
  }
  return t251;
}

u_value f_check_46_basic_1(u_value v_n) {
  u_value s254 = v_n;
  u_value t255 = NULL;
  if ((!t255) && (u_is_con(s254, &n_syntax_46_Name) && u_is_con(u_field(s254, 0), &n_option_46_None))) {
    u_value v_b = u_field(s254, 1);
    static struct u_object l_Bool_256 = U_LIT("Bool");
    static struct u_object l_Int_257 = U_LIT("Int");
    static struct u_object l_Str_258 = U_LIT("Str");
    static struct u_object l_Char_259 = U_LIT("Char");
    t255 = f_list_46_contains_2(v_b, u_mk_con(&n_list_46_Cons, 2, &l_Bool_256, u_mk_con(&n_list_46_Cons, 2, &l_Int_257, u_mk_con(&n_list_46_Cons, 2, &l_Str_258, u_mk_con(&n_list_46_Cons, 2, &l_Char_259, u_mk_con(&n_list_46_Nil, 0))))));
  }
  if (!t255) {
    t255 = u_mk_bool(0);
  }
  if (!t255) {
    u_no_case(s254);
  }
  return t255;
}

u_value f_check_46_literal_1(u_value v_v) {
  u_value s260 = v_v;
  u_value t261 = NULL;
  if ((!t261) && u_is_con(s260, &n_syntax_46_VUnit)) {
    t261 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t261) && u_is_con(s260, &n_syntax_46_VBool)) {
    t261 = f_check_46_tbool_0();
  }
  if ((!t261) && u_is_con(s260, &n_syntax_46_VInt)) {
    t261 = f_check_46_tint_0();
  }
  if ((!t261) && u_is_con(s260, &n_syntax_46_VStr)) {
    t261 = f_check_46_tstr_0();
  }
  if ((!t261) && u_is_con(s260, &n_syntax_46_VChar)) {
    t261 = f_check_46_tchar_0();
  }
  if (!t261) {
    u_no_case(s260);
  }
  return t261;
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
  u_value s262 = v_n;
  u_value t263 = NULL;
  if ((!t263) && (u_is_con(s262, &n_syntax_46_Name) && u_is_con(u_field(s262, 0), &n_option_46_None))) {
    u_value v_b = u_field(s262, 1);
    t263 = f_check_46_builtin_scheme_1(v_b);
  }
  if (!t263) {
    t263 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t263) {
    u_no_case(s262);
  }
  return t263;
}

u_value f_check_46_builtin_scheme_1(u_value v_b) {
  static struct u_object l_A_264 = U_LIT("A");
  u_value t268 = u_mk_con(&n_syntax_46_TParam, 1, &l_A_264);
  u_value t267;
  {
    u_value v_a = t268;
    u_value s265 = v_b;
    u_value t266 = NULL;
    if ((!t266) && u_equals(s265, u_mk_lit("+"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("-"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("*"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("/"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("%"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("<"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("=="))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("compare"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("and"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("or"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("chars"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tlist_1(f_check_46_tchar_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("string"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tchar_0()), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("ord"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("chr"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("^"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("str_size"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("str_join"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("str_slice"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("str_split"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tlist_1(f_check_46_tstr_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("str_get"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("str_contains"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("str_compare"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("str_show"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("str_quote"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("str_number"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_option_name_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("is_digit"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("is_letter"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t266) && u_equals(s265, u_mk_lit("is_space"))) {
      t266 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if (!t266) {
      t266 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t266) {
      u_no_case(s265);
    }
    t267 = t266;
  }
  return t267;
}

u_value f_check_46_is_meta_1(u_value v_t) {
  u_value s269 = v_t;
  u_value t270 = NULL;
  if ((!t270) && u_is_con(s269, &n_syntax_46_TMeta)) {
    t270 = u_mk_bool(1);
  }
  if (!t270) {
    t270 = u_mk_bool(0);
  }
  if (!t270) {
    u_no_case(s269);
  }
  return t270;
}

u_value f_check_46_free_1(u_value v_t) {
  u_value s271 = v_t;
  u_value t272 = NULL;
  if ((!t272) && u_is_con(s271, &n_syntax_46_TParam)) {
    t272 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t272) && u_is_con(s271, &n_syntax_46_TMeta)) {
    t272 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t272) && u_is_con(s271, &n_syntax_46_THole)) {
    t272 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t272) && u_is_con(s271, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s271, 1);
    u_value t273 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e274 = &t273;
    u_value l275 = v_args;
    while (u_is_con(l275, &n_list_46_Cons)) {
      u_value v_a = u_field(l275, 0);
      if (u_truth(u_mk_bool(1))) {
        e274 = u_append(e274, f_check_46_free_1(v_a));
      }
      l275 = u_field(l275, 1);
    }
    t272 = f_list_46_distinct_1(f_list_46_concat_1(t273));
  }
  if ((!t272) && u_is_con(s271, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s271, 0);
    u_value v_r = u_field(s271, 1);
    u_value t276 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e277 = &t276;
    u_value l278 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l278, &n_list_46_Cons)) {
      u_value v_a = u_field(l278, 0);
      if (u_truth(u_mk_bool(1))) {
        e277 = u_append(e277, f_check_46_free_1(v_a));
      }
      l278 = u_field(l278, 1);
    }
    t272 = f_list_46_distinct_1(f_list_46_concat_1(t276));
  }
  if ((!t272) && u_is_con(s271, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s271, 0);
    u_value t279 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e280 = &t279;
    u_value l281 = v_ts;
    while (u_is_con(l281, &n_list_46_Cons)) {
      u_value v_a = u_field(l281, 0);
      if (u_truth(u_mk_bool(1))) {
        e280 = u_append(e280, f_check_46_free_1(v_a));
      }
      l281 = u_field(l281, 1);
    }
    t272 = f_list_46_distinct_1(f_list_46_concat_1(t279));
  }
  if (!t272) {
    u_no_case(s271);
  }
  return t272;
}

u_value f_check_46_holes_1(u_value v_t) {
  u_value s282 = v_t;
  u_value t283 = NULL;
  if ((!t283) && u_is_con(s282, &n_syntax_46_THole)) {
    t283 = u_mk_bool(1);
  }
  if ((!t283) && u_is_con(s282, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s282, 1);
    u_value t284 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e285 = &t284;
    u_value l286 = v_args;
    while (u_is_con(l286, &n_list_46_Cons)) {
      u_value v_a = u_field(l286, 0);
      if (u_truth(u_mk_bool(1))) {
        e285 = u_append(e285, f_check_46_holes_1(v_a));
      }
      l286 = u_field(l286, 1);
    }
    t283 = f_list_46_contains_2(u_mk_bool(1), t284);
  }
  if ((!t283) && u_is_con(s282, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s282, 0);
    u_value v_r = u_field(s282, 1);
    u_value t287 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e288 = &t287;
    u_value l289 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l289, &n_list_46_Cons)) {
      u_value v_a = u_field(l289, 0);
      if (u_truth(u_mk_bool(1))) {
        e288 = u_append(e288, f_check_46_holes_1(v_a));
      }
      l289 = u_field(l289, 1);
    }
    t283 = f_list_46_contains_2(u_mk_bool(1), t287);
  }
  if ((!t283) && u_is_con(s282, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s282, 0);
    u_value t290 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e291 = &t290;
    u_value l292 = v_ts;
    while (u_is_con(l292, &n_list_46_Cons)) {
      u_value v_a = u_field(l292, 0);
      if (u_truth(u_mk_bool(1))) {
        e291 = u_append(e291, f_check_46_holes_1(v_a));
      }
      l292 = u_field(l292, 1);
    }
    t283 = f_list_46_contains_2(u_mk_bool(1), t290);
  }
  if (!t283) {
    t283 = u_mk_bool(0);
  }
  if (!t283) {
    u_no_case(s282);
  }
  return t283;
}

u_value f_check_46_subst_2(u_value v_t, u_value v_m) {
  u_value s293 = v_t;
  u_value t294 = NULL;
  if ((!t294) && u_is_con(s293, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s293, 0);
    u_value v_args = u_field(s293, 1);
    u_value t295 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e296 = &t295;
    u_value l297 = v_args;
    while (u_is_con(l297, &n_list_46_Cons)) {
      u_value v_a = u_field(l297, 0);
      if (u_truth(u_mk_bool(1))) {
        e296 = u_append(e296, f_check_46_subst_2(v_a, v_m));
      }
      l297 = u_field(l297, 1);
    }
    t294 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t295);
  }
  if ((!t294) && u_is_con(s293, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s293, 0);
    u_value v_r = u_field(s293, 1);
    u_value t298 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e299 = &t298;
    u_value l300 = v_args;
    while (u_is_con(l300, &n_list_46_Cons)) {
      u_value v_a = u_field(l300, 0);
      if (u_truth(u_mk_bool(1))) {
        e299 = u_append(e299, f_check_46_subst_2(v_a, v_m));
      }
      l300 = u_field(l300, 1);
    }
    t294 = u_mk_con(&n_syntax_46_TFun, 2, t298, f_check_46_subst_2(v_r, v_m));
  }
  if ((!t294) && u_is_con(s293, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s293, 0);
    u_value t301 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e302 = &t301;
    u_value l303 = v_ts;
    while (u_is_con(l303, &n_list_46_Cons)) {
      u_value v_a = u_field(l303, 0);
      if (u_truth(u_mk_bool(1))) {
        e302 = u_append(e302, f_check_46_subst_2(v_a, v_m));
      }
      l303 = u_field(l303, 1);
    }
    t294 = u_mk_con(&n_syntax_46_TTuple, 1, t301);
  }
  if (!t294) {
    u_value s304 = f_list_46_lookup_2(v_t, v_m);
    u_value t305 = NULL;
    if ((!t305) && u_is_con(s304, &n_option_46_Some)) {
      u_value v_u = u_field(s304, 0);
      t305 = v_u;
    }
    if ((!t305) && u_is_con(s304, &n_option_46_None)) {
      t305 = v_t;
    }
    if (!t305) {
      u_no_case(s304);
    }
    t294 = t305;
  }
  if (!t294) {
    u_no_case(s293);
  }
  return t294;
}

u_value f_check_46_resolve_2(u_value v_su, u_value v_t) {
  u_value s306 = v_t;
  u_value t307 = NULL;
  if ((!t307) && u_is_con(s306, &n_syntax_46_TMeta)) {
    u_value v_i = u_field(s306, 0);
    u_value s308 = f_map_46_get_2(v_i, v_su);
    u_value t309 = NULL;
    if ((!t309) && u_is_con(s308, &n_option_46_Some)) {
      u_value v_u = u_field(s308, 0);
      t309 = f_check_46_resolve_2(v_su, v_u);
    }
    if ((!t309) && u_is_con(s308, &n_option_46_None)) {
      t309 = v_t;
    }
    if (!t309) {
      u_no_case(s308);
    }
    t307 = t309;
  }
  if (!t307) {
    t307 = v_t;
  }
  if (!t307) {
    u_no_case(s306);
  }
  return t307;
}

u_value f_check_46_occurs_3(u_value v_su, u_value v_i, u_value v_t) {
  u_value s310 = f_check_46_resolve_2(v_su, v_t);
  u_value t311 = NULL;
  if ((!t311) && u_is_con(s310, &n_syntax_46_TMeta)) {
    u_value v_j = u_field(s310, 0);
    t311 = u_eq(v_i, v_j);
  }
  if ((!t311) && u_is_con(s310, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s310, 1);
    t311 = f_list_46_exists_2(v_args, u_mk_closure(lam312, 2, v_su, v_i));
  }
  if ((!t311) && u_is_con(s310, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s310, 0);
    u_value v_r = u_field(s310, 1);
    u_value t314 = f_list_46_exists_2(v_args, u_mk_closure(lam313, 2, v_su, v_i));
    if (!u_truth(t314)) {
      t314 = f_check_46_occurs_3(v_su, v_i, v_r);
    }
    t311 = t314;
  }
  if ((!t311) && u_is_con(s310, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s310, 0);
    t311 = f_list_46_exists_2(v_ts, u_mk_closure(lam315, 2, v_su, v_i));
  }
  if (!t311) {
    t311 = u_mk_bool(0);
  }
  if (!t311) {
    u_no_case(s310);
  }
  return t311;
}

u_value f_check_46_constructor_scheme_1(u_value v_k) {
  u_value s316 = v_k;
  u_value t317 = NULL;
  if (!t317) {
    u_value v_ps = u_field(s316, 0);
    u_value v_ks = u_field(s316, 1);
    u_value v_r = u_field(s316, 2);
    u_value t318;
    if (u_truth(u_eq(v_ks, u_mk_con(&n_list_46_Nil, 0)))) {
      t318 = v_r;
    } else {
      t318 = f_check_46_fun_2(v_ks, v_r);
    }
    t317 = u_mk_tuple(2, v_ps, t318);
  }
  if (!t317) {
    u_no_case(s316);
  }
  return t317;
}

u_value f_check_46_constructor_of_2(u_value v_c, u_value v_k) {
  u_value s319 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t320 = NULL;
  if ((!t320) && u_is_con(s319, &n_option_46_Some)) {
    u_value v_kc = u_field(s319, 0);
    t320 = f_check_46_constructor_scheme_1(v_kc);
  }
  if ((!t320) && u_is_con(s319, &n_option_46_None)) {
    t320 = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t320) {
    u_no_case(s319);
  }
  return t320;
}

u_value f_check_46_candidates_3(u_value v_s, u_value v_env, u_value v_n) {
  u_value s321 = f_list_46_lookup_2(v_n, v_env);
  u_value t322 = NULL;
  if ((!t322) && u_is_con(s321, &n_option_46_Some)) {
    u_value v_sc = u_field(s321, 0);
    t322 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, v_sc), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t322) && u_is_con(s321, &n_option_46_None)) {
    u_value t323 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e324 = &t323;
    u_value l326 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam325, 1, v_s));
    while (u_is_con(l326, &n_list_46_Cons)) {
      u_value v_f = u_field(l326, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t327 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e328 = &t327;
        u_value l329 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(v_s), v_f);
        while (u_is_con(l329, &n_list_46_Cons)) {
          u_value v_sc = u_field(l329, 0);
          if (u_truth(u_mk_bool(1))) {
            e328 = u_append(e328, u_mk_tuple(2, v_f, v_sc));
          }
          l329 = u_field(l329, 1);
        }
        e324 = u_append(e324, t327);
      }
      l326 = u_field(l326, 1);
    }
    u_value t330 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e331 = &t330;
    u_value l333 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam332, 1, v_s));
    while (u_is_con(l333, &n_list_46_Cons)) {
      u_value v_k = u_field(l333, 0);
      if (u_truth(u_mk_bool(1))) {
        e331 = u_append(e331, u_mk_tuple(2, v_k, f_check_46_constructor_of_2(f_check_46_State_46_ctx_1(v_s), v_k)));
      }
      l333 = u_field(l333, 1);
    }
    u_value t334 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e335 = &t334;
    u_value l336 = f_check_46_builtin_1(v_n);
    while (u_is_con(l336, &n_list_46_Cons)) {
      u_value v_sc = u_field(l336, 0);
      if (u_truth(u_mk_bool(1))) {
        e335 = u_append(e335, u_mk_tuple(2, v_n, v_sc));
      }
      l336 = u_field(l336, 1);
    }
    t322 = f_list_46__43__43__2(f_list_46_concat_1(t323), f_list_46__43__43__2(t330, t334));
  }
  if (!t322) {
    u_no_case(s321);
  }
  return t322;
}

u_value f_check_46_with_arity_2(u_value v_cs, u_value v_k) {
  u_value s337 = v_cs;
  u_value t338 = NULL;
  if ((!t338) && u_is_con(s337, &n_list_46_Nil)) {
    t338 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t338) && u_is_con(s337, &n_list_46_Cons)) {
    u_value v_n = u_field(u_field(s337, 0), 0);
    u_value v_ps = u_field(u_field(u_field(s337, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s337, 0), 1), 1);
    u_value v_rest = u_field(s337, 1);
    u_value t339;
    if (u_truth(u_eq(f_syntax_46_arity_1(v_t), v_k))) {
      t339 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, v_ps, v_t)), f_check_46_with_arity_2(v_rest, v_k));
    } else {
      t339 = f_check_46_with_arity_2(v_rest, v_k);
    }
    t338 = t339;
  }
  if (!t338) {
    u_no_case(s337);
  }
  return t338;
}

u_value f_check_46_has_arity_2(u_value v_scs, u_value v_k) {
  u_value s340 = v_scs;
  u_value t341 = NULL;
  if ((!t341) && u_is_con(s340, &n_list_46_Nil)) {
    t341 = u_mk_bool(0);
  }
  if ((!t341) && u_is_con(s340, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s340, 0), 1);
    u_value v_rest = u_field(s340, 1);
    u_value t342 = u_eq(f_syntax_46_arity_1(v_t), v_k);
    if (!u_truth(t342)) {
      t342 = f_check_46_has_arity_2(v_rest, v_k);
    }
    t341 = t342;
  }
  if (!t341) {
    u_no_case(s340);
  }
  return t341;
}

u_value f_check_46_bind_1(u_value v_xs) {
  u_value t343 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e344 = &t343;
  u_value l345 = v_xs;
  while (u_is_con(l345, &n_list_46_Cons)) {
    u_value v_x = u_field(l345, 0);
    if (u_truth(u_mk_bool(1))) {
      e344 = u_append(e344, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_syntax_46_Var_46_typ_1(v_x))));
    }
    l345 = u_field(l345, 1);
  }
  return t343;
}

u_value f_check_46_fixed_2(u_value v_su, u_value v_env) {
  u_value s346 = v_env;
  u_value t347 = NULL;
  if ((!t347) && u_is_con(s346, &n_list_46_Nil)) {
    t347 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t347) && u_is_con(s346, &n_list_46_Cons)) {
    u_value v_ps = u_field(u_field(u_field(s346, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s346, 0), 1), 1);
    u_value v_rest = u_field(s346, 1);
    t347 = f_list_46__43__43__2(f_list_46_diff_2(f_check_46_free_1(f_check_46_zonk_type_2(v_su, v_t)), v_ps), f_check_46_fixed_2(v_su, v_rest));
  }
  if (!t347) {
    u_no_case(s346);
  }
  return t347;
}

u_value f_check_46_generalize_3(u_value v_su, u_value v_env, u_value v_t) {
  u_value t353 = f_check_46_fixed_2(v_su, v_env);
  u_value t354 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t352;
  {
    u_value v_fx = t353;
    u_value v_u = t354;
    u_value t348 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e349 = &t348;
    u_value l350 = f_check_46_free_1(v_u);
    while (u_is_con(l350, &n_list_46_Cons)) {
      u_value v_m = u_field(l350, 0);
      u_value t351 = f_check_46_is_meta_1(v_m);
      if (u_truth(t351)) {
        t351 = f_prelude_46_not_1(f_list_46_contains_2(v_m, v_fx));
      }
      if (u_truth(t351)) {
        e349 = u_append(e349, v_m);
      }
      l350 = u_field(l350, 1);
    }
    t352 = u_mk_tuple(2, t348, v_u);
  }
  return t352;
}

u_value f_check_46_generalized_3(u_value v_su, u_value v_env, u_value v_xs) {
  u_value t355 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e356 = &t355;
  u_value l357 = v_xs;
  while (u_is_con(l357, &n_list_46_Cons)) {
    u_value v_x = u_field(l357, 0);
    if (u_truth(u_mk_bool(1))) {
      e356 = u_append(e356, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), f_check_46_generalize_3(v_su, v_env, f_syntax_46_Var_46_typ_1(v_x))));
    }
    l357 = u_field(l357, 1);
  }
  return t355;
}

u_value f_check_46_case_of_1(u_value v_b) {
  u_value s358 = v_b;
  u_value t359 = NULL;
  if ((!t359) && u_is_con(s358, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s358, 0);
    u_value v_g = u_field(s358, 1);
    t359 = u_mk_tuple(2, v_p, u_eq(v_g, u_mk_con(&n_option_46_None, 0)));
  }
  if (!t359) {
    u_no_case(s358);
  }
  return t359;
}

u_value f_check_46_zonk_guard_2(u_value v_su, u_value v_g) {
  u_value s360 = v_g;
  u_value t361 = NULL;
  if ((!t361) && u_is_con(s360, &n_option_46_Some)) {
    u_value v_e = u_field(s360, 0);
    t361 = u_mk_con(&n_option_46_Some, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t361) && u_is_con(s360, &n_option_46_None)) {
    t361 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t361) {
    u_no_case(s360);
  }
  return t361;
}

u_value f_check_46_zonk_type_2(u_value v_su, u_value v_t) {
  u_value s362 = f_check_46_resolve_2(v_su, v_t);
  u_value t363 = NULL;
  if ((!t363) && u_is_con(s362, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s362, 0);
    u_value v_args = u_field(s362, 1);
    u_value t364 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e365 = &t364;
    u_value l366 = v_args;
    while (u_is_con(l366, &n_list_46_Cons)) {
      u_value v_a = u_field(l366, 0);
      if (u_truth(u_mk_bool(1))) {
        e365 = u_append(e365, f_check_46_zonk_type_2(v_su, v_a));
      }
      l366 = u_field(l366, 1);
    }
    t363 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t364);
  }
  if ((!t363) && u_is_con(s362, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s362, 0);
    u_value v_r = u_field(s362, 1);
    u_value t367 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e368 = &t367;
    u_value l369 = v_args;
    while (u_is_con(l369, &n_list_46_Cons)) {
      u_value v_a = u_field(l369, 0);
      if (u_truth(u_mk_bool(1))) {
        e368 = u_append(e368, f_check_46_zonk_type_2(v_su, v_a));
      }
      l369 = u_field(l369, 1);
    }
    t363 = u_mk_con(&n_syntax_46_TFun, 2, t367, f_check_46_zonk_type_2(v_su, v_r));
  }
  if ((!t363) && u_is_con(s362, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s362, 0);
    u_value t370 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e371 = &t370;
    u_value l372 = v_ts;
    while (u_is_con(l372, &n_list_46_Cons)) {
      u_value v_a = u_field(l372, 0);
      if (u_truth(u_mk_bool(1))) {
        e371 = u_append(e371, f_check_46_zonk_type_2(v_su, v_a));
      }
      l372 = u_field(l372, 1);
    }
    t363 = u_mk_con(&n_syntax_46_TTuple, 1, t370);
  }
  if (!t363) {
    u_value v_u = s362;
    t363 = v_u;
  }
  if (!t363) {
    u_no_case(s362);
  }
  return t363;
}

u_value f_check_46_zonk_var_2(u_value v_su, u_value v_x) {
  u_value s373 = v_x;
  u_value t374 = NULL;
  if ((!t374) && u_is_con(s373, &n_syntax_46_Var)) {
    u_value v_n = u_field(s373, 0);
    u_value v_t = u_field(s373, 1);
    t374 = u_mk_con(&n_syntax_46_Var, 2, v_n, f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t374) {
    u_no_case(s373);
  }
  return t374;
}

u_value f_check_46_zonk_vars_2(u_value v_su, u_value v_xs) {
  u_value t375 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e376 = &t375;
  u_value l377 = v_xs;
  while (u_is_con(l377, &n_list_46_Cons)) {
    u_value v_x = u_field(l377, 0);
    if (u_truth(u_mk_bool(1))) {
      e376 = u_append(e376, f_check_46_zonk_var_2(v_su, v_x));
    }
    l377 = u_field(l377, 1);
  }
  return t375;
}

u_value f_check_46_zonk_expr_2(u_value v_su, u_value v_e) {
  u_value s378 = v_e;
  u_value t379 = NULL;
  if ((!t379) && u_is_con(s378, &n_syntax_46_ELit)) {
    t379 = v_e;
  }
  if ((!t379) && u_is_con(s378, &n_syntax_46_EVar)) {
    u_value v_x = u_field(s378, 0);
    t379 = u_mk_con(&n_syntax_46_EVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t379) && u_is_con(s378, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s378, 0);
    u_value v_args = u_field(s378, 1);
    u_value t380 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e381 = &t380;
    u_value l382 = v_args;
    while (u_is_con(l382, &n_list_46_Cons)) {
      u_value v_a = u_field(l382, 0);
      if (u_truth(u_mk_bool(1))) {
        e381 = u_append(e381, f_check_46_zonk_expr_2(v_su, v_a));
      }
      l382 = u_field(l382, 1);
    }
    t379 = u_mk_con(&n_syntax_46_EApp, 2, f_check_46_zonk_expr_2(v_su, v_f), t380);
  }
  if ((!t379) && u_is_con(s378, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s378, 0);
    u_value v_b = u_field(s378, 1);
    t379 = u_mk_con(&n_syntax_46_ELam, 2, f_check_46_zonk_vars_2(v_su, v_xs), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t379) && u_is_con(s378, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s378, 0);
    u_value v_b = u_field(s378, 1);
    u_value t383 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e384 = &t383;
    u_value l385 = v_ps;
    while (u_is_con(l385, &n_list_46_Cons)) {
      u_value v_p = u_field(l385, 0);
      if (u_truth(u_mk_bool(1))) {
        e384 = u_append(e384, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l385 = u_field(l385, 1);
    }
    t379 = u_mk_con(&n_syntax_46_ELet, 2, t383, f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t379) && u_is_con(s378, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s378, 0);
    u_value v_l = u_field(s378, 1);
    u_value v_r = u_field(s378, 2);
    t379 = u_mk_con(&n_syntax_46_EIte, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_expr_2(v_su, v_l), f_check_46_zonk_expr_2(v_su, v_r));
  }
  if ((!t379) && u_is_con(s378, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s378, 0);
    u_value v_bs = u_field(s378, 1);
    u_value t386 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e387 = &t386;
    u_value l388 = v_bs;
    while (u_is_con(l388, &n_list_46_Cons)) {
      u_value v_b = u_field(l388, 0);
      if (u_truth(u_mk_bool(1))) {
        e387 = u_append(e387, f_check_46_zonk_expr_branch_2(v_su, v_b));
      }
      l388 = u_field(l388, 1);
    }
    t379 = u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t386);
  }
  if ((!t379) && u_is_con(s378, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s378, 0);
    u_value v_xs = u_field(s378, 1);
    u_value v_ls = u_field(s378, 2);
    u_value v_c = u_field(s378, 3);
    u_value t389 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e390 = &t389;
    u_value l391 = v_ls;
    while (u_is_con(l391, &n_list_46_Cons)) {
      u_value v_l = u_field(l391, 0);
      if (u_truth(u_mk_bool(1))) {
        e390 = u_append(e390, f_check_46_zonk_expr_2(v_su, v_l));
      }
      l391 = u_field(l391, 1);
    }
    t379 = u_mk_con(&n_syntax_46_EComp, 4, f_check_46_zonk_expr_2(v_su, v_r), f_check_46_zonk_vars_2(v_su, v_xs), t389, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t379) && u_is_con(s378, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s378, 0);
    u_value t392 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e393 = &t392;
    u_value l394 = v_es;
    while (u_is_con(l394, &n_list_46_Cons)) {
      u_value v_e = u_field(l394, 0);
      if (u_truth(u_mk_bool(1))) {
        e393 = u_append(e393, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l394 = u_field(l394, 1);
    }
    t379 = u_mk_con(&n_syntax_46_ETuple, 1, t392);
  }
  if ((!t379) && u_is_con(s378, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s378, 0);
    u_value v_t = u_field(s378, 1);
    t379 = u_mk_con(&n_syntax_46_ERaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t379) {
    u_no_case(s378);
  }
  return t379;
}

u_value f_check_46_zonk_binding_2(u_value v_su, u_value v_p) {
  u_value s395 = v_p;
  u_value t396 = NULL;
  if (!t396) {
    u_value v_x = u_field(s395, 0);
    u_value v_e = u_field(s395, 1);
    t396 = u_mk_tuple(2, f_check_46_zonk_var_2(v_su, v_x), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t396) {
    u_no_case(s395);
  }
  return t396;
}

u_value f_check_46_zonk_expr_branch_2(u_value v_su, u_value v_b) {
  u_value s397 = v_b;
  u_value t398 = NULL;
  if ((!t398) && u_is_con(s397, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s397, 0);
    u_value v_g = u_field(s397, 1);
    u_value v_e = u_field(s397, 2);
    t398 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t398) {
    u_no_case(s397);
  }
  return t398;
}

u_value f_check_46_zonk_pat_2(u_value v_su, u_value v_p) {
  u_value s399 = v_p;
  u_value t400 = NULL;
  if ((!t400) && u_is_con(s399, &n_syntax_46_PVar)) {
    u_value v_x = u_field(s399, 0);
    t400 = u_mk_con(&n_syntax_46_PVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t400) && u_is_con(s399, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s399, 0);
    u_value v_args = u_field(s399, 1);
    u_value t401 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e402 = &t401;
    u_value l403 = v_args;
    while (u_is_con(l403, &n_list_46_Cons)) {
      u_value v_a = u_field(l403, 0);
      if (u_truth(u_mk_bool(1))) {
        e402 = u_append(e402, f_check_46_zonk_pat_2(v_su, v_a));
      }
      l403 = u_field(l403, 1);
    }
    t400 = u_mk_con(&n_syntax_46_PApp, 2, v_n, t401);
  }
  if ((!t400) && u_is_con(s399, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s399, 0);
    u_value t404 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e405 = &t404;
    u_value l406 = v_ps;
    while (u_is_con(l406, &n_list_46_Cons)) {
      u_value v_q = u_field(l406, 0);
      if (u_truth(u_mk_bool(1))) {
        e405 = u_append(e405, f_check_46_zonk_pat_2(v_su, v_q));
      }
      l406 = u_field(l406, 1);
    }
    t400 = u_mk_con(&n_syntax_46_PTuple, 1, t404);
  }
  if (!t400) {
    t400 = v_p;
  }
  if (!t400) {
    u_no_case(s399);
  }
  return t400;
}

u_value f_check_46_zonk_stmt_2(u_value v_su, u_value v_x) {
  u_value s407 = v_x;
  u_value t408 = NULL;
  if ((!t408) && u_is_con(s407, &n_syntax_46_SSkip)) {
    t408 = v_x;
  }
  if ((!t408) && u_is_con(s407, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s407, 0);
    u_value t409 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e410 = &t409;
    u_value l411 = v_ps;
    while (u_is_con(l411, &n_list_46_Cons)) {
      u_value v_p = u_field(l411, 0);
      if (u_truth(u_mk_bool(1))) {
        e410 = u_append(e410, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l411 = u_field(l411, 1);
    }
    t408 = u_mk_con(&n_syntax_46_SAssign, 1, t409);
  }
  if ((!t408) && u_is_con(s407, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s407, 0);
    u_value v_ys = u_field(s407, 1);
    u_value t412 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e413 = &t412;
    u_value l414 = v_ys;
    while (u_is_con(l414, &n_list_46_Cons)) {
      u_value v_y = u_field(l414, 0);
      if (u_truth(u_mk_bool(1))) {
        e413 = u_append(e413, f_check_46_zonk_stmt_2(v_su, v_y));
      }
      l414 = u_field(l414, 1);
    }
    t408 = u_mk_con(&n_syntax_46_SBlock, 2, f_check_46_zonk_vars_2(v_su, v_xs), t412);
  }
  if ((!t408) && u_is_con(s407, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s407, 0);
    u_value v_l = u_field(s407, 1);
    u_value v_r = u_field(s407, 2);
    t408 = u_mk_con(&n_syntax_46_SIf, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_l), f_check_46_zonk_stmt_2(v_su, v_r));
  }
  if ((!t408) && u_is_con(s407, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s407, 0);
    u_value v_b = u_field(s407, 1);
    t408 = u_mk_con(&n_syntax_46_SWhile, 2, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t408) && u_is_con(s407, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s407, 0);
    u_value v_ins = u_field(s407, 1);
    u_value v_outs = u_field(s407, 2);
    u_value v_refs = u_field(s407, 3);
    u_value t415 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e416 = &t415;
    u_value l417 = v_ins;
    while (u_is_con(l417, &n_list_46_Cons)) {
      u_value v_e = u_field(l417, 0);
      if (u_truth(u_mk_bool(1))) {
        e416 = u_append(e416, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l417 = u_field(l417, 1);
    }
    t408 = u_mk_con(&n_syntax_46_SCall, 4, v_p, t415, f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs));
  }
  if ((!t408) && u_is_con(s407, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s407, 0);
    u_value v_bs = u_field(s407, 1);
    u_value t418 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e419 = &t418;
    u_value l420 = v_bs;
    while (u_is_con(l420, &n_list_46_Cons)) {
      u_value v_b = u_field(l420, 0);
      if (u_truth(u_mk_bool(1))) {
        e419 = u_append(e419, f_check_46_zonk_stmt_branch_2(v_su, v_b));
      }
      l420 = u_field(l420, 1);
    }
    t408 = u_mk_con(&n_syntax_46_SMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t418);
  }
  if ((!t408) && u_is_con(s407, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s407, 0);
    t408 = u_mk_con(&n_syntax_46_SAssert, 1, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t408) && u_is_con(s407, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s407, 0);
    u_value v_t = u_field(s407, 1);
    t408 = u_mk_con(&n_syntax_46_SRaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if ((!t408) && u_is_con(s407, &n_syntax_46_STry)) {
    u_value v_b = u_field(s407, 0);
    u_value v_hs = u_field(s407, 1);
    u_value t421 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e422 = &t421;
    u_value l423 = v_hs;
    while (u_is_con(l423, &n_list_46_Cons)) {
      u_value v_h = u_field(l423, 0);
      if (u_truth(u_mk_bool(1))) {
        e422 = u_append(e422, f_check_46_zonk_handler_2(v_su, v_h));
      }
      l423 = u_field(l423, 1);
    }
    t408 = u_mk_con(&n_syntax_46_STry, 2, f_check_46_zonk_stmt_2(v_su, v_b), t421);
  }
  if (!t408) {
    u_no_case(s407);
  }
  return t408;
}

u_value f_check_46_zonk_stmt_branch_2(u_value v_su, u_value v_b) {
  u_value s424 = v_b;
  u_value t425 = NULL;
  if ((!t425) && u_is_con(s424, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s424, 0);
    u_value v_g = u_field(s424, 1);
    u_value v_y = u_field(s424, 2);
    t425 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_y));
  }
  if (!t425) {
    u_no_case(s424);
  }
  return t425;
}

u_value f_check_46_zonk_handler_2(u_value v_su, u_value v_h) {
  u_value s426 = v_h;
  u_value t427 = NULL;
  if ((!t427) && u_is_con(s426, &n_syntax_46_Handler)) {
    u_value v_t = u_field(s426, 0);
    u_value v_p = u_field(s426, 1);
    u_value v_g = u_field(s426, 2);
    u_value v_b = u_field(s426, 3);
    t427 = u_mk_con(&n_syntax_46_Handler, 4, f_check_46_zonk_type_2(v_su, v_t), f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t427) {
    u_no_case(s426);
  }
  return t427;
}

u_value f_check_46_zonk_cmd_2(u_value v_su, u_value v_c) {
  u_value s428 = v_c;
  u_value t429 = NULL;
  if ((!t429) && u_is_con(s428, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s428, 0);
    u_value v_ps = u_field(s428, 1);
    u_value v_xss = u_field(s428, 2);
    u_value v_r = u_field(s428, 3);
    u_value v_b = u_field(s428, 4);
    u_value t430 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e431 = &t430;
    u_value l432 = v_xss;
    while (u_is_con(l432, &n_list_46_Cons)) {
      u_value v_xs = u_field(l432, 0);
      if (u_truth(u_mk_bool(1))) {
        e431 = u_append(e431, f_check_46_zonk_vars_2(v_su, v_xs));
      }
      l432 = u_field(l432, 1);
    }
    t429 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, t430, f_check_46_zonk_type_2(v_su, v_r), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s428, 0);
    u_value v_ps = u_field(s428, 1);
    u_value v_ins = u_field(s428, 2);
    u_value v_outs = u_field(s428, 3);
    u_value v_refs = u_field(s428, 4);
    u_value v_b = u_field(s428, 5);
    t429 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, f_check_46_zonk_vars_2(v_su, v_ins), f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s428, 0);
    t429 = u_mk_con(&n_syntax_46_CEval, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_CRun)) {
    u_value v_n = u_field(s428, 0);
    u_value v_b = u_field(s428, 1);
    t429 = u_mk_con(&n_syntax_46_CRun, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_CTest)) {
    u_value v_n = u_field(s428, 0);
    u_value v_b = u_field(s428, 1);
    t429 = u_mk_con(&n_syntax_46_CTest, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t429) {
    t429 = v_c;
  }
  if (!t429) {
    u_no_case(s428);
  }
  return t429;
}

u_value f_check_46_unknown_type_1(u_value v_t) {
  u_value t433;
  if (u_truth(u_eq(f_check_46_free_1(v_t), u_mk_con(&n_list_46_Nil, 0)))) {
    t433 = u_mk_con(&n_option_46_None, 0);
  } else {
    t433 = u_mk_con(&n_option_46_Some, 1, v_t);
  }
  return t433;
}

u_value f_check_46_unknown_in_1(u_value v_x) {
  u_value s434 = v_x;
  u_value t435 = NULL;
  if ((!t435) && u_is_con(s434, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s434, 0);
    t435 = f_check_46_unknown_in_exprs_1(f_list_46_seconds_1(v_ps));
  }
  if ((!t435) && u_is_con(s434, &n_syntax_46_SBlock)) {
    u_value v_ys = u_field(s434, 1);
    t435 = f_check_46_unknown_in_all_1(v_ys);
  }
  if ((!t435) && u_is_con(s434, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s434, 0);
    u_value v_l = u_field(s434, 1);
    u_value v_r = u_field(s434, 2);
    t435 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_option_46_or_else_2(f_check_46_unknown_in_1(v_l), f_check_46_unknown_in_1(v_r)));
  }
  if ((!t435) && u_is_con(s434, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s434, 0);
    u_value v_b = u_field(s434, 1);
    t435 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_check_46_unknown_in_1(v_b));
  }
  if ((!t435) && u_is_con(s434, &n_syntax_46_SCall)) {
    u_value v_ins = u_field(s434, 1);
    t435 = f_check_46_unknown_in_exprs_1(v_ins);
  }
  if ((!t435) && u_is_con(s434, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s434, 0);
    u_value v_bs = u_field(s434, 1);
    t435 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_a), f_check_46_unknown_in_branches_1(v_bs));
  }
  if ((!t435) && u_is_con(s434, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s434, 0);
    t435 = f_check_46_unknown_in_expr_1(v_c);
  }
  if ((!t435) && u_is_con(s434, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s434, 0);
    u_value v_t = u_field(s434, 1);
    t435 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t435) && u_is_con(s434, &n_syntax_46_STry)) {
    u_value v_b = u_field(s434, 0);
    u_value v_hs = u_field(s434, 1);
    t435 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_hs));
  }
  if (!t435) {
    t435 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t435) {
    u_no_case(s434);
  }
  return t435;
}

u_value f_check_46_unknown_in_expr_1(u_value v_e) {
  u_value s436 = v_e;
  u_value t437 = NULL;
  if ((!t437) && u_is_con(s436, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s436, 0);
    u_value v_t = u_field(s436, 1);
    t437 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s436, 0);
    u_value v_args = u_field(s436, 1);
    t437 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_f, v_args));
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_ELam)) {
    u_value v_b = u_field(s436, 1);
    t437 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s436, 0);
    u_value v_b = u_field(s436, 1);
    t437 = f_check_46_unknown_in_exprs_1(f_list_46__43__43__2(f_list_46_seconds_1(v_ps), u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s436, 0);
    u_value v_l = u_field(s436, 1);
    u_value v_r = u_field(s436, 2);
    t437 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)))));
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s436, 0);
    u_value v_bs = u_field(s436, 1);
    u_value t438 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e439 = &t438;
    u_value l440 = v_bs;
    while (u_is_con(l440, &n_list_46_Cons)) {
      u_value v_b = u_field(l440, 0);
      if (u_truth(u_mk_bool(1))) {
        e439 = u_append(e439, f_check_46_branch_exprs_1(v_b));
      }
      l440 = u_field(l440, 1);
    }
    t437 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_a, f_list_46_concat_1(t438)));
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s436, 0);
    u_value v_ls = u_field(s436, 2);
    u_value v_c = u_field(s436, 3);
    t437 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Cons, 2, v_c, v_ls)));
  }
  if ((!t437) && u_is_con(s436, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s436, 0);
    t437 = f_check_46_unknown_in_exprs_1(v_es);
  }
  if (!t437) {
    t437 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t437) {
    u_no_case(s436);
  }
  return t437;
}

u_value f_check_46_unknown_in_exprs_1(u_value v_es) {
  u_value s441 = v_es;
  u_value t442 = NULL;
  if ((!t442) && u_is_con(s441, &n_list_46_Nil)) {
    t442 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t442) && u_is_con(s441, &n_list_46_Cons)) {
    u_value v_e = u_field(s441, 0);
    u_value v_rest = u_field(s441, 1);
    t442 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_e), f_check_46_unknown_in_exprs_1(v_rest));
  }
  if (!t442) {
    u_no_case(s441);
  }
  return t442;
}

u_value f_check_46_branch_exprs_1(u_value v_b) {
  u_value s443 = v_b;
  u_value t444 = NULL;
  if ((!t444) && (u_is_con(s443, &n_syntax_46_Branch) && u_is_con(u_field(s443, 1), &n_option_46_Some))) {
    u_value v_g = u_field(u_field(s443, 1), 0);
    u_value v_e = u_field(s443, 2);
    t444 = u_mk_con(&n_list_46_Cons, 2, v_g, u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t444) && (u_is_con(s443, &n_syntax_46_Branch) && u_is_con(u_field(s443, 1), &n_option_46_None))) {
    u_value v_e = u_field(s443, 2);
    t444 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t444) {
    u_no_case(s443);
  }
  return t444;
}

u_value f_check_46_unknown_in_all_1(u_value v_xs) {
  u_value s445 = v_xs;
  u_value t446 = NULL;
  if ((!t446) && u_is_con(s445, &n_list_46_Nil)) {
    t446 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t446) && u_is_con(s445, &n_list_46_Cons)) {
    u_value v_y = u_field(s445, 0);
    u_value v_ys = u_field(s445, 1);
    t446 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_all_1(v_ys));
  }
  if (!t446) {
    u_no_case(s445);
  }
  return t446;
}

u_value f_check_46_unknown_in_branches_1(u_value v_bs) {
  u_value s447 = v_bs;
  u_value t448 = NULL;
  if ((!t448) && u_is_con(s447, &n_list_46_Nil)) {
    t448 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t448) && (u_is_con(s447, &n_list_46_Cons) && (u_is_con(u_field(s447, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s447, 0), 1), &n_option_46_Some)))) {
    u_value v_g = u_field(u_field(u_field(s447, 0), 1), 0);
    u_value v_y = u_field(u_field(s447, 0), 2);
    u_value v_rest = u_field(s447, 1);
    t448 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_g), f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest)));
  }
  if ((!t448) && (u_is_con(s447, &n_list_46_Cons) && (u_is_con(u_field(s447, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s447, 0), 1), &n_option_46_None)))) {
    u_value v_y = u_field(u_field(s447, 0), 2);
    u_value v_rest = u_field(s447, 1);
    t448 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest));
  }
  if (!t448) {
    u_no_case(s447);
  }
  return t448;
}

u_value f_check_46_unknown_in_handlers_1(u_value v_hs) {
  u_value s449 = v_hs;
  u_value t450 = NULL;
  if ((!t450) && u_is_con(s449, &n_list_46_Nil)) {
    t450 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t450) && (u_is_con(s449, &n_list_46_Cons) && (u_is_con(u_field(s449, 0), &n_syntax_46_Handler) && u_is_con(u_field(u_field(s449, 0), 1), &n_syntax_46_PWild)))) {
    u_value v_b = u_field(u_field(s449, 0), 3);
    u_value v_rest = u_field(s449, 1);
    t450 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest));
  }
  if ((!t450) && (u_is_con(s449, &n_list_46_Cons) && u_is_con(u_field(s449, 0), &n_syntax_46_Handler))) {
    u_value v_t = u_field(u_field(s449, 0), 0);
    u_value v_b = u_field(u_field(s449, 0), 3);
    u_value v_rest = u_field(s449, 1);
    t450 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest)));
  }
  if (!t450) {
    u_no_case(s449);
  }
  return t450;
}

u_value f_check_46_unknown_in_cmd_1(u_value v_c) {
  u_value s451 = v_c;
  u_value t452 = NULL;
  if ((!t452) && u_is_con(s451, &n_syntax_46_CFun)) {
    u_value v_b = u_field(s451, 4);
    t452 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t452) && u_is_con(s451, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s451, 0);
    t452 = f_check_46_unknown_in_expr_1(v_e);
  }
  if ((!t452) && u_is_con(s451, &n_syntax_46_CProc)) {
    u_value v_b = u_field(s451, 5);
    t452 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t452) && u_is_con(s451, &n_syntax_46_CRun)) {
    u_value v_b = u_field(s451, 1);
    t452 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t452) && u_is_con(s451, &n_syntax_46_CTest)) {
    u_value v_b = u_field(s451, 1);
    t452 = f_check_46_unknown_in_1(v_b);
  }
  if (!t452) {
    t452 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t452) {
    u_no_case(s451);
  }
  return t452;
}

u_value f_check_46_head_of_1(u_value v_p) {
  u_value s453 = v_p;
  u_value t454 = NULL;
  if ((!t454) && u_is_con(s453, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s453, 0);
    u_value v_args = u_field(s453, 1);
    t454 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_n), v_args));
  }
  if ((!t454) && u_is_con(s453, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s453, 0);
    t454 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ps));
  }
  if ((!t454) && u_is_con(s453, &n_syntax_46_PLit)) {
    u_value v_v = u_field(s453, 0);
    t454 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, v_v), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t454) {
    t454 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t454) {
    u_no_case(s453);
  }
  return t454;
}

u_value f_check_46_heads_1(u_value v_rows) {
  u_value s455 = v_rows;
  u_value t456 = NULL;
  if ((!t456) && u_is_con(s455, &n_list_46_Nil)) {
    t456 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t456) && (u_is_con(s455, &n_list_46_Cons) && u_is_con(u_field(s455, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s455, 0), 0);
    u_value v_rest = u_field(s455, 1);
    u_value s457 = f_check_46_head_of_1(v_p);
    u_value t458 = NULL;
    if ((!t458) && u_is_con(s457, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s457, 0), 0);
      t458 = u_mk_con(&n_list_46_Cons, 2, v_h, f_check_46_heads_1(v_rest));
    }
    if ((!t458) && u_is_con(s457, &n_option_46_None)) {
      t458 = f_check_46_heads_1(v_rest);
    }
    if (!t458) {
      u_no_case(s457);
    }
    t456 = t458;
  }
  if ((!t456) && (u_is_con(s455, &n_list_46_Cons) && u_is_con(u_field(s455, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s455, 1);
    t456 = f_check_46_heads_1(v_rest);
  }
  if (!t456) {
    u_no_case(s455);
  }
  return t456;
}

u_value f_check_46_field_types_3(u_value v_c, u_value v_k, u_value v_args) {
  u_value s459 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t460 = NULL;
  if ((!t460) && u_is_con(s459, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s459, 0), 0);
    u_value v_ks = u_field(u_field(s459, 0), 1);
    u_value t461 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e462 = &t461;
    u_value l463 = v_ks;
    while (u_is_con(l463, &n_list_46_Cons)) {
      u_value v_a = u_field(l463, 0);
      if (u_truth(u_mk_bool(1))) {
        e462 = u_append(e462, f_check_46_subst_2(v_a, f_list_46_zip_2(v_ps, v_args)));
      }
      l463 = u_field(l463, 1);
    }
    t460 = t461;
  }
  if ((!t460) && u_is_con(s459, &n_option_46_None)) {
    t460 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t460) {
    u_no_case(s459);
  }
  return t460;
}

u_value f_check_46_constructors_of_3(u_value v_c, u_value v_su, u_value v_t) {
  u_value s464 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t465 = NULL;
  if ((!t465) && (u_is_con(s464, &n_syntax_46_TSort) && (u_is_con(u_field(s464, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s464, 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(s464, 0), 1), u_mk_lit("Bool")) && u_is_con(u_field(s464, 1), &n_list_46_Nil)))))) {
    t465 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t465) && u_is_con(s464, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s464, 0);
    t465 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ts), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t465) && u_is_con(s464, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s464, 0);
    u_value v_args = u_field(s464, 1);
    u_value s466 = f_ctx_46_members_of_2(v_c, v_n);
    u_value t467 = NULL;
    if ((!t467) && u_is_con(s466, &n_option_46_Some)) {
      u_value v_ks = u_field(s466, 0);
      u_value t468 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e469 = &t468;
      u_value l470 = v_ks;
      while (u_is_con(l470, &n_list_46_Cons)) {
        u_value v_k = u_field(l470, 0);
        if (u_truth(u_mk_bool(1))) {
          e469 = u_append(e469, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_k), f_check_46_field_types_3(v_c, v_k, v_args)));
        }
        l470 = u_field(l470, 1);
      }
      t467 = u_mk_con(&n_option_46_Some, 1, t468);
    }
    if ((!t467) && u_is_con(s466, &n_option_46_None)) {
      t467 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t467) {
      u_no_case(s466);
    }
    t465 = t467;
  }
  if (!t465) {
    t465 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t465) {
    u_no_case(s464);
  }
  return t465;
}

u_value f_check_46_fields_of_4(u_value v_c, u_value v_su, u_value v_t, u_value v_h) {
  u_value s471 = f_check_46_constructors_of_3(v_c, v_su, v_t);
  u_value t472 = NULL;
  if ((!t472) && u_is_con(s471, &n_option_46_Some)) {
    u_value v_ks = u_field(s471, 0);
    u_value s473 = f_list_46_lookup_2(v_h, v_ks);
    u_value t474 = NULL;
    if ((!t474) && u_is_con(s473, &n_option_46_Some)) {
      u_value v_ts = u_field(s473, 0);
      t474 = v_ts;
    }
    if ((!t474) && u_is_con(s473, &n_option_46_None)) {
      t474 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t474) {
      u_no_case(s473);
    }
    t472 = t474;
  }
  if ((!t472) && u_is_con(s471, &n_option_46_None)) {
    t472 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t472) {
    u_no_case(s471);
  }
  return t472;
}

u_value f_check_46_covers_2(u_value v_hs, u_value v_ks) {
  u_value s475 = v_ks;
  u_value t476 = NULL;
  if ((!t476) && u_is_con(s475, &n_list_46_Nil)) {
    t476 = u_mk_bool(1);
  }
  if ((!t476) && u_is_con(s475, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s475, 0), 0);
    u_value v_rest = u_field(s475, 1);
    u_value t477 = f_list_46_contains_2(v_h, v_hs);
    if (u_truth(t477)) {
      t477 = f_check_46_covers_2(v_hs, v_rest);
    }
    t476 = t477;
  }
  if (!t476) {
    u_no_case(s475);
  }
  return t476;
}

u_value f_check_46_defaults_1(u_value v_rows) {
  u_value s478 = v_rows;
  u_value t479 = NULL;
  if ((!t479) && u_is_con(s478, &n_list_46_Nil)) {
    t479 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t479) && (u_is_con(s478, &n_list_46_Cons) && u_is_con(u_field(s478, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s478, 0), 0);
    u_value v_ps = u_field(u_field(s478, 0), 1);
    u_value v_rest = u_field(s478, 1);
    u_value s480 = f_check_46_head_of_1(v_p);
    u_value t481 = NULL;
    if ((!t481) && u_is_con(s480, &n_option_46_Some)) {
      t481 = f_check_46_defaults_1(v_rest);
    }
    if ((!t481) && u_is_con(s480, &n_option_46_None)) {
      t481 = u_mk_con(&n_list_46_Cons, 2, v_ps, f_check_46_defaults_1(v_rest));
    }
    if (!t481) {
      u_no_case(s480);
    }
    t479 = t481;
  }
  if ((!t479) && (u_is_con(s478, &n_list_46_Cons) && u_is_con(u_field(s478, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s478, 1);
    t479 = f_check_46_defaults_1(v_rest);
  }
  if (!t479) {
    u_no_case(s478);
  }
  return t479;
}

u_value f_check_46_specialize_3(u_value v_rows, u_value v_h, u_value v_n) {
  u_value s482 = v_rows;
  u_value t483 = NULL;
  if ((!t483) && u_is_con(s482, &n_list_46_Nil)) {
    t483 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t483) && (u_is_con(s482, &n_list_46_Cons) && u_is_con(u_field(s482, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s482, 0), 0);
    u_value v_ps = u_field(u_field(s482, 0), 1);
    u_value v_rest = u_field(s482, 1);
    u_value s484 = f_check_46_head_of_1(v_p);
    u_value t485 = NULL;
    if ((!t485) && u_is_con(s484, &n_option_46_Some)) {
      u_value v_g = u_field(u_field(s484, 0), 0);
      u_value v_args = u_field(u_field(s484, 0), 1);
      u_value t486;
      if (u_truth(u_eq(v_g, v_h))) {
        t486 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(v_args, v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
      } else {
        t486 = f_check_46_specialize_3(v_rest, v_h, v_n);
      }
      t485 = t486;
    }
    if ((!t485) && u_is_con(s484, &n_option_46_None)) {
      t485 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(f_list_46_replicate_2(v_n, u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
    }
    if (!t485) {
      u_no_case(s484);
    }
    t483 = t485;
  }
  if ((!t483) && (u_is_con(s482, &n_list_46_Cons) && u_is_con(u_field(s482, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s482, 1);
    t483 = f_check_46_specialize_3(v_rest, v_h, v_n);
  }
  if (!t483) {
    u_no_case(s482);
  }
  return t483;
}

u_value f_check_46_useful_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_q, u_value v_ts) {
  u_value s487 = u_mk_tuple(2, v_q, v_ts);
  u_value t488 = NULL;
  if ((!t488) && u_is_con(u_field(s487, 0), &n_list_46_Nil)) {
    t488 = u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t488) && (u_is_con(u_field(s487, 0), &n_list_46_Cons) && u_is_con(u_field(s487, 1), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s487, 0), 0);
    u_value v_ps = u_field(u_field(s487, 0), 1);
    u_value v_t = u_field(u_field(s487, 1), 0);
    u_value v_rest = u_field(u_field(s487, 1), 1);
    u_value s489 = f_check_46_head_of_1(v_p);
    u_value t490 = NULL;
    if ((!t490) && u_is_con(s489, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s489, 0), 0);
      u_value v_args = u_field(u_field(s489, 0), 1);
      t490 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_ps), f_list_46__43__43__2(f_check_46_fields_of_4(v_c, v_su, v_t, v_h), v_rest));
    }
    if ((!t490) && u_is_con(s489, &n_option_46_None)) {
      u_value s491 = f_check_46_constructors_of_3(v_c, v_su, v_t);
      u_value t492 = NULL;
      if ((!t492) && u_is_con(s491, &n_option_46_Some)) {
        u_value v_ks = u_field(s491, 0);
        u_value t493;
        if (u_truth(f_check_46_covers_2(f_check_46_heads_1(v_rows), v_ks))) {
          t493 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_ks, v_ps, v_rest);
        } else {
          t493 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
        }
        t492 = t493;
      }
      if ((!t492) && u_is_con(s491, &n_option_46_None)) {
        t492 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
      }
      if (!t492) {
        u_no_case(s491);
      }
      t490 = t492;
    }
    if (!t490) {
      u_no_case(s489);
    }
    t488 = t490;
  }
  if ((!t488) && (u_is_con(u_field(s487, 0), &n_list_46_Cons) && u_is_con(u_field(s487, 1), &n_list_46_Nil))) {
    t488 = u_mk_bool(0);
  }
  if (!t488) {
    u_no_case(s487);
  }
  return t488;
}

u_value f_check_46_useful_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_ps, u_value v_rest) {
  u_value s494 = v_ks;
  u_value t495 = NULL;
  if ((!t495) && u_is_con(s494, &n_list_46_Nil)) {
    t495 = u_mk_bool(0);
  }
  if ((!t495) && u_is_con(s494, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s494, 0), 0);
    u_value v_args = u_field(u_field(s494, 0), 1);
    u_value v_more = u_field(s494, 1);
    u_value t496 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_list_46__43__43__2(v_args, v_rest));
    if (!u_truth(t496)) {
      t496 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_more, v_ps, v_rest);
    }
    t495 = t496;
  }
  if (!t495) {
    u_no_case(s494);
  }
  return t495;
}

u_value f_check_46_witness_2(u_value v_h, u_value v_args) {
  u_value s497 = v_h;
  u_value t498 = NULL;
  if ((!t498) && u_is_con(s497, &n_check_46_HCon)) {
    u_value v_n = u_field(s497, 0);
    u_value t499;
    if (u_truth(u_eq(v_n, f_syntax_46_tuple_name_0()))) {
      t499 = u_mk_con(&n_syntax_46_PTuple, 1, v_args);
    } else {
      t499 = u_mk_con(&n_syntax_46_PApp, 2, v_n, v_args);
    }
    t498 = t499;
  }
  if ((!t498) && u_is_con(s497, &n_check_46_HLit)) {
    u_value v_v = u_field(s497, 0);
    t498 = u_mk_con(&n_syntax_46_PLit, 1, v_v);
  }
  if (!t498) {
    u_no_case(s497);
  }
  return t498;
}

u_value f_check_46_missing_4(u_value v_c, u_value v_su, u_value v_rows, u_value v_ts) {
  u_value s500 = v_ts;
  u_value t501 = NULL;
  if ((!t501) && u_is_con(s500, &n_list_46_Nil)) {
    u_value t502;
    if (u_truth(u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0)))) {
      t502 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
    } else {
      t502 = u_mk_con(&n_option_46_None, 0);
    }
    t501 = t502;
  }
  if ((!t501) && u_is_con(s500, &n_list_46_Cons)) {
    u_value v_t = u_field(s500, 0);
    u_value v_rest = u_field(s500, 1);
    t501 = f_check_46_missing_in_6(v_c, v_su, v_rows, f_check_46_heads_1(v_rows), f_check_46_constructors_of_3(v_c, v_su, v_t), v_rest);
  }
  if (!t501) {
    u_no_case(s500);
  }
  return t501;
}

u_value f_check_46_missing_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s503 = v_cs;
  u_value t504 = NULL;
  if ((!t504) && u_is_con(s503, &n_option_46_Some)) {
    u_value v_ks = u_field(s503, 0);
    u_value t505;
    if (u_truth(f_check_46_covers_2(v_hs, v_ks))) {
      t505 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_ks, v_rest);
    } else {
      t505 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
    }
    t504 = t505;
  }
  if ((!t504) && u_is_con(s503, &n_option_46_None)) {
    t504 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
  }
  if (!t504) {
    u_no_case(s503);
  }
  return t504;
}

u_value f_check_46_first_missing_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_rest) {
  u_value s506 = v_ks;
  u_value t507 = NULL;
  if ((!t507) && u_is_con(s506, &n_list_46_Nil)) {
    t507 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t507) && u_is_con(s506, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s506, 0), 0);
    u_value v_args = u_field(u_field(s506, 0), 1);
    u_value v_more = u_field(s506, 1);
    u_value s508 = f_check_46_missing_4(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_rest));
    u_value t509 = NULL;
    if ((!t509) && u_is_con(s508, &n_option_46_Some)) {
      u_value v_ws = u_field(s508, 0);
      t509 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_witness_2(v_h, f_list_46_take_2(f_list_46_length_1(v_args), v_ws)), f_list_46_drop_2(f_list_46_length_1(v_args), v_ws)));
    }
    if ((!t509) && u_is_con(s508, &n_option_46_None)) {
      t509 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_more, v_rest);
    }
    if (!t509) {
      u_no_case(s508);
    }
    t507 = t509;
  }
  if (!t507) {
    u_no_case(s506);
  }
  return t507;
}

u_value f_check_46_other_missing_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s510 = f_check_46_missing_4(v_c, v_su, f_check_46_defaults_1(v_rows), v_rest);
  u_value t511 = NULL;
  if ((!t511) && u_is_con(s510, &n_option_46_Some)) {
    u_value v_ws = u_field(s510, 0);
    t511 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_uncovered_2(v_hs, v_cs), v_ws));
  }
  if ((!t511) && u_is_con(s510, &n_option_46_None)) {
    t511 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t511) {
    u_no_case(s510);
  }
  return t511;
}

u_value f_check_46_uncovered_2(u_value v_hs, u_value v_cs) {
  u_value s512 = v_cs;
  u_value t513 = NULL;
  if ((!t513) && u_is_con(s512, &n_option_46_Some)) {
    u_value v_ks = u_field(s512, 0);
    u_value t514;
    if (u_truth(u_eq(v_hs, u_mk_con(&n_list_46_Nil, 0)))) {
      t514 = u_mk_con(&n_syntax_46_PWild, 0);
    } else {
      t514 = f_check_46_first_uncovered_2(v_hs, v_ks);
    }
    t513 = t514;
  }
  if ((!t513) && u_is_con(s512, &n_option_46_None)) {
    t513 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if (!t513) {
    u_no_case(s512);
  }
  return t513;
}

u_value f_check_46_first_uncovered_2(u_value v_hs, u_value v_ks) {
  u_value s515 = v_ks;
  u_value t516 = NULL;
  if ((!t516) && u_is_con(s515, &n_list_46_Nil)) {
    t516 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if ((!t516) && u_is_con(s515, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s515, 0), 0);
    u_value v_args = u_field(u_field(s515, 0), 1);
    u_value v_rest = u_field(s515, 1);
    u_value t517;
    if (u_truth(f_list_46_contains_2(v_h, v_hs))) {
      t517 = f_check_46_first_uncovered_2(v_hs, v_rest);
    } else {
      t517 = f_check_46_witness_2(v_h, f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)));
    }
    t516 = t517;
  }
  if (!t516) {
    u_no_case(s515);
  }
  return t516;
}

void p_check_46_fresh(u_value *r_t, u_value *r_s) {
  {
    u_value s518 = *r_s;
    int d519 = 0;
    if ((!d519) && u_is_con(s518, &n_check_46_State)) {
      u_value v_c = u_field(s518, 0);
      u_value v_su = u_field(s518, 1);
      u_value v_n = u_field(s518, 2);
      u_value v_m = u_field(s518, 3);
      u_value v_ms = u_field(s518, 4);
      u_value v_vs = u_field(s518, 5);
      d519 = 1;
      {
        u_value t520 = u_mk_con(&n_syntax_46_TMeta, 1, u_add(v_n, u_mk_int(1)));
        u_value t521 = u_mk_con(&n_check_46_State, 6, v_c, v_su, u_add(v_n, u_mk_int(1)), v_m, v_ms, v_vs);
        *r_t = t520;
        *r_s = t521;
      }
    }
    if (!d519) {
      u_no_case(s518);
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
    u_value s522 = *r_s;
    int d523 = 0;
    if ((!d523) && u_is_con(s522, &n_check_46_State)) {
      u_value v_c = u_field(s522, 0);
      u_value v_su = u_field(s522, 1);
      u_value v_n = u_field(s522, 2);
      u_value v_m = u_field(s522, 3);
      u_value v_ms = u_field(s522, 4);
      u_value v_vs = u_field(s522, 5);
      d523 = 1;
      {
        u_value t524 = u_mk_con(&n_check_46_State, 6, v_c, f_map_46_put_3(v_i, v_t, v_su), v_n, v_m, v_ms, v_vs);
        *r_s = t524;
      }
    }
    if (!d523) {
      u_no_case(s522);
    }
  }
}

void p_check_46_unify(u_value v_t1, u_value v_t2, u_value *r_s) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value t525 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t1);
    v_a = t525;
    u_value t526 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t2);
    v_b = t526;
    u_value s527 = u_mk_tuple(2, v_a, v_b);
    int d528 = 0;
    if ((!d528) && (u_is_con(u_field(s527, 0), &n_syntax_46_TMeta) && u_is_con(u_field(s527, 1), &n_syntax_46_TMeta))) {
      u_value v_i = u_field(u_field(s527, 0), 0);
      u_value v_j = u_field(u_field(s527, 1), 0);
      d528 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_i, v_j))) {
          {
            p_check_46_bind_meta(v_i, v_b, r_s);
          }
        }
      }
    }
    if ((!d528) && u_is_con(u_field(s527, 0), &n_syntax_46_TMeta)) {
      u_value v_i = u_field(u_field(s527, 0), 0);
      d528 = 1;
      {
        p_check_46_bind_meta(v_i, v_b, r_s);
      }
    }
    if ((!d528) && u_is_con(u_field(s527, 1), &n_syntax_46_TMeta)) {
      u_value v_j = u_field(u_field(s527, 1), 0);
      d528 = 1;
      {
        p_check_46_bind_meta(v_j, v_a, r_s);
      }
    }
    if ((!d528) && (u_is_con(u_field(s527, 0), &n_syntax_46_TParam) && u_is_con(u_field(s527, 1), &n_syntax_46_TParam))) {
      u_value v_x = u_field(u_field(s527, 0), 0);
      u_value v_y = u_field(u_field(s527, 1), 0);
      d528 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_x, v_y))) {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, v_a, v_b), "check.TypeError");
          }
        }
      }
    }
    if ((!d528) && (u_is_con(u_field(s527, 0), &n_syntax_46_TSort) && u_is_con(u_field(s527, 1), &n_syntax_46_TSort))) {
      u_value v_f = u_field(u_field(s527, 0), 0);
      u_value v_xs = u_field(u_field(s527, 0), 1);
      u_value v_g = u_field(u_field(s527, 1), 0);
      u_value v_ys = u_field(u_field(s527, 1), 1);
      d528 = 1;
      {
        u_value t529 = u_eq(v_f, v_g);
        if (u_truth(t529)) {
          t529 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys));
        }
        if (u_truth(t529)) {
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
    if ((!d528) && (u_is_con(u_field(s527, 0), &n_syntax_46_TFun) && u_is_con(u_field(s527, 1), &n_syntax_46_TFun))) {
      u_value v_xs = u_field(u_field(s527, 0), 0);
      u_value v_r1 = u_field(u_field(s527, 0), 1);
      u_value v_ys = u_field(u_field(s527, 1), 0);
      u_value v_r2 = u_field(u_field(s527, 1), 1);
      d528 = 1;
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
    if ((!d528) && (u_is_con(u_field(s527, 0), &n_syntax_46_TTuple) && u_is_con(u_field(s527, 1), &n_syntax_46_TTuple))) {
      u_value v_xs = u_field(u_field(s527, 0), 0);
      u_value v_ys = u_field(u_field(s527, 1), 0);
      d528 = 1;
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
    if (!d528) {
      d528 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t2)), "check.TypeError");
      }
    }
    if (!d528) {
      u_no_case(s527);
    }
  }
}

void p_check_46_unifies(u_value v_xs, u_value v_ys, u_value *r_s) {
  {
    u_value s530 = u_mk_tuple(2, v_xs, v_ys);
    int d531 = 0;
    if ((!d531) && (u_is_con(u_field(s530, 0), &n_list_46_Cons) && u_is_con(u_field(s530, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s530, 0), 0);
      u_value v_xs1 = u_field(u_field(s530, 0), 1);
      u_value v_y = u_field(u_field(s530, 1), 0);
      u_value v_ys1 = u_field(u_field(s530, 1), 1);
      d531 = 1;
      {
        p_check_46_unify(v_x, v_y, r_s);
        p_check_46_unifies(v_xs1, v_ys1, r_s);
      }
    }
    if ((!d531) && (u_is_con(u_field(s530, 0), &n_list_46_Nil) && u_is_con(u_field(s530, 1), &n_list_46_Nil))) {
      d531 = 1;
      {
      }
    }
    if (!d531) {
      d531 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)), "check.TypeError");
      }
    }
    if (!d531) {
      u_no_case(s530);
    }
  }
}

void p_check_46_fresh_list(u_value v_xs, u_value *r_ts, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s532 = v_xs;
    int d533 = 0;
    if ((!d533) && u_is_con(s532, &n_list_46_Nil)) {
      d533 = 1;
      {
        u_value t534 = u_mk_con(&n_list_46_Nil, 0);
        *r_ts = t534;
      }
    }
    if ((!d533) && u_is_con(s532, &n_list_46_Cons)) {
      u_value v_more = u_field(s532, 1);
      d533 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_fresh_list(v_more, &v_rest, r_s);
        u_value t535 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rest);
        *r_ts = t535;
      }
    }
    if (!d533) {
      u_no_case(s532);
    }
  }
}

void p_check_46_instance(u_value v_ps, u_value *r_m, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s536 = v_ps;
    int d537 = 0;
    if ((!d537) && u_is_con(s536, &n_list_46_Nil)) {
      d537 = 1;
      {
        u_value t538 = u_mk_con(&n_list_46_Nil, 0);
        *r_m = t538;
      }
    }
    if ((!d537) && u_is_con(s536, &n_list_46_Cons)) {
      u_value v_p = u_field(s536, 0);
      u_value v_qs = u_field(s536, 1);
      d537 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_instance(v_qs, &v_rest, r_s);
        u_value t539 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_p, v_t), v_rest);
        *r_m = t539;
      }
    }
    if (!d537) {
      u_no_case(s536);
    }
  }
}

void p_check_46_instantiate(u_value v_sc, u_value *r_t, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value s540 = v_sc;
    int d541 = 0;
    if (!d541) {
      u_value v_ps = u_field(s540, 0);
      u_value v_u = u_field(s540, 1);
      d541 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        u_value t542 = f_check_46_subst_2(f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u), v_m);
        *r_t = t542;
      }
    }
    if (!d541) {
      u_no_case(s540);
    }
  }
}

void p_check_46_unique(u_value v_ns) {
  {
    u_value s543 = f_list_46_repeated_1(v_ns);
    int d544 = 0;
    if ((!d544) && u_is_con(s543, &n_option_46_Some)) {
      u_value v_n = u_field(s543, 0);
      d544 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, f_syntax_46_qualified_1(v_n)), "check.TypeError");
      }
    }
    if ((!d544) && u_is_con(s543, &n_option_46_None)) {
      d544 = 1;
      {
      }
    }
    if (!d544) {
      u_no_case(s543);
    }
  }
}

void p_check_46_unique_ids(u_value v_ns) {
  {
    u_value s545 = f_list_46_repeated_1(v_ns);
    int d546 = 0;
    if ((!d546) && u_is_con(s545, &n_option_46_Some)) {
      u_value v_n = u_field(s545, 0);
      d546 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, v_n), "check.TypeError");
      }
    }
    if ((!d546) && u_is_con(s545, &n_option_46_None)) {
      d546 = 1;
      {
      }
    }
    if (!d546) {
      u_no_case(s545);
    }
  }
}

void p_check_46_one_of(u_value v_n, u_value v_ns, u_value *r_m) {
  {
    u_value s547 = v_ns;
    int d548 = 0;
    if ((!d548) && (u_is_con(s547, &n_list_46_Cons) && u_is_con(u_field(s547, 1), &n_list_46_Nil))) {
      u_value v_k = u_field(s547, 0);
      d548 = 1;
      {
        u_value t549 = v_k;
        *r_m = t549;
      }
    }
    if ((!d548) && u_is_con(s547, &n_list_46_Nil)) {
      d548 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if (!d548) {
      d548 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ns), "check.TypeError");
      }
    }
    if (!d548) {
      u_no_case(s547);
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
    u_value s550 = v_t;
    int d551 = 0;
    if ((!d551) && u_is_con(s550, &n_syntax_46_THole)) {
      d551 = 1;
      {
        p_check_46_fresh(r_u, r_s);
      }
    }
    if ((!d551) && u_is_con(s550, &n_syntax_46_TParam)) {
      d551 = 1;
      {
        u_value t552 = v_t;
        *r_u = t552;
      }
    }
    if ((!d551) && u_is_con(s550, &n_syntax_46_TMeta)) {
      d551 = 1;
      {
        u_value t553 = v_t;
        *r_u = t553;
      }
    }
    if ((!d551) && u_is_con(s550, &n_syntax_46_TFun)) {
      u_value v_args = u_field(s550, 0);
      u_value v_r = u_field(s550, 1);
      d551 = 1;
      {
        p_check_46_normalizes(v_args, &v_us, r_s);
        p_check_46_normalize(v_r, &v_v, r_s);
        u_value t554 = u_mk_con(&n_syntax_46_TFun, 2, v_us, v_v);
        *r_u = t554;
      }
    }
    if ((!d551) && u_is_con(s550, &n_syntax_46_TTuple)) {
      u_value v_ts = u_field(s550, 0);
      d551 = 1;
      {
        p_check_46_normalizes(v_ts, &v_us, r_s);
        u_value t555 = u_mk_con(&n_syntax_46_TTuple, 1, v_us);
        *r_u = t555;
      }
    }
    if ((!d551) && u_is_con(s550, &n_syntax_46_TSort)) {
      u_value v_n = u_field(s550, 0);
      u_value v_args = u_field(s550, 1);
      d551 = 1;
      {
        if (u_truth(f_check_46_basic_1(v_n))) {
          {
            p_check_46_arguments(v_n, u_mk_int(0), v_args);
            u_value t556 = v_t;
            *r_u = t556;
          }
        } else {
          {
            u_value s558 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam557, 1, *r_s));
            int d559 = 0;
            if ((!d559) && (u_is_con(s558, &n_list_46_Cons) && u_is_con(u_field(s558, 1), &n_list_46_Nil))) {
              u_value v_m = u_field(s558, 0);
              d559 = 1;
              {
                p_check_46_normalize_sort(v_m, v_args, r_u, r_s);
              }
            }
            if ((!d559) && u_is_con(s558, &n_list_46_Nil)) {
              d559 = 1;
              {
                u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
              }
            }
            if (!d559) {
              u_value v_ms = s558;
              d559 = 1;
              {
                u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ms), "check.TypeError");
              }
            }
            if (!d559) {
              u_no_case(s558);
            }
          }
        }
      }
    }
    if (!d551) {
      u_no_case(s550);
    }
  }
}

void p_check_46_normalize_sort(u_value v_n, u_value v_args, u_value *r_u, u_value *r_s) {
  {
    u_value v_us = NULL;
    u_value s560 = f_ctx_46_data_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
    int d561 = 0;
    if ((!d561) && u_is_con(s560, &n_option_46_Some)) {
      u_value v_k = u_field(s560, 0);
      d561 = 1;
      {
        p_check_46_arguments(v_n, v_k, v_args);
        p_check_46_normalizes(v_args, &v_us, r_s);
        u_value t562 = u_mk_con(&n_syntax_46_TSort, 2, v_n, v_us);
        *r_u = t562;
      }
    }
    if ((!d561) && u_is_con(s560, &n_option_46_None)) {
      d561 = 1;
      {
        u_value s563 = f_ctx_46_alias_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
        int d564 = 0;
        if ((!d564) && u_is_con(s563, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s563, 0), 0);
          u_value v_rhs = u_field(u_field(s563, 0), 1);
          d564 = 1;
          {
            p_check_46_arguments(v_n, f_list_46_length_1(v_ps), v_args);
            p_check_46_normalizes(v_args, &v_us, r_s);
            u_value t565 = f_check_46_subst_2(v_rhs, f_list_46_zip_2(f_check_46_params_1(v_ps), v_us));
            *r_u = t565;
          }
        }
        if ((!d564) && u_is_con(s563, &n_option_46_None)) {
          d564 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
          }
        }
        if (!d564) {
          u_no_case(s563);
        }
      }
    }
    if (!d561) {
      u_no_case(s560);
    }
  }
}

void p_check_46_normalizes(u_value v_ts, u_value *r_us, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_rest = NULL;
    u_value s566 = v_ts;
    int d567 = 0;
    if ((!d567) && u_is_con(s566, &n_list_46_Nil)) {
      d567 = 1;
      {
        u_value t568 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t568;
      }
    }
    if ((!d567) && u_is_con(s566, &n_list_46_Cons)) {
      u_value v_t = u_field(s566, 0);
      u_value v_more = u_field(s566, 1);
      d567 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalizes(v_more, &v_rest, r_s);
        u_value t569 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t569;
      }
    }
    if (!d567) {
      u_no_case(s566);
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
    u_value s570 = v_ts;
    int d571 = 0;
    if ((!d571) && u_is_con(s570, &n_list_46_Nil)) {
      d571 = 1;
      {
        u_value t572 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t572;
      }
    }
    if ((!d571) && u_is_con(s570, &n_list_46_Cons)) {
      u_value v_t = u_field(s570, 0);
      u_value v_more = u_field(s570, 1);
      d571 = 1;
      {
        p_check_46_known(v_t, v_what, &v_u, r_s);
        p_check_46_knowns(v_more, v_what, &v_rest, r_s);
        u_value t573 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t573;
      }
    }
    if (!d571) {
      u_no_case(s570);
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
    u_value s574 = v_xs;
    int d575 = 0;
    if ((!d575) && u_is_con(s574, &n_list_46_Nil)) {
      d575 = 1;
      {
        u_value t576 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t576;
      }
    }
    if ((!d575) && (u_is_con(s574, &n_list_46_Cons) && u_is_con(u_field(s574, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s574, 0), 0);
      u_value v_t = u_field(u_field(s574, 0), 1);
      u_value v_more = u_field(s574, 1);
      d575 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalize_vars(v_more, &v_rest, r_s);
        u_value t577 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t577;
      }
    }
    if (!d575) {
      u_no_case(s574);
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
    u_value s578 = v_xs;
    int d579 = 0;
    if ((!d579) && u_is_con(s578, &n_list_46_Nil)) {
      d579 = 1;
      {
        u_value t580 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t580;
      }
    }
    if ((!d579) && (u_is_con(s578, &n_list_46_Cons) && u_is_con(u_field(s578, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s578, 0), 0);
      u_value v_t = u_field(u_field(s578, 0), 1);
      u_value v_more = u_field(s578, 1);
      d579 = 1;
      {
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_known_vars(v_more, &v_rest, r_s);
        u_value t581 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t581;
      }
    }
    if (!d579) {
      u_no_case(s578);
    }
  }
}

void p_check_46_resolve_name(u_value v_s, u_value v_env, u_value v_n, u_value v_k, u_value *r_m, u_value *r_sc) {
  {
    u_value s582 = f_check_46_candidates_3(v_s, v_env, v_n);
    int d583 = 0;
    if ((!d583) && u_is_con(s582, &n_list_46_Nil)) {
      d583 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if ((!d583) && (u_is_con(s582, &n_list_46_Cons) && u_is_con(u_field(s582, 1), &n_list_46_Nil))) {
      u_value v_m1 = u_field(u_field(s582, 0), 0);
      u_value v_one = u_field(u_field(s582, 0), 1);
      d583 = 1;
      {
        u_value t584 = v_m1;
        u_value t585 = v_one;
        *r_m = t584;
        *r_sc = t585;
      }
    }
    if (!d583) {
      u_value v_cs = s582;
      d583 = 1;
      {
        if (u_truth(u_lt(v_k, u_mk_int(0)))) {
          {
            u_raise(u_mk_con(&n_check_46_OverloadedUnapplied, 1, v_n), "check.TypeError");
          }
        }
        u_value s586 = f_check_46_with_arity_2(v_cs, v_k);
        int d587 = 0;
        if ((!d587) && (u_is_con(s586, &n_list_46_Cons) && u_is_con(u_field(s586, 1), &n_list_46_Nil))) {
          u_value v_m1 = u_field(u_field(s586, 0), 0);
          u_value v_one = u_field(u_field(s586, 0), 1);
          d587 = 1;
          {
            u_value t588 = v_m1;
            u_value t589 = v_one;
            *r_m = t588;
            *r_sc = t589;
          }
        }
        if ((!d587) && u_is_con(s586, &n_list_46_Nil)) {
          d587 = 1;
          {
            u_raise(u_mk_con(&n_check_46_WrongArity, 2, v_n, v_k), "check.TypeError");
          }
        }
        if (!d587) {
          u_value v_more = s586;
          d587 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, f_list_46_firsts_1(v_more)), "check.TypeError");
          }
        }
        if (!d587) {
          u_no_case(s586);
        }
      }
    }
    if (!d583) {
      u_no_case(s582);
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
    u_value t590 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_u);
    *r_x = t590;
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
    u_value s591 = v_e;
    int d592 = 0;
    if ((!d592) && u_is_con(s591, &n_syntax_46_ELit)) {
      u_value v_v = u_field(s591, 0);
      d592 = 1;
      {
        u_value t593 = v_e;
        u_value t594 = f_check_46_literal_1(v_v);
        *r_e1 = t593;
        *r_t = t594;
      }
    }
    if ((!d592) && (u_is_con(s591, &n_syntax_46_EVar) && u_is_con(u_field(s591, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s591, 0), 0);
      u_value v_a = u_field(u_field(s591, 0), 1);
      d592 = 1;
      {
        p_check_46_resolve_name(*r_s, v_env, v_n, f_prelude_46__45__1(u_mk_int(1)), &v_m, &v_sc);
        p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
        u_value t595 = u_mk_con(&n_syntax_46_EVar, 1, v_x);
        u_value t596 = f_syntax_46_Var_46_typ_1(v_x);
        *r_e1 = t595;
        *r_t = t596;
      }
    }
    if ((!d592) && u_is_con(s591, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s591, 0);
      u_value v_args = u_field(s591, 1);
      d592 = 1;
      {
        p_check_46_application(v_env, v_f, v_args, r_e1, r_t, r_s);
      }
    }
    if ((!d592) && u_is_con(s591, &n_syntax_46_ELam)) {
      u_value v_bound = u_field(s591, 0);
      u_value v_body = u_field(s591, 1);
      d592 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t597 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, v_b);
        u_value t598 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), v_u);
        *r_e1 = t597;
        *r_t = t598;
      }
    }
    if ((!d592) && u_is_con(s591, &n_syntax_46_ELet)) {
      u_value v_bound = u_field(s591, 0);
      u_value v_body = u_field(s591, 1);
      d592 = 1;
      {
        p_check_46_formals(f_list_46_firsts_1(v_bound), &v_xs, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_bound), f_syntax_46_types_1(v_xs), &v_es, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_generalized_3(f_check_46_State_46_sub_1(*r_s), v_env, v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t599 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        u_value t600 = v_u;
        *r_e1 = t599;
        *r_t = t600;
      }
    }
    if ((!d592) && u_is_con(s591, &n_syntax_46_EIte)) {
      u_value v_cond = u_field(s591, 0);
      u_value v_left = u_field(s591, 1);
      u_value v_right = u_field(s591, 2);
      d592 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(v_env, v_left, &v_l, &v_u, r_s);
        p_check_46_check(v_env, v_right, v_u, &v_b, r_s);
        u_value t601 = u_mk_con(&n_syntax_46_EIte, 3, v_c, v_l, v_b);
        u_value t602 = v_u;
        *r_e1 = t601;
        *r_t = t602;
      }
    }
    if ((!d592) && u_is_con(s591, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s591, 0);
      u_value v_bound = u_field(s591, 1);
      u_value v_lists = u_field(s591, 2);
      u_value v_cond = u_field(s591, 3);
      d592 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_same_length(f_list_46_length_1(v_xs), f_list_46_length_1(v_lists));
        p_check_46_generators(v_env, v_xs, v_lists, &v_es, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_result, &v_b, &v_u, r_s);
        u_value t603 = u_mk_con(&n_syntax_46_EComp, 4, v_b, v_xs, v_es, v_c);
        u_value t604 = f_check_46_tlist_1(v_u);
        *r_e1 = t603;
        *r_t = t604;
      }
    }
    if ((!d592) && u_is_con(s591, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s591, 0);
      d592 = 1;
      {
        p_check_46_infers(v_env, v_items, &v_es, &v_ts, r_s);
        u_value t605 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
        u_value t606 = u_mk_con(&n_syntax_46_TTuple, 1, v_ts);
        *r_e1 = t605;
        *r_t = t606;
      }
    }
    if ((!d592) && u_is_con(s591, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s591, 0);
      u_value v_a = u_field(s591, 1);
      d592 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_normalize(v_a, &v_w, r_s);
        p_check_46_unify(v_u, v_w, r_s);
        p_check_46_fresh(r_t, r_s);
        u_value t607 = u_mk_con(&n_syntax_46_ERaise, 2, v_c, v_u);
        *r_e1 = t607;
      }
    }
    if ((!d592) && u_is_con(s591, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s591, 0);
      u_value v_cases = u_field(s591, 1);
      d592 = 1;
      {
        p_check_46_fresh(r_t, r_s);
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_expr_branches(v_env, v_cases, v_u, *r_t, &v_bs, r_s);
        u_value t608 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e609 = &t608;
        u_value l610 = v_bs;
        while (u_is_con(l610, &n_list_46_Cons)) {
          u_value v_br = u_field(l610, 0);
          if (u_truth(u_mk_bool(1))) {
            e609 = u_append(e609, f_check_46_case_of_1(v_br));
          }
          l610 = u_field(l610, 1);
        }
        p_check_46_coverage(*r_s, v_u, t608);
        u_value t611 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, v_bs);
        *r_e1 = t611;
      }
    }
    if (!d592) {
      u_no_case(s591);
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
    u_value s612 = v_f;
    int d613 = 0;
    if ((!d613) && (u_is_con(s612, &n_syntax_46_EVar) && (u_is_con(u_field(s612, 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(s612, 0), 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(u_field(s612, 0), 0), 0), &n_option_46_None))))) {
      u_value v_b = u_field(u_field(u_field(s612, 0), 0), 1);
      u_value v_a = u_field(u_field(s612, 0), 1);
      u_value t614 = f_prelude_46__62__2(u_str_size(v_b), u_mk_int(1));
      if (u_truth(t614)) {
        t614 = u_eq(u_str_get(v_b, u_mk_int(0)), u_mk_char(46));
      }
      if (u_truth(t614)) {
        d613 = 1;
        {
          p_check_46_field_access(v_env, v_b, v_a, v_args, r_e1, r_t, r_s);
        }
      }
    }
    if ((!d613) && (u_is_con(s612, &n_syntax_46_EVar) && u_is_con(u_field(s612, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s612, 0), 0);
      u_value v_a = u_field(u_field(s612, 0), 1);
      d613 = 1;
      {
        if (u_truth(f_prelude_46__62__2(f_list_46_length_1(f_check_46_candidates_3(*r_s, v_env, v_n)), u_mk_int(1)))) {
          {
            p_check_46_resolve_name(*r_s, v_env, v_n, f_list_46_length_1(v_args), &v_m, &v_sc);
            p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
            p_check_46_apply(v_env, f_syntax_46_Var_46_typ_1(v_x), v_args, &v_args1, r_t, r_s);
            u_value t615 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), v_args1);
            *r_e1 = t615;
          }
        } else {
          {
            p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
            p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
            u_value t616 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
            *r_e1 = t616;
          }
        }
      }
    }
    if (!d613) {
      d613 = 1;
      {
        p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
        p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
        u_value t617 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
        *r_e1 = t617;
      }
    }
    if (!d613) {
      u_no_case(s612);
    }
  }
}

void p_check_46_field_access(u_value v_env, u_value v_f, u_value v_a, u_value v_args, u_value *r_e1, u_value *r_t, u_value *r_s) {
  {
    u_value v_arg1 = NULL;
    u_value v_u = NULL;
    u_value v_acc = NULL;
    u_value v_x = NULL;
    u_value s618 = v_args;
    int d619 = 0;
    if ((!d619) && (u_is_con(s618, &n_list_46_Cons) && u_is_con(u_field(s618, 1), &n_list_46_Nil))) {
      u_value v_arg = u_field(s618, 0);
      d619 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_arg1, &v_u, r_s);
        u_value s620 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u);
        int d621 = 0;
        if ((!d621) && (u_is_con(s620, &n_syntax_46_TSort) && (u_is_con(u_field(s620, 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(s620, 0), 0), &n_option_46_Some)))) {
          u_value v_m = u_field(u_field(u_field(s620, 0), 0), 0);
          u_value v_tn = u_field(u_field(s620, 0), 1);
          d621 = 1;
          {
            u_value t622 = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), u_concat(v_tn, v_f));
            v_acc = t622;
          }
        }
        if (!d621) {
          u_value v_v = s620;
          d621 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownFieldType, 2, v_f, v_v), "check.TypeError");
          }
        }
        if (!d621) {
          u_no_case(s620);
        }
        u_value s623 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(*r_s), v_acc);
        int d624 = 0;
        if ((!d624) && (u_is_con(s623, &n_list_46_Cons) && u_is_con(u_field(s623, 1), &n_list_46_Nil))) {
          u_value v_sc = u_field(s623, 0);
          d624 = 1;
          {
            p_check_46_use(v_acc, v_a, v_sc, &v_x, r_s);
          }
        }
        if (!d624) {
          d624 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownField, 1, v_acc), "check.TypeError");
          }
        }
        if (!d624) {
          u_no_case(s623);
        }
        p_check_46_fresh(r_t, r_s);
        p_check_46_unify(f_syntax_46_Var_46_typ_1(v_x), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)), *r_t), r_s);
        u_value t625 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), u_mk_con(&n_list_46_Cons, 2, v_arg1, u_mk_con(&n_list_46_Nil, 0)));
        *r_e1 = t625;
      }
    }
    if (!d619) {
      d619 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, u_mk_int(1), f_list_46_length_1(v_args)), "check.TypeError");
      }
    }
    if (!d619) {
      u_no_case(s618);
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
    u_value s626 = u_mk_tuple(2, v_es, v_ts);
    int d627 = 0;
    if ((!d627) && (u_is_con(u_field(s626, 0), &n_list_46_Cons) && u_is_con(u_field(s626, 1), &n_list_46_Cons))) {
      u_value v_e = u_field(u_field(s626, 0), 0);
      u_value v_more = u_field(u_field(s626, 0), 1);
      u_value v_t = u_field(u_field(s626, 1), 0);
      u_value v_ts1 = u_field(u_field(s626, 1), 1);
      d627 = 1;
      {
        p_check_46_check(v_env, v_e, v_t, &v_e1, r_s);
        p_check_46_checks(v_env, v_more, v_ts1, &v_rest, r_s);
        u_value t628 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        *r_es1 = t628;
      }
    }
    if ((!d627) && (u_is_con(u_field(s626, 0), &n_list_46_Nil) && u_is_con(u_field(s626, 1), &n_list_46_Nil))) {
      d627 = 1;
      {
        u_value t629 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t629;
      }
    }
    if (!d627) {
      d627 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_es)), "check.TypeError");
      }
    }
    if (!d627) {
      u_no_case(s626);
    }
  }
}

void p_check_46_infers(u_value v_env, u_value v_es, u_value *r_es1, u_value *r_ts, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value v_us = NULL;
    u_value s630 = v_es;
    int d631 = 0;
    if ((!d631) && u_is_con(s630, &n_list_46_Nil)) {
      d631 = 1;
      {
        u_value t632 = u_mk_con(&n_list_46_Nil, 0);
        u_value t633 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t632;
        *r_ts = t633;
      }
    }
    if ((!d631) && u_is_con(s630, &n_list_46_Cons)) {
      u_value v_e = u_field(s630, 0);
      u_value v_more = u_field(s630, 1);
      d631 = 1;
      {
        p_check_46_infer(v_env, v_e, &v_e1, &v_t, r_s);
        p_check_46_infers(v_env, v_more, &v_rest, &v_us, r_s);
        u_value t634 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        u_value t635 = u_mk_con(&n_list_46_Cons, 2, v_t, v_us);
        *r_es1 = t634;
        *r_ts = t635;
      }
    }
    if (!d631) {
      u_no_case(s630);
    }
  }
}

void p_check_46_generators(u_value v_env, u_value v_xs, u_value v_lists, u_value *r_lists1, u_value *r_s) {
  {
    u_value v_l = NULL;
    u_value v_rest = NULL;
    u_value s636 = u_mk_tuple(2, v_xs, v_lists);
    int d637 = 0;
    if ((!d637) && (u_is_con(u_field(s636, 0), &n_list_46_Cons) && u_is_con(u_field(s636, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s636, 0), 0);
      u_value v_ys = u_field(u_field(s636, 0), 1);
      u_value v_e = u_field(u_field(s636, 1), 0);
      u_value v_es = u_field(u_field(s636, 1), 1);
      d637 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tlist_1(f_syntax_46_Var_46_typ_1(v_x)), &v_l, r_s);
        p_check_46_generators(f_list_46__43__43__2(f_check_46_bind_1(u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0))), v_env), v_ys, v_es, &v_rest, r_s);
        u_value t638 = u_mk_con(&n_list_46_Cons, 2, v_l, v_rest);
        *r_lists1 = t638;
      }
    }
    if (!d637) {
      d637 = 1;
      {
        u_value t639 = u_mk_con(&n_list_46_Nil, 0);
        *r_lists1 = t639;
      }
    }
    if (!d637) {
      u_no_case(s636);
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
    u_value s640 = v_cases;
    int d641 = 0;
    if ((!d641) && u_is_con(s640, &n_list_46_Nil)) {
      d641 = 1;
      {
        u_value t642 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t642;
      }
    }
    if ((!d641) && (u_is_con(s640, &n_list_46_Cons) && u_is_con(u_field(s640, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s640, 0), 0);
      u_value v_g = u_field(u_field(s640, 0), 1);
      u_value v_b = u_field(u_field(s640, 0), 2);
      u_value v_more = u_field(s640, 1);
      d641 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_b, v_r, &v_b1, r_s);
        p_check_46_expr_branches(v_env, v_more, v_t, v_r, &v_rest, r_s);
        u_value t643 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t643;
      }
    }
    if (!d641) {
      u_no_case(s640);
    }
  }
}

void p_check_46_guard(u_value v_env, u_value v_g, u_value *r_g1, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value s644 = v_g;
    int d645 = 0;
    if ((!d645) && u_is_con(s644, &n_option_46_Some)) {
      u_value v_e = u_field(s644, 0);
      d645 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tbool_0(), &v_e1, r_s);
        u_value t646 = u_mk_con(&n_option_46_Some, 1, v_e1);
        *r_g1 = t646;
      }
    }
    if ((!d645) && u_is_con(s644, &n_option_46_None)) {
      d645 = 1;
      {
        u_value t647 = u_mk_con(&n_option_46_None, 0);
        *r_g1 = t647;
      }
    }
    if (!d645) {
      u_no_case(s644);
    }
  }
}

void p_check_46_coverage(u_value v_s, u_value v_t, u_value v_cs) {
  {
    p_check_46_reachable(v_s, v_t, u_mk_con(&n_list_46_Nil, 0), v_cs);
    u_value t648 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e649 = &t648;
    u_value l650 = f_check_46_unguarded_1(v_cs);
    while (u_is_con(l650, &n_list_46_Cons)) {
      u_value v_p = u_field(l650, 0);
      if (u_truth(u_mk_bool(1))) {
        e649 = u_append(e649, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
      }
      l650 = u_field(l650, 1);
    }
    u_value s651 = f_check_46_missing_4(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t648, u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
    int d652 = 0;
    if ((!d652) && (u_is_con(s651, &n_option_46_Some) && u_is_con(u_field(s651, 0), &n_list_46_Cons))) {
      u_value v_w = u_field(u_field(s651, 0), 0);
      d652 = 1;
      {
        u_raise(u_mk_con(&n_check_46_NotExhaustive, 1, v_w), "check.TypeError");
      }
    }
    if (!d652) {
      d652 = 1;
      {
      }
    }
    if (!d652) {
      u_no_case(s651);
    }
  }
}

u_value f_check_46_unguarded_1(u_value v_cs) {
  u_value s653 = v_cs;
  u_value t654 = NULL;
  if ((!t654) && u_is_con(s653, &n_list_46_Nil)) {
    t654 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t654) && (u_is_con(s653, &n_list_46_Cons) && u_equals(u_field(u_field(s653, 0), 1), u_mk_bool(1)))) {
    u_value v_p = u_field(u_field(s653, 0), 0);
    u_value v_rest = u_field(s653, 1);
    t654 = u_mk_con(&n_list_46_Cons, 2, v_p, f_check_46_unguarded_1(v_rest));
  }
  if ((!t654) && (u_is_con(s653, &n_list_46_Cons) && u_equals(u_field(u_field(s653, 0), 1), u_mk_bool(0)))) {
    u_value v_rest = u_field(s653, 1);
    t654 = f_check_46_unguarded_1(v_rest);
  }
  if (!t654) {
    u_no_case(s653);
  }
  return t654;
}

void p_check_46_reachable(u_value v_s, u_value v_t, u_value v_before, u_value v_cs) {
  {
    u_value s655 = v_cs;
    int d656 = 0;
    if ((!d656) && u_is_con(s655, &n_list_46_Nil)) {
      d656 = 1;
      {
      }
    }
    if ((!d656) && u_is_con(s655, &n_list_46_Cons)) {
      u_value v_p = u_field(u_field(s655, 0), 0);
      u_value v_open = u_field(u_field(s655, 0), 1);
      u_value v_rest = u_field(s655, 1);
      d656 = 1;
      {
        u_value t657 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e658 = &t657;
        u_value l659 = v_before;
        while (u_is_con(l659, &n_list_46_Cons)) {
          u_value v_q = u_field(l659, 0);
          if (u_truth(u_mk_bool(1))) {
            e658 = u_append(e658, u_mk_con(&n_list_46_Cons, 2, v_q, u_mk_con(&n_list_46_Nil, 0)));
          }
          l659 = u_field(l659, 1);
        }
        if (u_truth(f_prelude_46_not_1(f_check_46_useful_5(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t657, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)))))) {
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
    if (!d656) {
      u_no_case(s655);
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
    u_value s660 = v_p;
    int d661 = 0;
    if ((!d661) && u_is_con(s660, &n_syntax_46_PWild)) {
      d661 = 1;
      {
        u_value t662 = v_p;
        u_value t663 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t662;
        *r_xs = t663;
      }
    }
    if ((!d661) && u_is_con(s660, &n_syntax_46_PLit)) {
      u_value v_v = u_field(s660, 0);
      d661 = 1;
      {
        p_check_46_unify(f_check_46_literal_1(v_v), v_t, r_s);
        u_value t664 = v_p;
        u_value t665 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t664;
        *r_xs = t665;
      }
    }
    if ((!d661) && (u_is_con(s660, &n_syntax_46_PVar) && u_is_con(u_field(s660, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s660, 0), 0);
      u_value v_a = u_field(u_field(s660, 0), 1);
      d661 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam666, 1, *r_s)), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            p_check_46_binders(u_mk_con(&n_syntax_46_PApp, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_t, r_p1, r_xs, r_s);
          }
        } else {
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_u, v_t, r_s);
            u_value t667 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u));
            u_value t668 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), u_mk_con(&n_list_46_Nil, 0));
            *r_p1 = t667;
            *r_xs = t668;
          }
        }
      }
    }
    if ((!d661) && u_is_con(s660, &n_syntax_46_PApp)) {
      u_value v_n = u_field(s660, 0);
      u_value v_args = u_field(s660, 1);
      d661 = 1;
      {
        u_value s670 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam669, 1, *r_s));
        int d671 = 0;
        if ((!d671) && (u_is_con(s670, &n_list_46_Cons) && u_is_con(u_field(s670, 1), &n_list_46_Nil))) {
          u_value v_k1 = u_field(s670, 0);
          d671 = 1;
          {
            u_value t672 = v_k1;
            v_k = t672;
          }
        }
        if ((!d671) && u_is_con(s670, &n_list_46_Nil)) {
          d671 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d671) {
          u_value v_ks = s670;
          d671 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ks), "check.TypeError");
          }
        }
        if (!d671) {
          u_no_case(s670);
        }
        u_value s673 = f_ctx_46_constr_of_2(f_check_46_State_46_ctx_1(*r_s), v_k);
        int d674 = 0;
        if ((!d674) && u_is_con(s673, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s673, 0), 0);
          u_value v_ks = u_field(u_field(s673, 0), 1);
          u_value v_r = u_field(u_field(s673, 0), 2);
          d674 = 1;
          {
            p_check_46_same_length(f_list_46_length_1(v_ks), f_list_46_length_1(v_args));
            p_check_46_instance(v_ps, &v_m, r_s);
            p_check_46_unify(f_check_46_subst_2(v_r, v_m), v_t, r_s);
            u_value t675 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e676 = &t675;
            u_value l677 = v_ks;
            while (u_is_con(l677, &n_list_46_Cons)) {
              u_value v_kt = u_field(l677, 0);
              if (u_truth(u_mk_bool(1))) {
                e676 = u_append(e676, f_check_46_subst_2(v_kt, v_m));
              }
              l677 = u_field(l677, 1);
            }
            p_check_46_binder_list(v_args, t675, &v_qs, r_xs, r_s);
            u_value t678 = u_mk_con(&n_syntax_46_PApp, 2, v_k, v_qs);
            *r_p1 = t678;
          }
        }
        if ((!d674) && u_is_con(s673, &n_option_46_None)) {
          d674 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d674) {
          u_no_case(s673);
        }
      }
    }
    if ((!d661) && u_is_con(s660, &n_syntax_46_PTuple)) {
      u_value v_items = u_field(s660, 0);
      d661 = 1;
      {
        p_check_46_fresh_list(v_items, &v_ts, r_s);
        p_check_46_unify(u_mk_con(&n_syntax_46_TTuple, 1, v_ts), v_t, r_s);
        p_check_46_binder_list(v_items, v_ts, &v_qs, r_xs, r_s);
        u_value t679 = u_mk_con(&n_syntax_46_PTuple, 1, v_qs);
        *r_p1 = t679;
      }
    }
    if (!d661) {
      u_no_case(s660);
    }
  }
}

void p_check_46_binder_list(u_value v_ps, u_value v_ts, u_value *r_qs, u_value *r_xs, u_value *r_s) {
  {
    u_value v_q = NULL;
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value v_zs = NULL;
    u_value s680 = u_mk_tuple(2, v_ps, v_ts);
    int d681 = 0;
    if ((!d681) && (u_is_con(u_field(s680, 0), &n_list_46_Cons) && u_is_con(u_field(s680, 1), &n_list_46_Cons))) {
      u_value v_p = u_field(u_field(s680, 0), 0);
      u_value v_more = u_field(u_field(s680, 0), 1);
      u_value v_t = u_field(u_field(s680, 1), 0);
      u_value v_ts1 = u_field(u_field(s680, 1), 1);
      d681 = 1;
      {
        p_check_46_binders(v_p, v_t, &v_q, &v_ys, r_s);
        p_check_46_binder_list(v_more, v_ts1, &v_rest, &v_zs, r_s);
        u_value t682 = u_mk_con(&n_list_46_Cons, 2, v_q, v_rest);
        u_value t683 = f_list_46__43__43__2(v_ys, v_zs);
        *r_qs = t682;
        *r_xs = t683;
      }
    }
    if ((!d681) && (u_is_con(u_field(s680, 0), &n_list_46_Nil) && u_is_con(u_field(s680, 1), &n_list_46_Nil))) {
      d681 = 1;
      {
        u_value t684 = u_mk_con(&n_list_46_Nil, 0);
        u_value t685 = u_mk_con(&n_list_46_Nil, 0);
        *r_qs = t684;
        *r_xs = t685;
      }
    }
    if (!d681) {
      d681 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_ps)), "check.TypeError");
      }
    }
    if (!d681) {
      u_no_case(s680);
    }
  }
}

void p_check_46_variable(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s686 = v_x;
    int d687 = 0;
    if ((!d687) && u_is_con(s686, &n_syntax_46_Var)) {
      u_value v_n = u_field(s686, 0);
      u_value v_a = u_field(s686, 1);
      d687 = 1;
      {
        if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, v_assignable)))) {
          {
            u_raise(u_mk_con(&n_check_46_NotAssignable, 1, v_n), "check.TypeError");
          }
        }
        u_value s688 = f_list_46_lookup_2(v_n, v_env);
        int d689 = 0;
        if ((!d689) && u_is_con(s688, &n_option_46_Some)) {
          u_value v_t = u_field(u_field(s688, 0), 1);
          d689 = 1;
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_t, v_u, r_s);
            u_value t690 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_t);
            *r_y = t690;
          }
        }
        if ((!d689) && u_is_con(s688, &n_option_46_None)) {
          d689 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
          }
        }
        if (!d689) {
          u_no_case(s688);
        }
      }
    }
    if (!d687) {
      u_no_case(s686);
    }
  }
}

void p_check_46_variables(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s691 = v_xs;
    int d692 = 0;
    if ((!d692) && u_is_con(s691, &n_list_46_Nil)) {
      d692 = 1;
      {
        u_value t693 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t693;
      }
    }
    if ((!d692) && u_is_con(s691, &n_list_46_Cons)) {
      u_value v_x = u_field(s691, 0);
      u_value v_more = u_field(s691, 1);
      d692 = 1;
      {
        p_check_46_variable(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_variables(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t694 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t694;
      }
    }
    if (!d692) {
      u_no_case(s691);
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
    u_value s695 = v_x;
    int d696 = 0;
    if ((!d696) && u_is_con(s695, &n_syntax_46_SSkip)) {
      d696 = 1;
      {
        u_value t697 = v_x;
        *r_y = t697;
      }
    }
    if ((!d696) && u_is_con(s695, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s695, 0);
      d696 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)));
        p_check_46_variables(v_env, v_assignable, f_list_46_firsts_1(v_pairs), &v_ys, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_pairs), f_syntax_46_types_1(v_ys), &v_es, r_s);
        u_value t698 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_zip_2(v_ys, v_es));
        *r_y = t698;
      }
    }
    if ((!d696) && u_is_con(s695, &n_syntax_46_SBlock)) {
      u_value v_locals = u_field(s695, 0);
      u_value v_body = u_field(s695, 1);
      d696 = 1;
      {
        p_check_46_untyped(v_locals, f_check_46_initialized_1(v_body));
        p_check_46_formals(v_locals, &v_ys, r_s);
        p_check_46_statements(f_list_46__43__43__2(f_check_46_bind_1(v_ys), v_env), f_list_46__43__43__2(f_syntax_46_names_1(v_ys), v_assignable), v_body, &v_xs, r_s);
        u_value t699 = u_mk_con(&n_syntax_46_SBlock, 2, v_ys, v_xs);
        *r_y = t699;
      }
    }
    if ((!d696) && u_is_con(s695, &n_syntax_46_SIf)) {
      u_value v_cond = u_field(s695, 0);
      u_value v_left = u_field(s695, 1);
      u_value v_right = u_field(s695, 2);
      d696 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_left, &v_l, r_s);
        p_check_46_statement(v_env, v_assignable, v_right, &v_r, r_s);
        u_value t700 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
        *r_y = t700;
      }
    }
    if ((!d696) && u_is_con(s695, &n_syntax_46_SWhile)) {
      u_value v_cond = u_field(s695, 0);
      u_value v_body = u_field(s695, 1);
      d696 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        u_value t701 = u_mk_con(&n_syntax_46_SWhile, 2, v_c, v_l);
        *r_y = t701;
      }
    }
    if ((!d696) && u_is_con(s695, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s695, 0);
      u_value v_ins = u_field(s695, 1);
      u_value v_outs = u_field(s695, 2);
      u_value v_refs = u_field(s695, 3);
      d696 = 1;
      {
        p_check_46_call_proc(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d696) && u_is_con(s695, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s695, 0);
      u_value v_cases = u_field(s695, 1);
      d696 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_cases, v_t, &v_bs, r_s);
        u_value t702 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e703 = &t702;
        u_value l704 = v_bs;
        while (u_is_con(l704, &n_list_46_Cons)) {
          u_value v_br = u_field(l704, 0);
          if (u_truth(u_mk_bool(1))) {
            e703 = u_append(e703, f_check_46_case_of_1(v_br));
          }
          l704 = u_field(l704, 1);
        }
        p_check_46_coverage(*r_s, v_t, t702);
        u_value t705 = u_mk_con(&n_syntax_46_SMatch, 2, v_c, v_bs);
        *r_y = t705;
      }
    }
    if ((!d696) && u_is_con(s695, &n_syntax_46_SAssert)) {
      u_value v_cond = u_field(s695, 0);
      d696 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        u_value t706 = u_mk_con(&n_syntax_46_SAssert, 1, v_c);
        *r_y = t706;
      }
    }
    if ((!d696) && u_is_con(s695, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s695, 0);
      u_value v_a = u_field(s695, 1);
      d696 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_unify(v_t, v_u, r_s);
        u_value t707 = u_mk_con(&n_syntax_46_SRaise, 2, v_c, v_t);
        *r_y = t707;
      }
    }
    if ((!d696) && u_is_con(s695, &n_syntax_46_STry)) {
      u_value v_body = u_field(s695, 0);
      u_value v_handlers = u_field(s695, 1);
      d696 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_handlers, &v_hs, r_s);
        u_value t708 = u_mk_con(&n_syntax_46_STry, 2, v_l, v_hs);
        *r_y = t708;
      }
    }
    if (!d696) {
      u_no_case(s695);
    }
  }
}

u_value f_check_46_initialized_1(u_value v_xs) {
  u_value s709 = v_xs;
  u_value t710 = NULL;
  if ((!t710) && (u_is_con(s709, &n_list_46_Cons) && (u_is_con(u_field(s709, 0), &n_syntax_46_SAssign) && (u_is_con(u_field(u_field(s709, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s709, 0), 0), 1), &n_list_46_Nil))))) {
    u_value v_x = u_field(u_field(u_field(u_field(s709, 0), 0), 0), 0);
    u_value v_rest = u_field(s709, 1);
    t710 = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), f_check_46_initialized_1(v_rest));
  }
  if (!t710) {
    t710 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t710) {
    u_no_case(s709);
  }
  return t710;
}

void p_check_46_untyped(u_value v_xs, u_value v_inits) {
  {
    u_value s711 = v_xs;
    int d712 = 0;
    if ((!d712) && u_is_con(s711, &n_list_46_Nil)) {
      d712 = 1;
      {
      }
    }
    if ((!d712) && (u_is_con(s711, &n_list_46_Cons) && u_is_con(u_field(s711, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s711, 0), 0);
      u_value v_t = u_field(u_field(s711, 0), 1);
      u_value v_rest = u_field(s711, 1);
      d712 = 1;
      {
        u_value t713 = f_check_46_holes_1(v_t);
        if (u_truth(t713)) {
          t713 = f_prelude_46_not_1(f_list_46_contains_2(v_n, v_inits));
        }
        if (u_truth(t713)) {
          {
            u_raise(u_mk_con(&n_check_46_MissingType, 1, v_n), "check.TypeError");
          }
        }
        p_check_46_untyped(v_rest, v_inits);
      }
    }
    if (!d712) {
      u_no_case(s711);
    }
  }
}

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s714 = v_xs;
    int d715 = 0;
    if ((!d715) && u_is_con(s714, &n_list_46_Nil)) {
      d715 = 1;
      {
        u_value t716 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t716;
      }
    }
    if ((!d715) && u_is_con(s714, &n_list_46_Cons)) {
      u_value v_x = u_field(s714, 0);
      u_value v_more = u_field(s714, 1);
      d715 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_statements(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t717 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t717;
      }
    }
    if (!d715) {
      u_no_case(s714);
    }
  }
}

u_value f_check_46_os_procs_1(u_value v_c) {
  u_value t722;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_718 = U_LIT("os_args");
    static struct u_object l_os_read_719 = U_LIT("os_read");
    static struct u_object l_os_write_720 = U_LIT("os_write");
    static struct u_object l_os_library_721 = U_LIT("os_library");
    t722 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_718), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_719), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_720), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_721), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)))));
  } else {
    t722 = u_mk_con(&n_list_46_Nil, 0);
  }
  return t722;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t723 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e724 = &t723;
  static struct u_object l_Program_725 = U_LIT("Program");
  u_value l726 = f_ctx_46_names_of_2(v_c, &l_Program_725);
  while (u_is_con(l726, &n_list_46_Cons)) {
    u_value v_d = u_field(l726, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e724 = u_append(e724, v_d);
    }
    l726 = u_field(l726, 1);
  }
  u_value s727 = t723;
  u_value t728 = NULL;
  if ((!t728) && u_is_con(s727, &n_list_46_Cons)) {
    u_value v_d = u_field(s727, 0);
    static struct u_object l_read_729 = U_LIT("read");
    static struct u_object l_library_730 = U_LIT("library");
    t728 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_729), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_730), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t728) {
    t728 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t728) {
    u_no_case(s727);
  }
  return t728;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s732 = f_check_46_visible_names_3(*r_s, v_p, u_mk_closure(lam731, 1, *r_s));
    int d733 = 0;
    if ((!d733) && (u_is_con(s732, &n_list_46_Cons) && u_is_con(u_field(s732, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s732, 0);
      d733 = 1;
      {
        u_value t734 = v_q1;
        v_q = t734;
      }
    }
    if ((!d733) && u_is_con(s732, &n_list_46_Nil)) {
      d733 = 1;
      {
        u_value t735 = v_p;
        v_q = t735;
      }
    }
    if (!d733) {
      u_value v_qs = s732;
      d733 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d733) {
      u_no_case(s732);
    }
    u_value s736 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_State_46_ctx_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_State_46_ctx_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_State_46_ctx_1(*r_s)))));
    int d737 = 0;
    if ((!d737) && u_is_con(s736, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s736, 0), 0);
      u_value v_is = u_field(u_field(s736, 0), 1);
      u_value v_os = u_field(u_field(s736, 0), 2);
      u_value v_rs = u_field(u_field(s736, 0), 3);
      d737 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t738 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e739 = &t738;
        u_value l740 = v_is;
        while (u_is_con(l740, &n_list_46_Cons)) {
          u_value v_t = u_field(l740, 0);
          if (u_truth(u_mk_bool(1))) {
            e739 = u_append(e739, f_check_46_subst_2(v_t, v_m));
          }
          l740 = u_field(l740, 1);
        }
        p_check_46_checks(v_env, v_ins, t738, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t741 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e742 = &t741;
        u_value l743 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l743, &n_list_46_Cons)) {
          u_value v_t = u_field(l743, 0);
          if (u_truth(u_mk_bool(1))) {
            e742 = u_append(e742, f_check_46_subst_2(v_t, v_m));
          }
          l743 = u_field(l743, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t741, r_s);
        u_value t744 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t744;
      }
    }
    if ((!d737) && u_is_con(s736, &n_option_46_None)) {
      d737 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d737) {
      u_no_case(s736);
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
    u_value s745 = v_cases;
    int d746 = 0;
    if ((!d746) && u_is_con(s745, &n_list_46_Nil)) {
      d746 = 1;
      {
        u_value t747 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t747;
      }
    }
    if ((!d746) && (u_is_con(s745, &n_list_46_Cons) && u_is_con(u_field(s745, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s745, 0), 0);
      u_value v_g = u_field(u_field(s745, 0), 1);
      u_value v_b = u_field(u_field(s745, 0), 2);
      u_value v_more = u_field(s745, 1);
      d746 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t748 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t748;
      }
    }
    if (!d746) {
      u_no_case(s745);
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
    u_value s749 = v_hs;
    int d750 = 0;
    if ((!d750) && u_is_con(s749, &n_list_46_Nil)) {
      d750 = 1;
      {
        u_value t751 = u_mk_con(&n_list_46_Nil, 0);
        *r_hs1 = t751;
      }
    }
    if ((!d750) && (u_is_con(s749, &n_list_46_Cons) && u_is_con(u_field(s749, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s749, 0), 0);
      u_value v_p = u_field(u_field(s749, 0), 1);
      u_value v_g = u_field(u_field(s749, 0), 2);
      u_value v_b = u_field(u_field(s749, 0), 3);
      u_value v_more = u_field(s749, 1);
      d750 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t752 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t752;
      }
    }
    if (!d750) {
      u_no_case(s749);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t753 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t753)) {
      t753 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t754 = t753;
    if (!u_truth(t754)) {
      t754 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t755 = t754;
    if (!u_truth(t755)) {
      t755 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n))), u_mk_con(&n_list_46_Nil, 0));
    }
    if (u_truth(t755)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t756 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t756)) {
      t756 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t757 = t756;
    if (!u_truth(t757)) {
      t757 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t757)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_758 = U_LIT("Bool");
    static struct u_object l_Int_759 = U_LIT("Int");
    static struct u_object l_Str_760 = U_LIT("Str");
    static struct u_object l_Char_761 = U_LIT("Char");
    static struct u_object l__45__62__762 = U_LIT("->");
    u_value t763 = f_list_46_contains_2(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_758, u_mk_con(&n_list_46_Cons, 2, &l_Int_759, u_mk_con(&n_list_46_Cons, 2, &l_Str_760, u_mk_con(&n_list_46_Cons, 2, &l_Char_761, u_mk_con(&n_list_46_Cons, 2, &l__45__62__762, u_mk_con(&n_list_46_Nil, 0)))))));
    if (!u_truth(t763)) {
      t763 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t764 = t763;
    if (!u_truth(t764)) {
      t764 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t764)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t765 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e766 = &t765;
    u_value l767 = v_ts;
    while (u_is_con(l767, &n_list_46_Cons)) {
      u_value v_t = u_field(l767, 0);
      if (u_truth(u_mk_bool(1))) {
        e766 = u_append(e766, f_check_46_free_1(v_t));
      }
      l767 = u_field(l767, 1);
    }
    u_value t768 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t765)), f_check_46_params_1(v_ps));
    v_undeclared = t768;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, u_mk_con(&n_list_46_Nil, 0)))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
  {
    u_value s769 = v_cs;
    int d770 = 0;
    if ((!d770) && u_is_con(s769, &n_list_46_Nil)) {
      d770 = 1;
      {
      }
    }
    if ((!d770) && (u_is_con(s769, &n_list_46_Cons) && u_is_con(u_field(u_field(s769, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s769, 0), 0);
      u_value v_n = u_field(u_field(u_field(s769, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s769, 0), 1), 1);
      u_value v_rest = u_field(s769, 1);
      d770 = 1;
      {
        u_value t771 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t771;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        u_value t772 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t772;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d770) && u_is_con(s769, &n_list_46_Cons)) {
      u_value v_rest = u_field(s769, 1);
      d770 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d770) {
      u_no_case(s769);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s773 = v_cs;
    int d774 = 0;
    if ((!d774) && u_is_con(s773, &n_list_46_Nil)) {
      d774 = 1;
      {
      }
    }
    if ((!d774) && (u_is_con(s773, &n_list_46_Cons) && u_is_con(u_field(u_field(s773, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s773, 0), 0);
      u_value v_n = u_field(u_field(u_field(s773, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s773, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s773, 0), 1), 2);
      u_value v_rest = u_field(s773, 1);
      d774 = 1;
      {
        u_value t775 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t775;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)));
        u_value t776 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_State_46_ctx_1(*r_s), v_n, v_ps, v_u));
        *r_s = t776;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d774) && u_is_con(s773, &n_list_46_Cons)) {
      u_value v_rest = u_field(s773, 1);
      d774 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d774) {
      u_no_case(s773);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
  {
    u_value s777 = v_cs;
    int d778 = 0;
    if ((!d778) && u_is_con(s777, &n_list_46_Nil)) {
      d778 = 1;
      {
      }
    }
    if ((!d778) && (u_is_con(s777, &n_list_46_Cons) && u_is_con(u_field(u_field(s777, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s777, 0), 0);
      u_value v_n = u_field(u_field(u_field(s777, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s777, 0), 1), 1);
      u_value v_ks = u_field(u_field(u_field(s777, 0), 1), 2);
      u_value v_rest = u_field(s777, 1);
      d778 = 1;
      {
        u_value t779 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t779;
        p_check_46_unique_ids(v_ps);
        u_value t780 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e781 = &t780;
        u_value l782 = v_ks;
        while (u_is_con(l782, &n_list_46_Cons)) {
          u_value v_k = u_field(l782, 0);
          if (u_truth(u_mk_bool(1))) {
            e781 = u_append(e781, f_syntax_46_Constr_46_name_1(v_k));
          }
          l782 = u_field(l782, 1);
        }
        u_value t783 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_State_46_ctx_1(*r_s), v_n, t780));
        *r_s = t783;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d778) && u_is_con(s777, &n_list_46_Cons)) {
      u_value v_rest = u_field(s777, 1);
      d778 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d778) {
      u_no_case(s777);
    }
  }
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s784 = v_ks;
    int d785 = 0;
    if ((!d785) && u_is_con(s784, &n_list_46_Nil)) {
      d785 = 1;
      {
      }
    }
    if ((!d785) && (u_is_con(s784, &n_list_46_Cons) && u_is_con(u_field(s784, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s784, 0), 0);
      u_value v_args = u_field(u_field(s784, 0), 1);
      u_value v_rest = u_field(s784, 1);
      d785 = 1;
      {
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t786 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_State_46_ctx_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t786;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d785) {
      u_no_case(s784);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s787 = v_xss;
  u_value t788 = NULL;
  if ((!t788) && u_is_con(s787, &n_list_46_Nil)) {
    t788 = v_r;
  }
  if ((!t788) && u_is_con(s787, &n_list_46_Cons)) {
    u_value v_xs = u_field(s787, 0);
    u_value v_rest = u_field(s787, 1);
    t788 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t788) {
    u_no_case(s787);
  }
  return t788;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s789 = v_xss;
  u_value t790 = NULL;
  if ((!t790) && u_is_con(s789, &n_list_46_Nil)) {
    t790 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t790) && u_is_con(s789, &n_list_46_Cons)) {
    u_value v_xs = u_field(s789, 0);
    t790 = v_xs;
  }
  if (!t790) {
    u_no_case(s789);
  }
  return t790;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s791 = v_xss;
    int d792 = 0;
    if ((!d792) && u_is_con(s791, &n_list_46_Nil)) {
      d792 = 1;
      {
        u_value t793 = u_mk_con(&n_list_46_Nil, 0);
        *r_yss = t793;
      }
    }
    if ((!d792) && u_is_con(s791, &n_list_46_Cons)) {
      u_value v_xs = u_field(s791, 0);
      u_value v_more = u_field(s791, 1);
      d792 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t794 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t794;
      }
    }
    if (!d792) {
      u_no_case(s791);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s795 = v_cs;
    int d796 = 0;
    if ((!d796) && u_is_con(s795, &n_list_46_Nil)) {
      d796 = 1;
      {
      }
    }
    if ((!d796) && (u_is_con(s795, &n_list_46_Cons) && u_is_con(u_field(u_field(s795, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s795, 0), 0);
      u_value v_n = u_field(u_field(u_field(s795, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s795, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s795, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s795, 0), 1), 3);
      u_value v_rest = u_field(s795, 1);
      d796 = 1;
      {
        u_value t797 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t797;
        p_check_46_overload(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), u_mk_con(&n_list_46_Nil, 0)));
        u_value t798 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t798;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d796) && u_is_con(s795, &n_list_46_Cons)) {
      u_value v_rest = u_field(s795, 1);
      d796 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d796) {
      u_no_case(s795);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s799 = v_cs;
    int d800 = 0;
    if ((!d800) && u_is_con(s799, &n_list_46_Nil)) {
      d800 = 1;
      {
      }
    }
    if ((!d800) && (u_is_con(s799, &n_list_46_Cons) && u_is_con(u_field(u_field(s799, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s799, 0), 0);
      u_value v_n = u_field(u_field(u_field(s799, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s799, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s799, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s799, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s799, 0), 1), 4);
      u_value v_rest = u_field(s799, 1);
      d800 = 1;
      {
        u_value t801 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t801;
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t802 = f_syntax_46_types_1(v_xs);
        v_ts = t802;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t803 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t803;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d800) && u_is_con(s799, &n_list_46_Cons)) {
      u_value v_rest = u_field(s799, 1);
      d800 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d800) {
      u_no_case(s799);
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
    u_value s804 = v_c;
    int d805 = 0;
    if ((!d805) && u_is_con(s804, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s804, 0);
      u_value v_ps = u_field(s804, 1);
      u_value v_bound = u_field(s804, 2);
      u_value v_res = u_field(s804, 3);
      u_value v_body = u_field(s804, 4);
      d805 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t806 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t806;
      }
    }
    if ((!d805) && u_is_con(s804, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s804, 0);
      u_value v_ps = u_field(s804, 1);
      u_value v_ins = u_field(s804, 2);
      u_value v_outs = u_field(s804, 3);
      u_value v_refs = u_field(s804, 4);
      u_value v_body = u_field(s804, 5);
      d805 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t807 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t807;
      }
    }
    if ((!d805) && u_is_con(s804, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s804, 0);
      d805 = 1;
      {
        p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_b, &v_t, r_s);
        u_value t808 = u_mk_con(&n_syntax_46_CEval, 1, v_b);
        *r_c1 = t808;
      }
    }
    if ((!d805) && u_is_con(s804, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s804, 0);
      u_value v_body = u_field(s804, 1);
      d805 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t809 = u_mk_con(&n_syntax_46_CRun, 2, v_n, v_y);
        *r_c1 = t809;
      }
    }
    if ((!d805) && u_is_con(s804, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s804, 0);
      u_value v_body = u_field(s804, 1);
      d805 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t810 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t810;
      }
    }
    if (!d805) {
      d805 = 1;
      {
        u_value t811 = v_c;
        *r_c1 = t811;
      }
    }
    if (!d805) {
      u_no_case(s804);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s812 = v_cs;
    int d813 = 0;
    if ((!d813) && u_is_con(s812, &n_list_46_Nil)) {
      d813 = 1;
      {
        u_value t814 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t814;
      }
    }
    if ((!d813) && u_is_con(s812, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s812, 0), 0);
      u_value v_c = u_field(u_field(s812, 0), 1);
      u_value v_more = u_field(s812, 1);
      d813 = 1;
      {
        u_value t815 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t815;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t816 = f_check_46_zonk_cmd_2(f_check_46_State_46_sub_1(*r_s), v_c1);
        v_c2 = t816;
        u_value s817 = f_check_46_unknown_in_cmd_1(v_c2);
        int d818 = 0;
        if ((!d818) && u_is_con(s817, &n_option_46_Some)) {
          u_value v_t = u_field(s817, 0);
          d818 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d818) && u_is_con(s817, &n_option_46_None)) {
          d818 = 1;
          {
          }
        }
        if (!d818) {
          u_no_case(s817);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t819 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t819;
      }
    }
    if (!d813) {
      u_no_case(s812);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s820 = v_cs;
  u_value t821 = NULL;
  if ((!t821) && u_is_con(s820, &n_list_46_Nil)) {
    t821 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t821) && (u_is_con(s820, &n_list_46_Cons) && u_is_con(u_field(u_field(s820, 0), 1), &n_syntax_46_CRun))) {
    u_value v_n = u_field(u_field(u_field(s820, 0), 1), 0);
    u_value v_more = u_field(s820, 1);
    t821 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t821) && (u_is_con(s820, &n_list_46_Cons) && u_is_con(u_field(u_field(s820, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s820, 0), 1), 0);
    u_value v_more = u_field(s820, 1);
    t821 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t821) && u_is_con(s820, &n_list_46_Cons)) {
    u_value v_more = u_field(s820, 1);
    t821 = f_check_46_block_names_1(v_more);
  }
  if (!t821) {
    u_no_case(s820);
  }
  return t821;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_s = NULL;
    u_value v_ds = NULL;
    u_value v_cs = NULL;
    u_value s822 = v_p;
    int d823 = 0;
    if ((!d823) && u_is_con(s822, &n_syntax_46_Program)) {
      u_value v_cmds = u_field(s822, 0);
      d823 = 1;
      {
        u_value t824 = f_check_46_flatten_2(v_main, v_cmds);
        v_ds = t824;
        u_value t825 = f_check_46_start_2(v_main, f_check_46_visibility_2(v_ds, v_main));
        v_s = t825;
        p_check_46_declare_datas(v_ds, &v_s);
        p_check_46_declare_sorts(v_ds, &v_s);
        p_check_46_declare_constrs(v_ds, &v_s);
        p_check_46_declare_funs(v_ds, &v_s);
        p_check_46_declare_procs(v_ds, &v_s);
        p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
        p_check_46_commands(v_ds, &v_cs, &v_s);
        u_value t826 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
        *r_q = t826;
      }
    }
    if (!d823) {
      u_no_case(s822);
    }
  }
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_827 = U_LIT("main");
    u_value t828 = f_check_46_start_2(&l_main_827, u_mk_con(&n_list_46_Nil, 0));
    v_s = t828;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t829 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(v_s), v_u);
    *r_t = t829;
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
  static struct u_object l_List_830 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_830), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_831 = U_LIT("list");
  static struct u_object l_List_832 = U_LIT("List");
  static struct u_object l_A_833 = U_LIT("A");
  static struct u_object l_Nil_834 = U_LIT("Nil");
  static struct u_object l_Cons_835 = U_LIT("Cons");
  static struct u_object l_A_836 = U_LIT("A");
  static struct u_object l_A_837 = U_LIT("A");
  static struct u_object l_list_838 = U_LIT("list");
  static struct u_object l_len_839 = U_LIT("len");
  static struct u_object l_A_840 = U_LIT("A");
  static struct u_object l_xs_841 = U_LIT("xs");
  static struct u_object l_A_842 = U_LIT("A");
  static struct u_object l_xs_843 = U_LIT("xs");
  static struct u_object l_Nil_844 = U_LIT("Nil");
  static struct u_object l_Cons_845 = U_LIT("Cons");
  static struct u_object l_ys_846 = U_LIT("ys");
  static struct u_object l__43__847 = U_LIT("+");
  static struct u_object l_len_848 = U_LIT("len");
  static struct u_object l_ys_849 = U_LIT("ys");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_831, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_832), u_mk_con(&n_list_46_Cons, 2, &l_A_833, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_834), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_835), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_836), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_837)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_838), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_839), u_mk_con(&n_list_46_Cons, 2, &l_A_840, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_841), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_842))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_843), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_844), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_845), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_846), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__847, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_848, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_849), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s850 = v_t;
  u_value t851 = NULL;
  if ((!t851) && u_is_con(s850, &n_c_46_CtVoid)) {
    static struct u_object l_void_852 = U_LIT("void");
    t851 = &l_void_852;
  }
  if ((!t851) && u_is_con(s850, &n_c_46_CtInt)) {
    static struct u_object l_int_853 = U_LIT("int");
    t851 = &l_int_853;
  }
  if ((!t851) && u_is_con(s850, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s850, 0);
    t851 = v_n;
  }
  if ((!t851) && (u_is_con(s850, &n_c_46_CtPtr) && u_is_con(u_field(s850, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s850, 0), 0);
    static struct u_object l__42__854 = U_LIT("*");
    t851 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__854);
  }
  if ((!t851) && u_is_con(s850, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s850, 0);
    static struct u_object l__32__42__855 = U_LIT(" *");
    t851 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__855);
  }
  if (!t851) {
    u_no_case(s850);
  }
  return t851;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s856 = v_t;
  u_value t857 = NULL;
  if ((!t857) && u_is_con(s856, &n_c_46_CtPtr)) {
    t857 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t857) {
    static struct u_object l__32__858 = U_LIT(" ");
    t857 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__858, v_x));
  }
  if (!t857) {
    u_no_case(s856);
  }
  return t857;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s859 = v_e;
  u_value t860 = NULL;
  if ((!t860) && u_is_con(s859, &n_c_46_CeVar)) {
    u_value v_x = u_field(s859, 0);
    t860 = v_x;
  }
  if ((!t860) && u_is_con(s859, &n_c_46_CeNum)) {
    u_value v_n = u_field(s859, 0);
    t860 = u_str_show(v_n);
  }
  if ((!t860) && u_is_con(s859, &n_c_46_CeStr)) {
    u_value v_s = u_field(s859, 0);
    t860 = u_str_quote(v_s);
  }
  if ((!t860) && u_is_con(s859, &n_c_46_CeCall)) {
    u_value v_f = u_field(s859, 0);
    u_value v_args = u_field(s859, 1);
    static struct u_object l__40__861 = U_LIT("(");
    static struct u_object l__44__32__862 = U_LIT(", ");
    u_value t863 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e864 = &t863;
    u_value l865 = v_args;
    while (u_is_con(l865, &n_list_46_Cons)) {
      u_value v_a = u_field(l865, 0);
      if (u_truth(u_mk_bool(1))) {
        e864 = u_append(e864, f_c_46_c_expr_1(v_a));
      }
      l865 = u_field(l865, 1);
    }
    static struct u_object l__41__866 = U_LIT(")");
    t860 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__861, u_concat(u_str_join(&l__44__32__862, t863), &l__41__866)));
  }
  if ((!t860) && u_is_con(s859, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s859, 0);
    u_value v_a = u_field(s859, 1);
    t860 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t860) && u_is_con(s859, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s859, 0);
    u_value v_a = u_field(s859, 1);
    u_value v_b = u_field(s859, 2);
    static struct u_object l__32__867 = U_LIT(" ");
    static struct u_object l__32__868 = U_LIT(" ");
    t860 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__867, u_concat(v_o, u_concat(&l__32__868, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t860) && u_is_con(s859, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s859, 0);
    u_value v_i = u_field(s859, 1);
    static struct u_object l__91__869 = U_LIT("[");
    static struct u_object l__93__870 = U_LIT("]");
    t860 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__869, u_concat(f_c_46_c_expr_1(v_i), &l__93__870)));
  }
  if ((!t860) && u_is_con(s859, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s859, 0);
    u_value v_f = u_field(s859, 1);
    static struct u_object l__45__62__871 = U_LIT("->");
    t860 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__871, v_f));
  }
  if ((!t860) && u_is_con(s859, &n_c_46_CeCast)) {
    u_value v_t = u_field(s859, 0);
    u_value v_a = u_field(s859, 1);
    static struct u_object l__40__872 = U_LIT("(");
    static struct u_object l__41__873 = U_LIT(")");
    t860 = u_concat(&l__40__872, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__873, f_c_46_c_operand_1(v_a))));
  }
  if ((!t860) && u_is_con(s859, &n_c_46_CeCond)) {
    u_value v_c = u_field(s859, 0);
    u_value v_a = u_field(s859, 1);
    u_value v_b = u_field(s859, 2);
    static struct u_object l__32__63__32__874 = U_LIT(" ? ");
    static struct u_object l__32__58__32__875 = U_LIT(" : ");
    t860 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__874, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__875, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t860) && u_is_con(s859, &n_c_46_CeInit)) {
    u_value v_es = u_field(s859, 0);
    static struct u_object l__123__876 = U_LIT("{");
    static struct u_object l__44__32__877 = U_LIT(", ");
    u_value t878 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e879 = &t878;
    u_value l880 = v_es;
    while (u_is_con(l880, &n_list_46_Cons)) {
      u_value v_a = u_field(l880, 0);
      if (u_truth(u_mk_bool(1))) {
        e879 = u_append(e879, f_c_46_c_expr_1(v_a));
      }
      l880 = u_field(l880, 1);
    }
    static struct u_object l__125__881 = U_LIT("}");
    t860 = u_concat(&l__123__876, u_concat(u_str_join(&l__44__32__877, t878), &l__125__881));
  }
  if (!t860) {
    u_no_case(s859);
  }
  return t860;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s882 = v_e;
  u_value t883 = NULL;
  if ((!t883) && u_is_con(s882, &n_c_46_CeVar)) {
    t883 = f_c_46_c_expr_1(v_e);
  }
  if ((!t883) && u_is_con(s882, &n_c_46_CeNum)) {
    u_value v_n = u_field(s882, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t883 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t883) && u_is_con(s882, &n_c_46_CeStr)) {
    t883 = f_c_46_c_expr_1(v_e);
  }
  if ((!t883) && u_is_con(s882, &n_c_46_CeCall)) {
    t883 = f_c_46_c_expr_1(v_e);
  }
  if ((!t883) && u_is_con(s882, &n_c_46_CeIndex)) {
    t883 = f_c_46_c_expr_1(v_e);
  }
  if ((!t883) && u_is_con(s882, &n_c_46_CeArrow)) {
    t883 = f_c_46_c_expr_1(v_e);
  }
  if (!t883) {
    static struct u_object l__40__884 = U_LIT("(");
    static struct u_object l__41__885 = U_LIT(")");
    t883 = u_concat(&l__40__884, u_concat(f_c_46_c_expr_1(v_e), &l__41__885));
  }
  if (!t883) {
    u_no_case(s882);
  }
  return t883;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s886 = v_s;
  u_value t887 = NULL;
  if ((!t887) && u_is_con(s886, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s886, 0);
    static struct u_object l__59__888 = U_LIT(";");
    t887 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__888)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t887) && (u_is_con(s886, &n_c_46_CsDecl) && u_is_con(u_field(s886, 2), &n_option_46_None))) {
    u_value v_t = u_field(s886, 0);
    u_value v_x = u_field(s886, 1);
    static struct u_object l__59__889 = U_LIT(";");
    t887 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__889)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t887) && (u_is_con(s886, &n_c_46_CsDecl) && u_is_con(u_field(s886, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s886, 0);
    u_value v_x = u_field(s886, 1);
    u_value v_e = u_field(u_field(s886, 2), 0);
    static struct u_object l__32__61__32__890 = U_LIT(" = ");
    static struct u_object l__59__891 = U_LIT(";");
    t887 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__890, u_concat(f_c_46_c_expr_1(v_e), &l__59__891)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t887) && u_is_con(s886, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s886, 0);
    u_value v_e = u_field(s886, 1);
    static struct u_object l__32__61__32__892 = U_LIT(" = ");
    static struct u_object l__59__893 = U_LIT(";");
    t887 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__892, u_concat(f_c_46_c_expr_1(v_e), &l__59__893)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t887) && (u_is_con(s886, &n_c_46_CsIf) && u_is_con(u_field(s886, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s886, 0);
    u_value v_l = u_field(s886, 1);
    static struct u_object l_if_32__40__894 = U_LIT("if (");
    static struct u_object l__41__32__123__895 = U_LIT(") {");
    static struct u_object l__32__32__896 = U_LIT("  ");
    static struct u_object l__125__897 = U_LIT("}");
    t887 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__894, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__895))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__896), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__897), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t887) && u_is_con(s886, &n_c_46_CsIf)) {
    u_value v_c = u_field(s886, 0);
    u_value v_l = u_field(s886, 1);
    u_value v_r = u_field(s886, 2);
    static struct u_object l_if_32__40__898 = U_LIT("if (");
    static struct u_object l__41__32__123__899 = U_LIT(") {");
    static struct u_object l__32__32__900 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__901 = U_LIT("} else {");
    static struct u_object l__32__32__902 = U_LIT("  ");
    static struct u_object l__125__903 = U_LIT("}");
    t887 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__898, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__899))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__900), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__901), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__902), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__903), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t887) && u_is_con(s886, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s886, 0);
    u_value v_b = u_field(s886, 1);
    static struct u_object l_while_32__40__904 = U_LIT("while (");
    static struct u_object l__41__32__123__905 = U_LIT(") {");
    static struct u_object l__32__32__906 = U_LIT("  ");
    static struct u_object l__125__907 = U_LIT("}");
    t887 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__904, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__905))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__906), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__907), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t887) && (u_is_con(s886, &n_c_46_CsReturn) && u_is_con(u_field(s886, 0), &n_option_46_None))) {
    static struct u_object l_return_59__908 = U_LIT("return;");
    t887 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__908), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t887) && (u_is_con(s886, &n_c_46_CsReturn) && u_is_con(u_field(s886, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s886, 0), 0);
    static struct u_object l_return_32__909 = U_LIT("return ");
    static struct u_object l__59__910 = U_LIT(";");
    t887 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__909, u_concat(f_c_46_c_expr_1(v_e), &l__59__910))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t887) && u_is_con(s886, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s886, 0);
    static struct u_object l__123__911 = U_LIT("{");
    static struct u_object l__32__32__912 = U_LIT("  ");
    static struct u_object l__125__913 = U_LIT("}");
    t887 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__911), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__912), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__913), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t887) {
    u_no_case(s886);
  }
  return t887;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t914 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e915 = &t914;
  u_value l916 = v_ss;
  while (u_is_con(l916, &n_list_46_Cons)) {
    u_value v_s = u_field(l916, 0);
    if (u_truth(u_mk_bool(1))) {
      e915 = u_append(e915, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l916 = u_field(l916, 1);
  }
  return f_list_46_concat_1(t914);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s917 = v_ps;
  u_value t918 = NULL;
  if ((!t918) && u_is_con(s917, &n_list_46_Nil)) {
    static struct u_object l_void_919 = U_LIT("void");
    t918 = &l_void_919;
  }
  if (!t918) {
    static struct u_object l__44__32__920 = U_LIT(", ");
    u_value t921 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e922 = &t921;
    u_value l923 = v_ps;
    while (u_is_con(l923, &n_list_46_Cons)) {
      u_value v_p = u_field(l923, 0);
      if (u_truth(u_mk_bool(1))) {
        e922 = u_append(e922, f_c_46_c_param_1(v_p));
      }
      l923 = u_field(l923, 1);
    }
    t918 = u_str_join(&l__44__32__920, t921);
  }
  if (!t918) {
    u_no_case(s917);
  }
  return t918;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s924 = v_p;
  u_value t925 = NULL;
  if (!t925) {
    u_value v_t = u_field(s924, 0);
    u_value v_x = u_field(s924, 1);
    t925 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t925) {
    u_no_case(s924);
  }
  return t925;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s926 = v_d;
  u_value t927 = NULL;
  if ((!t927) && u_is_con(s926, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s926, 0);
    static struct u_object l__35_include_32__928 = U_LIT("#include ");
    t927 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__928, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t927) && u_is_con(s926, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s926, 0);
    u_value v_f = u_field(s926, 1);
    u_value v_ps = u_field(s926, 2);
    static struct u_object l__40__929 = U_LIT("(");
    static struct u_object l__41__59__930 = U_LIT(");");
    t927 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__929, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__930))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t927) && u_is_con(s926, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s926, 0);
    u_value v_f = u_field(s926, 1);
    u_value v_ps = u_field(s926, 2);
    u_value v_b = u_field(s926, 3);
    static struct u_object l__40__931 = U_LIT("(");
    static struct u_object l__41__32__123__932 = U_LIT(") {");
    static struct u_object l__32__32__933 = U_LIT("  ");
    static struct u_object l__125__934 = U_LIT("}");
    t927 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__931, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__932))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__933, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__934, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t927) && (u_is_con(s926, &n_c_46_CdGlobal) && u_is_con(u_field(s926, 2), &n_option_46_None))) {
    u_value v_t = u_field(s926, 0);
    u_value v_x = u_field(s926, 1);
    static struct u_object l__59__935 = U_LIT(";");
    t927 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__935), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t927) && (u_is_con(s926, &n_c_46_CdGlobal) && u_is_con(u_field(s926, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s926, 0);
    u_value v_x = u_field(s926, 1);
    u_value v_e = u_field(u_field(s926, 2), 0);
    static struct u_object l__32__61__32__936 = U_LIT(" = ");
    static struct u_object l__59__937 = U_LIT(";");
    t927 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__936, u_concat(f_c_46_c_expr_1(v_e), &l__59__937))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t927) {
    u_no_case(s926);
  }
  return t927;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s938 = v_f;
  u_value t939 = NULL;
  if ((!t939) && u_is_con(s938, &n_c_46_CFile)) {
    u_value v_ds = u_field(s938, 0);
    static struct u_object l__10__10__940 = U_LIT("\n\n");
    u_value t941 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e942 = &t941;
    u_value l943 = v_ds;
    while (u_is_con(l943, &n_list_46_Cons)) {
      u_value v_d = u_field(l943, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__944 = U_LIT("\n");
        e942 = u_append(e942, u_str_join(&l__10__944, f_c_46_c_decl_1(v_d)));
      }
      l943 = u_field(l943, 1);
    }
    static struct u_object l__10__945 = U_LIT("\n");
    t939 = u_concat(u_str_join(&l__10__10__940, t941), &l__10__945);
  }
  if (!t939) {
    u_no_case(s938);
  }
  return t939;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__946 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__946, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__44__59__46__947 = U_LIT("()[],;.");
  return u_str_contains(&l__40__41__91__93__44__59__46__947, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t948 = u_is_letter(v_c);
  if (!u_truth(t948)) {
    t948 = u_is_digit(v_c);
  }
  u_value t949 = t948;
  if (!u_truth(t949)) {
    t949 = u_eq(v_c, u_mk_char(95));
  }
  return t949;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t950;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t950 = u_str_get(v_s, v_i);
  } else {
    t950 = u_chr(u_mk_int(0));
  }
  return t950;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s951 = v_c;
  u_value t952 = NULL;
  if ((!t952) && u_equals(s951, u_mk_char(34))) {
    t952 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t952) && u_equals(s951, u_mk_char(39))) {
    t952 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t952) && u_equals(s951, u_mk_char(92))) {
    t952 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t952) && u_equals(s951, u_mk_char(110))) {
    t952 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t952) && u_equals(s951, u_mk_char(116))) {
    t952 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t952) && u_equals(s951, u_mk_char(114))) {
    t952 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t952) {
    t952 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t952) {
    u_no_case(s951);
  }
  return t952;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t953;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t953 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t953 = v_i;
  }
  return t953;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t954 = u_mk_int(0);
    u_value t955 = u_mk_int(1);
    u_value t956 = u_mk_int(1);
    v_j = t954;
    *r_line = t955;
    *r_column = t956;
    u_value t961 = u_lt(v_j, v_i);
    while (u_truth(t961)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t957 = u_add(*r_line, u_mk_int(1));
            u_value t958 = u_mk_int(1);
            *r_line = t957;
            *r_column = t958;
          }
        } else {
          {
            u_value t959 = u_add(*r_column, u_mk_int(1));
            *r_column = t959;
          }
        }
        u_value t960 = u_add(v_j, u_mk_int(1));
        v_j = t960;
      }
      t961 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t962 = *r_i;
    u_value t963 = u_add(*r_i, u_mk_int(1));
    u_value t964 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t962;
    *r_i = t963;
    v_parts = t964;
    u_value t965 = *r_i;
    v_from = t965;
    u_value t976 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t976)) {
      {
        u_value t966 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t966;
        u_value t967 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t967)) {
          t967 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t967)) {
          {
            static struct u_object l_unterminated_32_lit_968 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_968, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s969 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d970 = 0;
            if ((!d970) && u_is_con(s969, &n_option_46_Some)) {
              u_value v_d = u_field(s969, 0);
              d970 = 1;
              {
                u_value t971 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t972 = u_add(*r_i, u_mk_int(2));
                v_parts = t971;
                *r_i = t972;
                u_value t973 = *r_i;
                v_from = t973;
              }
            }
            if ((!d970) && u_is_con(s969, &n_option_46_None)) {
              d970 = 1;
              {
                static struct u_object l_invalid_32_escape_974 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_974, *r_i), "lex.LexError");
              }
            }
            if (!d970) {
              u_no_case(s969);
            }
          }
        } else {
          {
            u_value t975 = u_add(*r_i, u_mk_int(1));
            *r_i = t975;
          }
        }
      }
      t976 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__977 = U_LIT("");
    u_value t978 = u_str_join(&l__977, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t979 = u_add(*r_i, u_mk_int(1));
    *r_text = t978;
    *r_i = t979;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t980 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t980;
    u_value t981 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t982 = v_j;
    *r_t = t981;
    *r_i = t982;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t983 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t983;
    u_value s984 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d985 = 0;
    if ((!d985) && u_is_con(s984, &n_option_46_Some)) {
      u_value v_n = u_field(s984, 0);
      d985 = 1;
      {
        u_value t986 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t987 = v_j;
        *r_t = t986;
        *r_i = t987;
      }
    }
    if ((!d985) && u_is_con(s984, &n_option_46_None)) {
      d985 = 1;
      {
        static struct u_object l_invalid_32_number_988 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_988, *r_i), "lex.LexError");
      }
    }
    if (!d985) {
      u_no_case(s984);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t989 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t989;
    u_value t990 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t991 = v_j;
    *r_t = t990;
    *r_i = t991;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t992 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t992;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t993 = *r_i;
    v_start = t993;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_994 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_994, v_start), "lex.LexError");
      }
    }
    u_value t995 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t995;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t996 = u_str_get(v_s, *r_i);
    v_c = t996;
    u_value s997 = v_c;
    int d998 = 0;
    if (!d998) {
      u_value t999 = u_is_letter(v_c);
      if (!u_truth(t999)) {
        t999 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t999)) {
        d998 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d998) {
      if (u_truth(u_is_digit(v_c))) {
        d998 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d998) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d998 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d998) && u_equals(s997, u_mk_char(34))) {
      d998 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d998) && u_equals(s997, u_mk_char(39))) {
      d998 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d998) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d998 = 1;
        {
          u_value t1000 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t1001 = u_add(*r_i, u_mk_int(1));
          *r_t = t1000;
          *r_i = t1001;
        }
      }
    }
    if (!d998) {
      d998 = 1;
      {
        static struct u_object l_unexpected_32_chara_1002 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_1002, *r_i), "lex.LexError");
      }
    }
    if (!d998) {
      u_no_case(s997);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t1003 = u_mk_int(0);
    v_i = t1003;
    u_value t1004 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1004;
    u_value t1011 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t1011)) {
      {
        u_value t1005 = v_i;
        v_start = t1005;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t1006 = u_add(v_i, u_mk_int(1));
            v_i = t1006;
          }
        } else {
          u_value t1007 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t1007)) {
            t1007 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t1007)) {
            {
              u_value t1009 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam1008, 0));
              v_i = t1009;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t1010 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t1010;
            }
          }
        }
      }
      t1011 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t1012 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t1012;
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
  u_value s1013 = v_s;
  u_value t1014 = NULL;
  if ((!t1014) && u_is_con(s1013, &n_set_46_SLeaf)) {
    t1014 = u_mk_bool(0);
  }
  if ((!t1014) && u_is_con(s1013, &n_set_46_SNode)) {
    u_value v_l = u_field(s1013, 0);
    u_value v_y = u_field(s1013, 1);
    u_value v_r = u_field(s1013, 2);
    u_value t1018 = u_compare(v_x, v_y);
    u_value t1017;
    {
      u_value v_c = t1018;
      u_value t1016;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t1016 = f_set_46_member_2(v_x, v_l);
      } else {
        u_value t1015;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t1015 = f_set_46_member_2(v_x, v_r);
        } else {
          t1015 = u_mk_bool(1);
        }
        t1016 = t1015;
      }
      t1017 = t1016;
    }
    t1014 = t1017;
  }
  if (!t1014) {
    u_no_case(s1013);
  }
  return t1014;
}

u_value f_set_46_insert_2(u_value v_x, u_value v_s) {
  u_value s1019 = v_s;
  u_value t1020 = NULL;
  if ((!t1020) && u_is_con(s1019, &n_set_46_SLeaf)) {
    t1020 = u_mk_con(&n_set_46_SNode, 4, u_mk_con(&n_set_46_SLeaf, 0), v_x, u_mk_con(&n_set_46_SLeaf, 0), u_mk_int(1));
  }
  if ((!t1020) && u_is_con(s1019, &n_set_46_SNode)) {
    u_value v_l = u_field(s1019, 0);
    u_value v_y = u_field(s1019, 1);
    u_value v_r = u_field(s1019, 2);
    u_value t1024 = u_compare(v_x, v_y);
    u_value t1023;
    {
      u_value v_c = t1024;
      u_value t1022;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t1022 = f_set_46_rebalance_3(f_set_46_insert_2(v_x, v_l), v_y, v_r);
      } else {
        u_value t1021;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t1021 = f_set_46_rebalance_3(v_l, v_y, f_set_46_insert_2(v_x, v_r));
        } else {
          t1021 = v_s;
        }
        t1022 = t1021;
      }
      t1023 = t1022;
    }
    t1020 = t1023;
  }
  if (!t1020) {
    u_no_case(s1019);
  }
  return t1020;
}

u_value f_set_46_from_list_1(u_value v_xs) {
  return f_set_46_insert_all_2(v_xs, u_mk_con(&n_set_46_SLeaf, 0));
}

u_value f_set_46_insert_all_2(u_value v_xs, u_value v_s) {
  u_value s1025 = v_xs;
  u_value t1026 = NULL;
  if ((!t1026) && u_is_con(s1025, &n_list_46_Nil)) {
    t1026 = v_s;
  }
  if ((!t1026) && u_is_con(s1025, &n_list_46_Cons)) {
    u_value v_x = u_field(s1025, 0);
    u_value v_rest = u_field(s1025, 1);
    t1026 = f_set_46_insert_all_2(v_rest, f_set_46_insert_2(v_x, v_s));
  }
  if (!t1026) {
    u_no_case(s1025);
  }
  return t1026;
}

u_value f_set_46_elements_1(u_value v_s) {
  return f_set_46_elements_onto_2(v_s, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_set_46_elements_onto_2(u_value v_s, u_value v_rest) {
  u_value s1027 = v_s;
  u_value t1028 = NULL;
  if ((!t1028) && u_is_con(s1027, &n_set_46_SLeaf)) {
    t1028 = v_rest;
  }
  if ((!t1028) && u_is_con(s1027, &n_set_46_SNode)) {
    u_value v_l = u_field(s1027, 0);
    u_value v_x = u_field(s1027, 1);
    u_value v_r = u_field(s1027, 2);
    t1028 = f_set_46_elements_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, v_x, f_set_46_elements_onto_2(v_r, v_rest)));
  }
  if (!t1028) {
    u_no_case(s1027);
  }
  return t1028;
}

u_value f_set_46_count_1(u_value v_s) {
  u_value s1029 = v_s;
  u_value t1030 = NULL;
  if ((!t1030) && u_is_con(s1029, &n_set_46_SLeaf)) {
    t1030 = u_mk_int(0);
  }
  if ((!t1030) && u_is_con(s1029, &n_set_46_SNode)) {
    u_value v_l = u_field(s1029, 0);
    u_value v_r = u_field(s1029, 2);
    t1030 = u_add(u_add(f_set_46_count_1(v_l), u_mk_int(1)), f_set_46_count_1(v_r));
  }
  if (!t1030) {
    u_no_case(s1029);
  }
  return t1030;
}

u_value f_set_46_depth_1(u_value v_s) {
  u_value s1031 = v_s;
  u_value t1032 = NULL;
  if ((!t1032) && u_is_con(s1031, &n_set_46_SLeaf)) {
    t1032 = u_mk_int(0);
  }
  if ((!t1032) && u_is_con(s1031, &n_set_46_SNode)) {
    u_value v_d = u_field(s1031, 3);
    t1032 = v_d;
  }
  if (!t1032) {
    u_no_case(s1031);
  }
  return t1032;
}

u_value f_set_46_snode_depth_2(u_value v_l, u_value v_r) {
  u_value t1033;
  if (u_truth(u_lt(f_set_46_depth_1(v_l), f_set_46_depth_1(v_r)))) {
    t1033 = f_set_46_depth_1(v_r);
  } else {
    t1033 = f_set_46_depth_1(v_l);
  }
  return u_add(u_mk_int(1), t1033);
}

u_value f_set_46_snode_3(u_value v_l, u_value v_x, u_value v_r) {
  return u_mk_con(&n_set_46_SNode, 4, v_l, v_x, v_r, f_set_46_snode_depth_2(v_l, v_r));
}

u_value f_set_46_turn_left_1(u_value v_s) {
  u_value s1034 = v_s;
  u_value t1035 = NULL;
  if ((!t1035) && (u_is_con(s1034, &n_set_46_SNode) && u_is_con(u_field(s1034, 2), &n_set_46_SNode))) {
    u_value v_a = u_field(s1034, 0);
    u_value v_x = u_field(s1034, 1);
    u_value v_b = u_field(u_field(s1034, 2), 0);
    u_value v_y = u_field(u_field(s1034, 2), 1);
    u_value v_c = u_field(u_field(s1034, 2), 2);
    t1035 = f_set_46_snode_3(f_set_46_snode_3(v_a, v_x, v_b), v_y, v_c);
  }
  if (!t1035) {
    t1035 = v_s;
  }
  if (!t1035) {
    u_no_case(s1034);
  }
  return t1035;
}

u_value f_set_46_turn_right_1(u_value v_s) {
  u_value s1036 = v_s;
  u_value t1037 = NULL;
  if ((!t1037) && (u_is_con(s1036, &n_set_46_SNode) && u_is_con(u_field(s1036, 0), &n_set_46_SNode))) {
    u_value v_a = u_field(u_field(s1036, 0), 0);
    u_value v_x = u_field(u_field(s1036, 0), 1);
    u_value v_b = u_field(u_field(s1036, 0), 2);
    u_value v_y = u_field(s1036, 1);
    u_value v_c = u_field(s1036, 2);
    t1037 = f_set_46_snode_3(v_a, v_x, f_set_46_snode_3(v_b, v_y, v_c));
  }
  if (!t1037) {
    t1037 = v_s;
  }
  if (!t1037) {
    u_no_case(s1036);
  }
  return t1037;
}

u_value f_set_46_smaller_1(u_value v_s) {
  u_value s1038 = v_s;
  u_value t1039 = NULL;
  if ((!t1039) && u_is_con(s1038, &n_set_46_SNode)) {
    u_value v_l = u_field(s1038, 0);
    t1039 = v_l;
  }
  if (!t1039) {
    t1039 = v_s;
  }
  if (!t1039) {
    u_no_case(s1038);
  }
  return t1039;
}

u_value f_set_46_greater_1(u_value v_s) {
  u_value s1040 = v_s;
  u_value t1041 = NULL;
  if ((!t1041) && u_is_con(s1040, &n_set_46_SNode)) {
    u_value v_r = u_field(s1040, 2);
    t1041 = v_r;
  }
  if (!t1041) {
    t1041 = v_s;
  }
  if (!t1041) {
    u_no_case(s1040);
  }
  return t1041;
}

u_value f_set_46_rebalance_3(u_value v_l, u_value v_x, u_value v_r) {
  u_value t1045;
  if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1))))) {
    u_value t1042;
    if (u_truth(u_lt(f_set_46_depth_1(f_set_46_smaller_1(v_l)), f_set_46_depth_1(f_set_46_greater_1(v_l))))) {
      t1042 = f_set_46_turn_left_1(v_l);
    } else {
      t1042 = v_l;
    }
    t1045 = f_set_46_turn_right_1(f_set_46_snode_3(t1042, v_x, v_r));
  } else {
    u_value t1044;
    if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1))))) {
      u_value t1043;
      if (u_truth(u_lt(f_set_46_depth_1(f_set_46_greater_1(v_r)), f_set_46_depth_1(f_set_46_smaller_1(v_r))))) {
        t1043 = f_set_46_turn_right_1(v_r);
      } else {
        t1043 = v_r;
      }
      t1044 = f_set_46_turn_left_1(f_set_46_snode_3(v_l, v_x, t1043));
    } else {
      t1044 = f_set_46_snode_3(v_l, v_x, v_r);
    }
    t1045 = t1044;
  }
  return t1045;
}

u_value f_set_46_well_formed_1(u_value v_s) {
  u_value s1046 = v_s;
  u_value t1047 = NULL;
  if ((!t1047) && u_is_con(s1046, &n_set_46_SLeaf)) {
    t1047 = u_mk_bool(1);
  }
  if ((!t1047) && u_is_con(s1046, &n_set_46_SNode)) {
    u_value v_l = u_field(s1046, 0);
    u_value v_r = u_field(s1046, 2);
    u_value v_d = u_field(s1046, 3);
    u_value t1048 = f_set_46_well_formed_1(v_l);
    if (u_truth(t1048)) {
      t1048 = f_set_46_well_formed_1(v_r);
    }
    u_value t1049 = t1048;
    if (u_truth(t1049)) {
      t1049 = u_eq(v_d, f_set_46_snode_depth_2(v_l, v_r));
    }
    u_value t1050 = t1049;
    if (u_truth(t1050)) {
      t1050 = f_prelude_46__60__61__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1)));
    }
    u_value t1051 = t1050;
    if (u_truth(t1051)) {
      t1051 = f_prelude_46__60__61__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1)));
    }
    t1047 = t1051;
  }
  if (!t1047) {
    u_no_case(s1046);
  }
  return t1047;
}

u_value f_set_46_upto_1(u_value v_n) {
  u_value t1052;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t1052 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1052 = f_list_46__43__43__2(f_set_46_upto_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t1052;
}

u_value f_parse_46_Notations_46_cmds_1(u_value v_x) {
  u_value s1053 = v_x;
  u_value t1054 = NULL;
  if ((!t1054) && u_is_con(s1053, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1053, 0);
    t1054 = v_y;
  }
  if (!t1054) {
    u_no_case(s1053);
  }
  return t1054;
}

u_value f_parse_46_Notations_46_ops_1(u_value v_x) {
  u_value s1055 = v_x;
  u_value t1056 = NULL;
  if ((!t1056) && u_is_con(s1055, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1055, 1);
    t1056 = v_y;
  }
  if (!t1056) {
    u_no_case(s1055);
  }
  return t1056;
}

u_value f_parse_46_Scope_46_params_1(u_value v_x) {
  u_value s1057 = v_x;
  u_value t1058 = NULL;
  if ((!t1058) && u_is_con(s1057, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1057, 0);
    t1058 = v_y;
  }
  if (!t1058) {
    u_no_case(s1057);
  }
  return t1058;
}

u_value f_parse_46_Scope_46_notations_1(u_value v_x) {
  u_value s1059 = v_x;
  u_value t1060 = NULL;
  if ((!t1060) && u_is_con(s1059, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1059, 1);
    t1060 = v_y;
  }
  if (!t1060) {
    u_no_case(s1059);
  }
  return t1060;
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
  static struct u_object l___1061 = U_LIT("_");
  static struct u_object l_assert_1062 = U_LIT("assert");
  static struct u_object l_assign_1063 = U_LIT("assign");
  static struct u_object l_call_1064 = U_LIT("call");
  static struct u_object l_case_1065 = U_LIT("case");
  static struct u_object l_catch_1066 = U_LIT("catch");
  static struct u_object l_data_1067 = U_LIT("data");
  static struct u_object l_else_1068 = U_LIT("else");
  static struct u_object l_end_1069 = U_LIT("end");
  static struct u_object l_eval_1070 = U_LIT("eval");
  static struct u_object l_false_1071 = U_LIT("false");
  static struct u_object l_for_1072 = U_LIT("for");
  static struct u_object l_function_1073 = U_LIT("function");
  static struct u_object l_if_1074 = U_LIT("if");
  static struct u_object l_import_1075 = U_LIT("import");
  static struct u_object l_in_1076 = U_LIT("in");
  static struct u_object l_infix_1077 = U_LIT("infix");
  static struct u_object l_lambda_1078 = U_LIT("lambda");
  static struct u_object l_let_1079 = U_LIT("let");
  static struct u_object l_local_1080 = U_LIT("local");
  static struct u_object l_match_1081 = U_LIT("match");
  static struct u_object l_out_1082 = U_LIT("out");
  static struct u_object l_postfix_1083 = U_LIT("postfix");
  static struct u_object l_prefix_1084 = U_LIT("prefix");
  static struct u_object l_procedure_1085 = U_LIT("procedure");
  static struct u_object l_raise_1086 = U_LIT("raise");
  static struct u_object l_ref_1087 = U_LIT("ref");
  static struct u_object l_run_1088 = U_LIT("run");
  static struct u_object l_skip_1089 = U_LIT("skip");
  static struct u_object l_test_1090 = U_LIT("test");
  static struct u_object l_then_1091 = U_LIT("then");
  static struct u_object l_true_1092 = U_LIT("true");
  static struct u_object l_type_1093 = U_LIT("type");
  static struct u_object l_try_1094 = U_LIT("try");
  static struct u_object l_while_1095 = U_LIT("while");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, &l___1061, u_mk_con(&n_list_46_Cons, 2, &l_assert_1062, u_mk_con(&n_list_46_Cons, 2, &l_assign_1063, u_mk_con(&n_list_46_Cons, 2, &l_call_1064, u_mk_con(&n_list_46_Cons, 2, &l_case_1065, u_mk_con(&n_list_46_Cons, 2, &l_catch_1066, u_mk_con(&n_list_46_Cons, 2, &l_data_1067, u_mk_con(&n_list_46_Cons, 2, &l_else_1068, u_mk_con(&n_list_46_Cons, 2, &l_end_1069, u_mk_con(&n_list_46_Cons, 2, &l_eval_1070, u_mk_con(&n_list_46_Cons, 2, &l_false_1071, u_mk_con(&n_list_46_Cons, 2, &l_for_1072, u_mk_con(&n_list_46_Cons, 2, &l_function_1073, u_mk_con(&n_list_46_Cons, 2, &l_if_1074, u_mk_con(&n_list_46_Cons, 2, &l_import_1075, u_mk_con(&n_list_46_Cons, 2, &l_in_1076, u_mk_con(&n_list_46_Cons, 2, &l_infix_1077, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1078, u_mk_con(&n_list_46_Cons, 2, &l_let_1079, u_mk_con(&n_list_46_Cons, 2, &l_local_1080, u_mk_con(&n_list_46_Cons, 2, &l_match_1081, u_mk_con(&n_list_46_Cons, 2, &l_out_1082, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1083, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1084, u_mk_con(&n_list_46_Cons, 2, &l_procedure_1085, u_mk_con(&n_list_46_Cons, 2, &l_raise_1086, u_mk_con(&n_list_46_Cons, 2, &l_ref_1087, u_mk_con(&n_list_46_Cons, 2, &l_run_1088, u_mk_con(&n_list_46_Cons, 2, &l_skip_1089, u_mk_con(&n_list_46_Cons, 2, &l_test_1090, u_mk_con(&n_list_46_Cons, 2, &l_then_1091, u_mk_con(&n_list_46_Cons, 2, &l_true_1092, u_mk_con(&n_list_46_Cons, 2, &l_type_1093, u_mk_con(&n_list_46_Cons, 2, &l_try_1094, u_mk_con(&n_list_46_Cons, 2, &l_while_1095, u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_list_46_contains_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1096 = U_LIT("=");
  static struct u_object l__61__62__1097 = U_LIT("=>");
  static struct u_object l__58__61__1098 = U_LIT(":=");
  static struct u_object l__58__1099 = U_LIT(":");
  static struct u_object l__124__1100 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1096, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1097, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1098, u_mk_con(&n_list_46_Cons, 2, &l__58__1099, u_mk_con(&n_list_46_Cons, 2, &l__124__1100, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1101 = v_ns;
  u_value t1102 = NULL;
  if ((!t1102) && u_is_con(s1101, &n_list_46_Nil)) {
    t1102 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1102) && (u_is_con(s1101, &n_list_46_Cons) && (u_is_con(u_field(s1101, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1101, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1101, 0), 1);
    u_value v_ops = u_field(u_field(s1101, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1102 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1102) && u_is_con(s1101, &n_list_46_Cons)) {
    u_value v_more = u_field(s1101, 1);
    t1102 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1102) {
    u_no_case(s1101);
  }
  return t1102;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1103 = v_ns;
  u_value t1104 = NULL;
  if ((!t1104) && u_is_con(s1103, &n_list_46_Nil)) {
    t1104 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1104) && (u_is_con(s1103, &n_list_46_Cons) && (u_is_con(u_field(s1103, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1103, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1103, 0), 1);
    u_value v_ops = u_field(u_field(s1103, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1104 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1104) && u_is_con(s1103, &n_list_46_Cons)) {
    u_value v_more = u_field(s1103, 1);
    t1104 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1104) {
    u_no_case(s1103);
  }
  return t1104;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1105 = v_ns;
  u_value t1106 = NULL;
  if ((!t1106) && u_is_con(s1105, &n_list_46_Nil)) {
    t1106 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1106) && (u_is_con(s1105, &n_list_46_Cons) && u_is_con(u_field(s1105, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1105, 0), 0);
    u_value v_p = u_field(u_field(s1105, 0), 1);
    u_value v_ops = u_field(u_field(s1105, 0), 2);
    u_value t1107 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1107)) {
      t1107 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1108 = t1107;
    if (u_truth(t1108)) {
      t1108 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1108)) {
      t1106 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1106) && u_is_con(s1105, &n_list_46_Cons)) {
    u_value v_more = u_field(s1105, 1);
    t1106 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1106) {
    u_no_case(s1105);
  }
  return t1106;
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
  u_value s1109 = v_cs;
  u_value t1110 = NULL;
  if ((!t1110) && u_is_con(s1109, &n_list_46_Nil)) {
    t1110 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1110) && (u_is_con(s1109, &n_list_46_Cons) && u_is_con(u_field(s1109, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1109, 0), 2);
    u_value v_more = u_field(s1109, 1);
    t1110 = f_list_46__43__43__2(v_ops, f_parse_46_operators_of_1(v_more));
  }
  if ((!t1110) && u_is_con(s1109, &n_list_46_Cons)) {
    u_value v_more = u_field(s1109, 1);
    t1110 = f_parse_46_operators_of_1(v_more);
  }
  if (!t1110) {
    u_no_case(s1109);
  }
  return t1110;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1111 = v_ts;
  u_value t1112 = NULL;
  if ((!t1112) && u_is_con(s1111, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1111, 0), 0);
    t1112 = v_t;
  }
  if (!t1112) {
    t1112 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1112) {
    u_no_case(s1111);
  }
  return t1112;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1113 = v_ts;
  u_value t1114 = NULL;
  if ((!t1114) && (u_is_con(s1113, &n_list_46_Cons) && u_is_con(u_field(s1113, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1113, 1), 0), 0);
    t1114 = v_t;
  }
  if (!t1114) {
    t1114 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1114) {
    u_no_case(s1113);
  }
  return t1114;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1115 = v_ts;
  u_value t1116 = NULL;
  if ((!t1116) && u_is_con(s1115, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1115, 0), 1);
    t1116 = v_i;
  }
  if (!t1116) {
    t1116 = u_mk_int(0);
  }
  if (!t1116) {
    u_no_case(s1115);
  }
  return t1116;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1117 = v_t;
  u_value t1118 = NULL;
  if ((!t1118) && u_is_con(s1117, &n_lex_46_TName)) {
    u_value v_n = u_field(s1117, 0);
    t1118 = v_n;
  }
  if ((!t1118) && u_is_con(s1117, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1117, 0);
    t1118 = v_s;
  }
  if ((!t1118) && u_is_con(s1117, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1117, 0);
    t1118 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1118) && u_is_con(s1117, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1117, 0);
    t1118 = u_str_show(v_n);
  }
  if ((!t1118) && u_is_con(s1117, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1117, 0);
    t1118 = u_str_quote(v_s);
  }
  if ((!t1118) && u_is_con(s1117, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1117, 0);
    static struct u_object l__39__1119 = U_LIT("'");
    static struct u_object l__39__1120 = U_LIT("'");
    t1118 = u_concat(&l__39__1119, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1120));
  }
  if ((!t1118) && u_is_con(s1117, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1121 = U_LIT("end of input");
    t1118 = &l_end_32_of_32_input_1121;
  }
  if (!t1118) {
    u_no_case(s1117);
  }
  return t1118;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1122 = f_parse_46_head_token_1(v_ts);
  u_value t1123 = NULL;
  if ((!t1123) && u_is_con(s1122, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1122, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1123 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1123) && u_is_con(s1122, &n_lex_46_TName)) {
    u_value v_n = u_field(s1122, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1123 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1123) {
    t1123 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1123) {
    u_no_case(s1122);
  }
  return t1123;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1124 = *r_ts;
    int d1125 = 0;
    if ((!d1125) && u_is_con(s1124, &n_list_46_Cons)) {
      u_value v_more = u_field(s1124, 1);
      d1125 = 1;
      {
        u_value t1126 = v_more;
        *r_ts = t1126;
      }
    }
    if (!d1125) {
      d1125 = 1;
      {
      }
    }
    if (!d1125) {
      u_no_case(s1124);
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
        static struct u_object l__32_expected_32_but_32__1127 = U_LIT(" expected but ");
        static struct u_object l__32_found_1128 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1127, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1128))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1129 = f_parse_46_head_token_1(*r_ts);
    int d1130 = 0;
    if ((!d1130) && u_is_con(s1129, &n_lex_46_TName)) {
      u_value v_m = u_field(s1129, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1130 = 1;
        {
          u_value t1131 = v_m;
          *r_n = t1131;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1130) {
      d1130 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1132 = U_LIT("name expected but ");
        static struct u_object l__32_found_1133 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1132, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1133)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1130) {
      u_no_case(s1129);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1134 = f_parse_46_head_token_1(*r_ts);
    int d1135 = 0;
    if ((!d1135) && u_is_con(s1134, &n_lex_46_TName)) {
      u_value v_m = u_field(s1134, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_m))) {
        d1135 = 1;
        {
          static struct u_object l_operator_32__1136 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1137 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1136, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1137)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1135) {
      d1135 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1135) {
      u_no_case(s1134);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1138 = f_parse_46_head_token_1(*r_ts);
    int d1139 = 0;
    if ((!d1139) && u_is_con(s1138, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1138, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1139 = 1;
        {
          u_value t1140 = v_s;
          *r_n = t1140;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1139) {
      d1139 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1139) {
      u_no_case(s1138);
    }
  }
}

u_value f_parse_46_Ops_46_notations_1(u_value v_x) {
  u_value s1141 = v_x;
  u_value t1142 = NULL;
  if ((!t1142) && u_is_con(s1141, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1141, 0);
    t1142 = v_y;
  }
  if (!t1142) {
    u_no_case(s1141);
  }
  return t1142;
}

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x) {
  u_value s1143 = v_x;
  u_value t1144 = NULL;
  if ((!t1144) && u_is_con(s1143, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1143, 1);
    t1144 = v_y;
  }
  if (!t1144) {
    u_no_case(s1143);
  }
  return t1144;
}

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x) {
  u_value s1145 = v_x;
  u_value t1146 = NULL;
  if ((!t1146) && u_is_con(s1145, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1145, 2);
    t1146 = v_y;
  }
  if (!t1146) {
    u_no_case(s1145);
  }
  return t1146;
}

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x) {
  u_value s1147 = v_x;
  u_value t1148 = NULL;
  if ((!t1148) && u_is_con(s1147, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1147, 3);
    t1148 = v_y;
  }
  if (!t1148) {
    u_no_case(s1147);
  }
  return t1148;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1149 = v_rest;
    int d1150 = 0;
    if ((!d1150) && u_is_con(s1149, &n_list_46_Nil)) {
      d1150 = 1;
      {
      }
    }
    if ((!d1150) && (u_is_con(s1149, &n_list_46_Cons) && u_is_con(u_field(s1149, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1149, 0), 0);
      u_value v_i = u_field(u_field(s1149, 0), 1);
      d1150 = 1;
      {
        static struct u_object l_unexpected_32_opera_1151 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1151, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1150) {
      d1150 = 1;
      {
        static struct u_object l_missing_32_operator_1152 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1152, v_at), "parse.ParseError");
      }
    }
    if (!d1150) {
      u_no_case(s1149);
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
    u_value s1153 = v_items;
    int d1154 = 0;
    if ((!d1154) && (u_is_con(s1153, &n_list_46_Cons) && u_is_con(u_field(s1153, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1153, 0), 0);
      u_value v_more = u_field(s1153, 1);
      d1154 = 1;
      {
        u_value t1155 = v_x;
        u_value t1156 = v_more;
        *r_a = t1155;
        *r_rest = t1156;
      }
    }
    if ((!d1154) && (u_is_con(s1153, &n_list_46_Cons) && u_is_con(u_field(s1153, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1153, 0), 0);
      u_value v_i = u_field(u_field(s1153, 0), 1);
      u_value v_more = u_field(s1153, 1);
      d1154 = 1;
      {
        u_value s1157 = f_parse_46_prefix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op);
        int d1158 = 0;
        if ((!d1158) && u_is_con(s1157, &n_option_46_Some)) {
          u_value v_p = u_field(s1157, 0);
          d1158 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1159 = u_apply(f_parse_46_Ops_46_prefix_op_1(v_o), 2, v_op, v_b);
            *r_a = t1159;
          }
        }
        if ((!d1158) && u_is_con(s1157, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op))) {
            d1158 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1160 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1160), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1158) && u_is_con(s1157, &n_option_46_None)) {
          d1158 = 1;
          {
            static struct u_object l_undeclared_32_opera_1161 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1161, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1158) {
          u_no_case(s1157);
        }
      }
    }
    if ((!d1154) && u_is_con(s1153, &n_list_46_Nil)) {
      d1154 = 1;
      {
        static struct u_object l_missing_32_operand_1162 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1162, v_at), "parse.ParseError");
      }
    }
    if (!d1154) {
      u_no_case(s1153);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1163 = v_items;
    int d1164 = 0;
    if ((!d1164) && (u_is_con(s1163, &n_list_46_Cons) && u_is_con(u_field(s1163, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1163, 0), 0);
      u_value v_i = u_field(u_field(s1163, 0), 1);
      u_value v_tail = u_field(s1163, 1);
      d1164 = 1;
      {
        u_value s1165 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op), f_parse_46_infix_of_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op));
        int d1166 = 0;
        if ((!d1166) && u_is_con(u_field(s1165, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1165, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1166 = 1;
            {
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_postfix_op_1(v_o), 2, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1166) && u_is_con(u_field(s1165, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1165, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1165, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1166 = 1;
            {
              u_value t1167;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1167 = v_p;
              } else {
                t1167 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1167, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)));
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_infix_op_1(v_o), 3, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1166) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op)))) {
            d1166 = 1;
            {
              static struct u_object l_undeclared_32_opera_1168 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1168, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1166) {
          d1166 = 1;
          {
            u_value t1169 = v_lhs;
            u_value t1170 = v_items;
            *r_a = t1169;
            *r_rest = t1170;
          }
        }
        if (!d1166) {
          u_no_case(s1165);
        }
      }
    }
    if (!d1164) {
      d1164 = 1;
      {
        u_value t1171 = v_lhs;
        u_value t1172 = v_items;
        *r_a = t1171;
        *r_rest = t1172;
      }
    }
    if (!d1164) {
      u_no_case(s1163);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1173 = v_rest;
    int d1174 = 0;
    if ((!d1174) && (u_is_con(s1173, &n_list_46_Cons) && u_is_con(u_field(s1173, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1173, 0), 0);
      u_value v_i = u_field(u_field(s1173, 0), 1);
      u_value t1177 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1177)) {
        t1177 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1177)) {
        d1174 = 1;
        {
          static struct u_object l_operator_32__1175 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1176 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1175, u_concat(v_op, &l__32_is_32_not_32_associat_1176)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1174) {
      d1174 = 1;
      {
      }
    }
    if (!d1174) {
      u_no_case(s1173);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1178 = v_fp;
  u_value t1179 = NULL;
  if ((!t1179) && u_is_con(s1178, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1178, 0), 1);
    t1179 = u_eq(v_q, v_p);
  }
  if (!t1179) {
    t1179 = u_mk_bool(0);
  }
  if (!t1179) {
    u_no_case(s1178);
  }
  return t1179;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1180 = v_ts;
  u_value t1181 = NULL;
  if ((!t1181) && (u_is_con(s1180, &n_list_46_Cons) && (u_is_con(u_field(s1180, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1180, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1180, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1180, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1180, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1180, 0), 1);
    t1181 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1181) && (u_is_con(s1180, &n_list_46_Cons) && u_is_con(u_field(s1180, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1180, 0);
    t1181 = v_t;
  }
  if ((!t1181) && u_is_con(s1180, &n_list_46_Nil)) {
    t1181 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1181) {
    static struct u_object l__40_invalid_41__1182 = U_LIT("(invalid)");
    t1181 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1182), v_ts);
  }
  if (!t1181) {
    u_no_case(s1180);
  }
  return t1181;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1183 = v_ts;
  u_value t1184 = NULL;
  if ((!t1184) && (u_is_con(s1183, &n_list_46_Cons) && (u_is_con(u_field(s1183, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1183, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1183, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1183, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1183, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1183, 0), 1);
    t1184 = v_cs;
  }
  if (!t1184) {
    t1184 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1184) {
    u_no_case(s1183);
  }
  return t1184;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1185 = U_LIT("->");
  u_value t1192;
  if (u_truth(u_eq(v_op, &l__45__62__1185))) {
    u_value t1186 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1187 = &t1186;
    u_value l1188 = v_l;
    while (u_is_con(l1188, &n_list_46_Cons)) {
      u_value v_t = u_field(l1188, 0);
      if (u_truth(u_mk_bool(1))) {
        e1187 = u_append(e1187, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1188 = u_field(l1188, 1);
    }
    t1192 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1186, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1189 = U_LIT("*");
    u_value t1191;
    if (u_truth(u_eq(v_op, &l__42__1189))) {
      static struct u_object l__40_product_41__1190 = U_LIT("(product)");
      t1191 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1190), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1191 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1192 = t1191;
  }
  return t1192;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1193 = v_t;
  u_value t1194 = NULL;
  if ((!t1194) && (u_is_con(s1193, &n_syntax_46_TSort) && (u_is_con(u_field(s1193, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1193, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1193, 0), 1), u_mk_lit("(invalid)")))))) {
    t1194 = u_mk_bool(1);
  }
  if ((!t1194) && u_is_con(s1193, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1193, 1);
    t1194 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1194) && u_is_con(s1193, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1193, 0);
    u_value v_r = u_field(s1193, 1);
    u_value t1195 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1195)) {
      t1195 = f_parse_46_has_invalid_1(v_r);
    }
    t1194 = t1195;
  }
  if ((!t1194) && u_is_con(s1193, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1193, 0);
    t1194 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1194) {
    t1194 = u_mk_bool(0);
  }
  if (!t1194) {
    u_no_case(s1193);
  }
  return t1194;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1196 = v_ts;
  u_value t1197 = NULL;
  if ((!t1197) && u_is_con(s1196, &n_list_46_Nil)) {
    t1197 = u_mk_bool(0);
  }
  if ((!t1197) && u_is_con(s1196, &n_list_46_Cons)) {
    u_value v_t = u_field(s1196, 0);
    u_value v_more = u_field(s1196, 1);
    u_value t1198 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1198)) {
      t1198 = f_parse_46_any_invalid_1(v_more);
    }
    t1197 = t1198;
  }
  if (!t1197) {
    u_no_case(s1196);
  }
  return t1197;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1199 = v_t;
  u_value t1200 = NULL;
  if ((!t1200) && u_is_con(s1199, &n_lex_46_TName)) {
    u_value v_n = u_field(s1199, 0);
    u_value t1201 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1201)) {
      t1201 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1200 = t1201;
  }
  if ((!t1200) && (u_is_con(s1199, &n_lex_46_TPunct) && u_equals(u_field(s1199, 0), u_mk_char(40)))) {
    t1200 = u_mk_bool(1);
  }
  if (!t1200) {
    t1200 = u_mk_bool(0);
  }
  if (!t1200) {
    u_no_case(s1199);
  }
  return t1200;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1202 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1202;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1203 = f_parse_46_type_one_1(v_r);
    *r_t = t1203;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1204 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1204, v_start), "parse.ParseError");
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
    u_value t1205 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1205;
    u_value t1206 = u_mk_bool(1);
    v_next = t1206;
    u_value t1207 = u_mk_bool(1);
    v_go = t1207;
    u_value t1216 = v_go;
    while (u_truth(t1216)) {
      {
        u_value s1208 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1209 = 0;
        if ((!d1209) && u_is_con(s1208, &n_option_46_Some)) {
          u_value v_op = u_field(s1208, 0);
          d1209 = 1;
          {
            u_value t1210 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1211 = u_mk_bool(1);
            v_rev = t1210;
            v_next = t1211;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1209) && u_is_con(s1208, &n_option_46_None)) {
          u_value t1214 = v_next;
          if (u_truth(t1214)) {
            t1214 = f_parse_46_starts_type_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1214)) {
            d1209 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1212 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1213 = u_mk_bool(0);
              v_rev = t1212;
              v_next = t1213;
            }
          }
        }
        if (!d1209) {
          d1209 = 1;
          {
            u_value t1215 = u_mk_bool(0);
            v_go = t1215;
          }
        }
        if (!d1209) {
          u_no_case(s1208);
        }
      }
      t1216 = v_go;
    }
    u_value s1217 = v_rev;
    int d1218 = 0;
    if ((!d1218) && u_is_con(s1217, &n_list_46_Nil)) {
      d1218 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1219 = U_LIT("type expected but ");
        static struct u_object l__32_found_1220 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1219, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1220)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1218) {
      d1218 = 1;
      {
        u_value t1221 = f_list_46_reverse_1(v_rev);
        *r_items = t1221;
      }
    }
    if (!d1218) {
      u_no_case(s1217);
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
            u_value t1222 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
            *r_r = t1222;
          }
        } else {
          if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Scope_46_params_1(v_sc)))) {
            {
              u_value t1223 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1223;
            }
          } else {
            {
              u_value t1224 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1224;
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
    u_value t1225 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1225;
    u_value t1226 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1226)) {
      t1226 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1226)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1227 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1227;
        u_value t1229 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1229)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1228 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1228;
          }
          t1229 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1230 = f_list_46_reverse_1(v_rev);
    *r_r = t1230;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1231 = v_es;
  u_value t1232 = NULL;
  if ((!t1232) && u_is_con(s1231, &n_list_46_Nil)) {
    t1232 = v_rest;
  }
  if ((!t1232) && u_is_con(s1231, &n_list_46_Cons)) {
    u_value v_e = u_field(s1231, 0);
    u_value v_more = u_field(s1231, 1);
    static struct u_object l_Cons_1233 = U_LIT("Cons");
    t1232 = f_parse_46_expr_op2_3(&l_Cons_1233, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1232) {
    u_no_case(s1231);
  }
  return t1232;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1234 = v_t;
  u_value t1235 = NULL;
  if ((!t1235) && u_is_con(s1234, &n_lex_46_TName)) {
    u_value v_n = u_field(s1234, 0);
    static struct u_object l_let_1236 = U_LIT("let");
    static struct u_object l_if_1237 = U_LIT("if");
    static struct u_object l_lambda_1238 = U_LIT("lambda");
    static struct u_object l_match_1239 = U_LIT("match");
    static struct u_object l_true_1240 = U_LIT("true");
    static struct u_object l_false_1241 = U_LIT("false");
    u_value t1243 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1236, u_mk_con(&n_list_46_Cons, 2, &l_if_1237, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1238, u_mk_con(&n_list_46_Cons, 2, &l_match_1239, u_mk_con(&n_list_46_Cons, 2, &l_true_1240, u_mk_con(&n_list_46_Cons, 2, &l_false_1241, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1243)) {
      u_value t1242 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1242)) {
        t1242 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1243 = t1242;
    }
    t1235 = t1243;
  }
  if ((!t1235) && u_is_con(s1234, &n_lex_46_TNum)) {
    t1235 = u_mk_bool(1);
  }
  if ((!t1235) && u_is_con(s1234, &n_lex_46_TStr)) {
    t1235 = u_mk_bool(1);
  }
  if ((!t1235) && u_is_con(s1234, &n_lex_46_TChar)) {
    t1235 = u_mk_bool(1);
  }
  if ((!t1235) && u_is_con(s1234, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1234, 0);
    u_value t1244 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1244)) {
      t1244 = u_eq(v_c, u_mk_char(91));
    }
    t1235 = t1244;
  }
  if (!t1235) {
    t1235 = u_mk_bool(0);
  }
  if (!t1235) {
    u_no_case(s1234);
  }
  return t1235;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    static struct u_object l_raise_1245 = U_LIT("raise");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_raise_1245))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, r_e, r_ts);
        u_value t1246 = u_mk_con(&n_syntax_46_ERaise, 2, *r_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_e = t1246;
      }
    } else {
      {
        p_parse_46_operators(v_sc, r_e, r_ts);
      }
    }
  }
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
    u_value t1247 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1247;
    u_value t1248 = u_mk_bool(1);
    v_next = t1248;
    u_value t1249 = u_mk_bool(1);
    v_go = t1249;
    u_value t1258 = v_go;
    while (u_truth(t1258)) {
      {
        u_value s1250 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1251 = 0;
        if ((!d1251) && u_is_con(s1250, &n_option_46_Some)) {
          u_value v_op = u_field(s1250, 0);
          d1251 = 1;
          {
            u_value t1252 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1253 = u_mk_bool(1);
            v_rev = t1252;
            v_next = t1253;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1251) && u_is_con(s1250, &n_option_46_None)) {
          u_value t1256 = v_next;
          if (u_truth(t1256)) {
            t1256 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1256)) {
            d1251 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1254 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1255 = u_mk_bool(0);
              v_rev = t1254;
              v_next = t1255;
            }
          }
        }
        if (!d1251) {
          d1251 = 1;
          {
            u_value t1257 = u_mk_bool(0);
            v_go = t1257;
          }
        }
        if (!d1251) {
          u_no_case(s1250);
        }
      }
      t1258 = v_go;
    }
    u_value s1259 = v_rev;
    int d1260 = 0;
    if ((!d1260) && u_is_con(s1259, &n_list_46_Nil)) {
      d1260 = 1;
      {
        static struct u_object l_expression_32_expec_1261 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1262 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1261, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1262)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1260) {
      d1260 = 1;
      {
        u_value t1263 = f_list_46_reverse_1(v_rev);
        *r_items = t1263;
      }
    }
    if (!d1260) {
      u_no_case(s1259);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1264 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1264;
    u_value t1266 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1266)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1265 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1265;
      }
      t1266 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1267 = f_list_46_reverse_1(v_rev);
    *r_es = t1267;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1268 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1268;
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
    u_value t1269 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1269)) {
      t1269 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1273 = t1269;
    while (u_truth(t1273)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1270 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1270;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1271 = U_LIT(".");
            u_value t1272 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1271, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1272;
          }
        }
      }
      u_value t1274 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1274)) {
        t1274 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1273 = t1274;
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
    u_value s1275 = f_parse_46_head_token_1(*r_ts);
    int d1276 = 0;
    if ((!d1276) && (u_is_con(s1275, &n_lex_46_TName) && u_equals(u_field(s1275, 0), u_mk_lit("let")))) {
      d1276 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1276) && (u_is_con(s1275, &n_lex_46_TName) && u_equals(u_field(s1275, 0), u_mk_lit("if")))) {
      d1276 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1277 = U_LIT("then");
        p_parse_46_keyword(&l_then_1277, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1278 = U_LIT("else");
        p_parse_46_keyword(&l_else_1278, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1279 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1279;
      }
    }
    if ((!d1276) && (u_is_con(s1275, &n_lex_46_TName) && u_equals(u_field(s1275, 0), u_mk_lit("lambda")))) {
      d1276 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1276) && (u_is_con(s1275, &n_lex_46_TName) && u_equals(u_field(s1275, 0), u_mk_lit("match")))) {
      d1276 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1280 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1280;
      }
    }
    if ((!d1276) && (u_is_con(s1275, &n_lex_46_TName) && u_equals(u_field(s1275, 0), u_mk_lit("true")))) {
      d1276 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1281 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1281;
      }
    }
    if ((!d1276) && (u_is_con(s1275, &n_lex_46_TName) && u_equals(u_field(s1275, 0), u_mk_lit("false")))) {
      d1276 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1282 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1282;
      }
    }
    if ((!d1276) && u_is_con(s1275, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1275, 0);
      d1276 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1283 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1283;
      }
    }
    if ((!d1276) && u_is_con(s1275, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1275, 0);
      d1276 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1284 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1284;
      }
    }
    if ((!d1276) && u_is_con(s1275, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1275, 0);
      d1276 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1285 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1285;
      }
    }
    if ((!d1276) && (u_is_con(s1275, &n_lex_46_TPunct) && u_equals(u_field(s1275, 0), u_mk_char(40)))) {
      d1276 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1286 = v_es;
        int d1287 = 0;
        if ((!d1287) && (u_is_con(s1286, &n_list_46_Cons) && u_is_con(u_field(s1286, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1286, 0);
          d1287 = 1;
          {
            u_value t1288 = v_x;
            *r_e = t1288;
          }
        }
        if (!d1287) {
          d1287 = 1;
          {
            u_value t1289 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1289;
          }
        }
        if (!d1287) {
          u_no_case(s1286);
        }
      }
    }
    if ((!d1276) && (u_is_con(s1275, &n_lex_46_TPunct) && u_equals(u_field(s1275, 0), u_mk_char(91)))) {
      d1276 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1276) && u_is_con(s1275, &n_lex_46_TName)) {
      u_value v_n = u_field(s1275, 0);
      u_value t1291 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1291)) {
        t1291 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t1291)) {
        d1276 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1290 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1290;
        }
      }
    }
    if (!d1276) {
      d1276 = 1;
      {
        static struct u_object l_expression_32_expec_1292 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1293 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1292, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1293)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1276) {
      u_no_case(s1275);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1294 = v_ps;
  u_value t1295 = NULL;
  if ((!t1295) && u_is_con(s1294, &n_list_46_Nil)) {
    t1295 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1295) && (u_is_con(s1294, &n_list_46_Cons) && u_is_con(u_field(s1294, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1294, 0), 0);
    u_value v_more = u_field(s1294, 1);
    u_value s1296 = f_parse_46_pattern_vars_1(v_more);
    u_value t1297 = NULL;
    if ((!t1297) && u_is_con(s1296, &n_option_46_Some)) {
      u_value v_xs = u_field(s1296, 0);
      t1297 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1297) && u_is_con(s1296, &n_option_46_None)) {
      t1297 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1297) {
      u_no_case(s1296);
    }
    t1295 = t1297;
  }
  if (!t1295) {
    t1295 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1295) {
    u_no_case(s1294);
  }
  return t1295;
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
    static struct u_object l__58__61__1298 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1298), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1299 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1300 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1299;
    v_es = t1300;
    u_value t1304 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1304)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1301 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1301), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1302 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1303 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1302;
        v_es = t1303;
      }
      t1304 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1305 = U_LIT("in");
    p_parse_46_keyword(&l_in_1305, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1306 = f_list_46_reverse_1(v_ps);
    u_value t1307 = f_list_46_reverse_1(v_es);
    v_ps = t1306;
    v_es = t1307;
    u_value s1308 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1309 = 0;
    if ((!d1309) && u_is_con(u_field(s1308, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1308, 0), 0);
      d1309 = 1;
      {
        u_value t1310 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1310;
      }
    }
    if ((!d1309) && (u_is_con(u_field(s1308, 0), &n_option_46_None) && (u_is_con(u_field(s1308, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1308, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1308, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1308, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1308, 1), 0);
      u_value v_c = u_field(u_field(s1308, 2), 0);
      d1309 = 1;
      {
        u_value t1311 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1311;
      }
    }
    if (!d1309) {
      d1309 = 1;
      {
        u_value t1312 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1312;
      }
    }
    if (!d1309) {
      u_no_case(s1308);
    }
  }
}

void p_parse_46_parse_lambda(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    p_parse_46_advance(r_ts);
    u_value t1313 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1313;
    static struct u_object l__61__62__1314 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1314)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1315 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1315;
        u_value t1317 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1317)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1316 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1316;
          }
          t1317 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1318 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1318), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1319 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1319;
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
        static struct u_object l_Nil_1320 = U_LIT("Nil");
        u_value t1321 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1320), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1321;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1322 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1322))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1323 = U_LIT("in");
            p_parse_46_keyword(&l_in_1323, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1324 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1325 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1324;
            v_lists = t1325;
            u_value t1329 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1329)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1326 = U_LIT("in");
                p_parse_46_keyword(&l_in_1326, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1327 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1328 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1327;
                v_lists = t1328;
              }
              t1329 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1330 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1330;
            static struct u_object l_if_1331 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1331))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1332 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1332;
          }
        } else {
          {
            u_value t1333 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1333;
            u_value t1335 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1335)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1334 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1334;
              }
              t1335 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1336 = U_LIT("Nil");
            u_value t1337 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1336), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1337;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1338 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1338;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1339 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1339))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1340 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1340;
      }
    } else {
      {
        u_value t1341 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1341;
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
    u_value t1342 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1342;
    static struct u_object l_case_1343 = U_LIT("case");
    u_value t1346 = f_parse_46_at_keyword_2(*r_ts, &l_case_1343);
    while (u_truth(t1346)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1344 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1344), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1345 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1345;
      }
      static struct u_object l_case_1347 = U_LIT("case");
      t1346 = f_parse_46_at_keyword_2(*r_ts, &l_case_1347);
    }
    static struct u_object l_else_1348 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1348))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1349 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1349;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1350 = U_LIT("end");
    p_parse_46_keyword(&l_end_1350, r_ts);
    u_value t1351 = f_list_46_reverse_1(v_rev);
    *r_cs = t1351;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1352 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1352))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1353 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1353, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
  u_value s1354 = v_ps;
  u_value t1355 = NULL;
  if ((!t1355) && u_is_con(s1354, &n_list_46_Nil)) {
    t1355 = v_rest;
  }
  if ((!t1355) && u_is_con(s1354, &n_list_46_Cons)) {
    u_value v_p = u_field(s1354, 0);
    u_value v_more = u_field(s1354, 1);
    static struct u_object l_Cons_1356 = U_LIT("Cons");
    t1355 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1356), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1355) {
    u_no_case(s1354);
  }
  return t1355;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1357 = v_t;
  u_value t1358 = NULL;
  if ((!t1358) && u_is_con(s1357, &n_lex_46_TName)) {
    u_value v_n = u_field(s1357, 0);
    static struct u_object l___1359 = U_LIT("_");
    static struct u_object l_true_1360 = U_LIT("true");
    static struct u_object l_false_1361 = U_LIT("false");
    u_value t1363 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1359, u_mk_con(&n_list_46_Cons, 2, &l_true_1360, u_mk_con(&n_list_46_Cons, 2, &l_false_1361, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1363)) {
      u_value t1362 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1362)) {
        t1362 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1363 = t1362;
    }
    t1358 = t1363;
  }
  if ((!t1358) && u_is_con(s1357, &n_lex_46_TNum)) {
    t1358 = u_mk_bool(1);
  }
  if ((!t1358) && u_is_con(s1357, &n_lex_46_TStr)) {
    t1358 = u_mk_bool(1);
  }
  if ((!t1358) && u_is_con(s1357, &n_lex_46_TChar)) {
    t1358 = u_mk_bool(1);
  }
  if ((!t1358) && u_is_con(s1357, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1357, 0);
    u_value t1364 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1364)) {
      t1364 = u_eq(v_c, u_mk_char(91));
    }
    t1358 = t1364;
  }
  if (!t1358) {
    t1358 = u_mk_bool(0);
  }
  if (!t1358) {
    u_no_case(s1357);
  }
  return t1358;
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
    u_value t1365 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1365;
    u_value t1366 = u_mk_bool(1);
    v_next = t1366;
    u_value t1367 = u_mk_bool(1);
    v_go = t1367;
    u_value t1376 = v_go;
    while (u_truth(t1376)) {
      {
        u_value s1368 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1369 = 0;
        if ((!d1369) && u_is_con(s1368, &n_option_46_Some)) {
          u_value v_op = u_field(s1368, 0);
          d1369 = 1;
          {
            u_value t1370 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1371 = u_mk_bool(1);
            v_rev = t1370;
            v_next = t1371;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1369) && u_is_con(s1368, &n_option_46_None)) {
          u_value t1374 = v_next;
          if (u_truth(t1374)) {
            t1374 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1374)) {
            d1369 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1372 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1373 = u_mk_bool(0);
              v_rev = t1372;
              v_next = t1373;
            }
          }
        }
        if (!d1369) {
          d1369 = 1;
          {
            u_value t1375 = u_mk_bool(0);
            v_go = t1375;
          }
        }
        if (!d1369) {
          u_no_case(s1368);
        }
      }
      t1376 = v_go;
    }
    u_value s1377 = v_rev;
    int d1378 = 0;
    if ((!d1378) && u_is_con(s1377, &n_list_46_Nil)) {
      d1378 = 1;
      {
        static struct u_object l_pattern_32_expected_1379 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1380 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1379, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1380)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1378) {
      d1378 = 1;
      {
        u_value t1381 = f_list_46_reverse_1(v_rev);
        *r_items = t1381;
      }
    }
    if (!d1378) {
      u_no_case(s1377);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1382 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1382;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1383 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1383;
        u_value t1385 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1385)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1384 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1384;
          }
          t1385 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1386 = f_list_46_reverse_1(v_rev);
    *r_ps = t1386;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1387 = f_parse_46_head_token_1(*r_ts);
    int d1388 = 0;
    if ((!d1388) && (u_is_con(s1387, &n_lex_46_TName) && u_equals(u_field(s1387, 0), u_mk_lit("_")))) {
      d1388 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1389 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1389;
      }
    }
    if ((!d1388) && (u_is_con(s1387, &n_lex_46_TName) && u_equals(u_field(s1387, 0), u_mk_lit("true")))) {
      d1388 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1390 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1390;
      }
    }
    if ((!d1388) && (u_is_con(s1387, &n_lex_46_TName) && u_equals(u_field(s1387, 0), u_mk_lit("false")))) {
      d1388 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1391 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1391;
      }
    }
    if ((!d1388) && u_is_con(s1387, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1387, 0);
      d1388 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1392 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1392;
      }
    }
    if ((!d1388) && u_is_con(s1387, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1387, 0);
      d1388 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1393 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1393;
      }
    }
    if ((!d1388) && u_is_con(s1387, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1387, 0);
      d1388 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1394 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1394;
      }
    }
    if ((!d1388) && (u_is_con(s1387, &n_lex_46_TPunct) && u_equals(u_field(s1387, 0), u_mk_char(40)))) {
      d1388 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1395 = v_ps;
        int d1396 = 0;
        if ((!d1396) && (u_is_con(s1395, &n_list_46_Cons) && u_is_con(u_field(s1395, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1395, 0);
          d1396 = 1;
          {
            u_value t1397 = v_q;
            *r_p = t1397;
          }
        }
        if (!d1396) {
          d1396 = 1;
          {
            u_value t1398 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1398;
          }
        }
        if (!d1396) {
          u_no_case(s1395);
        }
      }
    }
    if ((!d1388) && (u_is_con(s1387, &n_lex_46_TPunct) && u_equals(u_field(s1387, 0), u_mk_char(91)))) {
      d1388 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1399 = U_LIT("Nil");
            u_value t1400 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1399), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1400;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1401 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1401;
            u_value t1403 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1403)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1402 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1402;
              }
              t1403 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1404 = U_LIT("Nil");
            u_value t1405 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1404), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1405;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1406 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1406;
          }
        }
      }
    }
    if (!d1388) {
      d1388 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1407 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1407;
          }
        } else {
          {
            u_value t1408 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1408;
          }
        }
      }
    }
    if (!d1388) {
      u_no_case(s1387);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1409 = v_t;
  u_value t1410 = NULL;
  if ((!t1410) && u_is_con(s1409, &n_lex_46_TName)) {
    u_value v_n = u_field(s1409, 0);
    static struct u_object l_skip_1411 = U_LIT("skip");
    static struct u_object l_if_1412 = U_LIT("if");
    static struct u_object l_while_1413 = U_LIT("while");
    static struct u_object l_call_1414 = U_LIT("call");
    static struct u_object l_match_1415 = U_LIT("match");
    static struct u_object l_assert_1416 = U_LIT("assert");
    static struct u_object l_assign_1417 = U_LIT("assign");
    static struct u_object l_raise_1418 = U_LIT("raise");
    static struct u_object l_try_1419 = U_LIT("try");
    t1410 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1411, u_mk_con(&n_list_46_Cons, 2, &l_if_1412, u_mk_con(&n_list_46_Cons, 2, &l_while_1413, u_mk_con(&n_list_46_Cons, 2, &l_call_1414, u_mk_con(&n_list_46_Cons, 2, &l_match_1415, u_mk_con(&n_list_46_Cons, 2, &l_assert_1416, u_mk_con(&n_list_46_Cons, 2, &l_assign_1417, u_mk_con(&n_list_46_Cons, 2, &l_raise_1418, u_mk_con(&n_list_46_Cons, 2, &l_try_1419, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1410) {
    t1410 = u_mk_bool(0);
  }
  if (!t1410) {
    u_no_case(s1409);
  }
  return t1410;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1420 = v_cs;
  u_value t1421 = NULL;
  if ((!t1421) && u_is_con(s1420, &n_list_46_Nil)) {
    t1421 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1421) && (u_is_con(s1420, &n_list_46_Cons) && u_is_con(u_field(s1420, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1420, 0), 0);
    u_value v_g = u_field(u_field(s1420, 0), 1);
    u_value v_b = u_field(u_field(s1420, 0), 2);
    u_value v_more = u_field(s1420, 1);
    t1421 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1421) {
    u_no_case(s1420);
  }
  return t1421;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1422 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1422;
    u_value t1423 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1423;
    u_value t1424 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1424;
    static struct u_object l_local_1425 = U_LIT("local");
    u_value t1427 = f_parse_46_at_keyword_2(*r_ts, &l_local_1425);
    while (u_truth(t1427)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1426 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1426)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1426 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1428 = U_LIT("local");
      t1427 = f_parse_46_at_keyword_2(*r_ts, &l_local_1428);
    }
    u_value t1430 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1430)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1429 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1429;
      }
      t1430 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1431 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1431;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1432 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1432;
    static struct u_object l__58__1433 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1433)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__1434 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1434)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1435 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1435;
      }
    }
    u_value t1436 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1436;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1437 = f_parse_46_head_token_1(*r_ts);
    int d1438 = 0;
    if ((!d1438) && (u_is_con(s1437, &n_lex_46_TName) && u_equals(u_field(s1437, 0), u_mk_lit("skip")))) {
      d1438 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1439 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1439;
      }
    }
    if ((!d1438) && (u_is_con(s1437, &n_lex_46_TName) && u_equals(u_field(s1437, 0), u_mk_lit("if")))) {
      d1438 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1438) && (u_is_con(s1437, &n_lex_46_TName) && u_equals(u_field(s1437, 0), u_mk_lit("while")))) {
      d1438 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1440 = U_LIT("end");
        p_parse_46_keyword(&l_end_1440, r_ts);
        u_value t1441 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1441;
      }
    }
    if ((!d1438) && (u_is_con(s1437, &n_lex_46_TName) && u_equals(u_field(s1437, 0), u_mk_lit("call")))) {
      d1438 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1438) && (u_is_con(s1437, &n_lex_46_TName) && u_equals(u_field(s1437, 0), u_mk_lit("match")))) {
      d1438 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1442 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1442;
      }
    }
    if ((!d1438) && (u_is_con(s1437, &n_lex_46_TName) && u_equals(u_field(s1437, 0), u_mk_lit("assert")))) {
      d1438 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1443 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1443;
      }
    }
    if ((!d1438) && (u_is_con(s1437, &n_lex_46_TName) && u_equals(u_field(s1437, 0), u_mk_lit("assign")))) {
      d1438 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1438) && (u_is_con(s1437, &n_lex_46_TName) && u_equals(u_field(s1437, 0), u_mk_lit("raise")))) {
      d1438 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1444 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1444;
      }
    }
    if ((!d1438) && (u_is_con(s1437, &n_lex_46_TName) && u_equals(u_field(s1437, 0), u_mk_lit("try")))) {
      d1438 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1445 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1445, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1446 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1446;
      }
    }
    if (!d1438) {
      d1438 = 1;
      {
        static struct u_object l_statement_32_expect_1447 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1448 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1447, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1448)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1438) {
      u_no_case(s1437);
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
    static struct u_object l_else_1449 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1449)))) {
      {
        static struct u_object l_end_1450 = U_LIT("end");
        p_parse_46_keyword(&l_end_1450, r_ts);
        u_value t1451 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1451;
      }
    } else {
      static struct u_object l_if_1452 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1452)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1453 = U_LIT("end");
          p_parse_46_keyword(&l_end_1453, r_ts);
        }
      }
    }
    u_value t1454 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1454;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1455 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1455), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1456 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1456;
    u_value t1459 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1459)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1457 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1457), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1458 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1458;
      }
      t1459 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1460 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1460;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1461 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1461;
    static struct u_object l_case_1462 = U_LIT("case");
    u_value t1465 = f_parse_46_at_keyword_2(*r_ts, &l_case_1462);
    while (u_truth(t1465)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1463 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1463), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1464 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1464;
      }
      static struct u_object l_case_1466 = U_LIT("case");
      t1465 = f_parse_46_at_keyword_2(*r_ts, &l_case_1466);
    }
    static struct u_object l_else_1467 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1467))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1468 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1468;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1469 = U_LIT("end");
    p_parse_46_keyword(&l_end_1469, r_ts);
    u_value t1470 = f_list_46_reverse_1(v_rev);
    *r_cs = t1470;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1472;
  if (u_truth(v_first)) {
    t1472 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1471 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1471)) {
      t1471 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1472 = t1471;
  }
  return t1472;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1474 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1474)) {
    static struct u_object l_out_1473 = U_LIT("out");
    t1474 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1473));
  }
  u_value t1476 = t1474;
  if (u_truth(t1476)) {
    static struct u_object l_ref_1475 = U_LIT("ref");
    t1476 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1475));
  }
  return t1476;
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
    u_value t1477 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1478 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1479 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1480 = u_mk_bool(1);
    v_ins = t1477;
    v_outs = t1478;
    v_refs = t1479;
    v_first = t1480;
    static struct u_object l_in_1481 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1481))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1482 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
        u_value t1483 = u_mk_bool(0);
        v_ins = t1482;
        v_first = t1483;
        u_value t1485 = f_parse_46_item_follows_1(*r_ts);
        while (u_truth(t1485)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_expr(v_sc, &v_e, r_ts);
            u_value t1484 = u_mk_con(&n_list_46_Cons, 2, v_e, v_ins);
            v_ins = t1484;
          }
          t1485 = f_parse_46_item_follows_1(*r_ts);
        }
      }
    }
    static struct u_object l_out_1486 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1486, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_outs, r_ts);
        u_value t1487 = u_mk_bool(0);
        v_first = t1487;
      }
    }
    static struct u_object l_ref_1488 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1488, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    u_value t1489 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), f_list_46_reverse_1(v_ins), v_outs, v_refs);
    *r_s = t1489;
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
    u_value t1490 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1490;
    u_value t1492 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1492)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1491 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_rev);
        v_rev = t1491;
      }
      t1492 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1493 = f_list_46_reverse_1(v_rev);
    *r_xs = t1493;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1494 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1494;
    static struct u_object l__58__1495 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1495)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1496 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1496;
  }
}

void p_parse_46_parse_formals1(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_formal(v_sc, &v_x, r_ts);
    u_value t1497 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1497;
    u_value t1499 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1499)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formal(v_sc, &v_x, r_ts);
        u_value t1498 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1498;
      }
      t1499 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1500 = f_list_46_reverse_1(v_rev);
    *r_xs = t1500;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1501 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1501;
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
  u_value s1502 = v_cs;
  u_value t1503 = NULL;
  if ((!t1503) && u_is_con(s1502, &n_list_46_Nil)) {
    t1503 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1503) && (u_is_con(s1502, &n_list_46_Cons) && u_is_con(u_field(s1502, 0), &n_syntax_46_CEval))) {
    u_value v_more = u_field(s1502, 1);
    t1503 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1503) && (u_is_con(s1502, &n_list_46_Cons) && u_is_con(u_field(s1502, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1502, 1);
    t1503 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1503) && (u_is_con(s1502, &n_list_46_Cons) && u_is_con(u_field(s1502, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1502, 1);
    t1503 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1503) && u_is_con(s1502, &n_list_46_Cons)) {
    u_value v_c = u_field(s1502, 0);
    u_value v_more = u_field(s1502, 1);
    t1503 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1503) {
    u_no_case(s1502);
  }
  return t1503;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1504 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1505 = &t1504;
  u_value l1506 = v_cs;
  while (u_is_con(l1506, &n_list_46_Cons)) {
    u_value v_c = u_field(l1506, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1505 = u_append(e1505, v_c);
    }
    l1506 = u_field(l1506, 1);
  }
  return t1504;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1507 = v_c;
  u_value t1508 = NULL;
  if ((!t1508) && u_is_con(s1507, &n_syntax_46_CNotation)) {
    t1508 = u_mk_bool(1);
  }
  if (!t1508) {
    t1508 = u_mk_bool(0);
  }
  if (!t1508) {
    u_no_case(s1507);
  }
  return t1508;
}

u_value f_parse_46_Context_46_notations_1(u_value v_x) {
  u_value s1509 = v_x;
  u_value t1510 = NULL;
  if ((!t1510) && u_is_con(s1509, &n_parse_46_Context)) {
    u_value v_y = u_field(s1509, 0);
    t1510 = v_y;
  }
  if (!t1510) {
    u_no_case(s1509);
  }
  return t1510;
}

u_value f_parse_46_Context_46_modules_1(u_value v_x) {
  u_value s1511 = v_x;
  u_value t1512 = NULL;
  if ((!t1512) && u_is_con(s1511, &n_parse_46_Context)) {
    u_value v_y = u_field(s1511, 1);
    t1512 = v_y;
  }
  if (!t1512) {
    u_no_case(s1511);
  }
  return t1512;
}

u_value f_parse_46_Context_46_loading_1(u_value v_x) {
  u_value s1513 = v_x;
  u_value t1514 = NULL;
  if ((!t1514) && u_is_con(s1513, &n_parse_46_Context)) {
    u_value v_y = u_field(s1513, 2);
    t1514 = v_y;
  }
  if (!t1514) {
    u_no_case(s1513);
  }
  return t1514;
}

u_value f_parse_46_Context_46_base_1(u_value v_x) {
  u_value s1515 = v_x;
  u_value t1516 = NULL;
  if ((!t1516) && u_is_con(s1515, &n_parse_46_Context)) {
    u_value v_y = u_field(s1515, 3);
    t1516 = v_y;
  }
  if (!t1516) {
    u_no_case(s1515);
  }
  return t1516;
}

u_value f_parse_46_Context_46_dir_1(u_value v_x) {
  u_value s1517 = v_x;
  u_value t1518 = NULL;
  if ((!t1518) && u_is_con(s1517, &n_parse_46_Context)) {
    u_value v_y = u_field(s1517, 4);
    t1518 = v_y;
  }
  if (!t1518) {
    u_no_case(s1517);
  }
  return t1518;
}

u_value f_parse_46_with_commands_2(u_value v_cx, u_value v_cs) {
  return u_mk_con(&n_parse_46_Context, 5, f_parse_46_with_notations_2(f_parse_46_Context_46_notations_1(v_cx), v_cs), f_parse_46_Context_46_modules_1(v_cx), f_parse_46_Context_46_loading_1(v_cx), f_parse_46_Context_46_base_1(v_cx), f_parse_46_Context_46_dir_1(v_cx));
}

u_value f_parse_46_with_module_2(u_value v_cx, u_value v_c) {
  return u_mk_con(&n_parse_46_Context, 5, f_parse_46_Context_46_notations_1(v_cx), f_list_46__43__43__2(f_parse_46_modules_of_1(v_c), f_parse_46_Context_46_modules_1(v_cx)), f_parse_46_Context_46_loading_1(v_cx), f_parse_46_Context_46_base_1(v_cx), f_parse_46_Context_46_dir_1(v_cx));
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1519 = v_c;
  u_value t1520 = NULL;
  if ((!t1520) && u_is_con(s1519, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1519, 0);
    u_value v_cs = u_field(s1519, 1);
    u_value t1521 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1522 = &t1521;
    u_value l1523 = v_cs;
    while (u_is_con(l1523, &n_list_46_Cons)) {
      u_value v_d = u_field(l1523, 0);
      if (u_truth(u_mk_bool(1))) {
        e1522 = u_append(e1522, f_parse_46_modules_of_1(v_d));
      }
      l1523 = u_field(l1523, 1);
    }
    t1520 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1521));
  }
  if (!t1520) {
    t1520 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1520) {
    u_no_case(s1519);
  }
  return t1520;
}

u_value f_parse_46_own_notations_2(u_value v_cx, u_value v_n) {
  u_value s1524 = f_list_46_lookup_2(v_n, f_parse_46_Context_46_modules_1(v_cx));
  u_value t1525 = NULL;
  if ((!t1525) && u_is_con(s1524, &n_option_46_Some)) {
    u_value v_ns = u_field(s1524, 0);
    t1525 = v_ns;
  }
  if ((!t1525) && u_is_con(s1524, &n_option_46_None)) {
    t1525 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1525) {
    u_no_case(s1524);
  }
  return t1525;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1526 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1526;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1527 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1527;
        u_value t1529 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1529)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1528 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1528;
          }
          t1529 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1530 = f_list_46_reverse_1(v_rev);
    *r_ps = t1530;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1531 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1531;
    u_value s1532 = f_parse_46_head_token_1(*r_ts);
    int d1533 = 0;
    if ((!d1533) && (u_is_con(s1532, &n_lex_46_TName) && u_equals(u_field(s1532, 0), u_mk_lit("prefix")))) {
      d1533 = 1;
      {
        u_value t1534 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1534;
      }
    }
    if ((!d1533) && (u_is_con(s1532, &n_lex_46_TName) && u_equals(u_field(s1532, 0), u_mk_lit("postfix")))) {
      d1533 = 1;
      {
        u_value t1535 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1535;
      }
    }
    if (!d1533) {
      d1533 = 1;
      {
        u_value s1536 = f_parse_46_second_token_1(*r_ts);
        int d1537 = 0;
        if ((!d1537) && (u_is_con(s1536, &n_lex_46_TName) && u_equals(u_field(s1536, 0), u_mk_lit("left")))) {
          d1537 = 1;
          {
            u_value t1538 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1538;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1537) && (u_is_con(s1536, &n_lex_46_TName) && u_equals(u_field(s1536, 0), u_mk_lit("right")))) {
          d1537 = 1;
          {
            u_value t1539 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1539;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1537) {
          d1537 = 1;
          {
          }
        }
        if (!d1537) {
          u_no_case(s1536);
        }
      }
    }
    if (!d1533) {
      u_no_case(s1532);
    }
    p_parse_46_advance(r_ts);
    u_value s1540 = f_parse_46_head_token_1(*r_ts);
    int d1541 = 0;
    if ((!d1541) && u_is_con(s1540, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1540, 0);
      d1541 = 1;
      {
        u_value t1542 = v_n;
        v_p = t1542;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1541) {
      d1541 = 1;
      {
        static struct u_object l_precedence_32_expec_1543 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1544 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1543, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1544)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1541) {
      u_no_case(s1540);
    }
    u_value t1545 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1546 = u_mk_bool(1);
    v_ops = t1545;
    v_go = t1546;
    u_value t1552 = v_go;
    while (u_truth(t1552)) {
      {
        u_value s1547 = f_parse_46_head_token_1(*r_ts);
        int d1548 = 0;
        if ((!d1548) && u_is_con(s1547, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1547, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1548 = 1;
            {
              u_value t1549 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1549;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1548) && u_is_con(s1547, &n_lex_46_TName)) {
          u_value v_n = u_field(s1547, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1548 = 1;
            {
              u_value t1550 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1550;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1548) {
          d1548 = 1;
          {
            u_value t1551 = u_mk_bool(0);
            v_go = t1551;
          }
        }
        if (!d1548) {
          u_no_case(s1547);
        }
      }
      t1552 = v_go;
    }
    u_value s1553 = v_ops;
    int d1554 = 0;
    if ((!d1554) && u_is_con(s1553, &n_list_46_Nil)) {
      d1554 = 1;
      {
        static struct u_object l_operator_32_expecte_1555 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1556 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1555, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1556)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1554) {
      d1554 = 1;
      {
        u_value t1557 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1557;
      }
    }
    if (!d1554) {
      u_no_case(s1553);
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
    u_value t1558 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1559 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1558;
    *r_fs = t1559;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1560 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
            u_value t1561 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
            v_args = t1560;
            *r_fs = t1561;
            u_value t1564 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1564)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t1562 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t1563 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t1562;
                *r_fs = t1563;
              }
              t1564 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1565 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t1566 = f_list_46_reverse_1(*r_fs);
    *r_k = t1565;
    *r_fs = t1566;
    u_value s1567 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d1568 = 0;
    if ((!d1568) && u_is_con(s1567, &n_option_46_Some)) {
      u_value v_f = u_field(s1567, 0);
      d1568 = 1;
      {
        static struct u_object l_repeated_32_field_32__1569 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__1570 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__1569, u_concat(v_f, u_concat(&l__32_of_32__1570, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1568) && u_is_con(s1567, &n_option_46_None)) {
      d1568 = 1;
      {
      }
    }
    if (!d1568) {
      u_no_case(s1567);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1571 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1572 = 0;
    if ((!d1572) && (u_is_con(u_field(s1571, 0), &n_lex_46_TName) && (u_is_con(u_field(s1571, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1571, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1571, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1572 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1573 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1573;
        }
      }
    }
    if (!d1572) {
      d1572 = 1;
      {
        u_value t1574 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1574;
      }
    }
    if (!d1572) {
      u_no_case(s1571);
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
    static struct u_object l__58__61__1575 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1575), r_ts);
    static struct u_object l__124__1576 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1576)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1577 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1577;
    static struct u_object l__124__1578 = U_LIT("|");
    u_value t1580 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1578));
    while (u_truth(t1580)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1579 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1579;
      }
      static struct u_object l__124__1581 = U_LIT("|");
      t1580 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1581));
    }
    u_value s1582 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d1583 = 0;
    if ((!d1583) && u_is_con(s1582, &n_option_46_Some)) {
      u_value v_f = u_field(s1582, 0);
      d1583 = 1;
      {
        static struct u_object l_field_32__1584 = U_LIT("field ");
        static struct u_object l__32_of_32__1585 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_1586 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__1584, u_concat(v_f, u_concat(&l__32_of_32__1585, u_concat(v_n, &l__32_has_32_different_32_t_1586)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1583) && u_is_con(s1582, &n_option_46_None)) {
      d1583 = 1;
      {
      }
    }
    if (!d1583) {
      u_no_case(s1582);
    }
    u_value t1587 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)));
    *r_cs = t1587;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s1588 = v_fs;
  u_value t1589 = NULL;
  if ((!t1589) && u_is_con(s1588, &n_list_46_Nil)) {
    t1589 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1589) && (u_is_con(s1588, &n_list_46_Cons) && u_is_con(u_field(s1588, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s1588, 0), 0);
    u_value v_rest = u_field(s1588, 1);
    t1589 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t1589) && (u_is_con(s1588, &n_list_46_Cons) && u_is_con(u_field(s1588, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1588, 1);
    t1589 = f_parse_46_field_names_1(v_rest);
  }
  if (!t1589) {
    u_no_case(s1588);
  }
  return t1589;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s1590 = v_fs;
  u_value t1591 = NULL;
  if ((!t1591) && u_is_con(s1590, &n_list_46_Nil)) {
    t1591 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1591) && u_is_con(s1590, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1590, 0), 0);
    u_value v_t = u_field(u_field(u_field(s1590, 0), 1), 3);
    u_value v_rest = u_field(s1590, 1);
    u_value t1592 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1593 = &t1592;
    u_value l1594 = f_parse_46_fields_named_2(v_f, v_rest);
    while (u_is_con(l1594, &n_list_46_Cons)) {
      u_value v_e = u_field(l1594, 0);
      if (u_truth(u_mk_bool(1))) {
        e1593 = u_append(e1593, f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t));
      }
      l1594 = u_field(l1594, 1);
    }
    u_value t1595;
    if (u_truth(f_list_46_contains_2(u_mk_bool(1), t1592))) {
      t1595 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t1595 = f_parse_46_differing_1(v_rest);
    }
    t1591 = t1595;
  }
  if (!t1591) {
    u_no_case(s1590);
  }
  return t1591;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s1596 = v_e;
  u_value t1597 = NULL;
  if (!t1597) {
    u_value v_t = u_field(u_field(s1596, 1), 3);
    t1597 = v_t;
  }
  if (!t1597) {
    u_no_case(s1596);
  }
  return t1597;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s1598 = v_fs;
  u_value t1599 = NULL;
  if ((!t1599) && u_is_con(s1598, &n_list_46_Nil)) {
    t1599 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1599) && u_is_con(s1598, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s1598, 0), 0);
    u_value v_d = u_field(u_field(s1598, 0), 1);
    u_value v_rest = u_field(s1598, 1);
    u_value t1600;
    if (u_truth(u_eq(v_g, v_f))) {
      t1600 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t1600 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t1599 = t1600;
  }
  if (!t1599) {
    u_no_case(s1598);
  }
  return t1599;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1601 = v_ks;
  u_value t1602 = NULL;
  if ((!t1602) && u_is_con(s1601, &n_list_46_Nil)) {
    t1602 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1602) && (u_is_con(s1601, &n_list_46_Cons) && u_is_con(u_field(u_field(s1601, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1601, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1601, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1601, 0), 1);
    u_value v_rest = u_field(s1601, 1);
    t1602 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1602) {
    u_no_case(s1601);
  }
  return t1602;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1603 = v_fs;
  u_value t1604 = NULL;
  if ((!t1604) && u_is_con(s1603, &n_list_46_Nil)) {
    t1604 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1604) && (u_is_con(s1603, &n_list_46_Cons) && u_is_con(u_field(u_field(s1603, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1603, 0), 0), 0);
    u_value v_t = u_field(u_field(s1603, 0), 1);
    u_value v_rest = u_field(s1603, 1);
    t1604 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1604) && (u_is_con(s1603, &n_list_46_Cons) && u_is_con(u_field(u_field(s1603, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1603, 1);
    t1604 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1604) {
    u_no_case(s1603);
  }
  return t1604;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1609 = f_parse_46_fields_of_1(v_ks);
  u_value t1608;
  {
    u_value v_fs = t1609;
    u_value t1605 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1606 = &t1605;
    u_value l1607 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1607, &n_list_46_Cons)) {
      u_value v_f = u_field(l1607, 0);
      if (u_truth(u_mk_bool(1))) {
        e1606 = u_append(e1606, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1607 = u_field(l1607, 1);
    }
    t1608 = t1605;
  }
  return t1608;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1610 = U_LIT("x");
  u_value t1611 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1612 = &t1611;
  u_value l1613 = v_ps;
  while (u_is_con(l1613, &n_list_46_Cons)) {
    u_value v_p = u_field(l1613, 0);
    if (u_truth(u_mk_bool(1))) {
      e1612 = u_append(e1612, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1613 = u_field(l1613, 1);
  }
  u_value t1621 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1610), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1622 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1611);
  u_value t1620;
  {
    u_value v_x = t1621;
    u_value v_owner = t1622;
    static struct u_object l__46__1614 = U_LIT(".");
    static struct u_object l_x_1615 = U_LIT("x");
    u_value t1616 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1617 = &t1616;
    u_value l1618 = v_es;
    while (u_is_con(l1618, &n_list_46_Cons)) {
      u_value v_e = u_field(l1618, 0);
      if (u_truth(u_mk_bool(1))) {
        e1617 = u_append(e1617, f_parse_46_field_case_1(v_e));
      }
      l1618 = u_field(l1618, 1);
    }
    u_value t1619;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1619 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1619 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1620 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1614, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1615), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1616, t1619)));
  }
  return t1620;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1623 = v_es;
  u_value t1624 = NULL;
  if ((!t1624) && u_is_con(s1623, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1623, 0), 1), 3);
    t1624 = v_t;
  }
  if (!t1624) {
    t1624 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1624) {
    u_no_case(s1623);
  }
  return t1624;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s1625 = v_e;
  u_value t1626 = NULL;
  if (!t1626) {
    u_value v_k = u_field(u_field(s1625, 1), 0);
    u_value v_i = u_field(u_field(s1625, 1), 1);
    u_value v_n = u_field(u_field(s1625, 1), 2);
    u_value t1627 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1628 = &t1627;
    u_value l1629 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1629, &n_list_46_Cons)) {
      u_value v_j = u_field(l1629, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1631;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1630 = U_LIT("y");
          t1631 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1630), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1631 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1628 = u_append(e1628, t1631);
      }
      l1629 = u_field(l1629, 1);
    }
    static struct u_object l_y_1632 = U_LIT("y");
    t1626 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1627), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1632), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1626) {
    u_no_case(s1625);
  }
  return t1626;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_MissingField_1633 = U_LIT("MissingField");
  static struct u_object l__46__1634 = U_LIT(".");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_ERaise, 2, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_MissingField_1633), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_t, u_concat(&l__46__1634, v_f)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t1635;
  if (u_truth(u_eq(v_i, v_n))) {
    t1635 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1635 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t1635;
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
    u_value t1636 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1636;
    u_value t1638 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1638)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1637 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1637;
      }
      t1638 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1639 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1639), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1640 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1640), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1641 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1641;
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
    u_value t1642 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1643 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1644 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1645 = u_mk_bool(1);
    v_ins = t1642;
    v_outs = t1643;
    v_refs = t1644;
    v_first = t1645;
    static struct u_object l_in_1646 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1646))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_ins, r_ts);
        u_value t1647 = u_mk_bool(0);
        v_first = t1647;
      }
    }
    static struct u_object l_out_1648 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1648, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_outs, r_ts);
        u_value t1649 = u_mk_bool(0);
        v_first = t1649;
      }
    }
    static struct u_object l_ref_1650 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1650, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    static struct u_object l__58__61__1651 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1651), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1652 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1652;
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
    u_value t1653 = f_parse_46_Context_46_notations_1(v_cx);
    v_ns = t1653;
    u_value s1654 = f_parse_46_head_token_1(*r_ts);
    int d1655 = 0;
    if ((!d1655) && (u_is_con(s1654, &n_lex_46_TName) && u_equals(u_field(s1654, 0), u_mk_lit("import")))) {
      d1655 = 1;
      {
        u_value t1656 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1656;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Context_46_modules_1(v_cx))))) {
          {
            u_value t1657 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1657;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_cx, v_at, r_cs);
            u_value t1658 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1658;
          }
        }
      }
    }
    if ((!d1655) && u_is_con(s1654, &n_lex_46_TName)) {
      u_value v_w = u_field(s1654, 0);
      static struct u_object l_prefix_1660 = U_LIT("prefix");
      static struct u_object l_postfix_1661 = U_LIT("postfix");
      static struct u_object l_infix_1662 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1660, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1661, u_mk_con(&n_list_46_Cons, 2, &l_infix_1662, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1655 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1659 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1659;
        }
      }
    }
    if ((!d1655) && (u_is_con(s1654, &n_lex_46_TName) && u_equals(u_field(s1654, 0), u_mk_lit("type")))) {
      d1655 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1663 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1663), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1664 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1664;
      }
    }
    if ((!d1655) && (u_is_con(s1654, &n_lex_46_TName) && u_equals(u_field(s1654, 0), u_mk_lit("data")))) {
      d1655 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1655) && (u_is_con(s1654, &n_lex_46_TName) && u_equals(u_field(s1654, 0), u_mk_lit("function")))) {
      d1655 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1665 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1665;
      }
    }
    if ((!d1655) && (u_is_con(s1654, &n_lex_46_TName) && u_equals(u_field(s1654, 0), u_mk_lit("procedure")))) {
      d1655 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1666 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1666;
      }
    }
    if ((!d1655) && (u_is_con(s1654, &n_lex_46_TName) && u_equals(u_field(s1654, 0), u_mk_lit("eval")))) {
      d1655 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_e, r_ts);
        u_value t1667 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CEval, 1, v_e), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1667;
      }
    }
    if ((!d1655) && (u_is_con(s1654, &n_lex_46_TName) && u_equals(u_field(s1654, 0), u_mk_lit("run")))) {
      d1655 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1668 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1668), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1669 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1669;
      }
    }
    if ((!d1655) && (u_is_con(s1654, &n_lex_46_TName) && u_equals(u_field(s1654, 0), u_mk_lit("test")))) {
      d1655 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1670 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1670), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1671 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1671;
      }
    }
    if (!d1655) {
      d1655 = 1;
      {
        static struct u_object l_declaration_32_expe_1672 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1673 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1672, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1673)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1655) {
      u_no_case(s1654);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_cx) {
  {
    u_value s1674 = v_cs;
    int d1675 = 0;
    if ((!d1675) && u_is_con(s1674, &n_list_46_Nil)) {
      d1675 = 1;
      {
      }
    }
    if ((!d1675) && (u_is_con(s1674, &n_list_46_Cons) && u_is_con(u_field(s1674, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1674, 0), 0);
      u_value v_p = u_field(u_field(s1674, 0), 1);
      u_value v_ops = u_field(u_field(s1674, 0), 2);
      u_value v_more = u_field(s1674, 1);
      d1675 = 1;
      {
        u_value t1676 = f_parse_46_with_commands_2(*r_cx, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_cx = t1676;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1675) && (u_is_con(s1674, &n_list_46_Cons) && u_is_con(u_field(s1674, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1674, 0), 0);
      u_value v_ds = u_field(u_field(s1674, 0), 1);
      u_value v_more = u_field(s1674, 1);
      d1675 = 1;
      {
        u_value t1677 = f_parse_46_with_module_2(*r_cx, u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds));
        *r_cx = t1677;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1675) && (u_is_con(s1674, &n_list_46_Cons) && u_is_con(u_field(s1674, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1674, 0), 0);
      u_value v_more = u_field(s1674, 1);
      d1675 = 1;
      {
        u_value t1678 = f_parse_46_with_commands_2(*r_cx, f_parse_46_own_notations_2(*r_cx, v_n));
        *r_cx = t1678;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1675) && u_is_con(s1674, &n_list_46_Cons)) {
      u_value v_more = u_field(s1674, 1);
      d1675 = 1;
      {
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if (!d1675) {
      u_no_case(s1674);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_cx, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1679 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1679;
    u_value t1681 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1681)) {
      {
        p_parse_46_parse_command(*r_cx, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_cx);
        u_value t1680 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1680;
      }
      t1681 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1682 = f_list_46_reverse_1(v_rev);
    *r_cs = t1682;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1683 = v_dir;
    int d1684 = 0;
    if ((!d1684) && u_is_con(s1683, &n_option_46_Some)) {
      u_value v_d = u_field(s1683, 0);
      d1684 = 1;
      {
        {
          struct u_handler *h1687 = u_push();
          if (setjmp(h1687->env) == 0) {
            {
              static struct u_object l__47__1685 = U_LIT("/");
              static struct u_object l__46_u_1686 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1685, u_concat(v_n, &l__46_u_1686))), r_text);
            }
            u_pop(h1687);
          } else {
            u_pop(h1687);
            int d1688 = 0;
            if ((!d1688) && (u_has_type(h1687, "os.OsError") && u_is_con(u_caught(h1687), &n_os_46_OsError))) {
              d1688 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1688) {
              u_reraise(h1687);
            }
          }
        }
      }
    }
    if ((!d1684) && u_is_con(s1683, &n_option_46_None)) {
      d1684 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1684) {
      u_no_case(s1683);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1689 = u_push();
      if (setjmp(h1689->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1689);
      } else {
        u_pop(h1689);
        int d1690 = 0;
        if ((!d1690) && (u_has_type(h1689, "os.OsError") && u_is_con(u_caught(h1689), &n_os_46_OsError))) {
          d1690 = 1;
          {
            static struct u_object l_unknown_32_module_32__1691 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1691, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1690) {
          u_reraise(h1689);
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
        static struct u_object l_cyclic_32_import_32_of_1692 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1692, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Context_46_dir_1(v_cx), v_at, &v_text);
    u_value t1693 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_base_notations_1(v_cx), f_parse_46_Context_46_modules_1(v_cx), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Context_46_loading_1(v_cx)), f_parse_46_Context_46_base_1(v_cx), f_parse_46_Context_46_dir_1(v_cx));
    v_mcx = t1693;
    {
      struct u_handler *h1694 = u_push();
      if (setjmp(h1694->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mcx, &v_mts);
        }
        u_pop(h1694);
      } else {
        u_pop(h1694);
        int d1695 = 0;
        if ((!d1695) && (u_has_type(h1694, "lex.LexError") && u_is_con(u_caught(h1694), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1694), 0);
          u_value v_i = u_field(u_caught(h1694), 1);
          d1695 = 1;
          {
            static struct u_object l__32_in_32__1696 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1696, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1695) && (u_has_type(h1694, "parse.ParseError") && u_is_con(u_caught(h1694), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1694), 0);
          u_value v_i = u_field(u_caught(h1694), 1);
          d1695 = 1;
          {
            static struct u_object l__32_in_32__1697 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1697, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1695) {
          u_reraise(h1694);
        }
      }
    }
  }
}

u_value f_parse_46_base_notations_1(u_value v_cx) {
  u_value s1698 = f_parse_46_Context_46_base_1(v_cx);
  u_value t1699 = NULL;
  if ((!t1699) && u_is_con(s1698, &n_option_46_Some)) {
    u_value v_b = u_field(s1698, 0);
    t1699 = v_b;
  }
  if ((!t1699) && u_is_con(s1698, &n_option_46_None)) {
    t1699 = f_parse_46_Context_46_notations_1(v_cx);
  }
  if (!t1699) {
    u_no_case(s1698);
  }
  return t1699;
}

void p_parse_46_parse_text(u_value v_text, u_value v_cx, u_value *r_p) {
  {
    u_value v_ts = NULL;
    u_value v_cs = NULL;
    u_value v_after = NULL;
    u_value t1700 = v_cx;
    v_after = t1700;
    {
      struct u_handler *h1701 = u_push();
      if (setjmp(h1701->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1701);
      } else {
        u_pop(h1701);
        int d1702 = 0;
        if ((!d1702) && (u_has_type(h1701, "lex.LexError") && u_is_con(u_caught(h1701), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1701), 0);
          u_value v_i = u_field(u_caught(h1701), 1);
          d1702 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1702) {
          u_reraise(h1701);
        }
      }
    }
    p_parse_46_parse_commands(&v_cs, &v_after, &v_ts);
    u_value t1703 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1703;
  }
}

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p) {
  {
    u_value v_cx = NULL;
    u_value t1704 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    v_cx = t1704;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), &v_cx);
    p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_Context_46_notations_1(v_cx), f_parse_46_Context_46_modules_1(v_cx), u_mk_con(&n_list_46_Cons, 2, v_name, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_Some, 1, f_parse_46_Context_46_notations_1(v_cx)), v_dir), r_p);
  }
}

void p_parse_46_parse_library(u_value v_n, u_value *r_p) {
  {
    u_value v_text = NULL;
    u_value v_pre = NULL;
    p_parse_46_library_text(v_n, u_mk_int(0), &v_text);
    static struct u_object l_prelude_1705 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1705))) {
      {
        static struct u_object l_prelude_1706 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1706, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1707 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1707, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1708 = v_xs;
  u_value t1709 = NULL;
  if ((!t1709) && (u_is_con(s1708, &n_list_46_Cons) && u_is_con(u_field(s1708, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1708, 0);
    t1709 = v_x;
  }
  if ((!t1709) && u_is_con(s1708, &n_list_46_Cons)) {
    u_value v_more = u_field(s1708, 1);
    t1709 = f_parse_46_last_part_1(v_more);
  }
  if (!t1709) {
    static struct u_object l__1710 = U_LIT("");
    t1709 = &l__1710;
  }
  if (!t1709) {
    u_no_case(s1708);
  }
  return t1709;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1715 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1714;
  {
    u_value v_f = t1715;
    u_value t1712 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1712)) {
      static struct u_object l__46_u_1711 = U_LIT(".u");
      t1712 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1711);
    }
    u_value t1713;
    if (u_truth(t1712)) {
      t1713 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1713 = v_f;
    }
    t1714 = t1713;
  }
  return t1714;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1720 = u_str_split(v_path, u_mk_char(47));
  u_value t1719;
  {
    u_value v_parts = t1720;
    u_value t1718;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1716 = U_LIT(".");
      t1718 = &l__46__1716;
    } else {
      static struct u_object l__47__1717 = U_LIT("/");
      t1718 = u_str_join(&l__47__1717, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1719 = t1718;
  }
  return t1719;
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
    static struct u_object l_prelude_1721 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1721, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1722 = u_mk_tuple(2, v_cs, v_ds);
    int d1723 = 0;
    if ((!d1723) && (u_is_con(u_field(s1722, 0), &n_list_46_Cons) && u_is_con(u_field(s1722, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1722, 0), 0);
      u_value v_more = u_field(u_field(s1722, 0), 1);
      u_value v_d = u_field(u_field(s1722, 1), 0);
      u_value v_rest = u_field(u_field(s1722, 1), 1);
      d1723 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1723) {
      d1723 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1723) {
      u_no_case(s1722);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1724 = v_paths;
    int d1725 = 0;
    if ((!d1725) && u_is_con(s1724, &n_list_46_Nil)) {
      d1725 = 1;
      {
      }
    }
    if ((!d1725) && u_is_con(s1724, &n_list_46_Cons)) {
      u_value v_path = u_field(s1724, 0);
      u_value v_more = u_field(s1724, 1);
      d1725 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1725) {
      u_no_case(s1724);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1726 = v_paths;
    int d1727 = 0;
    if ((!d1727) && u_is_con(s1726, &n_list_46_Nil)) {
      d1727 = 1;
      {
      }
    }
    if ((!d1727) && u_is_con(s1726, &n_list_46_Cons)) {
      u_value v_path = u_field(s1726, 0);
      u_value v_more = u_field(s1726, 1);
      d1727 = 1;
      {
        u_value t1728 = u_mk_bool(0);
        v_failed = t1728;
        {
          struct u_handler *h1729 = u_push();
          if (setjmp(h1729->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1729);
          } else {
            u_pop(h1729);
            int d1730 = 0;
            if ((!d1730) && (u_has_type(h1729, "parse.ParseError") && u_is_con(u_caught(h1729), &n_parse_46_ParseError))) {
              d1730 = 1;
              {
                u_value t1731 = u_mk_bool(1);
                v_failed = t1731;
              }
            }
            if (!d1730) {
              u_reraise(h1729);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1727) {
      u_no_case(s1726);
    }
  }
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s1732 = v_x;
  u_value t1733 = NULL;
  if ((!t1733) && u_is_con(s1732, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1732, 0);
    t1733 = v_y;
  }
  if (!t1733) {
    u_no_case(s1732);
  }
  return t1733;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s1734 = v_x;
  u_value t1735 = NULL;
  if ((!t1735) && u_is_con(s1734, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1734, 1);
    t1735 = v_y;
  }
  if (!t1735) {
    u_no_case(s1734);
  }
  return t1735;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s1736 = v_x;
  u_value t1737 = NULL;
  if ((!t1737) && u_is_con(s1736, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1736, 2);
    t1737 = v_y;
  }
  if (!t1737) {
    u_no_case(s1736);
  }
  return t1737;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s1738 = v_x;
  u_value t1739 = NULL;
  if ((!t1739) && u_is_con(s1738, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1738, 3);
    t1739 = v_y;
  }
  if (!t1739) {
    u_no_case(s1738);
  }
  return t1739;
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
  static struct u_object l_u_value_1740 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1740);
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
  static struct u_object l_u_value_1741 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1741));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t1742 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1743 = &t1742;
  u_value l1744 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l1744, &n_list_46_Cons)) {
    u_value v_c = u_field(l1744, 0);
    if (u_truth(u_mk_bool(1))) {
      e1743 = u_append(e1743, f_compile_46_mangle_char_1(v_c));
    }
    l1744 = u_field(l1744, 1);
  }
  return u_string(f_list_46_concat_1(t1742));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t1745 = u_is_letter(v_c);
  if (!u_truth(t1745)) {
    t1745 = u_is_digit(v_c);
  }
  u_value t1746 = t1745;
  if (!u_truth(t1746)) {
    t1746 = u_eq(v_c, u_mk_char(95));
  }
  u_value t1749;
  if (u_truth(t1746)) {
    t1749 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___1747 = U_LIT("_");
    static struct u_object l___1748 = U_LIT("_");
    t1749 = u_chars(u_concat(&l___1747, u_concat(u_str_show(u_ord(v_c)), &l___1748)));
  }
  return t1749;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__1750 = U_LIT("f_");
  static struct u_object l___1751 = U_LIT("_");
  return u_concat(&l_f__1750, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___1751, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__1752 = U_LIT("p_");
  return u_concat(&l_p__1752, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__1753 = U_LIT("v_");
  return u_concat(&l_v__1753, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__1754 = U_LIT("r_");
  return u_concat(&l_r__1754, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__1755 = U_LIT("+");
  static struct u_object l_u_add_1756 = U_LIT("u_add");
  static struct u_object l__45__1757 = U_LIT("-");
  static struct u_object l_u_sub_1758 = U_LIT("u_sub");
  static struct u_object l__42__1759 = U_LIT("*");
  static struct u_object l_u_mul_1760 = U_LIT("u_mul");
  static struct u_object l__47__1761 = U_LIT("/");
  static struct u_object l_u_div_1762 = U_LIT("u_div");
  static struct u_object l__37__1763 = U_LIT("%");
  static struct u_object l_u_mod_1764 = U_LIT("u_mod");
  static struct u_object l__60__1765 = U_LIT("<");
  static struct u_object l_u_lt_1766 = U_LIT("u_lt");
  static struct u_object l__61__61__1767 = U_LIT("==");
  static struct u_object l_u_eq_1768 = U_LIT("u_eq");
  static struct u_object l_compare_1769 = U_LIT("compare");
  static struct u_object l_u_compare_1770 = U_LIT("u_compare");
  static struct u_object l_chars_1771 = U_LIT("chars");
  static struct u_object l_u_chars_1772 = U_LIT("u_chars");
  static struct u_object l_string_1773 = U_LIT("string");
  static struct u_object l_u_string_1774 = U_LIT("u_string");
  static struct u_object l_ord_1775 = U_LIT("ord");
  static struct u_object l_u_ord_1776 = U_LIT("u_ord");
  static struct u_object l_chr_1777 = U_LIT("chr");
  static struct u_object l_u_chr_1778 = U_LIT("u_chr");
  static struct u_object l__94__1779 = U_LIT("^");
  static struct u_object l_u_concat_1780 = U_LIT("u_concat");
  static struct u_object l_str_size_1781 = U_LIT("str_size");
  static struct u_object l_u_str_size_1782 = U_LIT("u_str_size");
  static struct u_object l_str_join_1783 = U_LIT("str_join");
  static struct u_object l_u_str_join_1784 = U_LIT("u_str_join");
  static struct u_object l_str_slice_1785 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_1786 = U_LIT("u_str_slice");
  static struct u_object l_str_split_1787 = U_LIT("str_split");
  static struct u_object l_u_str_split_1788 = U_LIT("u_str_split");
  static struct u_object l_str_get_1789 = U_LIT("str_get");
  static struct u_object l_u_str_get_1790 = U_LIT("u_str_get");
  static struct u_object l_str_contains_1791 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_1792 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_1793 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_1794 = U_LIT("u_str_compare");
  static struct u_object l_str_show_1795 = U_LIT("str_show");
  static struct u_object l_u_str_show_1796 = U_LIT("u_str_show");
  static struct u_object l_str_quote_1797 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_1798 = U_LIT("u_str_quote");
  static struct u_object l_str_number_1799 = U_LIT("str_number");
  static struct u_object l_u_str_number_1800 = U_LIT("u_str_number");
  static struct u_object l_is_digit_1801 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_1802 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_1803 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_1804 = U_LIT("u_is_letter");
  static struct u_object l_is_space_1805 = U_LIT("is_space");
  static struct u_object l_u_is_space_1806 = U_LIT("u_is_space");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__1755), u_mk_tuple(2, &l_u_add_1756, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__1757), u_mk_tuple(2, &l_u_sub_1758, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__1759), u_mk_tuple(2, &l_u_mul_1760, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__1761), u_mk_tuple(2, &l_u_div_1762, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__1763), u_mk_tuple(2, &l_u_mod_1764, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__1765), u_mk_tuple(2, &l_u_lt_1766, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__1767), u_mk_tuple(2, &l_u_eq_1768, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_1769), u_mk_tuple(2, &l_u_compare_1770, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_1771), u_mk_tuple(2, &l_u_chars_1772, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_1773), u_mk_tuple(2, &l_u_string_1774, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_1775), u_mk_tuple(2, &l_u_ord_1776, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_1777), u_mk_tuple(2, &l_u_chr_1778, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__1779), u_mk_tuple(2, &l_u_concat_1780, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_1781), u_mk_tuple(2, &l_u_str_size_1782, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_1783), u_mk_tuple(2, &l_u_str_join_1784, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_1785), u_mk_tuple(2, &l_u_str_slice_1786, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_1787), u_mk_tuple(2, &l_u_str_split_1788, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_1789), u_mk_tuple(2, &l_u_str_get_1790, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_1791), u_mk_tuple(2, &l_u_str_contains_1792, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_1793), u_mk_tuple(2, &l_u_str_compare_1794, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_1795), u_mk_tuple(2, &l_u_str_show_1796, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_1797), u_mk_tuple(2, &l_u_str_quote_1798, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_1799), u_mk_tuple(2, &l_u_str_number_1800, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_1801), u_mk_tuple(2, &l_u_is_digit_1802, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_1803), u_mk_tuple(2, &l_u_is_letter_1804, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_1805), u_mk_tuple(2, &l_u_is_space_1806, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)))))))))))))))))))))))))));
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__1807 = U_LIT("&");
  static struct u_object l_n__1808 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__1807, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__1808, f_compile_46_mangle_1(v_n))));
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
  u_value s1809 = v_n;
  u_value t1810 = NULL;
  if ((!t1810) && (u_is_con(s1809, &n_syntax_46_Name) && u_is_con(u_field(s1809, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s1809, 0), 0);
    u_value v_b = u_field(s1809, 1);
    static struct u_object l_const_32_struct_32_u_n_1811 = U_LIT("const struct u_name");
    static struct u_object l_n__1812 = U_LIT("n_");
    t1810 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1811), u_concat(&l_n__1812, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t1810) && (u_is_con(s1809, &n_syntax_46_Name) && u_is_con(u_field(s1809, 0), &n_option_46_None))) {
    u_value v_b = u_field(s1809, 1);
    static struct u_object l_const_32_struct_32_u_n_1813 = U_LIT("const struct u_name");
    static struct u_object l_n__1814 = U_LIT("n_");
    static struct u_object l_NULL_1815 = U_LIT("NULL");
    t1810 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1813), u_concat(&l_n__1814, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1815), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t1810) {
    u_no_case(s1809);
  }
  return t1810;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s1816 = v_cs;
  u_value t1817 = NULL;
  if ((!t1817) && u_is_con(s1816, &n_list_46_Nil)) {
    t1817 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1817) && (u_is_con(s1816, &n_list_46_Cons) && u_is_con(u_field(s1816, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s1816, 0), 2);
    u_value v_rest = u_field(s1816, 1);
    u_value t1818 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1819 = &t1818;
    u_value l1820 = v_ks;
    while (u_is_con(l1820, &n_list_46_Cons)) {
      u_value v_k = u_field(l1820, 0);
      if (u_truth(u_mk_bool(1))) {
        e1819 = u_append(e1819, f_syntax_46_Constr_46_name_1(v_k));
      }
      l1820 = u_field(l1820, 1);
    }
    t1817 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t1818, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t1817) && u_is_con(s1816, &n_list_46_Cons)) {
    u_value v_rest = u_field(s1816, 1);
    t1817 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t1817) {
    u_no_case(s1816);
  }
  return t1817;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s1821 = v_ks;
  u_value t1822 = NULL;
  if ((!t1822) && u_is_con(s1821, &n_list_46_Nil)) {
    t1822 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1822) && u_is_con(s1821, &n_list_46_Cons)) {
    u_value v_k = u_field(s1821, 0);
    u_value v_rest = u_field(s1821, 1);
    u_value t1823;
    if (u_truth(f_list_46_contains_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t1823 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t1823 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t1822 = t1823;
  }
  if (!t1822) {
    u_no_case(s1821);
  }
  return t1822;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t1824 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1825 = &t1824;
  u_value l1826 = v_xs;
  while (u_is_con(l1826, &n_list_46_Cons)) {
    u_value v_x = u_field(l1826, 0);
    if (u_truth(u_mk_bool(1))) {
      e1825 = u_append(e1825, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l1826 = u_field(l1826, 1);
  }
  return t1824;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__1827 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__1827, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s1828 = f_list_46_lookup_2(v_x, v_env);
  u_value t1829 = NULL;
  if ((!t1829) && u_is_con(s1828, &n_option_46_Some)) {
    u_value v_c = u_field(s1828, 0);
    t1829 = v_c;
  }
  if (!t1829) {
    t1829 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t1829) {
    u_no_case(s1828);
  }
  return t1829;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s1830 = v_c;
  u_value t1831 = NULL;
  if ((!t1831) && (u_is_con(s1830, &n_c_46_CeUnary) && u_equals(u_field(s1830, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s1830, 1);
    t1831 = v_p;
  }
  if (!t1831) {
    static struct u_object l__38__1832 = U_LIT("&");
    t1831 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1832, v_c);
  }
  if (!t1831) {
    u_no_case(s1830);
  }
  return t1831;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s1833 = v_t;
  u_value t1834 = NULL;
  if ((!t1834) && (u_is_con(s1833, &n_syntax_46_TSort) && u_is_con(u_field(s1833, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s1833, 0);
    t1834 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t1834) && u_is_con(s1833, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s1833, 0);
    u_value v_args = u_field(s1833, 1);
    static struct u_object l__91__1835 = U_LIT("[");
    static struct u_object l__44__32__1836 = U_LIT(", ");
    u_value t1837 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1838 = &t1837;
    u_value l1839 = v_args;
    while (u_is_con(l1839, &n_list_46_Cons)) {
      u_value v_a = u_field(l1839, 0);
      if (u_truth(u_mk_bool(1))) {
        e1838 = u_append(e1838, f_compile_46_type_key_1(v_a));
      }
      l1839 = u_field(l1839, 1);
    }
    static struct u_object l__93__1840 = U_LIT("]");
    t1834 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__1835, u_concat(u_str_join(&l__44__32__1836, t1837), &l__93__1840)));
  }
  if ((!t1834) && u_is_con(s1833, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1833, 0);
    u_value v_r = u_field(s1833, 1);
    static struct u_object l__40__1841 = U_LIT("(");
    static struct u_object l__44__32__1842 = U_LIT(", ");
    u_value t1843 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1844 = &t1843;
    u_value l1845 = v_args;
    while (u_is_con(l1845, &n_list_46_Cons)) {
      u_value v_a = u_field(l1845, 0);
      if (u_truth(u_mk_bool(1))) {
        e1844 = u_append(e1844, f_compile_46_type_key_1(v_a));
      }
      l1845 = u_field(l1845, 1);
    }
    static struct u_object l__41__32__45__62__32__1846 = U_LIT(") -> ");
    t1834 = u_concat(&l__40__1841, u_concat(u_str_join(&l__44__32__1842, t1843), u_concat(&l__41__32__45__62__32__1846, f_compile_46_type_key_1(v_r))));
  }
  if ((!t1834) && u_is_con(s1833, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1833, 0);
    static struct u_object l__40__1847 = U_LIT("(");
    static struct u_object l__32__42__32__1848 = U_LIT(" * ");
    u_value t1849 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1850 = &t1849;
    u_value l1851 = v_ts;
    while (u_is_con(l1851, &n_list_46_Cons)) {
      u_value v_a = u_field(l1851, 0);
      if (u_truth(u_mk_bool(1))) {
        e1850 = u_append(e1850, f_compile_46_type_key_1(v_a));
      }
      l1851 = u_field(l1851, 1);
    }
    static struct u_object l__41__1852 = U_LIT(")");
    t1834 = u_concat(&l__40__1847, u_concat(u_str_join(&l__32__42__32__1848, t1849), &l__41__1852));
  }
  if (!t1834) {
    static struct u_object l__63__1853 = U_LIT("?");
    t1834 = &l__63__1853;
  }
  if (!t1834) {
    u_no_case(s1833);
  }
  return t1834;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t1854 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1855 = &t1854;
  u_value l1856 = v_cs;
  while (u_is_con(l1856, &n_list_46_Cons)) {
    u_value v_c = u_field(l1856, 0);
    if (u_truth(u_mk_bool(1))) {
      e1855 = u_append(e1855, f_compile_46_fun_arities_1(v_c));
    }
    l1856 = u_field(l1856, 1);
  }
  u_value t1857 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1858 = &t1857;
  u_value l1859 = v_cs;
  while (u_is_con(l1859, &n_list_46_Cons)) {
    u_value v_c = u_field(l1859, 0);
    if (u_truth(u_mk_bool(1))) {
      e1858 = u_append(e1858, f_compile_46_constr_arities_1(v_c));
    }
    l1859 = u_field(l1859, 1);
  }
  u_value t1861 = f_list_46_concat_1(t1854);
  u_value t1862 = f_list_46_concat_1(t1857);
  u_value t1860;
  {
    u_value v_fs = t1861;
    u_value v_ks = t1862;
    t1860 = u_mk_con(&n_compile_46_Globals, 4, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()));
  }
  return t1860;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s1863 = v_fs;
  u_value t1864 = NULL;
  if ((!t1864) && u_is_con(s1863, &n_list_46_Nil)) {
    t1864 = v_m;
  }
  if ((!t1864) && u_is_con(s1863, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1863, 0), 0);
    u_value v_k = u_field(u_field(s1863, 0), 1);
    u_value v_rest = u_field(s1863, 1);
    u_value s1865 = f_map_46_get_2(v_f, v_m);
    u_value t1866 = NULL;
    if ((!t1866) && u_is_con(s1865, &n_option_46_Some)) {
      u_value v_ks = u_field(s1865, 0);
      t1866 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t1866) && u_is_con(s1865, &n_option_46_None)) {
      t1866 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t1866) {
      u_no_case(s1865);
    }
    t1864 = t1866;
  }
  if (!t1864) {
    u_no_case(s1863);
  }
  return t1864;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s1867 = v_gl;
  u_value t1868 = NULL;
  if ((!t1868) && u_is_con(s1867, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s1867, 2);
    u_value s1869 = f_map_46_get_2(v_n, v_fm);
    u_value t1870 = NULL;
    if ((!t1870) && u_is_con(s1869, &n_option_46_Some)) {
      u_value v_ks = u_field(s1869, 0);
      t1870 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t1870) && u_is_con(s1869, &n_option_46_None)) {
      t1870 = u_mk_bool(0);
    }
    if (!t1870) {
      u_no_case(s1869);
    }
    t1868 = t1870;
  }
  if (!t1868) {
    u_no_case(s1867);
  }
  return t1868;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s1871 = v_gl;
  u_value t1872 = NULL;
  if ((!t1872) && u_is_con(s1871, &n_compile_46_Globals)) {
    u_value v_km = u_field(s1871, 3);
    t1872 = f_map_46_get_2(v_n, v_km);
  }
  if (!t1872) {
    u_no_case(s1871);
  }
  return t1872;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s1873 = v_c;
  u_value t1874 = NULL;
  if ((!t1874) && u_is_con(s1873, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s1873, 0);
    u_value v_xss = u_field(s1873, 2);
    t1874 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1874) {
    t1874 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1874) {
    u_no_case(s1873);
  }
  return t1874;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s1875 = v_c;
  u_value t1876 = NULL;
  if ((!t1876) && u_is_con(s1875, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s1875, 2);
    u_value t1877 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1878 = &t1877;
    u_value l1879 = v_ks;
    while (u_is_con(l1879, &n_list_46_Cons)) {
      u_value v_k = u_field(l1879, 0);
      if (u_truth(u_mk_bool(1))) {
        e1878 = u_append(e1878, f_compile_46_constr_arity_1(v_k));
      }
      l1879 = u_field(l1879, 1);
    }
    t1876 = t1877;
  }
  if (!t1876) {
    t1876 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1876) {
    u_no_case(s1875);
  }
  return t1876;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s1880 = v_k;
  u_value t1881 = NULL;
  if ((!t1881) && u_is_con(s1880, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s1880, 0);
    u_value v_ts = u_field(s1880, 1);
    t1881 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t1881) {
    u_no_case(s1880);
  }
  return t1881;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s1882 = v_xs;
  u_value t1883 = NULL;
  if ((!t1883) && u_is_con(s1882, &n_list_46_Nil)) {
    t1883 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1883) && u_is_con(s1882, &n_list_46_Cons)) {
    u_value v_x = u_field(s1882, 0);
    u_value v_rest = u_field(s1882, 1);
    t1883 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t1883) {
    u_no_case(s1882);
  }
  return t1883;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_1884 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_1884, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_1885 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_1885, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s1886 = v_cs;
  u_value t1887 = NULL;
  if ((!t1887) && u_is_con(s1886, &n_list_46_Nil)) {
    t1887 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t1887) && (u_is_con(s1886, &n_list_46_Cons) && u_is_con(u_field(s1886, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s1886, 0);
    t1887 = v_d;
  }
  if ((!t1887) && u_is_con(s1886, &n_list_46_Cons)) {
    u_value v_d = u_field(s1886, 0);
    u_value v_rest = u_field(s1886, 1);
    static struct u_object l__38__38__1888 = U_LIT("&&");
    t1887 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__1888, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t1887) {
    u_no_case(s1886);
  }
  return t1887;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s1889 = v_l;
  u_value t1890 = NULL;
  if ((!t1890) && u_is_con(s1889, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_1891 = U_LIT("u_mk_tuple");
    t1890 = f_c_46_c_call_2(&l_u_mk_tuple_1891, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1890) && u_is_con(s1889, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s1889, 0);
    static struct u_object l_u_mk_bool_1892 = U_LIT("u_mk_bool");
    u_value t1893;
    if (u_truth(v_b)) {
      t1893 = u_mk_int(1);
    } else {
      t1893 = u_mk_int(0);
    }
    t1890 = f_c_46_c_call_2(&l_u_mk_bool_1892, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t1893), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1890) && u_is_con(s1889, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s1889, 0);
    u_value t1894 = u_lt(f_prelude_46__45__1(u_mk_int_str("1000000000000000000")), v_n);
    if (u_truth(t1894)) {
      t1894 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t1897;
    if (u_truth(t1894)) {
      static struct u_object l_u_mk_int_1895 = U_LIT("u_mk_int");
      t1897 = f_c_46_c_call_2(&l_u_mk_int_1895, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_1896 = U_LIT("u_mk_int_str");
      t1897 = f_c_46_c_call_2(&l_u_mk_int_str_1896, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t1890 = t1897;
  }
  if ((!t1890) && u_is_con(s1889, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s1889, 0);
    static struct u_object l_u_mk_lit_1898 = U_LIT("u_mk_lit");
    t1890 = f_c_46_c_call_2(&l_u_mk_lit_1898, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1890) && u_is_con(s1889, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s1889, 0);
    static struct u_object l_u_mk_char_1899 = U_LIT("u_mk_char");
    t1890 = f_c_46_c_call_2(&l_u_mk_char_1899, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1890) {
    u_no_case(s1889);
  }
  return t1890;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s1900 = v_p;
  u_value t1901 = NULL;
  if ((!t1901) && u_is_con(s1900, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s1900, 0);
    static struct u_object l_u_equals_1902 = U_LIT("u_equals");
    t1901 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_1902, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1901) && u_is_con(s1900, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s1900, 0);
    u_value v_ps = u_field(s1900, 1);
    static struct u_object l_u_is_con_1903 = U_LIT("u_is_con");
    t1901 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_1903, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t1901) && u_is_con(s1900, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1900, 0);
    t1901 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1901) {
    t1901 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1901) {
    u_no_case(s1900);
  }
  return t1901;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s1904 = v_ps;
  u_value t1905 = NULL;
  if ((!t1905) && u_is_con(s1904, &n_list_46_Nil)) {
    t1905 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1905) && u_is_con(s1904, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1904, 0), 0);
    u_value v_i = u_field(u_field(s1904, 0), 1);
    u_value v_rest = u_field(s1904, 1);
    t1905 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t1905) {
    u_no_case(s1904);
  }
  return t1905;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s1906 = v_p;
  u_value t1907 = NULL;
  if ((!t1907) && (u_is_con(s1906, &n_syntax_46_PVar) && u_is_con(u_field(s1906, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1906, 0), 0);
    t1907 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1907) && u_is_con(s1906, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1906, 1);
    t1907 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t1907) && u_is_con(s1906, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1906, 0);
    t1907 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1907) {
    t1907 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1907) {
    u_no_case(s1906);
  }
  return t1907;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s1908 = v_ps;
  u_value t1909 = NULL;
  if ((!t1909) && u_is_con(s1908, &n_list_46_Nil)) {
    t1909 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1909) && u_is_con(s1908, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1908, 0), 0);
    u_value v_i = u_field(u_field(s1908, 0), 1);
    u_value v_rest = u_field(s1908, 1);
    t1909 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t1909) {
    u_no_case(s1908);
  }
  return t1909;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s1910 = v_p;
  u_value t1911 = NULL;
  if ((!t1911) && (u_is_con(s1910, &n_syntax_46_PVar) && u_is_con(u_field(s1910, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1910, 0), 0);
    t1911 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1911) && u_is_con(s1910, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1910, 1);
    u_value t1912 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1913 = &t1912;
    u_value l1914 = v_ps;
    while (u_is_con(l1914, &n_list_46_Cons)) {
      u_value v_q = u_field(l1914, 0);
      if (u_truth(u_mk_bool(1))) {
        e1913 = u_append(e1913, f_compile_46_pattern_names_1(v_q));
      }
      l1914 = u_field(l1914, 1);
    }
    t1911 = f_list_46_concat_1(t1912);
  }
  if ((!t1911) && u_is_con(s1910, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1910, 0);
    u_value t1915 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1916 = &t1915;
    u_value l1917 = v_ps;
    while (u_is_con(l1917, &n_list_46_Cons)) {
      u_value v_q = u_field(l1917, 0);
      if (u_truth(u_mk_bool(1))) {
        e1916 = u_append(e1916, f_compile_46_pattern_names_1(v_q));
      }
      l1917 = u_field(l1917, 1);
    }
    t1911 = f_list_46_concat_1(t1915);
  }
  if (!t1911) {
    t1911 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1911) {
    u_no_case(s1910);
  }
  return t1911;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s1918 = v_e;
  u_value t1919 = NULL;
  if ((!t1919) && u_is_con(s1918, &n_syntax_46_ELit)) {
    t1919 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1919) && (u_is_con(s1918, &n_syntax_46_EVar) && u_is_con(u_field(s1918, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1918, 0), 0);
    u_value t1920;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t1920 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1920 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t1919 = t1920;
  }
  if ((!t1919) && u_is_con(s1918, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s1918, 0);
    u_value v_args = u_field(s1918, 1);
    u_value t1921 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1922 = &t1921;
    u_value l1923 = v_args;
    while (u_is_con(l1923, &n_list_46_Cons)) {
      u_value v_a = u_field(l1923, 0);
      if (u_truth(u_mk_bool(1))) {
        e1922 = u_append(e1922, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1923 = u_field(l1923, 1);
    }
    t1919 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t1921));
  }
  if ((!t1919) && u_is_con(s1918, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s1918, 0);
    u_value v_b = u_field(s1918, 1);
    t1919 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t1919) && u_is_con(s1918, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s1918, 0);
    u_value v_b = u_field(s1918, 1);
    u_value t1924 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1925 = &t1924;
    u_value l1926 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l1926, &n_list_46_Cons)) {
      u_value v_a = u_field(l1926, 0);
      if (u_truth(u_mk_bool(1))) {
        e1925 = u_append(e1925, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1926 = u_field(l1926, 1);
    }
    t1919 = f_list_46__43__43__2(f_list_46_concat_1(t1924), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t1919) && u_is_con(s1918, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s1918, 0);
    u_value v_l = u_field(s1918, 1);
    u_value v_r = u_field(s1918, 2);
    t1919 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t1919) && u_is_con(s1918, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s1918, 0);
    u_value v_cs = u_field(s1918, 1);
    u_value t1927 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1928 = &t1927;
    u_value l1929 = v_cs;
    while (u_is_con(l1929, &n_list_46_Cons)) {
      u_value v_b = u_field(l1929, 0);
      if (u_truth(u_mk_bool(1))) {
        e1928 = u_append(e1928, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l1929 = u_field(l1929, 1);
    }
    t1919 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t1927));
  }
  if ((!t1919) && u_is_con(s1918, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s1918, 0);
    u_value v_xs = u_field(s1918, 1);
    u_value v_ls = u_field(s1918, 2);
    u_value v_c = u_field(s1918, 3);
    u_value t1930 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1931 = &t1930;
    u_value l1932 = v_ls;
    while (u_is_con(l1932, &n_list_46_Cons)) {
      u_value v_l = u_field(l1932, 0);
      if (u_truth(u_mk_bool(1))) {
        e1931 = u_append(e1931, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l1932 = u_field(l1932, 1);
    }
    t1919 = f_list_46__43__43__2(f_list_46_concat_1(t1930), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t1919) && u_is_con(s1918, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s1918, 0);
    u_value t1933 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1934 = &t1933;
    u_value l1935 = v_es;
    while (u_is_con(l1935, &n_list_46_Cons)) {
      u_value v_a = u_field(l1935, 0);
      if (u_truth(u_mk_bool(1))) {
        e1934 = u_append(e1934, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1935 = u_field(l1935, 1);
    }
    t1919 = f_list_46_concat_1(t1933);
  }
  if ((!t1919) && u_is_con(s1918, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s1918, 0);
    t1919 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if (!t1919) {
    u_no_case(s1918);
  }
  return t1919;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s1936 = v_b;
  u_value t1937 = NULL;
  if ((!t1937) && (u_is_con(s1936, &n_syntax_46_Branch) && u_is_con(u_field(s1936, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s1936, 0);
    u_value v_g = u_field(u_field(s1936, 1), 0);
    u_value v_e = u_field(s1936, 2);
    t1937 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t1937) && (u_is_con(s1936, &n_syntax_46_Branch) && u_is_con(u_field(s1936, 1), &n_option_46_None))) {
    u_value v_p = u_field(s1936, 0);
    u_value v_e = u_field(s1936, 2);
    t1937 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t1937) {
    u_no_case(s1936);
  }
  return t1937;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s1938 = v_xss;
  u_value t1939 = NULL;
  if ((!t1939) && u_is_con(s1938, &n_list_46_Nil)) {
    t1939 = v_b;
  }
  if ((!t1939) && u_is_con(s1938, &n_list_46_Cons)) {
    u_value v_xs = u_field(s1938, 0);
    u_value v_rest = u_field(s1938, 1);
    t1939 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t1939) {
    u_no_case(s1938);
  }
  return t1939;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value s1940 = *r_g;
    int d1941 = 0;
    if ((!d1941) && u_is_con(s1940, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1940, 0);
      u_value v_ds = u_field(s1940, 1);
      d1941 = 1;
      {
        u_value t1942 = u_concat(v_stem, u_str_show(v_n));
        u_value t1943 = u_mk_con(&n_compile_46_Gen, 2, u_add(v_n, u_mk_int(1)), v_ds);
        *r_x = t1942;
        *r_g = t1943;
      }
    }
    if (!d1941) {
      u_no_case(s1940);
    }
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value s1944 = *r_g;
    int d1945 = 0;
    if ((!d1945) && u_is_con(s1944, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1944, 0);
      u_value v_ds = u_field(s1944, 1);
      d1945 = 1;
      {
        u_value t1946 = u_mk_con(&n_compile_46_Gen, 2, v_n, f_list_46__43__43__2(v_ds, u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
        *r_g = t1946;
      }
    }
    if (!d1945) {
      u_no_case(s1944);
    }
  }
}

void p_compile_46_global_value(u_value v_gl, u_value v_n, u_value v_t, u_value *r_r) {
  {
    if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
      {
        static struct u_object l_u_mk_closure_1947 = U_LIT("u_mk_closure");
        static struct u_object l_w__1948 = U_LIT("w_");
        u_value t1949 = f_c_46_c_call_2(&l_u_mk_closure_1947, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__1948, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        *r_r = t1949;
      }
    } else {
      {
        u_value s1950 = f_compile_46_fields_count_2(v_gl, v_n);
        int d1951 = 0;
        if ((!d1951) && (u_is_con(s1950, &n_option_46_Some) && u_equals(u_field(s1950, 0), u_mk_int(0)))) {
          d1951 = 1;
          {
            static struct u_object l_u_mk_con_1952 = U_LIT("u_mk_con");
            u_value t1953 = f_c_46_c_call_2(&l_u_mk_con_1952, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1953;
          }
        }
        if ((!d1951) && u_is_con(s1950, &n_option_46_Some)) {
          d1951 = 1;
          {
            static struct u_object l_u_mk_closure_1954 = U_LIT("u_mk_closure");
            static struct u_object l_wc__1955 = U_LIT("wc_");
            u_value t1956 = f_c_46_c_call_2(&l_u_mk_closure_1954, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__1955, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1956;
          }
        }
        if ((!d1951) && u_is_con(s1950, &n_option_46_None)) {
          d1951 = 1;
          {
            u_value s1957 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
            int d1958 = 0;
            if ((!d1958) && u_is_con(s1957, &n_option_46_Some)) {
              u_value v_f = u_field(u_field(s1957, 0), 0);
              d1958 = 1;
              {
                static struct u_object l_u_mk_closure_1959 = U_LIT("u_mk_closure");
                static struct u_object l_w_1960 = U_LIT("w");
                u_value t1961 = f_c_46_c_call_2(&l_u_mk_closure_1959, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_1960, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
                *r_r = t1961;
              }
            }
            if ((!d1958) && u_is_con(s1957, &n_option_46_None)) {
              d1958 = 1;
              {
                static struct u_object l_the_32_variable_32__1962 = U_LIT("the variable ");
                u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__1962, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
              }
            }
            if (!d1958) {
              u_no_case(s1957);
            }
          }
        }
        if (!d1951) {
          u_no_case(s1950);
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
    u_value s1963 = v_e;
    int d1964 = 0;
    if ((!d1964) && (u_is_con(s1963, &n_syntax_46_ELit) && u_is_con(u_field(s1963, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s1963, 0), 0);
      d1964 = 1;
      {
        static struct u_object l_l__1965 = U_LIT("l_");
        u_value t1966;
        if (u_truth(u_lt(u_str_size(v_s), u_mk_int(16)))) {
          t1966 = u_str_size(v_s);
        } else {
          t1966 = u_mk_int(16);
        }
        static struct u_object l___1967 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__1965, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), t1966))), &l___1967)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__1968 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_1969 = U_LIT("U_LIT");
        static struct u_object l__38__1970 = U_LIT("&");
        u_value t1971 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__1968), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_1969, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t1972 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1970, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t1971;
        *r_r = t1972;
      }
    }
    if ((!d1964) && u_is_con(s1963, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s1963, 0);
      d1964 = 1;
      {
        u_value t1973 = u_mk_con(&n_list_46_Nil, 0);
        u_value t1974 = f_compile_46_literal_value_1(v_l);
        *r_ss = t1973;
        *r_r = t1974;
      }
    }
    if ((!d1964) && (u_is_con(s1963, &n_syntax_46_EVar) && u_is_con(u_field(s1963, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s1963, 0), 0);
      u_value v_ty = u_field(u_field(s1963, 0), 1);
      d1964 = 1;
      {
        u_value s1975 = f_list_46_lookup_2(v_n, v_env);
        int d1976 = 0;
        if ((!d1976) && u_is_con(s1975, &n_option_46_Some)) {
          u_value v_c = u_field(s1975, 0);
          d1976 = 1;
          {
            u_value t1977 = u_mk_con(&n_list_46_Nil, 0);
            u_value t1978 = v_c;
            *r_ss = t1977;
            *r_r = t1978;
          }
        }
        if ((!d1976) && u_is_con(s1975, &n_option_46_None)) {
          d1976 = 1;
          {
            p_compile_46_global_value(v_gl, v_n, v_ty, r_r);
            u_value t1979 = u_mk_con(&n_list_46_Nil, 0);
            *r_ss = t1979;
          }
        }
        if (!d1976) {
          u_no_case(s1975);
        }
      }
    }
    if ((!d1964) && (u_is_con(s1963, &n_syntax_46_EApp) && (u_is_con(u_field(s1963, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s1963, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s1963, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s1963, 0), 0), 1);
      u_value v_args = u_field(s1963, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d1964 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d1964) && u_is_con(s1963, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s1963, 0);
      u_value v_args = u_field(s1963, 1);
      d1964 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_1980 = U_LIT("u_apply");
        u_value t1981 = f_list_46__43__43__2(v_as, v_bs);
        u_value t1982 = f_c_46_c_call_2(&l_u_apply_1980, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t1981;
        *r_r = t1982;
      }
    }
    if ((!d1964) && u_is_con(s1963, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s1963, 0);
      u_value v_b = u_field(s1963, 1);
      d1964 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d1964) && u_is_con(s1963, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s1963, 0);
      u_value v_b = u_field(s1963, 1);
      d1964 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1983 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1983, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t1984 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t1984;
        u_value t1985 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1985;
      }
    }
    if ((!d1964) && u_is_con(s1963, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s1963, 0);
      u_value v_l = u_field(s1963, 1);
      u_value v_rr = u_field(s1963, 2);
      d1964 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1986 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1986, &v_t, r_g);
        u_value t1987 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t1987;
        u_value t1988 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1988;
      }
    }
    if ((!d1964) && u_is_con(s1963, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s1963, 0);
      u_value v_cases = u_field(s1963, 1);
      d1964 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_1989 = U_LIT("s");
        p_compile_46_gen_name(&l_s_1989, &v_d, r_g);
        static struct u_object l_t_1990 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1990, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_1991 = U_LIT("NULL");
        u_value t1992 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1991))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t1992;
        static struct u_object l__33__1993 = U_LIT("!");
        static struct u_object l_u_no_case_1994 = U_LIT("u_no_case");
        u_value t1995 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1993, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_1994, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1995;
        u_value t1996 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1996;
      }
    }
    if ((!d1964) && u_is_con(s1963, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s1963, 0);
      u_value v_xs = u_field(s1963, 1);
      u_value v_lists = u_field(s1963, 2);
      u_value v_c = u_field(s1963, 3);
      d1964 = 1;
      {
        static struct u_object l_t_1997 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1997, &v_t, r_g);
        static struct u_object l_e_1998 = U_LIT("e");
        p_compile_46_gen_name(&l_e_1998, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_1999 = U_LIT("u_mk_con");
        static struct u_object l__38__2000 = U_LIT("&");
        u_value t2001 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_1999, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__2000, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t2001;
        u_value t2002 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2002;
      }
    }
    if ((!d1964) && u_is_con(s1963, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s1963, 0);
      d1964 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_2003 = U_LIT("u_mk_tuple");
        u_value t2004 = f_c_46_c_call_2(&l_u_mk_tuple_2003, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t2004;
      }
    }
    if ((!d1964) && u_is_con(s1963, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s1963, 0);
      u_value v_ty = u_field(s1963, 1);
      d1964 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2005 = U_LIT("u_raise");
        static struct u_object l_NULL_2006 = U_LIT("NULL");
        u_value t2007 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2005, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        u_value t2008 = u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2006);
        *r_ss = t2007;
        *r_r = t2008;
      }
    }
    if (!d1964) {
      u_no_case(s1963);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s2009 = v_es;
    int d2010 = 0;
    if ((!d2010) && u_is_con(s2009, &n_list_46_Nil)) {
      d2010 = 1;
      {
        u_value t2011 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2012 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2011;
        *r_rs = t2012;
      }
    }
    if ((!d2010) && u_is_con(s2009, &n_list_46_Cons)) {
      u_value v_e = u_field(s2009, 0);
      u_value v_rest = u_field(s2009, 1);
      d2010 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t2013 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2014 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t2013;
        *r_rs = t2014;
      }
    }
    if (!d2010) {
      u_no_case(s2009);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s2015 = v_rs;
    int d2016 = 0;
    if ((!d2016) && u_is_con(s2015, &n_list_46_Nil)) {
      d2016 = 1;
      {
        u_value t2017 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2017;
      }
    }
    if ((!d2016) && u_is_con(s2015, &n_list_46_Cons)) {
      u_value v_r = u_field(s2015, 0);
      u_value v_rest = u_field(s2015, 1);
      d2016 = 1;
      {
        static struct u_object l_t_2018 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2018, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t2019 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t2019;
      }
    }
    if (!d2016) {
      u_no_case(s2015);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s2020 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2021 = NULL;
  if ((!t2021) && (u_is_con(u_field(s2020, 0), &n_list_46_Cons) && (u_is_con(u_field(s2020, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2020, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2020, 0), 0);
    u_value v_more = u_field(u_field(s2020, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2020, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2020, 1), 1);
    t2021 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t2021) {
    t2021 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2021) {
    u_no_case(s2020);
  }
  return t2021;
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
    u_value s2022 = v_args;
    int d2023 = 0;
    if ((!d2023) && (u_is_con(s2022, &n_list_46_Cons) && (u_is_con(u_field(s2022, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2022, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s2022, 0);
      u_value v_b = u_field(u_field(s2022, 1), 0);
      static struct u_object l_and_2030 = U_LIT("and");
      u_value t2032 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2030));
      if (!u_truth(t2032)) {
        static struct u_object l_or_2031 = U_LIT("or");
        t2032 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_2031));
      }
      if (u_truth(t2032)) {
        d2023 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_2024 = U_LIT("t");
          p_compile_46_gen_name(&l_t_2024, &v_t, r_g);
          static struct u_object l_and_2025 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2025)))) {
            {
              u_value t2026 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2026;
            }
          } else {
            {
              static struct u_object l__33__2027 = U_LIT("!");
              u_value t2028 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2027, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2028;
            }
          }
          u_value t2029 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t2029;
        }
      }
    }
    if (!d2023) {
      d2023 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t2033 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t2033)) {
          t2033 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t2033)) {
          {
            u_value t2034 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t2034;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_2035 = U_LIT("u_mk_con");
              u_value t2036 = f_c_46_c_call_2(&l_u_mk_con_2035, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t2036;
            }
          } else {
            {
              u_value s2037 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
              int d2038 = 0;
              if ((!d2038) && u_is_con(s2037, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s2037, 0), 0);
                u_value v_k = u_field(u_field(s2037, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d2038 = 1;
                  {
                    u_value t2039 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t2039;
                  }
                }
              }
              if (!d2038) {
                d2038 = 1;
                {
                  p_compile_46_global_value(v_gl, v_n, v_ty, &v_f);
                  static struct u_object l_u_apply_2040 = U_LIT("u_apply");
                  u_value t2041 = f_c_46_c_call_2(&l_u_apply_2040, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t2041;
                }
              }
              if (!d2038) {
                u_no_case(s2037);
              }
            }
          }
        }
      }
    }
    if (!d2023) {
      u_no_case(s2022);
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
    u_value t2042 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2043 = &t2042;
    u_value l2044 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l2044, &n_list_46_Cons)) {
      u_value v_x = u_field(l2044, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2043 = u_append(e2043, v_x);
      }
      l2044 = u_field(l2044, 1);
    }
    u_value t2045 = t2042;
    v_caps = t2045;
    static struct u_object l_lam_2046 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_2046, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_2047 = U_LIT("env");
    static struct u_object l_args_2048 = U_LIT("args");
    u_value t2049 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2047), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_2048));
    v_decls = t2049;
    static struct u_object l_env_2050 = U_LIT("env");
    static struct u_object l_args_2051 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2050), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2051), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_2052 = U_LIT("u_mk_closure");
    u_value t2053 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2054 = &t2053;
    u_value l2055 = v_caps;
    while (u_is_con(l2055, &n_list_46_Cons)) {
      u_value v_x = u_field(l2055, 0);
      if (u_truth(u_mk_bool(1))) {
        e2054 = u_append(e2054, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2055 = u_field(l2055, 1);
    }
    u_value t2056 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2057 = f_c_46_c_call_2(&l_u_mk_closure_2052, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2053));
    *r_ss = t2056;
    *r_r = t2057;
  }
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s2058 = v_xs;
  u_value t2059 = NULL;
  if ((!t2059) && u_is_con(s2058, &n_list_46_Nil)) {
    t2059 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2059) && u_is_con(s2058, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2058, 0), 0);
    u_value v_i = u_field(u_field(s2058, 0), 1);
    u_value v_rest = u_field(s2058, 1);
    t2059 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t2059) {
    u_no_case(s2058);
  }
  return t2059;
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
    u_value s2060 = v_cases;
    int d2061 = 0;
    if ((!d2061) && u_is_con(s2060, &n_list_46_Nil)) {
      d2061 = 1;
      {
        u_value t2062 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2062;
      }
    }
    if ((!d2061) && (u_is_con(s2060, &n_list_46_Cons) && u_is_con(u_field(s2060, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2060, 0), 0);
      u_value v_guard = u_field(u_field(s2060, 0), 1);
      u_value v_e = u_field(u_field(s2060, 0), 2);
      u_value v_more = u_field(s2060, 1);
      d2061 = 1;
      {
        u_value t2063 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2063;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2064 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t2064;
        u_value s2065 = v_guard;
        int d2066 = 0;
        if ((!d2066) && u_is_con(s2065, &n_option_46_Some)) {
          u_value v_c = u_field(s2065, 0);
          d2066 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2067 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2067;
          }
        }
        if ((!d2066) && u_is_con(s2065, &n_option_46_None)) {
          d2066 = 1;
          {
          }
        }
        if (!d2066) {
          u_no_case(s2065);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2068 = U_LIT("!");
        u_value t2069 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2068, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2069;
      }
    }
    if (!d2061) {
      u_no_case(s2060);
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
    u_value s2070 = u_mk_tuple(2, v_xs, v_lists);
    int d2071 = 0;
    if ((!d2071) && (u_is_con(u_field(s2070, 0), &n_list_46_Cons) && u_is_con(u_field(s2070, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2070, 0), 0);
      u_value v_more = u_field(u_field(s2070, 0), 1);
      u_value v_list = u_field(u_field(s2070, 1), 0);
      u_value v_others = u_field(u_field(s2070, 1), 1);
      d2071 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2072 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2072, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2073 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2073;
        static struct u_object l_u_is_con_2074 = U_LIT("u_is_con");
        u_value t2075 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2074, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2075;
      }
    }
    if (!d2071) {
      d2071 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2076 = U_LIT("u_append");
        u_value t2077 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2076, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2077;
      }
    }
    if (!d2071) {
      u_no_case(s2070);
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
    u_value s2078 = v_s;
    int d2079 = 0;
    if ((!d2079) && u_is_con(s2078, &n_syntax_46_SSkip)) {
      d2079 = 1;
      {
        u_value t2080 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2080;
      }
    }
    if ((!d2079) && u_is_con(s2078, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2078, 0);
      d2079 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2081 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2081;
      }
    }
    if ((!d2079) && u_is_con(s2078, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2078, 0);
      u_value v_body = u_field(s2078, 1);
      d2079 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t2082 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2083 = &t2082;
        u_value l2084 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2084, &n_list_46_Cons)) {
          u_value v_x = u_field(l2084, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2085 = U_LIT("NULL");
            e2083 = u_append(e2083, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2085))));
          }
          l2084 = u_field(l2084, 1);
        }
        u_value t2086 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2082, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2086;
      }
    }
    if ((!d2079) && u_is_con(s2078, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2078, 0);
      u_value v_l = u_field(s2078, 1);
      u_value v_r = u_field(s2078, 2);
      d2079 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2087 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2087;
      }
    }
    if ((!d2079) && u_is_con(s2078, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2078, 0);
      u_value v_b = u_field(s2078, 1);
      d2079 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2088 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2088, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2089 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2089;
      }
    }
    if ((!d2079) && u_is_con(s2078, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2078, 0);
      u_value v_ins = u_field(s2078, 1);
      u_value v_outs = u_field(s2078, 2);
      u_value v_refs = u_field(s2078, 3);
      d2079 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        u_value t2090 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2091 = &t2090;
        u_value l2092 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2092, &n_list_46_Cons)) {
          u_value v_x = u_field(l2092, 0);
          if (u_truth(u_mk_bool(1))) {
            e2091 = u_append(e2091, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
          }
          l2092 = u_field(l2092, 1);
        }
        u_value t2093 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2090))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2093;
      }
    }
    if ((!d2079) && u_is_con(s2078, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2078, 0);
      u_value v_cases = u_field(s2078, 1);
      d2079 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2094 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2094, &v_d, r_g);
        static struct u_object l_d_2095 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2095, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2096 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2096;
        static struct u_object l__33__2097 = U_LIT("!");
        static struct u_object l_u_no_case_2098 = U_LIT("u_no_case");
        u_value t2099 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2097, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2098, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2099;
      }
    }
    if ((!d2079) && (u_is_con(s2078, &n_syntax_46_SAssert) && (u_is_con(u_field(s2078, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2078, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2078, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2078, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2078, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2078, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2078, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2078, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2078, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2078, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2078, 0), 1), 1), 0);
      d2079 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2100 = U_LIT("u_assert_equal");
        u_value t2101 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2100, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2101;
      }
    }
    if ((!d2079) && u_is_con(s2078, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2078, 0);
      d2079 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2102 = U_LIT("u_assert_true");
        u_value t2103 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2102, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2103;
      }
    }
    if ((!d2079) && u_is_con(s2078, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2078, 0);
      u_value v_ty = u_field(s2078, 1);
      d2079 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2104 = U_LIT("u_raise");
        u_value t2105 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2104, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2105;
      }
    }
    if ((!d2079) && u_is_con(s2078, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2078, 0);
      u_value v_hs = u_field(s2078, 1);
      d2079 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2106 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2106, &v_d, r_g);
        static struct u_object l_d_2107 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2107, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2108 = U_LIT("u_pop");
        static struct u_object l__33__2109 = U_LIT("!");
        static struct u_object l_u_reraise_2110 = U_LIT("u_reraise");
        u_value t2111 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2108, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2109, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2110, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2111;
        static struct u_object l_struct_32_u_handler_2112 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2113 = U_LIT("u_push");
        static struct u_object l__61__61__2114 = U_LIT("==");
        static struct u_object l_setjmp_2115 = U_LIT("setjmp");
        static struct u_object l_env_2116 = U_LIT("env");
        static struct u_object l_u_pop_2117 = U_LIT("u_pop");
        u_value t2118 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2112)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2113, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2114, f_c_46_c_call_2(&l_setjmp_2115, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2116), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2117, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2118;
      }
    }
    if (!d2079) {
      u_no_case(s2078);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2119 = v_xs;
    int d2120 = 0;
    if ((!d2120) && u_is_con(s2119, &n_list_46_Nil)) {
      d2120 = 1;
      {
        u_value t2121 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2121;
      }
    }
    if ((!d2120) && u_is_con(s2119, &n_list_46_Cons)) {
      u_value v_x = u_field(s2119, 0);
      u_value v_rest = u_field(s2119, 1);
      d2120 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2122 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2122;
      }
    }
    if (!d2120) {
      u_no_case(s2119);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2123 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2124 = NULL;
  if ((!t2124) && (u_is_con(u_field(s2123, 0), &n_list_46_Cons) && (u_is_con(u_field(s2123, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2123, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2123, 0), 0);
    u_value v_more = u_field(u_field(s2123, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2123, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2123, 1), 1);
    t2124 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2124) {
    t2124 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2124) {
    u_no_case(s2123);
  }
  return t2124;
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
    static struct u_object l_u_caught_2125 = U_LIT("u_caught");
    u_value t2126 = f_c_46_c_call_2(&l_u_caught_2125, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2126;
    u_value s2127 = v_hs;
    int d2128 = 0;
    if ((!d2128) && u_is_con(s2127, &n_list_46_Nil)) {
      d2128 = 1;
      {
        u_value t2129 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2129;
      }
    }
    if ((!d2128) && (u_is_con(s2127, &n_list_46_Cons) && u_is_con(u_field(s2127, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2127, 0), 0);
      u_value v_p = u_field(u_field(s2127, 0), 1);
      u_value v_guard = u_field(u_field(s2127, 0), 2);
      u_value v_s = u_field(u_field(s2127, 0), 3);
      u_value v_more = u_field(s2127, 1);
      d2128 = 1;
      {
        u_value t2130 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2130;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2131 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2131;
        u_value s2132 = v_guard;
        int d2133 = 0;
        if ((!d2133) && u_is_con(s2132, &n_option_46_Some)) {
          u_value v_c = u_field(s2132, 0);
          d2133 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2134 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2134;
          }
        }
        if ((!d2133) && u_is_con(s2132, &n_option_46_None)) {
          d2133 = 1;
          {
          }
        }
        if (!d2133) {
          u_no_case(s2132);
        }
        u_value s2135 = v_p;
        int d2136 = 0;
        if ((!d2136) && u_is_con(s2135, &n_syntax_46_PWild)) {
          d2136 = 1;
          {
            u_value t2137 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2137;
          }
        }
        if (!d2136) {
          d2136 = 1;
          {
            static struct u_object l_u_has_type_2138 = U_LIT("u_has_type");
            u_value t2139 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2138, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2139;
          }
        }
        if (!d2136) {
          u_no_case(s2135);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2140 = U_LIT("!");
        u_value t2141 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2140, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2141;
      }
    }
    if (!d2128) {
      u_no_case(s2127);
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
    u_value s2142 = v_cases;
    int d2143 = 0;
    if ((!d2143) && u_is_con(s2142, &n_list_46_Nil)) {
      d2143 = 1;
      {
        u_value t2144 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2144;
      }
    }
    if ((!d2143) && (u_is_con(s2142, &n_list_46_Cons) && u_is_con(u_field(s2142, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2142, 0), 0);
      u_value v_guard = u_field(u_field(s2142, 0), 1);
      u_value v_s = u_field(u_field(s2142, 0), 2);
      u_value v_more = u_field(s2142, 1);
      d2143 = 1;
      {
        u_value t2145 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2145;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2146 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2146;
        u_value s2147 = v_guard;
        int d2148 = 0;
        if ((!d2148) && u_is_con(s2147, &n_option_46_Some)) {
          u_value v_c = u_field(s2147, 0);
          d2148 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2149 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2149;
          }
        }
        if ((!d2148) && u_is_con(s2147, &n_option_46_None)) {
          d2148 = 1;
          {
          }
        }
        if (!d2148) {
          u_no_case(s2147);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2150 = U_LIT("!");
        u_value t2151 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2150, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2151;
      }
    }
    if (!d2143) {
      u_no_case(s2142);
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
    u_value s2152 = v_c;
    int d2153 = 0;
    if ((!d2153) && u_is_con(s2152, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2152, 0);
      u_value v_xss = u_field(s2152, 2);
      u_value v_b = u_field(s2152, 4);
      d2153 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2154 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2155 = &t2154;
        u_value l2156 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2156, &n_list_46_Cons)) {
          u_value v_x = u_field(l2156, 0);
          if (u_truth(u_mk_bool(1))) {
            e2155 = u_append(e2155, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2156 = u_field(l2156, 1);
        }
        u_value t2157 = t2154;
        v_params = t2157;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2158 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2158;
          }
        } else {
          {
            u_value t2159 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2159;
          }
        }
        u_value t2160 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2161 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2160;
        *r_ms = t2161;
      }
    }
    if ((!d2153) && u_is_con(s2152, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2152, 0);
      u_value v_ins = u_field(s2152, 2);
      u_value v_outs = u_field(s2152, 3);
      u_value v_refs = u_field(s2152, 4);
      u_value v_body = u_field(s2152, 5);
      d2153 = 1;
      {
        u_value t2162 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2163 = &t2162;
        u_value l2164 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2164, &n_list_46_Cons)) {
          u_value v_x = u_field(l2164, 0);
          if (u_truth(u_mk_bool(1))) {
            e2163 = u_append(e2163, f_compile_46_pointer_1(v_x));
          }
          l2164 = u_field(l2164, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2162), v_body, &v_bs, r_g);
        u_value t2165 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2166 = &t2165;
        u_value l2167 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2167, &n_list_46_Cons)) {
          u_value v_x = u_field(l2167, 0);
          if (u_truth(u_mk_bool(1))) {
            e2166 = u_append(e2166, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2167 = u_field(l2167, 1);
        }
        u_value t2168 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2169 = &t2168;
        u_value l2170 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2170, &n_list_46_Cons)) {
          u_value v_x = u_field(l2170, 0);
          if (u_truth(u_mk_bool(1))) {
            e2169 = u_append(e2169, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2170 = u_field(l2170, 1);
        }
        u_value t2171 = f_list_46__43__43__2(t2165, t2168);
        v_params = t2171;
        u_value t2172 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2173 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2172;
        *r_ms = t2173;
      }
    }
    if ((!d2153) && u_is_con(s2152, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s2152, 0);
      d2153 = 1;
      {
        p_compile_46_compile_expr(v_gl, u_mk_con(&n_list_46_Nil, 0), v_e, &v_bs, &v_rb, r_g);
        static struct u_object l_u_print_line_2174 = U_LIT("u_print_line");
        u_value t2175 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2176 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_line_2174, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2175;
        *r_ms = t2176;
      }
    }
    if ((!d2153) && (u_is_con(s2152, &n_syntax_46_CRun) && u_is_con(u_field(s2152, 1), &n_syntax_46_SBlock))) {
      u_value v_xs = u_field(u_field(s2152, 1), 0);
      u_value v_body = u_field(u_field(s2152, 1), 1);
      d2153 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_body, &v_bs, r_g);
        u_value t2177 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2178 = &t2177;
        u_value l2179 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2179, &n_list_46_Cons)) {
          u_value v_x = u_field(l2179, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2180 = U_LIT("NULL");
            e2178 = u_append(e2178, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2180))));
          }
          l2179 = u_field(l2179, 1);
        }
        u_value t2181 = t2177;
        v_copy = t2181;
        u_value t2182 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2183 = &t2182;
        u_value l2184 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2184, &n_list_46_Cons)) {
          u_value v_x = u_field(l2184, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_u_print_binding_2185 = U_LIT("u_print_binding");
            e2183 = u_append(e2183, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_binding_2185, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_syntax_46_Name_46_base_1(v_x)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x)), u_mk_con(&n_list_46_Nil, 0))))));
          }
          l2184 = u_field(l2184, 1);
        }
        u_value t2186 = t2182;
        v_back = t2186;
        u_value t2187 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2188 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_copy, f_list_46__43__43__2(v_bs, v_back))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2187;
        *r_ms = t2188;
      }
    }
    if ((!d2153) && u_is_con(s2152, &n_syntax_46_CRun)) {
      u_value v_body = u_field(s2152, 1);
      d2153 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2189 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2190 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2189;
        *r_ms = t2190;
      }
    }
    if ((!d2153) && u_is_con(s2152, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s2152, 1);
      d2153 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2191 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2192 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2191;
        *r_ms = t2192;
      }
    }
    if (!d2153) {
      d2153 = 1;
      {
        u_value t2193 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2194 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2193;
        *r_ms = t2194;
      }
    }
    if (!d2153) {
      u_no_case(s2152);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2195 = v_cs;
    int d2196 = 0;
    if ((!d2196) && u_is_con(s2195, &n_list_46_Nil)) {
      d2196 = 1;
      {
        u_value t2197 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2198 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2197;
        *r_ms = t2198;
      }
    }
    if ((!d2196) && u_is_con(s2195, &n_list_46_Cons)) {
      u_value v_c = u_field(s2195, 0);
      u_value v_rest = u_field(s2195, 1);
      d2196 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_rest, &v_d2, &v_m2, r_g);
        u_value t2199 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2200 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2199;
        *r_ms = t2200;
      }
    }
    if (!d2196) {
      u_no_case(s2195);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_handler_2201 = U_LIT("handler");
  u_value t2228 = u_mk_con(&n_c_46_CeVar, 1, &l_handler_2201);
  u_value t2227;
  {
    u_value v_h = t2228;
    static struct u_object l_static_32_u_value_2202 = U_LIT("static u_value");
    static struct u_object l_cache_2203 = U_LIT("cache");
    static struct u_object l_NULL_2204 = U_LIT("NULL");
    static struct u_object l__61__61__2205 = U_LIT("==");
    static struct u_object l_cache_2206 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2207 = U_LIT("U_COMPUTING");
    static struct u_object l_u_fail_2208 = U_LIT("u_fail");
    static struct u_object l_recursive_32_initia_2209 = U_LIT("recursive initialization of ");
    static struct u_object l_cache_2210 = U_LIT("cache");
    static struct u_object l_cache_2211 = U_LIT("cache");
    static struct u_object l_cache_2212 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2213 = U_LIT("U_COMPUTING");
    static struct u_object l_struct_32_u_handler_2214 = U_LIT("struct u_handler");
    static struct u_object l_handler_2215 = U_LIT("handler");
    static struct u_object l_u_push_2216 = U_LIT("u_push");
    static struct u_object l__33__61__2217 = U_LIT("!=");
    static struct u_object l_setjmp_2218 = U_LIT("setjmp");
    static struct u_object l_env_2219 = U_LIT("env");
    static struct u_object l_u_pop_2220 = U_LIT("u_pop");
    static struct u_object l_cache_2221 = U_LIT("cache");
    static struct u_object l_NULL_2222 = U_LIT("NULL");
    static struct u_object l_u_reraise_2223 = U_LIT("u_reraise");
    static struct u_object l_u_pop_2224 = U_LIT("u_pop");
    static struct u_object l_cache_2225 = U_LIT("cache");
    static struct u_object l_cache_2226 = U_LIT("cache");
    t2227 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2202), &l_cache_2203, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2204))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2205, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2206), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2207)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2208, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2209, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2210), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2211))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2212), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2213)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2214)), &l_handler_2215, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2216, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__33__61__2217, f_c_46_c_call_2(&l_setjmp_2218, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, v_h, &l_env_2219), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2220, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2221), u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2222)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2223, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2224, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2225), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2226))), u_mk_con(&n_list_46_Nil, 0))))));
  }
  return t2227;
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2229 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2230 = &t2229;
  u_value l2231 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l2231, &n_list_46_Cons)) {
    u_value v_p = u_field(l2231, 0);
    if (u_truth(u_mk_bool(1))) {
      e2230 = u_append(e2230, f_compile_46_fun_wrapper_1(v_p));
    }
    l2231 = u_field(l2231, 1);
  }
  u_value t2232 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2233 = &t2232;
  u_value l2234 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l2234, &n_list_46_Cons)) {
    u_value v_p = u_field(l2234, 0);
    if (u_truth(u_mk_bool(1))) {
      e2233 = u_append(e2233, f_compile_46_constr_wrapper_1(v_p));
    }
    l2234 = u_field(l2234, 1);
  }
  u_value t2235 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2236 = &t2235;
  u_value l2237 = f_list_46_seconds_1(f_compile_46_builtin_functions_0());
  while (u_is_con(l2237, &n_list_46_Cons)) {
    u_value v_p = u_field(l2237, 0);
    if (u_truth(u_mk_bool(1))) {
      e2236 = u_append(e2236, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2237 = u_field(l2237, 1);
  }
  return f_list_46__43__43__2(t2229, f_list_46__43__43__2(f_list_46_concat_1(t2232), t2235));
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2238 = v_p;
  u_value t2239 = NULL;
  if (!t2239) {
    u_value v_n = u_field(s2238, 0);
    u_value v_k = u_field(s2238, 1);
    static struct u_object l_w__2240 = U_LIT("w_");
    t2239 = f_compile_46_wrapper_3(u_concat(&l_w__2240, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2239) {
    u_no_case(s2238);
  }
  return t2239;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2241 = v_p;
  u_value t2242 = NULL;
  if (!t2242) {
    u_value v_f = u_field(s2241, 0);
    u_value v_k = u_field(s2241, 1);
    static struct u_object l_w_2243 = U_LIT("w");
    t2242 = f_compile_46_wrapper_3(u_concat(&l_w_2243, v_f), v_f, v_k);
  }
  if (!t2242) {
    u_no_case(s2241);
  }
  return t2242;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2244 = U_LIT("env");
  static struct u_object l_args_2245 = U_LIT("args");
  u_value t2246 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2247 = &t2246;
  u_value l2248 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2248, &n_list_46_Cons)) {
    u_value v_i = u_field(l2248, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2249 = U_LIT("args");
      e2247 = u_append(e2247, f_compile_46_index_2(&l_args_2249, v_i));
    }
    l2248 = u_field(l2248, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2244), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2245), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2246))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2250 = v_p;
  u_value t2251 = NULL;
  if ((!t2251) && u_equals(u_field(s2250, 1), u_mk_int(0))) {
    t2251 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2251) {
    u_value v_n = u_field(s2250, 0);
    u_value v_k = u_field(s2250, 1);
    static struct u_object l_wc__2252 = U_LIT("wc_");
    static struct u_object l_env_2253 = U_LIT("env");
    static struct u_object l_args_2254 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2255 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2256 = U_LIT("args");
    t2251 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2252, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2253), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2254), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2255, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2256), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2251) {
    u_no_case(s2250);
  }
  return t2251;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2257;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2257 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2257 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2257;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2258 = v_d;
  u_value t2259 = NULL;
  if ((!t2259) && u_is_con(s2258, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2258, 0);
    u_value v_f = u_field(s2258, 1);
    u_value v_ps = u_field(s2258, 2);
    t2259 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2259) {
    t2259 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2259) {
    u_no_case(s2258);
  }
  return t2259;
}

void p_compile_46_compile_program(u_value v_q, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2260 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2260;
    u_value t2261 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t2261;
    p_compile_46_compile_cmds(v_gl, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value s2262 = v_g;
    int d2263 = 0;
    if ((!d2263) && u_is_con(s2262, &n_compile_46_Gen)) {
      u_value v_lifted = u_field(s2262, 1);
      d2263 = 1;
      {
        u_value t2264 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, v_lifted));
        v_fs = t2264;
      }
    }
    if (!d2263) {
      u_no_case(s2262);
    }
    static struct u_object l_ulang_46_h_2265 = U_LIT("ulang.h");
    u_value t2266 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2267 = &t2266;
    u_value l2268 = v_fs;
    while (u_is_con(l2268, &n_list_46_Cons)) {
      u_value v_d = u_field(l2268, 0);
      if (u_truth(u_mk_bool(1))) {
        e2267 = u_append(e2267, f_compile_46_prototype_1(v_d));
      }
      l2268 = u_field(l2268, 1);
    }
    static struct u_object l_main_2269 = U_LIT("main");
    static struct u_object l_argc_2270 = U_LIT("argc");
    static struct u_object l_char_2271 = U_LIT("char");
    static struct u_object l_argv_2272 = U_LIT("argv");
    static struct u_object l_u_args_2273 = U_LIT("u_args");
    static struct u_object l_argc_2274 = U_LIT("argc");
    static struct u_object l_argv_2275 = U_LIT("argv");
    u_value t2276 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2265), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2266), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2269, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2270), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2271))), &l_argv_2272), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2273, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2274), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2275), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2276;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_target) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2277 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2277, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2278 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2278, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q);
    p_compile_46_compile_program(v_q, &v_f);
    p_os_write(v_target, f_c_46_c_file_1(v_f));
  }
}

u_value lam312(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam313(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam315(u_value *env, u_value *args) {
  u_value v_su = env[0];
  u_value v_i = env[1];
  u_value v_a = args[0];
  return f_check_46_occurs_3(v_su, v_i, v_a);
}

u_value lam325(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_f = args[0];
  return f_ctx_46_is_fun_2(f_check_46_State_46_ctx_1(v_s), v_f);
}

u_value lam332(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam557(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_t = args[0];
  return f_ctx_46_is_sort_2(f_check_46_State_46_ctx_1(v_s), v_t);
}

u_value lam666(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam669(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam731(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_q = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_q);
}

u_value lam1008(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  {
    u_value v_args = NULL;
    p_os_args(&v_args);
    u_value s2279 = v_args;
    int d2280 = 0;
    if ((!d2280) && u_is_con(s2279, &n_list_46_Nil)) {
      d2280 = 1;
      {
      }
    }
    if ((!d2280) && (u_is_con(s2279, &n_list_46_Cons) && (u_is_con(u_field(s2279, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2279, 1), 1), &n_list_46_Nil)))) {
      u_value v_source = u_field(s2279, 0);
      u_value v_target = u_field(u_field(s2279, 1), 0);
      d2280 = 1;
      {
        p_compile_46_compile_file(v_source, v_target);
      }
    }
    if (!d2280) {
      d2280 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_sou_2281 = U_LIT("usage: ulang source.u target.c");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_sou_2281), "os.OsError");
      }
    }
    if (!d2280) {
      u_no_case(s2279);
    }
    u_print_binding("args", v_args);
  }
  return 0;
}
